#' Predict per-team qualifier xAdv for one 12-entry pod
#'
#' Pod-aware kernel for 3b-5's validation. Given a draft pod's 12
#' rosters, computes per-team qualifier-stage advance probability under
#' the rule: rank teams within each sim by cumulative starting-lineup
#' points over `ranking_weeks`, top `advance_n` advance.
#'
#' Implementation notes (the prompt's hard constraints):
#' - **One joint correlated draw over the union of all 12 rosters**, not
#'   12 independent per-team draws. Pod-mates that share NFL teams /
#'   games inherit the same realizations in each sim, and the per-sim
#'   totals are mutually comparable for ranking. We call 3b-2 once,
#'   route the `matrix_list` output straight into 3b-3 per team, and
#'   sum the resulting weekly totals over `ranking_weeks`.
#' - **Within-sim ranking uses `ties.method = "first"`** so each sim has
#'   exactly `advance_n` advancers -- the conservation law (sum of the
#'   per-team predicted_xadv across the pod equals advance_n) then holds
#'   to floating-point exactness. Continuous draws make exact ties
#'   measure-zero anyway; this choice protects the synthetic test cases
#'   that deliberately construct ties.
#' - **Bye / short-position weeks** are handled by 3b-2 (zeros for byed
#'   players) and 3b-3 (sorting pushes them to the bottom). The
#'   validator itself does no special-casing.
#'
#' @param pod_rosters Named list, length 12 (or however many entries
#'   the pod has): `entry_id -> character vector of underdog_id`.
#' @param positions Named character vector mapping `underdog_id` to
#'   position (`"QB"`, `"RB"`, `"WR"`, `"TE"`). Must cover every roster
#'   member.
#' @param layerA_draws Long Layer A draws data.frame (`underdog_id,
#'   sim_idx, week, draw_value`).
#' @param schedule Per-player-per-week data.frame with columns
#'   `underdog_id, week, team, opponent, is_bye` -- same shape 3b-2
#'   consumes.
#' @param lineup_spec Slate's lineup spec (output of
#'   [load_slate_lineup_spec()]).
#' @param ranking_weeks Integer vector of weeks summed to form the
#'   qualifier ranking metric. For Season-slate 4-stage tournaments this
#'   is `1:14`; pull from `tournament$stages[[1]]$weeks`.
#' @param advance_n Integer count of teams that advance per pod
#'   (BBM7/Puppy/Dachshund = 2, Mini Golden/Frenchie 3 = 4). Pull from
#'   `tournament$stages[[1]]$advancement$n`. Never assume a default.
#' @param n_sims Output sim count (default 10000).
#' @param seed Optional integer seed for [sample_correlated_draws()].
#' @param corr_params Correlation parameters (default
#'   [default_corr_params]).
#' @param precomputed_marginals Optional cache from
#'   [precompute_layerA_marginals()]. The validator's top-level run
#'   passes a slate-wide cache so per-pod calls avoid re-sorting Layer
#'   A draws.
#' @param availability_p_miss Optional named numeric vector
#'   (`underdog_id -> p_miss`). When supplied, an independent Bernoulli
#'   missed-week mask (`q = 1 - p_miss`) is applied post-inverse-CDF via
#'   [.mask_matrix_list()] -- the same draw-zeroing the tensor build applies --
#'   so the validator prices availability. `NULL` (default) = no mask.
#' @return A list with:
#'   \itemize{
#'     \item `predicted_xadv` -- named numeric vector
#'       (`entry_id -> probability`).
#'     \item `season_totals` -- `[n_teams x n_sims]` matrix of per-sim
#'       qualifier-stage totals. Mean across the row is our equivalent
#'       of `bbmdb_team_projection`.
#'   }
#' @export
predict_pod_xadv <- function(pod_rosters,
                             positions,
                             layerA_draws,
                             schedule,
                             lineup_spec,
                             ranking_weeks,
                             advance_n,
                             n_sims = 10000L,
                             seed = NULL,
                             corr_params = default_corr_params,
                             precomputed_marginals = NULL,
                             availability_p_miss = NULL) {
  if (!is.list(pod_rosters) || length(pod_rosters) == 0L) {
    cli::cli_abort("`pod_rosters` must be a non-empty list of entry_id -> roster.")
  }
  if (is.null(names(pod_rosters)) || any(!nzchar(names(pod_rosters)))) {
    cli::cli_abort("`pod_rosters` must be a named list (entry_id -> roster).")
  }
  advance_n <- as.integer(advance_n)
  if (is.na(advance_n) || advance_n < 1L) {
    cli::cli_abort("`advance_n` must be a positive integer.")
  }
  n_teams <- length(pod_rosters)
  if (advance_n >= n_teams) {
    cli::cli_abort(
      "`advance_n` ({advance_n}) must be < `length(pod_rosters)` ({n_teams})."
    )
  }
  ranking_weeks <- sort(unique(as.integer(ranking_weeks)))

  union_ids <- unique(unlist(pod_rosters, use.names = FALSE))
  miss_pos <- setdiff(union_ids, names(positions))
  if (length(miss_pos) > 0L) {
    cli::cli_abort(
      "`positions` missing entries for {length(miss_pos)} roster member(s): {head(miss_pos, 5)}"
    )
  }

  ml <- sample_correlated_draws(
    player_ids            = union_ids,
    layerA_draws          = layerA_draws,
    schedule              = schedule,
    corr_params           = corr_params,
    n_sims                = n_sims,
    seed                  = seed,
    output_format         = "matrix_list",
    precomputed_marginals = precomputed_marginals
  )

  # Draw-level availability: price missed weeks on the SAME conditional draws
  # the tensor build masks, so the validator scores teams with the shipped
  # availability (apples-to-apples vs BBMDB actuals, which include injuries).
  # Default NULL -> no mask (synthetic-fixture tests are unaffected).
  if (!is.null(availability_p_miss)) {
    q <- 1 - availability_p_miss[union_ids]
    q[is.na(q)] <- 1
    names(q) <- union_ids
    ml <- .mask_matrix_list(ml, q, seed)
  }

  # Restrict to the qualifier-stage ranking weeks. matrix_list is keyed
  # by character week numbers from sample_correlated_draws.
  ranking_keys <- as.character(ranking_weeks)
  ranking_keys <- intersect(ranking_keys, names(ml))
  if (length(ranking_keys) == 0L) {
    cli::cli_abort(
      "No `ranking_weeks` overlap with simulator output (got weeks: {names(ml)})."
    )
  }
  ml <- ml[ranking_keys]

  season_totals <- matrix(0, nrow = n_teams, ncol = n_sims,
                          dimnames = list(names(pod_rosters), NULL))
  for (i in seq_len(n_teams)) {
    team_pids <- intersect(pod_rosters[[i]], union_ids)
    team_pos  <- positions[team_pids]
    team_ml <- lapply(ml, function(M) {
      keep <- intersect(rownames(M), team_pids)
      M[keep, , drop = FALSE]
    })
    weekly_totals <- optimize_lineup_totals(
      scores      = team_ml,
      positions   = team_pos,
      lineup_spec = lineup_spec
    )
    season_totals[i, ] <- rowSums(weekly_totals)
  }

  # Per-sim rank with `ties.method = "first"` -- exactly advance_n teams
  # advance per column, conservation law holds exactly.
  ranks <- apply(season_totals, 2L,
                 function(x) rank(x, ties.method = "first"))
  advance <- ranks >= (n_teams - advance_n + 1L)
  predicted <- rowMeans(advance)
  names(predicted) <- names(pod_rosters)

  list(predicted_xadv = predicted, season_totals = season_totals)
}

#' Run the full BBMDB xAdv validation
#'
#' Loads BBMDB + scraper, finds the post-NFL-draft entry overlap (the
#' apples-to-apples set), filters to teams whose tournament has a
#' canonical config (~20 Season-slate teams currently), blends + simulates
#' Layer A once for the slate, then walks the unique pods invoking
#' [predict_pod_xadv()].
#'
#' Returns a per-team data.frame, aggregate metrics
#' (Spearman / MAE / signed bias), and a `team_projection` diagnostic
#' cross-check (Pearson between our season-mean and
#' `bbmdb_team_projection`). Optionally writes a markdown report.
#'
#' **Important framing** (carried into the report header): this is
#' **simulator-vs-simulator**. BBMDB is itself an assumption-driven
#' Monte Carlo; agreement validates *shared* assumptions, not ground
#' truth. Real-outcome validation needs a full played season.
#'
#' @param bbmdb_path Path to `bbmdb_teams.parquet`. Defaults to the
#'   canonical local path; pass a different path or skip the run when
#'   that file isn't present (the test suite does the latter).
#' @param scraper_path Path to the udbb-scraper export. Defaults to the
#'   in-repo canonical location.
#' @param slate_ids Character vector of slate IDs to validate. Defaults
#'   to `"nfl_2026_season"` -- the only slate with the 4-stage
#'   tournament shape the validator currently understands.
#' @param feed Optional pre-blended feed (from [blend_slate()]). When
#'   `NULL` (default), the validator blends and simulates Layer A on the
#'   fly. Pass an existing feed to skip the ~60s blend cost across
#'   repeated runs.
#' @param layerA_sim Optional pre-computed Layer A simulation output
#'   (`list(enriched_feed, draws)` from [simulate_slate()]). When `NULL`,
#'   produced internally.
#' @param layerA_n_sims Number of Layer A marginal draws to generate
#'   (default 10000L). Only used when `layerA_sim` is `NULL`.
#' @param n_sims Number of correlated sims per pod (default 10000L).
#' @param base_seed Integer base seed; each pod gets `base_seed + i` where
#'   `i` is the pod's position in the sorted unique-draft-id list.
#' @param report_path Optional path to write a markdown report. When set,
#'   the per-team CSV and aggregates are also written alongside (under
#'   the same dirname).
#' @param verbose Print progress (default `TRUE`).
#' @return A list with:
#'   \itemize{
#'     \item `per_team` -- data.frame, one row per BBMDB validation entry,
#'       with `entry_id`, `draft_id`, `tournament_name`, `tournament_id`,
#'       `advance_n`, `bbmdb_xadv`, `predicted_xadv`, `abs_err`,
#'       `our_team_projection`, `bbmdb_team_projection`, `status`.
#'     \item `aggregates` -- list with `n_validated`, `spearman`, `mae`,
#'       `mse`, `mean_signed_error`.
#'     \item `projection_xcheck` -- list with `pearson`, `n`,
#'       `our_range`, `bbmdb_range`.
#'   }
#' @export
validate_xadv_against_bbmdb <- function(bbmdb_path,
                                        scraper_path = NULL,
                                        slate_ids = "nfl_2026_season",
                                        feed = NULL,
                                        layerA_sim = NULL,
                                        layerA_n_sims = 10000L,
                                        n_sims = 10000L,
                                        base_seed = 1L,
                                        report_path = NULL,
                                        verbose = TRUE) {
  if (!file.exists(bbmdb_path)) {
    cli::cli_abort(c(
      "BBMDB parquet not found.",
      i = "Looked for: {.path {bbmdb_path}}"
    ))
  }
  scraper_path <- scraper_path %||%
    .inst_path("data/scraped_drafts", "udbb-scraper-latest.json")

  if (verbose) cli::cli_alert_info("Loading BBMDB + scraper")
  bbmdb <- arrow::read_parquet(bbmdb_path)
  picks <- load_scraped_drafts(scraper_path)

  # Resolve each entry's tournament config by Underdog UUID. Skip teams
  # whose tournament has no canonical config (e.g. Field General,
  # Frenchie Eliminator currently) without crashing.
  all_tnmts <- load_tournaments()
  by_uuid <- list()
  for (t in all_tnmts) {
    by_uuid[[t$underdog_tournament_id]] <- t
  }

  # Walk overlap of post-NFL-draft BBMDB rows with scraper entries.
  apples <- bbmdb[identical_post(bbmdb$tournament_corpus), , drop = FALSE]
  apples <- apples[apples$underdog_entry_id %in% picks$draft_entry_id, ,
                   drop = FALSE]
  apples$slate_uuid       <- NA_character_
  apples$tournament_uuid  <- NA_character_
  apples$draft_id         <- NA_character_
  apples$tournament_id    <- NA_character_
  apples$advance_n        <- NA_integer_
  apples$status           <- NA_character_

  for (i in seq_len(nrow(apples))) {
    eid <- apples$underdog_entry_id[i]
    rows <- picks[picks$draft_entry_id == eid, , drop = FALSE]
    if (nrow(rows) == 0L) {
      apples$status[i] <- "scraper_no_picks"
      next
    }
    apples$draft_id[i]        <- rows$draft_id[1L]
    apples$slate_uuid[i]      <- rows$slate_id[1L]
    apples$tournament_uuid[i] <- rows$tournament_id[1L]
    cfg <- by_uuid[[rows$tournament_id[1L]]]
    if (is.null(cfg)) {
      apples$status[i] <- "no_config"
      next
    }
    apples$tournament_id[i] <- cfg$tournament_id
    if (!(cfg$slate_id %in% slate_ids)) {
      apples$status[i] <- "out_of_scope_slate"
      next
    }
    apples$advance_n[i] <- as.integer(
      cfg$stages[[1L]]$advancement$n %||% NA_integer_
    )
    apples$status[i] <- "ready"
  }
  if (verbose) {
    cli::cli_alert_info("Overlap: {nrow(apples)} BBMDB post-draft entries match scraper")
    cli::cli_alert_info("Status breakdown: {paste(names(table(apples$status)), table(apples$status), sep='=', collapse=', ')}")
  }

  ready <- apples[apples$status == "ready", , drop = FALSE]

  # Build Layer A draws once for the in-scope slate (we only support
  # nfl_2026_season today, so just one slate).
  if (is.null(layerA_sim)) {
    if (is.null(feed)) {
      slate_to_blend <- slate_ids[1L]
      if (verbose) {
        cli::cli_alert_info("Blending slate {.val {slate_to_blend}}")
      }
      sources_path <- .inst_path("data/sources", "_manifest.yaml")
      slates_path  <- .inst_path("data/slates",  "_manifest.yaml")
      feed <- blend_slate(
        slate_id              = slate_to_blend,
        sources_manifest_path = sources_path,
        slates_manifest_path  = slates_path,
        write_json            = FALSE
      )
    }
    if (verbose) {
      cli::cli_alert_info("Simulating Layer A (n_sims = {layerA_n_sims})")
    }
    layerA_sim <- simulate_slate(feed, n_sims = layerA_n_sims,
                                 seed = base_seed)
  }
  enriched_feed <- layerA_sim$enriched_feed
  layerA_draws  <- layerA_sim$draws

  # Per-player position + per-(player, week) schedule from the feed.
  positions <- .positions_from_feed(enriched_feed)
  schedule  <- .schedule_from_feed(enriched_feed)

  # Per-player availability miss-rate, so the validator scores teams with the
  # shipped draw-zeroing (the tensor masks; the validator must too, else it
  # prices teams ~1/q hot vs BBMDB actuals, which carry real injuries).
  avail_pmiss <- vapply(enriched_feed$players, function(p) {
    v <- p$availability_p_miss
    if (is.null(v) || is.na(v)) 0 else as.numeric(v)
  }, numeric(1))
  names(avail_pmiss) <- vapply(enriched_feed$players,
                               function(p) p$underdog_id %||% NA_character_,
                               character(1))

  # ID-system bridge: the scraper's player UUIDs come from Underdog's live
  # `/v1/slates/<sid>/players` endpoint; the slate CSV (which seeds the
  # blender feed) uses a *different* Underdog UUID system for the same
  # players. Bridge via normalized (first_name, last_name, position_name).
  # Players with no bridge match are dropped from rosters (with a
  # progress-line tally surfaced when verbose).
  bridge <- .scraper_to_feed_id_map(picks, enriched_feed)
  if (verbose) {
    n_uniq_picks_players <- length(unique(picks$underdog_id))
    cli::cli_alert_info(
      "ID bridge resolved {length(bridge)} of {n_uniq_picks_players} unique scraper player UUIDs to feed entries"
    )
  }

  # Slate lineup spec (Season for now; per-slate when we expand).
  lineup_spec <- load_slate_lineup_spec(slate_ids[1L])

  # Pre-compute marginals once over the union of all roster players in
  # the ready set's pods -- but only over feed-side IDs (the union of
  # scraper IDs after bridging).
  ready_drafts <- unique(ready$draft_id)
  pod_picks <- picks[picks$draft_id %in% ready_drafts, , drop = FALSE]
  pod_picks$feed_uid <- bridge[pod_picks$underdog_id]
  pod_picks <- pod_picks[!is.na(pod_picks$feed_uid), , drop = FALSE]
  union_ids <- unique(pod_picks$feed_uid)
  if (verbose) {
    cli::cli_alert_info(
      "{length(ready_drafts)} distinct pod(s); {length(union_ids)} unique players in scope (post-bridge)"
    )
  }
  ranking_weeks <- as.integer(seq_len(14L))
  marginals <- precompute_layerA_marginals(
    layerA_draws = layerA_draws,
    player_ids   = union_ids,
    weeks        = ranking_weeks
  )

  apples$predicted_xadv      <- NA_real_
  apples$our_team_projection <- NA_real_

  for (i in seq_along(ready_drafts)) {
    d_id <- ready_drafts[i]
    pod_rows <- picks[picks$draft_id == d_id, , drop = FALSE]
    pod_rows$feed_uid <- bridge[pod_rows$underdog_id]
    pod_rows <- pod_rows[!is.na(pod_rows$feed_uid), , drop = FALSE]
    pod_rosters <- split(pod_rows$feed_uid, pod_rows$draft_entry_id)
    pod_rosters <- lapply(pod_rosters, unique)
    # Drop entries with no roster (every player failed to bridge) so we
    # don't crash the kernel; mark them in the report.
    bad <- vapply(pod_rosters, function(r) length(r) == 0L, logical(1))
    if (any(bad)) {
      for (eid in names(pod_rosters)[bad]) {
        idx <- which(apples$underdog_entry_id == eid & apples$draft_id == d_id)
        apples$status[idx] <- "no_feed_overlap"
      }
      pod_rosters <- pod_rosters[!bad]
    }
    if (length(pod_rosters) < 2L) {
      if (verbose) {
        cli::cli_alert_warning("Pod {substr(d_id, 1, 8)} skipped: fewer than 2 bridged rosters.")
      }
      next
    }

    # Resolve the tournament rule for this pod.
    tnmt_uuid <- pod_rows$tournament_id[1L]
    cfg <- by_uuid[[tnmt_uuid]]
    advance_n <- as.integer(cfg$stages[[1L]]$advancement$n)
    # Cap advance_n if fewer than 12 rosters survived bridging.
    advance_n <- min(advance_n, length(pod_rosters) - 1L)

    if (verbose) {
      cli::cli_alert_info(
        "Pod {i}/{length(ready_drafts)} (draft {substr(d_id, 1, 8)}, tournament {cfg$tournament_id}, advance_n={advance_n})"
      )
    }
    res <- predict_pod_xadv(
      pod_rosters           = pod_rosters,
      positions             = positions,
      layerA_draws          = layerA_draws,
      schedule              = schedule,
      lineup_spec           = lineup_spec,
      ranking_weeks         = ranking_weeks,
      advance_n             = advance_n,
      n_sims                = n_sims,
      seed                  = base_seed + i,
      precomputed_marginals = marginals,
      availability_p_miss   = avail_pmiss
    )
    for (eid in names(res$predicted_xadv)) {
      idx <- which(apples$underdog_entry_id == eid & apples$draft_id == d_id)
      if (length(idx) == 1L) {
        apples$predicted_xadv[idx]      <- res$predicted_xadv[[eid]]
        apples$our_team_projection[idx] <-
          mean(res$season_totals[eid, ])
      }
    }
  }

  # Aggregate report
  validated <- apples[!is.na(apples$predicted_xadv), , drop = FALSE]
  validated$abs_err <- abs(validated$predicted_xadv - validated$bbmdb_xadv)
  signed_err <- validated$predicted_xadv - validated$bbmdb_xadv

  spearman <- if (nrow(validated) >= 3L) {
    suppressWarnings(stats::cor(validated$predicted_xadv,
                                validated$bbmdb_xadv,
                                method = "spearman"))
  } else NA_real_

  aggregates <- list(
    n_validated       = nrow(validated),
    spearman          = spearman,
    mae               = if (nrow(validated) > 0L) mean(validated$abs_err) else NA_real_,
    mse               = if (nrow(validated) > 0L) mean(signed_err^2) else NA_real_,
    mean_signed_error = if (nrow(validated) > 0L) mean(signed_err) else NA_real_
  )

  proj_xcheck <- if (nrow(validated) >= 3L) {
    list(
      pearson = suppressWarnings(stats::cor(validated$our_team_projection,
                                            validated$bbmdb_team_projection)),
      n       = nrow(validated),
      our_range   = range(validated$our_team_projection),
      bbmdb_range = range(validated$bbmdb_team_projection)
    )
  } else list(pearson = NA_real_, n = nrow(validated))

  out_cols <- c("underdog_entry_id", "draft_id", "tournament_name",
                "tournament_id", "advance_n", "bbmdb_xadv",
                "predicted_xadv", "abs_err",
                "our_team_projection", "bbmdb_team_projection", "status")
  per_team <- apples
  per_team$abs_err <- ifelse(
    is.na(per_team$predicted_xadv), NA_real_,
    abs(per_team$predicted_xadv - per_team$bbmdb_xadv)
  )
  per_team <- per_team[, intersect(out_cols, names(per_team)), drop = FALSE]
  names(per_team)[names(per_team) == "underdog_entry_id"] <- "entry_id"
  per_team <- per_team[order(per_team$tournament_id, per_team$entry_id), ]

  result <- list(
    per_team          = per_team,
    aggregates        = aggregates,
    projection_xcheck = proj_xcheck
  )

  if (!is.null(report_path)) {
    .write_validation_report(result, report_path,
                              n_sims = n_sims, base_seed = base_seed)
  }
  if (verbose) .print_validation_summary(result)
  result
}

# ---- helpers ---------------------------------------------------------------

#' Build a bridge map: scraper `underdog_id` -> feed-side `underdog_id`
#'
#' Underdog exposes two distinct UUIDs for the same player: the slate
#' CSV (used by the blender feed) uses one; the live
#' `/v1/slates/<sid>/players` endpoint (used by the scraper) uses
#' another. The bridge keys on [normalize_player_key()] -- the
#' centralized normalizer handles generational suffixes (`Jr` / `III`),
#' multi-token last names (`St. Brown`), unicode apostrophes /
#' hyphens, and the CB->WR fantasy-position remap that turns Travis
#' Hunter from "scraper-says-defender" into the WR slot he actually
#' starts at. Ambiguous matches (same key across multiple feed
#' candidates) are dropped; the validator marks affected entries as
#' `no_feed_overlap` if too few players make it through.
#' @keywords internal
.scraper_to_feed_id_map <- function(picks, feed) {
  feed_ids   <- names(feed$players)
  feed_names <- vapply(feed_ids,
                       function(uid) feed$players[[uid]]$name %||% NA_character_,
                       character(1))
  feed_pos   <- vapply(feed_ids,
                       function(uid) feed$players[[uid]]$position %||% NA_character_,
                       character(1))
  feed_keys  <- normalize_player_key(feed_names, feed_pos)
  feed_index <- split(feed_ids, feed_keys)

  uniq_picks <- unique(picks[, c("underdog_id", "first_name", "last_name",
                                  "position_name")])
  uniq_picks <- uniq_picks[!is.na(uniq_picks$underdog_id) &
                            !is.na(uniq_picks$first_name) &
                            !is.na(uniq_picks$last_name) &
                            !is.na(uniq_picks$position_name), ,
                           drop = FALSE]
  pick_keys <- normalize_player_key(
    paste(uniq_picks$first_name, uniq_picks$last_name),
    uniq_picks$position_name
  )

  bridge <- character(0)
  for (i in seq_along(pick_keys)) {
    k <- pick_keys[i]
    if (is.na(k)) next
    cand <- feed_index[[k]]
    if (!is.null(cand) && length(cand) == 1L) {
      bridge[[uniq_picks$underdog_id[i]]] <- cand
    }
  }
  bridge
}

#' @keywords internal
identical_post <- function(corpus) {
  vapply(corpus, function(x) identical(as.character(x), "post_nfl_draft"),
         logical(1))
}

#' @keywords internal
.positions_from_feed <- function(feed) {
  players <- feed$players
  ids <- names(players)
  pos <- vapply(players, function(p) p$position %||% NA_character_,
                character(1))
  names(pos) <- ids
  pos[!is.na(pos)]
}

#' @keywords internal
.schedule_from_feed <- function(feed) {
  players <- feed$players
  chunks <- list()
  for (pid in names(players)) {
    p <- players[[pid]]
    team <- p$team %||% NA_character_
    weekly <- p$weekly %||% list()
    if (length(weekly) == 0L) next
    for (wk in weekly) {
      w <- as.integer(wk$week %||% NA_integer_)
      if (is.na(w)) next
      is_bye <- isTRUE(wk$is_bye)
      chunks[[length(chunks) + 1L]] <- data.frame(
        underdog_id = pid,
        week        = w,
        team        = team,
        opponent    = if (is_bye) NA_character_ else (wk$opponent %||% NA_character_),
        is_bye      = is_bye,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, chunks)
}

#' @keywords internal
.print_validation_summary <- function(result) {
  agg <- result$aggregates
  xcheck <- result$projection_xcheck
  cli::cli_h2("BBMDB validation summary (simulator-vs-simulator)")
  cli::cli_alert_info("Validated teams: {agg$n_validated}")
  cli::cli_alert_info("Spearman(predicted, BBMDB) = {round(agg$spearman, 3)}")
  cli::cli_alert_info("MAE = {round(agg$mae, 3)}  | mean signed error = {round(agg$mean_signed_error, 3)}")
  cli::cli_alert_info("Projection cross-check: Pearson(ours, BBMDB) = {round(xcheck$pearson, 3)} on n = {xcheck$n}")
}

#' @keywords internal
.write_validation_report <- function(result, report_path, n_sims, base_seed) {
  dir.create(dirname(report_path), recursive = TRUE, showWarnings = FALSE)
  agg <- result$aggregates
  xcheck <- result$projection_xcheck
  csv_path <- sub("\\.md$", ".csv", report_path)
  utils::write.csv(result$per_team, csv_path, row.names = FALSE)

  lines <- c(
    "# BBMDB xAdv validation",
    "",
    "> **Simulator-vs-simulator.** BBMDB is itself an assumption-driven",
    "> Monte Carlo. Agreement here validates *shared* assumptions",
    "> (correlation form, position-pair structure, optimizer math) --",
    "> it is **not** ground truth. Real-outcome validation requires a",
    "> full played season.",
    "",
    sprintf("- generated_at: `%s`", format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
    sprintf("- n_sims:        `%d`", as.integer(n_sims)),
    sprintf("- base_seed:     `%d`", as.integer(base_seed)),
    sprintf("- per_team CSV:  `%s`", basename(csv_path)),
    "",
    "## Aggregate metrics",
    "",
    sprintf("- Validated teams:        **%d**", agg$n_validated),
    sprintf("- Spearman rank corr.:    **%.3f**", agg$spearman),
    sprintf("- MAE:                    **%.3f**", agg$mae),
    sprintf("- Mean signed error:      **%+.3f**", agg$mean_signed_error),
    sprintf("- Projection Pearson:     **%.3f** (n = %d)",
            xcheck$pearson, xcheck$n),
    "",
    "## Caveats specific to this corpus",
    "",
    "- N is small (~20). No bucketed calibration; results are directional.",
    "- BBMDB does not expose post-draft per-player projections, so the",
    "  projection cross-check is only a partial way to disentangle",
    "  projections-vs-math; a divergence there localizes the gap, but",
    "  agreement there does not rule out projection-driven xAdv error.",
    "- Teams whose tournament lacks a canonical config (currently the",
    "  Frenchie Eliminator and Field General entries) are skipped and",
    "  reported as `no_config` / `out_of_scope_slate`."
  )
  writeLines(lines, report_path)
}
