#' Config-driven multi-stage tournament EV engine
#'
#' Generalizes the BBM7 vertical (#16/#18) so that **all** tournament
#' structure -- stage count, pod sizes, advance counts, payout heads and
#' tables, the finalist denominator, total entries, entry fee -- is read
#' from `tournament_cfg`. BBM7 and the other Season-slate tournaments
#' (Puppy, Dachshund, ...) differ only in those config values, never in
#' code. `R/tournament_ev_bbm7.R` keeps the `*_bbm7*` names as thin
#' wrappers for back-compatibility.
#'
#' Shape handled: a qualifier stage scored on a cumulative week window,
#' followed by single-week elimination rounds, ending in a single-pod
#' final ranked outright -- the Season-slate format every in-scope
#' tournament uses. The engine loops over `stages` rather than hardcoding
#' four of them, and derives every bucket size / tier width from config,
#' so a tournament with a different number of rounds or a single payout
#' head works without code changes. The 32-bit-overflow-safe
#' `as.numeric()` casts from #18 are retained in [.range_avg_payout()].

# ---- field-size guardrail ---------------------------------------------------

#' Money-conservation-valid synthetic-field base for any tournament
#'
#' The field-mean EV from [build_field_payouts()] equals the prize pool
#' only when the synthetic field is an exact multiple of `total_field_size`
#' divided by the final-stage seat count -- the number of full-field
#' entries each finalist represents (BBM7: 672,336 / 667 = 1,008; Puppy:
#' 225,000 / 625 = 360). Derived from config, never hardcoded.
#'
#' @param tournament_cfg Parsed config from [load_tournament()].
#' @return Integer base; valid field sizes are positive multiples of it.
#' @export
tournament_field_multiple <- function(tournament_cfg) {
  full_field <- as.integer(tournament_cfg$total_field_size %||% NA_integer_)
  stages <- tournament_cfg$stages
  final_seats <- if (length(stages) > 0L) {
    as.integer(stages[[length(stages)]]$seats_entering %||% NA_integer_)
  } else NA_integer_
  if (is.na(full_field) || is.na(final_seats) || final_seats <= 0L) {
    cli::cli_abort(
      "Cannot derive field multiple: config missing `total_field_size` or final-stage `seats_entering`.")
  }
  if (full_field %% final_seats != 0L) {
    cli::cli_abort(c(
      "`total_field_size` ({full_field}) is not an exact multiple of the \\
       final-stage seat count ({final_seats}).",
      i = "The money-conservation guarantee needs a clean per-finalist field share."
    ))
  }
  as.integer(full_field / final_seats)
}

#' Snap a requested synthetic-field size to a money-conservation-valid one
#'
#' @param tournament_cfg Parsed config from [load_tournament()].
#' @param n_field Requested field size.
#' @param snap If `TRUE` (default) returns the nearest positive multiple of
#'   [tournament_field_multiple()]; if `FALSE`, errors when `n_field` is not
#'   already a valid multiple, naming the nearby valid sizes.
#' @return Integer valid field size.
#' @export
resolve_tournament_field_size <- function(tournament_cfg, n_field, snap = TRUE) {
  base <- tournament_field_multiple(tournament_cfg)
  n_field <- as.integer(n_field)
  if (!is.na(n_field) && n_field > 0L && n_field %% base == 0L) {
    return(n_field)
  }
  if (!isTRUE(snap)) {
    lo <- max(base, (as.integer(n_field) %/% base) * base)
    hi <- lo + base
    cli::cli_abort(c(
      "n_field = {n_field} is not a multiple of {base} (the money-conservation base).",
      i = "Nearest valid sizes: {lo} or {hi}. Pass a multiple of {base} \\
           (e.g. {base}, {2L * base}, {10L * base})."
    ))
  }
  mult <- max(1L, as.integer(round(as.numeric(n_field) / base)))
  mult * base
}

# ---- per-stage scores (config-driven week windows) --------------------------

#' Resolve each stage's week vector from config (Season default if NULL).
#' @keywords internal
.resolve_stage_weeks <- function(tournament_cfg) {
  if (is.null(tournament_cfg) || is.null(tournament_cfg$stages)) {
    return(list(`1` = 1:14, `2` = 15L, `3` = 16L, `4` = 17L))
  }
  stages <- tournament_cfg$stages
  out <- lapply(stages, function(s) as.integer(s$weeks))
  names(out) <- as.character(seq_along(stages))
  out
}

#' Simulate per-stage scores for a population of rosters
#'
#' One joint correlated draw over the union of all `rosters` players (so
#' cross-roster NFL-game/team correlation is preserved within each sim),
#' each roster optimized via the 3b-3 lineup kernel, then aggregated into
#' one metric per stage: the cumulative optimal-lineup points over that
#' stage's `weeks`. The stage week windows are read from `tournament_cfg`
#' (defaulting to the Season layout 1-14 / 15 / 16 / 17 when `NULL`).
#'
#' @param rosters Named list `entry_id -> chr underdog_id`.
#' @param positions Named character vector `underdog_id -> position`.
#' @param layerA_draws Long Layer A draws data.frame.
#' @param schedule Per-player-per-week schedule data.frame.
#' @param lineup_spec Slate lineup spec (from [load_slate_lineup_spec()]).
#' @param tournament_cfg Optional parsed config; supplies the per-stage
#'   week windows. `NULL` uses the Season default.
#' @param corr_params Correlation parameters; default [default_corr_params].
#' @param n_sims Output sim count (default 10000).
#' @param seed Optional integer seed.
#' @param precomputed_marginals Optional shared marginal cache.
#' @param contrib_entry_id Optional entry_id; when set, per-player optimal-
#'   lineup contributions for that team are computed from the same joint
#'   draw and returned in `contrib` (head-5 attribution).
#' @param availability_p_miss Optional named numeric vector
#'   (`underdog_id -> p_miss`). When supplied, the draw-level availability mask
#'   (`q = 1 - p_miss`) is applied post-inverse-CDF via [.mask_matrix_list()] --
#'   the same zeroing the tensor build applies -- so curves are calibrated
#'   against an attrition-priced field. `NULL` (default) = no mask.
#' @return Named list with:
#'   \itemize{
#'     \item `stage_scores` -- named list (`"1"`..`"N"`) of
#'       `[n_teams x n_sims]` matrices, one per stage.
#'     \item `q_cum`, `w15`, `w16`, `w17` -- back-compat aliases for the
#'       Season layout (`stage_scores[["1"/"2"/"3"/"4"]]`) when present.
#'     \item `contrib` -- when requested, `list(stage = named list of
#'       `[n_team_players x n_sims]` per-stage contribution matrices)`;
#'       else `NULL`.
#'   }
#' @export
simulate_per_stage_scores <- function(rosters,
                                      positions,
                                      layerA_draws,
                                      schedule,
                                      lineup_spec,
                                      tournament_cfg = NULL,
                                      corr_params = default_corr_params,
                                      n_sims = 10000L,
                                      seed = NULL,
                                      precomputed_marginals = NULL,
                                      contrib_entry_id = NULL,
                                      availability_p_miss = NULL) {
  if (!is.list(rosters) || length(rosters) == 0L) {
    cli::cli_abort("`rosters` must be a non-empty list of entry_id -> roster.")
  }
  stage_weeks <- .resolve_stage_weeks(tournament_cfg)
  n_stage <- length(stage_weeks)
  union_ids <- unique(unlist(rosters, use.names = FALSE))

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

  # Draw-level availability: mask the field/pod's conditional draws so the
  # tournament curves are calibrated against an attrition-priced field -- the
  # SAME mask the tensor build applies (else the field scores ~1/q hot and the
  # advancement/payout curves under-price the user's masked roster). Default
  # NULL -> no mask (mechanics tests / fixtures unaffected). See
  # DRAW_ZEROING_DESIGN.md and [.mask_matrix_list()].
  if (!is.null(availability_p_miss)) {
    q <- 1 - availability_p_miss[union_ids]
    q[is.na(q)] <- 1
    names(q) <- union_ids
    ml <- .mask_matrix_list(ml, q, seed)
  }

  n_teams <- length(rosters)
  stage_scores <- lapply(seq_len(n_stage), function(.) {
    matrix(0, nrow = n_teams, ncol = n_sims, dimnames = list(names(rosters), NULL))
  })
  names(stage_scores) <- as.character(seq_len(n_stage))

  contrib_target <- if (!is.null(contrib_entry_id)) {
    idx <- match(contrib_entry_id, names(rosters))
    if (is.na(idx)) {
      cli::cli_abort("`contrib_entry_id` {.val {contrib_entry_id}} not found in `rosters`.")
    }
    idx
  } else NA_integer_
  contrib_out <- NULL

  for (i in seq_len(n_teams)) {
    team_pids <- intersect(rosters[[i]], union_ids)
    team_pos  <- positions[team_pids]
    team_pos  <- team_pos[!is.na(team_pos)]
    team_pids <- intersect(team_pids, names(team_pos))
    if (length(team_pids) == 0L) next
    team_ml <- lapply(ml, function(M) {
      keep <- intersect(rownames(M), team_pids)
      M[keep, , drop = FALSE]
    })
    wt <- optimize_lineup_totals(
      scores      = team_ml,
      positions   = team_pos,
      lineup_spec = lineup_spec
    )
    week_cols <- colnames(wt)
    for (k in seq_len(n_stage)) {
      cols <- intersect(as.character(stage_weeks[[k]]), week_cols)
      if (length(cols) > 0L) {
        stage_scores[[k]][i, ] <- rowSums(wt[, cols, drop = FALSE])
      }
    }

    if (!is.na(contrib_target) && i == contrib_target) {
      cw <- optimize_lineup_contributions(
        scores      = team_ml,
        positions   = team_pos,
        lineup_spec = lineup_spec
      )
      stage_contrib <- lapply(seq_len(n_stage), function(.) {
        matrix(0, length(team_pids), n_sims, dimnames = list(team_pids, NULL))
      })
      names(stage_contrib) <- as.character(seq_len(n_stage))
      for (wk in names(cw)) {
        Cm <- cw[[wk]]; rws <- rownames(Cm); wnum <- as.integer(wk)
        for (k in seq_len(n_stage)) {
          if (wnum %in% stage_weeks[[k]]) {
            stage_contrib[[k]][rws, ] <- stage_contrib[[k]][rws, ] + Cm
          }
        }
      }
      contrib_out <- list(stage = stage_contrib)
    }
  }

  out <- list(stage_scores = stage_scores, contrib = contrib_out)
  # Back-compat aliases for the Season layout.
  out$q_cum <- stage_scores[["1"]]
  if (!is.null(stage_scores[["2"]])) out$w15 <- stage_scores[["2"]]
  if (!is.null(stage_scores[["3"]])) out$w16 <- stage_scores[["3"]]
  if (!is.null(stage_scores[["4"]])) out$w17 <- stage_scores[["4"]]
  out
}

# ---- stage-structure readers ------------------------------------------------

#' @keywords internal
.req_int <- function(x, what) {
  v <- suppressWarnings(as.integer(x))
  if (length(v) != 1L || is.na(v)) {
    cli::cli_abort("Config is missing a required integer: {what}.")
  }
  v
}

#' Read the structural parameters the EV engine needs from a config.
#' @keywords internal
.stage_structure <- function(tournament_cfg) {
  stages <- tournament_cfg$stages
  n_stage <- length(stages)
  if (n_stage < 2L) {
    cli::cli_abort("Tournament needs >= 2 stages (a qualifier and a final).")
  }
  pod   <- vapply(seq_len(n_stage), function(k)
    .req_int(stages[[k]]$pod_size, sprintf("stages[[%d]]$pod_size", k)), integer(1))
  seats <- vapply(seq_len(n_stage), function(k)
    .req_int(stages[[k]]$seats_entering, sprintf("stages[[%d]]$seats_entering", k)),
    integer(1))
  # advance count per stage (final has none -> NA).
  advn <- vapply(seq_len(n_stage), function(k) {
    if (k == n_stage) return(NA_integer_)
    .req_int(stages[[k]]$advancement$n, sprintf("stages[[%d]]$advancement$n", k))
  }, integer(1))
  full_field <- .req_int(tournament_cfg$total_field_size, "total_field_size")
  list(n_stage = n_stage, pod = pod, seats = seats, advn = advn,
       full_field = full_field)
}

#' Championship-bucket eligibility tags for a loser/finalist bucket, by
#' depth from the final. Standard season-long vocabulary.
#' @keywords internal
.champ_tags <- function(k, n_stage) {
  if (k == n_stage) return("finalist")
  depth <- n_stage - k                     # 1 = lost in the round before final
  primary <- switch(as.character(depth),
    "1" = "semifinals_loser",
    "2" = c("quarterfinals_loser", "quarterfinals_loser_lower"),
    cli::cli_abort(c(
      "No standard championship payout tag for a stage {depth} rounds before the final.",
      i = "The season-long vocabulary covers finalist / semifinals_loser / \\
           quarterfinals_loser; extend it for deeper brackets."
    )))
  primary
}

# ---- field payouts (money conservation + advancer pools) --------------------

#' Compute payouts for every (team, sim) of a field-sample run, generically
#'
#' Config-driven generalization of `build_bbm7_field_payouts`. For each
#' sim: qualifier-round payout per team by field rank on stage-1 metric
#' (if the `qualifier_round` head exists); then pod advancement through
#' every stage; then championship payout per progression bucket (if the
#' `championship_round` head exists). Tier ranks scale from the sample
#' size to `total_field_size`, so the field-mean converges to
#' `prize_pool / total_field_size` (money conservation).
#'
#' @param scores Output of [simulate_per_stage_scores()].
#' @param tournament_cfg Parsed config from [load_tournament()].
#' @param seed Optional integer seed for pod assignment.
#' @return List with `per_team_ev` (data.frame), `field_mean_total_ev`,
#'   and `pools` (empirical distributions for the team-EV evaluator:
#'   `q_cum_pool` and `stage_entrant_metric[[k]]` for k = 2..N).
#' @export
build_field_payouts <- function(scores, tournament_cfg, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  ss <- scores$stage_scores
  if (is.null(ss)) {
    # Accept legacy Season-layout scores {q_cum, w15, w16, w17}.
    ss <- list()
    if (!is.null(scores$q_cum)) ss[["1"]] <- scores$q_cum
    if (!is.null(scores$w15))   ss[["2"]] <- scores$w15
    if (!is.null(scores$w16))   ss[["3"]] <- scores$w16
    if (!is.null(scores$w17))   ss[["4"]] <- scores$w17
    if (length(ss) == 0L) {
      cli::cli_abort("`scores` must come from simulate_per_stage_scores().")
    }
  }
  st <- .stage_structure(tournament_cfg)
  n_stage <- st$n_stage
  if (length(ss) != n_stage) {
    cli::cli_abort(c(
      "scores have {length(ss)} stages but config has {n_stage}.",
      i = "Score the field with the same tournament_cfg."))
  }
  M <- ss                                   # metric per stage (list of matrices)
  n_teams <- nrow(M[[1L]]); n_sims <- ncol(M[[1L]])
  entry_ids <- rownames(M[[1L]]) %||% as.character(seq_len(n_teams))

  base <- tournament_field_multiple(tournament_cfg)
  if (n_teams %% base != 0L) {
    lo <- max(base, (n_teams %/% base) * base); hi <- lo + base
    cli::cli_abort(c(
      "Field sample has {n_teams} teams, not a multiple of {base}.",
      x = "Money conservation requires a field that is a multiple of {base} \\
           ({.code total_field_size / final-stage seats}).",
      i = "Regenerate the field at {lo} or {hi} (see {.fn resolve_tournament_field_size})."
    ))
  }

  q_tiers <- tournament_cfg$payouts$qualifier_round$tiers
  c_tiers <- tournament_cfg$payouts$championship_round$tiers
  has_q <- !is.null(q_tiers); has_c <- !is.null(c_tiers)

  qualifier_pay_per <- matrix(0, n_teams, n_sims, dimnames = list(entry_ids, NULL))
  champ_pay_per     <- matrix(0, n_teams, n_sims, dimnames = list(entry_ids, NULL))

  # Pre-build per-bucket cap tables.
  qual_caps <- if (has_q) {
    .tier_caps_from_yaml(q_tiers, eligibility = NULL, total_slots = st$full_field)
  } else NULL
  champ_caps <- if (has_c) {
    caps <- vector("list", n_stage)
    caps[[n_stage]] <- .tier_caps_from_yaml(
      c_tiers, eligibility = "finalist", total_slots = st$seats[n_stage])
    for (k in seq.int(n_stage - 1L, 2L)) {
      bucket_size <- st$seats[k] - st$seats[k + 1L]
      caps[[k]] <- .tier_caps_combined(
        c_tiers, primary_eligibility = .champ_tags(k, n_stage),
        fallback_eligibility = "qualifier_advancer",
        total_slots = bucket_size)
    }
    caps
  } else NULL

  # Empirical pools for the team-EV evaluator.
  entrant_pool <- vector("list", n_stage)   # [[k]] = stage-k metric of stage-k entrants

  for (j in seq_len(n_sims)) {
    Mj <- lapply(M, function(mm) mm[, j])

    # ---- qualifier-round payout (field rank on stage-1 metric) ----
    if (has_q) {
      rk_q <- rank(-Mj[[1L]], ties.method = "first")
      qualifier_pay_per[, j] <- vapply(rk_q, function(k)
        .range_avg_payout(k, n_teams, st$full_field, qual_caps), numeric(1))
    }

    # ---- pod advancement through all stages ----
    entering <- vector("list", n_stage)
    entering[[1L]] <- seq_len(n_teams)
    for (k in seq_len(n_stage - 1L)) {
      idx <- entering[[k]]
      pod_k <- st$pod[k]; adv_k <- st$advn[k]
      shuf <- if (k == 1L) sample.int(n_teams, n_teams) else sample(idx)
      winners <- integer(0)
      for (s in seq.int(1L, length(shuf) - pod_k + 1L, by = pod_k)) {
        pod_idx <- shuf[s:(s + pod_k - 1L)]
        top <- pod_idx[order(-Mj[[k]][pod_idx])][seq_len(adv_k)]
        winners <- c(winners, top)
      }
      entering[[k + 1L]] <- winners
    }
    for (k in seq_len(n_stage)) {
      entrant_pool[[k]] <- c(entrant_pool[[k]], Mj[[k]][entering[[k]]])
    }

    # ---- championship payouts per progression bucket ----
    if (has_c) {
      # finalists (stage n), ranked by stage-n metric.
      fin <- entering[[n_stage]]
      if (length(fin) > 0L) {
        ford <- fin[order(-Mj[[n_stage]][fin])]
        for (r in seq_along(ford)) {
          champ_pay_per[ford[r], j] <-
            .range_avg_payout(r, length(ford), st$seats[n_stage], champ_caps[[n_stage]])
        }
      }
      # losers at each elimination stage k = n-1 .. 2, ranked by stage-k metric.
      for (k in seq.int(n_stage - 1L, 2L)) {
        losers <- setdiff(entering[[k]], entering[[k + 1L]])
        if (length(losers) == 0L) next
        lord <- losers[order(-Mj[[k]][losers])]
        bucket_size <- st$seats[k] - st$seats[k + 1L]
        for (r in seq_along(lord)) {
          champ_pay_per[lord[r], j] <-
            .range_avg_payout(r, length(lord), bucket_size, champ_caps[[k]])
        }
      }
    }
  }

  per_team_q_ev <- rowMeans(qualifier_pay_per)
  per_team_c_ev <- rowMeans(champ_pay_per)
  per_team_total_ev <- per_team_q_ev + per_team_c_ev
  per_team_ev <- data.frame(
    entry_id = entry_ids, qualifier_round_ev = per_team_q_ev,
    championship_round_ev = per_team_c_ev, total_ev = per_team_total_ev,
    stringsAsFactors = FALSE)

  pools <- list(q_cum_pool = as.numeric(M[[1L]]),
                stage_entrant_metric = entrant_pool)
  # Back-compat alias names used by the BBM7 evaluator.
  if (n_stage >= 2L) pools$qualifier_advancer_w15 <- entrant_pool[[2L]]
  if (n_stage >= 3L) pools$qf_advancer_w16        <- entrant_pool[[3L]]
  if (n_stage >= 4L) pools$sf_advancer_w17        <- entrant_pool[[4L]]

  list(per_team_ev = per_team_ev,
       field_mean_total_ev = mean(per_team_total_ev),
       pools = pools)
}

# ---- single-team EV + per-player attribution --------------------------------

#' Compute expected value for one team, generically
#'
#' Config-driven generalization of `compute_team_bbm7_ev`. Per-team
#' qualifier advance probability, qualifier-round expected payout (field
#' percentile from `field_cache$pools`), championship expected payout
#' (forward-simulate every elimination stage against advancer-conditional
#' opponent pools), and additive per-player EV attribution.
#'
#' @inheritParams build_field_payouts
#' @param pod_rosters Named list (entry_id -> chr underdog_ids).
#' @param team_entry_id The entry_id within `pod_rosters` to evaluate.
#' @param positions,layerA_draws,schedule,lineup_spec See
#'   [simulate_per_stage_scores()].
#' @param field_cache Output of [build_field_payouts()].
#' @param corr_params,n_sims,seed,availability_p_miss See
#'   [simulate_per_stage_scores()].
#' @return List with `team_ev`, `per_player_ev`, `advance_probs`, `per_sim`.
#' @export
compute_team_ev <- function(pod_rosters, team_entry_id, positions,
                            layerA_draws, schedule, lineup_spec,
                            tournament_cfg, field_cache,
                            corr_params = default_corr_params,
                            n_sims = 10000L, seed = NULL,
                            availability_p_miss = NULL) {
  if (!(team_entry_id %in% names(pod_rosters))) {
    cli::cli_abort("`team_entry_id` {.val {team_entry_id}} not found in `pod_rosters`.")
  }
  st <- .stage_structure(tournament_cfg)
  n_stage <- st$n_stage

  pod_scores <- simulate_per_stage_scores(
    rosters = pod_rosters, positions = positions, layerA_draws = layerA_draws,
    schedule = schedule, lineup_spec = lineup_spec, tournament_cfg = tournament_cfg,
    corr_params = corr_params, n_sims = n_sims, seed = seed,
    contrib_entry_id = team_entry_id, availability_p_miss = availability_p_miss)
  M <- lapply(pod_scores$stage_scores, function(mm) mm[team_entry_id, ])

  # ---- qualifier-round payout via field-rank percentile ----
  q_tiers <- tournament_cfg$payouts$qualifier_round$tiers
  pool_q  <- field_cache$pools$q_cum_pool
  if (!is.null(q_tiers) && !is.null(pool_q)) {
    q_pct  <- vapply(M[[1L]], function(x) mean(pool_q >= x), numeric(1))
    rk_full <- pmax(1L, as.integer(ceiling(q_pct * st$full_field)))
    q_pay  <- .payout_lookup(rk_full, q_tiers, eligibility = NULL)
  } else {
    q_pay <- numeric(n_sims)
  }

  # ---- qualifier advance: top-advn[1] of the pod by stage-1 metric ----
  pod_q <- pod_scores$stage_scores[[1L]]
  q_ranks_in_pod <- apply(pod_q, 2L, function(x) rank(-x, ties.method = "first"))
  advance_q <- q_ranks_in_pod[team_entry_id, ] <= st$advn[1L]

  # ---- championship bracket per sim (forward-sim each elimination stage) ----
  c_tiers <- tournament_cfg$payouts$championship_round$tiers
  has_c <- !is.null(c_tiers)
  entrant_pool <- field_cache$pools$stage_entrant_metric
  if (is.null(entrant_pool)) {
    # Legacy alias pools (qualifier_advancer_w15 / qf_advancer_w16 / sf_advancer_w17).
    entrant_pool <- vector("list", n_stage)
    if (n_stage >= 2L) entrant_pool[[2L]] <- field_cache$pools$qualifier_advancer_w15
    if (n_stage >= 3L) entrant_pool[[3L]] <- field_cache$pools$qf_advancer_w16
    if (n_stage >= 4L) entrant_pool[[4L]] <- field_cache$pools$sf_advancer_w17
  }
  c_pay <- numeric(n_sims)
  depth <- rep(1L, n_sims)                  # 1 = qualifier loser
  for (j in seq_len(n_sims)) {
    if (!advance_q[j]) next
    reached <- 2L                           # entered stage 2
    lost_at <- NA_integer_
    for (k in seq.int(2L, n_stage - 1L)) {
      opp <- sample(entrant_pool[[k]], st$pod[k] - 1L, replace = TRUE)
      if (M[[k]][j] <= max(opp)) { lost_at <- k; break }
      reached <- k + 1L
    }
    if (is.na(lost_at)) {
      depth[j] <- n_stage                   # finalist
      if (has_c) {
        fin_rank <- max(1L, as.integer(
          ceiling(mean(entrant_pool[[n_stage]] >= M[[n_stage]][j]) * st$seats[n_stage])))
        c_pay[j] <- .payout_lookup(fin_rank, c_tiers, eligibility = "finalist")
      }
    } else {
      depth[j] <- lost_at
      if (has_c) {
        bucket_size <- st$seats[lost_at] - st$seats[lost_at + 1L]
        within <- max(1L, as.integer(
          ceiling(mean(entrant_pool[[lost_at]] >= M[[lost_at]][j]) * bucket_size)))
        rk <- st$seats[lost_at + 1L] + within
        c_pay[j] <- .payout_lookup(rk, c_tiers,
          eligibility = c(.champ_tags(lost_at, n_stage), "qualifier_advancer"))
      }
    }
  }

  per_player_ev <- .attribute_player_ev(
    contrib = pod_scores$contrib, q_pay = q_pay, c_pay = c_pay, depth = depth)

  per_sim <- data.frame(sim_idx = seq_len(n_sims),
                        q_total = M[[1L]],
                        qualifier_round_pay = q_pay, champ_pay = c_pay,
                        depth = depth, stringsAsFactors = FALSE)
  for (k in seq_len(n_stage)) per_sim[[paste0("stage", k)]] <- M[[k]]

  list(
    team_ev = list(qualifier_round_ev = mean(q_pay),
                   championship_round_ev = mean(c_pay),
                   total_ev = mean(q_pay) + mean(c_pay)),
    per_player_ev = per_player_ev,
    advance_probs = list(
      qualifier = mean(advance_q),
      by_stage  = vapply(seq_len(n_stage), function(k) mean(depth >= k), numeric(1))),
    per_sim = per_sim)
}

# ---- per-player attribution (head 5) ----------------------------------------

#' Additively attribute a team's realized per-sim winnings across its
#' roster from per-stage optimal-lineup contributions.
#'
#' Qualifier-round payout splits across the stage-1 (qualifier-window)
#' contributions; championship payout splits across the progression-path
#' stages the team actually played (stages 2..`depth`). Splitting each
#' payout in proportion to a player's share of the contributing points
#' makes the per-player EVs sum to the team EV exactly.
#' @keywords internal
.attribute_player_ev <- function(contrib, q_pay, c_pay, depth) {
  if (is.null(contrib) || is.null(contrib$stage)) {
    cli::cli_abort(
      "Per-player contributions missing; call simulate_per_stage_scores(contrib_entry_id=).")
  }
  stage_c <- contrib$stage
  players <- rownames(stage_c[["1"]])
  n_sims  <- length(q_pay)

  # qualifier round: split across stage-1 contributions.
  q_c <- stage_c[["1"]]
  denom_q <- colSums(q_c)
  q_attr <- sweep(q_c, 2L, ifelse(denom_q > 0, q_pay / denom_q, 0), `*`)

  # championship: split across stages 2..depth[j].
  champ_c <- matrix(0, nrow = length(players), ncol = n_sims,
                    dimnames = list(players, NULL))
  n_stage <- length(stage_c)
  for (d in 2:n_stage) {
    sel <- depth == d
    if (!any(sel)) next
    acc <- matrix(0, length(players), sum(sel))
    for (k in 2:d) acc <- acc + stage_c[[as.character(k)]][, sel, drop = FALSE]
    champ_c[, sel] <- acc
  }
  denom_c <- colSums(champ_c)
  c_attr <- sweep(champ_c, 2L, ifelse(denom_c > 0, c_pay / denom_c, 0), `*`)

  player_q_ev <- rowMeans(q_attr)
  player_c_ev <- rowMeans(c_attr)
  out <- data.frame(
    underdog_id = players, qualifier_round_ev = player_q_ev,
    championship_round_ev = player_c_ev, total_ev = player_q_ev + player_c_ev,
    row.names = NULL, stringsAsFactors = FALSE)
  out <- out[order(-out$total_ev), , drop = FALSE]
  rownames(out) <- NULL
  attr(out, "metric") <- paste(
    "additive EV attribution (realized value flow);",
    "NOT marginal / value-over-replacement -- not 'EV lost if dropped'")
  out
}

# ---- tier-cap helpers -------------------------------------------------------

#' Build a tier-cap table from a YAML tiers list filtered by eligibility.
#' Returns `{n_slots, usd}` segments in rank order; pads to `total_slots`
#' with a $0 catch-all. With `anchor_to_first = TRUE` the table starts at
#' the first matching tier's `rank_from` instead of rank 1 -- used for
#' loser buckets, whose tiers carry global standings ranks but whose caps
#' are indexed bucket-locally (slot 1 = best team in the bucket).
#' @keywords internal
.tier_caps_from_yaml <- function(tiers, eligibility = NULL, total_slots = NULL,
                                 anchor_to_first = FALSE) {
  rows <- list()
  for (t in tiers) {
    if (!is.null(eligibility)) {
      etag <- t$eligibility %||% NA_character_
      if (!isTRUE(etag %in% eligibility)) next
    }
    rows[[length(rows) + 1L]] <- list(
      rank_from = as.integer(t$rank_from), rank_to = as.integer(t$rank_to),
      usd = as.numeric(t$usd))
  }
  rows <- rows[order(vapply(rows, `[[`, integer(1), "rank_from"))]
  offset <- if (isTRUE(anchor_to_first) && length(rows) > 0L) {
    rows[[1L]]$rank_from - 1L
  } else 0L
  caps <- list(); cursor <- 1L
  for (r in rows) {
    rf <- r$rank_from - offset; rt <- r$rank_to - offset
    if (rf > cursor) {
      caps[[length(caps) + 1L]] <- list(n_slots = rf - cursor, usd = 0)
    }
    caps[[length(caps) + 1L]] <- list(n_slots = rt - rf + 1L, usd = r$usd)
    cursor <- rt + 1L
  }
  if (!is.null(total_slots) && cursor <= total_slots) {
    caps[[length(caps) + 1L]] <- list(n_slots = total_slots - cursor + 1L, usd = 0)
  }
  caps
}

#' Build tier caps for a loser bucket: the primary-eligibility tiers get
#' their $$ for as many slots as they natively span; the rest of the
#' bucket falls back to the `fallback_eligibility` tier's $$. `primary_slots`
#' is derived from the primary tiers' own rank widths (no hardcoded counts).
#'
#' The primary tiers are anchored at the bucket top (`anchor_to_first`):
#' their `rank_from` values are global standings ranks (e.g. SF losers
#' start at rank `final_seats + 1`), but the caps table is consumed
#' bucket-locally by [.range_avg_payout()]. Without the anchor, the global
#' ranks above the bucket become a leading $0 pad that pushes the bucket's
#' tail money past `total_slots`, where it silently leaks out of the pool
#' (BBM7: $50,025/sim; Dachshund: $3,744/sim -- enough to fail the money-
#' conservation gate).
#' @keywords internal
.tier_caps_combined <- function(tiers, primary_eligibility, fallback_eligibility,
                                total_slots) {
  primary_caps <- .tier_caps_from_yaml(tiers, eligibility = primary_eligibility,
                                       total_slots = NULL, anchor_to_first = TRUE)
  primary_n <- sum(vapply(primary_caps, `[[`, integer(1), "n_slots"))
  fallback_usd <- 0
  for (t in tiers) {
    if (identical(t$eligibility %||% NA_character_, fallback_eligibility)) {
      fallback_usd <- as.numeric(t$usd); break
    }
  }
  remaining <- total_slots - primary_n
  if (remaining > 0L) {
    primary_caps[[length(primary_caps) + 1L]] <- list(n_slots = remaining, usd = fallback_usd)
  }
  primary_caps
}

#' Expected payout for a sample team's representative range over a bucket.
#' Sample rank `k` of `n_sample` covers full-field bucket ranks
#' `((k-1)*n_full/n_sample, k*n_full/n_sample]`. The rank-scaling terms are
#' cast to double to stay overflow-safe past rank ~3,194 (n_full=672,336).
#' @keywords internal
.range_avg_payout <- function(rank_in_sample, n_sample, n_full, caps) {
  if (n_sample <= 0L) return(0)
  rk_low  <- floor((as.numeric(rank_in_sample) - 1) * as.numeric(n_full) / n_sample) + 1
  rk_high <- floor(as.numeric(rank_in_sample) * as.numeric(n_full) / n_sample)
  if (rk_high < rk_low) rk_high <- rk_low
  total <- 0; cursor <- 1L
  for (tc in caps) {
    tier_end <- cursor + tc$n_slots - 1L
    overlap_low  <- max(cursor, rk_low); overlap_high <- min(tier_end, rk_high)
    if (overlap_high >= overlap_low) {
      total <- total + (overlap_high - overlap_low + 1L) * tc$usd
    }
    cursor <- tier_end + 1L
    if (cursor > rk_high) break
  }
  total / (rk_high - rk_low + 1L)
}

#' Look up payout $ for a vector of full-field ranks against a tier table,
#' optionally restricted to accepted `eligibility` tags.
#' @keywords internal
.payout_lookup <- function(ranks, tiers, eligibility = NULL) {
  out <- numeric(length(ranks))
  if (length(tiers) == 0L) return(out)
  starts <- vapply(tiers, function(t) as.integer(t$rank_from %||% NA_integer_), integer(1))
  ends   <- vapply(tiers, function(t) as.integer(t$rank_to %||% NA_integer_), integer(1))
  usds   <- vapply(tiers, function(t) as.numeric(t$usd %||% 0), numeric(1))
  elig   <- vapply(tiers, function(t) as.character(t$eligibility %||% NA_character_),
                   character(1))
  for (i in seq_along(ranks)) {
    r <- ranks[i]; if (is.na(r)) next
    matches <- which(starts <= r & ends >= r)
    if (length(matches) == 0L) next
    if (!is.null(eligibility)) {
      keep <- which(is.na(elig[matches]) | elig[matches] %in% eligibility)
      matches <- matches[keep]; if (length(matches) == 0L) next
    }
    out[i] <- usds[matches[1L]]
  }
  out
}
