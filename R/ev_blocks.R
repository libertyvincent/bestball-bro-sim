#' EV building blocks: the sim <-> extension contract artifacts
#'
#' Implements the sim side of `docs/ev_building_blocks_contract.md`:
#'
#' * **Artifact A** -- per-slate path-aligned joint draws: a downsampled
#'   `int16` tensor (`[path][player][week]`, scores x10) of *correlated*
#'   season paths over the top draftable players, plus a JSON sidecar.
#'   Built from the existing Layer A draws (the `v2/draws/<slate>.parquet`
#'   long format) passed through one **joint** [sample_correlated_draws()]
#'   call -- the shared path axis is the entire correlation mechanism.
#' * **Artifact B** -- per-tournament curves: advancement probabilities
#'   `g1(R1)/g2(R2)/g3(R3)` vs the survivor-selected field of each round,
#'   and payout-by-exit-bucket curves `payout_QF(R2)/payout_SF(R3)/
#'   h_final(R4)`. Lookup tables + linear interpolation; built from the
#'   same sim run's synthetic-field pools ([build_field_payouts()]).
#' * **Reference eval loop** -- the R implementation of the extension's
#'   per-path combine, used by the validation gate (curve EV must
#'   reproduce the full Layer-B sim EV) and the path-count protocol.
#'
#' The wire formats here are the agreement with the extension repo; change
#' them only by amending the contract doc.

# ---- Artifact A: path-aligned joint draws ------------------------------------

#' Build a per-player-per-week schedule data.frame from a blended feed
#'
#' The standard `(underdog_id, week, team, opponent, is_bye)` long table
#' that [sample_correlated_draws()] and [simulate_per_stage_scores()]
#' consume, extracted from `feed$players[[i]]$weekly`.
#'
#' @param feed A blended feed from [blend_slate()].
#' @return A data.frame with columns `underdog_id`, `week`, `team`,
#'   `opponent`, `is_bye`.
#' @export
schedule_from_feed <- function(feed) {
  chunks <- list()
  for (uid in names(feed$players)) {
    pl <- feed$players[[uid]]
    for (wk in pl$weekly %||% list()) {
      w <- as.integer(wk$week %||% NA_integer_)
      if (is.na(w)) next
      chunks[[length(chunks) + 1L]] <- data.frame(
        underdog_id = uid, week = w, team = pl$team %||% NA_character_,
        opponent = if (isTRUE(wk$is_bye)) NA_character_
                   else (wk$opponent %||% NA_character_),
        is_bye = isTRUE(wk$is_bye), stringsAsFactors = FALSE)
    }
  }
  do.call(rbind, chunks)
}

#' Positions named vector (underdog_id -> position) from a blended feed
#' @param feed A blended feed from [blend_slate()].
#' @return Named character vector; players with no position are dropped.
#' @export
positions_from_feed <- function(feed) {
  positions <- vapply(names(feed$players),
                      function(uid) feed$players[[uid]]$position %||% NA_character_,
                      character(1))
  names(positions) <- names(feed$players)
  positions[!is.na(positions)]
}

#' Pick the top-n realistically-draftable players from a feed
#'
#' Ranked by ADP when present (ascending), then by blended season
#' projection (descending) for players with no ADP. Players with no
#' usable projection or position are excluded.
#' @keywords internal
.ev_top_players <- function(feed, top_n) {
  ids <- names(feed$players)
  adp <- vapply(feed$players, function(p) as.numeric(p$adp %||% NA_real_), numeric(1))
  sm  <- vapply(feed$players, function(p) as.numeric(p$season_mean %||% NA_real_), numeric(1))
  pos <- vapply(feed$players, function(p) p$position %||% NA_character_, character(1))
  keep <- !is.na(sm) & !is.na(pos)
  ids <- ids[keep]; adp <- adp[keep]; sm <- sm[keep]
  ord <- order(is.na(adp), adp, -sm)
  utils::head(ids[ord], top_n)
}

#' Build Artifact A: the path-aligned joint-draws tensor for a slate
#'
#' Runs one **joint** correlated draw over the top `top_n` draftable
#' players (so cross-player NFL-game/team correlation is preserved within
#' each path) and quantizes the result into an integer tensor.
#'
#' Path alignment is the artifact's hard invariant: path `i` is the same
#' simulated world for every player. The joint [sample_correlated_draws()]
#' call guarantees it at build time; [write_ev_draws()] /
#' [read_ev_draws()] preserve it through serialization (no per-player
#' reordering anywhere). See the alignment tests in
#' `test-ev-blocks.R`.
#'
#' @param feed A blended feed from [blend_slate()] (positions, ADP,
#'   schedule source).
#' @param layerA_draws Long Layer A draws (`underdog_id, sim_idx, week,
#'   draw_value`) -- in production, the existing
#'   `v2/draws/<slate>.parquet` read back with [arrow::read_parquet()];
#'   the draws are reused, never regenerated.
#' @param slate_id Slate identifier carried into the sidecar.
#' @param lineup_spec Optional slate lineup spec from
#'   [load_slate_lineup_spec()] (`{slate_id, slots:[{pos, n, eligible}]}`).
#'   When supplied it is carried on the object and serialized verbatim into
#'   the draws sidecar, so the feed is self-describing: the extension's
#'   round-score assembler needs it (together with `stage_weeks` from the
#'   curves file) to turn the tensor into round scores. `NULL` omits it.
#' @param n_paths Number of correlated paths to ship (set by the
#'   ranking-stability protocol; see the contract doc).
#' @param top_n Number of draftable players to include (default 300).
#' @param corr_params Correlation parameters; default [default_corr_params].
#' @param seed Optional integer seed for the joint draw.
#' @param quant_scale Score quantization factor (default 10 = 0.1-pt
#'   resolution).
#' @param weeks Integer vector of weeks to ship (default `1:17`, the
#'   weeks the Season tournaments use). The slate data may carry more
#'   (e.g. NFL week 18); those are drawn jointly but trimmed from the
#'   tensor, so the same seed yields the same paths regardless of the
#'   week selection.
#' @return A `bbbro_ev_draws` object: list with `tensor` (integer array,
#'   dim `[n_weeks, n_players, n_paths]`, stored week-fastest so the raw
#'   buffer is `[path][player][week]` C-order), `player_ids`, `positions`,
#'   `weeks`, `n_paths`, `quant_scale`, `slate_id`, and (when supplied)
#'   `lineup_spec`.
#' @export
build_ev_draws <- function(feed, layerA_draws, slate_id,
                           n_paths = 500L, top_n = 300L,
                           corr_params = default_corr_params, seed = NULL,
                           quant_scale = 10, weeks = 1:17,
                           lineup_spec = NULL) {
  if (!is.null(lineup_spec)) {
    .validate_lineup_spec(lineup_spec)
    if (!is.null(lineup_spec$slate_id) &&
        !identical(lineup_spec$slate_id, slate_id)) {
      cli::cli_abort(c(
        "`lineup_spec` is for slate {.val {lineup_spec$slate_id}}, \\
         not the draws' slate {.val {slate_id}}."))
    }
  }
  positions <- positions_from_feed(feed)
  schedule  <- schedule_from_feed(feed)

  top_ids <- .ev_top_players(feed, top_n)
  top_ids <- intersect(top_ids, unique(layerA_draws$underdog_id))
  top_ids <- intersect(top_ids, names(positions))
  if (length(top_ids) == 0L) {
    cli::cli_abort("No draftable players overlap between `feed` and `layerA_draws`.")
  }

  # ONE joint draw across all players: the shared path axis IS the
  # correlation mechanism (contract hard invariant).
  ml <- sample_correlated_draws(
    player_ids    = top_ids,
    layerA_draws  = layerA_draws,
    schedule      = schedule,
    corr_params   = corr_params,
    n_sims        = as.integer(n_paths),
    seed          = seed,
    output_format = "matrix_list")

  drawn_weeks <- sort(as.integer(names(ml)))
  weeks <- intersect(sort(as.integer(weeks)), drawn_weeks)
  if (length(weeks) == 0L) {
    cli::cli_abort("None of the requested `weeks` are present in the Layer A draws.")
  }
  n_players <- length(top_ids)
  n_weeks   <- length(weeks)

  # In-memory layout: dim [week, player, path]. R is column-major, so the
  # linearized buffer runs week-fastest, then player, then path -- i.e. the
  # file is [path][player][week] in C-order, path-major as the contract
  # requires (a path's full board is contiguous).
  tensor <- array(0L, dim = c(n_weeks, n_players, as.integer(n_paths)))
  for (wi in seq_along(weeks)) {
    M <- ml[[as.character(weeks[wi])]]
    missing <- setdiff(top_ids, rownames(M))
    if (length(missing) > 0L) {
      cli::cli_abort(c(
        "Week {weeks[wi]} correlated draws are missing {length(missing)} player(s).",
        x = "Path alignment cannot be guaranteed with partial player coverage.",
        i = "First missing: {.val {utils::head(missing, 3)}}"))
    }
    # Row-subset to the canonical player order. NEVER touch column (path) order.
    M <- M[top_ids, , drop = FALSE]
    v <- as.integer(round(M * quant_scale))
    if (any(v > 32767L | v < -32768L)) {
      cli::cli_abort("Quantized scores exceed int16 range; lower `quant_scale`.")
    }
    tensor[wi, , ] <- v
  }

  structure(list(
    tensor      = tensor,
    player_ids  = top_ids,
    positions   = positions[top_ids],
    weeks       = weeks,
    n_paths     = as.integer(n_paths),
    quant_scale = quant_scale,
    slate_id    = slate_id,
    lineup_spec = lineup_spec
  ), class = "bbbro_ev_draws")
}

#' Write Artifact A to disk (raw int16 tensor + JSON sidecar)
#'
#' The binary file is the tensor's raw little-endian `int16` buffer in
#' `[path][player][week]` C-order (path-major). The sidecar carries
#' everything the extension needs to index it.
#'
#' @param ev_draws A `bbbro_ev_draws` from [build_ev_draws()].
#' @param out_dir Feed root directory.
#' @return List with relative `bin_path` / `sidecar_path` and their
#'   sha256 checksums, invisibly.
#' @export
write_ev_draws <- function(ev_draws, out_dir) {
  stopifnot(inherits(ev_draws, "bbbro_ev_draws"))
  slate_id    <- ev_draws$slate_id
  bin_rel     <- file.path("v2", "ev", paste0(slate_id, "_draws.bin"))
  sidecar_rel <- file.path("v2", "ev", paste0(slate_id, "_draws.json"))
  bin_abs     <- file.path(out_dir, bin_rel)
  sidecar_abs <- file.path(out_dir, sidecar_rel)
  dir.create(dirname(bin_abs), recursive = TRUE, showWarnings = FALSE)

  # R's column-major linearization of [week, player, path] = week fastest,
  # path slowest = the contract's [path][player][week] file order. Passing
  # the path (not a connection) lets writeBin open, flush, and close the
  # file itself -- the sha below is computed on the complete file.
  writeBin(as.integer(ev_draws$tensor), bin_abs, size = 2L, endian = "little")

  sidecar <- list(
    slate_id     = slate_id,
    dtype        = "int16",
    endianness   = "little",
    axis_order   = c("path", "player", "week"),
    n_paths      = ev_draws$n_paths,
    n_players    = length(ev_draws$player_ids),
    n_weeks      = length(ev_draws$weeks),
    weeks        = ev_draws$weeks,
    quant_scale  = ev_draws$quant_scale,
    # 0-based indices for the JS consumer.
    player_index = as.list(stats::setNames(
      seq_along(ev_draws$player_ids) - 1L, ev_draws$player_ids)),
    positions    = as.list(ev_draws$positions),
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  # Self-describing feed: the round-score assembler needs the lineup_spec
  # (same `{slate_id, slots:[{pos,n,eligible}]}` shape the #30 fixture
  # embedded) alongside the tensor. Inserted before generated_at; omitted
  # when the draws were built without a spec (e.g. synthetic-slate tests).
  if (!is.null(ev_draws$lineup_spec)) {
    sidecar <- append(sidecar, list(lineup_spec = ev_draws$lineup_spec),
                      after = which(names(sidecar) == "positions"))
  }
  jsonlite::write_json(sidecar, sidecar_abs, auto_unbox = TRUE, pretty = TRUE,
                       null = "null", na = "null")

  invisible(list(
    bin_path        = bin_rel,
    sidecar_path    = sidecar_rel,
    bin_sha256      = .file_sha256(bin_abs),
    sidecar_sha256  = .file_sha256(sidecar_abs)))
}

#' Read Artifact A back from disk
#'
#' Round-trip reader for [write_ev_draws()] output -- used by the
#' alignment tests and the reference eval loop, and as the executable
#' spec of how the extension must deserialize the buffer.
#'
#' @param out_dir Feed root directory.
#' @param slate_id Slate identifier.
#' @return A `bbbro_ev_draws` object.
#' @export
read_ev_draws <- function(out_dir, slate_id) {
  bin_abs     <- file.path(out_dir, "v2", "ev", paste0(slate_id, "_draws.bin"))
  sidecar_abs <- file.path(out_dir, "v2", "ev", paste0(slate_id, "_draws.json"))
  if (!file.exists(bin_abs) || !file.exists(sidecar_abs)) {
    cli::cli_abort("EV draws not found for slate {.val {slate_id}} under {.path {out_dir}}.")
  }
  sc <- jsonlite::fromJSON(sidecar_abs, simplifyVector = TRUE)
  n_total <- sc$n_paths * sc$n_players * sc$n_weeks
  v <- readBin(bin_abs, what = "integer", n = n_total, size = 2L,
               signed = TRUE, endian = "little")
  if (length(v) != n_total) {
    cli::cli_abort("EV draws binary is truncated: read {length(v)} of {n_total} values.")
  }
  # player_index is 0-based; invert it to recover the canonical row order.
  pidx <- unlist(sc$player_index)
  player_ids <- names(sort(pidx))
  positions  <- unlist(sc$positions)[player_ids]

  structure(list(
    tensor      = array(v, dim = c(sc$n_weeks, sc$n_players, sc$n_paths)),
    player_ids  = player_ids,
    positions   = positions,
    weeks       = as.integer(sc$weeks),
    n_paths     = as.integer(sc$n_paths),
    quant_scale = sc$quant_scale,
    slate_id    = sc$slate_id
  ), class = "bbbro_ev_draws")
}

#' Extract one player's [week x path] score matrix from an ev_draws object
#' (de-quantized). Used by tests and the eval loop.
#' @keywords internal
.ev_player_matrix <- function(ev_draws, underdog_id) {
  i <- match(underdog_id, ev_draws$player_ids)
  if (is.na(i)) {
    cli::cli_abort("Player {.val {underdog_id}} not in the EV draws.")
  }
  M <- ev_draws$tensor[, i, , drop = FALSE]
  M <- array(M, dim = dim(ev_draws$tensor)[c(1L, 3L)])
  dimnames(M) <- list(as.character(ev_draws$weeks), NULL)
  M / ev_draws$quant_scale
}

# ---- Artifact B: per-tournament curves ----------------------------------------

#' Advancement-probability curve: P(advance | round score) vs an opponent pool
#'
#' For a pod of `pod_size` with `advance_n` advancing, against opponents
#' drawn iid from `pool`:
#' `P(advance | s) = sum_{j=0}^{advance_n-1} C(p-1, j) (1-F(s))^j F(s)^(p-1-j)`
#' where `F` is the pool's ECDF -- the probability that at most
#' `advance_n - 1` of the `pod_size - 1` opponents beat `s`.
#' @keywords internal
.advance_prob_curve <- function(pool, pod_size, advance_n, n_grid = 256L) {
  pool <- sort(pool[is.finite(pool)])
  x <- unique(stats::quantile(pool, seq(0, 1, length.out = n_grid),
                              names = FALSE, type = 7))
  # Extend one step past the pool's support so out-of-range scores
  # extrapolate to certainty (F = 0 -> never advance; F = 1 -> always)
  # rather than clamping at the pool-granularity boundary values.
  x <- c(min(pool) - .pool_step(pool), x, max(pool) + .pool_step(pool))
  Fx <- stats::ecdf(pool)(x)
  n_opp <- pod_size - 1L
  y <- vapply(Fx, function(F) {
    sum(vapply(0:(advance_n - 1L), function(j)
      choose(n_opp, j) * (1 - F)^j * F^(n_opp - j), numeric(1)))
  }, numeric(1))
  list(x = x, y = y)
}

#' Grid-extension step: a small positive distance relative to the pool's range.
#' @keywords internal
.pool_step <- function(pool) {
  max(1, 0.001 * (max(pool) - min(pool)))
}

#' Expected-payout-on-exit curve: $ vs round score for one progression bucket
#'
#' Mirrors [compute_team_ev()]'s bucket placement exactly: a team exiting
#' with round score `s` lands at within-bucket rank
#' `ceiling(P(pool >= s) * bucket_size)`, i.e. global standings rank
#' `rank_offset + within`, then the tier table is consulted. Mirroring
#' the full engine (rather than re-deriving) keeps the validation gate a
#' measure of the *factorization* error only.
#' @keywords internal
.exit_payout_curve <- function(pool, tiers, eligibility, rank_offset,
                               bucket_size, n_grid = 256L) {
  pool <- sort(pool[is.finite(pool)])
  x <- unique(stats::quantile(pool, seq(0, 1, length.out = n_grid),
                              names = FALSE, type = 7))
  # Extend past the pool's support: a score above every pool value lands at
  # the TOP of the bucket (survival = 0 -> within-rank 1), which the
  # in-pool grid can never reach (its best point maps to within-rank
  # ceiling(bucket / n_pool) -- a pool-granularity artifact that would
  # otherwise clamp exceptional scores to the wrong payout).
  x <- c(min(pool) - .pool_step(pool), x, max(pool) + .pool_step(pool))
  y <- vapply(x, function(s) {
    within <- max(1L, as.integer(ceiling(mean(pool >= s) * bucket_size)))
    .payout_lookup(rank_offset + within, tiers, eligibility = eligibility)
  }, numeric(1))
  list(x = x, y = y)
}

#' Build Artifact B: the six per-tournament curves
#'
#' Encapsulates the tournament's entire field / bracket / ladder as
#' functions of round scores, built from the same sim run's
#' synthetic-field stage scores (so player-model calibration matches the
#' draws). Supports the 4-stage Season shape (qualifier -> QF -> SF ->
#' final) that every in-scope tournament uses.
#'
#' @param tournament_cfg Parsed config from [load_tournament()].
#' @param field_scores Output of [simulate_per_stage_scores()] for a
#'   synthetic field generated by [generate_field()] -- the same sim run
#'   as the draws artifact.
#' @param n_grid Number of curve grid points (default 256).
#' @param seed Optional seed for the pod-advancement simulation inside
#'   [build_field_payouts()].
#' @return A `bbbro_tournament_curves` object: `tournament_id`,
#'   `slate_id`, `stage_weeks` (named list `r1`..`r4`), `structure`
#'   (pods/seats/advance), and `curves` (named list `g1`, `g2`, `g3`,
#'   `payout_qf`, `payout_sf`, `h_final`, each `{x, y}`).
#' @export
build_tournament_curves <- function(tournament_cfg, field_scores,
                                    n_grid = 256L, seed = NULL) {
  st <- .stage_structure(tournament_cfg)
  if (st$n_stage != 4L) {
    cli::cli_abort(c(
      "build_tournament_curves() supports the 4-stage Season shape only.",
      i = "This tournament has {st$n_stage} stages; extend the curve vocabulary first."))
  }
  fp <- build_field_payouts(field_scores, tournament_cfg, seed = seed)
  pools  <- fp$pools
  q_pool <- pools$q_cum_pool
  ent    <- pools$stage_entrant_metric
  c_tiers <- tournament_cfg$payouts$championship_round$tiers
  if (is.null(c_tiers)) {
    cli::cli_abort("Tournament {.val {tournament_cfg$tournament_id}} has no championship_round payout table.")
  }

  stage_weeks <- .resolve_stage_weeks(tournament_cfg)

  curves <- list(
    # Advancement vs the survivor-selected field of each round.
    g1 = .advance_prob_curve(q_pool,    st$pod[1L], st$advn[1L], n_grid),
    g2 = .advance_prob_curve(ent[[2L]], st$pod[2L], st$advn[2L], n_grid),
    g3 = .advance_prob_curve(ent[[3L]], st$pod[3L], st$advn[3L], n_grid),
    # Payout by exit bucket (global standings rank -> ladder).
    payout_qf = .exit_payout_curve(
      ent[[2L]], c_tiers,
      eligibility = c(.champ_tags(2L, 4L), "qualifier_advancer"),
      rank_offset = st$seats[3L], bucket_size = st$seats[2L] - st$seats[3L],
      n_grid = n_grid),
    payout_sf = .exit_payout_curve(
      ent[[3L]], c_tiers,
      eligibility = c(.champ_tags(3L, 4L), "qualifier_advancer"),
      rank_offset = st$seats[4L], bucket_size = st$seats[3L] - st$seats[4L],
      n_grid = n_grid),
    h_final = .exit_payout_curve(
      ent[[4L]], c_tiers, eligibility = "finalist",
      rank_offset = 0L, bucket_size = st$seats[4L], n_grid = n_grid))

  structure(list(
    tournament_id = tournament_cfg$tournament_id,
    slate_id      = tournament_cfg$slate_id,
    stage_weeks   = list(r1 = stage_weeks[["1"]], r2 = stage_weeks[["2"]],
                         r3 = stage_weeks[["3"]], r4 = stage_weeks[["4"]]),
    structure     = list(pod_sizes = st$pod, seats = st$seats,
                         advance_n = st$advn[seq_len(3L)]),
    built_from    = list(n_field = nrow(field_scores$stage_scores[[1L]]),
                         n_sims  = ncol(field_scores$stage_scores[[1L]])),
    curves        = curves
  ), class = "bbbro_tournament_curves")
}

#' Write Artifact B (curves) to disk as JSON
#' @param curves A `bbbro_tournament_curves` from [build_tournament_curves()].
#' @param out_dir Feed root directory.
#' @return List with relative `path` and `sha256`, invisibly.
#' @export
write_tournament_curves <- function(curves, out_dir) {
  stopifnot(inherits(curves, "bbbro_tournament_curves"))
  rel <- file.path("v2", "ev", paste0(curves$tournament_id, "_curves.json"))
  abs <- file.path(out_dir, rel)
  dir.create(dirname(abs), recursive = TRUE, showWarnings = FALSE)
  payload <- unclass(curves)
  payload$generated_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  jsonlite::write_json(payload, abs, auto_unbox = TRUE, pretty = TRUE,
                       digits = NA, null = "null", na = "null")
  invisible(list(path = rel, sha256 = .file_sha256(abs)))
}

#' Read Artifact B (curves) back from disk
#' @param out_dir Feed root directory.
#' @param tournament_id Tournament identifier.
#' @return A `bbbro_tournament_curves` object.
#' @export
read_tournament_curves <- function(out_dir, tournament_id) {
  abs <- file.path(out_dir, "v2", "ev", paste0(tournament_id, "_curves.json"))
  if (!file.exists(abs)) {
    cli::cli_abort("EV curves not found for tournament {.val {tournament_id}} under {.path {out_dir}}.")
  }
  obj <- jsonlite::fromJSON(abs, simplifyVector = TRUE)
  obj$generated_at <- NULL
  # jsonlite simplifies {x, y} lists to data-frame-ish lists of vectors; keep as-is.
  structure(obj, class = "bbbro_tournament_curves")
}

#' Evaluate a curve at arbitrary points (linear interpolation, clamped ends)
#' @keywords internal
.curve_eval <- function(curve, x) {
  stats::approx(curve$x, curve$y, xout = x, rule = 2, ties = "ordered")$y
}

# ---- Reference eval loop (the extension contract, in R) -----------------------

#' Per-path round scores for a roster from the EV draws tensor
#'
#' The roster-dependent step of the extension eval loop: for every path,
#' the roster's weekly best-ball lineup totals are aggregated into the
#' tournament's round windows (Season: R1 = sum of weeks 1-14, R2/R3/R4 =
#' weeks 15/16/17).
#'
#' @param roster_ids Character vector of underdog_ids on the roster.
#' @param ev_draws A `bbbro_ev_draws` object.
#' @param stage_weeks Named list `r1`..`r4` of integer week vectors (from
#'   the curves object).
#' @param lineup_spec Slate lineup spec from [load_slate_lineup_spec()].
#' @return data.frame with one row per path: `R1`, `R2`, `R3`, `R4`.
#' @export
roster_round_scores <- function(roster_ids, ev_draws, stage_weeks, lineup_spec) {
  idx <- match(roster_ids, ev_draws$player_ids)
  if (anyNA(idx)) {
    cli::cli_abort(c(
      "{sum(is.na(idx))} roster player(s) not present in the EV draws.",
      i = "Missing: {.val {utils::head(roster_ids[is.na(idx)], 5)}}"))
  }
  n_paths <- ev_draws$n_paths
  # Matrix-list view of the roster's slice (per week [n_roster x n_paths]),
  # preserving path (column) order.
  ml <- lapply(seq_along(ev_draws$weeks), function(wi) {
    M <- ev_draws$tensor[wi, idx, , drop = FALSE]
    M <- array(M, dim = c(length(idx), n_paths))
    rownames(M) <- roster_ids
    M / ev_draws$quant_scale
  })
  names(ml) <- as.character(ev_draws$weeks)

  wt <- optimize_lineup_totals(
    scores      = ml,
    positions   = ev_draws$positions[roster_ids],
    lineup_spec = lineup_spec)

  round_sum <- function(wks) {
    cols <- intersect(as.character(wks), colnames(wt))
    if (length(cols) == 0L) return(rep(0, n_paths))
    rowSums(wt[, cols, drop = FALSE])
  }
  data.frame(
    R1 = round_sum(stage_weeks$r1), R2 = round_sum(stage_weeks$r2),
    R3 = round_sum(stage_weeks$r3), R4 = round_sum(stage_weeks$r4))
}

#' Curve-based roster EV: the extension's per-path combine, in R
#'
#' `$ = g1(R1) * [ (1-g2(R2)) * payout_QF(R2) + g2(R2) * (1-g3(R3)) *
#' payout_SF(R3) + g2(R2) * g3(R3) * h_final(R4) ]`, averaged over paths.
#'
#' @param roster_ids Character vector of underdog_ids on the roster.
#' @param ev_draws A `bbbro_ev_draws` object.
#' @param curves A `bbbro_tournament_curves` object.
#' @param lineup_spec Slate lineup spec.
#' @return List with `ev` (mean $ per entry) and `per_path` (data.frame
#'   of round scores, probabilities, and $ per path).
#' @export
evaluate_roster_curve_ev <- function(roster_ids, ev_draws, curves, lineup_spec) {
  rs <- roster_round_scores(roster_ids, ev_draws, curves$stage_weeks, lineup_spec)
  cv <- curves$curves
  g1  <- .curve_eval(cv$g1, rs$R1)
  g2  <- .curve_eval(cv$g2, rs$R2)
  g3  <- .curve_eval(cv$g3, rs$R3)
  pqf <- .curve_eval(cv$payout_qf, rs$R2)
  psf <- .curve_eval(cv$payout_sf, rs$R3)
  hf  <- .curve_eval(cv$h_final,  rs$R4)
  dollars <- g1 * ((1 - g2) * pqf + g2 * (1 - g3) * psf + g2 * g3 * hf)
  per_path <- cbind(rs, g1 = g1, g2 = g2, g3 = g3, dollars = dollars)
  list(ev = mean(dollars), per_path = per_path)
}

#' Rank candidate players by curve-based marginal EV (common random numbers)
#'
#' `marginal_EV(X) = EV(roster + X) - EV(roster)`, both sides evaluated on
#' the SAME path set so per-path noise cancels (CRN). This is the ranking
#' the extension shows at each pick.
#'
#' @param roster_ids Current roster (may be empty at pick 1).
#' @param candidate_ids Available players to rank.
#' @param ev_draws A `bbbro_ev_draws` object.
#' @param curves A `bbbro_tournament_curves` object.
#' @param lineup_spec Slate lineup spec.
#' @return data.frame `underdog_id`, `position`, `marginal_ev`, sorted
#'   descending.
#' @export
rank_marginal_ev <- function(roster_ids, candidate_ids, ev_draws, curves,
                             lineup_spec) {
  base_ev <- if (length(roster_ids) > 0L) {
    evaluate_roster_curve_ev(roster_ids, ev_draws, curves, lineup_spec)$ev
  } else 0
  mev <- vapply(candidate_ids, function(x) {
    evaluate_roster_curve_ev(c(roster_ids, x), ev_draws, curves, lineup_spec)$ev - base_ev
  }, numeric(1))
  out <- data.frame(
    underdog_id = candidate_ids,
    position    = unname(ev_draws$positions[candidate_ids]),
    marginal_ev = unname(mev),
    stringsAsFactors = FALSE)
  out <- out[order(-out$marginal_ev), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# ---- Publisher + _meta registration -------------------------------------------

#' Publish EV building-block artifacts and register them in `_meta.json`
#'
#' Writes Artifact A (draws tensor + sidecar) for the slate and Artifact B
#' (curves JSON) for each tournament, then updates the feed root's
#' `_meta.json`: the slate entry gains `v2_draws_path` /
#' `v2_draws_sha256` / `v2_draws_sidecar_path` / `v2_draws_sidecar_sha256`,
#' and a `tournaments` map keyed by tournament_id with `curves_path` /
#' `curves_sha256`. Fetched slate/tournament-aware by the extension,
#' sha-gated like the v2 feed.
#'
#' @param out_dir Feed root directory.
#' @param ev_draws A `bbbro_ev_draws` for the slate.
#' @param curves_list List of `bbbro_tournament_curves` (one per
#'   tournament on the slate).
#' @return Invisibly, the `_meta.json` path.
#' @export
publish_ev_blocks <- function(out_dir, ev_draws, curves_list = list()) {
  stopifnot(inherits(ev_draws, "bbbro_ev_draws"))
  slate_id <- ev_draws$slate_id

  draws_reg <- write_ev_draws(ev_draws, out_dir)
  curve_regs <- list()
  for (cv in curves_list) {
    stopifnot(inherits(cv, "bbbro_tournament_curves"))
    if (!identical(cv$slate_id, slate_id)) {
      cli::cli_abort(c(
        "Tournament {.val {cv$tournament_id}} is on slate {.val {cv$slate_id}}, \\
         not the draws' slate {.val {slate_id}}.",
        x = "Curves must come from the same sim run / slate as the draws."))
    }
    curve_regs[[cv$tournament_id]] <- write_tournament_curves(cv, out_dir)
  }

  # Update _meta.json, preserving everything other publishers wrote.
  meta_path <- file.path(out_dir, "_meta.json")
  manifest  <- if (file.exists(meta_path)) {
    jsonlite::fromJSON(meta_path, simplifyVector = FALSE)
  } else {
    list(season = NULL, generated_at = NULL, slates = list())
  }
  if (is.null(manifest$slates)) manifest$slates <- list()
  entry <- manifest$slates[[slate_id]] %||% list()
  entry$v2_draws_path           <- draws_reg$bin_path
  entry$v2_draws_sha256         <- draws_reg$bin_sha256
  entry$v2_draws_sidecar_path   <- draws_reg$sidecar_path
  entry$v2_draws_sidecar_sha256 <- draws_reg$sidecar_sha256
  entry$v2_draws_generated_at   <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  tn <- entry$tournaments %||% list()
  for (tid in names(curve_regs)) {
    tn[[tid]] <- list(curves_path   = curve_regs[[tid]]$path,
                      curves_sha256 = curve_regs[[tid]]$sha256)
  }
  entry$tournaments <- tn
  manifest$slates[[slate_id]] <- entry

  dir.create(dirname(meta_path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(manifest, meta_path, auto_unbox = TRUE, pretty = TRUE,
                       null = "null", na = "null")
  cli::cli_alert_success("Registered EV blocks for {.val {slate_id}} in {.path {meta_path}}")
  invisible(meta_path)
}
