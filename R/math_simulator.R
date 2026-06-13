#' Simulate one roster's season-points distribution
#'
#' The keystone team-level Monte Carlo. Composes the merged pieces of
#' Layer A / 3b-2 / 3b-3:
#' \enumerate{
#'   \item Layer A marginal weekly draws -> per-player per-week
#'     empirical CDF.
#'   \item [sample_correlated_draws()] (3b-2) -> correlated weekly draws
#'     for this roster only, with team / same-game / cross factors
#'     applied per the slate-shared `corr_params`.
#'   \item [optimize_lineup_totals()] (3b-3) -> the optimal starting-
#'     lineup total per (sim, week) under the slate's lineup spec.
#'   \item Aggregate across weeks for the season-points distribution.
#' }
#'
#' Generic on purpose: returns the full per-(sim, week) lineup-total
#' matrix so downstream modules can slice it however their tournament
#' demands (season sum over any week window for BBM, per-week for WW,
#' weekly pairs for Eliminator). Tournament advancement, field
#' generation, and EV accounting are out of scope (3b-6 / 3b-7 / 3c /
#' 3d).
#'
#' @param roster Character vector of `underdog_id` values on the roster.
#' @param positions Named character vector mapping `underdog_id` to
#'   position (`"QB"`, `"RB"`, `"WR"`, `"TE"`). Must cover every member
#'   of `roster`.
#' @param layerA_draws Long Layer A draws (`underdog_id, sim_idx, week,
#'   draw_value`) -- typically the parquet written by [publish_v2()].
#'   May contain players outside the roster; only roster rows are used.
#' @param schedule Per-player-per-week schedule data.frame with columns
#'   `underdog_id, week, team, opponent, is_bye`. Same format
#'   [sample_correlated_draws()] consumes.
#' @param slate_id Slate ID used to load the lineup spec via
#'   [load_slate_lineup_spec()]. Ignored if `lineup_spec` is passed.
#' @param lineup_spec Optional pre-loaded lineup spec (e.g. in tests
#'   where there is no filesystem manifest entry). Overrides `slate_id`.
#' @param corr_params A `list(team, game, cross)` correlation
#'   configuration. Defaults to [default_corr_params] (Option A).
#' @param n_sims Output sim count (default 10000L).
#' @param seed Optional integer RNG seed.
#' @param weeks Optional integer vector of weeks to simulate. Defaults to
#'   all weeks present in `layerA_draws`. The season-total aggregate is
#'   the row-sum across these weeks; the raw weekly matrix retains the
#'   per-week breakdown so downstream callers can re-aggregate.
#' @param precomputed_marginals Optional output of
#'   [precompute_layerA_marginals()]. Threaded through to 3b-2 to skip
#'   the per-roster sort; supply once and reuse across many rosters in
#'   batch contexts ([simulate_teams()] does this automatically).
#' @param availability_p_miss Optional named numeric vector
#'   (`underdog_id -> p_miss`). When supplied, the draw-level availability mask
#'   (`q = 1 - p_miss`) is applied post-inverse-CDF via [.mask_matrix_list()] --
#'   the same zeroing the tensor build and curve field-build apply -- so this
#'   scoring path prices attrition consistently. `NULL` (default) = no mask.
#' @return A `list` with:
#'   \itemize{
#'     \item `weekly_totals` -- numeric matrix `[n_sims x n_weeks]` with
#'       `colnames` set to the week numbers.
#'     \item `season_totals` -- numeric vector of length `n_sims`,
#'       row-sum of `weekly_totals` over the simulated weeks.
#'     \item `summary` -- list with `mean`, `sd`, `p10`, `p25`, `p50`,
#'       `p75`, `p90`, `p95`, `p99`.
#'   }
#' @export
simulate_team_season <- function(roster,
                                 positions,
                                 layerA_draws,
                                 schedule,
                                 slate_id = NULL,
                                 lineup_spec = NULL,
                                 corr_params = default_corr_params,
                                 n_sims = 10000L,
                                 seed = NULL,
                                 weeks = NULL,
                                 precomputed_marginals = NULL,
                                 availability_p_miss = NULL) {
  roster <- unique(as.character(roster))
  if (length(roster) == 0L) {
    cli::cli_abort("`roster` is empty.")
  }
  if (is.null(lineup_spec)) {
    if (is.null(slate_id)) {
      cli::cli_abort("Provide either `slate_id` or `lineup_spec`.")
    }
    lineup_spec <- load_slate_lineup_spec(slate_id)
  }
  if (!is.character(positions) || is.null(names(positions))) {
    cli::cli_abort("`positions` must be a named character vector (underdog_id -> position).")
  }
  miss_pos <- setdiff(roster, names(positions))
  if (length(miss_pos) > 0L) {
    cli::cli_abort("`positions` is missing roster member(s): {miss_pos}")
  }

  if (is.null(weeks)) {
    weeks <- sort(unique(as.integer(layerA_draws$week)))
  } else {
    weeks <- sort(unique(as.integer(weeks)))
  }
  if (length(weeks) == 0L) {
    cli::cli_abort("No weeks to simulate (empty `layerA_draws$week` and no `weeks` override).")
  }

  corr_draws <- sample_correlated_draws(
    player_ids            = roster,
    layerA_draws          = layerA_draws,
    schedule              = schedule,
    corr_params           = corr_params,
    n_sims                = n_sims,
    seed                  = seed,
    precomputed_marginals = precomputed_marginals,
    output_format         = "matrix_list"
  )

  # Draw-level availability: mask the roster's conditional draws post-inv-CDF
  # at the per-player q (same process as the tensor / curve-field). Default
  # NULL -> no mask (behavior unchanged). See DRAW_ZEROING_DESIGN.md.
  if (!is.null(availability_p_miss)) {
    q <- 1 - availability_p_miss[roster]
    q[is.na(q)] <- 1
    names(q) <- roster
    corr_draws <- .mask_matrix_list(corr_draws, q, seed)
  }

  weekly_totals <- optimize_lineup_totals(
    scores      = corr_draws,
    positions   = positions[roster],
    lineup_spec = lineup_spec,
    weeks       = weeks
  )

  season_totals <- rowSums(weekly_totals)

  list(
    weekly_totals = weekly_totals,
    season_totals = season_totals,
    summary       = .season_summary(season_totals)
  )
}

#' Simulate many rosters with shared slate-level pre-computation
#'
#' Batch wrapper around [simulate_team_season()] for 3b-5's validator and
#' anywhere else that runs the engine over many rosters in one pass. Two
#' optimizations vs. naive looping:
#' \itemize{
#'   \item **Marginals pre-sorted once.** Computes
#'     [precompute_layerA_marginals()] over the union of all rosters'
#'     `underdog_id`s and threads the cache through each per-team call.
#'     For a slate of ~1400 players and 450 rosters of ~30 players each
#'     the union is typically a few hundred players -- this is the main
#'     batch speedup.
#'   \item **Per-team deterministic seeding.** Each team gets seed
#'     `base_seed + i` (1-indexed). Reproducible and independent across
#'     teams.
#' }
#'
#' @inheritParams simulate_team_season
#' @param rosters Named list of `underdog_id` vectors, one per team. If
#'   unnamed, teams are auto-labeled `"team_1"`, `"team_2"`, ....
#' @param base_seed Optional integer base seed. Per-team seeds are
#'   `base_seed + i`. `NULL` -> per-team `NULL` (nondeterministic).
#' @param summarize_only When `FALSE` (default), returns a named list of
#'   per-team [simulate_team_season()] outputs. When `TRUE`, returns just
#'   the per-team summaries + a `[n_sims x n_teams]` season-total matrix
#'   -- this avoids holding every per-team `weekly_totals` matrix (which
#'   at 10K x 17 doubles is ~1.4MB per team).
#' @return If `summarize_only = FALSE`: a named list of
#'   [simulate_team_season()] outputs. If `summarize_only = TRUE`: a list
#'   with `summaries` (data.frame, one row per team) and
#'   `season_totals_by_team` (numeric matrix `[n_sims x n_teams]`).
#' @export
simulate_teams <- function(rosters,
                           positions,
                           layerA_draws,
                           schedule,
                           slate_id = NULL,
                           lineup_spec = NULL,
                           corr_params = default_corr_params,
                           n_sims = 10000L,
                           base_seed = NULL,
                           weeks = NULL,
                           summarize_only = FALSE,
                           availability_p_miss = NULL) {
  if (!is.list(rosters) || length(rosters) == 0L) {
    cli::cli_abort("`rosters` must be a non-empty list of underdog_id vectors.")
  }
  if (is.null(names(rosters)) || any(!nzchar(names(rosters)))) {
    names(rosters) <- paste0("team_", seq_along(rosters))
  }
  if (is.null(lineup_spec)) {
    if (is.null(slate_id)) {
      cli::cli_abort("Provide either `slate_id` or `lineup_spec`.")
    }
    lineup_spec <- load_slate_lineup_spec(slate_id)
  }

  union_ids <- unique(unlist(rosters, use.names = FALSE))
  if (is.null(weeks)) {
    weeks_to_use <- sort(unique(as.integer(layerA_draws$week)))
  } else {
    weeks_to_use <- sort(unique(as.integer(weeks)))
  }
  marginals <- precompute_layerA_marginals(
    layerA_draws = layerA_draws,
    player_ids   = union_ids,
    weeks        = weeks_to_use
  )

  n_teams <- length(rosters)
  if (summarize_only) {
    summary_list <- vector("list", n_teams)
    season_mat <- matrix(0, nrow = n_sims, ncol = n_teams,
                         dimnames = list(NULL, names(rosters)))
  } else {
    per_team <- vector("list", n_teams)
    names(per_team) <- names(rosters)
  }

  for (i in seq_len(n_teams)) {
    t_name <- names(rosters)[i]
    t_seed <- if (is.null(base_seed)) NULL else as.integer(base_seed) + i
    res <- simulate_team_season(
      roster                = rosters[[i]],
      positions             = positions,
      layerA_draws          = layerA_draws,
      schedule              = schedule,
      lineup_spec           = lineup_spec,
      corr_params           = corr_params,
      n_sims                = n_sims,
      seed                  = t_seed,
      weeks                 = weeks,
      precomputed_marginals = marginals,
      availability_p_miss   = availability_p_miss
    )
    if (summarize_only) {
      summary_list[[i]] <- res$summary
      season_mat[, i]   <- res$season_totals
    } else {
      per_team[[i]] <- res
    }
  }

  if (!summarize_only) return(per_team)

  summaries <- do.call(rbind, lapply(seq_len(n_teams), function(i) {
    s <- summary_list[[i]]
    data.frame(
      team_id = names(rosters)[i],
      mean = s$mean, sd = s$sd,
      p10 = s$p10, p25 = s$p25, p50 = s$p50,
      p75 = s$p75, p90 = s$p90, p95 = s$p95, p99 = s$p99,
      stringsAsFactors = FALSE
    )
  }))
  list(summaries = summaries, season_totals_by_team = season_mat)
}

# ---- internals --------------------------------------------------------------

#' @keywords internal
.season_summary <- function(x) {
  list(
    mean = mean(x),
    sd   = stats::sd(x),
    p10  = unname(stats::quantile(x, 0.10, names = FALSE)),
    p25  = unname(stats::quantile(x, 0.25, names = FALSE)),
    p50  = unname(stats::quantile(x, 0.50, names = FALSE)),
    p75  = unname(stats::quantile(x, 0.75, names = FALSE)),
    p90  = unname(stats::quantile(x, 0.90, names = FALSE)),
    p95  = unname(stats::quantile(x, 0.95, names = FALSE)),
    p99  = unname(stats::quantile(x, 0.99, names = FALSE))
  )
}
