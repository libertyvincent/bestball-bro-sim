#' Generate Layer A player projections for one slate
#'
#' v1 / slate architecture. The slate's Underdog CSV is the canonical
#' player universe; every row gets a projection. Veterans (rows whose
#' name + team + position match a player in nflverse `player_stats`
#' history) go through the weekly→season methodology in [.project_v0()].
#' Rookies (rows with no historical match) go through the draft-capital
#' comparables module in [project_rookies()].
#'
#' Underdog's `projectedPoints` is preserved in the output as
#' `underdog_projected_points` (reference / traceability only — it does
#' NOT feed into any computation we emit).
#'
#' Deferred to v1.5+: component stat models, aging curves, team-context
#' blending, workload/depth-chart awareness, refined variance, player
#' correlations, ADP-based VOR adjustments. See LAYER_A.md.
#'
#' @param slate_id Slate identifier in `inst/data/slates/_manifest.yaml`,
#'   e.g. `"nfl_2026_season"`.
#' @param history_years How many prior seasons to use as both the
#'   veteran weekly history and the rookie comparables window.
#'   Defaults to 5.
#' @return A data frame with columns: `underdog_id`, `gsis_id` (NA for
#'   rookies), `name`, `team`, `position`, `season_mean`, `season_std`,
#'   `season_p10..p95`, `position_rank`, `vor`, `adp`,
#'   `underdog_projected_points`.
#' @export
generate_projections <- function(slate_id,
                                 history_years = 5L) {
  manifest <- load_slate_manifest()
  entry <- manifest[[slate_id]]
  if (is.null(entry)) cli::cli_abort("Unknown slate_id: {.val {slate_id}}")
  season     <- as.integer(entry$season)
  scoring_id <- entry$scoring_id

  cli::cli_alert_info("Generating v1 projections for slate {.val {slate_id}} ({season}, {scoring_id})")

  cli::cli_alert("Loading slate player universe")
  slate <- load_slate_data(slate_id)

  cli::cli_alert("Loading historical player stats ({season - history_years} - {season - 1})")
  historical <- pull_player_stats((season - history_years):(season - 1))

  cli::cli_alert("Loading draft picks ({season - history_years} - {season})")
  drafts <- pull_draft_picks((season - history_years):season)

  cli::cli_alert("Loading scoring config: {scoring_id}")
  scoring <- load_scoring_config(scoring_id)

  .project_slate(slate, historical, drafts, scoring, season)
}

#' Slate orchestrator: split veterans/rookies, project each, merge, rank
#'
#' Pure inner — accepts already-loaded data frames so the public
#' `generate_projections()` stays a thin orchestrator and the v1 logic
#' remains testable without nflreadr or network access.
#'
#' @param slate_df Slate player universe from [load_slate_data()].
#' @param historical_stats_df Weekly player stats from
#'   [pull_player_stats()].
#' @param drafts_df Draft picks from [pull_draft_picks()].
#' @param scoring_cfg `bbbro_scoring` object.
#' @param season Current NFL season.
#' @keywords internal
.project_slate <- function(slate_df, historical_stats_df, drafts_df,
                           scoring_cfg, season) {
  skill <- c("QB", "RB", "WR", "TE")
  s <- slate_df[!is.na(slate_df$position) &
                  slate_df$position %in% skill &
                  !is.na(slate_df$underdog_id), , drop = FALSE]
  s <- s[!duplicated(s$underdog_id), , drop = FALSE]
  if (nrow(s) == 0L) return(.empty_slate_proj_df())

  matched_gsis <- .match_slate_to_history(s, historical_stats_df)

  vet_idx       <- !is.na(matched_gsis)
  vets_slate    <- s[vet_idx, , drop = FALSE]
  vets_slate$gsis_id <- matched_gsis[vet_idx]
  rookies_slate <- s[!vet_idx, , drop = FALSE]

  cli::cli_alert("Slate split: {nrow(vets_slate)} veterans, {nrow(rookies_slate)} rookies")

  vet_proj    <- .project_slate_veterans(vets_slate, historical_stats_df,
                                         scoring_cfg)
  rookie_proj <- .project_slate_rookies(rookies_slate, drafts_df,
                                        historical_stats_df, scoring_cfg,
                                        season)

  combined <- rbind(vet_proj, rookie_proj)

  extras <- data.frame(
    underdog_id               = s$underdog_id,
    adp                       = s$adp,
    underdog_projected_points = s$projected_points,
    stringsAsFactors = FALSE
  )
  combined <- merge(combined, extras, by = "underdog_id",
                    all.x = TRUE, sort = FALSE)

  .add_slate_rank_and_vor(combined)
}

#' Veteran projections for slate-shaped rows, keyed by underdog_id
#'
#' Wraps [.project_v0()] (the weekly→season computation) and merges its
#' output back to the slate by `gsis_id`. Drops `.project_v0`'s
#' position_rank/VOR columns — those are recomputed across the full
#' slate in [.add_slate_rank_and_vor()].
#'
#' @keywords internal
.project_slate_veterans <- function(vets_slate, historical_stats_df,
                                    scoring_cfg) {
  if (nrow(vets_slate) == 0L) return(.empty_slate_proj_df_inner())

  rosters_like <- data.frame(
    full_name = vets_slate$full_name,
    gsis_id   = vets_slate$gsis_id,
    team      = vets_slate$team_abbr,
    position  = vets_slate$position,
    stringsAsFactors = FALSE
  )
  v0 <- .project_v0(rosters_like, historical_stats_df, scoring_cfg)
  v0$position_rank <- NULL
  v0$vor           <- NULL

  v0 <- merge(
    v0,
    data.frame(gsis_id     = vets_slate$gsis_id,
               underdog_id = vets_slate$underdog_id,
               stringsAsFactors = FALSE),
    by = "gsis_id", sort = FALSE)

  data.frame(
    underdog_id = v0$underdog_id,
    name        = v0$name,
    team        = v0$team,
    position    = v0$position,
    gsis_id     = v0$gsis_id,
    season_mean = v0$season_mean,
    season_std  = v0$season_std,
    season_p10  = v0$season_p10, season_p25 = v0$season_p25,
    season_p50  = v0$season_p50, season_p75 = v0$season_p75,
    season_p90  = v0$season_p90, season_p95 = v0$season_p95,
    stringsAsFactors = FALSE
  )
}

#' Rookie projections for slate-shaped rows
#'
#' Thin wrapper that normalizes the rookie projection output into the
#' same column layout as the veteran path (so they can be `rbind`'d).
#' Rookies have no `gsis_id` — slate's `underdog_id` is the canonical key.
#'
#' @keywords internal
.project_slate_rookies <- function(rookies_slate, drafts_df,
                                   historical_stats_df, scoring_cfg, season) {
  if (nrow(rookies_slate) == 0L) return(.empty_slate_proj_df_inner())
  rp <- project_rookies(rookies_slate, drafts_df, historical_stats_df,
                        scoring_cfg, season)
  data.frame(
    underdog_id = rp$underdog_id,
    name        = rp$full_name,
    team        = rp$team_abbr,
    position    = rp$position,
    gsis_id     = NA_character_,
    season_mean = rp$season_mean,
    season_std  = rp$season_std,
    season_p10  = rp$season_p10, season_p25 = rp$season_p25,
    season_p50  = rp$season_p50, season_p75 = rp$season_p75,
    season_p90  = rp$season_p90, season_p95 = rp$season_p95,
    stringsAsFactors = FALSE
  )
}

#' Pure inner: veteran projection from already-loaded data
#'
#' Unchanged from v0 — still the canonical weekly→season computation,
#' just no longer the public entry point. Operates on a rosters-shaped
#' frame (one row per veteran, keyed by `gsis_id`) and a historical
#' player_stats frame. Returns a projections data frame with rank and
#' VOR computed within its input set; slate callers strip those and
#' recompute across the merged veteran + rookie pool.
#'
#' @param rosters_df Roster table (or slate-veteran shim). Required
#'   columns: `gsis_id`, `full_name`, `team`, `position`.
#' @param historical_stats_df Weekly player stats — one row per
#'   player-game (`player_id`, `position`, `season`, plus `week` or
#'   `season_type` and scoring stats).
#' @param scoring_cfg `bbbro_scoring` object.
#' @keywords internal
.project_v0 <- function(rosters_df, historical_stats_df, scoring_cfg) {
  skill_positions <- c("QB", "RB", "WR", "TE")

  # --- Prep rosters: skill players only, dedupe to one row per gsis_id ---
  rosters_df <- rosters_df[rosters_df$position %in% skill_positions, ,
                            drop = FALSE]
  rosters_df <- rosters_df[!is.na(rosters_df$gsis_id), , drop = FALSE]
  rosters_df <- rosters_df[!duplicated(rosters_df$gsis_id), , drop = FALSE]

  # --- Prep historical: skill players, regular season only ---
  historical_stats_df <- historical_stats_df[
    historical_stats_df$position %in% skill_positions &
      !is.na(historical_stats_df$player_id), , drop = FALSE]
  if ("season_type" %in% names(historical_stats_df)) {
    historical_stats_df <- historical_stats_df[
      is.na(historical_stats_df$season_type) |
        historical_stats_df$season_type == "REG", , drop = FALSE]
  } else if ("week" %in% names(historical_stats_df)) {
    historical_stats_df <- historical_stats_df[
      is.na(historical_stats_df$week) |
        historical_stats_df$week <= 18L, , drop = FALSE]
  }

  # --- Fantasy points for each player-week ---
  historical_stats_df$fp <-
    compute_fantasy_points(historical_stats_df, scoring_cfg)

  # --- Restrict to each player's most recent season ---
  recent_season <- tapply(historical_stats_df$season,
                          historical_stats_df$player_id, max)
  recent <- historical_stats_df[
    historical_stats_df$season ==
      recent_season[historical_stats_df$player_id], , drop = FALSE]

  # --- Per-player season projection from weekly FP ---
  #   season mean = sum of weekly FP
  #   season std  = weekly sd scaled to a full season by sqrt(games_played),
  #                 preserving each player's own week-to-week variance.
  weekly_fp <- split(recent$fp, recent$player_id)
  prior <- data.frame(
    player_id  = names(weekly_fp),
    prior_mean = vapply(weekly_fp, sum, numeric(1)),
    prior_std  = vapply(weekly_fp, function(x) {
      if (length(x) >= 2L) stats::sd(x) * sqrt(length(x)) else NA_real_
    }, numeric(1)),
    stringsAsFactors = FALSE
  )
  prior$position <- recent$position[match(prior$player_id, recent$player_id)]

  # --- Position-level fallbacks for players with no prior data ---
  pos_stats <- do.call(rbind, lapply(skill_positions, function(p) {
    sub <- prior[prior$position == p, ]
    data.frame(
      position        = p,
      pos_median_mean = stats::median(sub$prior_mean, na.rm = TRUE),
      pos_median_std  = stats::median(sub$prior_std,  na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))

  # --- Join rosters to prior projections and position fallbacks ---
  proj <- data.frame(
    name      = rosters_df$full_name,
    team      = rosters_df$team,
    position  = rosters_df$position,
    gsis_id   = rosters_df$gsis_id,
    stringsAsFactors = FALSE
  )
  m <- match(proj$gsis_id, prior$player_id)
  proj$prior_mean <- prior$prior_mean[m]
  proj$prior_std  <- prior$prior_std[m]
  proj            <- merge(proj, pos_stats, by = "position", sort = FALSE)

  # --- season_mean / season_std: player's own value, else position median ---
  proj$season_mean <- ifelse(is.na(proj$prior_mean),
                              proj$pos_median_mean,
                              proj$prior_mean)
  proj$season_std  <- ifelse(is.na(proj$prior_std),
                              proj$pos_median_std,
                              proj$prior_std)

  # --- Normal-distribution percentiles ---
  proj$season_p10 <- stats::qnorm(0.10, proj$season_mean, proj$season_std)
  proj$season_p25 <- stats::qnorm(0.25, proj$season_mean, proj$season_std)
  proj$season_p50 <- proj$season_mean
  proj$season_p75 <- stats::qnorm(0.75, proj$season_mean, proj$season_std)
  proj$season_p90 <- stats::qnorm(0.90, proj$season_mean, proj$season_std)
  proj$season_p95 <- stats::qnorm(0.95, proj$season_mean, proj$season_std)

  .add_position_rank_and_vor(proj)
}

#' Add within-slate position_rank + VOR to a slate projections frame
#'
#' Ranks rookies and veterans against the same pool; replacement levels
#' (QB12, RB24, WR36, TE12 — 12 teams × scoring starts) are drawn from
#' the in-slate distribution so different slate sizes (Eliminator,
#' Superflex, etc.) self-calibrate.
#'
#' @keywords internal
.add_slate_rank_and_vor <- function(proj) {
  proj <- proj[order(proj$position, -proj$season_mean), , drop = FALSE]
  proj$position_rank <- with(proj, ave(season_mean, position,
                                        FUN = function(x) {
                                          paste0("", seq_along(x))
                                        }))
  proj$position_rank <- paste0(proj$position, proj$position_rank)

  replacement_ranks <- c(QB = 12L, RB = 24L, WR = 36L, TE = 12L)
  replacement_levels <- vapply(names(replacement_ranks), function(p) {
    pos_means <- sort(proj$season_mean[proj$position == p],
                       decreasing = TRUE)
    rk <- replacement_ranks[[p]]
    if (length(pos_means) >= rk) pos_means[rk] else 0
  }, numeric(1))
  proj$vor <- pmax(0, proj$season_mean - replacement_levels[proj$position])

  col_order <- c("underdog_id", "gsis_id", "name", "team", "position",
                 "season_mean", "season_std",
                 "season_p10", "season_p25", "season_p50",
                 "season_p75", "season_p90", "season_p95",
                 "position_rank", "vor",
                 "adp", "underdog_projected_points")
  col_order <- intersect(col_order, names(proj))
  proj <- proj[, col_order, drop = FALSE]
  proj[order(-proj$vor), , drop = FALSE]
}

#' Add position_rank and VOR to a `.project_v0` output (legacy helper)
#'
#' Preserved so [.project_v0()] continues to return rank/VOR for its
#' direct callers (tests and any non-slate code paths).
#'
#' @keywords internal
.add_position_rank_and_vor <- function(proj) {
  proj <- proj[order(proj$position, -proj$season_mean), , drop = FALSE]
  proj$position_rank <- with(proj, ave(season_mean, position,
                                        FUN = function(x) {
                                          paste0("", seq_along(x))
                                        }))
  proj$position_rank <- paste0(proj$position, proj$position_rank)

  replacement_ranks <- c(QB = 12L, RB = 24L, WR = 36L, TE = 12L)
  replacement_levels <- vapply(names(replacement_ranks), function(p) {
    pos_means <- sort(proj$season_mean[proj$position == p],
                       decreasing = TRUE)
    rk <- replacement_ranks[[p]]
    if (length(pos_means) >= rk) pos_means[rk] else 0
  }, numeric(1))
  proj$vor <- pmax(0, proj$season_mean - replacement_levels[proj$position])

  proj <- proj[, c("name", "team", "position", "gsis_id",
                   "season_mean", "season_std",
                   "season_p10", "season_p25", "season_p50",
                   "season_p75", "season_p90", "season_p95",
                   "position_rank", "vor")]
  proj[order(-proj$vor), , drop = FALSE]
}

#' Empty slate projection (final shape) — used when slate is empty
#' @keywords internal
.empty_slate_proj_df <- function() {
  data.frame(
    underdog_id = character(0),
    gsis_id     = character(0),
    name        = character(0),
    team        = character(0),
    position    = character(0),
    season_mean = numeric(0), season_std = numeric(0),
    season_p10  = numeric(0), season_p25 = numeric(0), season_p50 = numeric(0),
    season_p75  = numeric(0), season_p90 = numeric(0), season_p95 = numeric(0),
    position_rank = character(0), vor = numeric(0),
    adp = numeric(0), underdog_projected_points = numeric(0),
    stringsAsFactors = FALSE
  )
}

#' Empty intermediate slate projection (pre-merge) — vet/rookie path shape
#' @keywords internal
.empty_slate_proj_df_inner <- function() {
  data.frame(
    underdog_id = character(0),
    name        = character(0),
    team        = character(0),
    position    = character(0),
    gsis_id     = character(0),
    season_mean = numeric(0), season_std = numeric(0),
    season_p10  = numeric(0), season_p25 = numeric(0), season_p50 = numeric(0),
    season_p75  = numeric(0), season_p90 = numeric(0), season_p95 = numeric(0),
    stringsAsFactors = FALSE
  )
}

#' Apply user "knowing ball" adjustments (stub)
#' @export
apply_adjustments <- function(projections, adjustments_path) {
  cli::cli_abort("Not yet implemented (v0.1 milestone)")
}
