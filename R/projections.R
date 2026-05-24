#' Generate Layer A player projections (v0 — naive)
#'
#' v0 implementation: projects each player's most recent regular season.
#' Season mean is the sum of their weekly fantasy points; season std is their
#' week-to-week sd scaled to a full season (`sd * sqrt(games_played)`), which
#' preserves each player's own variance; then normal-distribution percentiles.
#' Players without prior-season stats (rookies, position-change,
#' returning-from-absence) get their position's median.
#'
#' Intentionally naive. v1 will replace with the hybrid methodology in
#' LAYER_A.md (top-down team allocation + component models + comparable
#' players for variance shape).
#'
#' @param season NFL season (defaults to current).
#' @param scoring_id Scoring system ID (defaults to `"half_ppr_underdog"`).
#' @param adjustments_path Optional path to a user adjustments YAML.
#' @param history_years How many prior seasons to use for the historical
#'   variance fit. Defaults to 5.
#' @return A data frame with columns: name, team, position, gsis_id,
#'   season_mean, season_std, season_p10..p95, position_rank, vor.
#' @export
generate_projections <- function(season = NULL,
                                 scoring_id = "half_ppr_underdog",
                                 adjustments_path = NULL,
                                 history_years = 5L) {
  season <- season %||% current_season()
  cli::cli_alert_info("Generating v0 projections for {season}")

  cli::cli_alert("Loading rosters for {season}")
  rosters <- pull_rosters(season)

  cli::cli_alert("Loading historical player stats ({season - history_years} - {season - 1})")
  historical <- pull_player_stats((season - history_years):(season - 1))

  cli::cli_alert("Loading scoring config: {scoring_id}")
  scoring <- load_scoring_config(scoring_id)

  .project_v0(rosters, historical, scoring)
}

#' Pure inner: project from already-loaded data
#'
#' Separated for testability — accepts data frames directly, no network.
#'
#' @param rosters_df Roster table (from nflreadr::load_rosters). Required
#'   columns: `gsis_id`, `full_name`, `team`, `position`.
#' @param historical_stats_df Weekly player stats — one row per player-game,
#'   e.g. from `pull_player_stats()`. Required columns: `player_id`,
#'   `position`, `season`, plus `week` or `season_type` and scoring stats.
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

  # --- Position rank ---
  proj <- proj[order(proj$position, -proj$season_mean), ]
  proj$position_rank <- with(proj, ave(season_mean, position,
                                        FUN = function(x) {
                                          paste0("", seq_along(x))
                                        }))
  proj$position_rank <- paste0(proj$position, proj$position_rank)

  # --- VOR vs replacement level ---
  # Replacement: 12 teams x scoring_starts (QB=1, RB=2, WR=3, TE=1)
  # = QB12, RB24, WR36, TE12.
  replacement_ranks <- c(QB = 12L, RB = 24L, WR = 36L, TE = 12L)
  replacement_levels <- vapply(names(replacement_ranks), function(p) {
    pos_means <- sort(proj$season_mean[proj$position == p],
                       decreasing = TRUE)
    rk <- replacement_ranks[[p]]
    if (length(pos_means) >= rk) pos_means[rk] else 0
  }, numeric(1))
  proj$vor <- pmax(0, proj$season_mean - replacement_levels[proj$position])

  # --- Final select + sort ---
  proj <- proj[, c("name", "team", "position", "gsis_id",
                   "season_mean", "season_std",
                   "season_p10", "season_p25", "season_p50",
                   "season_p75", "season_p90", "season_p95",
                   "position_rank", "vor")]
  proj[order(-proj$vor), ]
}

#' Apply user "knowing ball" adjustments (stub)
#' @export
apply_adjustments <- function(projections, adjustments_path) {
  cli::cli_abort("Not yet implemented (v0.1 milestone)")
}
