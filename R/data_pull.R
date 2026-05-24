#' Pull NFL play-by-play data via nflreadr
#'
#' Thin wrapper over [nflreadr::load_pbp()] with the season-range defaults
#' relevant to Layer A modeling. nflreadr handles its own caching; configure
#' via `options(nflreadr.cache = "filesystem")` for cross-session caching.
#'
#' @param seasons Integer vector of NFL seasons. Defaults to last 5 seasons.
#' @return A tibble of play-by-play data.
#' @export
pull_pbp <- function(seasons = (current_season() - 5):(current_season() - 1)) {
  nflreadr::load_pbp(seasons)
}

#' Pull weekly player stats via nflreadr
#'
#' Uses the canonical `stats_player` release tag — the legacy
#' `player_stats_{season}.csv` paths are deprecated per Seb's note in the
#' nflverse Discord (May 2026).
#'
#' @param seasons Integer vector of NFL seasons.
#' @return A tibble of weekly player stats.
#' @export
pull_player_stats <- function(seasons = 2010:(current_season() - 1)) {
  nflreadr::load_player_stats(seasons)
}

#' Pull season rosters via nflreadr
#'
#' Uses the season-level roster release (`load_rosters`) rather than the
#' weekly snapshots: the `weekly_rosters` release is not published until a
#' season is under way, so it 404s for an upcoming season. `.project_v0()`
#' dedupes to one row per `gsis_id` regardless, so season-level is sufficient.
#'
#' @param seasons Integer vector of NFL seasons.
#' @return A tibble of season roster rows.
#' @export
pull_rosters <- function(seasons = current_season()) {
  nflreadr::load_rosters(seasons)
}

#' Pull schedules via nflreadr
#'
#' Note: as of May 2026 there was a known upstream issue where
#' `load_schedules({current})` returned empty if the `release_games` job
#' was failing. This wrapper warns and falls back to the prior season's
#' schedule in that case so downstream code doesn't NPE.
#'
#' @param seasons Integer vector of NFL seasons.
#' @return A tibble of schedule rows.
#' @export
pull_schedules <- function(seasons = current_season()) {
  sched <- nflreadr::load_schedules(seasons)
  if (nrow(sched) == 0 && length(seasons) == 1) {
    cli::cli_warn(c(
      "load_schedules({seasons}) returned empty.",
      i = "Falling back to {seasons - 1}.",
      i = "Check the nflverse Discord for upstream pipeline status."
    ))
    sched <- nflreadr::load_schedules(seasons - 1)
  }
  sched
}

#' Pull injuries / practice participation via nflreadr
#' @export
pull_injuries <- function(seasons = current_season()) {
  nflreadr::load_injuries(seasons)
}

#' Pull ffverse expected fantasy points (load_ff_opportunity)
#'
#' Useful as a feature and as a sanity-check benchmark for our own projections.
#' @export
pull_ff_opportunity <- function(seasons = (current_season() - 3):(current_season() - 1)) {
  nflreadr::load_ff_opportunity(seasons)
}

#' Pull FantasyPros consensus rankings via nflreadr
#'
#' Used as a market-prior anchor for projection validation.
#' @export
pull_ff_rankings <- function() {
  nflreadr::load_ff_rankings()
}

#' Pull a daily snapshot of Underdog ADP
#'
#' Hits the Underdog `/v1/slates/{slate_id}/scoring_types/{scoring_type_id}/appearances`
#' endpoint and persists a timestamped snapshot to `inst/adp_snapshots/`
#' (gitignored — published versions live in `bestball-bro-data`).
#'
#' Used by Layer B for ADP-aware EV calculations and by validation for
#' projection-vs-market divergence reports.
#'
#' @param slate_id Underdog slate UUID.
#' @param scoring_type_id Underdog scoring type UUID; defaults to NFL.
#' @return A tibble of player appearances with current ADP.
#' @export
pull_underdog_adp <- function(slate_id,
                              scoring_type_id = "ccf300b0-9197-5951-bd96-cba84ad71e86") {
  cli::cli_abort(c(
    "Not yet implemented.",
    i = "Decide: httr2 vs curl, auth required vs public read, snapshot persistence path."
  ))
}

