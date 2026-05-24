#' Load the slate manifest
#'
#' The slate manifest at `inst/data/slates/_manifest.yaml` is the registry
#' of every Underdog NFL slate this package projects for. Each entry pins a
#' slate to its CSV file (the canonical player universe), display name,
#' season, and scoring config. Slates with `enabled: false` are stubs
#' awaiting their CSV — `generate_real_feed.R` skips them.
#'
#' @return A list keyed by `slate_id` with `underdog_slate_id`, `display_name`,
#'   `season`, `scoring_id`, `csv_file`, `enabled`.
#' @export
load_slate_manifest <- function() {
  path <- .inst_path("data/slates", "_manifest.yaml")
  if (path == "") {
    cli::cli_abort(c(
      "Slate manifest not found.",
      i = "Looked for: {.path inst/data/slates/_manifest.yaml}"
    ))
  }
  yaml::read_yaml(path)$slates
}

#' Load a slate's normalized player universe
#'
#' Reads the slate's Underdog CSV and returns a tibble-shaped data frame with
#' our internal column names. The CSV is the canonical player universe — every
#' projection we emit for the slate is for a row in this file (by
#' `underdog_id`). Underdog's own `projectedPoints` is preserved as
#' `underdog_projected_points` (reference only — does NOT feed our projection).
#'
#' @param slate_id Slate ID from `load_slate_manifest()`, e.g.
#'   `"nfl_2026_season"`.
#' @return Data frame with columns: `underdog_id`, `first_name`, `last_name`,
#'   `full_name`, `team_abbr`, `position`, `adp`, `projected_points`,
#'   `salary`, `position_rank`, `lineup_status`, `bye_week`.
#' @export
load_slate_data <- function(slate_id) {
  manifest <- load_slate_manifest()
  entry <- manifest[[slate_id]]
  if (is.null(entry)) {
    cli::cli_abort("Unknown slate_id: {.val {slate_id}}")
  }
  csv_path <- .inst_path("data/slates", entry$csv_file)
  if (csv_path == "") {
    cli::cli_abort(c(
      "Slate CSV not found.",
      i = "Looked for: {.path inst/data/slates/{entry$csv_file}}"
    ))
  }
  raw <- utils::read.csv(csv_path, stringsAsFactors = FALSE,
                         na.strings = c("", "NA", "-"))
  full_name <- trimws(paste(raw$firstName, raw$lastName))
  adp <- suppressWarnings(as.numeric(raw$adp))
  proj <- suppressWarnings(as.numeric(raw$projectedPoints))
  data.frame(
    underdog_id      = raw$id,
    first_name       = raw$firstName,
    last_name        = raw$lastName,
    full_name        = full_name,
    team_abbr        = team_full_to_abbr(raw$teamName),
    position         = raw$slotName,
    adp              = adp,
    projected_points = proj,
    salary           = suppressWarnings(as.numeric(raw$salary)),
    position_rank    = raw$positionRank,
    lineup_status    = raw$lineupStatus,
    bye_week         = suppressWarnings(as.integer(raw$byeWeek)),
    stringsAsFactors = FALSE
  )
}

#' Map an NFL team's full name to its nflverse abbreviation
#'
#' Underdog's CSV uses display names ("Atlanta Falcons", "Los Angeles
#' Rams"); nflverse uses two/three-letter abbreviations ("ATL", "LA").
#' All matches use the **nflverse roster convention**: `AZ` (not ARI),
#' `LA` (not LAR), `NO` (not NOR), `KC` (not KAN), `GB` (not GNB),
#' `NE` (not NWE), `SF` (not SFO), `TB` (not TAM), `LV` (not LVR), `JAX`.
#'
#' Unknown / missing team names return `NA_character_` — common for slate
#' rows where the player is a free agent.
#'
#' @param team_name Character vector of full team names.
#' @return Character vector of abbreviations, same length as `team_name`.
#' @export
team_full_to_abbr <- function(team_name) {
  map <- c(
    "Arizona Cardinals"     = "AZ",
    "Atlanta Falcons"       = "ATL",
    "Baltimore Ravens"      = "BAL",
    "Buffalo Bills"         = "BUF",
    "Carolina Panthers"     = "CAR",
    "Chicago Bears"         = "CHI",
    "Cincinnati Bengals"    = "CIN",
    "Cleveland Browns"      = "CLE",
    "Dallas Cowboys"        = "DAL",
    "Denver Broncos"        = "DEN",
    "Detroit Lions"         = "DET",
    "Green Bay Packers"     = "GB",
    "Houston Texans"        = "HOU",
    "Indianapolis Colts"    = "IND",
    "Jacksonville Jaguars"  = "JAX",
    "Kansas City Chiefs"    = "KC",
    "Las Vegas Raiders"     = "LV",
    "Los Angeles Chargers"  = "LAC",
    "Los Angeles Rams"      = "LA",
    "Miami Dolphins"        = "MIA",
    "Minnesota Vikings"     = "MIN",
    "New England Patriots"  = "NE",
    "New Orleans Saints"    = "NO",
    "New York Giants"       = "NYG",
    "New York Jets"         = "NYJ",
    "Philadelphia Eagles"   = "PHI",
    "Pittsburgh Steelers"   = "PIT",
    "San Francisco 49ers"   = "SF",
    "Seattle Seahawks"      = "SEA",
    "Tampa Bay Buccaneers"  = "TB",
    "Tennessee Titans"      = "TEN",
    "Washington Commanders" = "WAS"
  )
  unname(map[team_name])
}

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

#' Pull NFL draft picks via nflreadr
#'
#' Used by the v1 rookie projection module (`project_rookies()`): the
#' current year identifies who is a rookie and their draft capital;
#' historical years build the comparable distribution.
#'
#' @param seasons Integer vector of draft years. Defaults to a window that
#'   covers `history_years` of comparables plus the current year.
#' @return A tibble of draft picks.
#' @export
pull_draft_picks <- function(seasons = (current_season() - 5L):current_season()) {
  nflreadr::load_draft_picks(seasons)
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

