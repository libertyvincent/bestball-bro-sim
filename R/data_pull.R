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

