#' Load a scoring configuration from YAML
#'
#' Reads `inst/scoring/{scoring_id}.yaml` and returns a `bbbro_scoring` object.
#'
#' @param scoring_id Scoring config ID, e.g. `"half_ppr_underdog"`.
#' @return A list with class `bbbro_scoring`.
#' @export
#' @examples
#' \dontrun{
#' s <- load_scoring_config("half_ppr_underdog")
#' s$rules$receiving$reception_points  # 0.5
#' }
load_scoring_config <- function(scoring_id) {
  path <- .inst_path("scoring", paste0(scoring_id, ".yaml"))
  if (path == "") {
    cli::cli_abort(c(
      "Scoring config not found.",
      i = "Looked for: {.path inst/scoring/{scoring_id}.yaml}"
    ))
  }
  cfg <- yaml::read_yaml(path)
  class(cfg) <- c("bbbro_scoring", class(cfg))
  cfg
}

#' Compute fantasy points from a stats data frame using a scoring config
#'
#' Vectorized — operates on a data frame and returns a numeric vector with one
#' element per row. Handles missing columns gracefully (treats them as zero),
#' so it works on tibbles from nflreadr even if some optional stat columns
#' (`passing_2pt_conversions`, etc.) are absent in some seasons.
#'
#' Expected column names match nflverse conventions:
#' `passing_yards`, `passing_tds`, `interceptions`,
#' `rushing_yards`, `rushing_tds`,
#' `receptions`, `receiving_yards`, `receiving_tds`,
#' `*_fumbles_lost`, `*_2pt_conversions`.
#'
#' Position-dependent TE premium applied if `scoring_cfg$rules$te_premium_points > 0`
#' and the data frame has a `position` column.
#'
#' @param stats_df A data frame of stats.
#' @param scoring_cfg A `bbbro_scoring` object from `load_scoring_config()`.
#' @return Numeric vector of fantasy points, one per row of `stats_df`.
#' @export
compute_fantasy_points <- function(stats_df, scoring_cfg) {
  r <- scoring_cfg$rules
  n <- nrow(stats_df)

  # Safe accessor: return numeric vector of length n, zeros for NA / missing col
  safe <- function(col) {
    if (!(col %in% names(stats_df))) return(rep(0, n))
    v <- as.numeric(stats_df[[col]])
    v[is.na(v)] <- 0
    v
  }

  passing_pts <-
    safe("passing_yards")           / r$passing$yards_per_point +
    safe("passing_tds")             * r$passing$td_points +
    safe("interceptions")           * r$passing$int_points +
    safe("passing_2pt_conversions") * (r$passing$two_point_points %||% 2)

  rushing_pts <-
    safe("rushing_yards")           / r$rushing$yards_per_point +
    safe("rushing_tds")             * r$rushing$td_points +
    safe("rushing_2pt_conversions") * (r$rushing$two_point_points %||% 2)

  receiving_pts <-
    safe("receptions")                * r$receiving$reception_points +
    safe("receiving_yards")           / r$receiving$yards_per_point +
    safe("receiving_tds")             * r$receiving$td_points +
    safe("receiving_2pt_conversions") * (r$receiving$two_point_points %||% 2)

  # Fumbles - sum across all sources (nflreadr splits by play type)
  fumble_pts <-
    (safe("rushing_fumbles_lost") +
     safe("receiving_fumbles_lost") +
     safe("sack_fumbles_lost")) * (r$rushing$fumble_lost_points %||% -2)

  # TE premium - only applied to TE position when configured
  te_bonus <- rep(0, n)
  premium <- r$te_premium_points %||% 0
  if (premium > 0 && "position" %in% names(stats_df)) {
    te_bonus <- ifelse(stats_df$position == "TE",
                       safe("receptions") * premium, 0)
  }

  passing_pts + rushing_pts + receiving_pts + fumble_pts + te_bonus
}
