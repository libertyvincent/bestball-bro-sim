#' Write the projection feed to JSON per FEED_SPEC
#'
#' Serializes a projection data frame to `v1/projections/nfl_{season}.json`
#' under `out_dir`, following the per-player schema in FEED_SPEC.md. The `v1/`
#' prefix keeps our feed segregated from the legacy Clay `projections/` tree
#' that lives at the root of bestball-bro-data.
#'
#' v0 includes the required fields plus the optional ones we already have
#' (gsis_id, season percentiles, games_played). Omits weekly / stats /
#' correlations until those become available in v1+.
#'
#' @param projections Data frame from `generate_projections()`.
#' @param out_dir Output directory (typically a `bestball-bro-data/` sibling).
#' @param season NFL season for the filename (defaults to current).
#' @param model_version Feed model version string written into _meta.
#' @return The full output path, invisibly.
#' @export
publish_projections <- function(projections,
                                out_dir = "../bestball-bro-data",
                                season = NULL,
                                model_version = "0.0.1") {
  season <- season %||% current_season()
  feed <- .projections_to_feed(projections, season, model_version)
  out_path <- file.path(out_dir, "v1", "projections", paste0("nfl_", season, ".json"))
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(feed, out_path, auto_unbox = TRUE, pretty = TRUE,
                       null = "null", na = "null")
  cli::cli_alert_success("Wrote {nrow(projections)} players to {.path {out_path}}")
  invisible(out_path)
}

#' Convert a projection data frame to the FEED_SPEC list shape (pure)
#'
#' Separated for testability — no I/O.
#'
#' @keywords internal
.projections_to_feed <- function(projections, season, model_version = "0.0.1") {
  players <- lapply(seq_len(nrow(projections)), function(i) {
    row <- projections[i, ]
    list(
      name              = row$name,
      team              = row$team,
      position          = row$position,
      gsis_id           = row$gsis_id,
      season = list(
        mean   = round(row$season_mean, 1),
        median = round(row$season_p50,  1),
        std    = round(row$season_std,  1),
        percentiles = list(
          p10 = round(row$season_p10, 1),
          p25 = round(row$season_p25, 1),
          p50 = round(row$season_p50, 1),
          p75 = round(row$season_p75, 1),
          p90 = round(row$season_p90, 1),
          p95 = round(row$season_p95, 1)
        ),
        games_played = list(mean = 16, p10 = 13, p50 = 16, p90 = 17)
      ),
      vor              = round(row$vor, 1),
      position_rank    = row$position_rank
    )
  })

  list(
    `_meta` = list(
      season              = season,
      scoring             = "half_ppr_underdog",
      generated_at        = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      model_version       = model_version,
      n_players           = length(players),
      methodology         = "v0_prior_season_passthrough"
    ),
    players = players
  )
}

#' Write the manifest `_meta.json`
#'
#' Inventories all published files with paths, versions, and sha256 hashes.
#' The extension fetches this first to detect updates and decide what to fetch.
#'
#' v0 includes only the projections file. Tournament configs / building blocks
#' get added when their publishers ship.
#'
#' @param out_dir Output directory.
#' @param season NFL season.
#' @param model_version Feed version string.
#' @return Path to the written manifest, invisibly.
#' @export
publish_manifest <- function(out_dir = "../bestball-bro-data",
                             season = NULL,
                             model_version = "0.0.1") {
  season <- season %||% current_season()

  manifest <- list(
    season       = season,
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    files        = list(),
    tournaments  = list(),
    scoring_systems = list()
  )

  # Add projections file if it exists
  proj_rel  <- paste0("v1/projections/nfl_", season, ".json")
  proj_path <- file.path(out_dir, proj_rel)
  if (file.exists(proj_path)) {
    manifest$files$projections <- list(
      path    = proj_rel,
      version = model_version,
      sha256  = .file_sha256(proj_path)
    )
  }

  out_path <- file.path(out_dir, "_meta.json")
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(manifest, out_path, auto_unbox = TRUE, pretty = TRUE,
                       null = "null", na = "null")
  cli::cli_alert_success("Wrote manifest to {.path {out_path}}")
  invisible(out_path)
}

#' Compute SHA-256 of a file
#'
#' Uses the `digest` package if available; falls back to openssl shell-out.
#'
#' @keywords internal
.file_sha256 <- function(path) {
  if (requireNamespace("digest", quietly = TRUE)) {
    return(digest::digest(path, algo = "sha256", file = TRUE))
  }
  # Fallback for environments without digest installed
  res <- tryCatch(
    system2("sha256sum", args = shQuote(path), stdout = TRUE, stderr = TRUE),
    error = function(e) ""
  )
  if (length(res) > 0 && nchar(res[1]) > 0) {
    sub(" .*$", "", res[1])
  } else {
    cli::cli_warn("Could not compute SHA-256 (install the `digest` package).")
    ""
  }
}

# ---- Below: stubs awaiting v0.1+ ----

#' Write sim draws to parquet
#' @export
publish_sim_draws <- function(sim_draws, out_dir = "../bestball-bro-data",
                              season = NULL) {
  cli::cli_abort("Not yet implemented (deferred until run_season_sims() lands)")
}

#' Write Layer B building blocks for one tournament to JSON
#' @export
publish_building_blocks <- function(tournament_id, building_blocks,
                                     out_dir = "../bestball-bro-data") {
  cli::cli_abort("Not yet implemented (deferred until Layer B precompute lands)")
}

#' Write tournaments_index.json
#' @export
publish_tournaments_index <- function(out_dir = "../bestball-bro-data") {
  cli::cli_abort("Not yet implemented (deferred until contest-title aliases are settled)")
}
