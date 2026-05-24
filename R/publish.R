#' Write a slate's projection feed to JSON
#'
#' v1 / slate architecture: each slate gets its own file at
#' `v1/projections/<slate_id>.json` under `out_dir`. The per-player
#' record carries `underdog_id` (canonical key), `gsis_id` (null for
#' rookies), our `season.*` projection, derived `vor` /
#' `position_rank`, plus the slate's `adp` and Underdog's own
#' `underdog_projected_points` as a reference-only field.
#'
#' @param projections Data frame from [generate_projections()].
#' @param out_dir Output directory (root of the feed tree, typically a
#'   `bestball-bro-data/` sibling). The `v1/projections/` subtree is
#'   created if it does not exist.
#' @param slate_id Slate ID — used both as the filename and as
#'   `_meta.slate_id`.
#' @param slate_meta List with the slate's manifest entry
#'   (`underdog_slate_id`, `display_name`, `season`, `scoring_id`).
#' @param model_version Feed model version string written into `_meta`.
#' @return The full output path, invisibly.
#' @export
publish_projections <- function(projections, out_dir, slate_id, slate_meta,
                                model_version = "1.0.0") {
  feed <- .projections_to_feed(projections, slate_id, slate_meta, model_version)
  out_path <- file.path(out_dir, "v1", "projections",
                        paste0(slate_id, ".json"))
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(feed, out_path, auto_unbox = TRUE, pretty = TRUE,
                       null = "null", na = "null")
  cli::cli_alert_success("Wrote {nrow(projections)} players to {.path {out_path}}")
  invisible(out_path)
}

#' Convert a slate projection data frame to the FEED_SPEC list shape (pure)
#'
#' Separated for testability — no I/O.
#'
#' @keywords internal
.projections_to_feed <- function(projections, slate_id, slate_meta,
                                 model_version = "1.0.0") {
  players <- lapply(seq_len(nrow(projections)), function(i) {
    row <- projections[i, ]
    list(
      underdog_id = row$underdog_id,
      gsis_id     = if (is.na(row$gsis_id)) NULL else row$gsis_id,
      name        = row$name,
      team        = row$team,
      position    = row$position,
      season = list(
        mean   = round(row$season_mean, 1),
        std    = round(row$season_std,  1),
        median = round(row$season_p50,  1),
        percentiles = list(
          p10 = round(row$season_p10, 1),
          p25 = round(row$season_p25, 1),
          p50 = round(row$season_p50, 1),
          p75 = round(row$season_p75, 1),
          p90 = round(row$season_p90, 1),
          p95 = round(row$season_p95, 1)
        )
      ),
      vor                       = round(row$vor, 1),
      position_rank             = row$position_rank,
      adp                       = if (is.na(row$adp)) NULL else row$adp,
      underdog_projected_points = if (is.na(row$underdog_projected_points)) NULL
                                  else row$underdog_projected_points
    )
  })

  list(
    `_meta` = list(
      slate_id          = slate_id,
      underdog_slate_id = slate_meta$underdog_slate_id,
      display_name      = slate_meta$display_name,
      season            = slate_meta$season,
      scoring_id        = slate_meta$scoring_id,
      methodology       = "v1_nflverse_veterans_comparables_rookies",
      generated_at      = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      model_version     = model_version,
      player_count      = length(players)
    ),
    players = players
  )
}

#' Write the multi-slate manifest `_meta.json`
#'
#' Inventories every slate this feed publishes. Keyed by `slate_id` so
#' the extension can look up a slate by its Underdog UUID, fetch the
#' projections file, and verify content via `sha256`.
#'
#' @param out_dir Output directory (feed root).
#' @param slates Named list of slate entries — `names(slates)` are the
#'   slate IDs; each value is a list with `underdog_slate_id`, `path`
#'   (relative to feed root), and `version`. `sha256` is computed here
#'   from the file on disk.
#' @param season NFL season (carried in the manifest header).
#' @return Path to the written manifest, invisibly.
#' @export
publish_manifest <- function(out_dir, slates, season) {
  manifest <- list(
    season       = as.integer(season),
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    slates       = list()
  )
  for (sid in names(slates)) {
    entry <- slates[[sid]]
    file_abs <- file.path(out_dir, entry$path)
    manifest$slates[[sid]] <- list(
      underdog_slate_id = entry$underdog_slate_id,
      path              = entry$path,
      sha256            = if (file.exists(file_abs)) .file_sha256(file_abs)
                          else "",
      version           = entry$version
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
