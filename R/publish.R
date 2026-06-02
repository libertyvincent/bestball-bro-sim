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
