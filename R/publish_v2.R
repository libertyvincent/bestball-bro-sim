#' Publish v2 blended-consensus projections for all enabled slates
#'
#' Loops over `inst/data/slates/_manifest.yaml`, runs [blend_slate()]
#' on each enabled slate (writing `v2/projections/<slate_id>.json` to
#' `out_dir`), and updates the feed root's `_meta.json` to register v2
#' paths + checksums alongside any v1 entries already there.
#'
#' Sprint 2 keeps v1 and v2 published in parallel; the extension flips
#' to v2 in Sprint 4. Until then both feeds publish and v2 is the audit
#' surface for the new methodology.
#'
#' @param out_dir Output directory (feed root). Defaults to `"build"`.
#' @param cache_dir HTTP cache directory passed through to [blend_slate()].
#' @return Invisibly `TRUE`.
#' @export
publish_v2 <- function(out_dir = "build",
                       cache_dir = file.path("~", ".bestball-bro", "cache")) {
  sources_path <- .inst_path("data/sources", "_manifest.yaml")
  if (sources_path == "") {
    cli::cli_abort(c(
      "Sources manifest not found.",
      i = "Looked for: {.path inst/data/sources/_manifest.yaml}"
    ))
  }
  slates_path <- .inst_path("data/slates", "_manifest.yaml")
  slates      <- load_slate_manifest()
  enabled_ids <- names(Filter(function(s) isTRUE(s$enabled), slates))

  cli::cli_alert_info("publish_v2: {length(enabled_ids)} enabled slate(s)")

  for (sid in enabled_ids) {
    out_path <- file.path(out_dir, "v2", "projections",
                          paste0(sid, ".json"))
    blend_slate(
      slate_id              = sid,
      sources_manifest_path = sources_path,
      slates_manifest_path  = slates_path,
      out_path              = out_path,
      cache_dir             = cache_dir
    )
  }

  .update_root_meta_for_v2(out_dir, enabled_ids, slates)
  invisible(TRUE)
}

#' Register v2 outputs in the feed root's `_meta.json`
#'
#' Preserves whatever the v1 path already wrote -- we only add a
#' `v2_path`, `v2_sha256`, `v2_generated_at` triple per slate. The
#' extension can read the file blind and pick v1 or v2 by which fields
#' exist.
#' @keywords internal
.update_root_meta_for_v2 <- function(out_dir, slate_ids, slate_manifest) {
  meta_path <- file.path(out_dir, "_meta.json")
  manifest  <- if (file.exists(meta_path)) {
    jsonlite::fromJSON(meta_path, simplifyVector = FALSE)
  } else {
    list(season = NULL, generated_at = NULL, slates = list())
  }
  if (is.null(manifest$slates)) manifest$slates <- list()

  for (sid in slate_ids) {
    v2_rel <- file.path("v2", "projections", paste0(sid, ".json"))
    v2_abs <- file.path(out_dir, v2_rel)
    if (!file.exists(v2_abs)) next
    entry  <- manifest$slates[[sid]] %||% list()
    entry$underdog_slate_id <- entry$underdog_slate_id %||%
      slate_manifest[[sid]]$underdog_slate_id
    entry$v2_path           <- v2_rel
    entry$v2_sha256         <- .file_sha256(v2_abs)
    entry$v2_generated_at   <- format(Sys.time(),
                                       "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    manifest$slates[[sid]] <- entry
  }
  if (is.null(manifest$generated_at)) {
    manifest$generated_at <- format(Sys.time(),
                                     "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  }

  dir.create(dirname(meta_path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(manifest, meta_path, auto_unbox = TRUE, pretty = TRUE,
                       null = "null", na = "null")
  cli::cli_alert_success("Updated v2 entries in {.path {meta_path}}")
  invisible(meta_path)
}
