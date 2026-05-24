devtools::load_all(".")

# v1 / slate architecture: loop over every slate marked enabled in the
# manifest, project it, publish it, then write a single multi-slate
# _meta.json at the feed root.
#
# Target layout:
#   real_v0_feed/_meta.json                          (multi-slate manifest)
#   real_v0_feed/v1/projections/<slate_id>.json      (one per enabled slate)

out_dir <- "C:/Users/vince/Desktop/real_v0_feed"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

manifest <- load_slate_manifest()
enabled  <- Filter(function(e) isTRUE(e$enabled), manifest)
if (length(enabled) == 0L) {
  stop("No enabled slates in inst/data/slates/_manifest.yaml")
}

model_version <- "1.0.0"
season_for_meta <- NULL
slates_for_meta <- list()

for (slate_id in names(enabled)) {
  entry <- enabled[[slate_id]]
  cat("\n=== Slate:", slate_id, "===\n")

  proj <- generate_projections(slate_id = slate_id)
  publish_projections(proj, out_dir = out_dir, slate_id = slate_id,
                      slate_meta = entry, model_version = model_version)

  slates_for_meta[[slate_id]] <- list(
    underdog_slate_id = entry$underdog_slate_id,
    path              = paste0("v1/projections/", slate_id, ".json"),
    version           = model_version
  )
  if (is.null(season_for_meta)) season_for_meta <- entry$season
  cat("  Player count:", nrow(proj), "\n")
}

publish_manifest(out_dir = out_dir, slates = slates_for_meta,
                 season = season_for_meta)
cat("\nDone. Files written to:", out_dir, "\n")
cat("  manifest    :", file.path(out_dir, "_meta.json"), "\n")
for (sid in names(slates_for_meta)) {
  cat("  projections :",
      file.path(out_dir, slates_for_meta[[sid]]$path), "\n")
}
