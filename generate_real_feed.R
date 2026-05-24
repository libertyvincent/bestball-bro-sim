devtools::load_all(".")

# --- Choose season: 2026 if rosters exist, else fall back to 2025 ----------
season <- 2026
tryCatch({
  rosters_check <- nflreadr::load_rosters(season)
  if (nrow(rosters_check) == 0) {
    season <- 2025
    cat("2026 rosters empty, falling back to 2025\n")
  }
}, error = function(e) {
  season <<- 2025
  cat("2026 rosters not available, falling back to 2025\n")
})
cat("Generating projections for season:", season, "\n")

projections <- generate_projections(season = season)

# --- Publish in the v1/ versioned layout -----------------------------------
# Target:  real_v0_feed/_meta.json                       (manifest, at root)
#          real_v0_feed/v1/projections/nfl_<season>.json  (projections)
#
# publish_projections()/publish_manifest() take a directory and build the
# `projections/...` + `_meta.json` paths themselves; they don't know about the
# v1/ version dir. So we point them at real_v0_feed/v1, then relocate _meta.json
# up to the feed root and rewrite the projections path to be root-relative.
out_dir <- "C:/Users/vince/Desktop/real_v0_feed"
v1_dir  <- file.path(out_dir, "v1")
dir.create(v1_dir, showWarnings = FALSE, recursive = TRUE)

proj_path <- publish_projections(projections, out_dir = v1_dir, season = season)

publish_manifest(v1_dir, season = season)
manifest <- jsonlite::read_json(file.path(v1_dir, "_meta.json"))
manifest$files$projections$path <- paste0("v1/", manifest$files$projections$path)
jsonlite::write_json(manifest, file.path(out_dir, "_meta.json"),
                     auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null")
file.remove(file.path(v1_dir, "_meta.json"))

cat("Done. Files written to:", out_dir, "\n")
cat("  manifest    :", file.path(out_dir, "_meta.json"), "\n")
cat("  projections :", proj_path, "\n")
cat("Player count :", nrow(projections), "\n")
