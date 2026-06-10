# Verify _meta.availability_adjustment is logged and dump the review list.
suppressMessages(devtools::load_all(".", quiet = TRUE))

feed <- blend_slate(
  "nfl_2026_season",
  "inst/data/sources/_manifest.yaml",
  "inst/data/slates/_manifest.yaml",
  tempfile(fileext = ".json"),
  write_json = FALSE
)

cat("\n=== _meta$availability_adjustment ===\n")
cat(jsonlite::toJSON(feed[["_meta"]]$availability_adjustment,
                     auto_unbox = TRUE, pretty = TRUE))
cat("\n\n=== Brooks-class review file ===\n")
cat(readLines("build/availability_review_nfl_2026_season.txt"), sep = "\n")
cat("\n")
