# (a)-path invariance harness: blend the Season slate and write the feed to the
# path given as arg. Run once with the CB->WR source remap applied and once
# without (git stash the field_empirics.R change); the two feeds must be
# byte-identical, proving season_mean/std/VOR/tiers (and therefore the tensor /
# projections built from the feed) do not route through load_scraped_drafts.
suppressMessages(devtools::load_all(".", quiet = TRUE))
args <- commandArgs(trailingOnly = TRUE)
out  <- if (length(args)) args[[1]] else "build/feed_invariance.json"

feed <- blend_slate(
  "nfl_2026_season",
  .inst_path("data/sources", "_manifest.yaml"),
  .inst_path("data/slates",  "_manifest.yaml"),
  out_path    = out,
  write_json  = TRUE
)
cat(sprintf("wrote feed: %s  (%d players)\n", out, length(feed$players)))
