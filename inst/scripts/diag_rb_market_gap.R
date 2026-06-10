# Diagnose: RB season_mean runs hot vs Underdog market — artifact or stance?
# Reproduces the position-level gap from BLEND INTERNALS (not published JSON).
# Read-only: blend_slate(write_json = FALSE). No pipeline changes.

suppressMessages(devtools::load_all(".", quiet = TRUE))

cache_dir <- file.path("~", ".bestball-bro", "cache")
feed <- blend_slate(
  slate_id              = "nfl_2026_season",
  sources_manifest_path = "inst/data/sources/_manifest.yaml",
  slates_manifest_path  = "inst/data/slates/_manifest.yaml",
  out_path              = tempfile(fileext = ".json"),
  cache_dir             = cache_dir,
  write_json            = FALSE
)

P <- feed$players
getn <- function(x, d = NA_real_) if (is.null(x)) d else as.numeric(x)

df <- do.call(rbind, lapply(P, function(p) {
  sb <- p$source_breakdown %||% list()
  data.frame(
    name     = p$name %||% NA_character_,
    pos      = p$position %||% NA_character_,
    smean    = getn(p$season_mean),
    ud       = getn(p$underdog_projected_points),
    clay     = getn(sb$clay$raw_points),
    etr      = getn(sb$etr$calibrated_points),
    legup    = getn(sb$legup$calibrated_points),
    prank    = getn(p$position_rank),
    tier     = getn(p$tier),
    adp      = getn(p$adp),
    stringsAsFactors = FALSE
  )
}))

# Only players with BOTH our season_mean and an Underdog projection.
both <- df[!is.na(df$smean) & !is.na(df$ud), ]

cat("\n================ 1. PER-POSITION GAP (internals) ================\n")
cat(sprintf("%-4s %5s %9s %9s %8s %7s\n",
            "pos","n","mean_sm","mean_ud","gap_pts","gap_%"))
for (pos in c("QB","RB","WR","TE")) {
  s <- both[both$pos == pos, ]
  if (!nrow(s)) next
  msm <- mean(s$smean); mud <- mean(s$ud)
  cat(sprintf("%-4s %5d %9.1f %9.1f %+8.1f %+6.1f%%\n",
              pos, nrow(s), msm, mud, msm-mud, 100*(msm-mud)/mud))
}

cat("\n================ 2. PER-SOURCE RB GAP vs UD ================\n")
cat("(Clay = raw point projection / calibration reference;\n")
cat(" ETR & LegUp = RANK-ONLY, points are Clay-curve calibrated — no native points.)\n\n")
rb <- both[both$pos == "RB", ]
src_gap <- function(col) {
  s <- rb[!is.na(rb[[col]]), ]
  c(n = nrow(s), gap = mean(s[[col]] - s$ud), pct = 100*mean(s[[col]]-s$ud)/mean(s$ud))
}
for (col in c("clay","etr","legup")) {
  g <- src_gap(col)
  cat(sprintf("  %-6s vs UD:  n=%3d  gap=%+6.1f pts  (%+5.1f%%)\n",
              col, g["n"], g["gap"], g["pct"]))
}
g <- c(n = nrow(rb), gap = mean(rb$smean - rb$ud), pct = 100*mean(rb$smean-rb$ud)/mean(rb$ud))
cat(sprintf("  %-6s vs UD:  n=%3d  gap=%+6.1f pts  (%+5.1f%%)\n",
            "BLEND", g["n"], g["gap"], g["pct"]))

cat("\n================ 3. RB GAP BY TIER / RANK BUCKET ================\n")
rb$bucket <- ifelse(rb$prank <= 12, "1-12 (R1-3)",
              ifelse(rb$prank <= 36, "13-36", "37+"))
for (b in c("1-12 (R1-3)","13-36","37+")) {
  s <- rb[rb$bucket == b, ]
  if (!nrow(s)) next
  cat(sprintf("  RB %-12s n=%3d  mean_sm=%6.1f  mean_ud=%6.1f  gap=%+6.1f (%+5.1f%%)\n",
              b, nrow(s), mean(s$smean), mean(s$ud),
              mean(s$smean-s$ud), 100*mean(s$smean-s$ud)/mean(s$ud)))
}

cat("\n================ 4. TOP 10 INDIVIDUAL RB GAPS ================\n")
rb$gap <- rb$smean - rb$ud
top <- rb[order(-rb$gap), ][1:10, ]
cat(sprintf("%-22s %5s %6s %6s %6s %6s %6s %6s\n",
            "name","prank","smean","ud","gap","clay","etr","legup"))
for (i in seq_len(nrow(top))) {
  t <- top[i, ]
  cat(sprintf("%-22s %5.0f %6.1f %6.1f %+6.1f %6.1f %6.1f %6.1f\n",
              substr(t$name,1,22), t$prank, t$smean, t$ud, t$gap,
              t$clay, t$etr, t$legup))
}

cat("\n================ 5. SANITY: blend level vs Clay level (RB) ================\n")
cat("If ETR/LegUp inherit Clay's level by calibration, mean(blend) ~= mean(clay).\n")
cat(sprintf("  RB mean clay=%.1f  etr_cal=%.1f  legup_cal=%.1f  blend=%.1f\n",
            mean(rb$clay, na.rm=TRUE), mean(rb$etr, na.rm=TRUE),
            mean(rb$legup, na.rm=TRUE), mean(rb$smean)))

cat("\nDONE.\n")
