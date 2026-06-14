#!/usr/bin/env Rscript
# G2 (STOP 3): run the real-set BBMDB validator against the NEW masked pipeline
# (validate_xadv_against_bbmdb reblends + resims internally) and report the
# actual points-space ratio / MAE / Spearman. Expected ~1.008 / <=0.124 / >0.3.
suppressMessages(devtools::load_all(".", quiet = TRUE))

bbmdb    <- bbmdb_corpus_path()
scraper  <- "inst/data/scraped_drafts/udbb-scraper-latest.json"
stopifnot(file.exists(bbmdb), file.exists(scraper))

res <- validate_xadv_against_bbmdb(
  bbmdb_path    = bbmdb,
  scraper_path  = scraper,
  layerA_n_sims = 5000L,
  n_sims        = 5000L,
  base_seed     = 1L,
  verbose       = FALSE
)

ag <- res$aggregates
pt <- res$per_team
ok <- !is.na(pt$our_team_projection) & !is.na(pt$bbmdb_team_projection)
ratio <- mean(pt$our_team_projection[ok]) / mean(pt$bbmdb_team_projection[ok])

cat("\n================ G2: BBMDB real-set (new masked pipeline) ================\n")
cat(sprintf("  n_validated         = %d\n", ag$n_validated))
cat(sprintf("  points-space ratio  = %.4f   (target ~1.008)\n", ratio))
cat(sprintf("  MAE (xadv)          = %.4f   (floor <= 0.124)\n", ag$mae))
cat(sprintf("  Spearman            = %.4f   (floor > 0.3)\n", ag$spearman))
cat(sprintf("  proj Pearson xcheck = %.4f\n", res$projection_xcheck$pearson))
cat(sprintf("  mean signed err     = %+.4f\n", ag$mean_signed_error))
cat(sprintf("\n  G2: %s\n",
    if (ratio > 0.9 && ratio < 1.1 && ag$mae <= 0.124 && ag$spearman > 0.3)
      "PASS" else "**CHECK**"))
cat("DONE.\n")
