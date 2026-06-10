# Capture xAdv-vs-BBMDB validator aggregates with availability ON.
suppressMessages(devtools::load_all(".", quiet = TRUE))
bbmdb_path   <- "C:/Users/vince/Desktop/bbmdb_scraper/data/bbmdb_teams.parquet"
scraper_path <- file.path("inst", "data", "scraped_drafts",
                          "udbb-scraper-latest.json")

res <- validate_xadv_against_bbmdb(
  bbmdb_path    = bbmdb_path,
  scraper_path  = scraper_path,
  layerA_n_sims = 5000L, n_sims = 5000L, base_seed = 1L, verbose = FALSE
)
a <- res$aggregates
cat(sprintf("AVAIL-ON  n=%d  Spearman=%.3f  MAE=%.3f  signed=%+.3f  Pearson(proj)=%.3f\n",
            a$n_validated, a$spearman, a$mae, a$mean_signed_error,
            res$projection_xcheck$pearson))
