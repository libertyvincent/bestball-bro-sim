# Two checks on the availability fix's xAdv effect (run OFF then ON):
#   Check 1 (composition): are validated teams RB-lighter than their pod-mates?
#   Check 2 (points-space): does our team projected points over-shoot BBMDB
#                           less ON than OFF? (level convergence claim)
# Usage: Rscript diag_avail_validator_checks.R <label>   (label = on | off)
# Blends per the CURRENT availability.yaml state, so toggle `enabled` between
# the two invocations.

suppressMessages(devtools::load_all(".", quiet = TRUE))
args  <- commandArgs(trailingOnly = TRUE)
label <- if (length(args)) args[[1]] else "on"

bbmdb_path   <- bbmdb_corpus_path()
scraper_path <- .inst_path("data/scraped_drafts", "udbb-scraper-latest.json")

# Blend the feed under the current yaml state, then validate against BBMDB.
feed <- blend_slate("nfl_2026_season",
                    .inst_path("data/sources", "_manifest.yaml"),
                    .inst_path("data/slates",  "_manifest.yaml"),
                    write_json = FALSE)

res <- validate_xadv_against_bbmdb(
  bbmdb_path    = bbmdb_path,
  scraper_path  = scraper_path,
  feed          = feed,
  layerA_n_sims = 3000L, n_sims = 3000L, base_seed = 1L, verbose = FALSE
)
saveRDS(res, sprintf("build/val_%s.rds", label))

pt  <- res$per_team
val <- pt[!is.na(pt$predicted_xadv), , drop = FALSE]
a   <- res$aggregates

cat(sprintf("\n=== [%s] aggregates ===\n", toupper(label)))
cat(sprintf("n=%d  Spearman=%.3f  MAE=%.3f  signed(xAdv)=%+.3f\n",
            a$n_validated, a$spearman, a$mae, a$mean_signed_error))

# ---- Check 2: points-space over-projection (our team pts vs BBMDB) ----------
gap   <- val$our_team_projection - val$bbmdb_team_projection
ratio <- val$our_team_projection / val$bbmdb_team_projection
cat(sprintf("\n=== [%s] Check 2: points-space (qualifier wks 1-14) ===\n", toupper(label)))
cat(sprintf("mean(our)=%.1f  mean(bbmdb)=%.1f  mean signed gap=%+.2f pts  mean ratio=%.4f\n",
            mean(val$our_team_projection), mean(val$bbmdb_team_projection),
            mean(gap), mean(ratio)))
cat(sprintf("signed-gap quartiles: %s\n",
            paste(sprintf("%+.1f", stats::quantile(gap, c(0,.25,.5,.75,1))), collapse="  ")))

# ---- Check 1: RB composition of validated teams vs their pod-mates ----------
# Only needs the feed + picks + bridge (no sims). Run on the ON pass.
if (identical(label, "on")) {
  picks  <- load_scraped_drafts(scraper_path)
  bridge <- .scraper_to_feed_id_map(picks, feed)
  pos_of <- vapply(feed$players, function(p) p$position %||% NA_character_, character(1))
  sm_of  <- vapply(feed$players, function(p) {
    v <- p$season_mean; if (is.null(v) || is.na(v)) 0 else as.numeric(v)
  }, numeric(1))

  rb_count_share  <- function(uids) {
    uids <- uids[!is.na(uids)]
    if (!length(uids)) return(NA_real_)
    mean(pos_of[uids] == "RB", na.rm = TRUE)
  }
  rb_point_share <- function(uids) {
    uids <- uids[!is.na(uids)]
    tot <- sum(sm_of[uids]); if (tot <= 0) return(NA_real_)
    sum(sm_of[uids][pos_of[uids] == "RB"]) / tot
  }

  deltas_cnt <- c(); deltas_pts <- c()
  for (j in seq_len(nrow(val))) {
    d_id <- val$draft_id[j]; eid <- val$entry_id[j]
    pod_rows <- picks[picks$draft_id == d_id, , drop = FALSE]
    pod_rows$feed_uid <- bridge[pod_rows$underdog_id]
    pod_rows <- pod_rows[!is.na(pod_rows$feed_uid), , drop = FALSE]
    rosters  <- lapply(split(pod_rows$feed_uid, pod_rows$draft_entry_id), unique)
    if (length(rosters) < 2L || is.null(rosters[[eid]])) next

    cnt <- vapply(rosters, rb_count_share, numeric(1))
    pts <- vapply(rosters, rb_point_share, numeric(1))
    deltas_cnt <- c(deltas_cnt, cnt[[eid]] - mean(cnt, na.rm = TRUE))
    deltas_pts <- c(deltas_pts, pts[[eid]] - mean(pts, na.rm = TRUE))
  }

  cat(sprintf("\n=== [ON] Check 1: validated team RB share - pod-mean RB share (n=%d) ===\n",
              length(deltas_cnt)))
  cat("(negative => validated team is RB-LIGHTER than its pod average)\n")
  cat(sprintf("RB COUNT-share delta:  mean=%+.4f  median=%+.4f  frac<0=%.0f%%\n",
              mean(deltas_cnt), stats::median(deltas_cnt), 100*mean(deltas_cnt < 0)))
  cat(sprintf("  quartiles: %s\n",
              paste(sprintf("%+.3f", stats::quantile(deltas_cnt, c(0,.25,.5,.75,1))), collapse="  ")))
  cat(sprintf("RB POINT-share delta:  mean=%+.4f  median=%+.4f  frac<0=%.0f%%\n",
              mean(deltas_pts), stats::median(deltas_pts), 100*mean(deltas_pts < 0)))
  cat(sprintf("  quartiles: %s\n",
              paste(sprintf("%+.3f", stats::quantile(deltas_pts, c(0,.25,.5,.75,1))), collapse="  ")))
}
cat("\nDONE.\n")
