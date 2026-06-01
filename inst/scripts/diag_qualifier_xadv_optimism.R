# =====================================================================
# Diagnose the qualifier-xAdv optimism (THROWAWAY -- diagnosis only).
#
# Our per-team qualifier xAdv runs systematically above BBMDB's
# (Puppy ~0.23 vs ~0.12; 3b-5-era BBM7 +0.086 signed), opposite sides of
# the 2/12 = 16.7% baseline. Spread hypothesis already ruled out.
# Localize the real mechanism. NO engine / blender / correlation changes.
#
# Step 0  -- characterize the gap per validation team / tournament,
#            confirm vintage comparability.
# Step 1  -- THE FORK: regress per-team gap on roster shape
#            (stack / ceiling-variance / chalk) vs level features
#            (within-pod relative strength).
# Step 2  -- conditional follow-ups:
#            2a opponent-pool composition (our teams vs real podmates)
#            2b proj-delta correlation, raw-sum vs lineup-aware basis
#            2c what BBMDB's xAdv actually conditions on (its own
#               projection-vs-field curve), and pod-relative vs
#               field-relative strength under our projections.
#
# Notes:
# - Canonical compute path: validate_xadv_against_bbmdb() (same call the
#   gated Puppy test makes), with feed + Layer A passed in so the same
#   blend backs the shape features.
# - Current BBMDB parquet (2026-05-28 wave) has only 22 post-draft teams:
#   Puppy 13 / BBM7 2 / Dachshund 3 / others 4. The 3b-5-era 20-team BBM7
#   wave is NOT in this snapshot; the comparable 2/12 pool is 18 teams.
# - All 22 post-draft rows have EMPTY roster_total_view (BBMDB per-player
#   contributions exist only for the pre-draft Big Board wave), so a
#   per-player ours-vs-BBMDB comparison is not possible from this data.
# - Expensive results (validator run) are cached in build/ so the cheap
#   analyses can be iterated without re-simulating.
# =====================================================================
suppressMessages(devtools::load_all(quiet = TRUE))
suppressMessages(library(arrow))
`%||%` <- function(a, b) if (is.null(a)) b else a

LAYERA_N <- 4000L
POD_N    <- 4000L
BASELINE <- 2 / 12

bbmdb_path   <- "C:/Users/vince/Desktop/bbmdb_scraper/data/bbmdb_teams.parquet"
scraper_path <- file.path("inst", "data", "scraped_drafts", "udbb-scraper-latest.json")
sources_path <- file.path("inst", "data", "sources", "_manifest.yaml")
slates_path  <- file.path("inst", "data", "slates",  "_manifest.yaml")
cache_path   <- "build/diag_xadv_cache.rds"

# ---------------------------------------------------------------------
# Setup (cached): blend + Layer A + canonical validator run
# ---------------------------------------------------------------------
picks <- load_scraped_drafts(scraper_path)
bbmdb <- read_parquet(bbmdb_path)

if (file.exists(cache_path)) {
  cat("== Setup: loading cached blend + validator run ==\n")
  cc <- readRDS(cache_path)
  feed <- cc$feed; val <- cc$val
} else {
  cat("== Setup: blending + simulating Layer A (n =", LAYERA_N, ") ==\n")
  feed <- blend_slate(slate_id = "nfl_2026_season",
                      sources_manifest_path = sources_path,
                      slates_manifest_path  = slates_path, write_json = FALSE)
  layerA_sim <- simulate_slate(feed, n_sims = LAYERA_N, seed = 1L)
  cat("\nRunning canonical validator (n_sims =", POD_N, ")...\n")
  val <- validate_xadv_against_bbmdb(
    bbmdb_path = bbmdb_path, scraper_path = scraper_path,
    feed = feed, layerA_sim = layerA_sim,
    n_sims = POD_N, base_seed = 1L, verbose = TRUE)
  saveRDS(list(feed = feed, val = val), cache_path)
  rm(layerA_sim)  # large; not needed below
}

per <- val$per_team
per <- per[!is.na(per$predicted_xadv) & !is.na(per$bbmdb_xadv), , drop = FALSE]
per$gap <- per$predicted_xadv - per$bbmdb_xadv

# Comparable pool: 2/12 qualifier tournaments only.
comp  <- per[per$advance_n == 2L, , drop = FALSE]
other <- per[per$advance_n != 2L, , drop = FALSE]

# ---------------------------------------------------------------------
# STEP 0 -- characterize the gap
# ---------------------------------------------------------------------
cat("\n=====================================================\n")
cat("STEP 0 -- gap characterization (2/12 baseline =", round(BASELINE, 4), ")\n")
cat("=====================================================\n")
post_rows <- bbmdb[bbmdb$tournament_corpus == "post_nfl_draft", , drop = FALSE]
cat("BBMDB vintage: scraped", paste(range(post_rows$bbmdb_last_scraped), collapse = " .. "), "\n")
cat("Scraper export: udbb-scraper-latest.json (2026-05-27)\n")
cat("Our blend/sim: run", format(Sys.time(), "%Y-%m-%d"), "with current published source feeds\n")

step0 <- function(df) data.frame(
  n              = nrow(df),
  our_xadv       = mean(df$predicted_xadv),
  bbmdb_xadv     = mean(df$bbmdb_xadv),
  gap            = mean(df$predicted_xadv) - mean(df$bbmdb_xadv),
  our_vs_base    = mean(df$predicted_xadv) - BASELINE,
  bbmdb_vs_base  = mean(df$bbmdb_xadv) - BASELINE)
tab0 <- do.call(rbind, c(list(POOLED_2of12 = step0(comp)),
                         lapply(split(comp, comp$tournament_id), step0)))
print(round(tab0, 4))
cat("\nPer-team signed gap summary (pooled 2/12):\n"); print(round(summary(comp$gap), 4))
cat("Teams with gap > 0:", sum(comp$gap > 0), "/", nrow(comp), "\n")
cat("Teams with our_xadv > baseline:", sum(comp$predicted_xadv > BASELINE), "/", nrow(comp), "\n")
cat("Teams with bbmdb_xadv < baseline:", sum(comp$bbmdb_xadv < BASELINE), "/", nrow(comp), "\n")
if (nrow(other)) {
  cat("\n[Supplementary] non-2/12 validation teams (advance_n != 2):\n")
  print(as.data.frame(other[, c("tournament_id", "advance_n", "predicted_xadv", "bbmdb_xadv")]),
        row.names = FALSE)
}

# ---------------------------------------------------------------------
# Shape + level features per validation team
# ---------------------------------------------------------------------
bridge <- bestballBroSim:::.scraper_to_feed_id_map(picks, feed)

fids  <- names(feed$players)
fteam <- vapply(fids, function(u) feed$players[[u]]$team %||% NA_character_, character(1))
fpos  <- vapply(fids, function(u) feed$players[[u]]$position %||% NA_character_, character(1))
fmean <- vapply(fids, function(u) feed$players[[u]]$season_mean %||% NA_real_, numeric(1))
fstd  <- vapply(fids, function(u) feed$players[[u]]$season_std %||% NA_real_, numeric(1))
names(fteam) <- names(fpos) <- names(fmean) <- names(fstd) <- fids

# Static best-ball "paper lineup" strength: QB1 + RB1-2 + WR1-3 + TE1 + best
# remaining RB/WR/TE flex, by season_mean. Deterministic lineup-aware proxy
# usable for podmates without re-running sims.
lineup_paper <- function(roster) {
  rp <- fpos[roster]; rm <- fmean[roster]
  ok <- !is.na(rp) & !is.na(rm); rp <- rp[ok]; rm <- rm[ok]
  if (!length(rm)) return(NA_real_)
  take <- function(pos, k) head(sort(rm[rp == pos], decreasing = TRUE), k)
  qb <- take("QB", 1); rb <- take("RB", 2); wr <- take("WR", 3); te <- take("TE", 1)
  used <- c(qb, rb, wr, te)
  # flex = best remaining RB/WR/TE after removing already-used values.
  rem <- rm[rp %in% c("RB", "WR", "TE")]
  for (v in c(rb, wr, te)) {
    i <- match(v, rem); if (!is.na(i)) rem <- rem[-i]
  }
  flex <- if (length(rem)) max(rem) else 0
  sum(used) + flex
}

shape_features <- function(eid, d_id) {
  pr <- picks[picks$draft_id == d_id & picks$draft_entry_id == eid, , drop = FALSE]
  roster <- unique(bridge[pr$underdog_id]); roster <- roster[!is.na(roster)]
  rp <- fpos[roster]; rt <- fteam[roster]; rs <- fstd[roster]; rm <- fmean[roster]
  tt <- table(rt[!is.na(rt)])
  qb_ids <- roster[!is.na(rp) & rp == "QB"]
  qb_stack <- 0L
  for (q in qb_ids) {
    tm <- rt[[q]]; if (is.na(tm)) next
    pcs <- sum(rt == tm & rp %in% c("WR", "TE"), na.rm = TRUE)
    qb_stack <- max(qb_stack, 1L + pcs)
  }
  data.frame(
    entry_id       = eid,
    n_bridged      = length(roster),
    qb_stack       = qb_stack,                                  # QB + same-team pass catchers
    max_team_stack = if (length(tt)) as.integer(max(tt)) else 0L,
    n_stacks_3plus = sum(tt >= 3L),
    roster_var     = sum(rs^2, na.rm = TRUE),                   # ceiling/variance proxy
    roster_meanstd = mean(rs, na.rm = TRUE),
    paper_sum      = sum(rm, na.rm = TRUE),                     # raw-sum strength
    lineup_paper   = lineup_paper(roster),                      # static lineup-aware strength
    mean_adp       = mean(pr$projection_adp_at_pick, na.rm = TRUE),  # chalk (lower = chalkier)
    stringsAsFactors = FALSE)
}

cat("\nComputing shape + pod-relative features...\n")
feat <- list(); podtab <- list()
for (i in seq_len(nrow(comp))) {
  eid <- comp$entry_id[i]; d_id <- comp$draft_id[i]
  feat[[i]] <- shape_features(eid, d_id)

  pod <- picks[picks$draft_id == d_id, , drop = FALSE]
  pod_strength <- vapply(split(pod$underdog_id, pod$draft_entry_id), function(uids) {
    r <- unique(bridge[uids]); r <- r[!is.na(r)]
    if (!length(r)) NA_real_ else lineup_paper(r)
  }, numeric(1))
  pod_strength <- pod_strength[!is.na(pod_strength)]
  if (eid %in% names(pod_strength) && length(pod_strength) >= 3L) {
    z <- (pod_strength[[eid]] - mean(pod_strength)) / stats::sd(pod_strength)
    feat[[i]]$pod_rel_strength <- z
    feat[[i]]$pod_rank <- as.integer(rank(-pod_strength)[[eid]])  # 1 = strongest in pod
  } else {
    feat[[i]]$pod_rel_strength <- NA_real_
    feat[[i]]$pod_rank <- NA_integer_
  }
  feat[[i]]$pod_size_bridged <- length(pod_strength)
  podtab[[i]] <- data.frame(
    draft_id = d_id, entry_id = names(pod_strength),
    lineup_paper = as.numeric(pod_strength),
    is_ours = names(pod_strength) %in% comp$entry_id,
    stringsAsFactors = FALSE)
}
F <- do.call(rbind, feat)
PODS <- unique(do.call(rbind, podtab))
D <- merge(as.data.frame(comp), F, by = "entry_id")

# ---------------------------------------------------------------------
# STEP 1 -- the decisive regression: gap vs team shape
# ---------------------------------------------------------------------
cat("\n=====================================================\n")
cat("STEP 1 -- gap vs roster shape (THE FORK), n =", nrow(D), "\n")
cat("=====================================================\n")
variance_features <- c("qb_stack", "max_team_stack", "n_stacks_3plus",
                       "roster_var", "roster_meanstd")
level_features    <- c("paper_sum", "lineup_paper", "mean_adp", "pod_rel_strength")

cat("Bivariate correlation with per-team gap (pearson / spearman / p-value):\n")
corline <- function(x, y) {
  ct <- suppressWarnings(stats::cor.test(x, y))
  sp <- suppressWarnings(stats::cor(x, y, method = "spearman", use = "complete.obs"))
  sprintf("r=%+.3f  rho=%+.3f  p=%.3f", ct$estimate, sp, ct$p.value)
}
cat("  -- variance / ceiling / stack features --\n")
for (cc in variance_features)
  cat(sprintf("  %-18s %s\n", cc, corline(D$gap, D[[cc]])))
cat("  -- level / strength / chalk features --\n")
for (cc in level_features)
  cat(sprintf("  %-18s %s\n", cc, corline(D$gap, D[[cc]])))

cat("\nGrouped R^2 (small n -- directional, adjusted in parens):\n")
r2 <- function(fml) {
  m <- summary(lm(fml, data = D))
  sprintf("R^2 = %.3f (adj %.3f)", m$r.squared, m$adj.r.squared)
}
cat("  variance-only  gap ~ qb_stack + roster_var:        ", r2(gap ~ qb_stack + roster_var), "\n")
cat("  level-only     gap ~ pod_rel_strength + mean_adp:  ", r2(gap ~ pod_rel_strength + mean_adp), "\n")
cat("  combined       gap ~ qb_stack + roster_var + pod_rel_strength:\n")
mc <- lm(gap ~ scale(qb_stack) + scale(roster_var) + scale(pod_rel_strength), data = D)
print(summary(mc)$coefficients)

cat("\nGap by stack bucket (flat = level bias; rising = variance capture):\n")
D$stack_bucket <- cut(D$qb_stack, breaks = c(-1, 1, 2, 99), labels = c("QB only", "QB+1", "QB+2plus"))
print(aggregate(cbind(gap, predicted_xadv, bbmdb_xadv) ~ stack_bucket, D,
                function(x) round(mean(x), 3)))
cat("\nGap by roster_var tercile:\n")
D$var_bucket <- cut(D$roster_var, breaks = quantile(D$roster_var, c(0, 1/3, 2/3, 1)),
                    labels = c("low var", "mid var", "high var"), include.lowest = TRUE)
print(aggregate(cbind(gap, predicted_xadv, bbmdb_xadv) ~ var_bucket, D,
                function(x) round(mean(x), 3)))

# ---------------------------------------------------------------------
# PER-TEAM GAP TABLE + CSV (written before any further analysis)
# ---------------------------------------------------------------------
cat("\n=====================================================\n")
cat("PER-TEAM GAP TABLE (sorted by gap, descending)\n")
cat("=====================================================\n")
out <- D[order(-D$gap),
         c("tournament_id", "entry_id", "predicted_xadv", "bbmdb_xadv", "gap",
           "pod_rel_strength", "pod_rank", "qb_stack", "max_team_stack",
           "roster_var", "mean_adp", "our_team_projection", "bbmdb_team_projection")]
out$entry_id <- substr(out$entry_id, 1, 8)
num <- vapply(out, is.numeric, logical(1))
out[num] <- lapply(out[num], round, 3)
print(out, row.names = FALSE)

write.csv(D, "build/diag_qualifier_xadv_per_team.csv", row.names = FALSE)
cat("\nWrote build/diag_qualifier_xadv_per_team.csv\n")

# ---------------------------------------------------------------------
# STEP 2a -- opponent-pool composition (level mechanism)
# ---------------------------------------------------------------------
cat("\n=====================================================\n")
cat("STEP 2a -- opponent-pool composition\n")
cat("=====================================================\n")
ours_lp <- PODS$lineup_paper[PODS$is_ours]
opps_lp <- PODS$lineup_paper[!PODS$is_ours]
cat(sprintf("Static paper-lineup strength: our teams mean = %.1f (n=%d), podmates mean = %.1f (n=%d)\n",
            mean(ours_lp), length(ours_lp), mean(opps_lp), length(opps_lp)))
cat(sprintf("  podmate strength SD (within all pods pooled) = %.1f\n", stats::sd(opps_lp)))
cat(sprintf("Our teams' mean within-pod strength z = %+.3f | mean within-pod rank = %.1f of ~12\n",
            mean(D$pod_rel_strength, na.rm = TRUE), mean(D$pod_rank, na.rm = TRUE)))
cat(sprintf("corr(gap, pod_rel_strength) = %+.3f\n",
            cor(D$gap, D$pod_rel_strength, use = "complete.obs")))
cat(sprintf("corr(our_xadv, pod_rel_strength) = %+.3f   <- ours is pod-relative by construction\n",
            cor(D$predicted_xadv, D$pod_rel_strength, use = "complete.obs")))
cat(sprintf("corr(bbmdb_xadv, pod_rel_strength) = %+.3f  <- ~0 would mean BBMDB can't see the pod\n",
            cor(D$bbmdb_xadv, D$pod_rel_strength, use = "complete.obs")))
cat(sprintf("corr(bbmdb_xadv, bbmdb_team_projection) = %+.3f <- high = BBMDB xadv ~ f(own proj vs fixed field)\n",
            cor(D$bbmdb_xadv, D$bbmdb_team_projection, use = "complete.obs")))

# ---------------------------------------------------------------------
# STEP 2b -- proj-delta correlation: raw-sum vs lineup-aware basis
# ---------------------------------------------------------------------
cat("\n=====================================================\n")
cat("STEP 2b -- proj-delta correlation (3b-5's 0.76 recheck)\n")
cat("=====================================================\n")
cat("(units differ across sources; deltas computed on z-scored quantities)\n")
cat(sprintf("corr(our_team_projection [lineup-aware sims], bbmdb_team_projection) = %+.3f\n",
            cor(D$our_team_projection, D$bbmdb_team_projection, use = "complete.obs")))
cat(sprintf("corr(paper_sum [raw-sum], bbmdb_team_projection)                     = %+.3f\n",
            cor(D$paper_sum, D$bbmdb_team_projection, use = "complete.obs")))
D$delta_la  <- as.numeric(scale(D$our_team_projection)) - as.numeric(scale(D$bbmdb_team_projection))
D$delta_raw <- as.numeric(scale(D$paper_sum))           - as.numeric(scale(D$bbmdb_team_projection))
cat(sprintf("\ncorr(gap, proj-delta on RAW-SUM basis)      = %+.3f\n",
            cor(D$gap, D$delta_raw, use = "complete.obs")))
cat(sprintf("corr(gap, proj-delta on LINEUP-AWARE basis) = %+.3f\n",
            cor(D$gap, D$delta_la, use = "complete.obs")))
cat(sprintf("corr(gap, our_team_projection)              = %+.3f\n",
            cor(D$gap, D$our_team_projection, use = "complete.obs")))
cat(sprintf("corr(gap, bbmdb_team_projection)            = %+.3f\n",
            cor(D$gap, D$bbmdb_team_projection, use = "complete.obs")))

# ---------------------------------------------------------------------
# STEP 2c -- what does BBMDB's xAdv condition on, and where do our teams
#            sit pod-relative vs field-relative?
# ---------------------------------------------------------------------
cat("\n=====================================================\n")
cat("STEP 2c -- BBMDB conditioning + pod- vs field-relative strength\n")
cat("=====================================================\n")

# (i) Data limitation check: per-player BBMDB contributions for post-draft teams.
post_idx <- which(bbmdb$tournament_corpus == "post_nfl_draft")
n_rtv <- vapply(post_idx, function(i) nrow(as.data.frame(bbmdb$roster_total_view[[i]])),
                integer(1))
cat(sprintf("(i) BBMDB roster_total_view rows for the %d post-draft teams: %s non-empty\n",
            length(post_idx), sum(n_rtv > 0)))
cat("    -> per-player ours-vs-BBMDB selection test NOT possible from this snapshot.\n")
cat("    -> (sibling_entry_ids exist per pod; scraping podmates would identify the level split.)\n")

# (ii) BBMDB's xadv-vs-own-projection curve: where is BBMDB's implied
#      "field-average" team (xadv = 2/12) and where do our teams sit on it?
fit <- lm(bbmdb_xadv ~ bbmdb_team_projection, data = D)
slope <- coef(fit)[2]; intercept <- coef(fit)[1]
proj_at_baseline <- (BASELINE - intercept) / slope
cat(sprintf("\n(ii) BBMDB curve: bbmdb_xadv = %.4f + %.6f * bbmdb_team_projection  (R^2 = %.3f)\n",
            intercept, slope, summary(fit)$r.squared))
cat(sprintf("     BBMDB's implied field-average team projection (xadv = baseline): %.1f\n",
            proj_at_baseline))
cat(sprintf("     Our validation teams' mean bbmdb_team_projection: %.1f (range %.1f-%.1f)\n",
            mean(D$bbmdb_team_projection), min(D$bbmdb_team_projection), max(D$bbmdb_team_projection)))
cat(sprintf("     => BBMDB sees these teams as %.1f pts BELOW its field-average team\n",
            proj_at_baseline - mean(D$bbmdb_team_projection)))
cat(sprintf("     Per-pt sensitivity: %.5f xadv per BBMDB projection pt\n", slope))

# (iii) Pod-relative vs field-relative strength under OUR projections.
#       The 198 podmates are real Underdog drafters across ~18 separate
#       drafts -- a fair sample of "the field". If our teams' field
#       percentile implies an advance prob close to our pod-relative xAdv,
#       pod composition is NOT the driver and the level gap is a
#       projection-source disagreement.
field_lp <- PODS$lineup_paper[!PODS$is_ours]   # field sample = real podmates
pctile <- vapply(D$lineup_paper, function(x) mean(field_lp < x), numeric(1))
# Deterministic-ranking approximation: P(top-2 of 12) for a team at field
# percentile q = P(at most 1 of 11 iid opponents is stronger)
det_xadv <- pctile^11 + 11 * (1 - pctile) * pctile^10
cat(sprintf("\n(iii) Our teams' mean percentile in the real-drafter field (our projections): %.3f\n",
            mean(pctile)))
cat(sprintf("      Deterministic-ranking implied field-relative advance prob: %.3f\n", mean(det_xadv)))
cat("      (Upper bound -- ignores weekly variance, which pulls advance probs toward the baseline.)\n")
cat(sprintf("      Compare: our pod-relative xAdv = %.3f | BBMDB field-relative xAdv = %.3f | baseline = %.3f\n",
            mean(D$predicted_xadv), mean(D$bbmdb_xadv), BASELINE))
cat(sprintf("      corr(field percentile, pod_rel_strength) = %+.3f (pods ~ field sample if high)\n",
            cor(pctile, D$pod_rel_strength, use = "complete.obs")))

D$field_pctile <- pctile
D$det_field_xadv <- det_xadv
write.csv(D, "build/diag_qualifier_xadv_per_team.csv", row.names = FALSE)
cat("\nRe-wrote build/diag_qualifier_xadv_per_team.csv (with field percentile cols)\n")
cat("DONE\n")
