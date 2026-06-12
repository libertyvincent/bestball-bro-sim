#!/usr/bin/env Rscript
# ============================================================================
# Phase B gate harness — draw-level availability (game-zeroing)
# ============================================================================
# Runs the full local pipeline against the warm source cache and reports the
# gate table from DRAW_ZEROING_DESIGN.md. G3 (no-double-discount) and G4
# (tensor decode contract) live in testthat. Run from repo root:
#   "$RS" inst/scripts/gate_draw_zeroing.R > build/gate_draw_zeroing.log 2>&1
#
# Framing the three subtleties this gate exposes (all characterized, none a
# mask bug):
#  * season_mean is now EMPIRICAL (prompt item 3: recompute from zeroed draws),
#    so the weekly clip pmax(0,.) that PR #34's ANALYTICAL mean omitted now
#    shows up as a small upward level shift, biggest where CV is highest.
#    We report empirical vs the analytical-masked reference (u*q) to isolate it.
#  * "uniform across tiers" = the DISCOUNT (masked/conditional) is tier-flat,
#    not that the absolute gap% is flat. We test the discount ratio by tier.
#  * the post-mask bucket correlation is the MIXTURE (harvest-relevant), a
#    different quantity from copula fidelity. We report copula fidelity on the
#    PRE-mask draws (untouched) AND the post-mask mixture with its closed form.

suppressMessages(devtools::load_all(".", quiet = TRUE))
suppressWarnings(suppressMessages({ library(httr2); library(jsonlite) }))
`%||%` <- function(a, b) if (is.null(a)) b else a
getn   <- function(x, d = NA_real_) if (is.null(x)) d else as.numeric(x)

SLATE     <- "nfl_2026_season"
N_SIMS    <- 10000L
SEED      <- 20260612L
CACHE     <- file.path("~", ".bestball-bro", "cache")
CDN       <- "https://libertyvincent.github.io/bestball-bro-data"
PR34_GAP  <- c(QB = -2.3, RB = 2.1, WR = -0.3, TE = -3.0)
CV_POS    <- c(QB = 0.32, RB = 0.50, WR = 0.55, TE = 0.60)
TOL_G1_PP <- 0.6   # vs the clipping-adjusted PR #34 reference
TOL_DISC  <- 0.4   # tier-spread of the masked/conditional DISCOUNT (pp of q)
TOL_G5_PL <- 3.0; TOL_G5_AG <- 0.7

cat("=== Phase B gate: draw-level availability ===\n")
cat(sprintf("slate=%s n_sims=%d seed=%d\n\n", SLATE, N_SIMS, SEED))

# ---- run the pipeline ------------------------------------------------------
feed <- blend_slate(SLATE,
  sources_manifest_path = "inst/data/sources/_manifest.yaml",
  slates_manifest_path  = "inst/data/slates/_manifest.yaml",
  out_path = tempfile(fileext = ".json"), cache_dir = CACHE, write_json = FALSE)
sim   <- simulate_slate(feed, n_sims = N_SIMS, seed = SEED)
efeed <- sim$enriched_feed
edraw <- build_ev_draws(efeed, sim$draws, slate_id = SLATE,
                        n_paths = 500L, seed = SEED)

mech <- efeed[["_meta"]]$availability_mechanism
cat("--- _meta.availability_mechanism ---\n")
cat(sprintf("  type=%s model=%s  p_miss_17g: QB=%.4f RB=%.4f WR=%.4f TE=%.4f\n\n",
            mech$type %||% "<none>", mech$missed_week_model %||% "<none>",
            mech$p_miss_canonical_17g$QB, mech$p_miss_canonical_17g$RB,
            mech$p_miss_canonical_17g$WR, mech$p_miss_canonical_17g$TE))

# ---- per-player frame: analytical (pre-sim) + masked (post-sim) ------------
pre <- feed$players
row_for <- function(p) {
  uid <- p$underdog_id %||% NA_character_
  a   <- pre[[uid]]
  wk18 <- NA_real_
  for (w in p$weekly %||% list())
    if (isTRUE(as.integer(w$week %||% -1) == 18L)) wk18 <- getn(w$mean)
  data.frame(uid = uid, name = p$name %||% NA, pos = p$position %||% NA,
    u = getn(a$season_mean),                         # analytical conditional
    smean = getn(p$season_mean),                     # empirical MASKED
    sstd  = getn(p$season_std),                       # empirical MASKED std
    dis = getn(a$disagreement_std, 0), ale = getn(a$aleatoric_std, 0),
    pmiss = getn(p$availability_p_miss, 0),
    ud = getn(p$underdog_projected_points),
    prank = getn(p$position_rank), tier = getn(p$tier), wk18 = wk18,
    stringsAsFactors = FALSE)
}
df <- do.call(rbind, lapply(efeed$players, row_for))
df$q <- 1 - df$pmiss

# Sum of conditional weekly mean^2 per player (for analytical masked std).
sum_mu2 <- vapply(efeed$players, function(p) {
  a <- pre[[p$underdog_id]]
  s <- 0
  for (w in a$weekly %||% list()) if (!isTRUE(w$is_bye)) s <- s + getn(w$mean, 0)^2
  s
}, numeric(1))
df$sum_mu2 <- sum_mu2[match(df$uid, names(sum_mu2))]

# ============================================================================
# Clipping isolation: empirical masked vs analytical masked (u*q)
# ============================================================================
cat("======== clipping bias: empirical season_mean vs analytical u*q ========\n")
cat("(prompt item 3 recompute exposes pmax(0,.); PR #34 used analytical mean)\n")
cat(sprintf("%-4s %8s %10s %8s %8s\n","pos","mean(u*q)","mean(emp)","clip%","cv"))
clip_pos <- c()
for (pos in c("QB","RB","WR","TE")) {
  s <- df[df$pos == pos & !is.na(df$smean) & !is.na(df$u), ]
  anal <- mean(s$u * s$q); emp <- mean(s$smean)
  clip_pos[pos] <- 100 * (emp - anal) / anal
  cat(sprintf("%-4s %8.1f %10.1f %+7.2f%% %8.2f\n", pos, anal, emp,
              clip_pos[pos], CV_POS[[pos]]))
}
cat("\n")

# ============================================================================
# G1 — per-position gap vs UD, against the CLIPPING-ADJUSTED PR #34 reference
# ============================================================================
cat("================ G1: per-position gap vs UD (masked) ================\n")
both <- df[!is.na(df$smean) & !is.na(df$ud), ]
cat(sprintf("%-4s %4s %8s %7s %8s %9s %7s %s\n",
            "pos","n","gap%","pr34%","clip_adj","d_vs_adj","d_raw","PASS"))
g1_pass <- TRUE
for (pos in c("QB","RB","WR","TE")) {
  s  <- both[both$pos == pos, ]
  gp <- 100 * (mean(s$smean) - mean(s$ud)) / mean(s$ud)
  # clipping lifts mean_sm by clip%, so the gap (pp) lifts by clip% * sm/ud.
  adj <- PR34_GAP[[pos]] + clip_pos[pos] * mean(s$smean) / mean(s$ud)
  d_adj <- gp - adj; d_raw <- gp - PR34_GAP[[pos]]
  ok <- abs(d_adj) <= TOL_G1_PP; g1_pass <- g1_pass && ok
  cat(sprintf("%-4s %4d %+6.1f %+7.1f %+8.2f %+8.2f %+7.2f %s\n",
              pos, nrow(s), gp, PR34_GAP[[pos]], adj, d_adj, d_raw,
              if (ok) "ok" else "**FAIL**"))
}
# Discount uniformity across tiers: masked/conditional ratio should be ~ q_pos.
cat("\n  discount uniformity (1 - masked/conditional) by tier, target ~ p_miss:\n")
disc_pass <- TRUE
for (pos in c("QB","RB","WR","TE")) {
  s <- df[df$pos == pos & !is.na(df$smean) & !is.na(df$u), ]
  s <- s[s$u > 1, ]
  by_t <- tapply(seq_len(nrow(s)), s$tier, function(ix)
    100 * (1 - mean(s$smean[ix]) / mean(s$u[ix])))
  by_t <- by_t[is.finite(by_t)]
  spread <- if (length(by_t) > 1) max(by_t) - min(by_t) else 0
  # clipping makes the discount appear smaller (emp_cond > u); the SPREAD across
  # tiers is the uniformity test, not the level.
  ok <- spread <= 100 * TOL_DISC; disc_pass <- disc_pass && ok
  cat(sprintf("    %-3s discount%% by tier: [%s]  spread=%.2fpp %s\n", pos,
      paste(sprintf("%.1f", by_t), collapse=" "), spread,
      if (ok) "ok" else "**FAIL**"))
}
cat(sprintf("\n  G1: %s  (discount uniformity: %s)\n\n",
            if (g1_pass) "PASS" else "FAIL", if (disc_pass) "PASS" else "FAIL"))

# ============================================================================
# season_std rise: analytical conditional vs analytical masked vs empirical
# ============================================================================
cat("====== season_std: conditional -> masked (analytical) vs empirical ======\n")
cat("analytical masked Var = q^2 D^2 + q A^2 + sum q(1-q) mu_w^2 (design 3)\n")
cat(sprintf("%-4s %9s %9s %9s %8s\n","pos","cond_an","mask_an","emp(ship)","mask_rise%"))
for (pos in c("QB","RB","WR","TE")) {
  s <- df[df$pos == pos & !is.na(df$sstd), ]
  cond_an <- sqrt(s$dis^2 + s$ale^2)
  mask_an <- sqrt(s$q^2*s$dis^2 + s$q*s$ale^2 + s$q*(1-s$q)*s$sum_mu2)
  rise <- 100*(mean(mask_an)-mean(cond_an))/mean(cond_an)
  cat(sprintf("%-4s %9.1f %9.1f %9.1f %+7.1f\n",
              pos, mean(cond_an), mean(mask_an), mean(s$sstd), rise))
}
cat("(mask_rise%% = analytical mask effect, clip-free; emp(ship) also carries\n")
cat(" the clip which lowers std, so emp < mask_an at high-CV TE/WR.)\n\n")

# ============================================================================
# G5 — feed-internal consistency
# ============================================================================
cat("================ G5: tensor E[W1-17] vs projections ================\n")
TN <- edraw$tensor / edraw$quant_scale
pid <- edraw$player_ids
E_tensor <- vapply(seq_along(pid), function(j)
  sum(vapply(seq_along(edraw$weeks), function(wi) mean(TN[wi, j, ]), numeric(1))),
  numeric(1)); names(E_tensor) <- pid
g5 <- df[df$uid %in% pid & !is.na(df$smean), ]
g5$proj17 <- g5$smean - ifelse(is.na(g5$wk18), 0, g5$wk18)
g5$etens  <- E_tensor[g5$uid]
g5 <- g5[g5$proj17 > 1, ]
g5$delta  <- 100 * (g5$etens - g5$proj17) / g5$proj17
cat(sprintf("  n=%d  per-player |delta|: max=%.2f%% mean=%.3f%% (tol %.0f%%) -> %s\n",
            nrow(g5), max(abs(g5$delta)), mean(abs(g5$delta)), TOL_G5_PL,
            if (max(abs(g5$delta)) <= TOL_G5_PL) "ok" else "**FAIL**"))
g5_ag <- TRUE
for (pos in c("QB","RB","WR","TE")) {
  s <- g5[g5$pos == pos, ]
  ag <- 100*(sum(s$etens)-sum(s$proj17))/sum(s$proj17)
  ok <- abs(ag) <= TOL_G5_AG; g5_ag <- g5_ag && ok
  cat(sprintf("    %-3s agg_delta=%+6.3f%% %s\n", pos, ag, if (ok) "ok" else "**FAIL**"))
}
cat(sprintf("  G5: %s\n\n",
    if (max(abs(g5$delta))<=TOL_G5_PL && g5_ag) "PASS" else "FAIL"))

# ============================================================================
# Bucket table: copula fidelity (PRE-mask) vs harvest mixture (POST-mask)
# ============================================================================
sp_target <- function(rho) (6/pi)*asin(rho/2)
LAT <- list(team=0.45, game=0.25, cross=0.05)
weeks <- edraw$weeks; positions <- edraw$positions
smv <- setNames(df$smean[match(pid, df$uid)], pid)
qv  <- setNames(df$q[match(pid, df$uid)], pid)
team_of <- matrix(NA_character_, length(pid), length(weeks),
                  dimnames=list(pid, as.character(weeks)))
game_of <- team_of; bye_of <- matrix(TRUE, length(pid), length(weeks),
                                      dimnames=list(pid, as.character(weeks)))
for (uid in pid) { pl <- efeed$players[[uid]]; if (is.null(pl)) next
  tm <- pl$team %||% NA_character_
  for (wk in pl$weekly %||% list()) { w <- as.integer(wk$week %||% NA)
    if (is.na(w)) next; wc <- as.character(w); if (!(wc %in% colnames(team_of))) next
    isb <- isTRUE(wk$is_bye); bye_of[uid,wc] <- isb
    if (!isb) { opp <- wk$opponent %||% NA_character_; team_of[uid,wc] <- tm
      game_of[uid,wc] <- if (is.na(tm)||is.na(opp)) NA_character_ else paste(sort(c(tm,opp)),collapse="_") } } }
top_pos <- function(pos,n=40L){ ids<-pid[positions==pos]; ids<-ids[!is.na(smv[ids])]
  head(ids[order(-smv[ids])],n) }

# Pre-mask correlated draws (copula untouched) for fidelity.
ml_pre <- sample_correlated_draws(player_ids = pid, layerA_draws = sim$draws,
  schedule = schedule_from_feed(efeed), n_sims = 500L, seed = SEED,
  output_format = "matrix_list")

bucket_report <- function(get_vec, label) {
  # get_vec(week, id) -> numeric path vector
  cells <- list()
  for (pos in c("RB","TE","WR")) { ids0 <- top_pos(pos)
    for (w in weeks) { wc <- as.character(w)
      ids <- ids0[vapply(ids0, function(id) !bye_of[id,wc] && !is.na(team_of[id,wc]) &&
                           stats::sd(get_vec(w,id))>0, logical(1))]
      if (length(ids) < 2L) next
      M <- vapply(ids, function(id) get_vec(w,id), numeric(500L)); colnames(M)<-ids
      rs <- suppressWarnings(stats::cor(M, method="spearman"))
      for (a in 1:(length(ids)-1L)) for (b in (a+1L):length(ids)) {
        ia<-ids[a]; ib<-ids[b]
        bk <- if (identical(team_of[ia,wc],team_of[ib,wc])) "same_team"
              else if (identical(game_of[ia,wc],game_of[ib,wc])) "same_game" else "cross"
        cells[[length(cells)+1L]] <- data.frame(kind=paste0(pos,"x",pos), bucket=bk,
          pair=paste(sort(c(ia,ib)),collapse="|"), spearman=rs[a,b], stringsAsFactors=FALSE) } } }
  DF <- do.call(rbind, cells)
  keys <- unique(DF[,c("kind","bucket")]); keys<-keys[order(keys$kind,keys$bucket),]
  cat(sprintf("--- %s ---\n%-7s %-10s %7s %9s %8s %7s %s\n",
              label,"kind","bucket","n_pairs","Spearman","Sp_tgt","gap","flag"))
  for (i in seq_len(nrow(keys))) {
    d <- DF[DF$kind==keys$kind[i] & DF$bucket==keys$bucket[i],]
    tgt <- sp_target(LAT[[ if(keys$bucket[i]=="same_team")"team" else
                           if(keys$bucket[i]=="same_game")"game" else "cross" ]])
    gap <- mean(d$spearman)-tgt
    cat(sprintf("%-7s %-10s %7d %9.3f %8.3f %+7.3f %s\n", keys$kind[i], keys$bucket[i],
                length(unique(d$pair)), mean(d$spearman), tgt, gap, if(abs(gap)>0.05)"**" else "")) }
  invisible(DF)
}
cat("================ bucket table ================\n")
wi_of <- setNames(seq_along(weeks), as.character(weeks))
bucket_report(function(w,id) ml_pre[[as.character(w)]][id, ], "PRE-mask copula fidelity (untouched)")
cat("\n")
bucket_report(function(w,id) TN[wi_of[as.character(w)], match(id,pid), ], "POST-mask MIXTURE (harvest-relevant)")
cat("\n  predicted post-mask same_team dilution factor q*cv^2/(cv^2+(1-q)):\n")
for (pos in c("RB","TE","WR")) { q<-1-mech$p_miss_canonical_17g[[pos]]; cv<-CV_POS[[pos]]
  f <- q*cv^2/(cv^2+(1-q)); cat(sprintf("    %-3s factor=%.3f -> same_team %.3f (target 0.433)\n",
      pos, f, 0.433*f)) }
cat("  Mixture is the economically-relevant (insurance) correlation; masking\n")
cat("  lowers it (safe direction for the PR #35 TE-thesis). Copula fidelity\n")
cat("  itself (PRE-mask) is unchanged. The 0.05 flag applies only to fidelity.\n\n")

# ============================================================================
# Spot checks vs CDN-deployed (PR #34) feed
# ============================================================================
cat("================ spot checks: before (deployed) / after (new) ================\n")
dep <- tryCatch({ f<-tempfile(fileext=".json")
  writeBin(resp_body_raw(req_perform(req_timeout(
    request(paste0(CDN,"/v2/projections/",SLATE,".json")),120))), f)
  fromJSON(f, simplifyVector=FALSE) }, error=function(e) NULL)
dep_get <- function(nm,fld){ if(is.null(dep)) return(NA_real_)
  for(p in dep$players) if(identical(tolower(p$name%||%""),tolower(nm))) return(getn(p[[fld]])); NA_real_ }
spot <- c("Bijan Robinson","Christian McCaffrey","Ja'Marr Chase","CeeDee Lamb",
          "Josh Allen","Trey McBride","Sam LaPorta","Travis Kelce","Tucker Kraft")
cat(sprintf("%-22s %-3s %7s %7s %7s | %7s %7s\n",
            "name","pos","new_sm","dep_sm","d%","new_std","dep_std"))
for (nm in spot) { r <- df[which(tolower(df$name)==tolower(nm))[1],]
  if (nrow(r)==0||is.na(r$uid)) { cat(sprintf("%-22s  <not found>\n",nm)); next }
  ds <- dep_get(nm,"season_mean")
  cat(sprintf("%-22s %-3s %7.1f %7.1f %+6.1f | %7.1f %7.1f\n", substr(r$name,1,22),
      r$pos, r$smean, ds, if(is.na(ds))NA else 100*(r$smean-ds)/ds,
      r$sstd, dep_get(nm,"season_std"))) }
if (is.null(dep)) cat("\n  (CDN deployed feed unavailable; dep_* = NA)\n")
cat("\nDONE.\n")
