#!/usr/bin/env Rscript
# ============================================================================
# TE cross-correlation diagnostic (DIAGNOSE-ONLY)
# ============================================================================
# Verifies the EV brain's one load-bearing unverified assumption: that the
# live tensor's TE room does NOT boom/bust together (the max-of-N harvest
# value collapses as cross-correlation rises). The intended design is the
# Option A copula triple (team 0.45 / game 0.25 / cross 0.05); this script
# measures what the LIVE tensor actually carries.
#
# Reads ONLY published artifacts off the CDN. No package source, tensor
# generation, feed, config, or test is touched. Run from repo root:
#   Rscript diagnostics/te_correlation_check.R
#
# Build identity is recorded at runtime (the feed rebuilds daily from
# source); we never mix builds within a part.
#
# Statistics (per hub review):
#   * Spearman = implementation-fidelity stat (monotone-invariant, survives
#     the inverse-CDF marginal map). Compared to LATENT targets via
#     rho_S = (6/pi)*asin(rho/2):  team .45->.434  game .25->.239  cross .05->.0478.
#     The +/-0.05 mismatch flag applies to THIS comparison.
#   * Pearson = economic stat the harvest math feels. Sub-latent Pearson is
#     expected attenuation, not a flag.
#   * Primary granularity: per-week across the 500 paths, restricted to weeks
#     where both players are non-bye/active (positive variance). Pooled-across-
#     weeks reported as a robustness check only.

suppressWarnings(suppressMessages({
  library(httr2); library(jsonlite); library(digest)
}))

`%||%` <- function(a, b) if (is.null(a)) b else a
CDN  <- "https://libertyvincent.github.io/bestball-bro-data"
CACHE <- "build/te_corr"          # gitignored scratch
dir.create(CACHE, recursive = TRUE, showWarnings = FALSE)
TOP_N_PER_POS <- 40L              # fantasy-relevant restriction
LATENT <- list(team = 0.45, game = 0.25, cross = 0.05)
sp_target <- function(rho) (6 / pi) * asin(rho / 2)   # Gaussian Spearman

# ---- fetch + record build identity -----------------------------------------
fetch <- function(rel, dest) {
  resp <- req_perform(req_timeout(request(paste0(CDN, "/", rel)), 180))
  writeBin(resp_body_raw(resp), dest); dest
}
bin_f  <- fetch("v2/ev/nfl_2026_season_draws.bin",  file.path(CACHE, "draws.bin"))
side_f <- fetch("v2/ev/nfl_2026_season_draws.json", file.path(CACHE, "draws.json"))
proj_f <- fetch("v2/projections/nfl_2026_season.json", file.path(CACHE, "proj.json"))

sc   <- fromJSON(side_f, simplifyVector = TRUE)
proj <- fromJSON(proj_f, simplifyVector = FALSE)
BUILD <- list(
  draws_generated_at = sc$generated_at,
  bin_sha256         = digest(file = bin_f,  algo = "sha256"),
  sidecar_sha256     = digest(file = side_f, algo = "sha256"),
  proj_generated_at  = proj[["_meta"]]$generated_at,
  availability_on    = !is.null(proj[["_meta"]]$availability_mechanism))
cat("=== BUILD PINNED AT RUNTIME ===\n")
for (k in names(BUILD)) cat(sprintf("  %-20s %s\n", k, BUILD[[k]]))

# ---- decode tensor: [week, player, path], scores /quant_scale --------------
n_total <- sc$n_paths * sc$n_players * sc$n_weeks
v  <- readBin(bin_f, what = "integer", n = n_total, size = 2L,
              signed = TRUE, endian = "little")
stopifnot(length(v) == n_total)
TN <- array(v, dim = c(sc$n_weeks, sc$n_players, sc$n_paths)) / sc$quant_scale
weeks      <- as.integer(sc$weeks)
pidx       <- unlist(sc$player_index)                 # 0-based
player_ids <- names(sort(pidx))                       # canonical row order
positions  <- unlist(sc$positions)[player_ids]
stopifnot(identical(player_ids, names(positions)))

# ---- schedule (team / opponent / bye) from the SAME-BUILD projection feed --
# team_of[player, week] and game_of (unordered NFL game key) drive bucketing.
team_of <- matrix(NA_character_, nrow = sc$n_players, ncol = sc$n_weeks,
                  dimnames = list(player_ids, as.character(weeks)))
game_of <- team_of
bye_of  <- matrix(TRUE, nrow = sc$n_players, ncol = sc$n_weeks,
                  dimnames = list(player_ids, as.character(weeks)))
season_mean <- setNames(rep(NA_real_, sc$n_players), player_ids)
for (uid in player_ids) {
  pl <- proj$players[[uid]]
  if (is.null(pl)) next
  season_mean[uid] <- as.numeric(pl$season_mean %||% NA_real_)
  tm <- pl$team %||% NA_character_
  for (wk in pl$weekly %||% list()) {
    w <- as.integer(wk$week %||% NA_integer_); if (is.na(w)) next
    wc <- as.character(w); if (!(wc %in% colnames(team_of))) next
    is_bye <- isTRUE(wk$is_bye)
    bye_of[uid, wc] <- is_bye
    if (!is_bye) {
      opp <- wk$opponent %||% NA_character_
      team_of[uid, wc] <- tm
      game_of[uid, wc] <- if (is.na(tm) || is.na(opp)) NA_character_
                          else paste(sort(c(tm, opp)), collapse = "_")
    }
  }
}

# ---- fantasy-relevant restriction: top-N per position by season mean -------
top_ids_by_pos <- function(pos) {
  ids <- player_ids[positions == pos]
  ids <- ids[!is.na(season_mean[ids])]
  head(ids[order(-season_mean[ids])], TOP_N_PER_POS)
}
TOP <- lapply(c("QB","RB","WR","TE"), top_ids_by_pos)
names(TOP) <- c("QB","RB","WR","TE")

# ---- per-week pairwise correlations, classified into buckets ---------------
# For a week w and a player-id set, build [500 x k] active matrix and take the
# pairwise cor (Pearson & Spearman) in one shot, then melt the upper triangle
# (or the cross block) and classify each (pair, week) cell by team/game keys.
wk_active <- function(ids, w) {
  wc <- as.character(w)
  ok <- vapply(ids, function(id) {
    !bye_of[id, wc] && !is.na(team_of[id, wc]) &&
      stats::sd(TN[match(w, weeks), match(id, player_ids), ]) > 0
  }, logical(1))
  ids[ok]
}
path_mat <- function(ids, w) {  # [500 x length(ids)]
  wi <- match(w, weeks)
  m <- t(TN[wi, match(ids, player_ids), , drop = FALSE][1, , ])
  colnames(m) <- ids; m
}

cells <- list()  # accumulate data.frame rows
add_within <- function(pos) {
  ids0 <- TOP[[pos]]
  for (w in weeks) {
    ids <- wk_active(ids0, w); if (length(ids) < 2L) next
    M <- path_mat(ids, w)
    rp <- suppressWarnings(stats::cor(M, method = "pearson"))
    rs <- suppressWarnings(stats::cor(M, method = "spearman"))
    wc <- as.character(w)
    for (a in 1:(length(ids) - 1L)) for (b in (a + 1L):length(ids)) {
      ia <- ids[a]; ib <- ids[b]
      bucket <- if (identical(team_of[ia, wc], team_of[ib, wc])) "same_team"
                else if (identical(game_of[ia, wc], game_of[ib, wc])) "same_game"
                else "cross"
      cells[[length(cells) + 1L]] <<- data.frame(
        kind = paste0(pos, "x", pos), bucket = bucket,
        pair = paste(sort(c(ia, ib)), collapse = "|"), week = w,
        pearson = rp[a, b], spearman = rs[a, b], stringsAsFactors = FALSE)
    }
  }
}
# TExWR same-team reference (stack behaviour): cross-position block.
add_te_wr_same_team <- function() {
  for (w in weeks) {
    te <- wk_active(TOP$TE, w); wr <- wk_active(TOP$WR, w)
    if (!length(te) || !length(wr)) next
    M <- path_mat(c(te, wr), w)
    rp <- suppressWarnings(stats::cor(M, method = "pearson"))
    rs <- suppressWarnings(stats::cor(M, method = "spearman"))
    wc <- as.character(w)
    for (t in te) for (r in wr) {
      if (!identical(team_of[t, wc], team_of[r, wc])) next  # same-team only
      cells[[length(cells) + 1L]] <<- data.frame(
        kind = "TExWR", bucket = "same_team",
        pair = paste(t, r, sep = "|"), week = w,
        pearson = rp[t, r], spearman = rs[t, r], stringsAsFactors = FALSE)
    }
  }
}

for (p in c("TE","RB","WR")) add_within(p)
add_te_wr_same_team()
DF <- do.call(rbind, cells)

# ---- aggregate per (kind, bucket): per-week mean +/- sd, n -----------------
agg <- function(d) {
  data.frame(
    n_cells   = nrow(d),
    n_pairs   = length(unique(d$pair)),
    pearson   = mean(d$pearson),
    pearson_sd= stats::sd(d$pearson),
    spearman  = mean(d$spearman),
    spearman_sd = stats::sd(d$spearman))
}
keys <- unique(DF[, c("kind","bucket")])
keys <- keys[order(keys$kind, keys$bucket), ]
report <- do.call(rbind, lapply(seq_len(nrow(keys)), function(i) {
  d <- DF[DF$kind == keys$kind[i] & DF$bucket == keys$bucket[i], ]
  cbind(keys[i, ], agg(d))
}))

sp_tgt_of <- function(bucket) sp_target(LATENT[[ if (bucket=="same_team") "team"
                                          else if (bucket=="same_game") "game"
                                          else "cross" ]])
report$spearman_target <- vapply(report$bucket, sp_tgt_of, numeric(1))
report$spearman_gap    <- report$spearman - report$spearman_target
report$FLAG <- ifelse(abs(report$spearman_gap) > 0.05, "**", "")

cat("\n=== PART 1: per-week correlation by bucket (top-", TOP_N_PER_POS,
    "/pos by season mean) ===\n", sep = "")
cat("Spearman_target = (6/pi)asin(rho_latent/2); FLAG ** if |Spearman-target|>0.05\n")
cat("Pearson is the economic stat (sub-latent = expected attenuation, not flagged)\n\n")
print(within(report, {
  pearson <- round(pearson, 3); pearson_sd <- round(pearson_sd, 3)
  spearman <- round(spearman, 3); spearman_sd <- round(spearman_sd, 3)
  spearman_target <- round(spearman_target, 3); spearman_gap <- round(spearman_gap, 3)
}), row.names = FALSE)

# ---- robustness: pooled-across-weeks (same_team & cross only) ---------------
pooled_pair <- function(ia, ib) {
  wok <- weeks[vapply(weeks, function(w) {
    wc <- as.character(w)
    !bye_of[ia,wc] && !bye_of[ib,wc] &&
      stats::sd(TN[match(w,weeks),match(ia,player_ids),])>0 &&
      stats::sd(TN[match(w,weeks),match(ib,player_ids),])>0
  }, logical(1))]
  if (length(wok) < 3L) return(c(NA, NA))
  xa <- c(); xb <- c()
  for (w in wok) { wi<-match(w,weeks)
    xa <- c(xa, TN[wi, match(ia,player_ids), ]); xb <- c(xb, TN[wi, match(ib,player_ids), ]) }
  c(stats::cor(xa, xb, method="pearson"), stats::cor(xa, xb, method="spearman"))
}
pooled_bucket <- function(kind, bucket) {
  d <- DF[DF$kind==kind & DF$bucket==bucket, ]
  prs <- unique(d$pair); if (!length(prs)) return(NULL)
  m <- t(vapply(prs, function(pk){ ab<-strsplit(pk,"\\|")[[1]]; pooled_pair(ab[1],ab[2]) }, numeric(2)))
  data.frame(kind=kind, bucket=bucket, n_pairs=sum(!is.na(m[,1])),
             pearson_pooled=mean(m[,1],na.rm=TRUE), spearman_pooled=mean(m[,2],na.rm=TRUE))
}
cat("\n=== robustness: pooled-across-weeks (mixes within-week + mean co-move) ===\n")
pooled <- do.call(rbind, list(
  pooled_bucket("TExTE","cross"), pooled_bucket("TExTE","same_team"),
  pooled_bucket("WRxWR","cross"), pooled_bucket("RBxRB","cross")))
print(within(pooled, { pearson_pooled<-round(pearson_pooled,3)
  spearman_pooled<-round(spearman_pooled,3) }), row.names = FALSE)

saveRDS(list(build=BUILD, report=report, pooled=pooled, cells=DF),
        file.path(CACHE, "part1_result.rds"))
cat("\nDONE Part 1.\n")

# ============================================================================
# PART 2 — sensitivity: max-of-N harvest value vs rho_cross
# ============================================================================
# Per hub: Part 1 showed attenuation is negligible at top-40, so treat
# latent rho ~= realized rho in the sweep. Report BOTH % shrinkage and the
# ABSOLUTE point shrinkage (Delta of the Delta-E[weekly max], summed weeks).
set.seed(1L)

# Empirical per-(player, week) marginal from the tensor (sorted 500 draws).
marg <- function(id, w) sort(TN[match(w, weeks), match(id, player_ids), ])
inv7 <- function(sorted, u) {            # type-7 empirical inverse CDF
  m <- length(sorted); if (m == 0L) return(rep(0, length(u)))
  if (m == 1L) return(rep(sorted, length(u)))
  u <- pmin(pmax(u, 0), 1); h <- (m - 1) * u + 1
  q <- pmin(pmax(as.integer(floor(h)), 1L), m - 1L); fr <- h - q
  sorted[q] + fr * (sorted[q + 1L] - sorted[q])
}
# Representative rooms by in-tensor season-mean rank.
rank_ids <- function(pos, ranks) {
  ids <- player_ids[positions == pos]; ids <- ids[!is.na(season_mean[ids])]
  ids <- ids[order(-season_mean[ids])]; ids[ranks]
}
TE_ROOM5 <- rank_ids("TE", c(8, 15, 22, 28, 35))   # TE1..TE5 by mean
TE_ROOM4 <- TE_ROOM5[1:4]
RB_ROOM2 <- rank_ids("RB", c(8, 22))
cat("\n=== PART 2 rooms (underdog_id @ season-mean rank) ===\n")
cat("TE room (ranks 8/15/22/28/35):\n"); for (i in seq_along(TE_ROOM5))
  cat(sprintf("  TE%d  %s  mean=%.1f\n", i, TE_ROOM5[i], season_mean[TE_ROOM5[i]]))
cat("RB contrast room (ranks 8/22):\n"); for (id in RB_ROOM2)
  cat(sprintf("  %s  mean=%.1f\n", id, season_mean[id]))

# E[season-summed weekly max] of a room, from the REAL tensor draws.
real_room_value <- function(ids) {
  tot <- 0
  for (w in weeks) {
    M <- TN[match(w, weeks), match(ids, player_ids), , drop = FALSE][1, , ]
    if (is.null(dim(M))) M <- matrix(M, nrow = 1)   # single-player room
    tot <- tot + mean(apply(M, 2, max))             # max over room per path
  }
  tot
}
# Same, but with cross-player dependence destroyed: independent path
# permutation for EVERY (player, week) cell, averaged over R replicates.
shuffled_room_value <- function(ids, R = 200L) {
  np <- length(ids); vals <- numeric(R)
  for (r in seq_len(R)) {
    tot <- 0
    for (w in weeks) {
      M <- TN[match(w, weeks), match(ids, player_ids), , drop = FALSE][1, , ]
      if (is.null(dim(M))) M <- matrix(M, nrow = 1)
      for (j in seq_len(np)) M[j, ] <- M[j, sample.int(ncol(M))]  # per-cell perm
      tot <- tot + mean(apply(M, 2, max))
    }
    vals[r] <- tot
  }
  c(mean = mean(vals), sd = stats::sd(vals))
}

cat("\n=== Shuffle test: tensor-priced correlation drag on room value ===\n")
cat("(real = as-shipped; shuffled = cross-dependence destroyed; gap = drag)\n")
for (room in list(list(nm="TE room (4)", ids=TE_ROOM4),
                  list(nm="RB room (2)", ids=RB_ROOM2))) {
  rv <- real_room_value(room$ids); sv <- shuffled_room_value(room$ids)
  cat(sprintf("  %-12s real=%.1f  shuffled=%.1f (sd %.2f)  gap=%.2f pts (%.2f%%)\n",
              room$nm, rv, sv["mean"], sv["sd"], sv["mean"] - rv,
              100 * (sv["mean"] - rv) / rv))
}

# --- Synthetic rho sweep: equicorrelation copula, tensor marginals ----------
# All-cross TE room (typical: different teams/games). Latent Z = sqrt(rho)*G +
# sqrt(1-rho)*E (pairwise corr rho), fresh factors per week, mapped to each
# TE's week-w empirical marginal. V(m,rho) = E[season-summed max over first m].
N_SWEEP <- 10000L
room_value_synth <- function(ids, rho, n = N_SWEEP) {
  k <- length(ids); Vm <- numeric(k)   # cumulative-room values for sizes 1..k
  acc <- numeric(k)
  for (w in weeks) {
    G <- stats::rnorm(n); E <- matrix(stats::rnorm(n * k), n, k)
    Z <- sqrt(rho) * G + sqrt(1 - rho) * E
    U <- stats::pnorm(Z)
    D <- vapply(seq_len(k), function(j) inv7(marg(ids[j], w), U[, j]), numeric(n))
    cmax <- D[, 1]
    acc[1] <- acc[1] + mean(cmax)
    for (m in 2:k) { cmax <- pmax(cmax, D[, m]); acc[m] <- acc[m] + mean(cmax) }
  }
  acc
}
RHOS <- c(0.05, 0.15, 0.30, 0.50)
V <- vapply(RHOS, function(r) room_value_synth(TE_ROOM5, r), numeric(5))
rownames(V) <- paste0("room", 1:5); colnames(V) <- paste0("rho", RHOS)

# Marginal value of the m-th TE = V(m) - V(m-1), per rho.
marg_val <- rbind(TE3 = V[3, ] - V[2, ], TE4 = V[4, ] - V[3, ],
                  TE5 = V[5, ] - V[4, ])
base <- marg_val[, 1]                                # rho = 0.05 baseline
abs_shrink <- base - marg_val                        # points (Delta of Delta)
pct_shrink <- 100 * abs_shrink / base

cat("\n=== Synthetic rho sweep: marginal value of TE3/TE4/TE5 (pts, summed wks) ===\n")
print(round(marg_val, 2))
cat("\n--- ABSOLUTE shrinkage vs rho=0.05 (pts) ---\n"); print(round(abs_shrink, 2))
cat("\n--- %% shrinkage vs rho=0.05 ---\n"); print(round(pct_shrink, 1))

# Consistency cross-check: synthetic V(4) @ rho=0.05 vs the real tensor 4-TE room.
cat(sprintf("\nCross-check: synth V(4)@rho=0.05 = %.1f vs real tensor 4-TE room = %.1f\n",
            V[4, 1], real_room_value(TE_ROOM4)))

saveRDS(list(build=BUILD, te_room5=TE_ROOM5, rb_room2=RB_ROOM2,
             marg_val=marg_val, abs_shrink=abs_shrink, pct_shrink=pct_shrink, V=V),
        file.path(CACHE, "part2_result.rds"))
cat("\nDONE Part 2.\n")
