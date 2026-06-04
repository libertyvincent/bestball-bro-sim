# Loose end -- was PR #27's "unique over-priced ~68%" the compute_team_ev
# MC artifact? PR #27 measured v1 over-pricing against compute_team_ev (the
# static-opponent, fat-tail-undercounting reference). Re-measure the v1
# pricing error against the TRUSTED co-moving oracle and contrast.
#
# For a variance-stratified sample of real field rosters:
#   * v1            = evaluate_roster_curve_ev
#   * oracle-comove = oracle_roster_ev (trusted truth)
#   * oracle-static = oracle_roster_ev static (== v1 sanity)
#   * ctev          = compute_team_ev (old reference, 3-seed mean)
# Report v1 - oracle (TRUE error) vs v1 - ctev (PR #27 error), by variance
# tercile. If high-variance rosters show big v1-ctev but small/negative
# v1-oracle, PR #27's over-pricing was the artifact.
#
# Reuses build/oracle_field_regate.rds (N=4000 F=2600). Run from repo root:
#   "<Rscript>" inst/scripts/oracle_pr27.R > build/oracle_pr27.log 2>&1

suppressMessages(devtools::load_all(".", quiet = TRUE))
SLATE_ID <- "nfl_2026_season"; SEED <- 1L; FIELD_TEAMS <- 2700L
N_PATHS <- 4000L; MAX_FIELD <- 2600L
N_PER_TERCILE <- 8L; CTEV_SIMS <- 4000L; CTEV_SEEDS <- 3L

t0 <- Sys.time(); stamp <- function(m) cat(sprintf("[%6.1f min] %s\n",
  as.numeric(difftime(Sys.time(),t0,units="mins")), m))
ckpt <- readRDS(file.path("build","ev_smoke_ckpt.rds"))
layerA <- ckpt$layerA; pool_draws <- ckpt$pool_draws; field_scores <- ckpt$field_scores
feed <- blend_slate(SLATE_ID,
  sources_manifest_path=file.path("inst","data","sources","_manifest.yaml"),
  slates_manifest_path =file.path("inst","data","slates","_manifest.yaml"), write_json=FALSE)
positions <- positions_from_feed(feed); schedule <- schedule_from_feed(feed)
lineup_spec <- load_slate_lineup_spec(SLATE_ID); pcfg <- load_tournament("puppy2")
curves_p2 <- build_tournament_curves(pcfg, field_scores, n_grid=256L, seed=SEED)
field_cache_p2 <- build_field_payouts(field_scores, pcfg, seed=SEED)
picks <- load_scraped_drafts(); pool <- load_slate_data(SLATE_ID)
targets <- compute_field_targets(picks, slate_id=SLATE_ID)
field <- generate_field(SLATE_ID, picks=picks, player_pool=pool, targets=targets, n_teams=FIELD_TEAMS, seed=SEED)
field_rosters <- split(field$rosters$underdog_id, field$rosters$entry_id)
covered_ids <- pool_draws$player_ids; sw <- curves_p2$stage_weeks; cstruct <- oracle_cfg_struct(pcfg)
ev_draws <- pool_draws; ev_draws$tensor <- pool_draws$tensor[,,seq_len(N_PATHS),drop=FALSE]; ev_draws$n_paths <- N_PATHS
ofield <- readRDS(file.path("build","oracle_field_regate.rds"))
stopifnot(ofield$N==N_PATHS)
stamp(sprintf("loaded oracle field N=%d F=%d", ofield$N, ofield$F))

# Reconstruct the column->entry_id map (oracle_score_field's deterministic selection).
cov_n <- vapply(field_rosters, function(r) sum(r %in% covered_ids), integer(1))
elig <- names(field_rosters)[cov_n >= 15L]
if (length(elig) > MAX_FIELD) { set.seed(SEED); elig <- sample(elig, MAX_FIELD) }
stopifnot(length(elig) == ofield$F)

# Roster total-score variance (boom-bust) from the oracle field scores.
tot <- ofield$fR$R1 + ofield$fR$R2 + ofield$fR$R3 + ofield$fR$R4   # [N x F]
rsd <- matrixStats::colSds(tot)
terc <- cut(rsd, quantile(rsd, c(0,1/3,2/3,1)), include.lowest=TRUE, labels=c("low","mid","high"))
set.seed(SEED)
samp_idx <- unlist(lapply(c("low","mid","high"), function(g)
  sample(which(terc==g), min(N_PER_TERCILE, sum(terc==g)))))
stamp(sprintf("sampled %d rosters across variance terciles", length(samp_idx)))

ctev_ev <- function(roster, e_id, seeds) {
  pod_pool <- setdiff(names(field_rosters), e_id)
  vals <- numeric(length(seeds))
  for (si in seq_along(seeds)) {
    set.seed(seeds[si]*7L); pid <- sample(pod_pool, 11L)
    pod <- c(field_rosters[pid], stats::setNames(list(roster), e_id))
    vals[si] <- compute_team_ev(pod_rosters=pod, team_entry_id=e_id, positions=positions,
      layerA_draws=layerA, schedule=schedule, lineup_spec=lineup_spec, tournament_cfg=pcfg,
      field_cache=field_cache_p2, n_sims=CTEV_SIMS, seed=seeds[si])$team_ev$total_ev
  }
  c(mean=mean(vals), sd=stats::sd(vals))
}

D <- data.frame(entry=character(0), terc=character(0), rsd=numeric(0),
                v1=numeric(0), os=numeric(0), oc=numeric(0), ctev=numeric(0), ctev_sd=numeric(0))
for (col in samp_idx) {
  eid <- elig[col]; roster <- intersect(field_rosters[[eid]], covered_ids)
  rs <- roster_round_scores(roster, ev_draws, sw, lineup_spec)
  v1 <- evaluate_roster_curve_ev(roster, ev_draws, curves_p2, lineup_spec)$ev
  os <- oracle_roster_ev(rs, ofield, mode="static", final_mode="binom")$ev
  oc <- oracle_roster_ev(rs, ofield, mode="comove", final_mode="binom")$ev
  ct <- ctev_ev(field_rosters[[eid]], eid, SEED + seq_len(CTEV_SEEDS))
  D <- rbind(D, data.frame(entry=eid, terc=as.character(terc[col]), rsd=rsd[col],
                           v1=v1, os=os, oc=oc, ctev=ct["mean"], ctev_sd=ct["sd"]))
}
stamp("evaluated all rosters")
D$err_v1_oracle <- D$v1 - D$oc     # TRUE pricing error (oracle = truth)
D$err_v1_ctev   <- D$v1 - D$ctev   # PR #27-style error (old reference)

cat("\n=== PER-ROSTER (v1 vs trusted oracle vs old ctev) ===\n")
for (i in seq_len(nrow(D))) cat(sprintf(
  "  %-12s %-4s rsd%5.1f  v1 $%6.2f  os $%6.2f  oc $%6.2f  ctev $%6.2f(+-%.2f) | v1-oracle $%+6.2f  v1-ctev $%+6.2f\n",
  substr(D$entry[i],1,12), D$terc[i], D$rsd[i], D$v1[i], D$os[i], D$oc[i], D$ctev[i], D$ctev_sd[i],
  D$err_v1_oracle[i], D$err_v1_ctev[i]))

cat("\n=== BY VARIANCE TERCILE (mean signed error, mean |error|) ===\n")
for (g in c("low","mid","high")) {
  sub <- D[D$terc==g,]
  cat(sprintf("  %-4s (n=%d): v1-oracle signed $%+.2f |.|$%.2f  |  v1-ctev signed $%+.2f |.|$%.2f  | mean v1 $%.2f oc $%.2f ctev $%.2f\n",
      g, nrow(sub), mean(sub$err_v1_oracle), mean(abs(sub$err_v1_oracle)),
      mean(sub$err_v1_ctev), mean(abs(sub$err_v1_ctev)),
      mean(sub$v1), mean(sub$oc), mean(sub$ctev)))
}
cat("\n  [PR#27 artifact confirmed if high-tercile v1-ctev is large +$ (over-priced) but v1-oracle is small/negative]\n")
cat(sprintf("\n  sanity: mean |oracle-static - v1| = $%.3f (should be small -> oracle reimplements v1)\n",
            mean(abs(D$os - D$v1))))
saveRDS(D, file.path("build","oracle_pr27_results.rds"))
stamp("done")
