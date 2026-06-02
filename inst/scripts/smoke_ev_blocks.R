# Headline EV building-blocks run (docs/ev_building_blocks_contract.md, Track 1).
#
#   1. Build Artifact A (path-aligned joint draws) for nfl_2026_season from the
#      existing Layer A pipeline (blend + simulate_slate -- the same draws the
#      v2/draws parquet carries; reused, not regenerated).
#   2. Build Artifact B (the six curves) for Puppy 2 (anchor) + Dachshund from a
#      synthetic field scored by the same sim run.
#   3. VALIDATION GATE: curve-based EV vs the full Layer-B sim EV
#      (compute_team_ev) for test rosters spanning chalky -> unique. Reports
#      the leverage/duplication drift the contract calls out.
#   4. PATH-COUNT PROTOCOL: marginal-EV ranking stability at N = 250/500/1000
#      (Kendall-tau + top-10 overlap across disjoint path resamples) -> pick N.
#   5. Publish the artifacts (at the chosen N) + _meta registration to
#      build/ev_smoke/ for inspection.
#
# Run from the package root:
#   "<Rscript>" -e 'devtools::load_all()' -e 'source("inst/scripts/smoke_ev_blocks.R")'
# or via Rscript with devtools::load_all() inside (below).

suppressMessages(devtools::load_all(".", quiet = TRUE))

# ---- knobs --------------------------------------------------------------------
SLATE_ID        <- "nfl_2026_season"
LAYERA_SIMS     <- 1500L
POOL_PATHS      <- 4000L     # one big joint draw; protocol resamples subsets
TOP_N_PLAYERS   <- 300L
FIELD_TEAMS     <- 2700L     # multiple of 300 (puppy2) AND 180 (dachshund)
FIELD_SIMS      <- 400L
TEAM_EV_SIMS    <- 1500L     # compute_team_ev reference sims
N_GATE_ROSTERS  <- 8L
PROTOCOL_NS     <- c(250L, 500L, 1000L)
PROTOCOL_K      <- 4L        # disjoint resamples per N
OUT_DIR         <- file.path("build", "ev_smoke")
SEED            <- 1L

t_start <- Sys.time()
stamp <- function(label) {
  cat(sprintf("[%6.1f min] %s\n",
              as.numeric(difftime(Sys.time(), t_start, units = "mins")), label))
}

# Checkpoint the expensive intermediates so a failure downstream (or a
# parameter tweak in the gate/protocol) doesn't redo the heavy sims.
CKPT <- file.path("build", "ev_smoke_ckpt.rds")
ckpt <- if (file.exists(CKPT)) readRDS(CKPT) else list()
save_ckpt <- function() saveRDS(ckpt, CKPT)

# ---- 1. Layer A: blend + simulate (the existing pipeline) ----------------------
stamp("blending slate + simulating Layer A")
feed <- blend_slate(
  slate_id              = SLATE_ID,
  sources_manifest_path = file.path("inst", "data", "sources", "_manifest.yaml"),
  slates_manifest_path  = file.path("inst", "data", "slates", "_manifest.yaml"),
  write_json            = FALSE)
if (is.null(ckpt$layerA)) {
  sim <- simulate_slate(feed, n_sims = LAYERA_SIMS, seed = SEED)
  ckpt$layerA <- sim$draws; save_ckpt()
}
layerA <- ckpt$layerA

positions   <- positions_from_feed(feed)
schedule    <- schedule_from_feed(feed)
lineup_spec <- load_slate_lineup_spec(SLATE_ID)

# ---- 2a. Artifact A: one big joint draw (protocol pool) ------------------------
stamp(sprintf("building joint path pool (%d paths x %d players)", POOL_PATHS, TOP_N_PLAYERS))
if (is.null(ckpt$pool_draws)) {
  ckpt$pool_draws <- build_ev_draws(feed, layerA, slate_id = SLATE_ID,
                                    n_paths = POOL_PATHS, top_n = TOP_N_PLAYERS,
                                    seed = SEED)
  save_ckpt()
}
pool_draws <- ckpt$pool_draws
cat(sprintf("  players: %d   weeks: %d   paths: %d   tensor: %.1f MB raw int16\n",
            length(pool_draws$player_ids), length(pool_draws$weeks),
            pool_draws$n_paths,
            length(pool_draws$player_ids) * length(pool_draws$weeks) *
              pool_draws$n_paths * 2 / 1e6))

# Subset helper: a bbbro_ev_draws restricted to a path (column) subset.
# Paths are iid, so disjoint subsets are independent samples.
.subset_paths <- function(ed, path_idx) {
  out <- ed
  out$tensor <- ed$tensor[, , path_idx, drop = FALSE]
  out$n_paths <- length(path_idx)
  out
}

# ---- 2b. Artifact B: field + curves (same sim run) ----------------------------
stamp("generating + scoring the synthetic field (curves source)")
picks   <- load_scraped_drafts()
pool    <- load_slate_data(SLATE_ID)
targets <- compute_field_targets(picks, slate_id = SLATE_ID)
field   <- generate_field(SLATE_ID, picks = picks, player_pool = pool,
                          targets = targets, n_teams = FIELD_TEAMS, seed = SEED)
field_rosters <- split(field$rosters$underdog_id, field$rosters$entry_id)
cat(sprintf("  field rosters fully covered by top-%d draws: %d / %d\n",
            TOP_N_PLAYERS,
            sum(vapply(field_rosters, function(r)
              all(r %in% pool_draws$player_ids), logical(1))),
            length(field_rosters)))

if (is.null(ckpt$field_scores)) {
  ckpt$field_scores <- simulate_per_stage_scores(
    rosters = field_rosters, positions = positions, layerA_draws = layerA,
    schedule = schedule, lineup_spec = lineup_spec,
    n_sims = FIELD_SIMS, seed = SEED)
  save_ckpt()
}
field_scores <- ckpt$field_scores
stamp("field scored")

pcfg <- load_tournament("puppy2")
dcfg <- load_tournament("dachshund")
curves_p2 <- build_tournament_curves(pcfg, field_scores, n_grid = 256L, seed = SEED)
curves_da <- build_tournament_curves(dcfg, field_scores, n_grid = 256L, seed = SEED)
stamp("curves built (puppy2 + dachshund)")

# Also keep the field payout pools for the gate's compute_team_ev calls.
field_cache_p2 <- build_field_payouts(field_scores, pcfg, seed = SEED)

# ---- 3. VALIDATION GATE --------------------------------------------------------
# Curve EV must reproduce compute_team_ev. Test rosters span chalky -> unique
# (mean field ownership of their players).
stamp("validation gate: selecting test rosters by ownership")
ownership <- table(field$rosters$underdog_id) / length(field_rosters)
roster_chalk <- vapply(field_rosters, function(r)
  mean(ownership[r], na.rm = TRUE), numeric(1))
ord <- order(roster_chalk, decreasing = TRUE)
# Only rosters fully covered by the draws artifact (all 18 players in top-300).
covered <- vapply(field_rosters, function(r)
  all(r %in% pool_draws$player_ids), logical(1))
chalky_pool <- ord[covered[ord]]
n_each <- N_GATE_ROSTERS %/% 2L
test_idx <- c(utils::head(chalky_pool, n_each),     # chalkiest covered rosters
              utils::tail(chalky_pool, n_each))     # most unique covered rosters
test_ids <- names(field_rosters)[test_idx]
test_group <- rep(c("chalky", "unique"), each = n_each)

N_PODS_PER_ROSTER <- 3L     # average compute_team_ev over pod draws (pod-luck noise)
gate_paths <- .subset_paths(pool_draws, seq_len(1000L))
gate <- data.frame(entry_id = test_ids, group = test_group,
                   chalk = roster_chalk[test_idx],
                   curve_ev = NA_real_, sim_ev = NA_real_, sim_ev_sd = NA_real_,
                   curve_q_adv = NA_real_, sim_q_adv = NA_real_)
for (i in seq_along(test_ids)) {
  eid <- test_ids[i]
  roster <- field_rosters[[eid]]
  # Curve-based EV on 1,000 shipped paths (+ mean qualifier advance prob).
  cev <- evaluate_roster_curve_ev(roster, gate_paths, curves_p2, lineup_spec)
  # Full Layer-B sim EV, averaged over several pod draws so pod luck
  # (which 11 specific field rosters you face) doesn't masquerade as
  # factorization error.
  sim_evs <- numeric(N_PODS_PER_ROSTER); sim_qs <- numeric(N_PODS_PER_ROSTER)
  for (p in seq_len(N_PODS_PER_ROSTER)) {
    set.seed(SEED + i * 100L + p)
    podmates <- sample(setdiff(names(field_rosters), eid), 11L)
    pod <- c(field_rosters[podmates], stats::setNames(list(roster), eid))
    sev <- compute_team_ev(
      pod_rosters = pod, team_entry_id = eid, positions = positions,
      layerA_draws = layerA, schedule = schedule, lineup_spec = lineup_spec,
      tournament_cfg = pcfg, field_cache = field_cache_p2,
      n_sims = TEAM_EV_SIMS, seed = SEED + i * 100L + p)
    sim_evs[p] <- sev$team_ev$total_ev
    sim_qs[p]  <- sev$advance_probs$qualifier
  }
  gate$curve_ev[i]    <- cev$ev
  gate$sim_ev[i]      <- mean(sim_evs)
  gate$sim_ev_sd[i]   <- stats::sd(sim_evs)
  gate$curve_q_adv[i] <- mean(cev$per_path$g1)
  gate$sim_q_adv[i]   <- mean(sim_qs)
  stamp(sprintf("  gate roster %d/%d (%s): curve $%.3f vs sim $%.3f (sd %.3f over %d pods)",
                i, length(test_ids), test_group[i], cev$ev, mean(sim_evs),
                stats::sd(sim_evs), N_PODS_PER_ROSTER))
}
gate$abs_err <- abs(gate$curve_ev - gate$sim_ev)
gate$rel_err <- gate$abs_err / pmax(gate$sim_ev, 0.01)
gate$signed  <- gate$curve_ev - gate$sim_ev

cat("\n=== VALIDATION GATE (Puppy 2, curve EV vs full sim EV) ===\n")
print(gate[, c("entry_id", "group", "chalk", "curve_ev", "sim_ev", "sim_ev_sd",
               "signed", "rel_err", "curve_q_adv", "sim_q_adv")],
      row.names = FALSE, digits = 3)
for (g in c("chalky", "unique")) {
  sub <- gate[gate$group == g, ]
  cat(sprintf("  %s:  mean curve $%.3f  mean sim $%.3f  mean signed $%+.3f  MAE $%.3f (%.1f%%)  |  q_adv curve %.3f vs sim %.3f\n",
              g, mean(sub$curve_ev), mean(sub$sim_ev), mean(sub$signed),
              mean(sub$abs_err), 100 * mean(sub$abs_err) / mean(sub$sim_ev),
              mean(sub$curve_q_adv), mean(sub$sim_q_adv)))
}
cat(sprintf("  leverage/duplication signature (chalky signed - unique signed): $%+.3f\n",
            mean(gate$signed[gate$group == "chalky"]) -
              mean(gate$signed[gate$group == "unique"])))
cat(sprintf("  pod-draw noise floor (mean within-roster sim sd): $%.3f\n",
            mean(gate$sim_ev_sd)))

# ---- 4. PATH-COUNT PROTOCOL ----------------------------------------------------
# Marginal-EV ranking stability across disjoint path resamples at each N.
stamp("path-count protocol")
# Realistic mid-draft state: seat-1 snake roster 8 picks in, by ADP.
adp <- vapply(feed$players, function(p) as.numeric(p$adp %||% NA_real_), numeric(1))
names(adp) <- names(feed$players)
adp <- adp[!is.na(adp)]
adp <- adp[names(adp) %in% pool_draws$player_ids]
adp_order <- names(sort(adp))
snake_picks <- c(1L, 24L, 25L, 48L, 49L, 72L, 73L, 96L)
roster8 <- adp_order[snake_picks]
candidates <- setdiff(adp_order, roster8)[1:20]

kendall_tau <- function(r1, r2) {
  common <- intersect(r1, r2)
  suppressWarnings(stats::cor(match(common, r1), match(common, r2),
                              method = "kendall"))
}
topk_overlap <- function(r1, r2, k = 10L) {
  length(intersect(utils::head(r1, k), utils::head(r2, k))) / k
}

# Reference ranking on the FULL pool: the "truth" each resample is judged
# against. EV-regret = (full-pool marginal EV of the full-pool #1 pick) -
# (full-pool marginal EV of the resample's #1 pick). Converts ranking
# churn into dollars: churn among near-equal candidates is harmless;
# churn that costs EV is not.
rk_full <- rank_marginal_ev(roster8, candidates, pool_draws, curves_p2, lineup_spec)
full_mev <- stats::setNames(rk_full$marginal_ev, rk_full$underdog_id)
best_full <- max(full_mev)
cat(sprintf("\nFull-pool (N=%d) top-20 marginal EVs: best $%.3f, median $%.3f, IQR $%.3f\n",
            POOL_PATHS, best_full, stats::median(full_mev),
            stats::IQR(full_mev)))

protocol <- data.frame()
for (N in PROTOCOL_NS) {
  k_max <- min(PROTOCOL_K, POOL_PATHS %/% N)
  rankings <- list(); regrets <- numeric(k_max)
  for (k in seq_len(k_max)) {
    idx <- ((k - 1L) * N + 1L):(k * N)
    sub <- .subset_paths(pool_draws, idx)
    rk <- rank_marginal_ev(roster8, candidates, sub, curves_p2, lineup_spec)
    rankings[[k]] <- rk$underdog_id
    # Regret of this resample's top pick, valued on the full pool.
    regrets[k] <- best_full - full_mev[[rk$underdog_id[1L]]]
  }
  pairs <- utils::combn(seq_len(k_max), 2L)
  taus <- apply(pairs, 2L, function(pr) kendall_tau(rankings[[pr[1]]], rankings[[pr[2]]]))
  o10  <- apply(pairs, 2L, function(pr) topk_overlap(rankings[[pr[1]]], rankings[[pr[2]]]))
  o5   <- apply(pairs, 2L, function(pr) topk_overlap(rankings[[pr[1]]], rankings[[pr[2]]], k = 5L))
  protocol <- rbind(protocol, data.frame(
    n_paths = N, n_resamples = k_max,
    mean_kendall_tau = mean(taus), min_kendall_tau = min(taus),
    mean_top10_overlap = mean(o10), mean_top5_overlap = mean(o5),
    mean_pick_regret_usd = mean(regrets), max_pick_regret_usd = max(regrets)))
  stamp(sprintf("  N=%4d: tau=%.3f (min %.3f)  top10=%.2f  top5=%.2f  pick-regret mean $%.3f max $%.3f",
                N, mean(taus), min(taus), mean(o10), mean(o5),
                mean(regrets), max(regrets)))
}

cat("\n=== PATH-COUNT PROTOCOL (Puppy 2 marginal-EV ranking stability) ===\n")
print(protocol, row.names = FALSE, digits = 3)
# Primary criterion: the decision metric -- the resample's top pick costs
# (almost) nothing vs the full-pool best. Secondary: rank-order stability
# (informational; near-ties churn harmlessly forever).
REGRET_TOL <- 0.05   # $0.05 on a ~$4.44 mean entry EV (~1%)
ok <- protocol$mean_pick_regret_usd <= REGRET_TOL
N_CHOSEN <- if (any(ok)) min(protocol$n_paths[ok]) else max(protocol$n_paths)
cat(sprintf("\nCHOSEN N = %d  (smallest N with mean top-pick regret <= $%.2f)\n",
            N_CHOSEN, REGRET_TOL))

# ---- 5. Publish artifacts at the chosen N + _meta registration -----------------
stamp(sprintf("publishing artifacts at N = %d to %s", N_CHOSEN, OUT_DIR))
ship_draws <- .subset_paths(pool_draws, seq_len(N_CHOSEN))
publish_ev_blocks(OUT_DIR, ship_draws, list(curves_p2, curves_da))
meta <- jsonlite::fromJSON(file.path(OUT_DIR, "_meta.json"), simplifyVector = FALSE)
entry <- meta$slates[[SLATE_ID]]
bin_mb <- file.size(file.path(OUT_DIR, entry$v2_draws_path)) / 1e6
cat(sprintf("  draws: %s (%.2f MB raw)  sha %s...\n",
            entry$v2_draws_path, bin_mb, substr(entry$v2_draws_sha256, 1, 12)))
for (tid in names(entry$tournaments)) {
  cat(sprintf("  curves[%s]: %s  sha %s...\n", tid,
              entry$tournaments[[tid]]$curves_path,
              substr(entry$tournaments[[tid]]$curves_sha256, 1, 12)))
}

# Cache the heavyweight intermediates for fast re-runs / follow-up analysis.
saveRDS(list(gate = gate, protocol = protocol, N_chosen = N_CHOSEN,
             curves_p2 = curves_p2, curves_da = curves_da,
             roster8 = roster8, candidates = candidates),
        file.path("build", "ev_smoke_results.rds"))
stamp("done")
