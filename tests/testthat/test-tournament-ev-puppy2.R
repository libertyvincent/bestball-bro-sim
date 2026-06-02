# Tests for The Puppy 2 through the config-driven EV engine
# (R/tournament_ev.R). Pure config swap on the #25-fixed engine, mirroring
# the Dachshund tests, with one structural difference that makes it a
# cross-check of the loser-bucket anchor fix: BOTH loser buckets pay
# (SF losers $25, QF losers $5) -- the Dachshund only pays one.
#
# Everything runs off puppy2.yaml as committed, so these tests also guard
# the YAML itself: the tier sum, the single-head structure, and exact
# money conservation. No BBMDB qualifier gate -- the tournament is new
# (live 2026) and has no BBMDB history.

.puppy2_lineup_spec <- function() {
  list(slate_id = "puppy2-test", slots = list(
    list(pos = "QB",   n = 1L, eligible = c("QB")),
    list(pos = "RB",   n = 2L, eligible = c("RB")),
    list(pos = "WR",   n = 3L, eligible = c("WR")),
    list(pos = "TE",   n = 1L, eligible = c("TE")),
    list(pos = "FLEX", n = 1L, eligible = c("RB", "WR", "TE"))
  ))
}

PUPPY2_POOL <- 1000000            # published prize pool, sums from the tiers
PUPPY2_FIELD <- 225000L

# ---- field-multiple guardrail (config-derived, not hardcoded) ---------------

test_that("Puppy 2 field multiple is total_field_size / final seats (= 300)", {
  pcfg <- load_tournament("puppy2")
  expect_equal(tournament_field_multiple(pcfg), 300L)          # 225000 / 750
  expect_equal(resolve_tournament_field_size(pcfg, 1000L), 900L)   # 3 * 300
  expect_error(resolve_tournament_field_size(pcfg, 100L, snap = FALSE),
               "not a multiple of 300")
})

# ---- the YAML payout ladder itself -------------------------------------------

test_that("puppy2.yaml: fee $5, max 150 entries, single-head $1,000,000 ladder", {
  pcfg <- load_tournament("puppy2")
  expect_equal(pcfg$entry_fee_usd, 5)
  expect_equal(pcfg$max_entries_per_user, 150)

  # Single-head: no qualifier_round payout table.
  expect_null(pcfg$payouts$qualifier_round)
  tiers <- pcfg$payouts$championship_round$tiers
  expect_false(is.null(tiers))

  # Tier sum = the full $1,000,000 prize pool, exactly.
  tier_sum <- sum(vapply(tiers, function(t)
    (t$rank_to - t$rank_from + 1) * t$usd, numeric(1)))
  expect_equal(tier_sum, PUPPY2_POOL, tolerance = 1e-12)

  # Progression buckets reconcile: finalist tiers cover places 1-750, SF-loser
  # tiers 751-3750 (3,000 teams), QF-loser tiers 3751-37500 (33,750 teams).
  elig <- vapply(tiers, function(t) t$eligibility %||% NA_character_, character(1))
  fin_ranks <- unlist(lapply(tiers[elig == "finalist"],
                             function(t) t$rank_from:t$rank_to))
  sfl_ranks <- unlist(lapply(tiers[elig == "semifinals_loser"],
                             function(t) t$rank_from:t$rank_to))
  qfl_ranks <- unlist(lapply(tiers[elig == "quarterfinals_loser"],
                             function(t) t$rank_from:t$rank_to))
  expect_equal(sort(fin_ranks), 1:750)
  expect_equal(sort(sfl_ranks), 751:3750)
  expect_equal(sort(qfl_ranks), 3751:37500)
})

test_that("Puppy 2 per-bucket engine caps conserve the pool (per-tier accounting)", {
  pcfg <- load_tournament("puppy2")
  tiers <- pcfg$payouts$championship_round$tiers
  seats <- vapply(pcfg$stages, function(s) as.integer(s$seats_entering), integer(1))

  caps_money <- function(caps) sum(vapply(caps, function(c) c$n_slots * c$usd, numeric(1)))
  caps_slots <- function(caps) sum(vapply(caps, function(c) as.integer(c$n_slots), integer(1)))

  fin_caps <- bestballBroSim:::.tier_caps_from_yaml(
    tiers, eligibility = "finalist", total_slots = seats[4])
  sf_caps <- bestballBroSim:::.tier_caps_combined(
    tiers, primary_eligibility = bestballBroSim:::.champ_tags(3L, 4L),
    fallback_eligibility = "qualifier_advancer", total_slots = seats[3] - seats[4])
  qf_caps <- bestballBroSim:::.tier_caps_combined(
    tiers, primary_eligibility = bestballBroSim:::.champ_tags(2L, 4L),
    fallback_eligibility = "qualifier_advancer", total_slots = seats[2] - seats[3])

  # Every bucket's caps table is exactly bucket-sized -- with TWO paid loser
  # buckets, both must anchor at their own bucket top (the #25 fix on a
  # shape the Dachshund doesn't exercise) ...
  expect_equal(caps_slots(fin_caps), 750L)
  expect_equal(caps_slots(sf_caps),  3000L)
  expect_equal(caps_slots(qf_caps),  33750L)
  # ... and per-tier accounting recovers the published pool exactly:
  # $756,250 (finalists) + $75,000 (SF losers) + $168,750 (QF losers) = $1M.
  expect_equal(caps_money(fin_caps), 756250)
  expect_equal(caps_money(sf_caps),  75000)
  expect_equal(caps_money(qf_caps),  168750)
  expect_equal(caps_money(fin_caps) + caps_money(sf_caps) + caps_money(qf_caps),
               PUPPY2_POOL)
  # The QF-loser bucket is flat $5 across all 33,750 slots (money-back floor).
  expect_true(all(vapply(qf_caps, function(c) c$usd, numeric(1)) == 5))
})

# ---- money conservation (gate) -----------------------------------------------

test_that("Puppy 2 money conservation: gross = $4.444, net = -$0.556 (single-head)", {
  pcfg <- load_tournament("puppy2")
  base <- tournament_field_multiple(pcfg)                      # 300
  # m = 10 divides 750, so every bucket's sample-to-full rank scaling is an
  # integer width (75) and conservation is exact, not just asymptotic.
  n_field <- base * 10L                                        # 3,000
  n_sims <- 12L
  set.seed(1L)
  ids <- paste0("t", seq_len(n_field))
  mk <- function(mu) matrix(pmax(0, stats::rnorm(n_field * n_sims, mu, 30)),
                            nrow = n_field, dimnames = list(ids, NULL))
  scores <- list(stage_scores = list(`1` = mk(2010), `2` = mk(150),
                                     `3` = mk(150), `4` = mk(150)))
  fp <- build_field_payouts(scores, pcfg, seed = 1L)

  expected_gross <- PUPPY2_POOL / PUPPY2_FIELD                 # $4.4444...
  net <- fp$field_mean_total_ev - pcfg$entry_fee_usd
  message(sprintf(
    "[Puppy2 money cons.] n_field=%d gross=%.4f expected=%.4f net=%.4f gap=%.2e",
    n_field, fp$field_mean_total_ev, expected_gross, net,
    abs(fp$field_mean_total_ev - expected_gross)))

  # Conservation is exact (both loser buckets' caps anchor at their bucket top).
  expect_equal(fp$field_mean_total_ev, expected_gross, tolerance = 1e-9)
  # Net EV = -rake * fee; Puppy 2 rake = 1 - 1,000,000 / (5 * 225,000) = 1/9.
  expect_equal(net, expected_gross - 5, tolerance = 1e-9)
  expect_lt(net, 0)

  # Single-head correctness: no qualifier head -> qualifier EV identically 0
  # and total EV is the championship EV.
  expect_equal(max(abs(fp$per_team_ev$qualifier_round_ev)), 0)
  expect_equal(fp$per_team_ev$total_ev, fp$per_team_ev$championship_round_ev)
})

# ---- compute_team_ev payout lookups for both paid loser buckets ---------------

test_that("Puppy 2 .payout_lookup pays both loser buckets at their global ranks", {
  pcfg <- load_tournament("puppy2")
  tiers <- pcfg$payouts$championship_round$tiers
  sf_elig <- c(bestballBroSim:::.champ_tags(3L, 4L), "qualifier_advancer")
  qf_elig <- c(bestballBroSim:::.champ_tags(2L, 4L), "qualifier_advancer")
  # SF losers (global ranks 751-3750) all get $25.
  expect_equal(bestballBroSim:::.payout_lookup(c(751L, 2000L, 3750L), tiers,
                                               eligibility = sf_elig),
               c(25, 25, 25))
  # QF losers (global ranks 3751-37500) all get $5.
  expect_equal(bestballBroSim:::.payout_lookup(c(3751L, 20000L, 37500L), tiers,
                                               eligibility = qf_elig),
               c(5, 5, 5))
  # Finalists by rank.
  expect_equal(bestballBroSim:::.payout_lookup(c(1L, 100L, 750L), tiers,
                                               eligibility = "finalist"),
               c(100000, 1000, 400))
})

# ---- additivity gate ----------------------------------------------------------

test_that("Puppy 2: per-player EV sums to team EV (additivity), every team", {
  pcfg <- load_tournament("puppy2")
  spec <- .puppy2_lineup_spec()
  set.seed(21L)
  pids <- paste0("p", sprintf("%02d", 1:30))
  positions <- stats::setNames(
    c(rep("QB", 4), rep("RB", 8), rep("WR", 12), rep("TE", 6)), pids)
  tt <- paste0("T", sprintf("%02d", rep(1:6, length.out = 30)))
  sched <- do.call(rbind, lapply(1:17, function(w)
    data.frame(underdog_id = pids, week = w, team = tt,
               opponent = paste0("O", tt), is_bye = FALSE, stringsAsFactors = FALSE)))
  layerA <- do.call(rbind, lapply(1:17, function(w)
    do.call(rbind, lapply(pids, function(p)
      data.frame(underdog_id = p, sim_idx = 1:1200, week = w,
                 draw_value = pmax(0, stats::rnorm(1200, 18, 6)),
                 stringsAsFactors = FALSE)))))
  rosters <- list(e1 = pids[1:18], e2 = pids[2:19], e3 = pids[3:20],
                  e4 = pids[4:21], e5 = pids[5:22], e6 = pids[6:23])
  # Pools tuned to the synthetic scale so teams place sometimes (Puppy 2 stages).
  fake <- list(pools = list(
    q_cum_pool = stats::rnorm(50000, 2010, 130),
    stage_entrant_metric = list(NULL,
      stats::rnorm(5000, 144, 18), stats::rnorm(2000, 144, 18),
      stats::rnorm(800, 144, 18))))
  for (eid in names(rosters)) {
    res <- compute_team_ev(
      pod_rosters = rosters, team_entry_id = eid, positions = positions,
      layerA_draws = layerA, schedule = sched, lineup_spec = spec,
      tournament_cfg = pcfg, field_cache = fake, n_sims = 1200L, seed = 5L)
    pp <- res$per_player_ev
    expect_equal(sum(pp$total_ev), res$team_ev$total_ev, tolerance = 1e-9)
    expect_equal(sum(pp$qualifier_round_ev), res$team_ev$qualifier_round_ev,
                 tolerance = 1e-9)
    expect_equal(sum(pp$championship_round_ev), res$team_ev$championship_round_ev,
                 tolerance = 1e-9)
    # Single-head: every team's qualifier-round EV is exactly $0.
    expect_equal(res$team_ev$qualifier_round_ev, 0)
    expect_equal(res$team_ev$total_ev, res$team_ev$championship_round_ev,
                 tolerance = 1e-12)
  }
})
