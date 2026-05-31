# Tests for R/tournament_ev_bbm7.R -- the BBM7 EV machinery (heads 1-4).
# Per-player attribution (head 5) is deferred to a follow-up commit on
# the same branch.
#
# The expensive end-to-end tests (real Layer A blend + 1200-team field
# sample + 500 sims) are gated on the local BBMDB parquet being present
# so the CI suite stays fast; the cheap unit tests cover the payout-tier
# range-averaging math and the determinism contract on a synthetic
# fixture.

# ---- Unit tests for the payout helpers --------------------------------------

test_that(".tier_caps_from_yaml padded to total_slots with $0 catch-all", {
  tiers <- list(
    list(rank_from = 1, rank_to = 5,  usd = 100),
    list(rank_from = 6, rank_to = 10, usd = 50)
  )
  caps <- bestballBroSim:::.tier_caps_from_yaml(tiers, eligibility = NULL,
                                                total_slots = 100L)
  # 5 @ $100, 5 @ $50, 90 @ $0 catch-all.
  expect_equal(length(caps), 3L)
  expect_equal(caps[[1L]]$n_slots, 5L); expect_equal(caps[[1L]]$usd, 100)
  expect_equal(caps[[2L]]$n_slots, 5L); expect_equal(caps[[2L]]$usd, 50)
  expect_equal(caps[[3L]]$n_slots, 90L); expect_equal(caps[[3L]]$usd, 0)
})

test_that(".range_avg_payout integrates tier $$ over the representative range", {
  # 100-slot bucket: 1@$2M, 1@$1M, 8@$0, 90@$0
  caps <- list(
    list(n_slots = 1L,  usd = 2e6),
    list(n_slots = 1L,  usd = 1e6),
    list(n_slots = 98L, usd = 0)
  )
  # Sample of 10 teams covering 100 slots: each team represents 10 ranks.
  # Sample team 1 (rank 1) -> full ranks 1..10. Sum = 2M + 1M + 8*0 = 3M. Avg = 300K.
  expect_equal(bestballBroSim:::.range_avg_payout(1L, 10L, 100L, caps),
               (2e6 + 1e6) / 10)
  # Sample team 2 (rank 2) -> full ranks 11..20. All $0.
  expect_equal(bestballBroSim:::.range_avg_payout(2L, 10L, 100L, caps), 0)
})

test_that(".range_avg_payout x range_width sums to bucket total ($1450)", {
  caps <- list(
    list(n_slots = 5L,   usd = 100),
    list(n_slots = 95L,  usd = 10)
  )
  # 10 sample teams cover 100 slots; each represents range_width = 10
  # ranks. Per-team avg payout x range_width recovers the sum of $$$ in
  # that range, and the across-team sum recovers the full bucket pool.
  per_team <- vapply(seq_len(10L),
                     function(k) bestballBroSim:::.range_avg_payout(
                       k, 10L, 100L, caps), numeric(1))
  range_width <- 100L / 10L
  expect_equal(sum(per_team) * range_width, 5 * 100 + 95 * 10,
               tolerance = 1e-9)
  # And the per-team mean equals total_pool / total_slots -- this is the
  # money-conservation invariant at the unit level.
  expect_equal(mean(per_team), (5 * 100 + 95 * 10) / 100, tolerance = 1e-9)
})

test_that("BBM7 qualifier_round caps sum to the full $1.5M qualifier prize", {
  tcfg <- load_tournament("bbm7")
  caps <- bestballBroSim:::.tier_caps_from_yaml(
    tcfg$payouts$qualifier_round$tiers,
    eligibility = NULL, total_slots = tcfg$total_field_size)
  total <- sum(vapply(caps, function(c) c$n_slots * c$usd, numeric(1)))
  # Sum of the tier amounts published in the YAML.
  expect_equal(total, 1500000, tolerance = 1)
})

test_that("BBM7 championship bucket caps sum to the full ~$13.5M champ prize", {
  tcfg <- load_tournament("bbm7")
  # finalists
  fc <- bestballBroSim:::.tier_caps_from_yaml(
    tcfg$payouts$championship_round$tiers, eligibility = "finalist",
    total_slots = 667L)
  # primary_slots is now derived from the primary tiers' own rank widths.
  sl <- bestballBroSim:::.tier_caps_combined(
    tcfg$payouts$championship_round$tiers,
    primary_eligibility = "semifinals_loser",
    fallback_eligibility = "qualifier_advancer",
    total_slots = 7337L)
  ql <- bestballBroSim:::.tier_caps_combined(
    tcfg$payouts$championship_round$tiers,
    primary_eligibility = c("quarterfinals_loser",
                            "quarterfinals_loser_lower"),
    fallback_eligibility = "qualifier_advancer",
    total_slots = 104052L)
  total <- sum(vapply(fc, function(c) c$n_slots * c$usd, numeric(1))) +
    sum(vapply(sl, function(c) c$n_slots * c$usd, numeric(1))) +
    sum(vapply(ql, function(c) c$n_slots * c$usd, numeric(1)))
  expect_gt(total, 13.4e6)
  expect_lt(total, 13.6e6)
})

# ---- Determinism (synthetic, fast) -----------------------------------------

.season_spec_test <- function() {
  list(slate_id = "test-season", slots = list(
    list(pos = "QB",   n = 1L, eligible = c("QB")),
    list(pos = "RB",   n = 2L, eligible = c("RB")),
    list(pos = "WR",   n = 3L, eligible = c("WR")),
    list(pos = "TE",   n = 1L, eligible = c("TE")),
    list(pos = "FLEX", n = 1L, eligible = c("RB", "WR", "TE"))
  ))
}

test_that("simulate_per_stage_scores is deterministic under a fixed seed", {
  # Tiny synthetic pod just to verify the wiring/contract.
  set.seed(11L)
  pids <- paste0("p", sprintf("%02d", 1:30))
  positions <- setNames(
    c(rep("QB", 4), rep("RB", 8), rep("WR", 12), rep("TE", 6)), pids)
  team_teams <- paste0("T", sprintf("%02d", rep(1:6, length.out = 30)))
  sched <- do.call(rbind, lapply(seq_len(17L), function(w) {
    data.frame(underdog_id = pids, week = w,
               team = team_teams,
               opponent = paste0("OPP_", team_teams),
               is_bye = FALSE, stringsAsFactors = FALSE)
  }))
  layerA <- do.call(rbind, lapply(seq_len(17L), function(w) {
    do.call(rbind, lapply(pids, function(p) {
      data.frame(underdog_id = p, sim_idx = seq_len(2000L),
                 week = w,
                 draw_value = pmax(0, stats::rnorm(2000L, 10, 5)),
                 stringsAsFactors = FALSE)
    }))
  }))
  rosters <- list(
    e1 = pids[1:18], e2 = pids[2:19], e3 = pids[3:20],
    e4 = pids[4:21], e5 = pids[5:22], e6 = pids[6:23]
  )
  o1 <- simulate_per_stage_scores(rosters, positions, layerA, sched,
                                  .season_spec_test(), n_sims = 200L, seed = 7L)
  o2 <- simulate_per_stage_scores(rosters, positions, layerA, sched,
                                  .season_spec_test(), n_sims = 200L, seed = 7L)
  expect_identical(o1$q_cum, o2$q_cum)
  expect_identical(o1$w15,   o2$w15)
})

# ---- End-to-end run on real Season slate (gated, expensive) ----------------

test_that("real-set: BBM7 money-conservation gap < 15% of expected", {
  bbmdb_path <- "C:/Users/vince/Desktop/bbmdb_scraper/data/bbmdb_teams.parquet"
  if (!file.exists(bbmdb_path)) {
    testthat::skip("BBMDB parquet missing -- gated test.")
  }
  scraper_path <- testthat::test_path("..", "..", "inst", "data",
                                      "scraped_drafts",
                                      "udbb-scraper-latest.json")
  if (!file.exists(scraper_path)) {
    testthat::skip("Scraper export missing -- gated test.")
  }

  picks <- load_scraped_drafts(scraper_path)
  pool  <- load_slate_data("nfl_2026_season")
  targets <- compute_field_targets(picks, slate_id = "nfl_2026_season")

  tcfg <- load_tournament("bbm7")
  # Money conservation needs a clean multiple of the per-finalist field share
  # (1008 for BBM7); 2016 keeps this gated run tractable. The headline 10,080
  # run lives in inst/scripts/smoke_bbm7_ev.R.
  n_field <- resolve_bbm7_field_size(tcfg, 2016L)
  field <- generate_field("nfl_2026_season", picks = picks, player_pool = pool,
                          targets = targets, n_teams = n_field, seed = 1L)
  field_rosters <- split(field$rosters$underdog_id, field$rosters$entry_id)

  sources_path <- testthat::test_path("..", "..", "inst", "data",
                                      "sources", "_manifest.yaml")
  slates_path  <- testthat::test_path("..", "..", "inst", "data",
                                      "slates", "_manifest.yaml")
  feed <- blend_slate(slate_id = "nfl_2026_season",
                      sources_manifest_path = sources_path,
                      slates_manifest_path  = slates_path,
                      write_json = FALSE)
  sim <- simulate_slate(feed, n_sims = 1000L, seed = 1L)
  positions <- vapply(names(feed$players),
                      function(uid) feed$players[[uid]]$position %||% NA_character_,
                      character(1))
  names(positions) <- names(feed$players)
  positions <- positions[!is.na(positions)]
  sched_chunks <- list()
  for (uid in names(feed$players)) {
    pl <- feed$players[[uid]]
    for (wk in pl$weekly %||% list()) {
      w <- as.integer(wk$week %||% NA_integer_); if (is.na(w)) next
      sched_chunks[[length(sched_chunks)+1L]] <- data.frame(
        underdog_id = uid, week = w, team = pl$team %||% NA_character_,
        opponent = if (isTRUE(wk$is_bye)) NA_character_ else (wk$opponent %||% NA_character_),
        is_bye = isTRUE(wk$is_bye), stringsAsFactors = FALSE)
    }
  }
  schedule <- do.call(rbind, sched_chunks)
  lineup_spec <- load_slate_lineup_spec("nfl_2026_season")

  scores <- simulate_per_stage_scores(
    rosters = field_rosters, positions = positions,
    layerA_draws = sim$draws, schedule = schedule,
    lineup_spec = lineup_spec, n_sims = 500L, seed = 1L)
  fp <- build_bbm7_field_payouts(scores, tcfg, seed = 1L)

  expected <- 15000010 / tcfg$total_field_size
  gap_rel  <- abs(fp$field_mean_total_ev - expected) / expected
  message(sprintf(
    "[3b-7 money cons.] n_field=%d field-mean=%.2f  expected=%.2f  gap_rel=%.3f",
    n_field, fp$field_mean_total_ev, expected, gap_rel))
  expect_lt(gap_rel, 0.08)
})

test_that("real-set: qualifier advance prob from compute_team_bbm7_ev matches 3b-5 xAdv", {
  bbmdb_path <- "C:/Users/vince/Desktop/bbmdb_scraper/data/bbmdb_teams.parquet"
  if (!file.exists(bbmdb_path)) {
    testthat::skip("BBMDB parquet missing -- gated test.")
  }
  scraper_path <- testthat::test_path("..", "..", "inst", "data",
                                      "scraped_drafts",
                                      "udbb-scraper-latest.json")
  if (!file.exists(scraper_path)) {
    testthat::skip("Scraper export missing -- gated test.")
  }

  # Run 3b-5 once to get the reference qualifier advance prob for a BBM7
  # validation team.
  picks <- load_scraped_drafts(scraper_path)
  val <- validate_xadv_against_bbmdb(
    bbmdb_path = bbmdb_path, scraper_path = scraper_path,
    layerA_n_sims = 1500L, n_sims = 1500L, base_seed = 1L, verbose = FALSE)
  bbm7_rows <- val$per_team[val$per_team$tournament_id == "bbm7" &
                            !is.na(val$per_team$predicted_xadv), ]
  testthat::skip_if(nrow(bbm7_rows) < 1L, "No BBM7 validation team available.")
  eid_pick <- bbm7_rows$entry_id[1L]
  draft_pick <- bbm7_rows$draft_id[1L]
  ref_xadv <- bbm7_rows$predicted_xadv[1L]

  # Build inputs for compute_team_bbm7_ev for that team.
  feed <- blend_slate(
    slate_id = "nfl_2026_season",
    sources_manifest_path = testthat::test_path("..", "..", "inst", "data",
                                                "sources", "_manifest.yaml"),
    slates_manifest_path  = testthat::test_path("..", "..", "inst", "data",
                                                "slates", "_manifest.yaml"),
    write_json = FALSE)
  sim <- simulate_slate(feed, n_sims = 1500L, seed = 1L)
  bridge <- bestballBroSim:::.scraper_to_feed_id_map(picks, feed)
  pod_rows <- picks[picks$draft_id == draft_pick, ]
  pod_rows$feed_uid <- bridge[pod_rows$underdog_id]
  pod_rows <- pod_rows[!is.na(pod_rows$feed_uid), ]
  pod_rosters <- split(pod_rows$feed_uid, pod_rows$draft_entry_id)
  pod_rosters <- lapply(pod_rosters, unique)

  positions <- vapply(names(feed$players),
                      function(uid) feed$players[[uid]]$position %||% NA_character_,
                      character(1))
  names(positions) <- names(feed$players)
  positions <- positions[!is.na(positions)]
  sched_chunks <- list()
  for (uid in names(feed$players)) {
    pl <- feed$players[[uid]]
    for (wk in pl$weekly %||% list()) {
      w <- as.integer(wk$week %||% NA_integer_); if (is.na(w)) next
      sched_chunks[[length(sched_chunks)+1L]] <- data.frame(
        underdog_id = uid, week = w, team = pl$team %||% NA_character_,
        opponent = if (isTRUE(wk$is_bye)) NA_character_ else (wk$opponent %||% NA_character_),
        is_bye = isTRUE(wk$is_bye), stringsAsFactors = FALSE)
    }
  }
  schedule <- do.call(rbind, sched_chunks)
  lineup_spec <- load_slate_lineup_spec("nfl_2026_season")
  tcfg <- load_tournament("bbm7")

  # Field cache is just a placeholder for the qualifier-consistency
  # check; we don't need championship realism here.
  fake_cache <- list(pools = list(
    q_cum_pool = stats::rnorm(50000L, 2400, 150),
    qualifier_advancer_w15 = stats::rnorm(5000L, 150, 25),
    qf_advancer_w16        = stats::rnorm(1000L, 150, 25),
    sf_advancer_w17        = stats::rnorm(500L,  150, 25)
  ))
  res <- compute_team_bbm7_ev(
    pod_rosters = pod_rosters, team_entry_id = eid_pick,
    positions = positions, layerA_draws = sim$draws,
    schedule = schedule, lineup_spec = lineup_spec,
    tournament_cfg = tcfg, field_cache = fake_cache,
    n_sims = 1500L, seed = 1L)
  message(sprintf("[3b-7 qualifier consistency] our prob=%.3f  3b-5 ref=%.3f  delta=%.3f",
                  res$advance_probs$qualifier, ref_xadv,
                  res$advance_probs$qualifier - ref_xadv))
  expect_lt(abs(res$advance_probs$qualifier - ref_xadv), 0.06)
})

# ---- Field-size guardrail (Prereq B) ----------------------------------------

test_that("bbm7_field_multiple is total_field_size / final-stage seats", {
  tcfg <- load_tournament("bbm7")
  expect_equal(bbm7_field_multiple(tcfg), 1008L)   # 672336 / 667
})

test_that("resolve_bbm7_field_size snaps to the nearest valid multiple", {
  tcfg <- load_tournament("bbm7")
  expect_equal(resolve_bbm7_field_size(tcfg, 10000L), 10080L)  # 10 * 1008
  expect_equal(resolve_bbm7_field_size(tcfg, 10080L), 10080L)  # already valid
  expect_equal(resolve_bbm7_field_size(tcfg, 1L),     1008L)   # floors at base
  # snap = FALSE errors and names nearby valid sizes.
  expect_error(resolve_bbm7_field_size(tcfg, 1200L, snap = FALSE),
               "not a multiple of 1008")
})

# ---- Overflow fix (Prereq A) + money conservation at n_field = 10,080 -------

test_that("build_bbm7_field_payouts runs at n_field=10,080 without overflow", {
  # n_full * rank overflows 32-bit ints above rank ~3,194; at 10,080 the
  # top buckets exercise ranks far beyond that. With the as.numeric() fix
  # the field-mean still converges to prize_pool / total_field_size (the
  # money-conservation invariant) regardless of score realism, because the
  # range-averaging integrates each bucket's full prize pool.
  tcfg <- load_tournament("bbm7")
  n_field <- 10080L; n_sims <- 12L
  set.seed(1L)
  ids <- paste0("t", seq_len(n_field))
  mk <- function(mean) matrix(pmax(0, stats::rnorm(n_field * n_sims, mean, 30)),
                              nrow = n_field, dimnames = list(ids, NULL))
  scores <- list(q_cum = mk(2010), w15 = mk(150), w16 = mk(150), w17 = mk(150))
  fp <- build_bbm7_field_payouts(scores, tcfg, seed = 1L)
  expect_true(is.finite(fp$field_mean_total_ev))
  expected <- 15000010 / tcfg$total_field_size       # ~ $22.31 gross
  gap_rel <- abs(fp$field_mean_total_ev - expected) / expected
  expect_lt(gap_rel, 0.03)
})

test_that("build_bbm7_field_payouts errors on a non-clean-multiple field", {
  tcfg <- load_tournament("bbm7")
  n_field <- 1000L; n_sims <- 4L
  ids <- paste0("t", seq_len(n_field))
  mk <- function() matrix(stats::runif(n_field * n_sims), nrow = n_field,
                          dimnames = list(ids, NULL))
  scores <- list(q_cum = mk(), w15 = mk(), w16 = mk(), w17 = mk())
  expect_error(build_bbm7_field_payouts(scores, tcfg, seed = 1L),
               "not a multiple of 1008")
})

# ---- Head 5: per-player attribution -----------------------------------------

# Synthetic pod fixture: 6 entries over 30 players, real BBM7 stage shape.
.synthetic_bbm7_pod <- function(n_sims = 1500L, player_mean = 18, player_sd = 6,
                                seed = 11L) {
  set.seed(seed)
  pids <- paste0("p", sprintf("%02d", 1:30))
  positions <- stats::setNames(
    c(rep("QB", 4), rep("RB", 8), rep("WR", 12), rep("TE", 6)), pids)
  team_teams <- paste0("T", sprintf("%02d", rep(1:6, length.out = 30)))
  sched <- do.call(rbind, lapply(seq_len(17L), function(w) {
    data.frame(underdog_id = pids, week = w, team = team_teams,
               opponent = paste0("OPP_", team_teams),
               is_bye = FALSE, stringsAsFactors = FALSE)
  }))
  layerA <- do.call(rbind, lapply(seq_len(17L), function(w) {
    do.call(rbind, lapply(pids, function(p) {
      data.frame(underdog_id = p, sim_idx = seq_len(n_sims), week = w,
                 draw_value = pmax(0, stats::rnorm(n_sims, player_mean, player_sd)),
                 stringsAsFactors = FALSE)
    }))
  }))
  rosters <- list(e1 = pids[1:18], e2 = pids[2:19], e3 = pids[3:20],
                  e4 = pids[4:21], e5 = pids[5:22], e6 = pids[6:23])
  spec <- .season_spec_test()
  # Pools tuned to the synthetic score scale so teams place sometimes.
  fake_cache <- list(pools = list(
    q_cum_pool             = stats::rnorm(50000L, 2010, 130),
    qualifier_advancer_w15 = stats::rnorm(5000L, 144, 18),
    qf_advancer_w16        = stats::rnorm(2000L, 144, 18),
    sf_advancer_w17        = stats::rnorm(800L,  144, 18)))
  list(rosters = rosters, positions = positions, layerA = layerA,
       sched = sched, spec = spec, cache = fake_cache,
       tcfg = load_tournament("bbm7"))
}

test_that(".attribute_player_ev splits each payout additively across stages", {
  # Hand-built: 3 players, 2 sims. Stage-indexed contributions.
  pl <- c("a", "b", "c")
  mk <- function(m) matrix(m, nrow = 3, dimnames = list(pl, NULL))
  contrib <- list(stage = list(
    `1` = mk(c(30, 10, 0,   0, 0, 0)),   # sim1 a:30 b:10 c:0 ; sim2 all 0
    `2` = mk(c(0, 0, 0,     8, 2, 0)),   # sim2 (stage 2): a:8 b:2
    `3` = mk(c(0, 0, 0,     0, 0, 0)),
    `4` = mk(c(0, 0, 0,     0, 0, 0))
  ))
  q_pay <- c(100, 0)
  c_pay <- c(0, 50)
  depth <- c(1L, 2L)  # sim2: lost at stage 2 -> champ split over stage-2 contributions
  pp <- bestballBroSim:::.attribute_player_ev(contrib, q_pay, c_pay, depth)
  # qualifier: sim1 $100 split 30:10 -> a $75, b $25, mean over 2 sims.
  ev <- stats::setNames(pp$total_ev, pp$underdog_id)
  expect_equal(ev[["a"]], (75 + 0.8 * 50) / 2)   # q: 75/2 ; champ sim2: 8/10*50
  expect_equal(ev[["b"]], (25 + 0.2 * 50) / 2)
  expect_equal(ev[["c"]], 0)
  # Additive: sums to the team EV.
  expect_equal(sum(pp$qualifier_round_ev), mean(q_pay), tolerance = 1e-12)
  expect_equal(sum(pp$championship_round_ev), mean(c_pay), tolerance = 1e-12)
  expect_equal(sum(pp$total_ev), mean(q_pay) + mean(c_pay), tolerance = 1e-12)
})

test_that("per-player EV sums to team EV for every team (additivity gate)", {
  fx <- .synthetic_bbm7_pod(n_sims = 1500L)
  for (eid in names(fx$rosters)) {
    res <- compute_team_bbm7_ev(
      pod_rosters = fx$rosters, team_entry_id = eid, positions = fx$positions,
      layerA_draws = fx$layerA, schedule = fx$sched, lineup_spec = fx$spec,
      tournament_cfg = fx$tcfg, field_cache = fx$cache, n_sims = 1500L, seed = 7L)
    pp <- res$per_player_ev
    expect_equal(sum(pp$qualifier_round_ev),
                 res$team_ev$qualifier_round_ev, tolerance = 1e-9)
    expect_equal(sum(pp$championship_round_ev),
                 res$team_ev$championship_round_ev, tolerance = 1e-9)
    expect_equal(sum(pp$total_ev), res$team_ev$total_ev, tolerance = 1e-9)
  }
})

test_that("attribution is non-trivial and labelled as attribution, not marginal", {
  fx <- .synthetic_bbm7_pod(n_sims = 1500L)
  res <- compute_team_bbm7_ev(
    pod_rosters = fx$rosters, team_entry_id = "e1", positions = fx$positions,
    layerA_draws = fx$layerA, schedule = fx$sched, lineup_spec = fx$spec,
    tournament_cfg = fx$tcfg, field_cache = fx$cache, n_sims = 1500L, seed = 7L)
  expect_gt(res$team_ev$total_ev, 0)                 # team wins money in fixture
  expect_gt(sum(res$per_player_ev$total_ev > 0), 1L) # value spread across roster
  expect_match(attr(res$per_player_ev, "metric"), "attribution")
  expect_match(attr(res$per_player_ev, "metric"), "NOT marginal")
})

test_that("sanity: a high-scoring always-starter carries EV; a dead player ~ 0", {
  fx <- .synthetic_bbm7_pod(n_sims = 1500L)
  # Force p01 (a QB on e1) to dominate every week and p18 (TE on e1) to score
  # nothing -> p18 never starts, p01 always does.
  star <- "p01"; dead <- "p18"
  fx$layerA$draw_value[fx$layerA$underdog_id == star] <- 60
  fx$layerA$draw_value[fx$layerA$underdog_id == dead] <- 0
  res <- compute_team_bbm7_ev(
    pod_rosters = fx$rosters, team_entry_id = "e1", positions = fx$positions,
    layerA_draws = fx$layerA, schedule = fx$sched, lineup_spec = fx$spec,
    tournament_cfg = fx$tcfg, field_cache = fx$cache, n_sims = 1500L, seed = 7L)
  ev <- stats::setNames(res$per_player_ev$total_ev, res$per_player_ev$underdog_id)
  expect_equal(unname(ev[dead]), 0)                  # never started -> exactly 0
  expect_gt(ev[star], stats::median(res$per_player_ev$total_ev))
})

test_that("per-player attribution is deterministic under a fixed seed", {
  fx <- .synthetic_bbm7_pod(n_sims = 800L)
  args <- list(pod_rosters = fx$rosters, team_entry_id = "e2",
               positions = fx$positions, layerA_draws = fx$layerA,
               schedule = fx$sched, lineup_spec = fx$spec,
               tournament_cfg = fx$tcfg, field_cache = fx$cache,
               n_sims = 800L, seed = 3L)
  r1 <- do.call(compute_team_bbm7_ev, args)
  r2 <- do.call(compute_team_bbm7_ev, args)
  expect_equal(r1$per_player_ev, r2$per_player_ev)
})
