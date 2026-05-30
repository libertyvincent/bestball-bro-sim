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
  sl <- bestballBroSim:::.tier_caps_combined(
    tcfg$payouts$championship_round$tiers,
    primary_eligibility = "semifinals_loser",
    fallback_eligibility = "qualifier_advancer",
    primary_slots = 667L, total_slots = 7337L)
  ql <- bestballBroSim:::.tier_caps_combined(
    tcfg$payouts$championship_round$tiers,
    primary_eligibility = c("quarterfinals_loser",
                            "quarterfinals_loser_lower"),
    fallback_eligibility = "qualifier_advancer",
    primary_slots = 667L + 6003L, total_slots = 104052L)
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

  field <- generate_field("nfl_2026_season", picks = picks, player_pool = pool,
                          targets = targets, n_teams = 1200L, seed = 1L)
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
  tcfg <- load_tournament("bbm7")

  scores <- simulate_per_stage_scores(
    rosters = field_rosters, positions = positions,
    layerA_draws = sim$draws, schedule = schedule,
    lineup_spec = lineup_spec, n_sims = 500L, seed = 1L)
  fp <- build_bbm7_field_payouts(scores, tcfg, seed = 1L)

  expected <- 15000010 / tcfg$total_field_size
  gap_rel  <- abs(fp$field_mean_total_ev - expected) / expected
  message(sprintf(
    "[3b-7 money cons.] field-mean=%.2f  expected=%.2f  gap_rel=%.3f",
    fp$field_mean_total_ev, expected, gap_rel))
  expect_lt(gap_rel, 0.15)
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
