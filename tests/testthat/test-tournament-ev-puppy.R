# Tests for the config-driven generalization (R/tournament_ev.R) on a
# SECOND tournament: The Puppy. BBM7 no-drift is covered by test-bbm7-ev.R
# (now running through the same generic engine via thin wrappers).
#
# Puppy is structurally a 4-stage Season tournament (2/12 -> 1/10 -> 1/6 ->
# 625-final, total field 225,000) -- the same shape as BBM7 but different
# pod sizes, seats, and finalist denominator (225000/625 = 360). Its config
# ships a placeholder $0 payout block (the real tier table lives on
# Underdog's rules page, which isn't machine-readable), so the money-
# conservation and additivity checks inject a dual-head payout summing to
# the rake-implied prize pool: the money-conservation invariant
# (field-mean == total_payout / total_field) depends only on the prize-pool
# total and the head structure, not the intra-bucket tier shape.

.puppy_lineup_spec <- function() {
  list(slate_id = "puppy-test", slots = list(
    list(pos = "QB",   n = 1L, eligible = c("QB")),
    list(pos = "RB",   n = 2L, eligible = c("RB")),
    list(pos = "WR",   n = 3L, eligible = c("WR")),
    list(pos = "TE",   n = 1L, eligible = c("TE")),
    list(pos = "FLEX", n = 1L, eligible = c("RB", "WR", "TE"))
  ))
}

# Puppy config with a dual-head payout summing exactly to the rake-implied
# prize pool. Flat within-bucket amounts -- shape is irrelevant to money
# conservation; only the total and the (qualifier + championship) heads are.
.puppy_cfg_with_payouts <- function(rake = 0.108) {
  cfg <- load_tournament("puppy")
  entry <- cfg$entry_fee_usd
  pool  <- entry * cfg$total_field_size * (1 - rake)   # 5,017,500 at rake 0.108
  seats <- vapply(cfg$stages, function(s) as.integer(s$seats_entering), integer(1))
  n_fin <- seats[4]                                    # 625
  n_losers <- seats[2] - n_fin                         # 36,875 SF+QF losers
  qual_n <- 1000L; qual_usd <- 100; fin_usd <- 1000
  # catch-all absorbs the residual so the grand total is exactly `pool`.
  catch_usd <- (pool - qual_n * qual_usd - n_fin * fin_usd) / n_losers
  cfg$payouts <- list(
    qualifier_round = list(tiers = list(
      list(rank_from = 1L, rank_to = qual_n, usd = qual_usd))),
    championship_round = list(tiers = list(
      list(rank_from = 1L,        rank_to = n_fin,     usd = fin_usd,
           eligibility = "finalist"),
      list(rank_from = n_fin + 1L, rank_to = seats[2], usd = catch_usd,
           eligibility = "qualifier_advancer")))
  )
  attr(cfg, "prize_pool") <- pool
  cfg
}

# ---- field-multiple guardrail (config-derived, not hardcoded) ---------------

test_that("Puppy field multiple is total_field_size / final seats (= 360)", {
  pcfg <- load_tournament("puppy")
  expect_equal(tournament_field_multiple(pcfg), 360L)        # 225000 / 625
  expect_equal(resolve_tournament_field_size(pcfg, 10000L), 10080L)  # 28 * 360
  expect_error(resolve_tournament_field_size(pcfg, 1000L, snap = FALSE),
               "not a multiple of 360")
})

# ---- Puppy money conservation -----------------------------------------------

test_that("Puppy money conservation: field-mean gross = entry*(1-rake), net = -entry*rake", {
  pcfg <- .puppy_cfg_with_payouts(rake = 0.108)
  base <- tournament_field_multiple(pcfg)                    # 360
  n_field <- base * 30L                                      # 10,800 (clean)
  n_sims <- 12L
  set.seed(1L)
  ids <- paste0("t", seq_len(n_field))
  mk <- function(mean) matrix(pmax(0, stats::rnorm(n_field * n_sims, mean, 30)),
                              nrow = n_field, dimnames = list(ids, NULL))
  scores <- list(stage_scores = list(`1` = mk(2010), `2` = mk(150),
                                     `3` = mk(150), `4` = mk(150)))
  fp <- build_field_payouts(scores, pcfg, seed = 1L)
  entry <- pcfg$entry_fee_usd
  expected_gross <- attr(pcfg, "prize_pool") / pcfg$total_field_size  # ~22.30
  gap <- abs(fp$field_mean_total_ev - expected_gross) / expected_gross
  net <- fp$field_mean_total_ev - entry
  message(sprintf("[Puppy money cons.] n_field=%d gross=%.2f expected=%.2f net=%.2f (-entry*rake=%.2f) gap=%.4f",
                  n_field, fp$field_mean_total_ev, expected_gross, net, -entry * 0.108, gap))
  expect_lt(gap, 0.01)
  expect_equal(expected_gross, entry * (1 - 0.108), tolerance = 1e-6)  # 22.30
  expect_equal(net, -entry * 0.108, tolerance = 0.25)                  # ~ -2.70
})

# ---- Puppy additivity gate --------------------------------------------------

test_that("Puppy: per-player EV sums to team EV (additivity), every team", {
  pcfg <- .puppy_cfg_with_payouts()
  spec <- .puppy_lineup_spec()
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
  # Pools tuned to the synthetic scale so teams place sometimes (Puppy stages).
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
  }
})

test_that("compute_team_ev runs on the real puppy.yaml (placeholder payouts -> $0)", {
  pcfg <- load_tournament("puppy")
  spec <- .puppy_lineup_spec()
  set.seed(7L)
  pids <- paste0("p", sprintf("%02d", 1:24))
  positions <- stats::setNames(
    c(rep("QB", 3), rep("RB", 7), rep("WR", 9), rep("TE", 5)), pids)
  tt <- paste0("T", sprintf("%02d", rep(1:6, length.out = 24)))
  sched <- do.call(rbind, lapply(1:17, function(w)
    data.frame(underdog_id = pids, week = w, team = tt,
               opponent = paste0("O", tt), is_bye = FALSE, stringsAsFactors = FALSE)))
  layerA <- do.call(rbind, lapply(1:17, function(w)
    do.call(rbind, lapply(pids, function(p)
      data.frame(underdog_id = p, sim_idx = 1:400, week = w,
                 draw_value = pmax(0, stats::rnorm(400, 16, 6)),
                 stringsAsFactors = FALSE)))))
  rosters <- list(e1 = pids[1:18], e2 = pids[2:19], e3 = pids[3:20])
  fake <- list(pools = list(q_cum_pool = stats::rnorm(20000, 2010, 130),
    stage_entrant_metric = list(NULL, stats::rnorm(2000, 144, 18),
      stats::rnorm(800, 144, 18), stats::rnorm(400, 144, 18))))
  res <- compute_team_ev(rosters, "e1", positions, layerA, sched, spec,
                         pcfg, fake, n_sims = 400L, seed = 1L)
  # Advance probs are valid (payout-independent); EV is $0 under placeholder.
  expect_true(res$advance_probs$qualifier >= 0 && res$advance_probs$qualifier <= 1)
  expect_equal(res$team_ev$total_ev, 0)
  expect_equal(sum(res$per_player_ev$total_ev), 0)            # additivity holds trivially
})

# ---- Puppy qualifier consistency vs BBMDB (gated, expensive) ----------------

test_that("real-set: Puppy qualifier advance prob matches BBMDB (~13 teams) + engine", {
  bbmdb_path <- bbmdb_corpus_path()
  if (!file.exists(bbmdb_path)) testthat::skip("BBMDB parquet missing -- gated test.")
  scraper_path <- testthat::test_path("..", "..", "inst", "data",
                                      "scraped_drafts", "udbb-scraper-latest.json")
  if (!file.exists(scraper_path)) testthat::skip("Scraper export missing -- gated test.")

  picks <- load_scraped_drafts(scraper_path)
  val <- validate_xadv_against_bbmdb(
    bbmdb_path = bbmdb_path, scraper_path = scraper_path,
    layerA_n_sims = 1500L, n_sims = 1500L, base_seed = 1L, verbose = FALSE)
  puppy_rows <- val$per_team[val$per_team$tournament_id == "puppy" &
                             !is.na(val$per_team$predicted_xadv) &
                             !is.na(val$per_team$bbmdb_xadv), ]
  testthat::skip_if(nrow(puppy_rows) < 1L, "No Puppy validation team available.")

  # (a) Reported diagnostic -- 3b-5 predicted vs BBMDB across the Puppy teams.
  #     This is a property of predict_pod_xadv (3b-5), which this refactor does
  #     NOT touch, so it is surfaced (not hard-gated): a divergence here is a
  #     pre-existing 3b-5<->BBMDB calibration question, not a generalization
  #     regression. (Observed: our predicted xAdv runs systematically above
  #     BBMDB's for Puppy -- mean ~0.23 vs ~0.12 across the 13 teams -- worth a
  #     separate calibration look; flagged in the PR.)
  mae <- mean(abs(puppy_rows$predicted_xadv - puppy_rows$bbmdb_xadv))
  spear <- suppressWarnings(stats::cor(puppy_rows$predicted_xadv,
                                       puppy_rows$bbmdb_xadv, method = "spearman"))
  message(sprintf("[Puppy qualifier] n=%d  MAE(predicted, BBMDB)=%.3f  spearman=%.2f  mean_pred=%.3f mean_bbmdb=%.3f",
                  nrow(puppy_rows), mae, spear, mean(puppy_rows$predicted_xadv),
                  mean(puppy_rows$bbmdb_xadv)))

  # (b) The GATE: the generalized EV engine reproduces the 3b-5 advance prob
  #     for one Puppy team (engine-vs-3b5 consistency), run with puppy.yaml.
  eid_pick   <- puppy_rows$entry_id[1L]
  draft_pick <- puppy_rows$draft_id[1L]
  ref_xadv   <- puppy_rows$predicted_xadv[1L]

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
  pod_rosters <- lapply(split(pod_rows$feed_uid, pod_rows$draft_entry_id), unique)

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
  pcfg <- load_tournament("puppy")
  fake_cache <- list(pools = list(
    q_cum_pool = stats::rnorm(50000L, 2400, 150),
    stage_entrant_metric = list(NULL, stats::rnorm(5000L, 150, 25),
      stats::rnorm(1000L, 150, 25), stats::rnorm(500L, 150, 25))))
  res <- compute_team_ev(
    pod_rosters = pod_rosters, team_entry_id = eid_pick,
    positions = positions, layerA_draws = sim$draws, schedule = schedule,
    lineup_spec = lineup_spec, tournament_cfg = pcfg, field_cache = fake_cache,
    n_sims = 1500L, seed = 1L)
  message(sprintf("[Puppy engine-vs-3b5] engine=%.3f  3b-5 ref=%.3f  delta=%.3f",
                  res$advance_probs$qualifier, ref_xadv,
                  res$advance_probs$qualifier - ref_xadv))
  expect_lt(abs(res$advance_probs$qualifier - ref_xadv), 0.06)
})
