# Tests for the Dachshund through the config-driven EV engine
# (R/tournament_ev.R). First Season-slate tournament with a REAL payout
# table in its YAML (BBM7 aside): 28,080 entries, $8 fee, single-head
# $200,000 championship ladder -- no qualifier-round money.
#
# Unlike the Puppy tests (which inject a synthetic payout block because
# puppy.yaml still ships the $0 placeholder), everything here runs off
# dachshund.yaml as committed, so these tests also guard the YAML itself:
# the tier sum, the single-head structure, and exact money conservation.

.dachshund_lineup_spec <- function() {
  list(slate_id = "dachshund-test", slots = list(
    list(pos = "QB",   n = 1L, eligible = c("QB")),
    list(pos = "RB",   n = 2L, eligible = c("RB")),
    list(pos = "WR",   n = 3L, eligible = c("WR")),
    list(pos = "TE",   n = 1L, eligible = c("TE")),
    list(pos = "FLEX", n = 1L, eligible = c("RB", "WR", "TE"))
  ))
}

DACHSHUND_POOL <- 200000          # published prize pool, sums from the tiers
DACHSHUND_FIELD <- 28080L

# ---- field-multiple guardrail (config-derived, not hardcoded) ---------------

test_that("Dachshund field multiple is total_field_size / final seats (= 180)", {
  dcfg <- load_tournament("dachshund")
  expect_equal(tournament_field_multiple(dcfg), 180L)          # 28080 / 156
  expect_equal(resolve_tournament_field_size(dcfg, 1000L), 1080L)  # 6 * 180
  expect_error(resolve_tournament_field_size(dcfg, 100L, snap = FALSE),
               "not a multiple of 180")
})

# ---- the YAML payout ladder itself -------------------------------------------

test_that("dachshund.yaml: fee $8, max 4 entries, single-head $200,000 ladder", {
  dcfg <- load_tournament("dachshund")
  expect_equal(dcfg$entry_fee_usd, 8)
  expect_equal(dcfg$max_entries_per_user, 4)

  # Single-head: no qualifier_round payout table.
  expect_null(dcfg$payouts$qualifier_round)
  tiers <- dcfg$payouts$championship_round$tiers
  expect_false(is.null(tiers))

  # Tier sum = the full $200,000 prize pool, exactly.
  tier_sum <- sum(vapply(tiers, function(t)
    (t$rank_to - t$rank_from + 1) * t$usd, numeric(1)))
  expect_equal(tier_sum, DACHSHUND_POOL, tolerance = 1e-12)

  # Progression buckets reconcile: finalist tiers cover places 1-156,
  # SF-loser tiers cover places 157-936 (= 4,680 qualifier advancers minus
  # 936 still alive after the QF... i.e. 780 SF losers).
  elig <- vapply(tiers, function(t) t$eligibility %||% NA_character_, character(1))
  fin_ranks <- unlist(lapply(tiers[elig == "finalist"],
                             function(t) t$rank_from:t$rank_to))
  sfl_ranks <- unlist(lapply(tiers[elig == "semifinals_loser"],
                             function(t) t$rank_from:t$rank_to))
  expect_equal(sort(fin_ranks), 1:156)
  expect_equal(sort(sfl_ranks), 157:936)
})

test_that("Dachshund per-bucket engine caps conserve the pool (per-tier accounting)", {
  dcfg <- load_tournament("dachshund")
  tiers <- dcfg$payouts$championship_round$tiers
  seats <- vapply(dcfg$stages, function(s) as.integer(s$seats_entering), integer(1))

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

  # Every bucket's caps table is exactly bucket-sized (no money can spill
  # past the bucket end) ...
  expect_equal(caps_slots(fin_caps), 156L)
  expect_equal(caps_slots(sf_caps),  780L)
  expect_equal(caps_slots(qf_caps),  3744L)
  # ... and per-tier accounting recovers the published pool exactly:
  # $169,424 (finalists) + $30,576 (SF losers) + $0 (QF losers) = $200,000.
  expect_equal(caps_money(fin_caps), 169424)
  expect_equal(caps_money(sf_caps),  30576)
  expect_equal(caps_money(qf_caps),  0)
  expect_equal(caps_money(fin_caps) + caps_money(sf_caps) + caps_money(qf_caps),
               DACHSHUND_POOL)
})

# ---- money conservation (gate) -----------------------------------------------

test_that("Dachshund money conservation: gross = $7.123, net = -$0.877 (single-head)", {
  dcfg <- load_tournament("dachshund")
  base <- tournament_field_multiple(dcfg)                      # 180
  # m = 13 divides 156, so every bucket's sample-to-full rank scaling is an
  # integer width (12) and conservation is exact, not just asymptotic.
  n_field <- base * 13L                                        # 2,340
  n_sims <- 12L
  set.seed(1L)
  ids <- paste0("t", seq_len(n_field))
  mk <- function(mu) matrix(pmax(0, stats::rnorm(n_field * n_sims, mu, 30)),
                            nrow = n_field, dimnames = list(ids, NULL))
  scores <- list(stage_scores = list(`1` = mk(2010), `2` = mk(150),
                                     `3` = mk(150), `4` = mk(150)))
  fp <- build_field_payouts(scores, dcfg, seed = 1L)

  expected_gross <- DACHSHUND_POOL / DACHSHUND_FIELD           # $7.1225...
  net <- fp$field_mean_total_ev - dcfg$entry_fee_usd
  message(sprintf(
    "[Dachshund money cons.] n_field=%d gross=%.4f expected=%.4f net=%.4f gap=%.2e",
    n_field, fp$field_mean_total_ev, expected_gross, net,
    abs(fp$field_mean_total_ev - expected_gross)))

  # Conservation is exact (loser-bucket caps anchor at the bucket top).
  expect_equal(fp$field_mean_total_ev, expected_gross, tolerance = 1e-9)
  # Net EV = -rake * fee; Dachshund rake = 1 - 200000 / (8 * 28080) = 10.97%.
  expect_equal(net, expected_gross - 8, tolerance = 1e-9)
  expect_lt(net, 0)

  # Single-head correctness: no qualifier head -> qualifier EV identically 0
  # and total EV is the championship EV.
  expect_equal(max(abs(fp$per_team_ev$qualifier_round_ev)), 0)
  expect_equal(fp$per_team_ev$total_ev, fp$per_team_ev$championship_round_ev)
})

# ---- additivity gate ----------------------------------------------------------

test_that("Dachshund: per-player EV sums to team EV (additivity), every team", {
  dcfg <- load_tournament("dachshund")
  spec <- .dachshund_lineup_spec()
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
  # Pools tuned to the synthetic scale so teams place sometimes (Dachshund stages).
  fake <- list(pools = list(
    q_cum_pool = stats::rnorm(50000, 2010, 130),
    stage_entrant_metric = list(NULL,
      stats::rnorm(5000, 144, 18), stats::rnorm(2000, 144, 18),
      stats::rnorm(800, 144, 18))))
  for (eid in names(rosters)) {
    res <- compute_team_ev(
      pod_rosters = rosters, team_entry_id = eid, positions = positions,
      layerA_draws = layerA, schedule = sched, lineup_spec = spec,
      tournament_cfg = dcfg, field_cache = fake, n_sims = 1200L, seed = 5L)
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

# ---- qualifier consistency vs BBMDB (gated, expensive) -------------------------

test_that("real-set: Dachshund qualifier advance prob matches BBMDB (~3 teams) + engine", {
  bbmdb_path <- "C:/Users/vince/Desktop/bbmdb_scraper/data/bbmdb_teams.parquet"
  if (!file.exists(bbmdb_path)) testthat::skip("BBMDB parquet missing -- gated test.")
  scraper_path <- testthat::test_path("..", "..", "inst", "data",
                                      "scraped_drafts", "udbb-scraper-latest.json")
  if (!file.exists(scraper_path)) testthat::skip("Scraper export missing -- gated test.")

  picks <- load_scraped_drafts(scraper_path)
  val <- validate_xadv_against_bbmdb(
    bbmdb_path = bbmdb_path, scraper_path = scraper_path,
    layerA_n_sims = 1500L, n_sims = 1500L, base_seed = 1L, verbose = FALSE)
  dach_rows <- val$per_team[val$per_team$tournament_id == "dachshund" &
                            !is.na(val$per_team$predicted_xadv) &
                            !is.na(val$per_team$bbmdb_xadv), ]
  testthat::skip_if(nrow(dach_rows) < 1L, "No Dachshund validation team available.")

  # (a) Reported diagnostic -- 3b-5 predicted vs BBMDB across the Dachshund
  #     teams. Known selection-effect optimism (predicted runs above BBMDB;
  #     see inst/scripts/diag_qualifier_xadv_optimism.R) -- surfaced, not
  #     hard-gated. A new regression would show as a blow-up far beyond the
  #     documented level (~+0.1 to +0.25 pooled), so cap the mean signed gap.
  signed_gap <- mean(dach_rows$predicted_xadv - dach_rows$bbmdb_xadv)
  mae <- mean(abs(dach_rows$predicted_xadv - dach_rows$bbmdb_xadv))
  message(sprintf(
    "[Dachshund qualifier] n=%d  MAE(predicted, BBMDB)=%.3f  mean_signed_gap=%+.3f  mean_pred=%.3f mean_bbmdb=%.3f",
    nrow(dach_rows), mae, signed_gap, mean(dach_rows$predicted_xadv),
    mean(dach_rows$bbmdb_xadv)))
  expect_lt(abs(signed_gap), 0.40)        # regression guard, not a calibration gate

  # (b) The GATE: the generalized EV engine reproduces the 3b-5 advance prob
  #     for one Dachshund team (engine-vs-3b5 consistency), run with
  #     dachshund.yaml.
  eid_pick   <- dach_rows$entry_id[1L]
  draft_pick <- dach_rows$draft_id[1L]
  ref_xadv   <- dach_rows$predicted_xadv[1L]

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
  dcfg <- load_tournament("dachshund")
  fake_cache <- list(pools = list(
    q_cum_pool = stats::rnorm(50000L, 2400, 150),
    stage_entrant_metric = list(NULL, stats::rnorm(5000L, 150, 25),
      stats::rnorm(1000L, 150, 25), stats::rnorm(500L, 150, 25))))
  res <- compute_team_ev(
    pod_rosters = pod_rosters, team_entry_id = eid_pick,
    positions = positions, layerA_draws = sim$draws, schedule = schedule,
    lineup_spec = lineup_spec, tournament_cfg = dcfg, field_cache = fake_cache,
    n_sims = 1500L, seed = 1L)
  message(sprintf("[Dachshund engine-vs-3b5] engine=%.3f  3b-5 ref=%.3f  delta=%.3f",
                  res$advance_probs$qualifier, ref_xadv,
                  res$advance_probs$qualifier - ref_xadv))
  expect_lt(abs(res$advance_probs$qualifier - ref_xadv), 0.06)
})
