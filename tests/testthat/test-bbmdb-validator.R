# Tests for R/bbmdb_validator.R. Synthetic fixtures cover the kernel's
# correctness contracts (conservation, ranking, joint-draw correlation,
# determinism); the real-set smoke test is gated on the local BBMDB
# parquet being present and runs the full pipeline when it is.

.season_spec <- function() {
  list(slate_id = "test-season", slots = list(
    list(pos = "QB",   n = 1L, eligible = c("QB")),
    list(pos = "RB",   n = 2L, eligible = c("RB")),
    list(pos = "WR",   n = 3L, eligible = c("WR")),
    list(pos = "TE",   n = 1L, eligible = c("TE")),
    list(pos = "FLEX", n = 1L, eligible = c("RB", "WR", "TE"))
  ))
}

# Build a synthetic 12-team pod where each team has a Season-valid roster.
# Each roster: 2 QB / 4 RB / 5 WR / 2 TE = 13 players. Teams are drawn from
# a pool of synthetic NFL team labels; opponents always non-roster, so
# (when desired) we can disable inter-team correlation by making every
# roster player play a unique non-roster opponent.
.synthetic_pod <- function(n_teams = 12L,
                           n_weeks = 14L,
                           n_sims_A = 5000L,
                           seed_A = 7L,
                           teams_per_pool = 32L,
                           cross_team_pid = NULL) {
  set.seed(seed_A)
  positions_per_team <- c(rep("QB", 2L), rep("RB", 4L),
                          rep("WR", 5L), rep("TE", 2L))
  per_team <- length(positions_per_team)  # 13

  pod_rosters <- list()
  positions   <- character(0)
  pid_team    <- character(0)
  pids_so_far <- 0L
  for (i in seq_len(n_teams)) {
    eid <- sprintf("ENTRY_%02d", i)
    pids <- sprintf("P_%02d_%02d", i, seq_len(per_team))
    pod_rosters[[eid]] <- pids
    pos_vec <- setNames(positions_per_team, pids)
    positions <- c(positions, pos_vec)
    nfl_teams <- paste0("NT", sprintf("%02d",
      ((seq_len(per_team) + i) %% teams_per_pool) + 1L))
    names(nfl_teams) <- pids
    pid_team <- c(pid_team, nfl_teams)
    pids_so_far <- pids_so_far + per_team
  }
  # Optionally force a shared NFL team between two pod-mates to verify
  # cross-team correlation kicks in. Used by the joint-draw test.
  if (!is.null(cross_team_pid)) {
    for (k in names(cross_team_pid)) {
      pid_team[k] <- cross_team_pid[[k]]
    }
  }

  # Build schedule + Layer A draws. Every roster player plays a
  # non-roster opponent every week (no game-pair leakage like the 3b-4
  # fixture caught).
  sched_chunks <- list()
  la_chunks <- list()
  pos_mu <- c(QB = 18, RB = 13, WR = 11, TE = 8)
  pos_sd <- c(QB = 7,  RB = 6,  WR = 6,  TE = 4)
  all_pids <- names(positions)
  for (pid in all_pids) {
    tm <- pid_team[pid]
    p  <- positions[pid]
    for (w in seq_len(n_weeks)) {
      sched_chunks[[length(sched_chunks) + 1L]] <- data.frame(
        underdog_id = pid, week = as.integer(w),
        team = tm, opponent = paste0("OPP_", tm),
        is_bye = FALSE, stringsAsFactors = FALSE
      )
      vals <- pmax(0, stats::rnorm(n_sims_A, mean = pos_mu[p], sd = pos_sd[p]))
      la_chunks[[length(la_chunks) + 1L]] <- data.frame(
        underdog_id = pid, sim_idx = seq_len(n_sims_A),
        week = as.integer(w), draw_value = vals,
        stringsAsFactors = FALSE
      )
    }
  }
  list(
    pod_rosters  = pod_rosters,
    positions    = positions,
    schedule     = do.call(rbind, sched_chunks),
    layerA_draws = do.call(rbind, la_chunks),
    pid_team     = pid_team
  )
}

# ---- 1. Conservation -------------------------------------------------------

test_that("conservation: sum of predicted_xadv across pod == advance_n", {
  fx <- .synthetic_pod(n_teams = 12L, n_weeks = 4L, n_sims_A = 3000L,
                       seed_A = 11L)
  for (adv_n in c(2L, 4L)) {
    res <- predict_pod_xadv(
      pod_rosters  = fx$pod_rosters,
      positions    = fx$positions,
      layerA_draws = fx$layerA_draws,
      schedule     = fx$schedule,
      lineup_spec  = .season_spec(),
      ranking_weeks = 1:4,
      advance_n    = adv_n,
      n_sims       = 2000L,
      seed         = 42L
    )
    # Sum across all 12 teams equals advance_n exactly.
    expect_equal(sum(res$predicted_xadv), adv_n,
                 tolerance = 1e-10,
                 info = sprintf("advance_n=%d", adv_n))
  }
})

# ---- 2. Ranking correctness on a hand-built pod ---------------------------

test_that("ranking: hand-rigged strong teams advance most often", {
  # Give teams 1 and 2 a clear strength advantage by boosting every player's
  # Layer A draws by a large constant. The strongest pair should dominate
  # the predicted_xadv ordering and the sum-of-strengths ranking.
  fx <- .synthetic_pod(n_teams = 12L, n_weeks = 4L, n_sims_A = 5000L,
                       seed_A = 13L)
  strong_pids <- c(fx$pod_rosters[["ENTRY_01"]],
                   fx$pod_rosters[["ENTRY_02"]])
  fx$layerA_draws$draw_value[fx$layerA_draws$underdog_id %in% strong_pids] <-
    fx$layerA_draws$draw_value[fx$layerA_draws$underdog_id %in% strong_pids] + 12
  res <- predict_pod_xadv(
    pod_rosters  = fx$pod_rosters,
    positions    = fx$positions,
    layerA_draws = fx$layerA_draws,
    schedule     = fx$schedule,
    lineup_spec  = .season_spec(),
    ranking_weeks = 1:4,
    advance_n    = 2L,
    n_sims       = 3000L,
    seed         = 71L
  )
  # ENTRY_01 and ENTRY_02 (the boosted teams) should both have predicted_xadv
  # near 1 -- they almost always advance.
  expect_gt(res$predicted_xadv[["ENTRY_01"]], 0.9)
  expect_gt(res$predicted_xadv[["ENTRY_02"]], 0.9)
  # And the remaining 10 teams each have very low predicted_xadv.
  rest <- setdiff(names(res$predicted_xadv), c("ENTRY_01", "ENTRY_02"))
  expect_true(all(res$predicted_xadv[rest] < 0.1))
})

# ---- 3. Joint-draw correlation respected ----------------------------------

test_that("two pod teams sharing an NFL team show correlated season totals", {
  # Make team 1 and team 2 each have 3 players on the same NFL team
  # ("SHARED"). With non-zero team correlation in default_corr_params,
  # their per-sim totals should covary above the unstacked baseline.
  fx_corr <- .synthetic_pod(n_teams = 12L, n_weeks = 4L, n_sims_A = 5000L,
                            seed_A = 17L,
                            cross_team_pid = c(
                              "P_01_03" = "SHARED", "P_01_07" = "SHARED",
                              "P_01_12" = "SHARED",
                              "P_02_03" = "SHARED", "P_02_07" = "SHARED",
                              "P_02_12" = "SHARED"
                            ))
  res_corr <- predict_pod_xadv(
    pod_rosters  = fx_corr$pod_rosters,
    positions    = fx_corr$positions,
    layerA_draws = fx_corr$layerA_draws,
    schedule     = fx_corr$schedule,
    lineup_spec  = .season_spec(),
    ranking_weeks = 1:4,
    advance_n    = 2L,
    n_sims       = 3000L,
    seed         = 91L
  )
  # Joint correlation: teams 1 and 2 should covary positively.
  r_corr <- stats::cor(res_corr$season_totals["ENTRY_01", ],
                       res_corr$season_totals["ENTRY_02", ])

  # Now contrast with zero correlation: same fixture, all loadings on idio.
  res_zero <- predict_pod_xadv(
    pod_rosters  = fx_corr$pod_rosters,
    positions    = fx_corr$positions,
    layerA_draws = fx_corr$layerA_draws,
    schedule     = fx_corr$schedule,
    lineup_spec  = .season_spec(),
    ranking_weeks = 1:4,
    advance_n    = 2L,
    n_sims       = 3000L,
    seed         = 91L,
    corr_params  = list(team = 0, game = 0, cross = 0)
  )
  r_zero <- stats::cor(res_zero$season_totals["ENTRY_01", ],
                       res_zero$season_totals["ENTRY_02", ])

  message(sprintf("[3b-5 joint draw] r_corr=%.3f  r_zero=%.3f", r_corr, r_zero))
  # The correlated pod-mates produce noticeably more cross-team covariance
  # than the independent baseline. Loose absolute threshold (sampling noise
  # is real here) plus a relative check.
  expect_gt(r_corr - r_zero, 0.05)
  expect_lt(abs(r_zero),    0.05)
})

# ---- 4. Determinism --------------------------------------------------------

test_that("same seed -> identical output; different seed -> different", {
  fx <- .synthetic_pod(n_teams = 12L, n_weeks = 2L, n_sims_A = 1000L,
                       seed_A = 3L)
  call_it <- function(seed) {
    predict_pod_xadv(
      pod_rosters  = fx$pod_rosters,
      positions    = fx$positions,
      layerA_draws = fx$layerA_draws,
      schedule     = fx$schedule,
      lineup_spec  = .season_spec(),
      ranking_weeks = 1:2,
      advance_n    = 2L,
      n_sims       = 500L,
      seed         = seed
    )
  }
  r1 <- call_it(42L)
  r2 <- call_it(42L)
  r3 <- call_it(43L)
  expect_identical(r1$predicted_xadv, r2$predicted_xadv)
  expect_identical(r1$season_totals,  r2$season_totals)
  expect_false(identical(r1$predicted_xadv, r3$predicted_xadv))
})

# ---- 5. Smaller pod handles short positions / fewer entries --------------

test_that("validator handles a smaller pod (4 teams) without crashing", {
  fx <- .synthetic_pod(n_teams = 4L, n_weeks = 2L, n_sims_A = 500L,
                       seed_A = 5L)
  res <- predict_pod_xadv(
    pod_rosters  = fx$pod_rosters,
    positions    = fx$positions,
    layerA_draws = fx$layerA_draws,
    schedule     = fx$schedule,
    lineup_spec  = .season_spec(),
    ranking_weeks = 1:2,
    advance_n    = 2L,
    n_sims       = 200L,
    seed         = 1L
  )
  expect_equal(length(res$predicted_xadv), 4L)
  expect_equal(sum(res$predicted_xadv), 2, tolerance = 1e-10)
})

# ---- 6. Input validation ---------------------------------------------------

test_that("validator errors when advance_n >= pod size", {
  fx <- .synthetic_pod(n_teams = 4L, n_weeks = 1L, n_sims_A = 200L,
                       seed_A = 1L)
  expect_error(
    predict_pod_xadv(
      pod_rosters  = fx$pod_rosters, positions = fx$positions,
      layerA_draws = fx$layerA_draws, schedule = fx$schedule,
      lineup_spec  = .season_spec(),
      ranking_weeks = 1L,
      advance_n    = 4L,
      n_sims       = 100L
    ),
    "advance_n"
  )
})

# ---- 7. Real-set smoke test (gated on local BBMDB parquet) ----------------

test_that("real-set: BBMDB validation runs end-to-end and Spearman > 0.4", {
  bbmdb_path <- "C:/Users/vince/Desktop/bbmdb_scraper/data/bbmdb_teams.parquet"
  if (!file.exists(bbmdb_path)) {
    testthat::skip("BBMDB parquet not at canonical local path -- gated test.")
  }
  scraper_path <- testthat::test_path("..", "..", "inst", "data",
                                      "scraped_drafts",
                                      "udbb-scraper-latest.json")
  if (!file.exists(scraper_path)) {
    testthat::skip("Scraper export missing from inst/data/scraped_drafts.")
  }
  result <- validate_xadv_against_bbmdb(
    bbmdb_path    = bbmdb_path,
    scraper_path  = scraper_path,
    layerA_n_sims = 5000L,
    n_sims        = 5000L,
    base_seed     = 1L,
    verbose       = FALSE
  )
  message(sprintf(
    "[3b-5 real set] n=%d  Spearman=%.3f  MAE=%.3f  signed=%+.3f  Pearson(proj)=%.3f",
    result$aggregates$n_validated,
    result$aggregates$spearman,
    result$aggregates$mae,
    result$aggregates$mean_signed_error,
    result$projection_xcheck$pearson
  ))
  # Loose smoke floor per prompt -- N is small (~20), no bucketed claim.
  expect_gt(result$aggregates$n_validated, 10L)
  expect_gt(result$aggregates$spearman, 0.4)
})
