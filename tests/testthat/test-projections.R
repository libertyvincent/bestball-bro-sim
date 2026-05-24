# Hermetic tests for .project_v0 — no network, no nflreadr dependency.
# Shared fixtures (make_test_rosters / make_test_historical) live in
# helper-fixtures.R so other test files can reuse them.

test_that(".project_v0 runs end-to-end on mock data and returns expected columns", {
  out <- .project_v0(make_test_rosters(), make_test_historical(),
                     load_scoring_config("half_ppr_underdog"))

  # One row per rostered skill player
  expect_equal(nrow(out), 9)
  expect_named(out, c(
    "name", "team", "position", "gsis_id",
    "season_mean", "season_std",
    "season_p10", "season_p25", "season_p50",
    "season_p75", "season_p90", "season_p95",
    "position_rank", "vor"
  ))
})

test_that(".project_v0 season_mean is the sum of the most recent season's weekly FP", {
  rosters <- make_test_rosters()
  hist    <- make_test_historical()
  scoring <- load_scoring_config("half_ppr_underdog")

  out <- .project_v0(rosters, hist, scoring)

  # Bijan Robinson (00-0000002): season_mean = sum of his 2025 weekly FP
  hist$fp     <- compute_fantasy_points(hist, scoring)
  bijan_2025  <- hist$fp[hist$player_id == "00-0000002" & hist$season == 2025]
  bijan_2024  <- hist$fp[hist$player_id == "00-0000002" & hist$season == 2024]
  bijan       <- out[out$gsis_id == "00-0000002", ]

  expect_equal(bijan$season_mean, sum(bijan_2025))
  # Most recent season is used (2025 > 2024 here), not an older season.
  expect_gt(bijan$season_mean, sum(bijan_2024))
})

test_that(".project_v0 season_std is weekly sd scaled by sqrt(games_played)", {
  rosters <- make_test_rosters()
  hist    <- make_test_historical()
  scoring <- load_scoring_config("half_ppr_underdog")

  out <- .project_v0(rosters, hist, scoring)

  hist$fp <- compute_fantasy_points(hist, scoring)
  w       <- hist$fp[hist$player_id == "00-0000002" & hist$season == 2025]
  bijan   <- out[out$gsis_id == "00-0000002", ]

  expect_equal(bijan$season_std, stats::sd(w) * sqrt(length(w)))
})

test_that(".project_v0 excludes playoff weeks from the projection", {
  rosters <- make_test_rosters()
  hist    <- make_test_historical()
  scoring <- load_scoring_config("half_ppr_underdog")

  out <- .project_v0(rosters, hist, scoring)

  # Player 00-0000001 has a huge week-19 POST game; only REG weeks count.
  hist$fp <- compute_fantasy_points(hist, scoring)
  reg_fp  <- hist$fp[hist$player_id == "00-0000001" & hist$season == 2025 &
                       hist$season_type == "REG"]
  qb1     <- out[out$gsis_id == "00-0000001", ]

  expect_equal(qb1$season_mean, sum(reg_fp))
})

test_that(".project_v0 falls back to position median for players with no prior data", {
  rosters <- make_test_rosters()
  hist    <- make_test_historical()
  scoring <- load_scoring_config("half_ppr_underdog")

  out <- .project_v0(rosters, hist, scoring)

  # Rookie WR (00-0000009) has no prior stats. Expected season_mean = median
  # of the season means of the WRs that DO have prior data. (Note: in the
  # v1 slate flow, rookies never reach .project_v0 — they go through
  # project_rookies — but the legacy fallback is preserved for direct
  # callers and exercised here.)
  wr_with_data <- out$season_mean[out$position == "WR" &
                                    out$gsis_id != "00-0000009"]
  rookie       <- out[out$gsis_id == "00-0000009", ]

  expect_equal(rookie$season_mean, stats::median(wr_with_data))
})

test_that(".project_v0 assigns position ranks correctly", {
  out <- .project_v0(make_test_rosters(), make_test_historical(),
                     load_scoring_config("half_ppr_underdog"))

  # Each position should have ranks starting at 1
  for (pos in c("QB", "RB", "WR", "TE")) {
    ranks <- out$position_rank[out$position == pos]
    expect_true(paste0(pos, "1") %in% ranks)
  }

  # Top WR (rank 1) should have the highest season_mean among WRs
  wr_rows <- out[out$position == "WR", ]
  top_wr  <- wr_rows[wr_rows$position_rank == "WR1", ]
  expect_equal(top_wr$season_mean, max(wr_rows$season_mean))
})

test_that(".project_v0 produces sensible percentiles (p10 < p50 < p90)", {
  out <- .project_v0(make_test_rosters(), make_test_historical(),
                     load_scoring_config("half_ppr_underdog"))

  expect_true(all(out$season_p10 < out$season_p50))
  expect_true(all(out$season_p50 < out$season_p90))
  expect_true(all(out$season_p25 < out$season_p75))
})
