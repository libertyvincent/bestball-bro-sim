# Hermetic tests for .project_v0 — no network, no nflreadr dependency.
# Builds small fixtures that match the shapes nflreadr would return.

make_test_rosters <- function() {
  data.frame(
    full_name = c("Josh Allen", "Bijan Robinson", "CeeDee Lamb",
                  "Sam LaPorta", "Patrick Mahomes", "Christian McCaffrey",
                  "Justin Jefferson", "Travis Kelce", "Rookie WR"),
    gsis_id   = paste0("00-", sprintf("%07d", 1:9)),
    team      = c("BUF", "ATL", "DAL", "DET", "KC", "SF", "MIN", "KC", "JAX"),
    position  = c("QB", "RB", "WR", "TE", "QB", "RB", "WR", "TE", "WR"),
    stringsAsFactors = FALSE
  )
}

make_test_historical <- function() {
  # Weekly fixture — one row per player-game. .project_v0 uses each player's
  # most recent season (2025). Three regular-season weeks per player-season,
  # with per-week variation so weekly FP has non-zero spread. Player 1 also
  # gets a huge playoff (POST) game that must be excluded.
  # Every player (ids 1-8) has prior data; roster id 9 (Rookie WR) has none.
  base <- list(
    "00-0000001" = list(pos = "QB", py = 270, pt = 2, int = 1, ry = 28, rt = 0, rec = 0, recy = 0,  rect = 0),
    "00-0000005" = list(pos = "QB", py = 300, pt = 2, int = 1, ry = 22, rt = 0, rec = 0, recy = 0,  rect = 0),
    "00-0000002" = list(pos = "RB", py = 0,   pt = 0, int = 0, ry = 95, rt = 1, rec = 4, recy = 32, rect = 0),
    "00-0000006" = list(pos = "RB", py = 0,   pt = 0, int = 0, ry = 80, rt = 1, rec = 3, recy = 24, rect = 0),
    "00-0000003" = list(pos = "WR", py = 0,   pt = 0, int = 0, ry = 0,  rt = 0, rec = 7, recy = 95, rect = 1),
    "00-0000007" = list(pos = "WR", py = 0,   pt = 0, int = 0, ry = 0,  rt = 0, rec = 6, recy = 78, rect = 0),
    "00-0000004" = list(pos = "TE", py = 0,   pt = 0, int = 0, ry = 0,  rt = 0, rec = 5, recy = 55, rect = 0),
    "00-0000008" = list(pos = "TE", py = 0,   pt = 0, int = 0, ry = 0,  rt = 0, rec = 4, recy = 40, rect = 0)
  )
  week_mult   <- c(0.7, 1.0, 1.3)               # within-season weekly variation
  season_mult <- c("2024" = 0.85, "2025" = 1.0) # 2025 is the recent season

  rows <- list()
  for (pid in names(base)) {
    b <- base[[pid]]
    for (s in c(2024, 2025)) for (w in seq_along(week_mult)) {
      mlt <- season_mult[[as.character(s)]] * week_mult[w]
      rows[[length(rows) + 1L]] <- data.frame(
        player_id = pid, position = b$pos, season = s, week = w,
        season_type = "REG",
        passing_yards   = b$py   * mlt, passing_tds     = b$pt   * mlt,
        interceptions   = b$int  * mlt,
        rushing_yards   = b$ry   * mlt, rushing_tds     = b$rt   * mlt,
        receptions      = b$rec  * mlt, receiving_yards = b$recy * mlt,
        receiving_tds   = b$rect * mlt,
        stringsAsFactors = FALSE
      )
    }
  }
  # Playoff game for player 1 — must be excluded by .project_v0.
  rows[[length(rows) + 1L]] <- data.frame(
    player_id = "00-0000001", position = "QB", season = 2025, week = 19L,
    season_type = "POST",
    passing_yards = 999, passing_tds = 9, interceptions = 0,
    rushing_yards = 99, rushing_tds = 9,
    receptions = 0, receiving_yards = 0, receiving_tds = 0,
    stringsAsFactors = FALSE
  )
  do.call(rbind, rows)
}

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
  # of the season means of the WRs that DO have prior data.
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

test_that(".project_v0 output round-trips through publish without error", {
  out <- .project_v0(make_test_rosters(), make_test_historical(),
                     load_scoring_config("half_ppr_underdog"))

  tmp <- tempfile(); dir.create(tmp); on.exit(unlink(tmp, recursive = TRUE))
  out_path <- publish_projections(out, out_dir = tmp, season = 2026)
  expect_true(file.exists(out_path))

  parsed <- jsonlite::fromJSON(out_path, simplifyVector = FALSE)
  expect_length(parsed$players, 9)
  expect_true(all(c("_meta", "players") %in% names(parsed)))
})
