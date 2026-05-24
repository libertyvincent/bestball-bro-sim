test_that("scoring config loads", {
  s <- load_scoring_config("half_ppr_underdog")
  expect_s3_class(s, "bbbro_scoring")
  expect_equal(s$id, "half_ppr_underdog")
  expect_equal(s$rules$receiving$reception_points, 0.5)
  expect_equal(s$rules$passing$yards_per_point, 25)
  expect_equal(s$rules$te_premium_points, 0)
})

test_that("compute_fantasy_points computes QB stats correctly", {
  s <- load_scoring_config("half_ppr_underdog")

  # 300 pass yds / 25 = 12 pts
  # 2 pass TDs * 4   = 8 pts
  # 1 INT * -1       = -1 pt
  # 50 rush yds / 10 = 5 pts
  # 1 rush TD * 6    = 6 pts
  # Total            = 30
  qb_stats <- data.frame(
    position = "QB",
    passing_yards = 300, passing_tds = 2, interceptions = 1,
    rushing_yards = 50,  rushing_tds = 1
  )
  expect_equal(compute_fantasy_points(qb_stats, s), 30)
})

test_that("compute_fantasy_points handles WR half-PPR", {
  s <- load_scoring_config("half_ppr_underdog")

  # 8 rec * 0.5  = 4 pts
  # 100 yds / 10 = 10 pts
  # 1 rec TD * 6 = 6 pts
  # Total        = 20
  wr_stats <- data.frame(
    position = "WR",
    receptions = 8, receiving_yards = 100, receiving_tds = 1
  )
  expect_equal(compute_fantasy_points(wr_stats, s), 20)
})

test_that("compute_fantasy_points works on a multi-row data frame", {
  s <- load_scoring_config("half_ppr_underdog")

  stats <- data.frame(
    position       = c("QB", "WR", "RB"),
    passing_yards  = c(300,   0,   0),
    passing_tds    = c(  2,   0,   0),
    interceptions  = c(  1,   0,   0),
    rushing_yards  = c( 50,   0,  80),
    rushing_tds    = c(  1,   0,   0),
    receptions     = c(  0,   8,   4),
    receiving_yards = c( 0, 100,  30),
    receiving_tds  = c(  0,   1,   0)
  )

  fp <- compute_fantasy_points(stats, s)
  expect_length(fp, 3)
  expect_equal(fp[1], 30)            # QB: as above
  expect_equal(fp[2], 20)            # WR: as above
  expect_equal(fp[3], 8 + 2 + 3)     # RB: 80/10 + 4*0.5 + 30/10 = 13
})

test_that("compute_fantasy_points treats missing columns as zero (NA-safe)", {
  s <- load_scoring_config("half_ppr_underdog")

  # Only receptions present — everything else absent
  stats <- data.frame(position = "WR", receptions = 5)
  expect_equal(compute_fantasy_points(stats, s), 2.5)

  # NA values should be treated as 0, not propagate
  stats_na <- data.frame(
    position = "RB",
    rushing_yards = NA, rushing_tds = 1, receptions = NA
  )
  expect_equal(compute_fantasy_points(stats_na, s), 6)
})

test_that("compute_fantasy_points applies TE premium when configured", {
  # Build a scoring cfg with TE premium = 0.5 (full PPR for TEs)
  s <- load_scoring_config("half_ppr_underdog")
  s$rules$te_premium_points <- 0.5

  stats <- data.frame(
    position    = c("WR", "TE"),
    receptions  = c(  6,    6),
    receiving_yards = c( 0,  0)
  )
  fp <- compute_fantasy_points(stats, s)

  # WR: 6 * 0.5 = 3
  # TE: 6 * 0.5 + 6 * 0.5 (premium) = 6
  expect_equal(fp[1], 3)
  expect_equal(fp[2], 6)
})

test_that("compute_fantasy_points handles fumbles lost", {
  s <- load_scoring_config("half_ppr_underdog")

  stats <- data.frame(
    position = "RB",
    rushing_yards = 100,
    rushing_fumbles_lost = 1,
    receiving_fumbles_lost = 0
  )
  # 100/10 + 1 * -2 = 8
  expect_equal(compute_fantasy_points(stats, s), 8)
})
