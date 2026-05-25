# Calibration curves built from Clay's half-PPR totals.

.make_clay_fixture <- function() {
  # 30 players per skill position, strictly linearly decreasing in
  # half-PPR. Linear knot data -> the natural spline is exactly linear
  # between knots, so round-trip and monotonicity tests are precise.
  positions <- c("QB", "RB", "WR", "TE")
  base      <- c(QB = 300, RB = 280, WR = 260, TE = 240)
  rows <- list()
  for (pos in positions) {
    for (rk in 1:30) {
      rows[[length(rows) + 1L]] <- list(
        name      = sprintf("%s%02d", pos, rk),
        team      = "ATL",
        position  = pos,
        projected_points_half_ppr = base[[pos]] - (rk - 1L) * 5
      )
    }
  }
  rows
}

test_that("build_calibration_curves returns a closure per skill position", {
  curves <- build_calibration_curves(.make_clay_fixture())
  expect_setequal(names(curves), c("QB", "RB", "WR", "TE"))
  for (pos in c("QB", "RB", "WR", "TE")) {
    expect_true(is.function(curves[[pos]]),
                info = sprintf("curves[[%s]] should be a function", pos))
  }
})

test_that("curve is monotonically non-increasing for linear input", {
  curves <- build_calibration_curves(.make_clay_fixture())
  for (pos in c("QB", "RB", "WR", "TE")) {
    pts <- curves[[pos]](1:30)
    expect_true(all(diff(pts) <= 1e-9),
                info = sprintf("%s curve should be non-increasing", pos))
  }
})

test_that("rank-1 round-trips back to Clay's top value at that position", {
  curves <- build_calibration_curves(.make_clay_fixture())
  # Fixture's top values: QB1=300, RB1=280, WR1=260, TE1=240
  expect_equal(calibrate_rank_to_points(1, "QB", curves), 300)
  expect_equal(calibrate_rank_to_points(1, "RB", curves), 280)
  expect_equal(calibrate_rank_to_points(1, "WR", curves), 260)
  expect_equal(calibrate_rank_to_points(1, "TE", curves), 240)
})

test_that("rank-N round-trips back to Clay's lowest value at that position", {
  curves <- build_calibration_curves(.make_clay_fixture())
  # Fixture: 30 players per position, step -5. So rank-30 = base - 29*5.
  expect_equal(calibrate_rank_to_points(30, "QB", curves), 300 - 29 * 5)
  expect_equal(calibrate_rank_to_points(30, "RB", curves), 280 - 29 * 5)
})

test_that("ranks beyond Clay's coverage extrapolate flat at the minimum", {
  curves <- build_calibration_curves(.make_clay_fixture())
  rb_min <- 280 - 29 * 5  # rank-30 value
  expect_equal(calibrate_rank_to_points(31,  "RB", curves), rb_min)
  expect_equal(calibrate_rank_to_points(100, "RB", curves), rb_min)
})

test_that("ranks below 1 clamp to the position's maximum", {
  curves <- build_calibration_curves(.make_clay_fixture())
  expect_equal(calibrate_rank_to_points(0,  "WR", curves), 260)
  expect_equal(calibrate_rank_to_points(-5, "WR", curves), 260)
})

test_that("calibrate_rank_to_points handles vector input", {
  curves <- build_calibration_curves(.make_clay_fixture())
  pts <- calibrate_rank_to_points(c(1, 10, 20), "QB", curves)
  expect_length(pts, 3L)
  # Linear: pts[k] == 300 - (rank-1)*5
  expect_equal(pts, c(300, 300 - 9 * 5, 300 - 19 * 5))
})

test_that("aborts when a position has too few Clay data points", {
  too_small <- list(
    list(position = "QB", projected_points_half_ppr = 300),
    list(position = "QB", projected_points_half_ppr = 280)
  )
  expect_error(build_calibration_curves(too_small),
               regexp = "Not enough Clay data points")
})

test_that("each per-position closure binds its own n / pts_min / pts_max", {
  # Regression for the for-loop closure bug: positions have different
  # rank ranges and different min values. If the closures shared the
  # outer loop environment, QB's `n` and `pts_min` would equal TE's
  # (last iteration), and flat extrapolation would return TE's min
  # for QB at rank > TE's N.
  positions <- c("QB", "RB", "WR", "TE")
  pos_n     <- c(QB = 35, RB = 50, WR = 60, TE = 25)
  base      <- c(QB = 300, RB = 280, WR = 260, TE = 240)
  rows <- list()
  for (pos in positions) {
    for (rk in 1:pos_n[[pos]]) {
      rows[[length(rows) + 1L]] <- list(
        position = pos,
        projected_points_half_ppr = base[[pos]] - (rk - 1L) * 5
      )
    }
  }
  curves <- build_calibration_curves(rows)
  # Beyond each position's own N, expect that position's own minimum.
  for (pos in positions) {
    own_min <- base[[pos]] - (pos_n[[pos]] - 1L) * 5
    expect_equal(curves[[pos]](pos_n[[pos]] + 5), own_min,
                 info = sprintf("%s should extrapolate to its OWN min", pos))
  }
})
