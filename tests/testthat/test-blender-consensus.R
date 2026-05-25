# .compute_consensus: weighted mean + weighted variance, renormalized
# over whatever sources are actually present for the player.

test_that("three sources, all present: weighted mean is straight w*x sum", {
  res <- .compute_consensus(
    points_by_src  = list(clay = 300, etr = 270, legup = 285),
    weights_by_src = list(clay = 0.333, etr = 0.333, legup = 0.333)
  )
  expect_equal(res$mean, mean(c(300, 270, 285)), tolerance = 0.5)
  expect_true(res$disagreement_std > 0)
  # Renormalized weights still sum to 1
  expect_equal(sum(unlist(res$norm_weights)), 1, tolerance = 1e-9)
})

test_that("non-equal weights apply correctly", {
  res <- .compute_consensus(
    points_by_src  = list(clay = 200, etr = 100),
    weights_by_src = list(clay = 0.7,  etr = 0.3)
  )
  expect_equal(res$mean, 0.7 * 200 + 0.3 * 100, tolerance = 1e-9)
  expect_equal(res$norm_weights$clay,  0.7, tolerance = 1e-9)
  expect_equal(res$norm_weights$etr,   0.3, tolerance = 1e-9)
})

test_that("source absent for a player: remaining weights renormalize to 1", {
  # Player has clay + legup but not etr. The manifest weights were
  # (0.333, 0.333, 0.333) but here only clay+legup are present.
  res <- .compute_consensus(
    points_by_src  = list(clay = 200, legup = 100),
    weights_by_src = list(clay = 0.333, legup = 0.333)
  )
  expect_equal(sum(unlist(res$norm_weights)), 1, tolerance = 1e-9)
  expect_equal(res$norm_weights$clay,  0.5, tolerance = 1e-9)
  expect_equal(res$norm_weights$legup, 0.5, tolerance = 1e-9)
  expect_equal(res$mean, 150, tolerance = 1e-9)
})

test_that("single source: mean = that value, disagreement_std = 0", {
  res <- .compute_consensus(
    points_by_src  = list(clay = 200),
    weights_by_src = list(clay = 0.333)
  )
  expect_equal(res$mean, 200, tolerance = 1e-9)
  expect_equal(res$disagreement_std, 0, tolerance = 1e-9)
  expect_equal(res$norm_weights$clay, 1, tolerance = 1e-9)
})

test_that("zero sources: returns NA mean and empty norm_weights", {
  res <- .compute_consensus(points_by_src = list(), weights_by_src = list())
  expect_true(is.na(res$mean))
  expect_true(is.na(res$disagreement_std))
  expect_length(res$norm_weights, 0)
})

test_that("disagreement_std grows with cross-source spread", {
  tight <- .compute_consensus(
    points_by_src  = list(clay = 200, etr = 202, legup = 198),
    weights_by_src = list(clay = 0.333, etr = 0.333, legup = 0.333)
  )
  spread <- .compute_consensus(
    points_by_src  = list(clay = 250, etr = 150, legup = 200),
    weights_by_src = list(clay = 0.333, etr = 0.333, legup = 0.333)
  )
  expect_true(spread$disagreement_std > tight$disagreement_std)
})
