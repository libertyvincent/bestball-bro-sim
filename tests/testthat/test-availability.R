# Position-level availability adjustment: factor math, no double-discount,
# upstream application, and the Brooks-class review list.

test_that(".availability_factor scales a full-17-games player down", {
  # RB prior 15.2, Clay projects 17 -> factor 15.2/17.
  expect_equal(.availability_factor(17, 15.2), 15.2 / 17)
})

test_that("no double-discount: clay_games <= expected_games is untouched", {
  # A player Clay already projects below the positional prior (e.g. a
  # returning-from-injury RB at 10 games) must NOT be discounted again.
  expect_equal(.availability_factor(10, 15.2), 1)   # 15.2/10 > 1 -> clamp to 1
  expect_equal(.availability_factor(15.2, 15.2), 1) # exactly at prior -> 1
})

test_that(".availability_factor is a no-op for missing/degenerate inputs", {
  expect_equal(.availability_factor(NA, 15.2), 1)
  expect_equal(.availability_factor(0, 15.2), 1)
  expect_equal(.availability_factor(17, NA), 1)
  expect_equal(.availability_factor(17, NULL), 1)
})

.avail_clay_fixture <- function() {
  list(players = list(
    list(name = "Full RB",    position = "RB", games = 17,
         projected_points_half_ppr = 200, projected_points_full_ppr = 230),
    list(name = "Hurt RB",    position = "RB", games = 10,
         projected_points_half_ppr = 120, projected_points_full_ppr = 140),
    list(name = "Full QB",    position = "QB", games = 17,
         projected_points_half_ppr = 300, projected_points_full_ppr = 300)
  ))
}

test_that(".apply_availability_to_clay scales upstream and respects the clamp", {
  prior <- list(enabled = TRUE,
                expected_games = list(QB = 16.8, RB = 15.2, WR = 16.5, TE = 16.6))
  out <- .apply_availability_to_clay(.avail_clay_fixture(), prior)
  pl  <- out$clay_offense$players

  # Full-17 RB scaled by 15.2/17.
  expect_equal(pl[[1]]$projected_points_half_ppr, 200 * 15.2 / 17)
  expect_equal(pl[[1]]$projected_points_full_ppr, 230 * 15.2 / 17)
  # Hurt RB (10 games < 15.2 prior) untouched -- the no-double-discount rule.
  expect_equal(pl[[2]]$projected_points_half_ppr, 120)
  # QB scaled by its own milder factor.
  expect_equal(pl[[3]]$projected_points_half_ppr, 300 * 16.8 / 17)

  # Summary reflects one scaled + one clamped RB.
  rb <- out$summary$per_position$RB
  expect_equal(rb$players_scaled, 1L)
  expect_equal(rb$players_clamped, 1L)
})

test_that(".apply_availability_to_clay is a no-op when disabled", {
  prior <- list(enabled = FALSE, expected_games = list())
  out <- .apply_availability_to_clay(.avail_clay_fixture(), prior)
  expect_null(out$summary)
  expect_equal(out$clay_offense$players[[1]]$projected_points_half_ppr, 200)
})

test_that("the packaged availability prior loads and has RB steepest", {
  prior <- .load_availability_prior()
  expect_true(prior$enabled)
  E <- prior$expected_games
  expect_true(E$RB < E$WR && E$RB < E$TE && E$RB < E$QB)  # RB steepest discount
})

test_that(".write_availability_review flags Brooks-class outliers", {
  players <- list(
    # Brooks-class: market ~0 role, sources see a full season (overshoot).
    list(underdog_id = "a", name = "Jonathon Brooks", position = "RB",
         season_mean = 116, underdog_projected_points = 24.3),
    # near-zero-role: ud < 20, sm > 40.
    list(underdog_id = "b", name = "MarShawn Lloyd", position = "RB",
         season_mean = 44, underdog_projected_points = 4.5),
    # Normal player: not flagged.
    list(underdog_id = "c", name = "Bijan Robinson", position = "RB",
         season_mean = 270, underdog_projected_points = 294.9)
  )
  td <- tempfile("avail_review_"); dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  rv <- .write_availability_review("test_slate", players, out_dir = td)

  expect_equal(rv$count, 2L)
  expect_true("Jonathon Brooks" %in% rv$df$name)
  expect_true("MarShawn Lloyd" %in% rv$df$name)
  expect_false("Bijan Robinson" %in% rv$df$name)
  expect_true(file.exists(rv$path))
})
