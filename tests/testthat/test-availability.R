# Position-level availability: per-player miss-rate math (carries PR #34's
# clamp), the _meta mechanism marker, and the Brooks-class review list.

test_that(".availability_p_miss: full-17-games player gets the position rate", {
  # RB prior 15.2, Clay projects 17 -> p_miss = 1 - 15.2/17.
  expect_equal(.availability_p_miss(17, 15.2), 1 - 15.2 / 17)
  expect_equal(.availability_p_miss(17, 16.8), 1 - 16.8 / 17)  # QB
})

test_that("no double-discount: clay_games <= prior -> p_miss = 0", {
  # A player Clay already projects below the positional prior (suspension,
  # buried handcuff) must NOT be masked again.
  expect_equal(.availability_p_miss(10, 15.2), 0)   # 15.2/10 > 1 -> q clamps to 1
  expect_equal(.availability_p_miss(15.2, 15.2), 0) # exactly at prior -> 0
})

test_that(".availability_p_miss defaults missing clay_games to a 17-game season", {
  # ETR/LegUp-only players (no Clay match) inherit the canonical position rate.
  expect_equal(.availability_p_miss(NA, 15.2), 1 - 15.2 / 17)
  expect_equal(.availability_p_miss(0, 15.2),  1 - 15.2 / 17)
  expect_equal(.availability_p_miss(NULL, 15.2), 1 - 15.2 / 17)
})

test_that(".availability_p_miss is 0 when disabled or prior missing", {
  expect_equal(.availability_p_miss(17, 15.2, enabled = FALSE), 0)
  expect_equal(.availability_p_miss(17, NA), 0)
  expect_equal(.availability_p_miss(17, NULL), 0)
})

test_that(".availability_mechanism_marker describes draw-zeroing with priors", {
  prior <- list(enabled = TRUE,
                expected_games = list(QB = 16.8, RB = 15.2, WR = 16.5, TE = 16.6))
  m <- .availability_mechanism_marker(prior)
  expect_equal(m$type, "draw_zeroing")
  expect_equal(m$missed_week_model, "iid_bernoulli")
  expect_equal(m$expected_games$RB, 15.2)
  # canonical 17-game miss rate, RB steepest.
  expect_equal(m$p_miss_canonical_17g$RB, round(1 - 15.2 / 17, 4))
  expect_true(m$p_miss_canonical_17g$RB > m$p_miss_canonical_17g$WR)
})

test_that(".availability_mechanism_marker is NULL when disabled", {
  expect_null(.availability_mechanism_marker(
    list(enabled = FALSE, expected_games = list())))
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
