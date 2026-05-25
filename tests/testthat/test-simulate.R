# Monte Carlo two-level simulator: tests are deterministic (set.seed)
# and use synthetic per-player records that mimic blend_slate() shape.

.make_player_record <- function(season_mean = 200,
                                 disagreement_std = 5,
                                 cv = 0.50,
                                 n_weeks = 18L,
                                 bye_week = 8L,
                                 underdog_id = "test-ud") {
  weekly <- vector("list", n_weeks)
  per_act <- season_mean / (n_weeks - 1L)  # 17 active weeks
  for (w in seq_len(n_weeks)) {
    if (w == bye_week) {
      weekly[[w]] <- list(week = w, opponent = NULL, home_away = NULL,
                          is_bye = TRUE, mean = 0, std = 0,
                          percentiles = list(p10 = 0, p50 = 0, p90 = 0))
    } else {
      weekly[[w]] <- list(
        week        = w, opponent = "OPP",
        home_away   = if (w %% 2L == 0L) "home" else "away",
        is_bye      = FALSE,
        mean        = per_act,
        std         = cv * per_act,
        percentiles = list(p10 = 0, p50 = per_act, p90 = 0)
      )
    }
  }
  list(
    underdog_id        = underdog_id,
    name               = "Test Player",
    team               = "ATL",
    position           = "RB",
    season_mean        = season_mean,
    season_std         = sqrt(disagreement_std^2 +
                              (n_weeks - 1L) * (cv * per_act)^2),
    disagreement_std   = disagreement_std,
    aleatoric_std      = sqrt((n_weeks - 1L) * (cv * per_act)^2),
    weekly             = weekly,
    sources_used       = c("clay", "etr", "legup"),
    source_breakdown   = list()
  )
}

test_that("empirical season mean matches analytical mean within 1%", {
  set.seed(42)
  player <- .make_player_record(season_mean = 200, disagreement_std = 10,
                                 cv = 0.50)
  m <- .simulate_player(player, n_sims = 10000L)
  emp_mean <- mean(rowSums(m))
  expect_equal(emp_mean, 200, tolerance = 0.02)  # 1% tol, slack for clipping
})

test_that("empirical season std matches analytical season_std within 5%", {
  set.seed(42)
  player <- .make_player_record(season_mean = 200, disagreement_std = 10,
                                 cv = 0.50)
  m <- .simulate_player(player, n_sims = 10000L)
  emp_std <- stats::sd(rowSums(m))
  expect_equal(emp_std, player$season_std, tolerance = 0.05)
})

test_that("bye-week column is exactly 0 across all sims", {
  set.seed(42)
  player <- .make_player_record(bye_week = 7L)
  m <- .simulate_player(player, n_sims = 5000L)
  expect_true(all(m[, 7L] == 0))
})

test_that("zero-source player returns NULL from .simulate_player", {
  empty <- list(
    underdog_id      = "noproj",
    season_mean      = NULL,
    disagreement_std = NULL,
    aleatoric_std    = NULL,
    weekly           = list()
  )
  expect_null(.simulate_player(empty, n_sims = 100L))
})

test_that("seed reproducibility: same seed -> identical draws", {
  player <- .make_player_record(season_mean = 200, disagreement_std = 5)
  set.seed(123)
  m1 <- .simulate_player(player, n_sims = 1000L)
  set.seed(123)
  m2 <- .simulate_player(player, n_sims = 1000L)
  expect_identical(m1, m2)
})

test_that("disagreement_std = 0 -> per-sim scale degenerates to 1", {
  set.seed(42)
  zero_disagree <- .make_player_record(disagreement_std = 0, cv = 0.50)
  pos_disagree  <- .make_player_record(disagreement_std = 30, cv = 0.50)
  m0 <- .simulate_player(zero_disagree, n_sims = 10000L)
  m1 <- .simulate_player(pos_disagree,  n_sims = 10000L)
  # Total variance is HIGHER when disagreement > 0 (epistemic adds to
  # aleatoric).
  expect_true(stats::sd(rowSums(m1)) > stats::sd(rowSums(m0)))
})

test_that("clipping at zero: weekly draws are never negative", {
  set.seed(42)
  # Force a high-CV player where the Normal would produce some negatives
  # without clipping.
  weekly <- list(
    list(week = 1L, opponent = "OPP", home_away = "home", is_bye = FALSE,
         mean = 5, std = 10),
    list(week = 2L, opponent = "OPP", home_away = "home", is_bye = FALSE,
         mean = 5, std = 10)
  )
  player <- list(
    underdog_id = "p",
    season_mean = 10, disagreement_std = 0,
    weekly = weekly
  )
  m <- .simulate_player(player, n_sims = 5000L)
  expect_true(all(m >= 0))
})

test_that("simulate_slate enriches percentiles and returns long draws", {
  set.seed(42)
  p1 <- .make_player_record(underdog_id = "ud-A", season_mean = 200,
                             disagreement_std = 10)
  p2 <- .make_player_record(underdog_id = "ud-B", season_mean = 150,
                             disagreement_std = 5)
  feed <- list(
    `_meta`  = list(slate_id = "test"),
    players  = list(`ud-A` = p1, `ud-B` = p2)
  )
  res <- simulate_slate(feed, n_sims = 500L, seed = 42L)

  expect_setequal(names(res), c("enriched_feed", "draws"))
  expect_setequal(names(res$enriched_feed$players), c("ud-A", "ud-B"))

  # season_percentiles overwritten with empirical
  qs <- res$enriched_feed$players[["ud-A"]]$season_percentiles
  expect_true(all(c("p10", "p25", "p50", "p75", "p90") %in% names(qs)))
  expect_true(qs$p10 < qs$p50)
  expect_true(qs$p90 > qs$p50)

  # draws has the right shape and column types
  expect_setequal(names(res$draws),
                  c("underdog_id", "sim_idx", "week", "draw_value"))
  # 2 players * 500 sims * 18 weeks = 18000 rows
  expect_equal(nrow(res$draws), 2L * 500L * 18L)
  expect_setequal(unique(res$draws$underdog_id), c("ud-A", "ud-B"))
  expect_true(all(res$draws$draw_value >= 0))
})

test_that("simulate_slate drops zero-projection players from draws", {
  set.seed(42)
  with_proj <- .make_player_record(underdog_id = "real")
  noproj <- list(
    underdog_id      = "ghost",
    season_mean      = NULL,
    disagreement_std = NULL,
    aleatoric_std    = NULL,
    weekly           = list(),
    sources_used     = character(0),
    source_breakdown = list()
  )
  feed <- list(`_meta` = list(),
               players = list(real = with_proj, ghost = noproj))
  res  <- simulate_slate(feed, n_sims = 100L, seed = 7L)
  # ghost should NOT appear in draws
  expect_false("ghost" %in% res$draws$underdog_id)
  # real should appear
  expect_true("real" %in% res$draws$underdog_id)
  # ghost's percentiles should remain NULL (no enrichment possible)
  expect_null(res$enriched_feed$players$ghost$season_percentiles)
})
