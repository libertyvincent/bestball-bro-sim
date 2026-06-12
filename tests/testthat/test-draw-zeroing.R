# Draw-level availability (game-zeroing): the no-double-discount gate (G3),
# the missed-week mask mechanics, and the Stage-2 tensor mask. Pre-registered
# in DRAW_ZEROING_DESIGN.md (§2/§3/§8).

# Synthetic player: `season_mean` spread over (n_weeks - 1) active weeks plus
# one bye, with per-week std = cv * weekly_mean and no source disagreement
# (pure aleatoric, so the masked mean is clean to assert).
.dz_player <- function(season_mean, p_miss, cv = 0.50, bye = 8L,
                       n_weeks = 18L, position = "RB", uid = "p") {
  per_act <- season_mean / (n_weeks - 1L)
  weekly  <- vector("list", n_weeks)
  for (w in seq_len(n_weeks)) {
    if (w == bye) {
      weekly[[w]] <- list(week = w, is_bye = TRUE, mean = 0, std = 0)
    } else {
      weekly[[w]] <- list(week = w, is_bye = FALSE,
                          mean = per_act, std = cv * per_act)
    }
  }
  list(underdog_id = uid, position = position, season_mean = season_mean,
       disagreement_std = 0,
       aleatoric_std = sqrt((n_weeks - 1L) * (cv * per_act)^2),
       availability_p_miss = p_miss, weekly = weekly)
}

# ---- G3: no double discount ------------------------------------------------

test_that("G3: 17-game RB masks exactly once -> u*q (not u, not u*q^2)", {
  set.seed(1)
  u <- 200; q <- 15.2 / 17; pm <- 1 - q
  p    <- .dz_player(u, pm)
  cond <- .simulate_player(p, 20000L)
  mask <- .sample_missed_week_mask(p, 20000L, q)
  em   <- mean(rowSums(cond * mask))

  expect_equal(em, u * q, tolerance = 0.02)                # ~178.8 (single discount)
  expect_false(isTRUE(all.equal(em, u,       tolerance = 0.02)))   # not 200 (factor gone)
  expect_false(isTRUE(all.equal(em, u * q^2, tolerance = 0.02)))   # not 159.9 (double)
})

test_that("G3: sub-prior player (clay_games=12) is NOT discounted", {
  set.seed(3)
  u  <- 120
  pm <- .availability_p_miss(12, 15.2)   # 15.2/12 > 1 -> q clamps to 1 -> p_miss 0
  expect_equal(pm, 0)
  p    <- .dz_player(u, pm)
  cond <- .simulate_player(p, 20000L)
  mask <- .sample_missed_week_mask(p, 20000L, 1 - pm)
  em   <- mean(rowSums(cond * mask))
  expect_equal(em, u, tolerance = 0.02)  # no second discount; not u*15.2/17 = 107.3
})

test_that("G3: QB-rate masks once at low p_miss (the G5-blind regime)", {
  set.seed(2)
  u <- 300; q <- 16.8 / 17; pm <- 1 - q   # p_miss = 0.0118
  p    <- .dz_player(u, pm, cv = 0.32, position = "QB", uid = "qb")
  cond <- .simulate_player(p, 40000L)
  mask <- .sample_missed_week_mask(p, 40000L, q)
  em   <- mean(rowSums(cond * mask))
  expect_equal(em, u * q, tolerance = 0.01)                  # ~296.5
  expect_false(isTRUE(all.equal(em, u, tolerance = 0.005)))  # distinguishable from 300
})

# ---- missed-week mask mechanics --------------------------------------------

test_that("mask: byes are never missed; active weeks kept at rate q", {
  set.seed(4)
  q <- 15.2 / 17
  p    <- .dz_player(170, 1 - q, bye = 5L)
  mask <- .sample_missed_week_mask(p, 100000L, q)
  expect_true(all(mask[, 5L] == 1))                  # bye column never masked
  expect_equal(mean(mask[, 1L]), q, tolerance = 0.01)  # active week kept-rate ~ q
})

test_that("mask: q >= 1 (p_miss = 0) short-circuits to all-ones", {
  p    <- .dz_player(170, 0)
  mask <- .sample_missed_week_mask(p, 1000L, 1)
  expect_true(all(mask == 1))
})

test_that("expected played weeks over the 17 active weeks ~ prior", {
  set.seed(5)
  q <- 15.2 / 17
  p    <- .dz_player(170, 1 - q, bye = 8L)         # 18 weeks, 1 bye -> 17 active
  mask <- .sample_missed_week_mask(p, 50000L, q)
  played <- rowSums(mask[, setdiff(seq_len(18L), 8L)])
  expect_equal(mean(played), 15.2, tolerance = 0.05)  # 17 * q = 15.2
})

# ---- Stage-2 tensor mask ----------------------------------------------------

test_that(".availability_q_from_feed reads per-player p_miss, defaults to 1", {
  feed <- list(players = list(
    a = list(underdog_id = "a", availability_p_miss = 0.1059),
    b = list(underdog_id = "b")  # no rate -> default q = 1
  ))
  q <- .availability_q_from_feed(feed, c("a", "b", "c"))
  expect_equal(unname(q[["a"]]), 1 - 0.1059)
  expect_equal(unname(q[["b"]]), 1)
  expect_equal(unname(q[["c"]]), 1)  # absent from feed
})

test_that(".apply_availability_mask zeroes ~p_miss of cells, reproducibly", {
  feed <- list(players = list(a = list(underdog_id = "a",
                                       availability_p_miss = 0.5)))
  ml <- list(`1` = matrix(10, nrow = 1L, ncol = 10000L,
                          dimnames = list("a", NULL)))
  m1 <- .apply_availability_mask(ml, feed, "a", seed = 42L)
  m2 <- .apply_availability_mask(ml, feed, "a", seed = 42L)
  expect_identical(m1, m2)                               # same seed -> same mask
  expect_equal(mean(m1[["1"]]["a", ] == 0), 0.5, tolerance = 0.03)  # ~p_miss zeroed
})

test_that(".apply_availability_mask is a no-op when all q >= 1", {
  feed <- list(players = list(a = list(underdog_id = "a")))  # no rate
  ml <- list(`1` = matrix(7, nrow = 1L, ncol = 50L, dimnames = list("a", NULL)))
  expect_identical(.apply_availability_mask(ml, feed, "a", seed = 1L), ml)
})
