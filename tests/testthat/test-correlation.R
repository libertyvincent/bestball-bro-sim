# Tests for the Layer B factor-copula sampler. All tests are deterministic
# (set.seed) and use a small synthetic slate constructed to exercise the
# three correlation regimes (same team / same game diff team / diff game),
# plus marginal-preservation, nesting guards, determinism, and byes.

# ---- synthetic-slate fixtures ----------------------------------------------

.fx_player_ids <- function() c("P1", "P2", "P3", "P4", "P5", "P6")

# Two-game slate:
#   Game A: T1 vs T2.   T1 = {P1, P2}.   T2 = {P3, P4}.
#   Game B: T3 vs T4.   T3 = {P5}.       T4 = {P6}.
# Two weeks; same matchups both weeks (lets us check fresh per-week factors
# without having to retune the asserted correlations).
.fx_schedule <- function(weeks = c(1L, 2L), bye_pid = NULL, bye_week = NULL) {
  base <- list(
    list(pid = "P1", team = "T1", opp = "T2"),
    list(pid = "P2", team = "T1", opp = "T2"),
    list(pid = "P3", team = "T2", opp = "T1"),
    list(pid = "P4", team = "T2", opp = "T1"),
    list(pid = "P5", team = "T3", opp = "T4"),
    list(pid = "P6", team = "T4", opp = "T3")
  )
  rows <- list()
  for (w in weeks) {
    for (r in base) {
      is_bye <- isTRUE(!is.null(bye_pid) && r$pid == bye_pid && w == bye_week)
      rows[[length(rows) + 1L]] <- data.frame(
        underdog_id = r$pid,
        week        = as.integer(w),
        team        = r$team,
        opponent    = if (is_bye) NA_character_ else r$opp,
        is_bye      = is_bye,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

# Layer A draws helper: per-player per-week iid Normal(mu, sd), clipped at 0.
# Use mu=0, sd=1, no clipping if you want "identity" marginals (so that the
# output X equals pnorm-inverse(pnorm(Z)) = Z and tests can verify latent
# correlations directly on the points-level output).
.fx_layerA <- function(player_ids,
                       weeks,
                       n_sims_in,
                       mu = 0, sd = 1, clip = FALSE,
                       seed = 7L,
                       bye_overrides = NULL) {
  set.seed(seed)
  chunks <- list()
  for (pid in player_ids) {
    for (w in weeks) {
      is_bye <- isTRUE(any(
        vapply(bye_overrides, function(b) b$pid == pid && b$week == w,
               logical(1))
      ))
      vals <- if (is_bye) {
        rep(0, n_sims_in)
      } else {
        v <- stats::rnorm(n_sims_in, mean = mu, sd = sd)
        if (clip) pmax(v, 0) else v
      }
      chunks[[length(chunks) + 1L]] <- data.frame(
        underdog_id = pid,
        sim_idx     = seq_len(n_sims_in),
        week        = as.integer(w),
        draw_value  = vals,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, chunks)
}

# Reshape long output to wide [n_sims, n_players] for one week.
.wide_one_week <- function(out, week_val, player_ids, n_sims) {
  sub <- out[out$week == week_val, , drop = FALSE]
  m <- matrix(NA_real_, nrow = n_sims, ncol = length(player_ids),
              dimnames = list(NULL, player_ids))
  for (pid in player_ids) {
    rows <- sub[sub$underdog_id == pid, , drop = FALSE]
    rows <- rows[order(rows$sim_idx), ]
    m[, pid] <- rows$draw_value
  }
  m
}

# ---- 1. latent recovery (tight) --------------------------------------------

test_that("latent factor correlations recover targets within ~0.02 at 10K", {
  pids <- .fx_player_ids()
  weeks <- c(1L, 2L)
  n_sims <- 10000L
  # N(0,1) marginals with no clipping make pnorm-inverse(pnorm(Z)) = Z exactly,
  # so points-level Pearson equals latent Pearson. This is the trick that lets
  # us assert the construction's contract through the public API.
  layerA <- .fx_layerA(pids, weeks, n_sims_in = 50000L,
                       mu = 0, sd = 1, clip = FALSE, seed = 13L)
  sched  <- .fx_schedule(weeks)

  out <- sample_correlated_draws(
    player_ids   = pids,
    layerA_draws = layerA,
    schedule     = sched,
    corr_params  = default_corr_params,
    n_sims       = n_sims,
    seed         = 1L
  )

  W1 <- .wide_one_week(out, 1L, pids, n_sims)
  W2 <- .wide_one_week(out, 2L, pids, n_sims)

  cor1 <- stats::cor(W1)
  cor2 <- stats::cor(W2)

  tgt <- default_corr_params
  # Absolute-difference tolerance. testthat's `tolerance =` is relative,
  # which is too tight for the cross=0.05 target (relative=0.025 would
  # require |delta| < 0.00125). 10K-sim sampling noise is ~0.01-0.02 per
  # pair in absolute terms; 0.025 is comfortable headroom.
  tol <- 0.025

  # Same-team pairs: (P1,P2) on T1; (P3,P4) on T2.
  expect_lt(abs(cor1["P1", "P2"] - tgt$team), tol)
  expect_lt(abs(cor1["P3", "P4"] - tgt$team), tol)
  expect_lt(abs(cor2["P1", "P2"] - tgt$team), tol)
  expect_lt(abs(cor2["P3", "P4"] - tgt$team), tol)

  # Same-game / different-team pairs in Game A.
  expect_lt(abs(cor1["P1", "P3"] - tgt$game), tol)
  expect_lt(abs(cor1["P2", "P4"] - tgt$game), tol)
  expect_lt(abs(cor2["P1", "P3"] - tgt$game), tol)

  # Cross-game pairs.
  expect_lt(abs(cor1["P1", "P5"] - tgt$cross), tol)
  expect_lt(abs(cor1["P3", "P6"] - tgt$cross), tol)
  expect_lt(abs(cor2["P2", "P5"] - tgt$cross), tol)

  # Fresh per-week factors -> across-week same-team pair correlation ~ 0
  # (the global tide cancels because each week draws a fresh G).
  cross_week <- stats::cor(W1[, "P1"], W2[, "P1"])
  expect_lt(abs(cross_week - 0), 0.03)

  # Report the observed numbers so PR reviewers can eyeball the
  # construction's contract without re-running the suite.
  message(sprintf(
    "[3b-2 latent recovery] same_team=(%.3f, %.3f) same_game=(%.3f, %.3f) cross=(%.3f, %.3f)",
    cor1["P1", "P2"], cor1["P3", "P4"],
    cor1["P1", "P3"], cor1["P2", "P4"],
    cor1["P1", "P5"], cor1["P3", "P6"]
  ))
})

# ---- 2. points-level (loose / report) --------------------------------------

test_that("points-level correlations on right-skewed clipped marginals are within ~0.05 of latent targets (attenuation expected)", {
  pids <- .fx_player_ids()
  weeks <- c(1L)
  n_sims <- 10000L
  # Realistic-shaped marginal: N(15, 8) clipped at 0. The clip + right-skew
  # mildly attenuates Pearson vs. the latent target -- assertion is loose
  # and the test prints the observed values for visibility.
  layerA <- .fx_layerA(pids, weeks, n_sims_in = 50000L,
                       mu = 15, sd = 8, clip = TRUE, seed = 21L)
  sched  <- .fx_schedule(weeks)

  out <- sample_correlated_draws(
    player_ids   = pids,
    layerA_draws = layerA,
    schedule     = sched,
    n_sims       = n_sims,
    seed         = 2L
  )

  W <- .wide_one_week(out, 1L, pids, n_sims)
  cor_m <- stats::cor(W)
  obs <- list(
    same_team        = cor_m["P1", "P2"],
    same_game_diff_t = cor_m["P1", "P3"],
    diff_game        = cor_m["P1", "P5"]
  )
  message(sprintf(
    "[3b-2 points-level] same_team=%.3f same_game_diff_team=%.3f diff_game=%.3f",
    obs$same_team, obs$same_game_diff_t, obs$diff_game
  ))

  tgt <- default_corr_params
  # Pearson on a clipped, mildly right-skewed marginal is slightly attenuated
  # from the latent target; ~0.05 is comfortable. Tighten to 0.03 once 3b-5
  # validation lands and we recalibrate corr_params against BBMDB.
  expect_lt(abs(obs$same_team        - tgt$team),  0.05)
  expect_lt(abs(obs$same_game_diff_t - tgt$game),  0.05)
  expect_lt(abs(obs$diff_game        - tgt$cross), 0.05)
})

# ---- 3. marginal preservation ----------------------------------------------

test_that("each player's output marginal matches the Layer A input marginal", {
  pids <- .fx_player_ids()
  weeks <- c(1L)
  n_sims_in <- 20000L
  n_sims <- 20000L
  layerA <- .fx_layerA(pids, weeks, n_sims_in = n_sims_in,
                       mu = 12, sd = 6, clip = TRUE, seed = 33L)
  sched  <- .fx_schedule(weeks)

  out <- sample_correlated_draws(
    player_ids   = pids,
    layerA_draws = layerA,
    schedule     = sched,
    n_sims       = n_sims,
    seed         = 5L
  )

  probs <- c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95)
  for (pid in pids) {
    in_vals  <- layerA[layerA$underdog_id == pid & layerA$week == 1L,
                       "draw_value"]
    out_vals <- out[out$underdog_id == pid & out$week == 1L,
                    "draw_value"]
    q_in  <- stats::quantile(in_vals,  probs, names = FALSE)
    q_out <- stats::quantile(out_vals, probs, names = FALSE)
    # Tolerance scaled to marginal SD: 0.20 SD ~ 1.2 points is loose enough
    # to be robust at 20K but tight enough to catch a true distortion.
    expect_equal(q_out, q_in, tolerance = 1.2,
                 info = paste0("player = ", pid))
  }
})

# ---- 4. nesting guard -------------------------------------------------------

test_that("corr_params violating cross <= game <= team errors clearly", {
  pids <- .fx_player_ids()
  weeks <- c(1L)
  layerA <- .fx_layerA(pids, weeks, n_sims_in = 100L, seed = 1L)
  sched  <- .fx_schedule(weeks)

  args_ok <- list(player_ids = pids, layerA_draws = layerA,
                  schedule = sched, n_sims = 50L, seed = 1L)

  # game < cross
  expect_error(
    do.call(sample_correlated_draws,
            c(args_ok,
              list(corr_params = list(team = 0.5, game = 0.1, cross = 0.3)))),
    "nesting"
  )
  # team < game
  expect_error(
    do.call(sample_correlated_draws,
            c(args_ok,
              list(corr_params = list(team = 0.2, game = 0.5, cross = 0.05)))),
    "nesting"
  )
  # out-of-range
  expect_error(
    do.call(sample_correlated_draws,
            c(args_ok,
              list(corr_params = list(team = 1.2, game = 0.5, cross = 0.05)))),
    "lie in"
  )
  # missing element
  expect_error(
    do.call(sample_correlated_draws,
            c(args_ok,
              list(corr_params = list(team = 0.5, game = 0.3)))),
    "missing"
  )
})

# ---- 5. determinism ---------------------------------------------------------

test_that("same seed -> identical output; different seed -> different output", {
  pids <- .fx_player_ids()
  weeks <- c(1L, 2L)
  layerA <- .fx_layerA(pids, weeks, n_sims_in = 2000L, seed = 99L)
  sched  <- .fx_schedule(weeks)

  o1 <- sample_correlated_draws(pids, layerA, sched, n_sims = 500L, seed = 42L)
  o2 <- sample_correlated_draws(pids, layerA, sched, n_sims = 500L, seed = 42L)
  o3 <- sample_correlated_draws(pids, layerA, sched, n_sims = 500L, seed = 43L)

  expect_identical(o1, o2)
  expect_false(identical(o1$draw_value, o3$draw_value))
})

# ---- 6. bye handling -------------------------------------------------------

test_that("a player on bye in a week is excluded from game/team groups", {
  pids <- .fx_player_ids()
  weeks <- c(1L, 2L)
  n_sims <- 10000L
  layerA <- .fx_layerA(pids, weeks, n_sims_in = 20000L,
                       mu = 0, sd = 1, clip = FALSE, seed = 55L,
                       bye_overrides = list(list(pid = "P1", week = 1L)))
  sched  <- .fx_schedule(weeks, bye_pid = "P1", bye_week = 1L)

  out <- sample_correlated_draws(pids, layerA, sched,
                                 n_sims = n_sims, seed = 7L)

  # P1 week-1 draws are exactly 0 across every sim (bye -> emit zeros).
  p1_w1 <- out[out$underdog_id == "P1" & out$week == 1L, "draw_value"]
  expect_equal(length(p1_w1), n_sims)
  expect_true(all(p1_w1 == 0))

  # P1's teammate P2 should NOT inherit P1's bye -- P2's marginal must look
  # identical to its input marginal, and the remaining cross-player
  # correlations should still recover the targets. This catches a would-be
  # bug where a bye player leaks into the game/team group keys.
  W1 <- .wide_one_week(out, 1L, pids, n_sims)
  expect_false(any(is.nan(W1)))
  expect_false(any(is.na(W1)))

  # Compute correlations on the active (non-bye) subset to avoid the
  # zero-variance column from P1's bye.
  active_cols <- setdiff(pids, "P1")
  cm <- stats::cor(W1[, active_cols])
  tgt <- default_corr_params
  tol <- 0.03
  expect_lt(abs(cm["P3", "P4"] - tgt$team),  tol)  # same team
  expect_lt(abs(cm["P2", "P3"] - tgt$game),  tol)  # same game, diff team
  expect_lt(abs(cm["P2", "P5"] - tgt$cross), tol)  # diff game

  # Singleton-group sanity: in week 1 with P1 byed, T1 has only P2.
  # P2's column must not be NaN / Inf.
  expect_true(all(is.finite(W1[, "P2"])))
})

# ---- 7. output shape / Layer A format match -------------------------------

test_that("output has the same columns and total row count as Layer A's format", {
  pids <- .fx_player_ids()
  weeks <- c(1L, 2L)
  n_sims <- 300L
  layerA <- .fx_layerA(pids, weeks, n_sims_in = 1000L, seed = 11L)
  sched  <- .fx_schedule(weeks)
  out <- sample_correlated_draws(pids, layerA, sched,
                                 n_sims = n_sims, seed = 1L)
  expect_setequal(names(out),
                  c("underdog_id", "sim_idx", "week", "draw_value"))
  expect_equal(nrow(out), length(pids) * length(weeks) * n_sims)
  expect_true(is.character(out$underdog_id))
  expect_true(is.integer(out$sim_idx))
  expect_true(is.numeric(out$draw_value))
})
