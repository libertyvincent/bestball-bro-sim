# Tests for R/math_simulator.R. Synthetic mini-slate exercises the
# Layer A -> 3b-2 -> 3b-3 composition end-to-end.

# ---- fixtures --------------------------------------------------------------

.season_spec <- function() {
  list(slate_id = "test-season", slots = list(
    list(pos = "QB",   n = 1L, eligible = c("QB")),
    list(pos = "RB",   n = 2L, eligible = c("RB")),
    list(pos = "WR",   n = 3L, eligible = c("WR")),
    list(pos = "TE",   n = 1L, eligible = c("TE")),
    list(pos = "FLEX", n = 1L, eligible = c("RB", "WR", "TE"))
  ))
}

.superflex_spec <- function() {
  list(slate_id = "test-sflex", slots = list(
    list(pos = "QB",    n = 1L, eligible = c("QB")),
    list(pos = "RB",    n = 2L, eligible = c("RB")),
    list(pos = "WR",    n = 2L, eligible = c("WR")),
    list(pos = "TE",    n = 1L, eligible = c("TE")),
    list(pos = "FLEX",  n = 1L, eligible = c("RB", "WR", "TE")),
    list(pos = "SFLEX", n = 1L, eligible = c("QB", "RB", "WR", "TE"))
  ))
}

# Simple roster builder: positions and team assignments specified explicitly,
# scores drawn from Normal(mu_pos, sd_pos) clipped at 0 per (player, week).
# Returns (positions, schedule, layerA_draws, roster).
.mk_slate <- function(roster_def,        # list of list(pid, pos, team)
                      opp_by_team,        # named char vec: team -> opponent
                      n_weeks  = 2L,
                      n_sims_A = 5000L,
                      seed_A   = 1001L,
                      bye_overrides = NULL) {
  set.seed(seed_A)
  pids <- vapply(roster_def, `[[`, character(1), "pid")
  teams_per <- vapply(roster_def, `[[`, character(1), "team")
  positions <- setNames(
    vapply(roster_def, `[[`, character(1), "pos"),
    pids
  )

  # Schedule: each player plays their team's matchup every week.
  sched_chunks <- list()
  for (i in seq_along(pids)) {
    for (w in seq_len(n_weeks)) {
      is_bye <- isTRUE(any(vapply(bye_overrides,
                                  function(b) b$pid == pids[i] && b$week == w,
                                  logical(1))))
      sched_chunks[[length(sched_chunks) + 1L]] <- data.frame(
        underdog_id = pids[i],
        week        = as.integer(w),
        team        = teams_per[i],
        opponent    = if (is_bye) NA_character_ else opp_by_team[[teams_per[i]]],
        is_bye      = is_bye,
        stringsAsFactors = FALSE
      )
    }
  }
  schedule <- do.call(rbind, sched_chunks)

  # Layer A draws: per (player, week), draw n_sims_A from a position-typical
  # Normal clipped at 0. Bye -> all zeros.
  pos_mu <- c(QB = 18, RB = 13, WR = 11, TE = 8)
  pos_sd <- c(QB = 7,  RB = 6,  WR = 6,  TE = 4)
  la_chunks <- list()
  for (i in seq_along(pids)) {
    p <- positions[pids[i]]
    for (w in seq_len(n_weeks)) {
      is_bye <- isTRUE(any(vapply(bye_overrides,
                                  function(b) b$pid == pids[i] && b$week == w,
                                  logical(1))))
      vals <- if (is_bye) {
        rep(0, n_sims_A)
      } else {
        pmax(0, stats::rnorm(n_sims_A, mean = pos_mu[p], sd = pos_sd[p]))
      }
      la_chunks[[length(la_chunks) + 1L]] <- data.frame(
        underdog_id = pids[i],
        sim_idx     = seq_len(n_sims_A),
        week        = as.integer(w),
        draw_value  = vals,
        stringsAsFactors = FALSE
      )
    }
  }
  layerA_draws <- do.call(rbind, la_chunks)

  list(roster = pids, positions = positions, schedule = schedule,
       layerA_draws = layerA_draws)
}

# Season-spec roster: 2 QB, 4 RB, 5 WR, 2 TE = 13 players (enough to fill
# starters + leftover pool). Mixed teams unless otherwise noted.
.season_roster_def <- function(stacked = FALSE) {
  if (stacked) {
    # Stacked: QB1, WR1, WR2, TE1 all on T1 (QB + 3 pass-catchers).
    list(
      list(pid = "QB1", pos = "QB", team = "T1"),
      list(pid = "QB2", pos = "QB", team = "T5"),
      list(pid = "RB1", pos = "RB", team = "T6"),
      list(pid = "RB2", pos = "RB", team = "T7"),
      list(pid = "RB3", pos = "RB", team = "T8"),
      list(pid = "RB4", pos = "RB", team = "T9"),
      list(pid = "WR1", pos = "WR", team = "T1"),
      list(pid = "WR2", pos = "WR", team = "T1"),
      list(pid = "WR3", pos = "WR", team = "TA"),
      list(pid = "WR4", pos = "WR", team = "TB"),
      list(pid = "WR5", pos = "WR", team = "TC"),
      list(pid = "TE1", pos = "TE", team = "T1"),
      list(pid = "TE2", pos = "TE", team = "TD")
    )
  } else {
    # Unstacked: every player on a different team.
    teams <- paste0("T", LETTERS[1:13])
    pids <- c(paste0("QB", 1:2), paste0("RB", 1:4),
              paste0("WR", 1:5), paste0("TE", 1:2))
    poss <- c(rep("QB", 2), rep("RB", 4), rep("WR", 5), rep("TE", 2))
    lapply(seq_along(pids), function(i) {
      list(pid = pids[i], pos = poss[i], team = teams[i])
    })
  }
}

# Each roster team plays a *non-roster* opponent. This guarantees no two
# roster players share a game (unless they share a team), which is what
# "unstacked" actually means. Stacked rosters still work fine: players on
# the same roster team are co-located in the same game with their shared
# OPP_<team>.
.opp_map_for <- function(roster_def) {
  ts <- unique(vapply(roster_def, `[[`, character(1), "team"))
  setNames(paste0("OPP_", ts), ts)
}

# ---- 1. Reduces to independence -------------------------------------------

test_that("corr=(0,0,0) reduces to the optimizer over independent Layer A draws", {
  rd <- .season_roster_def(stacked = FALSE)
  s  <- .mk_slate(rd, .opp_map_for(rd), n_weeks = 2L, n_sims_A = 8000L,
                  seed_A = 7L)
  spec <- .season_spec()
  zero_corr <- list(team = 0, game = 0, cross = 0)
  n_sims <- 8000L

  # Reference: feed Layer A directly to the optimizer.
  ref <- optimize_lineup_totals(
    scores      = s$layerA_draws,
    positions   = s$positions[s$roster],
    lineup_spec = spec,
    weeks       = NULL
  )
  ref_season <- rowSums(ref)

  # 3b-4 with corr=0.
  out <- simulate_team_season(
    roster       = s$roster,
    positions    = s$positions,
    layerA_draws = s$layerA_draws,
    schedule     = s$schedule,
    lineup_spec  = spec,
    corr_params  = zero_corr,
    n_sims       = n_sims,
    seed         = 42L
  )
  obs_season <- out$season_totals

  # Both should sample from the same season-total distribution. At ~8K sims
  # the means should match within ~1% and the SDs within ~5%.
  rel_mean_diff <- abs(mean(obs_season) - mean(ref_season)) / mean(ref_season)
  rel_sd_diff   <- abs(stats::sd(obs_season) - stats::sd(ref_season)) /
    stats::sd(ref_season)
  message(sprintf("[3b-4 reduces-to-independence] mean_obs=%.2f mean_ref=%.2f rel=%.4f | sd_obs=%.2f sd_ref=%.2f rel=%.4f",
                  mean(obs_season), mean(ref_season), rel_mean_diff,
                  stats::sd(obs_season), stats::sd(ref_season), rel_sd_diff))
  expect_lt(rel_mean_diff, 0.02)
  expect_lt(rel_sd_diff,   0.07)

  # And mid-distribution quantiles should land close.
  q_obs <- stats::quantile(obs_season, c(0.25, 0.50, 0.75), names = FALSE)
  q_ref <- stats::quantile(ref_season, c(0.25, 0.50, 0.75), names = FALSE)
  expect_true(all(abs(q_obs - q_ref) / q_ref < 0.03))
})

# ---- 2. Marginal preservation end-to-end ----------------------------------

test_that("each player's correlated weekly draws match their Layer A marginal", {
  rd <- .season_roster_def(stacked = FALSE)
  s  <- .mk_slate(rd, .opp_map_for(rd), n_weeks = 1L, n_sims_A = 20000L,
                  seed_A = 11L)

  out_draws <- sample_correlated_draws(
    player_ids   = s$roster,
    layerA_draws = s$layerA_draws,
    schedule     = s$schedule,
    corr_params  = default_corr_params,
    n_sims       = 20000L,
    seed         = 99L
  )

  probs <- c(0.05, 0.25, 0.50, 0.75, 0.95)
  for (pid in s$roster) {
    in_vals  <- s$layerA_draws[s$layerA_draws$underdog_id == pid &
                                 s$layerA_draws$week == 1L, "draw_value"]
    out_vals <- out_draws[out_draws$underdog_id == pid &
                            out_draws$week == 1L, "draw_value"]
    q_in  <- stats::quantile(in_vals,  probs, names = FALSE)
    q_out <- stats::quantile(out_vals, probs, names = FALSE)
    expect_equal(q_out, q_in, tolerance = 0.6,
                 info = paste0("marginal preservation, player=", pid))
  }
})

# ---- 3. Correlation does its job ------------------------------------------

test_that("stacked roster -> correlation fattens upper tail; unstacked -> ~unchanged", {
  spec <- .season_spec()
  n_sims <- 10000L
  # Use a stacking-only corr config (cross=0) so the behavioral signal
  # isolates team/game factors. With default_corr_params (cross=0.05) the
  # global G tide adds a K(K-1)/2 pairwise effect across the whole roster
  # that inflates SD even on an unstacked roster -- correct behavior but
  # not what this test is asking about.
  corr_stack_only <- list(team = 0.45, game = 0.25, cross = 0)
  corr_zero       <- list(team = 0, game = 0, cross = 0)

  # STACKED: QB1 + WR1 + WR2 + TE1 all on T1.
  rd_s <- .season_roster_def(stacked = TRUE)
  ss   <- .mk_slate(rd_s, .opp_map_for(rd_s), n_weeks = 4L,
                    n_sims_A = 15000L, seed_A = 17L)
  out_s_corr <- simulate_team_season(
    roster = ss$roster, positions = ss$positions,
    layerA_draws = ss$layerA_draws, schedule = ss$schedule,
    lineup_spec = spec, corr_params = corr_stack_only,
    n_sims = n_sims, seed = 71L
  )
  out_s_zero <- simulate_team_season(
    roster = ss$roster, positions = ss$positions,
    layerA_draws = ss$layerA_draws, schedule = ss$schedule,
    lineup_spec = spec, corr_params = corr_zero,
    n_sims = n_sims, seed = 71L
  )

  sd_ratio_stacked <- stats::sd(out_s_corr$season_totals) /
    stats::sd(out_s_zero$season_totals)
  p90_diff_stacked <- stats::quantile(out_s_corr$season_totals, 0.90,
                                      names = FALSE) -
    stats::quantile(out_s_zero$season_totals, 0.90, names = FALSE)
  message(sprintf("[3b-4 stacked]   sd_corr/sd_zero=%.3f  p90_diff=%.2f",
                  sd_ratio_stacked, p90_diff_stacked))
  expect_gt(sd_ratio_stacked, 1.05)   # stack should fatten by >= 5%
  expect_gt(p90_diff_stacked, 0)       # and lift the upper tail

  # UNSTACKED: every player on a unique team. With cross=0 the team and
  # game factors find no shared groups, so loadings collapse to idio --
  # the season distribution should be essentially identical to the
  # zero-corr baseline.
  rd_u <- .season_roster_def(stacked = FALSE)
  us   <- .mk_slate(rd_u, .opp_map_for(rd_u), n_weeks = 4L,
                    n_sims_A = 15000L, seed_A = 23L)
  out_u_corr <- simulate_team_season(
    roster = us$roster, positions = us$positions,
    layerA_draws = us$layerA_draws, schedule = us$schedule,
    lineup_spec = spec, corr_params = corr_stack_only,
    n_sims = n_sims, seed = 73L
  )
  out_u_zero <- simulate_team_season(
    roster = us$roster, positions = us$positions,
    layerA_draws = us$layerA_draws, schedule = us$schedule,
    lineup_spec = spec, corr_params = corr_zero,
    n_sims = n_sims, seed = 73L
  )
  sd_ratio_unstacked <- stats::sd(out_u_corr$season_totals) /
    stats::sd(out_u_zero$season_totals)
  message(sprintf("[3b-4 unstacked] sd_corr/sd_zero=%.3f", sd_ratio_unstacked))
  expect_lt(abs(sd_ratio_unstacked - 1), 0.05)   # essentially unchanged

  # Stacking's SD bump should exceed the no-stack SD bump.
  expect_gt(sd_ratio_stacked, sd_ratio_unstacked)
})

# ---- 4. Spec-driven Season vs Superflex -----------------------------------

test_that("Season vs Superflex lineup spec pulled through without code change", {
  # 3-QB roster lets Superflex actually use SFLEX.
  rd <- list(
    list(pid = "QB1", pos = "QB", team = "T1"),
    list(pid = "QB2", pos = "QB", team = "T3"),
    list(pid = "QB3", pos = "QB", team = "T5"),
    list(pid = "RB1", pos = "RB", team = "T7"),
    list(pid = "RB2", pos = "RB", team = "T9"),
    list(pid = "RB3", pos = "RB", team = "TB"),
    list(pid = "WR1", pos = "WR", team = "TD"),
    list(pid = "WR2", pos = "WR", team = "TF"),
    list(pid = "WR3", pos = "WR", team = "TH"),
    list(pid = "WR4", pos = "WR", team = "TJ"),
    list(pid = "TE1", pos = "TE", team = "TL"),
    list(pid = "TE2", pos = "TE", team = "TN")
  )
  s <- .mk_slate(rd, .opp_map_for(rd), n_weeks = 1L, n_sims_A = 5000L,
                 seed_A = 31L)

  out_season <- simulate_team_season(
    roster = s$roster, positions = s$positions,
    layerA_draws = s$layerA_draws, schedule = s$schedule,
    lineup_spec = .season_spec(), n_sims = 5000L, seed = 91L
  )
  out_sflex <- simulate_team_season(
    roster = s$roster, positions = s$positions,
    layerA_draws = s$layerA_draws, schedule = s$schedule,
    lineup_spec = .superflex_spec(), n_sims = 5000L, seed = 91L
  )

  # Superflex starts an extra player (the SFLEX slot), so its mean season
  # total must exceed Season's by a clear margin.
  expect_gt(mean(out_sflex$season_totals), mean(out_season$season_totals))
  # And it changes the variance profile non-trivially.
  expect_true(stats::sd(out_sflex$season_totals) !=
                stats::sd(out_season$season_totals))
})

# ---- 5. Determinism --------------------------------------------------------

test_that("same seed -> identical output; batch seeding reproducible", {
  rd <- .season_roster_def(stacked = FALSE)
  s  <- .mk_slate(rd, .opp_map_for(rd), n_weeks = 1L, n_sims_A = 1000L,
                  seed_A = 5L)
  spec <- .season_spec()

  o1 <- simulate_team_season(s$roster, s$positions, s$layerA_draws,
                             s$schedule, lineup_spec = spec,
                             n_sims = 500L, seed = 42L)
  o2 <- simulate_team_season(s$roster, s$positions, s$layerA_draws,
                             s$schedule, lineup_spec = spec,
                             n_sims = 500L, seed = 42L)
  o3 <- simulate_team_season(s$roster, s$positions, s$layerA_draws,
                             s$schedule, lineup_spec = spec,
                             n_sims = 500L, seed = 43L)
  expect_identical(o1$season_totals, o2$season_totals)
  expect_false(identical(o1$season_totals, o3$season_totals))

  # Batch: deterministic across base_seed for the same roster ordering.
  rosters <- list(A = s$roster, B = s$roster, C = s$roster)
  b1 <- simulate_teams(rosters, s$positions, s$layerA_draws, s$schedule,
                       lineup_spec = spec, n_sims = 500L,
                       base_seed = 10L, summarize_only = TRUE)
  b2 <- simulate_teams(rosters, s$positions, s$layerA_draws, s$schedule,
                       lineup_spec = spec, n_sims = 500L,
                       base_seed = 10L, summarize_only = TRUE)
  expect_identical(b1$season_totals_by_team, b2$season_totals_by_team)

  # And per-team seed independence: distinct teams produce distinct draws.
  expect_false(identical(b1$season_totals_by_team[, "A"],
                         b1$season_totals_by_team[, "B"]))
})

# ---- 6. Calls 3b-2 / 3b-3 (public functions only) -------------------------

test_that("output agrees with a hand-composed call to 3b-2 + 3b-3", {
  rd <- .season_roster_def(stacked = FALSE)
  s  <- .mk_slate(rd, .opp_map_for(rd), n_weeks = 1L, n_sims_A = 2000L,
                  seed_A = 13L)
  spec <- .season_spec()
  n_sims <- 1000L

  # Hand-compose what 3b-4 should do:
  corr_draws <- sample_correlated_draws(
    player_ids   = s$roster,
    layerA_draws = s$layerA_draws,
    schedule     = s$schedule,
    corr_params  = default_corr_params,
    n_sims       = n_sims,
    seed         = 555L
  )
  hand_weekly <- optimize_lineup_totals(
    scores = corr_draws, positions = s$positions[s$roster],
    lineup_spec = spec
  )
  hand_season <- rowSums(hand_weekly)

  out <- simulate_team_season(
    roster = s$roster, positions = s$positions,
    layerA_draws = s$layerA_draws, schedule = s$schedule,
    lineup_spec = spec, corr_params = default_corr_params,
    n_sims = n_sims, seed = 555L
  )

  expect_equal(out$weekly_totals, hand_weekly, ignore_attr = FALSE)
  expect_equal(out$season_totals, hand_season)
})

# ---- 7. Batch wrapper basic shape -----------------------------------------

test_that("simulate_teams returns expected shapes for both summarize modes", {
  rd <- .season_roster_def(stacked = FALSE)
  s  <- .mk_slate(rd, .opp_map_for(rd), n_weeks = 1L, n_sims_A = 1500L,
                  seed_A = 19L)
  spec <- .season_spec()
  rosters <- list(t1 = s$roster, t2 = s$roster)

  full <- simulate_teams(rosters, s$positions, s$layerA_draws, s$schedule,
                         lineup_spec = spec, n_sims = 500L, base_seed = 1L,
                         summarize_only = FALSE)
  expect_setequal(names(full), c("t1", "t2"))
  expect_true(all(c("weekly_totals", "season_totals", "summary") %in%
                    names(full$t1)))
  expect_equal(length(full$t1$season_totals), 500L)

  summ <- simulate_teams(rosters, s$positions, s$layerA_draws, s$schedule,
                         lineup_spec = spec, n_sims = 500L, base_seed = 1L,
                         summarize_only = TRUE)
  expect_setequal(names(summ), c("summaries", "season_totals_by_team"))
  expect_equal(nrow(summ$summaries), 2L)
  expect_equal(dim(summ$season_totals_by_team), c(500L, 2L))
  expect_setequal(summ$summaries$team_id, c("t1", "t2"))
})

# ---- 8. precompute_layerA_marginals integration ---------------------------

test_that("precomputed marginals produce identical draws to the no-cache path", {
  rd <- .season_roster_def(stacked = FALSE)
  s  <- .mk_slate(rd, .opp_map_for(rd), n_weeks = 2L, n_sims_A = 1000L,
                  seed_A = 27L)

  m <- precompute_layerA_marginals(s$layerA_draws,
                                   player_ids = s$roster,
                                   weeks      = c(1L, 2L))

  d_no_cache <- sample_correlated_draws(
    player_ids = s$roster, layerA_draws = s$layerA_draws,
    schedule = s$schedule, n_sims = 500L, seed = 8L
  )
  d_cached <- sample_correlated_draws(
    player_ids = s$roster, layerA_draws = s$layerA_draws,
    schedule = s$schedule, n_sims = 500L, seed = 8L,
    precomputed_marginals = m
  )
  expect_identical(d_no_cache, d_cached)
})
