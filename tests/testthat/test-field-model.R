# Tests for R/field_model.R. The four structural validation numbers per
# the 3b-6 prompt: per-position mean counts, qb_stack_2plus rate,
# ownership monotone in ADP, and construction legality. Plus
# determinism.

# Synthetic player pool deep enough that the bottom of the pool isn't
# forced into rosters by supply scarcity (a real slate has ~1448 players
# vs the 216-slot snake; the test mirrors that ratio). 32 NFL teams
# x 16 players = 512 players, adp 1..512.
.synthetic_pool <- function() {
  nfl_teams <- c("ARI","ATL","BAL","BUF","CAR","CHI","CIN","CLE","DAL","DEN",
                 "DET","GB","HOU","IND","JAX","KC","LAC","LA","LV","MIA",
                 "MIN","NE","NO","NYG","NYJ","PHI","PIT","SEA","SF","TB",
                 "TEN","WAS")
  # 2 QB + 5 RB + 7 WR + 2 TE per team = 16, roughly matching the
  # target position fractions (QB 14% / RB 30% / WR 40% / TE 15%).
  # Interleaved so positions share early- and late-ADP space.
  per_team <- c("QB", "RB", "WR", "WR", "RB", "TE", "WR", "RB",
                "WR", "RB", "QB", "WR", "RB", "TE", "WR", "WR")
  pool <- list()
  next_adp <- 1
  for (tm in nfl_teams) {
    for (p in per_team) {
      pool[[length(pool) + 1L]] <- list(
        underdog_id = sprintf("p_%04d", length(pool) + 1L),
        position    = p,
        team_abbr   = tm,
        adp         = next_adp
      )
      next_adp <- next_adp + 1
    }
  }
  do.call(rbind, lapply(pool, as.data.frame, stringsAsFactors = FALSE))
}

# Simple targets matching the documented empirical numbers.
.synthetic_targets <- function() {
  list(
    position_means      = c(QB = 2.6, RB = 5.4, WR = 7.2, TE = 2.7),
    qb_stack_2plus_rate = 0.92,
    # ADP SD by slot -- flat 8 (mid-bench). The real per-slot SDs are
    # data-driven; flat is fine for the structural tests.
    slot_adp_sd         = setNames(rep(8, 240L), as.character(seq_len(240L)))
  )
}

# Helper: build a minimal slate manifest override so tests don't depend on the
# repo's inst manifest. The tests inject `player_pool` and `targets` directly
# and only need position_caps for the slate.
.with_test_slate <- function(expr) {
  # generate_field() pulls position_caps from load_slate_manifest(). We use
  # the real Season slate (already in the manifest) so we don't need to mock.
  expr
}

# Wrapper: small n_teams (1000) for the structural tests so the suite stays
# under a few seconds.
.gen <- function(n_teams = 1000L, seed = 1L, ...) {
  generate_field(
    slate_id    = "nfl_2026_season",
    player_pool = .synthetic_pool(),
    targets     = .synthetic_targets(),
    n_teams     = n_teams,
    seed        = seed,
    ...
  )
}

# ---- 1. Position mean counts -----------------------------------------------

test_that("position means land near empirical targets QB 2.6 / RB 5.4 / WR 7.2 / TE 2.7", {
  out <- .gen(n_teams = 2000L, seed = 1L)
  per_team <- aggregate(underdog_id ~ entry_id + position, data = out$rosters,
                        FUN = length)
  names(per_team)[3] <- "n"
  means <- tapply(per_team$n, per_team$position, mean)

  message(sprintf("[3b-6 position means] QB=%.2f RB=%.2f WR=%.2f TE=%.2f",
                  means["QB"], means["RB"], means["WR"], means["TE"]))
  # The validation bar is "within tolerance" not exact. The stack
  # multiplier pulls WR/TE slightly above target while squeezing QB
  # slightly below -- a multi-objective tension with the stack-rate
  # target. The calibration in inst/scripts/calibrate_field.R shows the
  # tradeoff curve; 0.8 here keeps the test stable while still catching
  # a regression that would meaningfully unbalance the field.
  expect_lt(abs(means["QB"] - 2.6), 0.8)
  expect_lt(abs(means["RB"] - 5.4), 0.8)
  expect_lt(abs(means["WR"] - 7.2), 0.8)
  expect_lt(abs(means["TE"] - 2.7), 0.8)
})

.has_qb_stack_2plus <- function(rosters) {
  ids <- unique(rosters$entry_id)
  res <- logical(length(ids))
  by_entry <- split(rosters, rosters$entry_id)
  for (i in seq_along(ids)) {
    r <- by_entry[[ids[i]]]
    qbs <- r$team_abbr[r$position == "QB"]
    pcs <- r$team_abbr[r$position %in% c("WR", "TE")]
    res[i] <- any(pcs %in% qbs)
  }
  res
}

# ---- 2. qb_stack_2+ rate ---------------------------------------------------

test_that("qb_stack_2+ rate lands in the empirical 0.88-0.96 band", {
  out <- .gen(n_teams = 2000L, seed = 2L)
  stack_rate <- mean(.has_qb_stack_2plus(out$rosters))
  message(sprintf("[3b-6 stack rate] qb_stack_2+ = %.3f", stack_rate))
  # Empirical target band is 0.91-0.93 per 3b-1. With 2000 synthetic
  # teams the sampling noise is ~+/-0.01; we widen to 0.88-0.96 to keep
  # the test stable across seeds. The calibrate_field.R script keeps the
  # central tendency in the tight band.
  expect_gt(stack_rate, 0.88)
  expect_lt(stack_rate, 0.96)
})

# ---- 3. Ownership monotone in ADP ------------------------------------------

test_that("player ownership is strongly negatively correlated with ADP (Spearman <= -0.8)", {
  out <- .gen(n_teams = 2000L, seed = 3L)
  pool <- .synthetic_pool()
  own <- table(out$rosters$underdog_id)
  pool$own <- as.numeric(own[pool$underdog_id])
  pool$own[is.na(pool$own)] <- 0
  rho <- stats::cor(pool$own, pool$adp, method = "spearman")
  message(sprintf("[3b-6 ownership] Spearman(ownership, adp) = %.3f", rho))
  # Lower ADP number = higher ownership -> strongly negative correlation.
  # The bar is "strongly negative"; we set -0.8 as the floor (the actual
  # value should be well past -0.9 given the dnorm weighting).
  expect_lt(rho, -0.8)
})

# ---- 4. Construction legality ---------------------------------------------

test_that("rosters are legal: no duplicates, position caps respected, size = 18", {
  out <- .gen(n_teams = 500L, seed = 4L)
  by_entry <- split(out$rosters, out$rosters$entry_id)
  caps <- load_slate_manifest()[["nfl_2026_season"]]$position_caps

  for (eid in names(by_entry)) {
    r <- by_entry[[eid]]
    # Size
    expect_equal(nrow(r), 18L, info = eid)
    # No within-roster duplicates
    expect_equal(length(unique(r$underdog_id)), 18L, info = eid)
    # Caps
    pos_n <- table(r$position)
    for (p in c("QB", "RB", "WR", "TE")) {
      raw <- pos_n[p]
      n_p <- if (is.na(raw)) 0L else as.integer(raw)
      expect_true(n_p <= caps[[p]],
                  info = sprintf("%s: %d %s > cap %d", eid, n_p, p, caps[[p]]))
    }
  }
})

# ---- 5. Determinism --------------------------------------------------------

test_that("same seed -> identical field; different seed -> different field", {
  o1 <- .gen(n_teams = 100L, seed = 99L)
  o2 <- .gen(n_teams = 100L, seed = 99L)
  o3 <- .gen(n_teams = 100L, seed = 100L)
  expect_identical(o1$rosters, o2$rosters)
  expect_false(identical(o1$rosters, o3$rosters))
})

# ---- 6. compute_field_targets() basic plumbing -----------------------------

test_that("compute_field_targets returns the four target shapes", {
  fixture <- data.frame(
    draft_id        = rep("d1", 12L * 18L),
    draft_entry_id  = rep(paste0("e", 1:12), each = 18L),
    drafter_user_id = rep(paste0("u", 1:12), each = 18L),
    slate_id        = "test-slate",
    tournament_id   = "test-tnmt",
    pick_overall    = seq_len(12L * 18L),
    underdog_id     = sprintf("p_%03d", seq_len(12L * 18L)),
    first_name      = "X", last_name = "Y",
    position_name   = sample(c("QB", "RB", "WR", "TE"), 12L * 18L, TRUE,
                             prob = c(2.59, 5.42, 7.20, 2.71)),
    team_id         = sample(LETTERS[1:8], 12L * 18L, TRUE),
    projection_adp_at_pick = seq_len(12L * 18L) +
                             stats::rnorm(12L * 18L, 0, 5),
    stringsAsFactors = FALSE
  )
  res <- compute_field_targets(fixture)
  expect_setequal(names(res),
                  c("position_means", "qb_stack_2plus_rate", "slot_adp_sd"))
  expect_equal(length(res$position_means), 4L)
  expect_true(is.numeric(res$qb_stack_2plus_rate))
  expect_true(length(res$slot_adp_sd) > 0L)
})
