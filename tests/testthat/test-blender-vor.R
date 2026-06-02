# Slate-aware VOR: replacement ranks derive from each slate's starting
# lineup (.replacement_ranks_from_lineup) instead of a hardcoded 1-QB
# constant, and .add_position_metrics consumes them.
#
# The two gates:
#   1. 1-QB lineups derive exactly the v1 constants (QB12/RB24/WR36/TE12),
#      so Season / Eliminator / Weekly Winners vor is unchanged.
#   2. Superflex derives QB24, so QB vor reflects ~2 startable QBs.

# ---- fixtures ---------------------------------------------------------------

.standard_lineup <- function() {
  list(slate_id = "test-season", slots = list(
    list(pos = "QB",   n = 1L, eligible = c("QB")),
    list(pos = "RB",   n = 2L, eligible = c("RB")),
    list(pos = "WR",   n = 3L, eligible = c("WR")),
    list(pos = "TE",   n = 1L, eligible = c("TE")),
    list(pos = "FLEX", n = 1L, eligible = c("RB", "WR", "TE"))
  ))
}

.superflex_lineup <- function() {
  list(slate_id = "test-sflex", slots = list(
    list(pos = "QB",    n = 1L, eligible = c("QB")),
    list(pos = "RB",    n = 2L, eligible = c("RB")),
    list(pos = "WR",    n = 2L, eligible = c("WR")),
    list(pos = "TE",    n = 1L, eligible = c("TE")),
    list(pos = "FLEX",  n = 1L, eligible = c("RB", "WR", "TE")),
    list(pos = "SFLEX", n = 1L, eligible = c("QB", "RB", "WR", "TE"))
  ))
}

# Synthetic blended players: n_per_pos players per position with season
# means descending from `top` in steps of `step`.
.synthetic_players <- function(n_per_pos = 40L, top = 400, step = 5) {
  out <- list()
  for (pos in c("QB", "RB", "WR", "TE")) {
    for (k in seq_len(n_per_pos)) {
      out[[length(out) + 1L]] <- list(
        underdog_id = sprintf("%s-%02d", pos, k),
        position    = pos,
        season_mean = top - (k - 1L) * step
      )
    }
  }
  out
}

# ---- .replacement_ranks_from_lineup -----------------------------------------

test_that("standard 1-QB lineup derives exactly the v1 constants", {
  ranks <- .replacement_ranks_from_lineup(.standard_lineup())
  expect_equal(ranks, c(QB = 12L, RB = 24L, WR = 36L, TE = 12L))
})

test_that("superflex lineup derives QB24 (base QB + SFLEX)", {
  ranks <- .replacement_ranks_from_lineup(.superflex_lineup())
  expect_equal(ranks, c(QB = 24L, RB = 24L, WR = 24L, TE = 12L))
})

test_that("pod_size scales all ranks", {
  ranks <- .replacement_ranks_from_lineup(.standard_lineup(), pod_size = 10L)
  expect_equal(ranks, c(QB = 10L, RB = 20L, WR = 30L, TE = 10L))
})

test_that("manifest 1-QB slates derive the old hardcoded ranks (no regression)", {
  v1_constant <- c(QB = 12L, RB = 24L, WR = 36L, TE = 12L)
  for (sid in c("nfl_2026_season", "nfl_2026_eliminator",
                "nfl_2026_weekly_winners")) {
    spec <- load_slate_lineup_spec(sid)
    expect_equal(.replacement_ranks_from_lineup(spec), v1_constant,
                 info = sid)
  }
})

test_that("manifest superflex slate derives QB24", {
  spec  <- load_slate_lineup_spec("nfl_2026_superflex")
  ranks <- .replacement_ranks_from_lineup(spec)
  expect_equal(ranks[["QB"]], 24L)
  expect_equal(ranks[["TE"]], 12L)
})

# ---- .add_position_metrics with derived ranks -------------------------------

test_that("vor under standard ranks matches the old hardcoded behavior", {
  players <- .synthetic_players()
  ranks   <- .replacement_ranks_from_lineup(.standard_lineup())
  out     <- .add_position_metrics(players, replacement_ranks = ranks)

  # Replicate the old math by hand: vor = max(0, mean - mean_at_old_rank).
  old_rank <- c(QB = 12L, RB = 24L, WR = 36L, TE = 12L)
  means <- vapply(players, function(p) p$season_mean, numeric(1))
  pos   <- vapply(players, function(p) p$position, character(1))
  for (i in seq_along(out)) {
    pool <- sort(means[pos == pos[i]], decreasing = TRUE)
    expected <- round(max(0, means[i] - pool[old_rank[[pos[i]]]]), 1)
    expect_equal(out[[i]]$vor, expected, info = out[[i]]$underdog_id)
  }
})

test_that("superflex ranks raise QB vor and unlock QB13-24", {
  players <- .synthetic_players()
  std  <- .add_position_metrics(players,
            replacement_ranks = .replacement_ranks_from_lineup(.standard_lineup()))
  sfx  <- .add_position_metrics(players,
            replacement_ranks = .replacement_ranks_from_lineup(.superflex_lineup()))

  vor_of <- function(out, id) {
    for (p in out) if (p$underdog_id == id) return(p$vor)
    NULL
  }

  # QB1 gains exactly the gap between the QB12 and QB24 means.
  qb_means <- sort(vapply(Filter(function(p) p$position == "QB", players),
                          function(p) p$season_mean, numeric(1)),
                   decreasing = TRUE)
  gap <- qb_means[12] - qb_means[24]
  expect_gt(gap, 0)
  expect_equal(vor_of(sfx, "QB-01") - vor_of(std, "QB-01"), gap)

  # QBs between the old and new replacement rank go from 0 to positive.
  expect_equal(vor_of(std, "QB-18"), 0)
  expect_gt(vor_of(sfx, "QB-18"), 0)

  # Non-QB positions: TE and RB unchanged (same dedicated slot count in
  # either lineup). WR replacement moves 36 -> 24 (superflex starts 2 WRs,
  # not 3), so the WR replacement value is higher and WR vor *decreases*.
  expect_equal(vor_of(sfx, "TE-01"), vor_of(std, "TE-01"))
  expect_equal(vor_of(sfx, "RB-01"), vor_of(std, "RB-01"))
  expect_lt(vor_of(sfx, "WR-01"), vor_of(std, "WR-01"))
})

test_that("replacement value falls back to 0 when the pool is shallower than the rank", {
  # Only 5 QBs: rank 12 unreachable -> repl value 0 -> vor = season_mean.
  players <- Filter(function(p) p$position == "QB",
                    .synthetic_players(n_per_pos = 5L))
  out <- .add_position_metrics(players,
           replacement_ranks = .replacement_ranks_from_lineup(.standard_lineup()))
  expect_equal(out[[1]]$vor, players[[1]]$season_mean)
})
