# Tests for R/lineup_optimizer.R. All deterministic -- synthetic rosters with
# fixed scores so totals are hand-computable, plus a slow per-sim reference
# impl for the vectorization cross-check.

# ---- fixtures ---------------------------------------------------------------

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

# Build a long-format scores data.frame from a (player x week) matrix and
# per-sim noise function. Each sim is a single column with the same scores
# unless `jitter_fn(sim_idx)` is provided.
.long_scores_from_matrix <- function(scores_pw, n_sims = 1L, jitter_fn = NULL) {
  pids <- rownames(scores_pw)
  weeks <- seq_len(ncol(scores_pw))
  out <- list()
  for (s in seq_len(n_sims)) {
    add <- if (is.null(jitter_fn)) matrix(0, nrow = nrow(scores_pw),
                                          ncol = ncol(scores_pw)) else jitter_fn(s)
    M_s <- scores_pw + add
    for (w in weeks) {
      out[[length(out) + 1L]] <- data.frame(
        underdog_id = pids,
        sim_idx     = s,
        week        = w,
        draw_value  = M_s[, w],
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, out)
}

# Slow per-sim reference: for each (sim, week), greedily pick top-k per
# pure-position slot, then apply the multi-slot rule. Used to cross-check
# the vectorized path.
.slow_lineup_total <- function(scores_vec, positions, lineup_spec) {
  # scores_vec: named numeric vec underdog_id -> score for one week.
  pos <- positions[names(scores_vec)]
  by_pos <- split(scores_vec, pos)
  by_pos <- lapply(by_pos, function(v) sort(v, decreasing = TRUE))

  pure <- Filter(function(s) length(s$eligible) == 1L &&
                              s$eligible == s$pos, lineup_spec$slots)
  multi <- Filter(function(s) !(length(s$eligible) == 1L &&
                                s$eligible == s$pos), lineup_spec$slots)

  total <- 0
  consumed <- setNames(integer(length(by_pos)), names(by_pos))
  for (s in pure) {
    v <- by_pos[[s$pos]]
    if (is.null(v) || length(v) == 0L) {
      consumed[s$pos] <- s$n; next
    }
    take <- min(s$n, length(v))
    total <- total + sum(v[seq_len(take)])
    consumed[s$pos] <- s$n
  }

  flex_pool_positions <- c("RB", "WR", "TE")
  L <- numeric(0)
  for (p in flex_pool_positions) {
    v <- by_pos[[p]]
    if (is.null(v)) next
    c_p <- consumed[p]
    if (is.na(c_p)) c_p <- 0L
    if (length(v) > c_p) L <- c(L, v[(c_p + 1L):length(v)])
  }
  L <- sort(L, decreasing = TRUE)

  is_flex  <- function(s) setequal(s$eligible, flex_pool_positions) && s$n == 1L
  is_sflex <- function(s) setequal(s$eligible, c("QB", flex_pool_positions)) && s$n == 1L
  has_sflex <- any(vapply(multi, is_sflex, logical(1)))

  f1 <- if (length(L) >= 1L) L[1L] else 0
  if (!has_sflex) return(total + f1)

  f2 <- if (length(L) >= 2L) L[2L] else 0
  qb_v <- by_pos[["QB"]]
  qb2 <- if (!is.null(qb_v) && length(qb_v) >= 2L) qb_v[2L] else 0
  total + f1 + max(qb2, f2)
}

# ---- 1. Hand-computed Season ------------------------------------------------

test_that("Season: hand-computed total, FLEX picks best RB/WR/TE leftover", {
  # Roster: 2 QB, 4 RB, 5 WR, 2 TE.
  # Scores chosen so the FLEX pick is unambiguous.
  scores <- c(
    QB1 = 30, QB2 = 22,
    RB1 = 25, RB2 = 18, RB3 = 14, RB4 = 5,    # RB starters = 25 + 18 = 43;
                                              # rest = {14, 5}
    WR1 = 22, WR2 = 20, WR3 = 17, WR4 = 12, WR5 = 6,  # starters = 59; rest = {12, 6}
    TE1 = 13, TE2 = 9                          # starter = 13; rest = {9}
  )
  positions <- c(QB1 = "QB", QB2 = "QB",
                 RB1 = "RB", RB2 = "RB", RB3 = "RB", RB4 = "RB",
                 WR1 = "WR", WR2 = "WR", WR3 = "WR", WR4 = "WR", WR5 = "WR",
                 TE1 = "TE", TE2 = "TE")
  # Leftover pool: RB3=14, RB4=5, WR4=12, WR5=6, TE2=9. Best is RB3=14 -> FLEX.
  expected <- 30 + 43 + 59 + 13 + 14   # = 159

  pw <- matrix(scores, nrow = length(scores), ncol = 1L,
               dimnames = list(names(scores), NULL))
  long <- .long_scores_from_matrix(pw, n_sims = 1L)
  totals <- optimize_lineup_totals(long, positions, .season_spec())
  expect_equal(dim(totals), c(1L, 1L))
  expect_equal(unname(totals[1L, 1L]), expected)
})

# ---- 2. Superflex two-QB edge case (beats naive greedy) ---------------------

test_that("Superflex: two strong leftover QBs -> closed form beats naive greedy", {
  # Roster: 3 QB (only 1 starts at QB), 3 RB, 2 WR, 1 TE.
  # Both leftover QBs (QB2, QB3) are huge; both flex-eligible leftovers are
  # mediocre. SFLEX should take QB2; FLEX takes the best RB/WR/TE leftover.
  # Naive "FLEX first, fill FLEX with best leftover-flex-eligible, then SFLEX
  # with next-best of (qb2 vs leftover-flex-eligible)" gets the same answer
  # here; the trickier case is when flex-eligible leftover f1 > qb2 > f2.
  # We test that case below in test 3. Here we cover: qb2 > f1 > f2.
  scores <- c(
    QB1 = 28, QB2 = 26, QB3 = 24,           # QB1 starts at QB
    RB1 = 18, RB2 = 15, RB3 = 8,            # RB top2 = 33; rest = {8}
    WR1 = 14, WR2 = 13, WR3 = 7,            # WR top2 = 27; rest = {7}
    TE1 = 10, TE2 = 5                       # TE1; rest = {5}
  )
  # Leftover pool L = {RB3=8, WR3=7, TE2=5}; f1=8, f2=7.
  # qb2 = 26.  qb2 > f1 > f2.
  # Closed form: SFLEX = qb2 (26); FLEX = f1 (8). flex_pair = 8 + max(26, 7) = 34.
  # Total = QB1 + RB_top2 + WR_top2 + TE1 + flex_pair
  #       = 28 + 33 + 27 + 10 + 34 = 132.
  expected <- 132
  positions <- c(QB1 = "QB", QB2 = "QB", QB3 = "QB",
                 RB1 = "RB", RB2 = "RB", RB3 = "RB",
                 WR1 = "WR", WR2 = "WR", WR3 = "WR",
                 TE1 = "TE", TE2 = "TE")

  pw <- matrix(scores, nrow = length(scores), ncol = 1L,
               dimnames = list(names(scores), NULL))
  long <- .long_scores_from_matrix(pw, n_sims = 1L)
  totals <- optimize_lineup_totals(long, positions, .superflex_spec())
  expect_equal(unname(totals[1L, 1L]), expected)

  # Naive "always FLEX-first, fill FLEX with best leftover-flex-eligible
  # (ignoring QBs), then fill SFLEX with best of remaining {qb2, leftover-
  # flex-eligible}". On THIS roster naive and optimal happen to agree, but
  # we sanity-check the more interesting case in test 2b below.
})

test_that("Superflex: f1 > qb2 > f2 -> optimal puts f1 in FLEX, qb2 in SFLEX", {
  # Constructed so the closed-form's `f1 + max(qb2, f2)` matters.
  scores <- c(
    QB1 = 30, QB2 = 18,                     # qb2 = 18
    RB1 = 22, RB2 = 20, RB3 = 25,           # sort: 25, 22, 20. top2 = 47; rest = {20}
    WR1 = 21, WR2 = 19,                     # WR top2 = 40; no rest
    TE1 = 12                                # TE1; no rest
  )
  # L = {RB3-as-leftover=20}; only one entry. f1 = 20, f2 = 0.
  # Need f1 > qb2 > f2: 20 > 18 > 0. ✓
  # Closed-form flex_pair = 20 + max(18, 0) = 38.
  # Total = 30 + (25+22) + (21+19) + 12 + 38 = 30+47+40+12+38 = 167.
  expected <- 167
  positions <- c(QB1 = "QB", QB2 = "QB",
                 RB1 = "RB", RB2 = "RB", RB3 = "RB",
                 WR1 = "WR", WR2 = "WR",
                 TE1 = "TE")
  pw <- matrix(scores, nrow = length(scores), ncol = 1L,
               dimnames = list(names(scores), NULL))
  long <- .long_scores_from_matrix(pw, n_sims = 1L)
  totals <- optimize_lineup_totals(long, positions, .superflex_spec())
  expect_equal(unname(totals[1L, 1L]), expected)
})

# ---- 3. Superflex normal case (SFLEX takes non-QB) --------------------------

test_that("Superflex: SFLEX correctly takes a non-QB when optimal", {
  # Strong leftover RB with weak backup QB -> SFLEX = leftover RB (f2),
  # FLEX = best leftover (f1).
  scores <- c(
    QB1 = 28, QB2 = 6,                       # qb2 = 6, very weak
    RB1 = 20, RB2 = 18, RB3 = 17, RB4 = 16,  # top2 = 38; rest = {17, 16}
    WR1 = 22, WR2 = 19, WR3 = 12,            # top2 = 41; rest = {12}
    TE1 = 14, TE2 = 11                       # TE1=14; rest = {11}
  )
  # L = {17, 16, 12, 11}. f1 = 17, f2 = 16. qb2 = 6.
  # flex_pair = 17 + max(6, 16) = 33.
  # Total = 28 + 38 + 41 + 14 + 33 = 154.
  expected <- 154
  positions <- c(QB1 = "QB", QB2 = "QB",
                 RB1 = "RB", RB2 = "RB", RB3 = "RB", RB4 = "RB",
                 WR1 = "WR", WR2 = "WR", WR3 = "WR",
                 TE1 = "TE", TE2 = "TE")
  pw <- matrix(scores, nrow = length(scores), ncol = 1L,
               dimnames = list(names(scores), NULL))
  long <- .long_scores_from_matrix(pw, n_sims = 1L)
  totals <- optimize_lineup_totals(long, positions, .superflex_spec())
  expect_equal(unname(totals[1L, 1L]), expected)
})

# ---- 4. Bye / short position ------------------------------------------------

test_that("Bye week: byed players score 0 and never displace real scorers", {
  # Two weeks. RB1 is byed in week 2 (score 0).
  scores_w1 <- c(QB1 = 30, QB2 = 20,
                 RB1 = 25, RB2 = 18, RB3 = 12,
                 WR1 = 22, WR2 = 20, WR3 = 17, WR4 = 8,
                 TE1 = 14)
  scores_w2 <- c(QB1 = 28, QB2 = 22,
                 RB1 = 0,  RB2 = 21, RB3 = 18,    # RB1 byed
                 WR1 = 20, WR2 = 19, WR3 = 16, WR4 = 10,
                 TE1 = 12)
  positions <- c(QB1 = "QB", QB2 = "QB",
                 RB1 = "RB", RB2 = "RB", RB3 = "RB",
                 WR1 = "WR", WR2 = "WR", WR3 = "WR", WR4 = "WR",
                 TE1 = "TE")
  pw <- cbind(scores_w1[names(positions)], scores_w2[names(positions)])
  rownames(pw) <- names(positions)
  long <- .long_scores_from_matrix(pw, n_sims = 1L)
  totals <- optimize_lineup_totals(long, positions, .season_spec())
  expect_equal(dim(totals), c(1L, 2L))

  # Week 1: QB=30, RB_top2=43, WR_top3=59, TE=14; L={RB3=12, WR4=8}; FLEX=12.
  # Total = 30 + 43 + 59 + 14 + 12 = 158.
  expect_equal(unname(totals[1L, 1L]), 158)
  # Week 2: RB starters = top-2 of {21, 18, 0} = 39; RB rest = {0}.
  #         WR_top3 = 20+19+16 = 55; WR rest = {10}.
  #         L = {0, 10}; FLEX = 10. (Byed RB1 sorts to bottom; doesn't displace.)
  # Total = 28 + 39 + 55 + 12 + 10 = 144.
  expect_equal(unname(totals[1L, 2L]), 144)
})

test_that("Short position: one RB on roster fills the available slot, missing -> 0", {
  scores <- c(QB1 = 25,
              RB1 = 18,             # only 1 RB; RB2 slot fills 0
              WR1 = 20, WR2 = 17, WR3 = 14,
              TE1 = 11)
  positions <- c(QB1 = "QB",
                 RB1 = "RB",
                 WR1 = "WR", WR2 = "WR", WR3 = "WR",
                 TE1 = "TE")
  pw <- matrix(scores, nrow = length(scores), ncol = 1L,
               dimnames = list(names(scores), NULL))
  long <- .long_scores_from_matrix(pw, n_sims = 1L)
  # No flex-eligible leftover at all -> FLEX = 0.
  # Total = 25 + 18 + 51 + 11 + 0 = 105.
  expect_silent(totals <- optimize_lineup_totals(long, positions, .season_spec()))
  expect_equal(unname(totals[1L, 1L]), 105)
})

# ---- 5. Vectorization cross-check ------------------------------------------

test_that("Vectorized totals == slow per-sim reference (Season, random scores)", {
  set.seed(101L)
  positions <- c(QB1 = "QB", QB2 = "QB",
                 RB1 = "RB", RB2 = "RB", RB3 = "RB", RB4 = "RB",
                 WR1 = "WR", WR2 = "WR", WR3 = "WR", WR4 = "WR", WR5 = "WR",
                 TE1 = "TE", TE2 = "TE")
  n_sims <- 200L
  n_weeks <- 3L
  # Build random scores: a [players x weeks x sims] array.
  pids <- names(positions)
  scores <- array(stats::runif(length(pids) * n_weeks * n_sims, 0, 30),
                  dim = c(length(pids), n_weeks, n_sims),
                  dimnames = list(pids, NULL, NULL))
  # Long form for the vectorized path.
  long <- do.call(rbind, lapply(seq_len(n_sims), function(s) {
    do.call(rbind, lapply(seq_len(n_weeks), function(w) {
      data.frame(underdog_id = pids, sim_idx = s, week = w,
                 draw_value = scores[, w, s],
                 stringsAsFactors = FALSE)
    }))
  }))

  spec <- .season_spec()
  fast <- optimize_lineup_totals(long, positions, spec)
  slow <- matrix(0, nrow = n_sims, ncol = n_weeks)
  for (s in seq_len(n_sims)) {
    for (w in seq_len(n_weeks)) {
      vec <- scores[, w, s]
      names(vec) <- pids
      slow[s, w] <- .slow_lineup_total(vec, positions, spec)
    }
  }
  expect_equal(fast, slow, tolerance = 1e-10, ignore_attr = TRUE)
})

test_that("Vectorized totals == slow per-sim reference (Superflex, random scores)", {
  set.seed(202L)
  positions <- c(QB1 = "QB", QB2 = "QB", QB3 = "QB",
                 RB1 = "RB", RB2 = "RB", RB3 = "RB", RB4 = "RB",
                 WR1 = "WR", WR2 = "WR", WR3 = "WR", WR4 = "WR",
                 TE1 = "TE", TE2 = "TE")
  n_sims <- 200L
  n_weeks <- 2L
  pids <- names(positions)
  scores <- array(stats::runif(length(pids) * n_weeks * n_sims, 0, 30),
                  dim = c(length(pids), n_weeks, n_sims),
                  dimnames = list(pids, NULL, NULL))
  long <- do.call(rbind, lapply(seq_len(n_sims), function(s) {
    do.call(rbind, lapply(seq_len(n_weeks), function(w) {
      data.frame(underdog_id = pids, sim_idx = s, week = w,
                 draw_value = scores[, w, s],
                 stringsAsFactors = FALSE)
    }))
  }))

  spec <- .superflex_spec()
  fast <- optimize_lineup_totals(long, positions, spec)
  slow <- matrix(0, nrow = n_sims, ncol = n_weeks)
  for (s in seq_len(n_sims)) {
    for (w in seq_len(n_weeks)) {
      vec <- scores[, w, s]
      names(vec) <- pids
      slow[s, w] <- .slow_lineup_total(vec, positions, spec)
    }
  }
  expect_equal(fast, slow, tolerance = 1e-10, ignore_attr = TRUE)
})

# ---- 6. Spec-driven swap ---------------------------------------------------

test_that("Season -> Superflex spec swap changes total without code change", {
  scores <- c(QB1 = 28, QB2 = 22,
              RB1 = 20, RB2 = 18, RB3 = 16,
              WR1 = 22, WR2 = 20, WR3 = 18, WR4 = 12,
              TE1 = 14, TE2 = 9)
  positions <- c(QB1 = "QB", QB2 = "QB",
                 RB1 = "RB", RB2 = "RB", RB3 = "RB",
                 WR1 = "WR", WR2 = "WR", WR3 = "WR", WR4 = "WR",
                 TE1 = "TE", TE2 = "TE")
  pw <- matrix(scores, nrow = length(scores), ncol = 1L,
               dimnames = list(names(scores), NULL))
  long <- .long_scores_from_matrix(pw, n_sims = 1L)

  t_season  <- optimize_lineup_totals(long, positions, .season_spec())
  t_sflex   <- optimize_lineup_totals(long, positions, .superflex_spec())

  # Season: QB=28, RB_top2=38, WR_top3=60, TE=14; L={RB3=16, WR4=12, TE2=9};
  # FLEX=16. Total = 28+38+60+14+16 = 156.
  expect_equal(unname(t_season[1L, 1L]), 156)

  # Superflex (note 2 WR not 3): QB=28, RB_top2=38, WR_top2=42, TE=14;
  # L = {RB3=16, WR3=18, WR4=12, TE2=9}; sorted desc: 18,16,12,9;
  # f1=18, f2=16. qb2=22. flex_pair = 18 + max(22, 16) = 40.
  # Total = 28+38+42+14+40 = 162.
  expect_equal(unname(t_sflex[1L, 1L]), 162)
  expect_true(unname(t_sflex[1L, 1L]) != unname(t_season[1L, 1L]))
})

# ---- 7. Slate loader integration -------------------------------------------

test_that("load_slate_lineup_spec returns a usable spec for a real slate", {
  spec <- load_slate_lineup_spec("nfl_2026_season")
  expect_equal(spec$slate_id, "nfl_2026_season")
  expect_true(length(spec$slots) > 0L)
  slot_pos <- vapply(spec$slots, `[[`, character(1), "pos")
  expect_true(all(c("QB", "RB", "WR", "TE", "FLEX") %in% slot_pos))

  sflex <- load_slate_lineup_spec("nfl_2026_superflex")
  sflex_pos <- vapply(sflex$slots, `[[`, character(1), "pos")
  expect_true("SFLEX" %in% sflex_pos)
  # Superflex spec on disk uses 2 WR, not 3. Confirm here so that a regression
  # to the prompt's "3 WR" assumption fails loudly.
  wr_slot <- Filter(function(s) s$pos == "WR", sflex$slots)[[1L]]
  expect_equal(wr_slot$n, 2L)
})

test_that("load_slate_lineup_spec errors on unknown slate", {
  expect_error(load_slate_lineup_spec("nope"), "Unknown slate_id")
})

# ---- 8. Input format flexibility -------------------------------------------

test_that("optimize_lineup_totals accepts named per-week list of matrices", {
  scores <- c(QB1 = 30, QB2 = 20,
              RB1 = 22, RB2 = 18, RB3 = 14,
              WR1 = 20, WR2 = 18, WR3 = 15,
              TE1 = 13)
  positions <- c(QB1 = "QB", QB2 = "QB",
                 RB1 = "RB", RB2 = "RB", RB3 = "RB",
                 WR1 = "WR", WR2 = "WR", WR3 = "WR",
                 TE1 = "TE")
  pw <- matrix(scores, nrow = length(scores), ncol = 1L,
               dimnames = list(names(scores), NULL))
  long  <- .long_scores_from_matrix(pw, n_sims = 1L)
  mlist <- list(`1` = pw)

  t_long <- optimize_lineup_totals(long,  positions, .season_spec())
  t_mat  <- optimize_lineup_totals(mlist, positions, .season_spec())
  expect_equal(t_long, t_mat, ignore_attr = TRUE)
})
