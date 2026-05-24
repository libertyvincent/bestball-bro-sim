# Hermetic tests for project_rookies / .identify_rookies / .project_slate —
# no network, no nflreadr.

make_rookie_slate <- function() {
  data.frame(
    underdog_id = paste0("ud-R", sprintf("%05d", 1:5)),
    full_name   = c("Rookie WR 1",  # 1st-round WR, hits the well-populated bin
                    "Rookie WR 2",  # 1st-round WR (second one to exercise dup-bin)
                    "Rookie RB",    # 1st-round RB, bin too small → fallback
                    "Rookie UDFA",  # no current-year draft entry → UDFA bin
                    "Rookie K"),    # ineligible position, must be skipped
    team_abbr   = c("BUF", "MIA", "ATL", "CHI", "DEN"),
    position    = c("WR", "WR", "RB", "WR", "K"),
    stringsAsFactors = FALSE
  )
}

make_draft_fixture <- function() {
  # Current-year (2026) draft picks for three of the rookies. Note: project_rookies
  # joins by name+team+position (rookies have no gsis_id), so pfr_player_name and
  # team here must match the slate's full_name / team_abbr (already nflverse).
  current <- data.frame(
    season   = 2026L,
    round    = c(1L, 1L, 1L),
    pick     = c(5L, 10L, 15L),
    position = c("WR", "WR", "RB"),
    gsis_id  = c("00-R000001", "00-R000002", "00-R000003"),
    pfr_player_name = c("Rookie WR 1", "Rookie WR 2", "Rookie RB"),
    team            = c("BUF", "MIA", "ATL"),
    stringsAsFactors = FALSE
  )
  # 12 historical 1st-round WR comparables spread across 2021-2025.
  hist_wr <- data.frame(
    season   = c(2021L, 2021L, 2022L, 2022L, 2023L, 2023L,
                 2023L, 2024L, 2024L, 2025L, 2025L, 2025L),
    round    = 1L,
    pick     = c(3L, 12L, 8L, 18L, 5L, 14L, 22L, 4L, 16L, 7L, 17L, 25L),
    position = "WR",
    gsis_id  = paste0("00-H", sprintf("%06d", 1:12)),
    pfr_player_name = paste0("Hist WR ", 1:12),
    team            = "BAL",
    stringsAsFactors = FALSE
  )
  # Only 3 historical 1st-round RB comparables → below 10-sample floor.
  hist_rb <- data.frame(
    season   = c(2022L, 2023L, 2024L),
    round    = 1L,
    pick     = c(8L, 12L, 4L),
    position = "RB",
    gsis_id  = paste0("00-H", sprintf("%06d", 21:23)),
    pfr_player_name = paste0("Hist RB ", 21:23),
    team            = "BAL",
    stringsAsFactors = FALSE
  )
  # A few 4th-round RB comparables to feed the position-only fallback.
  hist_rb_late <- data.frame(
    season   = c(2021L, 2022L, 2023L, 2024L, 2025L,
                 2021L, 2022L, 2023L, 2024L, 2025L),
    round    = 4L,
    pick     = 100L:109L,
    position = "RB",
    gsis_id  = paste0("00-H", sprintf("%06d", 31:40)),
    pfr_player_name = paste0("Hist RB late ", 31:40),
    team            = "BAL",
    stringsAsFactors = FALSE
  )
  rbind(current, hist_wr, hist_rb, hist_rb_late)
}

make_rookie_historical_stats <- function() {
  # Five regular-season weeks per comparable.
  rows <- list()

  wr_pids    <- paste0("00-H", sprintf("%06d", 1:12))
  wr_seasons <- c(2021L, 2021L, 2022L, 2022L, 2023L, 2023L,
                  2023L, 2024L, 2024L, 2025L, 2025L, 2025L)
  for (i in seq_along(wr_pids)) {
    base_yards <- 30 + i * 5   # 35, 40, ..., 90 yards/week
    for (w in 1:5) {
      rows[[length(rows) + 1L]] <- data.frame(
        player_id = wr_pids[i], position = "WR",
        season = wr_seasons[i], week = as.integer(w),
        season_type = "REG",
        passing_yards = 0, passing_tds = 0, interceptions = 0,
        rushing_yards = 0, rushing_tds = 0,
        receptions = 4L, receiving_yards = base_yards, receiving_tds = 0L,
        stringsAsFactors = FALSE
      )
    }
  }
  # RB comparables. Skip 00-H000023 (zero-output / cut all year).
  rb_pids    <- paste0("00-H", sprintf("%06d", c(21, 22, 31:40)))
  rb_seasons <- c(2022L, 2023L,                                      # 1st-rd RBs
                  2021L, 2022L, 2023L, 2024L, 2025L,                 # 4th-rd 31-35
                  2021L, 2022L, 2023L, 2024L, 2025L)                 # 4th-rd 36-40
  for (i in seq_along(rb_pids)) {
    base_rush <- 25 + i * 4
    for (w in 1:5) {
      rows[[length(rows) + 1L]] <- data.frame(
        player_id = rb_pids[i], position = "RB",
        season = rb_seasons[i], week = as.integer(w),
        season_type = "REG",
        passing_yards = 0, passing_tds = 0, interceptions = 0,
        rushing_yards = base_rush, rushing_tds = 0L,
        receptions = 2L, receiving_yards = 12L, receiving_tds = 0L,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

# ---- project_rookies unit tests ----

test_that("project_rookies returns the slate-rookie schema", {
  rookies <- make_rookie_slate()
  drafts  <- make_draft_fixture()
  hist    <- make_rookie_historical_stats()
  scoring <- load_scoring_config("half_ppr_underdog")

  suppressWarnings(  # fallbacks for the RB and UDFA bins
    out <- project_rookies(rookies, drafts, hist, scoring, season = 2026)
  )

  expect_named(out, c(
    "underdog_id", "full_name", "team_abbr", "position",
    "season_mean", "season_std",
    "season_p10", "season_p25", "season_p50",
    "season_p75", "season_p90", "season_p95"
  ))
  # 4 rookies projected (kicker is dropped — not a skill position)
  expect_equal(nrow(out), 4)
  expect_false("ud-R00005" %in% out$underdog_id)
})

test_that("1st-round WR rookie projects to the empirical median of WR comparables", {
  rookies <- make_rookie_slate()[1, , drop = FALSE]  # just Rookie WR 1
  drafts  <- make_draft_fixture()
  hist    <- make_rookie_historical_stats()
  scoring <- load_scoring_config("half_ppr_underdog")

  out <- project_rookies(rookies, drafts, hist, scoring, season = 2026)

  hist$fp <- compute_fantasy_points(hist, scoring)
  wr_pids <- paste0("00-H", sprintf("%06d", 1:12))
  wr_totals <- vapply(wr_pids,
                      function(p) sum(hist$fp[hist$player_id == p]),
                      numeric(1))
  expected_q <- stats::quantile(
    wr_totals, probs = c(0.10, 0.25, 0.50, 0.75, 0.90, 0.95),
    type = 7, names = FALSE)

  expect_equal(out$season_mean, stats::median(wr_totals))
  expect_equal(out$season_std,  stats::sd(wr_totals))
  expect_equal(out$season_p10,  expected_q[1])
  expect_equal(out$season_p50,  expected_q[3])
  expect_equal(out$season_p90,  expected_q[5])
  expect_true(out$season_p10 < out$season_p50)
  expect_true(out$season_p50 < out$season_p90)
})

test_that("undersized bin warns and falls back to position-only comparables", {
  rookies <- make_rookie_slate()[3, , drop = FALSE]  # 1st-round RB
  drafts  <- make_draft_fixture()
  hist    <- make_rookie_historical_stats()
  scoring <- load_scoring_config("half_ppr_underdog")

  expect_warning(
    out <- project_rookies(rookies, drafts, hist, scoring, season = 2026),
    "below 10-sample floor"
  )

  # Position-only fallback for RB pools the 2 played 1st-rounders +
  # 10 played 4th-rounders = 12 totals. Median should be > 0.
  expect_gt(out$season_mean, 0)
})

test_that("UDFA rookies (no current-year draft entry) fall back via UDFA bin", {
  rookies <- make_rookie_slate()[4, , drop = FALSE]  # Rookie UDFA WR
  drafts  <- make_draft_fixture()
  hist    <- make_rookie_historical_stats()
  scoring <- load_scoring_config("half_ppr_underdog")

  # No UDFA comparables → falls back to position-only WR. The warning
  # message names the bin so the user can see WHY it fell back.
  expect_warning(
    out <- project_rookies(rookies, drafts, hist, scoring, season = 2026),
    "WR/UDFA"
  )
  expect_equal(nrow(out), 1)
  expect_gt(out$season_mean, 0)
})

test_that("a drafted comparable who never played enters the bin with zero", {
  # 00-H000023 is a 1st-round RB in the fixture but has no historical stats
  # (cut / IR all year). Check we got 3 RB-1st samples (not 2).
  drafts  <- make_draft_fixture()
  hist    <- make_rookie_historical_stats()
  scoring <- load_scoring_config("half_ppr_underdog")

  comp <- .build_rookie_comparables(drafts, hist, scoring, season = 2026)
  rb_first <- comp[comp$position == "RB" & comp$bin == "1st", ]
  expect_equal(nrow(rb_first), 3)
  expect_true(any(rb_first$season_total == 0))
  expect_true("00-H000023" %in% rb_first$gsis_id)
})

# ---- .draft_capital_bin unit tests ----

test_that(".draft_capital_bin maps rounds to spec bins", {
  expect_equal(.draft_capital_bin(1L),           "1st")
  expect_equal(.draft_capital_bin(2L),           "2nd-3rd")
  expect_equal(.draft_capital_bin(3L),           "2nd-3rd")
  expect_equal(.draft_capital_bin(4L),           "4th-5th")
  expect_equal(.draft_capital_bin(5L),           "4th-5th")
  expect_equal(.draft_capital_bin(6L),           "6th-7th")
  expect_equal(.draft_capital_bin(7L),           "6th-7th")
  expect_equal(.draft_capital_bin(NA_integer_),  "UDFA")
})

# ---- .identify_rookies unit tests ----

test_that(".identify_rookies returns slate skill players absent from historical stats", {
  # The shared slate fixture has 9 skill players; players 1-8 mirror names
  # present in make_test_historical(), player 9 ("Rookie WR") does not.
  rookies <- .identify_rookies(make_test_slate(), make_test_historical())
  expect_equal(nrow(rookies), 1)
  expect_equal(rookies$underdog_id, "ud-0000009")
})

test_that(".identify_rookies uses name+position fallback when team changes", {
  # Traded-veteran scenario: slate has CeeDee Lamb on a different team than
  # history. Tier-1 (name+team+pos) misses; tier-2 (name+pos unique) hits.
  # He must NOT be flagged as a rookie.
  slate <- make_test_slate()
  slate$team_abbr[slate$full_name == "CeeDee Lamb"] <- "WAS"
  rookies <- .identify_rookies(slate, make_test_historical())
  expect_false("ud-0000003" %in% rookies$underdog_id)  # Lamb still a veteran
  expect_equal(rookies$underdog_id, "ud-0000009")      # only Rookie WR rookie
})

# ---- .project_slate integration: veterans unchanged, rookies via comparables ----

test_that(".project_slate veterans match .project_v0 output for the same players", {
  slate   <- make_test_slate()
  hist    <- make_test_historical()
  scoring <- load_scoring_config("half_ppr_underdog")
  drafts  <- data.frame(season = integer(0), round = integer(0),
                        pick = integer(0), position = character(0),
                        gsis_id = character(0),
                        pfr_player_name = character(0),
                        team = character(0),
                        stringsAsFactors = FALSE)

  slate_out <- suppressWarnings(
    .project_slate(slate, hist, drafts, scoring, season = 2026)
  )

  # Veterans: every slate veteran (players 1-8) appears, with season_mean
  # equal to the .project_v0 value keyed by their gsis_id (via the
  # internal name-based match).
  v0 <- .project_v0(make_test_rosters(), hist, scoring)
  vet_ids <- paste0("00-", sprintf("%07d", 1:8))
  cols <- c("season_mean", "season_std",
            "season_p10", "season_p25", "season_p50",
            "season_p75", "season_p90", "season_p95")
  v0_vet    <- v0[match(vet_ids, v0$gsis_id), cols]
  slate_vet <- slate_out[match(vet_ids, slate_out$gsis_id), cols]
  expect_equal(slate_vet, v0_vet, ignore_attr = "row.names")
})

test_that(".project_slate carries underdog_id, adp, and underdog_projected_points through", {
  slate   <- make_test_slate()
  hist    <- make_test_historical()
  scoring <- load_scoring_config("half_ppr_underdog")
  drafts  <- data.frame(season = integer(0), round = integer(0),
                        pick = integer(0), position = character(0),
                        gsis_id = character(0),
                        pfr_player_name = character(0),
                        team = character(0),
                        stringsAsFactors = FALSE)

  out <- suppressWarnings(
    .project_slate(slate, hist, drafts, scoring, season = 2026)
  )

  expect_equal(nrow(out), 9)
  expect_true(all(c("underdog_id", "adp",
                    "underdog_projected_points") %in% names(out)))
  # Spot check: Bijan's adp and Underdog projection came through from slate.
  bijan_slate <- slate[slate$full_name == "Bijan Robinson", ]
  bijan_out   <- out[out$underdog_id == bijan_slate$underdog_id, ]
  expect_equal(bijan_out$adp, bijan_slate$adp)
  expect_equal(bijan_out$underdog_projected_points,
               bijan_slate$projected_points)
})

test_that(".project_slate emits no gsis_id for rookies", {
  slate   <- make_test_slate()
  hist    <- make_test_historical()
  scoring <- load_scoring_config("half_ppr_underdog")
  drafts  <- data.frame(season = integer(0), round = integer(0),
                        pick = integer(0), position = character(0),
                        gsis_id = character(0),
                        pfr_player_name = character(0),
                        team = character(0),
                        stringsAsFactors = FALSE)

  out <- suppressWarnings(
    .project_slate(slate, hist, drafts, scoring, season = 2026)
  )

  rookie <- out[out$underdog_id == "ud-0000009", ]
  expect_equal(nrow(rookie), 1)
  expect_true(is.na(rookie$gsis_id))
})
