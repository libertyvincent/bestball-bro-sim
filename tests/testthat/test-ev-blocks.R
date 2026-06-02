# Tests for R/ev_blocks.R -- the EV building-blocks contract artifacts
# (docs/ev_building_blocks_contract.md).
#
# Fast synthetic tests covering the contract's hard invariants:
#   * Artifact A path alignment (stack correlation survives serialization;
#     structural no-reshuffle assert)
#   * int16 x10 quantization + binary round-trip identity
#   * Artifact B curve correctness on real tournament configs (Puppy 2 /
#     Dachshund ladders)
#   * the reference eval loop: round-score assembly, the combine formula,
#     and CRN marginal EV
#   * publish_ev_blocks() _meta.json registration
#
# The expensive real-data validation gate + path-count protocol live in
# inst/scripts/smoke_ev_blocks.R and the gated test at the bottom.

# ---- synthetic fixture --------------------------------------------------------

# A synthetic blended feed + Layer A draws: 30 players across 6 NFL teams,
# QB01/WR05 stacked on team T01. Mirrors the shape blend_slate() produces
# (only the fields ev_blocks reads).
.ev_fixture <- function(n_layerA_sims = 800L, seed = 42L) {
  set.seed(seed)
  pids <- paste0("p", sprintf("%02d", 1:30))
  pos  <- c(rep("QB", 5), rep("RB", 8), rep("WR", 12), rep("TE", 5))
  team <- paste0("T", sprintf("%02d", rep(1:6, length.out = 30)))
  # p01 (QB, T01) and p06 (RB, T01)... make an explicit QB-WR stack:
  # p01 = QB on T01; p14 = WR on T01 (rep(1:6) puts p07, p13, p19, p25 on T01).
  # Use the natural assignment: players with the same team index are teammates.
  names(pos) <- pids; names(team) <- pids

  mean_by_pos <- c(QB = 18, RB = 12, WR = 13, TE = 9)
  players <- list()
  for (i in seq_along(pids)) {
    p <- pids[i]
    weekly <- lapply(1:17, function(w) {
      is_bye <- (w == 7L && team[p] %in% c("T01", "T02"))   # T01/T02 bye week 7
      list(week = w, opponent = if (is_bye) NULL else paste0("OPP_", team[p]),
           is_bye = is_bye,
           mean = if (is_bye) 0 else mean_by_pos[[pos[p]]],
           std  = if (is_bye) 0 else mean_by_pos[[pos[p]]] * 0.45)
    })
    players[[p]] <- list(
      underdog_id = p, position = unname(pos[p]), team = unname(team[p]),
      adp = i, season_mean = mean_by_pos[[pos[p]]] * 16,
      weekly = weekly)
  }
  feed <- list(players = players)

  layerA <- do.call(rbind, lapply(1:17, function(w) {
    do.call(rbind, lapply(pids, function(p) {
      is_bye <- (w == 7L && team[p] %in% c("T01", "T02"))
      vals <- if (is_bye) rep(0, n_layerA_sims) else {
        pmax(0, stats::rnorm(n_layerA_sims, mean_by_pos[[pos[p]]],
                             mean_by_pos[[pos[p]]] * 0.45))
      }
      data.frame(underdog_id = p, sim_idx = seq_len(n_layerA_sims), week = w,
                 draw_value = vals, stringsAsFactors = FALSE)
    }))
  }))

  list(feed = feed, layerA = layerA, pids = pids, pos = pos, team = team)
}

.ev_lineup_spec <- function() {
  list(slate_id = "ev-test", slots = list(
    list(pos = "QB",   n = 1L, eligible = c("QB")),
    list(pos = "RB",   n = 2L, eligible = c("RB")),
    list(pos = "WR",   n = 3L, eligible = c("WR")),
    list(pos = "TE",   n = 1L, eligible = c("TE")),
    list(pos = "FLEX", n = 1L, eligible = c("RB", "WR", "TE"))
  ))
}

# Synthetic Puppy-2-shaped field scores at a conservation-valid size.
.ev_field_scores <- function(n_field = 3000L, n_sims = 12L, seed = 1L) {
  set.seed(seed)
  ids <- paste0("t", seq_len(n_field))
  mk <- function(mu) matrix(pmax(0, stats::rnorm(n_field * n_sims, mu, 30)),
                            nrow = n_field, dimnames = list(ids, NULL))
  list(stage_scores = list(`1` = mk(2010), `2` = mk(150),
                           `3` = mk(150), `4` = mk(150)))
}

# ---- Artifact A: build / serialize / path alignment ---------------------------

test_that("build_ev_draws: shape, quantization, and player selection", {
  fx <- .ev_fixture()
  ed <- build_ev_draws(fx$feed, fx$layerA, slate_id = "ev-test",
                       n_paths = 200L, top_n = 24L, seed = 7L)
  expect_s3_class(ed, "bbbro_ev_draws")
  # top_n=24 by ADP (= p01..p24 in the fixture).
  expect_equal(ed$player_ids, fx$pids[1:24])
  expect_equal(dim(ed$tensor), c(17L, 24L, 200L))
  expect_true(is.integer(ed$tensor) || all(ed$tensor == round(ed$tensor)))
  expect_true(all(ed$tensor >= -32768L & ed$tensor <= 32767L))
  expect_equal(ed$quant_scale, 10)
  expect_equal(ed$weeks, 1:17)
  # Bye weeks are exactly zero: T01/T02 players, week 7.
  bye_players <- which(ed$player_ids %in% names(fx$team)[fx$team %in% c("T01", "T02")])
  expect_true(all(ed$tensor[7L, bye_players, ] == 0L))
})

test_that("build_ev_draws trims to the requested weeks without changing the paths", {
  # The slate data may carry weeks no tournament uses (e.g. NFL week 18);
  # the `weeks` arg trims AFTER the joint draw, so the same seed gives the
  # same paths for the weeks kept.
  fx <- .ev_fixture()
  full <- build_ev_draws(fx$feed, fx$layerA, slate_id = "ev-test",
                         n_paths = 80L, top_n = 12L, seed = 7L)            # 1:17
  trim <- build_ev_draws(fx$feed, fx$layerA, slate_id = "ev-test",
                         n_paths = 80L, top_n = 12L, seed = 7L, weeks = 1:14)
  expect_equal(trim$weeks, 1:14)
  expect_equal(dim(trim$tensor), c(14L, 12L, 80L))
  # The kept weeks are bit-identical to the full build (path alignment is
  # unaffected by week selection).
  expect_identical(trim$tensor, full$tensor[1:14, , , drop = FALSE])
})

test_that("Artifact A HARD INVARIANT: structural no-reshuffle vs the joint draw", {
  # build_ev_draws must serialize sample_correlated_draws()'s joint output
  # verbatim -- same players, same path (column) order, no per-player
  # reordering. Replicate its internal call and compare row by row.
  fx <- .ev_fixture()
  seed <- 11L; n_paths <- 150L; top_n <- 24L
  ed <- build_ev_draws(fx$feed, fx$layerA, slate_id = "ev-test",
                       n_paths = n_paths, top_n = top_n, seed = seed)

  schedule  <- schedule_from_feed(fx$feed)
  ml <- sample_correlated_draws(
    player_ids = ed$player_ids, layerA_draws = fx$layerA, schedule = schedule,
    corr_params = default_corr_params, n_sims = n_paths, seed = seed,
    output_format = "matrix_list")
  for (w in c(1L, 7L, 15L, 17L)) {
    M <- ml[[as.character(w)]][ed$player_ids, , drop = FALSE]
    expect_equal(ed$tensor[w, , ],
                 matrix(as.integer(round(M * 10)), nrow = nrow(M)),
                 info = sprintf("week %d", w))
  }
})

test_that("Artifact A HARD INVARIANT: same-team stack correlation survives the tensor", {
  fx <- .ev_fixture(n_layerA_sims = 1000L)
  ed <- build_ev_draws(fx$feed, fx$layerA, slate_id = "ev-test",
                       n_paths = 400L, top_n = 30L, seed = 3L)
  # p01 = QB on T01; teammates of T01 = players at indices 1, 7, 13, 19, 25.
  qb_t01 <- "p01"
  wr_t01 <- fx$pids[fx$pos == "WR" & fx$team == "T01"][1L]   # same-team WR
  wr_other <- fx$pids[fx$pos == "WR" & fx$team == "T04"][1L] # different team/game
  m_qb  <- bestballBroSim:::.ev_player_matrix(ed, qb_t01)
  m_wr  <- bestballBroSim:::.ev_player_matrix(ed, wr_t01)
  m_wro <- bestballBroSim:::.ev_player_matrix(ed, wr_other)
  # Cross-path correlation of week-1 scores: stack >> cross-team.
  same_team_cor  <- stats::cor(m_qb["1", ], m_wr["1", ])
  cross_team_cor <- stats::cor(m_qb["1", ], m_wro["1", ])
  expect_gt(same_team_cor, 0.15)               # the stack is visible
  expect_gt(same_team_cor, cross_team_cor)     # and stronger than unrelated players
})

test_that("Artifact A: write -> read round-trip is the identity (serialization preserves alignment)", {
  fx <- .ev_fixture()
  ed <- build_ev_draws(fx$feed, fx$layerA, slate_id = "ev-test",
                       n_paths = 100L, top_n = 20L, seed = 5L)
  out_dir <- file.path(tempdir(), "ev_blocks_rt")
  reg <- write_ev_draws(ed, out_dir)
  expect_true(file.exists(file.path(out_dir, reg$bin_path)))
  expect_true(file.exists(file.path(out_dir, reg$sidecar_path)))
  # Binary size = n_paths * n_players * n_weeks * 2 bytes, exactly.
  expect_equal(file.size(file.path(out_dir, reg$bin_path)),
               100 * 20 * 17 * 2)

  rt <- read_ev_draws(out_dir, "ev-test")
  expect_equal(rt$player_ids, ed$player_ids)
  expect_equal(unname(rt$positions), unname(ed$positions))
  expect_equal(rt$weeks, ed$weeks)
  expect_equal(rt$n_paths, ed$n_paths)
  expect_equal(rt$quant_scale, ed$quant_scale)
  # The tensor itself: bit-identical through the int16 binary.
  expect_identical(as.integer(rt$tensor), as.integer(ed$tensor))
  expect_equal(dim(rt$tensor), dim(ed$tensor))
})

test_that("Artifact A sidecar carries the documented contract fields", {
  fx <- .ev_fixture()
  ed <- build_ev_draws(fx$feed, fx$layerA, slate_id = "ev-test",
                       n_paths = 50L, top_n = 12L, seed = 5L)
  out_dir <- file.path(tempdir(), "ev_blocks_sc")
  write_ev_draws(ed, out_dir)
  sc <- jsonlite::fromJSON(file.path(out_dir, "v2", "ev", "ev-test_draws.json"),
                           simplifyVector = TRUE)
  expect_setequal(
    c("slate_id", "dtype", "endianness", "axis_order", "n_paths", "n_players",
      "n_weeks", "weeks", "quant_scale", "player_index", "positions",
      "generated_at"),
    names(sc))
  expect_equal(sc$dtype, "int16")
  expect_equal(sc$endianness, "little")
  expect_equal(sc$axis_order, c("path", "player", "week"))
  expect_equal(sc$quant_scale, 10)
  # player_index is 0-based and covers every player exactly once.
  idx <- unlist(sc$player_index)
  expect_setequal(idx, 0:(sc$n_players - 1L))
})

# ---- Artifact B: curves on real tournament configs ----------------------------

test_that("Puppy 2 curves: g monotone in [0,1]; payouts match the flat ladder", {
  pcfg <- load_tournament("puppy2")
  fs <- .ev_field_scores(n_field = 3000L, n_sims = 12L, seed = 1L)
  cv <- build_tournament_curves(pcfg, fs, n_grid = 128L, seed = 1L)
  expect_s3_class(cv, "bbbro_tournament_curves")
  expect_equal(cv$tournament_id, "puppy2")
  expect_equal(cv$stage_weeks$r1, 1:14)
  expect_equal(cv$stage_weeks$r4, 17L)

  for (g in c("g1", "g2", "g3")) {
    cur <- cv$curves[[g]]
    expect_true(all(cur$y >= 0 & cur$y <= 1), info = g)
    expect_true(all(diff(cur$y) >= -1e-12), info = g)       # monotone non-decreasing
    expect_lt(cur$y[1L], 0.05)                              # worst score ~ never advances
    expect_gt(cur$y[length(cur$y)], 0.95)                   # best score ~ always advances
  }
  # Puppy 2's loser buckets are flat: QF losers $5, SF losers $25, everywhere.
  expect_true(all(cv$curves$payout_qf$y == 5))
  expect_true(all(cv$curves$payout_sf$y == 25))
  # Finalist curve spans the ladder: rank 750 -> $400 up to rank 1 -> $100,000.
  expect_equal(min(cv$curves$h_final$y), 400)
  expect_equal(max(cv$curves$h_final$y), 100000)
  expect_true(all(diff(cv$curves$h_final$y) >= -1e-9))      # more points -> more $
})

test_that("Dachshund curves: rank-dependent SF bucket and $0 QF bucket", {
  dcfg <- load_tournament("dachshund")
  # Dachshund field multiple = 180; 2340 = 13 * 180.
  set.seed(2L)
  n_field <- 2340L; n_sims <- 12L
  ids <- paste0("t", seq_len(n_field))
  mk <- function(mu) matrix(pmax(0, stats::rnorm(n_field * n_sims, mu, 30)),
                            nrow = n_field, dimnames = list(ids, NULL))
  fs <- list(stage_scores = list(`1` = mk(2010), `2` = mk(150),
                                 `3` = mk(150), `4` = mk(150)))
  cv <- build_tournament_curves(dcfg, fs, n_grid = 128L, seed = 1L)
  # QF losers win $0 (omitted bucket -> engine infers $0).
  expect_true(all(cv$curves$payout_qf$y == 0))
  # SF losers: $100 for the top 156 of 780, $24 below -- the curve takes
  # exactly those two values, high scores get the bigger one.
  expect_setequal(unique(cv$curves$payout_sf$y), c(24, 100))
  expect_equal(cv$curves$payout_sf$y[length(cv$curves$payout_sf$y)], 100)
  expect_equal(cv$curves$payout_sf$y[1L], 24)
  # Finalist ladder endpoints.
  expect_equal(min(cv$curves$h_final$y), 300)
  expect_equal(max(cv$curves$h_final$y), 30000)
})

test_that("curves write -> read round-trip preserves interpolation results", {
  pcfg <- load_tournament("puppy2")
  fs <- .ev_field_scores(n_field = 900L, n_sims = 8L, seed = 3L)
  cv <- build_tournament_curves(pcfg, fs, n_grid = 64L, seed = 1L)
  out_dir <- file.path(tempdir(), "ev_blocks_curves")
  reg <- write_tournament_curves(cv, out_dir)
  expect_true(file.exists(file.path(out_dir, reg$path)))
  rt <- read_tournament_curves(out_dir, "puppy2")
  # Same interpolation results at arbitrary query points.
  q <- seq(0, 3000, length.out = 50)
  for (nm in names(cv$curves)) {
    expect_equal(bestballBroSim:::.curve_eval(rt$curves[[nm]], q),
                 bestballBroSim:::.curve_eval(cv$curves[[nm]], q),
                 tolerance = 1e-12, info = nm)
  }
  expect_equal(rt$stage_weeks$r1, cv$stage_weeks$r1)
})

# ---- Reference eval loop -------------------------------------------------------

test_that("roster_round_scores matches a direct optimize_lineup_totals computation", {
  fx <- .ev_fixture()
  ed <- build_ev_draws(fx$feed, fx$layerA, slate_id = "ev-test",
                       n_paths = 120L, top_n = 24L, seed = 9L)
  roster <- ed$player_ids[1:18]
  spec <- .ev_lineup_spec()
  stage_weeks <- list(r1 = 1:14, r2 = 15L, r3 = 16L, r4 = 17L)
  rs <- roster_round_scores(roster, ed, stage_weeks, spec)
  expect_equal(nrow(rs), 120L)

  # Direct computation from the de-quantized tensor slices.
  ml <- lapply(seq_along(ed$weeks), function(wi) {
    M <- array(ed$tensor[wi, match(roster, ed$player_ids), , drop = FALSE],
               dim = c(length(roster), ed$n_paths))
    rownames(M) <- roster
    M / ed$quant_scale
  })
  names(ml) <- as.character(ed$weeks)
  wt <- optimize_lineup_totals(ml, ed$positions[roster], spec)
  expect_equal(rs$R1, unname(rowSums(wt[, as.character(1:14)])))
  expect_equal(rs$R2, unname(wt[, "15"]))
  expect_equal(rs$R4, unname(wt[, "17"]))
})

test_that("evaluate_roster_curve_ev implements the contract combine formula", {
  fx <- .ev_fixture()
  ed <- build_ev_draws(fx$feed, fx$layerA, slate_id = "ev-test",
                       n_paths = 100L, top_n = 24L, seed = 13L)
  pcfg <- load_tournament("puppy2")
  fs <- .ev_field_scores(n_field = 900L, n_sims = 8L, seed = 3L)
  cv <- build_tournament_curves(pcfg, fs, n_grid = 64L, seed = 1L)
  spec <- .ev_lineup_spec()
  roster <- ed$player_ids[1:18]

  res <- evaluate_roster_curve_ev(roster, ed, cv, spec)
  pp <- res$per_path
  # Recompute the combine by hand from the per-path columns.
  pqf <- bestballBroSim:::.curve_eval(cv$curves$payout_qf, pp$R2)
  psf <- bestballBroSim:::.curve_eval(cv$curves$payout_sf, pp$R3)
  hf  <- bestballBroSim:::.curve_eval(cv$curves$h_final,  pp$R4)
  manual <- pp$g1 * ((1 - pp$g2) * pqf + pp$g2 * (1 - pp$g3) * psf +
                     pp$g2 * pp$g3 * hf)
  expect_equal(pp$dollars, manual, tolerance = 1e-12)
  expect_equal(res$ev, mean(manual), tolerance = 1e-12)
  expect_gte(res$ev, 0)
})

test_that("rank_marginal_ev: CRN makes a zero-scoring add exactly $0 marginal", {
  fx <- .ev_fixture()
  # Give p30 zero draws everywhere -> never cracks a lineup -> marginal EV = 0 exactly.
  fx$layerA$draw_value[fx$layerA$underdog_id == "p30"] <- 0
  ed <- build_ev_draws(fx$feed, fx$layerA, slate_id = "ev-test",
                       n_paths = 100L, top_n = 30L, seed = 17L)
  pcfg <- load_tournament("puppy2")
  fs <- .ev_field_scores(n_field = 900L, n_sims = 8L, seed = 3L)
  cv <- build_tournament_curves(pcfg, fs, n_grid = 64L, seed = 1L)
  spec <- .ev_lineup_spec()

  roster <- ed$player_ids[1:17]                      # 17-man roster, drafting the 18th
  candidates <- c("p30", setdiff(ed$player_ids, roster)[1:5])
  candidates <- unique(candidates)
  rk <- rank_marginal_ev(roster, candidates, ed, cv, spec)
  expect_setequal(rk$underdog_id, candidates)
  # Common random numbers: the all-zero player changes nothing on any path.
  expect_equal(rk$marginal_ev[rk$underdog_id == "p30"], 0, tolerance = 1e-12)
  # Every real candidate has non-negative marginal EV (adding a player can
  # only improve a best-ball lineup), and at least one is strictly positive.
  expect_true(all(rk$marginal_ev >= -1e-9))
  expect_gt(max(rk$marginal_ev), 0)
  # Output is sorted descending.
  expect_equal(rk$marginal_ev, sort(rk$marginal_ev, decreasing = TRUE))
})

# ---- publisher + _meta registration --------------------------------------------

test_that("publish_ev_blocks writes artifacts and registers them in _meta.json", {
  fx <- .ev_fixture()
  ed <- build_ev_draws(fx$feed, fx$layerA, slate_id = "nfl_2026_season",
                       n_paths = 50L, top_n = 12L, seed = 5L)
  pcfg <- load_tournament("puppy2")
  dcfg <- load_tournament("dachshund")
  fs_p <- .ev_field_scores(n_field = 900L, n_sims = 8L, seed = 3L)
  set.seed(4L)
  n_field <- 900L; n_sims <- 8L
  ids <- paste0("t", seq_len(n_field))
  mk <- function(mu) matrix(pmax(0, stats::rnorm(n_field * n_sims, mu, 30)),
                            nrow = n_field, dimnames = list(ids, NULL))
  fs_d <- list(stage_scores = list(`1` = mk(2010), `2` = mk(150),
                                   `3` = mk(150), `4` = mk(150)))
  cv_p <- build_tournament_curves(pcfg, fs_p, n_grid = 64L, seed = 1L)
  cv_d <- build_tournament_curves(dcfg, fs_d, n_grid = 64L, seed = 1L)

  out_dir <- file.path(tempdir(), "ev_blocks_pub")
  unlink(out_dir, recursive = TRUE)
  # Pre-existing _meta from publish_v2 must be preserved, not clobbered.
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(
    list(season = 2026L, generated_at = "2026-06-01T00:00:00Z",
         slates = list(nfl_2026_season = list(
           underdog_slate_id = "a9c04e81", v2_path = "v2/projections/nfl_2026_season.json",
           v2_sha256 = "abc"))),
    file.path(out_dir, "_meta.json"), auto_unbox = TRUE, pretty = TRUE)

  publish_ev_blocks(out_dir, ed, list(cv_p, cv_d))

  meta <- jsonlite::fromJSON(file.path(out_dir, "_meta.json"), simplifyVector = FALSE)
  entry <- meta$slates$nfl_2026_season
  # Old keys preserved.
  expect_equal(entry$v2_path, "v2/projections/nfl_2026_season.json")
  expect_equal(entry$v2_sha256, "abc")
  expect_equal(meta$season, 2026L)
  # New draws keys present, sha matches the file on disk.
  expect_equal(entry$v2_draws_path, "v2/ev/nfl_2026_season_draws.bin")
  expect_equal(entry$v2_draws_sha256,
               bestballBroSim:::.file_sha256(file.path(out_dir, entry$v2_draws_path)))
  expect_true(!is.null(entry$v2_draws_sidecar_path))
  # Both tournaments registered with curve paths + checksums.
  expect_setequal(names(entry$tournaments), c("puppy2", "dachshund"))
  for (tid in c("puppy2", "dachshund")) {
    treg <- entry$tournaments[[tid]]
    expect_true(file.exists(file.path(out_dir, treg$curves_path)))
    expect_equal(treg$curves_sha256,
                 bestballBroSim:::.file_sha256(file.path(out_dir, treg$curves_path)))
  }
})

# ---- real-set standing test (gated, expensive) ---------------------------------
# The contract's path-count protocol as a permanent test: marginal-EV
# ranking stability at the shipped N flags a future slate that needs more
# paths. The validation gate (curve EV vs full Layer-B sim EV) is NOT
# reproduced here: it needs real field rosters vs the real field
# (generate_field + full field scoring) to be meaningful -- synthetic
# random rosters make the comparison pod-luck-dominated. It lives in
# inst/scripts/smoke_ev_blocks.R; its numbers are reported in the PR.

test_that("real-set: marginal-EV ranking stability at the shipped path count", {
  scraper_path <- testthat::test_path("..", "..", "inst", "data",
                                      "scraped_drafts", "udbb-scraper-latest.json")
  if (!file.exists(scraper_path)) testthat::skip("Scraper export missing -- gated test.")

  N_SHIPPED <- 500L     # update if the path-count protocol re-baselines N

  # ---- shared setup: real blend + Layer A + joint path pool ----
  feed <- blend_slate(
    slate_id = "nfl_2026_season",
    sources_manifest_path = testthat::test_path("..", "..", "inst", "data",
                                                "sources", "_manifest.yaml"),
    slates_manifest_path  = testthat::test_path("..", "..", "inst", "data",
                                                "slates", "_manifest.yaml"),
    write_json = FALSE)
  sim <- simulate_slate(feed, n_sims = 1000L, seed = 1L)
  lineup_spec <- load_slate_lineup_spec("nfl_2026_season")
  # 2 disjoint resamples at the shipped N.
  ed <- build_ev_draws(feed, sim$draws, slate_id = "nfl_2026_season",
                       n_paths = 2L * N_SHIPPED, top_n = 300L, seed = 1L)
  subset_paths <- function(e, idx) {
    e$tensor <- e$tensor[, , idx, drop = FALSE]; e$n_paths <- length(idx); e
  }

  # ---- on-scale curves without the expensive field machinery: random
  # rosters scored on the draws themselves give realistic round-score
  # distributions; resample them into a conservation-valid synthetic field.
  set.seed(2L)
  pos <- ed$positions
  mk_roster <- function() {
    c(sample(names(pos)[pos == "QB"], 2L), sample(names(pos)[pos == "RB"], 6L),
      sample(names(pos)[pos == "WR"], 7L), sample(names(pos)[pos == "TE"], 3L))
  }
  rosters <- replicate(40L, mk_roster(), simplify = FALSE)
  rs_all <- do.call(rbind, lapply(rosters, function(r)
    roster_round_scores(r, subset_paths(ed, 1:200),
                        list(r1 = 1:14, r2 = 15L, r3 = 16L, r4 = 17L), lineup_spec)))
  pcfg <- load_tournament("puppy2")
  n_field <- 900L; n_sims <- 12L
  ids <- paste0("t", seq_len(n_field))
  set.seed(3L)
  mk_fs <- function(v) matrix(sample(v, n_field * n_sims, replace = TRUE),
                              nrow = n_field, dimnames = list(ids, NULL))
  fs <- list(stage_scores = list(`1` = mk_fs(rs_all$R1), `2` = mk_fs(rs_all$R2),
                                 `3` = mk_fs(rs_all$R3), `4` = mk_fs(rs_all$R4)))
  curves <- build_tournament_curves(pcfg, fs, n_grid = 128L, seed = 1L)

  # ---- (a) path-count standing test: ranking stability at shipped N ----
  adp <- vapply(feed$players, function(p) as.numeric(p$adp %||% NA_real_), numeric(1))
  names(adp) <- names(feed$players)
  adp <- adp[!is.na(adp)]; adp <- adp[names(adp) %in% ed$player_ids]
  adp_order <- names(sort(adp))
  roster8 <- adp_order[c(1L, 24L, 25L, 48L, 49L, 72L, 73L, 96L)]
  candidates <- setdiff(adp_order, roster8)[1:20]

  rank1 <- rank_marginal_ev(roster8, candidates,
                            subset_paths(ed, 1:N_SHIPPED), curves, lineup_spec)
  rank2 <- rank_marginal_ev(roster8, candidates,
                            subset_paths(ed, (N_SHIPPED + 1L):(2L * N_SHIPPED)),
                            curves, lineup_spec)
  top10_overlap <- length(intersect(utils::head(rank1$underdog_id, 10),
                                    utils::head(rank2$underdog_id, 10))) / 10
  tau <- suppressWarnings(stats::cor(match(candidates, rank1$underdog_id),
                                     match(candidates, rank2$underdog_id),
                                     method = "kendall"))
  message(sprintf("[EV path-count standing] N=%d  top10_overlap=%.2f  kendall_tau=%.3f",
                  N_SHIPPED, top10_overlap, tau))
  expect_gte(top10_overlap, 0.7)
  expect_gte(tau, 0.6)
})

test_that("publish_ev_blocks rejects curves from a different slate", {
  fx <- .ev_fixture()
  ed <- build_ev_draws(fx$feed, fx$layerA, slate_id = "some_other_slate",
                       n_paths = 50L, top_n = 12L, seed = 5L)
  pcfg <- load_tournament("puppy2")
  fs <- .ev_field_scores(n_field = 900L, n_sims = 8L, seed = 3L)
  cv <- build_tournament_curves(pcfg, fs, n_grid = 64L, seed = 1L)
  expect_error(
    publish_ev_blocks(file.path(tempdir(), "ev_blocks_bad"), ed, list(cv)),
    "same sim run")
})
