# Regeneration test for the extension's golden EV-combine fixture
# (inst/fixtures/ev_combine_fixture_puppy2.json, emitted by
# inst/scripts/make_ev_combine_fixture.R).
#
# Reconstructs ev_draws / curves / lineup_spec / stage_weeks from ONLY the
# embedded `inputs`, re-runs the R reference loop (roster_round_scores ->
# evaluate_roster_curve_ev -> rank_marginal_ev), and asserts it reproduces
# the stored `expected` within float tolerance. This keeps the fixture
# self-consistent if the reference ever changes -- the fixture doubles as a
# regression test of the reference itself -- and pins the contract the JS
# port must match (including the uncovered-id handling).

# everything-as-lists parse so an `eligible` that is sometimes scalar,
# sometimes a vector, never collapses into an ambiguous data.frame.
.fix_path <- function() testthat::test_path("..", "..", "inst", "fixtures",
                                            "ev_combine_fixture_puppy2.json")

.num <- function(x) as.numeric(unlist(x, use.names = FALSE))
.int <- function(x) as.integer(unlist(x, use.names = FALSE))
.chr <- function(x) as.character(unlist(x, use.names = FALSE))

# Rebuild a bbbro_ev_draws from the embedded flat [path][player][week] int16
# buffer (== as.integer() of an R [week,player,path] column-major array).
.rebuild_draws <- function(inp) {
  d <- inp$draws
  n_paths <- as.integer(d$n_paths); n_players <- as.integer(d$n_players)
  n_weeks <- as.integer(d$n_weeks)
  pidx <- vapply(d$player_index, as.integer, integer(1))   # id -> 0-based
  player_ids <- names(sort(pidx))                          # canonical row order
  positions <- vapply(d$positions[player_ids], as.character, character(1))
  tensor <- array(.int(d$values), dim = c(n_weeks, n_players, n_paths))
  structure(list(
    tensor = tensor, player_ids = player_ids, positions = positions,
    weeks = .int(inp$weeks), n_paths = n_paths,
    quant_scale = .num(inp$quant_scale), slate_id = inp$slate),
    class = "bbbro_ev_draws")
}

.rebuild_curves <- function(inp) {
  cv <- lapply(inp$curves, function(g) list(x = .num(g$x), y = .num(g$y)))
  sw <- inp$stage_weeks
  structure(list(
    stage_weeks = list(r1 = .int(sw$r1), r2 = .int(sw$r2),
                       r3 = .int(sw$r3), r4 = .int(sw$r4)),
    curves = cv), class = "bbbro_tournament_curves")
}

.rebuild_spec <- function(inp) {
  slots <- lapply(inp$lineup_spec$slots, function(s) list(
    pos = as.character(s$pos), n = as.integer(s$n), eligible = .chr(s$eligible)))
  list(slate_id = as.character(inp$lineup_spec$slate_id), slots = slots)
}

test_that("golden fixture: round scores, EV, and ranking regenerate from embedded inputs", {
  fx <- jsonlite::fromJSON(.fix_path(), simplifyVector = FALSE)
  inp <- fx$inputs; exp <- fx$expected

  ed     <- .rebuild_draws(inp)
  curves <- .rebuild_curves(inp)
  spec   <- .rebuild_spec(inp)
  sw     <- curves$stage_weeks
  roster <- .chr(inp$test_roster)

  # sanity on the reconstruction itself
  expect_s3_class(ed, "bbbro_ev_draws")
  expect_equal(ed$n_paths, 64L)
  expect_equal(dim(ed$tensor), c(length(ed$weeks), length(ed$player_ids), 64L))
  expect_true(all(roster %in% ed$player_ids))

  # ---- round scores R1..R4 (full 64-path arrays) ----
  rs <- roster_round_scores(roster, ed, sw, spec)
  expect_equal(rs$R1, .num(exp$round_scores$R1), tolerance = 1e-6)
  expect_equal(rs$R2, .num(exp$round_scores$R2), tolerance = 1e-6)
  expect_equal(rs$R3, .num(exp$round_scores$R3), tolerance = 1e-6)
  expect_equal(rs$R4, .num(exp$round_scores$R4), tolerance = 1e-6)

  # ---- roster EV (the combine) ----
  ev <- evaluate_roster_curve_ev(roster, ed, curves, spec)$ev
  expect_equal(ev, .num(exp$roster_ev), tolerance = 1e-6)

  # ---- marginal ranking of the COVERED candidates ----
  cand_meta <- inp$candidates
  covered_cands <- vapply(cand_meta, function(c) as.character(c$id),
                          character(1))[vapply(cand_meta, function(c) isTRUE(c$covered), logical(1))]
  rk <- rank_marginal_ev(roster, covered_cands, ed, curves, spec)
  exp_ids <- vapply(exp$marginal_ranking, function(r) as.character(r$id), character(1))
  exp_mev <- vapply(exp$marginal_ranking, function(r) as.numeric(r$marginal_ev), numeric(1))
  expect_equal(rk$underdog_id, exp_ids)                 # same order
  expect_equal(rk$marginal_ev, exp_mev, tolerance = 1e-6)
  expect_equal(rk$marginal_ev, sort(rk$marginal_ev, decreasing = TRUE))  # sorted desc
})

test_that("golden fixture: uncovered candidate is flagged and never silently scored", {
  fx <- jsonlite::fromJSON(.fix_path(), simplifyVector = FALSE)
  inp <- fx$inputs; exp <- fx$expected
  ed <- .rebuild_draws(inp); curves <- .rebuild_curves(inp); spec <- .rebuild_spec(inp)
  roster <- .chr(inp$test_roster)

  uncovered <- .chr(exp$uncovered_candidates)
  expect_true(length(uncovered) >= 1L)
  # The fixture declares it uncovered, it is outside the tensor universe,
  # and it is absent from the expected ranking.
  cand_meta <- inp$candidates
  declared_uncovered <- vapply(cand_meta, function(c) as.character(c$id),
                               character(1))[!vapply(cand_meta, function(c) isTRUE(c$covered), logical(1))]
  expect_setequal(declared_uncovered, uncovered)
  expect_false(any(uncovered %in% ed$player_ids))
  exp_ids <- vapply(exp$marginal_ranking, function(r) as.character(r$id), character(1))
  expect_false(any(uncovered %in% exp_ids))

  # Contract: the reference does NOT silently score an uncovered id -- asking
  # it to rank one aborts (the extension must detect & exclude it instead).
  expect_error(
    rank_marginal_ev(roster, uncovered, ed, curves, spec),
    "not present in the EV draws")
})
