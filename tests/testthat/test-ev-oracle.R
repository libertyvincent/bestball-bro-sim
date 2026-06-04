# Tests for R/ev_oracle.R -- the co-moving low-variance EV oracle used as
# the trusted reference for the Phase-2 v1-vs-v2 re-gate.
#
# Pins the structural invariants the re-gate decision rests on:
#   * advancement closed forms (.oracle_adv_prob mirrors g1/g2/g3)
#   * oracle_cfg_struct parses the Puppy 2 bracket + ladder
#   * SEAT-CONSERVATION: the field survival weights integrate to the bracket
#     seat ratios (the survivor-carry is self-consistent; the leave-one-out
#     percentile is what keeps the deep rounds from inflating -- dropping it
#     pushes the final reach to ~134% and fails this test)
#   * oracle_roster_ev bounds / monotonicity / per-path mean == ev
#   * CRN: an all-zero add is exactly $0 marginal under the co-moving path set
#   * oracle-static tracks the v1 curve EV (it is the v1-equivalent evaluator)

# ---- synthetic fixture (compact; mirrors the ev-blocks shape) ------------------

.oracle_fixture <- function(n_layerA_sims = 600L, seed = 42L) {
  set.seed(seed)
  pids <- paste0("p", sprintf("%02d", 1:30))
  pos  <- c(rep("QB", 5), rep("RB", 8), rep("WR", 12), rep("TE", 5))
  team <- paste0("T", sprintf("%02d", rep(1:6, length.out = 30)))
  names(pos) <- pids; names(team) <- pids
  mean_by_pos <- c(QB = 18, RB = 12, WR = 13, TE = 9)
  # Per-player skill decreasing with ADP (p01 best -> p30 worst) so that
  # roster strength is real (low-ADP rosters genuinely outscore high-ADP).
  skill <- stats::setNames(seq(1.25, 0.75, length.out = length(pids)), pids)
  pmean <- function(p) mean_by_pos[[pos[p]]] * skill[[p]]
  players <- list()
  for (i in seq_along(pids)) {
    p <- pids[i]
    weekly <- lapply(1:17, function(w) {
      is_bye <- (w == 7L && team[p] %in% c("T01", "T02"))
      list(week = w, opponent = if (is_bye) NULL else paste0("OPP_", team[p]),
           is_bye = is_bye)
    })
    players[[p]] <- list(underdog_id = p, position = unname(pos[p]),
                         team = unname(team[p]), adp = i,
                         season_mean = pmean(p) * 16, weekly = weekly)
  }
  layerA <- do.call(rbind, lapply(1:17, function(w) {
    do.call(rbind, lapply(pids, function(p) {
      is_bye <- (w == 7L && team[p] %in% c("T01", "T02"))
      vals <- if (is_bye) rep(0, n_layerA_sims) else
        pmax(0, stats::rnorm(n_layerA_sims, pmean(p), pmean(p) * 0.45))
      data.frame(underdog_id = p, sim_idx = seq_len(n_layerA_sims), week = w,
                 draw_value = vals, stringsAsFactors = FALSE)
    }))
  }))
  list(feed = list(players = players), layerA = layerA, pids = pids, pos = pos)
}

.oracle_lineup_spec <- function() list(slate_id = "ev-test", slots = list(
  list(pos = "QB", n = 1L, eligible = "QB"), list(pos = "RB", n = 2L, eligible = "RB"),
  list(pos = "WR", n = 3L, eligible = "WR"), list(pos = "TE", n = 1L, eligible = "TE"),
  list(pos = "FLEX", n = 1L, eligible = c("RB", "WR", "TE"))))

# Build ev_draws + a field of random rosters scored on those paths + the
# oracle field object. Cached per-call inputs are deterministic (seeded).
.oracle_setup <- function(n_paths = 250L, n_field = 800L, seed = 7L) {
  fx <- .oracle_fixture()
  ed <- build_ev_draws(fx$feed, fx$layerA, slate_id = "nfl_2026_season",
                       n_paths = n_paths, top_n = 30L, seed = seed)
  spec <- .oracle_lineup_spec()
  sw <- list(r1 = 1:14, r2 = 15L, r3 = 16L, r4 = 17L)
  pos <- ed$positions
  qb <- names(pos)[pos == "QB"]; rb <- names(pos)[pos == "RB"]
  wr <- names(pos)[pos == "WR"]; te <- names(pos)[pos == "TE"]
  set.seed(seed + 1L)
  field <- stats::setNames(lapply(seq_len(n_field), function(i)
    c(sample(qb, 2L), sample(rb, 6L), sample(wr, 7L), sample(te, 3L))),
    paste0("f", seq_len(n_field)))
  cs <- oracle_cfg_struct(load_tournament("puppy2"))
  scored <- oracle_score_field(field, ed, sw, spec, min_covered = 15L)
  ofield <- oracle_build_field(scored, cs)
  list(ed = ed, spec = spec, sw = sw, cs = cs, ofield = ofield, pos = pos)
}

# ---- advancement closed forms -------------------------------------------------

test_that(".oracle_adv_prob reproduces the Puppy 2 g1/g2/g3 closed forms", {
  p <- c(0, 0.25, 0.5, 0.8, 1)
  g1 <- bestballBroSim:::.oracle_adv_prob(p, 12L, 2L)   # top 2 of 12
  g2 <- bestballBroSim:::.oracle_adv_prob(p, 10L, 1L)   # top 1 of 10
  g3 <- bestballBroSim:::.oracle_adv_prob(p,  5L, 1L)   # top 1 of 5
  expect_equal(g1, p^11 + 11 * (1 - p) * p^10)
  expect_equal(g2, p^9)
  expect_equal(g3, p^4)
  # boundary sanity
  expect_equal(g1[1], 0); expect_equal(g1[5], 1)
  expect_equal(unname(g3[3]), 0.0625)                   # 0.5^4
})

# ---- config parsing -----------------------------------------------------------

test_that("oracle_cfg_struct reads the Puppy 2 bracket, ladder, and loser buckets", {
  cs <- oracle_cfg_struct(load_tournament("puppy2"))
  expect_equal(cs$pod, c(12L, 10L, 5L, 750L))
  expect_equal(cs$advn[1:3], c(2L, 1L, 1L))
  expect_equal(cs$seats, c(225000L, 37500L, 3750L, 750L))
  expect_equal(cs$qf_usd, 5)        # quarterfinals_loser flat
  expect_equal(cs$sf_usd, 25)       # semifinals_loser flat
  # finalist ladder: top tier $100k at rank 1, and the bucket totals to
  # the prize pool minus the two loser buckets.
  expect_equal(cs$finalist_tiers[[1]], list(from = 1L, to = 1L, usd = 100000))
  fin_total <- sum(vapply(cs$finalist_tiers, function(t) t$usd * (t$to - t$from + 1L), numeric(1)))
  expect_equal(fin_total, 1e6 - 25 * 3000 - 5 * 33750)   # = 756250
})

# ---- seat-conservation (the survivor-carry + leave-one-out invariant) ---------

test_that("field survival weights integrate to the bracket seat ratios", {
  s <- .oracle_setup(n_paths = 250L, n_field = 800L, seed = 7L)
  W <- s$ofield$W; seats <- s$cs$seats
  # Mean entering-weight per round == fraction of the field reaching that
  # round == cumulative seat ratio. Closed-form targets: 2/12, 2/12*1/10,
  # *1/5. Finite-field tolerances; without leave-one-out the deep rounds
  # inflate well past these (final ~134%).
  expect_equal(mean(W[[2]]), seats[2] / seats[1], tolerance = 0.06)   # QF ~0.1667
  expect_equal(mean(W[[3]]), seats[3] / seats[1], tolerance = 0.12)   # SF ~0.01667
  expect_equal(mean(W[[4]]), seats[4] / seats[1], tolerance = 0.22)   # FIN ~0.003333
  # weights are valid survival probabilities, monotone non-increasing round to round
  for (k in 1:4) expect_true(all(W[[k]] >= 0 & W[[k]] <= 1))
  expect_true(all(W[[4]] <= W[[3]] & W[[3]] <= W[[2]] & W[[2]] <= W[[1]]))
})

# ---- single-roster EV: bounds, monotonicity, per-path identity ----------------

test_that("oracle_roster_ev is bounded, monotone in reach, and per-path consistent", {
  s <- .oracle_setup()
  roster <- names(s$pos)[1:18]
  rs <- roster_round_scores(roster, s$ed, s$sw, s$spec)
  for (fm in c("binom", "rank")) for (md in c("comove", "static")) {
    r <- oracle_roster_ev(rs, s$ofield, mode = md, final_mode = fm)
    expect_gte(r$ev, 0)
    expect_equal(r$ev, mean(r$per_path), tolerance = 1e-12)
    expect_equal(length(r$per_path), s$ofield$N)
    expect_true(all(r$reach >= 0 & r$reach <= 1))
    expect_true(r$reach["qf"] >= r$reach["sf"] && r$reach["sf"] >= r$reach["final"])
  }
})

test_that("a path-dominating roster reaches the final far more than a weak one", {
  s <- .oracle_setup()
  # Strongest available by season mean vs weakest, both cap-valid.
  pos <- s$pos
  strong <- c(names(pos)[pos == "QB"][1:2], names(pos)[pos == "RB"][1:6],
              names(pos)[pos == "WR"][1:7], names(pos)[pos == "TE"][1:3])
  weak   <- c(rev(names(pos)[pos == "QB"])[1:2], rev(names(pos)[pos == "RB"])[1:6],
              rev(names(pos)[pos == "WR"])[1:7], rev(names(pos)[pos == "TE"])[1:3])
  rs_s <- roster_round_scores(strong, s$ed, s$sw, s$spec)
  rs_w <- roster_round_scores(weak,   s$ed, s$sw, s$spec)
  es <- oracle_roster_ev(rs_s, s$ofield, mode = "comove")
  ew <- oracle_roster_ev(rs_w, s$ofield, mode = "comove")
  expect_gt(es$reach["final"], ew$reach["final"])
  expect_gt(es$ev, ew$ev)
})

# ---- CRN: an all-zero add is exactly $0 marginal ------------------------------

test_that("oracle marginal of an all-zero-scoring add is exactly $0 (CRN)", {
  fx <- .oracle_fixture()
  fx$layerA$draw_value[fx$layerA$underdog_id == "p30"] <- 0   # never cracks a lineup
  ed <- build_ev_draws(fx$feed, fx$layerA, slate_id = "nfl_2026_season",
                       n_paths = 200L, top_n = 30L, seed = 9L)
  spec <- .oracle_lineup_spec(); sw <- list(r1 = 1:14, r2 = 15L, r3 = 16L, r4 = 17L)
  cs <- oracle_cfg_struct(load_tournament("puppy2"))
  pos <- ed$positions
  set.seed(3L)
  field <- stats::setNames(lapply(1:600, function(i)
    c(sample(names(pos)[pos == "QB"], 2L), sample(names(pos)[pos == "RB"], 6L),
      sample(names(pos)[pos == "WR"], 7L), sample(names(pos)[pos == "TE"], 3L))),
    paste0("f", 1:600))
  ofield <- oracle_build_field(oracle_score_field(field, ed, sw, spec, min_covered = 15L), cs)
  # base 17-man roster that does NOT include p30, then add p30.
  base <- setdiff(names(pos), "p30")[1:17]
  rs_base <- roster_round_scores(base, ed, sw, spec)
  rs_add  <- roster_round_scores(c(base, "p30"), ed, sw, spec)
  m <- oracle_roster_ev(rs_add, ofield, mode = "comove")$ev -
       oracle_roster_ev(rs_base, ofield, mode = "comove")$ev
  expect_equal(m, 0, tolerance = 1e-9)
})

# ---- oracle-static tracks the v1 curve EV -------------------------------------

test_that("oracle-static ranks rosters like the v1 curve EV", {
  s <- .oracle_setup(n_paths = 300L, n_field = 800L, seed = 11L)
  # v1 curves from a synthetic conservation-shaped field.
  set.seed(1L); nf <- 3000L; ns <- 12L; ids <- paste0("t", seq_len(nf))
  mk <- function(mu) matrix(pmax(0, stats::rnorm(nf * ns, mu, 30)), nrow = nf,
                            dimnames = list(ids, NULL))
  fs <- list(stage_scores = list(`1` = mk(2010), `2` = mk(150), `3` = mk(150), `4` = mk(150)))
  cv <- build_tournament_curves(load_tournament("puppy2"), fs, n_grid = 128L, seed = 1L)
  pos <- s$pos
  # six rosters spanning strong -> weak by season order
  ord <- c(names(pos)[pos == "QB"], names(pos)[pos == "RB"],
           names(pos)[pos == "WR"], names(pos)[pos == "TE"])
  mk_roster <- function(shift) c(
    names(pos)[pos == "QB"][((0:1 + shift) %% 5) + 1],
    names(pos)[pos == "RB"][((0:5 + shift) %% 8) + 1],
    names(pos)[pos == "WR"][((0:6 + shift) %% 12) + 1],
    names(pos)[pos == "TE"][((0:2 + shift) %% 5) + 1])
  rosters <- lapply(0:5, mk_roster)
  v1 <- vapply(rosters, function(r) evaluate_roster_curve_ev(r, s$ed, cv, s$spec)$ev, numeric(1))
  os <- vapply(rosters, function(r)
    oracle_roster_ev(roster_round_scores(r, s$ed, s$sw, s$spec), s$ofield, mode = "static")$ev,
    numeric(1))
  # Same static evaluator family: strongly positively correlated, same #1.
  expect_gt(stats::cor(v1, os), 0.7)
  expect_equal(which.max(v1), which.max(os))
})
