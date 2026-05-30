# Tests for R/tournament_config_generator.R. Uses an in-memory slate
# manifest and synthetic specs so the suite is filesystem-light, then
# also verifies the on-disk generator-input row for BBM7 round-trips
# against the canonical hand-written bbm7.yaml.

.test_slate_manifest <- function() {
  list(
    nfl_2026_season = list(
      underdog_slate_id = "a9c04e81-1ace-4b16-a31d-4c725a47f16f",
      display_name      = "NFL 2026 Season",
      starting_lineup   = list(slots = list(
        list(pos = "QB",   n = 1L),
        list(pos = "RB",   n = 2L),
        list(pos = "WR",   n = 3L),
        list(pos = "TE",   n = 1L),
        list(pos = "FLEX", n = 1L, eligible = c("RB", "WR", "TE"))
      )),
      position_caps = list(QB = 4L, RB = 9L, WR = 10L, TE = 5L)
    )
  )
}

.spec_puppy <- function() {
  list(
    id                     = "puppy",
    display_name           = "The Puppy",
    underdog_tournament_id = "b42dca9a-7a0c-4c32-963d-a0ddfe179e0a",
    slate_id               = "nfl_2026_season",
    stages                 = c("2/12", "1/10", "1/6"),
    final_seats            = 625L,
    entry_fee_usd          = 25L,
    description            = "Wen Puppy? - $100k to first!",
    rules_url              = "https://example.test/puppy",
    image_url              = "https://example.test/puppy.png"
  )
}

.spec_mini_golden <- function() {
  list(
    id                     = "mini_golden",
    display_name           = "The Mini Golden",
    underdog_tournament_id = "49d2b4fe-fb5c-42a4-bc6a-e9b18547535a",
    slate_id               = "nfl_2026_season",
    stages                 = c("4/12", "2/5", "1/4"),
    final_seats            = 376L,
    entry_fee_usd          = 100L
  )
}

# ---- shorthand expansion ---------------------------------------------------

test_that("Puppy shorthand -> per-stage (advance, pod) and seat chain", {
  cfg <- generate_tournament_config(.spec_puppy(),
                                    slates_manifest = .test_slate_manifest())
  expect_equal(cfg$tournament_id, "puppy")
  expect_equal(cfg$slate_id,       "nfl_2026_season")
  expect_equal(cfg$total_field_size, 225000L)

  # Per-stage (pod_size, advance n).
  s <- cfg$stages
  expect_equal(s[[1L]]$pod_size, 12L);  expect_equal(s[[1L]]$advancement$n, 2L)
  expect_equal(s[[2L]]$pod_size, 10L);  expect_equal(s[[2L]]$advancement$n, 1L)
  expect_equal(s[[3L]]$pod_size, 6L);   expect_equal(s[[3L]]$advancement$n, 1L)
  expect_equal(s[[4L]]$advancement$type, "rank_all")
  expect_equal(s[[4L]]$pod_size, 625L)
  expect_equal(s[[4L]]$seats_entering, 625L)

  # Seat chain: 625 -> *6/1 -> 3750 -> *10/1 -> 37500 -> *12/2 -> 225000.
  expect_equal(s[[1L]]$seats_entering, 225000L)
  expect_equal(s[[2L]]$seats_entering, 37500L)
  expect_equal(s[[3L]]$seats_entering, 3750L)

  # Pod-structures hard-coded per stage.
  expect_equal(s[[1L]]$pod_structure, "within_draft")
  expect_equal(s[[2L]]$pod_structure, "random")
  expect_equal(s[[3L]]$pod_structure, "random")
  expect_equal(s[[4L]]$pod_structure, "single_pod")

  # Ranking metrics: cumulative weeks 1-14 for qualifier, per-week thereafter.
  expect_equal(s[[1L]]$advancement$ranking_metric,
               "cumulative_starting_lineup_points_weeks_1_14")
  expect_equal(s[[2L]]$advancement$ranking_metric, "week_15_starting_lineup_points")
  expect_equal(s[[3L]]$advancement$ranking_metric, "week_16_starting_lineup_points")
  expect_equal(s[[4L]]$advancement$ranking_metric, "week_17_starting_lineup_points")
})

test_that("Mini Golden 4/12 vs Puppy 2/12 changes stage-1 advance count", {
  puppy <- generate_tournament_config(.spec_puppy(),
                                      slates_manifest = .test_slate_manifest())
  mgold <- generate_tournament_config(.spec_mini_golden(),
                                      slates_manifest = .test_slate_manifest())
  expect_equal(puppy$stages[[1L]]$advancement$n, 2L)
  expect_equal(mgold$stages[[1L]]$advancement$n, 4L)
  # Mini Golden chain: 376 -> 1504 -> 3760 -> 11280.
  expect_equal(mgold$total_field_size, 11280L)
  expect_equal(mgold$stages[[2L]]$seats_entering, 3760L)
  expect_equal(mgold$stages[[3L]]$seats_entering, 1504L)
})

test_that("Bad shorthand: non-integral seat chain errors clearly", {
  bad <- .spec_puppy()
  # Stage 1 with `7/12` against the puppy 625 / 3750 / 37500 chain
  # produces 37500 * 12 / 7 = 64285.71... -- not integral, so the
  # backward walk must reject it.
  bad$stages <- c("7/12", "1/10", "1/6")
  expect_error(
    generate_tournament_config(bad, slates_manifest = .test_slate_manifest()),
    "divide evenly"
  )
})

test_that("Bad shorthand format errors clearly", {
  bad <- .spec_puppy()
  bad$stages <- c("2", "1/10", "1/6")  # missing pod size
  expect_error(
    generate_tournament_config(bad, slates_manifest = .test_slate_manifest()),
    "stage shorthand"
  )
})

# ---- YAML rendering --------------------------------------------------------

test_that("rendered YAML round-trips through yaml::read_yaml with all keys", {
  cfg <- generate_tournament_config(.spec_puppy(),
                                    slates_manifest = .test_slate_manifest())
  yaml_str <- render_tournament_yaml(cfg)
  parsed <- yaml::read_yaml(text = yaml_str)
  expect_equal(parsed$tournament_id, "puppy")
  expect_equal(parsed$total_field_size, 225000L)
  expect_equal(length(parsed$stages), 4L)
  # The YAML-1.1 boolean alias `n` survives as the string key "n" because
  # we explicitly quote it -- not as the parsed boolean FALSE.
  expect_true("n" %in% names(parsed$stages[[1L]]$advancement))
  expect_false("FALSE" %in% names(parsed$stages[[1L]]$advancement))
  expect_equal(parsed$stages[[1L]]$advancement$n, 2L)
})

test_that("generator is idempotent: rerunning produces identical YAML", {
  cfg1 <- generate_tournament_config(.spec_puppy(),
                                     slates_manifest = .test_slate_manifest())
  cfg2 <- generate_tournament_config(.spec_puppy(),
                                     slates_manifest = .test_slate_manifest())
  expect_identical(cfg1, cfg2)
  expect_identical(render_tournament_yaml(cfg1), render_tournament_yaml(cfg2))
})

# ---- Scraper enrichment ----------------------------------------------------

test_that("scraper-supplied per-round IDs override the TBD fallback", {
  fake_scraper <- list(
    unkeyed = list(
      list(
        api_endpoint = "/v1/tournaments/b42dca9a-7a0c-4c32-963d-a0ddfe179e0a",
        raw_response = list(tournament = list(
          description = "scraper-desc", image_url = "scraper-img",
          rules_url = "scraper-rules", cutoff_at = "2026-09-09T00:00:00Z",
          max_entries = 150L,
          tournament_rounds = list(
            list(number = 1L, id = "real-r1"),
            list(number = 2L, id = "real-r2"),
            list(number = 3L, id = "real-r3"),
            list(number = 4L, id = "real-r4")
          )
        ))
      )
    )
  )
  spec <- .spec_puppy()
  spec$description <- NULL   # let scraper fill in
  cfg <- generate_tournament_config(spec, scraper_export = fake_scraper,
                                    slates_manifest = .test_slate_manifest())
  # All 4 stages got real round IDs -> no auto_generated flag emitted.
  expect_null(cfg$auto_generated)
  expect_equal(cfg$stages[[1L]]$underdog_round_id, "real-r1")
  expect_equal(cfg$stages[[4L]]$underdog_round_id, "real-r4")
  expect_equal(cfg$description, "scraper-desc")
})

test_that("missing scraper enrichment -> TBD round IDs + auto_generated flag", {
  cfg <- generate_tournament_config(.spec_puppy(),
                                    scraper_export = NULL,
                                    slates_manifest = .test_slate_manifest())
  expect_true(isTRUE(cfg$auto_generated))
  expect_true(all(vapply(cfg$stages,
                         function(s) s$underdog_round_id == "TBD",
                         logical(1))))
})

test_that("scraper fallback to round_tournament_index gives only qualifier ID", {
  fake_scraper <- list(
    unkeyed = list(),  # no /v1/tournaments/<uuid> endpoint
    round_tournament_index = list(
      `77a05d4c-ROUND1` = list(tournament_id =
                               "b42dca9a-7a0c-4c32-963d-a0ddfe179e0a")
    )
  )
  cfg <- generate_tournament_config(.spec_puppy(),
                                    scraper_export = fake_scraper,
                                    slates_manifest = .test_slate_manifest())
  expect_true(isTRUE(cfg$auto_generated))
  expect_equal(cfg$stages[[1L]]$underdog_round_id, "77a05d4c-ROUND1")
  expect_equal(cfg$stages[[2L]]$underdog_round_id, "TBD")
})

# ---- Canonical validator acceptance ----------------------------------------

test_that("generated configs load via load_tournaments() and pass validation", {
  # Loads the on-disk inst/data/tournaments/ directory which includes the
  # generated puppy / dachshund / mini_golden / frenchie3 alongside the
  # hand-written bbm7 / eliminator / weekly_winners / frenchie3_superflex.
  tn <- load_tournaments()
  expected <- c("bbm7", "puppy", "dachshund", "mini_golden", "frenchie3",
                "eliminator_2026", "weekly_winners_2026",
                "frenchie3_superflex_double_entry")
  expect_true(all(expected %in% names(tn)),
              info = paste("Missing:",
                paste(setdiff(expected, names(tn)), collapse = ", ")))
  # All four generator-produced configs validated.
  for (id in c("puppy", "dachshund", "mini_golden", "frenchie3")) {
    expect_true(!is.null(tn[[id]]), info = id)
    expect_equal(tn[[id]]$tournament_id, id)
  }
})

# ---- BBM7 round-trip diff (the trust check) --------------------------------

test_that("generator-produced BBM7 matches hand-written schema (advancement identical)", {
  input_path <- testthat::test_path("..", "..", "inst", "data", "tournaments",
                                    "_generator_input.yaml")
  input <- yaml::read_yaml(input_path)
  spec_bbm7 <- Filter(function(t) identical(t$id, "bbm7"), input$tournaments)[[1L]]
  expect_false(is.null(spec_bbm7))

  scraper_path <- testthat::test_path("..", "..", "inst", "data",
                                      "scraped_drafts", "udbb-scraper-latest.json")
  scraper <- if (file.exists(scraper_path)) {
    jsonlite::fromJSON(scraper_path, simplifyVector = FALSE)
  } else NULL

  gen <- generate_tournament_config(spec_bbm7, scraper_export = scraper)

  hand_path <- testthat::test_path("..", "..", "inst", "data", "tournaments",
                                   "bbm7.yaml")
  hand <- yaml::read_yaml(hand_path)

  # Structural fields the generator owns must agree exactly.
  expect_equal(gen$tournament_id, hand$tournament_id)
  expect_equal(gen$underdog_tournament_id, hand$underdog_tournament_id)
  expect_equal(gen$slate_id, hand$slate_id)
  expect_equal(gen$underdog_slate_id, hand$underdog_slate_id)
  expect_equal(gen$total_field_size, hand$total_field_size)
  expect_equal(gen$draft_size, hand$draft_size)
  expect_equal(gen$draft_rounds, hand$draft_rounds)
  expect_equal(gen$roster, hand$roster)

  # The advancement chain must be bit-identical: stage IDs, round IDs (from
  # scraper), pod_size, seats_entering, advancement.n, ranking_metric.
  expect_equal(length(gen$stages), length(hand$stages))
  for (i in seq_along(gen$stages)) {
    g <- gen$stages[[i]]; h <- hand$stages[[i]]
    expect_equal(g$id,                h$id,                info = i)
    expect_equal(g$underdog_round_id, h$underdog_round_id, info = i)
    expect_equal(g$pod_size,          h$pod_size,          info = i)
    expect_equal(g$seats_entering,    h$seats_entering,    info = i)
    expect_equal(g$advancement$type,  h$advancement$type,  info = i)
    expect_equal(g$advancement$n,     h$advancement$n,     info = i)
    expect_equal(g$advancement$ranking_metric,
                 h$advancement$ranking_metric,             info = i)
  }

  # Payouts intentionally differ -- the hand-written bbm7 has the rich
  # tiered prize table that the generator's placeholder doesn't aim to
  # replicate. The prompt explicitly says to keep the hand-written one.
  expect_false(identical(gen$payouts, hand$payouts))
})
