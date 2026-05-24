test_that("BBM 2026 config loads and validates", {
  cfg <- load_tournament_config("bbm_2026")
  expect_s3_class(cfg, "bbbro_tournament")
  expect_equal(cfg$id, "bbm_2026")
  expect_equal(cfg$scoring, "half_ppr_underdog")
  expect_length(cfg$stages, 4)

  # First stage is regular season weeks 1-14, advancing top 2 of 12
  rs <- cfg$stages[[1]]
  expect_equal(rs$id, "regular_season")
  expect_equal(rs$eligible_from, "all")
  expect_equal(rs$advancement$type, "top_n")
  expect_equal(rs$advancement$count, 2)

  # Finals is week 17 with a real payout pool
  finals <- cfg$stages[[4]]
  expect_equal(finals$id, "finals")
  expect_identical(finals$advancement, "none")
  expect_true(finals$payout_pool$total > 0)
})

test_that("Weekly Winners 2026 config loads and validates", {
  cfg <- load_tournament_config("weekly_winners_2026")
  expect_s3_class(cfg, "bbbro_tournament")
  expect_equal(cfg$id, "weekly_winners_2026")

  # 17 weeks → 17 stages
  expect_length(cfg$stages, 17)

  # Every stage has eligible_from='all' and advancement='none'
  # (proves the stage abstraction handles non-advancement tournaments)
  for (stage in cfg$stages) {
    expect_equal(stage$eligible_from, "all")
    expect_identical(stage$advancement, "none")
    expect_true(stage$payout_pool$total > 0)
  }
})

test_that("validate_tournament_config rejects missing required fields", {
  bad <- list(id = "test", name = "Test")
  expect_error(
    validate_tournament_config(bad),
    "missing required fields"
  )
})

test_that("validate_tournament_config rejects invalid eligible_from", {
  bad <- list(
    id = "test", name = "Test", scoring = "half_ppr_underdog",
    roster_slots = list(), draft = list(),
    stages = list(list(
      id = "stage1",
      weeks = list(1),
      eligible_from = "nonexistent_stage",
      pod = list(type = "draft_pod", size = 12),
      advancement = "none",
      payout_pool = 0
    ))
  )
  expect_error(
    validate_tournament_config(bad),
    "not 'all' or a prior stage id"
  )
})

test_that("validate_tournament_config rejects duplicate stage IDs", {
  one_stage <- list(
    id = "duplicated",
    weeks = list(1),
    eligible_from = "all",
    pod = list(type = "draft_pod", size = 12),
    advancement = "none",
    payout_pool = 0
  )
  bad <- list(
    id = "test", name = "Test", scoring = "half_ppr_underdog",
    roster_slots = list(), draft = list(),
    stages = list(one_stage, one_stage)
  )
  expect_error(
    validate_tournament_config(bad),
    "Duplicate stage IDs"
  )
})

test_that("list_tournaments returns all available configs", {
  ids <- list_tournaments()
  expect_true("bbm_2026" %in% ids)
  expect_true("weekly_winners_2026" %in% ids)
})
