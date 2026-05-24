slate_meta_fixture <- function() {
  list(
    underdog_slate_id = "a9c04e81-1ace-4b16-a31d-4c725a47f16f",
    display_name      = "NFL 2026 Season",
    season            = 2026,
    scoring_id        = "half_ppr_underdog"
  )
}

slate_projection_fixture <- function() {
  data.frame(
    underdog_id = c("ud-0000001", "ud-0000002", "ud-R000099"),
    gsis_id     = c("00-0034857", "00-0037077", NA_character_),
    name        = c("Josh Allen", "Bijan Robinson", "Some Rookie"),
    team        = c("BUF", "ATL", "DAL"),
    position    = c("QB", "RB", "WR"),
    season_mean = c(380.5, 287.4, 95.2),
    season_std  = c( 50.2,  58.3, 35.1),
    season_p10  = c(316.2, 215.0, 50.0),
    season_p25  = c(346.6, 248.5, 70.0),
    season_p50  = c(380.5, 281.2, 95.2),
    season_p75  = c(414.4, 322.1, 120.0),
    season_p90  = c(444.8, 363.8, 145.0),
    season_p95  = c(463.0, 388.5, 162.5),
    position_rank = c("QB1", "RB1", "WR42"),
    vor           = c(120.0, 84.0, 0.0),
    adp           = c( 85.0,  1.5, NA_real_),
    underdog_projected_points = c(372.1, 294.9, 80.4),
    stringsAsFactors = FALSE
  )
}

test_that(".projections_to_feed produces FEED_SPEC shape", {
  feed <- .projections_to_feed(slate_projection_fixture(),
                               slate_id   = "nfl_2026_season",
                               slate_meta = slate_meta_fixture())

  expect_named(feed, c("_meta", "players"))
  expect_equal(feed$`_meta`$slate_id,     "nfl_2026_season")
  expect_equal(feed$`_meta`$season,       2026)
  expect_equal(feed$`_meta`$player_count, 3)
  expect_match(feed$`_meta`$methodology,  "v1_")
  expect_length(feed$players, 3)

  allen <- feed$players[[1]]
  expect_equal(allen$underdog_id, "ud-0000001")
  expect_equal(allen$gsis_id,     "00-0034857")
  expect_equal(allen$name,        "Josh Allen")
  expect_equal(allen$team,        "BUF")
  expect_equal(allen$position,    "QB")
  expect_equal(allen$season$mean, 380.5)
  expect_equal(allen$season$std,  50.2)
  expect_equal(allen$season$percentiles$p10, 316.2)
  expect_equal(allen$season$percentiles$p90, 444.8)
  expect_equal(allen$vor,                       120.0)
  expect_equal(allen$position_rank,             "QB1")
  expect_equal(allen$adp,                       85.0)
  expect_equal(allen$underdog_projected_points, 372.1)
})

test_that(".projections_to_feed emits NULL gsis_id and NULL adp where missing", {
  feed <- .projections_to_feed(slate_projection_fixture(),
                               slate_id   = "nfl_2026_season",
                               slate_meta = slate_meta_fixture())
  rookie <- feed$players[[3]]
  expect_null(rookie$gsis_id)  # rookie has no historical match
  expect_null(rookie$adp)      # NA adp serialized as null
})

test_that("publish_projections writes a per-slate JSON file at v1/projections/<slate_id>.json", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  out_path <- publish_projections(slate_projection_fixture(),
                                  out_dir    = tmp,
                                  slate_id   = "nfl_2026_season",
                                  slate_meta = slate_meta_fixture())

  expect_true(file.exists(out_path))
  expect_equal(
    normalizePath(out_path, winslash = "/"),
    normalizePath(file.path(tmp, "v1", "projections",
                            "nfl_2026_season.json"),
                  winslash = "/")
  )

  parsed <- jsonlite::fromJSON(out_path, simplifyVector = FALSE)
  expect_named(parsed, c("_meta", "players"))
  expect_length(parsed$players, 3)
  expect_equal(parsed$players[[1]]$name, "Josh Allen")
  expect_equal(parsed$`_meta`$slate_id, "nfl_2026_season")
})

test_that("publish_manifest inventories every slate with its sha256", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  publish_projections(slate_projection_fixture(),
                      out_dir = tmp, slate_id = "nfl_2026_season",
                      slate_meta = slate_meta_fixture())

  manifest_path <- publish_manifest(
    out_dir = tmp,
    slates  = list(
      nfl_2026_season = list(
        underdog_slate_id = "a9c04e81-1ace-4b16-a31d-4c725a47f16f",
        path              = "v1/projections/nfl_2026_season.json",
        version           = "1.0.0"
      )
    ),
    season = 2026
  )

  expect_true(file.exists(manifest_path))
  m <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
  expect_equal(m$season, 2026)
  expect_true("nfl_2026_season" %in% names(m$slates))
  entry <- m$slates$nfl_2026_season
  expect_equal(entry$path, "v1/projections/nfl_2026_season.json")
  expect_equal(entry$underdog_slate_id,
               "a9c04e81-1ace-4b16-a31d-4c725a47f16f")
  expect_true(nchar(entry$sha256) > 0)
})

test_that(".project_slate output round-trips through publish_projections", {
  slate   <- make_test_slate()
  hist    <- make_test_historical()
  scoring <- load_scoring_config("half_ppr_underdog")
  drafts  <- data.frame(season = integer(0), round = integer(0),
                        pick = integer(0), position = character(0),
                        gsis_id = character(0),
                        pfr_player_name = character(0),
                        team = character(0),
                        stringsAsFactors = FALSE)

  proj <- suppressWarnings(
    .project_slate(slate, hist, drafts, scoring, season = 2026)
  )
  tmp <- tempfile(); dir.create(tmp); on.exit(unlink(tmp, recursive = TRUE))
  out_path <- publish_projections(proj, out_dir = tmp,
                                  slate_id   = "nfl_2026_season",
                                  slate_meta = slate_meta_fixture())
  expect_true(file.exists(out_path))
  parsed <- jsonlite::fromJSON(out_path, simplifyVector = FALSE)
  expect_length(parsed$players, 9)
})
