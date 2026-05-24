test_that(".projections_to_feed produces FEED_SPEC shape", {
  proj <- data.frame(
    name = c("Josh Allen", "Bijan Robinson"),
    team = c("BUF", "ATL"),
    position = c("QB", "RB"),
    gsis_id = c("00-0034857", "00-0037077"),
    season_mean = c(380.5, 287.4),
    season_std  = c( 50.2,  58.3),
    season_p10  = c(316.2, 215.0),
    season_p25  = c(346.6, 248.5),
    season_p50  = c(380.5, 281.2),
    season_p75  = c(414.4, 322.1),
    season_p90  = c(444.8, 363.8),
    season_p95  = c(463.0, 388.5),
    vor         = c(120.0,  84.0),
    position_rank = c("QB1", "RB1"),
    stringsAsFactors = FALSE
  )

  feed <- .projections_to_feed(proj, season = 2026)

  expect_named(feed, c("_meta", "players"))
  expect_equal(feed$`_meta`$season, 2026)
  expect_equal(feed$`_meta`$n_players, 2)
  expect_match(feed$`_meta`$methodology, "v0")
  expect_length(feed$players, 2)

  p1 <- feed$players[[1]]
  expect_equal(p1$name, "Josh Allen")
  expect_equal(p1$team, "BUF")
  expect_equal(p1$position, "QB")
  expect_equal(p1$gsis_id, "00-0034857")
  expect_equal(p1$season$mean, 380.5)
  expect_equal(p1$season$std, 50.2)
  expect_equal(p1$season$percentiles$p10, 316.2)
  expect_equal(p1$season$percentiles$p90, 444.8)
  expect_equal(p1$vor, 120.0)
  expect_equal(p1$position_rank, "QB1")
})

test_that("publish_projections writes a real JSON file", {
  proj <- data.frame(
    name = "Test Player", team = "DAL", position = "WR",
    gsis_id = "00-9999999",
    season_mean = 200, season_std = 40,
    season_p10 = 150, season_p25 = 175, season_p50 = 200,
    season_p75 = 225, season_p90 = 250, season_p95 = 265,
    vor = 50, position_rank = "WR1",
    stringsAsFactors = FALSE
  )

  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  out_path <- publish_projections(proj, out_dir = tmp, season = 2026)
  expect_true(file.exists(out_path))
  expect_equal(
    normalizePath(out_path, winslash = "/"),
    normalizePath(file.path(tmp, "v1", "projections", "nfl_2026.json"),
                  winslash = "/")
  )

  parsed <- jsonlite::fromJSON(out_path, simplifyVector = FALSE)
  expect_named(parsed, c("_meta", "players"))
  expect_length(parsed$players, 1)
  expect_equal(parsed$players[[1]]$name, "Test Player")
})

test_that("publish_manifest inventories the projections file when present", {
  proj <- data.frame(
    name = "Test", team = "BUF", position = "QB", gsis_id = "00-1",
    season_mean = 300, season_std = 50,
    season_p10 = 230, season_p25 = 270, season_p50 = 300,
    season_p75 = 330, season_p90 = 365, season_p95 = 380,
    vor = 0, position_rank = "QB1",
    stringsAsFactors = FALSE
  )

  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  publish_projections(proj, out_dir = tmp, season = 2026)
  manifest_path <- publish_manifest(out_dir = tmp, season = 2026)

  expect_true(file.exists(manifest_path))
  m <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
  expect_equal(m$season, 2026)
  expect_equal(m$files$projections$path, "v1/projections/nfl_2026.json")
  expect_true(nchar(m$files$projections$sha256) > 0)
})
