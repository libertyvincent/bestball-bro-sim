slate_meta_fixture <- function() {
  list(
    underdog_slate_id = "a9c04e81-1ace-4b16-a31d-4c725a47f16f",
    display_name      = "NFL 2026 Season",
    season            = 2026,
    scoring_id        = "half_ppr_underdog"
  )
}

test_that("publish_manifest inventories every slate with its sha256", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  # Write a feed file by hand so the manifest has something to checksum.
  feed_path <- file.path(tmp, "v2", "projections", "nfl_2026_season.json")
  dir.create(dirname(feed_path), recursive = TRUE)
  jsonlite::write_json(
    list(`_meta` = slate_meta_fixture(), players = list()),
    feed_path, auto_unbox = TRUE, pretty = TRUE
  )

  manifest_path <- publish_manifest(
    out_dir = tmp,
    slates  = list(
      nfl_2026_season = list(
        underdog_slate_id = "a9c04e81-1ace-4b16-a31d-4c725a47f16f",
        path              = "v2/projections/nfl_2026_season.json",
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
  expect_equal(entry$path, "v2/projections/nfl_2026_season.json")
  expect_equal(entry$underdog_slate_id,
               "a9c04e81-1ace-4b16-a31d-4c725a47f16f")
  expect_true(nchar(entry$sha256) > 0)
})

test_that("publish_manifest writes empty sha256 for a missing feed file", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  manifest_path <- publish_manifest(
    out_dir = tmp,
    slates  = list(
      nfl_2026_season = list(
        underdog_slate_id = "a9c04e81-1ace-4b16-a31d-4c725a47f16f",
        path              = "v2/projections/does_not_exist.json",
        version           = "1.0.0"
      )
    ),
    season = 2026
  )

  m <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
  expect_equal(m$slates$nfl_2026_season$sha256, "")
})
