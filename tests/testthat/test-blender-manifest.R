# Sanity-check the published sources manifest -- the blender depends on
# its top-level shape and on every enabled slate having a slate_weights
# entry. If somebody edits the YAML and breaks the schema, this catches it.

test_that("sources manifest parses and has the expected top-level keys", {
  path <- .inst_path("data/sources", "_manifest.yaml")
  expect_true(nzchar(path), info = "sources manifest must exist")
  m <- yaml::read_yaml(path)
  expect_setequal(
    names(m),
    c("data_repo_base_url", "sources", "slate_weights", "aleatoric_cv")
  )
  expect_match(m$data_repo_base_url, "^https?://", perl = TRUE)
})

test_that("every declared source has the fields the blender consumes", {
  m <- yaml::read_yaml(.inst_path("data/sources", "_manifest.yaml"))
  expect_setequal(names(m$sources), c("clay", "etr", "legup"))
  for (nm in names(m$sources)) {
    src <- m$sources[[nm]]
    expect_true(!is.null(src$enabled),
                info = sprintf("%s$enabled must be present", nm))
    expect_true(!is.null(src$format),
                info = sprintf("%s$format must be present", nm))
    expect_true(!is.null(src$type),
                info = sprintf("%s$type must be present", nm))
  }
  # Clay specifically needs the weekly + grades sub-URLs the blender pulls.
  expect_true(!is.null(m$sources$clay$feed_url))
  expect_true(!is.null(m$sources$clay$weekly_team_scoring_url))
})

test_that("aleatoric_cv covers all skill positions with sane CV magnitudes", {
  m <- yaml::read_yaml(.inst_path("data/sources", "_manifest.yaml"))
  expect_setequal(names(m$aleatoric_cv), c("QB", "RB", "WR", "TE"))
  for (pos in names(m$aleatoric_cv)) {
    cv <- as.numeric(m$aleatoric_cv[[pos]])
    expect_true(cv > 0 && cv < 1,
                info = sprintf("%s cv out of (0,1): %s", pos, cv))
  }
})

test_that("every enabled slate has a slate_weights entry summing to ~1", {
  slates <- load_slate_manifest()
  enabled <- names(Filter(function(s) isTRUE(s$enabled), slates))
  m <- yaml::read_yaml(.inst_path("data/sources", "_manifest.yaml"))
  for (sid in enabled) {
    w <- m$slate_weights[[sid]]
    expect_true(!is.null(w),
                info = sprintf("slate_weights missing for %s", sid))
    total <- sum(unlist(w))
    expect_equal(total, 1.0, tolerance = 0.01,
                 info = sprintf("%s weights sum to %s, expected ~1",
                                sid, total))
  }
})
