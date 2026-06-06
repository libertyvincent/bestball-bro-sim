# Integration: publish_v2 -> publish_ev_blocks_pipeline end to end.
#
# Runs the real deploy wiring (Sprint: "Publish the EV-blocks artifacts"):
# publish_v2 writes the Layer A parquet + the _meta v2 keys; the pipeline
# reuses that parquet to build Artifact A (int16 tensor) + the per-tournament
# curves and MERGES the EV keys into the same _meta.json. Asserts the gate:
# files present, EV keys present, v2 projection keys NOT clobbered, shas match
# the deployed files, sidecar/bin shapes correct.
#
# Network on first run (gh-pages source feeds), then warm-cache. Skipped
# without arrow/dplyr or a cold cache, exactly like test-publish-v2-integration.

.ev_cache_warm <- function() {
  cache_dir <- file.path("~", ".bestball-bro", "cache")
  if (!dir.exists(cache_dir)) return(FALSE)
  expected <- c("sources/clay_2026_offense.json",
                "sources/clay_2026_weekly_team_scoring.json",
                "sources/etr_2026_season.json", "sources/legup_2026_ud.json")
  base <- "https://libertyvincent.github.io/bestball-bro-data"
  keys <- vapply(expected, function(p)
    digest::digest(paste0(base, "/", p), algo = "sha256", serialize = FALSE), character(1))
  all(file.exists(file.path(cache_dir, paste0(keys, ".json"))))
}

test_that("publish_ev_blocks_pipeline lands v2/ev/* and merges _meta without clobbering v2 keys", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("dplyr")
  skip_if_not(.ev_cache_warm(),
              "Source cache cold; run blend_slate('nfl_2026_season', ...) once first.")

  td <- tempfile("ev_pub_int_"); dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)

  # 1. publish_v2 (tiny) authors _meta v2 keys + writes the Layer A parquet.
  publish_v2(out_dir = td, n_sims = 100L, seed = 42L)
  m0 <- jsonlite::fromJSON(file.path(td, "_meta.json"), simplifyVector = FALSE)
  v2_path0 <- m0$slates$nfl_2026_season$v2_path
  v2_sha0  <- m0$slates$nfl_2026_season$v2_sha256
  expect_true(file.exists(file.path(td, "v2", "draws", "nfl_2026_season.parquet")))

  # 2. the EV pipeline (fast field; the wiring is under test, not curve
  # quality). field_targets pins the ADP-only CI path (no scraped drafts),
  # so the test exercises exactly what production CI runs.
  cfg <- list(list(slate_id = "nfl_2026_season",
                   tournament_ids = c("puppy2", "dachshund"),
                   n_paths = 64L, top_n = 80L, field_teams = 900L, field_sims = 20L,
                   field_targets = bestballBroSim:::.default_field_targets()))
  publish_ev_blocks_pipeline(td, config = cfg, marginal_sims = 100L, seed = 1L)

  # ---- files present in the publish dir ----
  for (f in c("v2/ev/nfl_2026_season_draws.bin", "v2/ev/nfl_2026_season_draws.json",
              "v2/ev/puppy2_curves.json", "v2/ev/dachshund_curves.json")) {
    expect_true(file.exists(file.path(td, f)), info = f)
  }

  m <- jsonlite::fromJSON(file.path(td, "_meta.json"), simplifyVector = FALSE)
  e <- m$slates$nfl_2026_season
  # ---- EV keys present ----
  expect_equal(e$v2_draws_path, "v2/ev/nfl_2026_season_draws.bin")
  expect_equal(e$v2_draws_sidecar_path, "v2/ev/nfl_2026_season_draws.json")
  expect_true(nchar(e$v2_draws_sha256) > 32L)
  expect_true(nchar(e$v2_draws_sidecar_sha256) > 32L)
  expect_false(is.null(e$v2_draws_generated_at))
  expect_equal(e$tournaments$puppy2$curves_path, "v2/ev/puppy2_curves.json")
  expect_equal(e$tournaments$dachshund$curves_path, "v2/ev/dachshund_curves.json")
  expect_true(nchar(e$tournaments$puppy2$curves_sha256) > 32L)
  expect_true(nchar(e$tournaments$dachshund$curves_sha256) > 32L)
  # ---- the live-draft bridge: source_id UUID + title per tournament ----
  # The extension maps a live draft's source_id -> tid -> curves via these.
  # Values are the draft's tournament UUID (puppy2 from the Sprint-2 live
  # capture; dachshund from the scraped draft exports) + the display title.
  expect_equal(e$tournaments$puppy2$underdog_tournament_id,
               "e9f88543-f815-4db2-a076-1271fb35160c")
  expect_equal(e$tournaments$puppy2$title, "The Puppy 2")
  expect_equal(e$tournaments$dachshund$underdog_tournament_id,
               "1f35c88b-e5c4-4b8c-b42a-db74f55d7a18")
  expect_equal(e$tournaments$dachshund$title, "The Dachshund")

  # ---- merge-without-clobber: the v2 projection keys survive ----
  expect_equal(e$v2_path, v2_path0)
  expect_equal(e$v2_sha256, v2_sha0)
  expect_equal(m$season, 2026L)

  # ---- shas in _meta match the deployed files (the freshness gate) ----
  sha <- function(rel) bestballBroSim:::.file_sha256(file.path(td, rel))
  expect_equal(sha(e$v2_draws_path), e$v2_draws_sha256)
  expect_equal(sha(e$v2_draws_sidecar_path), e$v2_draws_sidecar_sha256)
  expect_equal(sha(e$tournaments$puppy2$curves_path), e$tournaments$puppy2$curves_sha256)
  expect_equal(sha(e$tournaments$dachshund$curves_path), e$tournaments$dachshund$curves_sha256)

  # ---- sidecar / bin shape: player_index count and N*players*17*2 bytes ----
  sc <- jsonlite::fromJSON(file.path(td, e$v2_draws_sidecar_path), simplifyVector = TRUE)
  np <- length(sc$player_index)
  expect_true(np > 40L && np <= 80L)
  expect_equal(sc$n_weeks, 17L)
  expect_equal(file.size(file.path(td, e$v2_draws_path)), 64L * np * 17L * 2L)

  # ---- the sidecar is self-describing: it carries the Season lineup_spec ----
  # (QB1 / RB2 / WR3 / TE1 / FLEX1[RB,WR,TE]) in the #30 fixture shape, so the
  # extension's round-score assembler has lineup_spec + stage_weeks from live
  # data. Equals load_slate_lineup_spec() and the fixture's inputs.lineup_spec.
  expect_equal(sc$lineup_spec$slate_id, "nfl_2026_season")
  expect_equal(sc$lineup_spec$slots$pos, c("QB", "RB", "WR", "TE", "FLEX"))
  expect_equal(sc$lineup_spec$slots$n, c(1L, 2L, 3L, 1L, 1L))
  expect_equal(sc$lineup_spec$slots$eligible[[1L]], "QB")     # single -> scalar
  expect_equal(sc$lineup_spec$slots$eligible[[5L]], c("RB", "WR", "TE"))  # FLEX -> array
  fix <- jsonlite::fromJSON(
    testthat::test_path("..", "..", "inst", "fixtures",
                        "ev_combine_fixture_puppy2.json"), simplifyVector = TRUE)
  expect_equal(sc$lineup_spec, fix$inputs$lineup_spec)
})
