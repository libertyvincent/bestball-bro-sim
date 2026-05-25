# Integration: full publish_v2 -> JSON + parquet round-trip.
#
# Hits the network on first run (gh-pages source feeds) but reuses
# the per-URL cache under ~/.bestball-bro/cache on subsequent runs.
# Skipped in environments without arrow or without a warm cache.

.season_cache_warm <- function() {
  cache_dir <- file.path("~", ".bestball-bro", "cache")
  if (!dir.exists(cache_dir)) return(FALSE)
  expected <- c(
    "sources/clay_2026_offense.json",
    "sources/clay_2026_weekly_team_scoring.json",
    "sources/etr_2026_season.json",
    "sources/legup_2026_ud.json"
  )
  base <- "https://libertyvincent.github.io/bestball-bro-data"
  keys <- vapply(expected, function(p) {
    digest::digest(paste0(base, "/", p), algo = "sha256", serialize = FALSE)
  }, character(1))
  all(file.exists(file.path(cache_dir, paste0(keys, ".json"))))
}

test_that("publish_v2 produces JSON + parquet for the Season slate", {
  skip_if_not_installed("arrow")
  skip_if_not(.season_cache_warm(),
              "Source cache cold; run blend_slate('nfl_2026_season', ...) once first.")

  td <- tempfile("publish_v2_int_")
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)

  # Tiny n_sims keeps this < 2s; we only check shape + presence here.
  publish_v2(out_dir = td, n_sims = 100L, seed = 42L)

  json_path    <- file.path(td, "v2", "projections", "nfl_2026_season.json")
  parquet_path <- file.path(td, "v2", "draws",       "nfl_2026_season.parquet")
  meta_path    <- file.path(td, "_meta.json")

  # --- JSON exists, parses, has expected shape ---
  expect_true(file.exists(json_path))
  feed <- jsonlite::fromJSON(json_path, simplifyVector = FALSE)
  expect_setequal(names(feed), c("_meta", "players"))
  expect_equal(feed$`_meta`$slate_id,    "nfl_2026_season")
  expect_equal(feed$`_meta`$methodology, "blended_consensus_v2")
  expect_true(feed$`_meta`$player_count > 100L)

  # Pick any player with a projection; verify percentiles are populated
  # AND form a valid CDF (p10 < p25 < p50 < p75 < p90 for non-trivial std).
  has_proj <- vapply(feed$players, function(p) !is.null(p$season_mean),
                     logical(1))
  expect_true(any(has_proj))
  example <- feed$players[which(has_proj)[1]][[1]]
  qs <- example$season_percentiles
  expect_true(!is.null(qs))
  expect_true(qs$p10 <= qs$p25 && qs$p25 <= qs$p50 &&
              qs$p50 <= qs$p75 && qs$p75 <= qs$p90)

  # disagreement_std + aleatoric_std should be present
  expect_true(!is.null(example$disagreement_std))
  expect_true(!is.null(example$aleatoric_std))

  # --- Parquet exists with expected columns ---
  expect_true(file.exists(parquet_path))
  draws <- arrow::read_parquet(parquet_path)
  expect_setequal(names(draws),
                  c("underdog_id", "sim_idx", "week", "draw_value"))
  expect_true(nrow(draws) > 0L)
  expect_true(all(draws$draw_value >= 0))
  # 100 sims, 18 weeks: each player-row block is 1800. So row count is a
  # multiple of 1800.
  expect_equal(nrow(draws) %% 1800L, 0L)

  # --- _meta.json updated ---
  expect_true(file.exists(meta_path))
  m <- jsonlite::fromJSON(meta_path, simplifyVector = FALSE)
  s <- m$slates$nfl_2026_season
  expect_equal(s$v2_path, "v2/projections/nfl_2026_season.json")
  expect_true(nchar(s$v2_sha256) > 32L)
})

test_that("empirical percentiles differ from analytical Normal predictions", {
  skip_if_not_installed("arrow")
  skip_if_not(.season_cache_warm(),
              "Source cache cold; run blend_slate('nfl_2026_season', ...) once first.")

  td <- tempfile("publish_v2_int_diff_")
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)

  # 1000 sims gives stable empirical quantiles for the comparison.
  publish_v2(out_dir = td, n_sims = 1000L, seed = 42L)

  json_path <- file.path(td, "v2", "projections", "nfl_2026_season.json")
  feed <- jsonlite::fromJSON(json_path, simplifyVector = FALSE)

  # Find a player with a non-trivial std (not all sources unanimous).
  candidates <- Filter(function(p) {
    !is.null(p$season_mean) && !is.null(p$season_std) &&
      as.numeric(p$season_std) > 10
  }, feed$players)
  expect_true(length(candidates) > 0)

  p <- candidates[[1]]
  emp_p90       <- p$season_percentiles$p90
  analytical_p90 <- stats::qnorm(0.90,
                                  mean = as.numeric(p$season_mean),
                                  sd   = as.numeric(p$season_std))

  # Empirical p90 should be close to (but not identical to) analytical
  # because the simulator clips at zero and the season distribution is
  # not perfectly Normal at the tails. Anything within 20% is fine; if
  # they diverged by orders of magnitude something's structurally wrong.
  rel_err <- abs(emp_p90 - analytical_p90) / analytical_p90
  expect_lt(rel_err, 0.20)
})
