# .generate_weekly: per-week mean from a Clay-shaped weekly team scoring,
# with per-week std = cv * weekly_mean, and aleatoric_season_var = sum
# of weekly variances over active weeks.

.make_uniform_weekly_team <- function(team_abbr = "ATL",
                                       weekly_score = 20.0,
                                       bye_week = 7L,
                                       n_weeks = 18L) {
  weeks <- vector("list", n_weeks)
  for (w in seq_len(n_weeks)) {
    if (w == bye_week) {
      weeks[[w]] <- list(week = as.integer(w), opponent = NULL, location = NULL,
                         team_nfl_score = 0.0, opponent_nfl_score = 0.0,
                         win_prob = NULL, is_bye = TRUE)
    } else {
      weeks[[w]] <- list(week = as.integer(w), opponent = "OPP",
                         location = if (w %% 2L == 0L) "H" else "V",
                         team_nfl_score = weekly_score,
                         opponent_nfl_score = weekly_score,
                         win_prob = 0.5, is_bye = FALSE)
    }
  }
  list(teams = setNames(list(list(weeks = weeks)), team_abbr))
}

test_that("uniform weekly scoring -> weekly_mean = season_mean / active_weeks", {
  wt <- .make_uniform_weekly_team("ATL", weekly_score = 20, bye_week = 7L)
  out <- .generate_weekly(season_mean = 170, team_abbr = "ATL",
                          weekly_team = wt, cv = 0.50)
  recs <- out$records
  active_means <- vapply(recs, function(r) if (r$is_bye) NA_real_ else r$mean,
                          numeric(1))
  active_means <- active_means[!is.na(active_means)]
  expect_length(active_means, 17L)
  expect_true(all(abs(active_means - 10) < 1e-9),
              info = "every active week should mean = 170/17 = 10")
})

test_that("bye week emits mean=0, std=0, is_bye=TRUE", {
  wt <- .make_uniform_weekly_team("ATL", weekly_score = 20, bye_week = 11L)
  out <- .generate_weekly(season_mean = 170, team_abbr = "ATL",
                          weekly_team = wt, cv = 0.50)
  bye <- out$records[[11]]
  expect_true(isTRUE(bye$is_bye))
  expect_equal(bye$mean, 0)
  expect_equal(bye$std,  0)
  expect_null(bye$opponent)
  expect_null(bye$home_away)
})

test_that("weekly_std equals cv * weekly_mean on every active week", {
  wt <- .make_uniform_weekly_team("ATL", weekly_score = 25, bye_week = 7L)
  out <- .generate_weekly(season_mean = 200, team_abbr = "ATL",
                          weekly_team = wt, cv = 0.5)
  for (r in out$records) {
    if (r$is_bye) next
    expect_equal(r$std, r$mean * 0.5, tolerance = 1e-2)
  }
})

test_that("aleatoric_season_var equals sum of weekly_std^2 over active weeks", {
  wt <- .make_uniform_weekly_team("ATL", weekly_score = 20, bye_week = 7L)
  out <- .generate_weekly(season_mean = 170, team_abbr = "ATL",
                          weekly_team = wt, cv = 0.50)
  # weekly_std = 0.5 * 10 = 5 for each active week; 17 active weeks
  # sum(5^2 * 17) = 425
  expect_equal(out$aleatoric_season_var, 425, tolerance = 1e-9)
})

test_that("location H/V translates to home/away strings", {
  wt <- .make_uniform_weekly_team("ATL", weekly_score = 20, bye_week = 5L)
  out <- .generate_weekly(season_mean = 170, team_abbr = "ATL",
                          weekly_team = wt, cv = 0.50)
  has_home <- has_away <- FALSE
  for (r in out$records) {
    if (r$is_bye) next
    expect_true(r$home_away %in% c("home", "away"),
                info = "home_away must be 'home' or 'away', never V/H")
    has_home <- has_home || identical(r$home_away, "home")
    has_away <- has_away || identical(r$home_away, "away")
  }
  expect_true(has_home && has_away,
              info = "fixture has both home and away weeks")
})

test_that("missing team returns empty record set + zero aleatoric var", {
  wt <- .make_uniform_weekly_team("ATL", weekly_score = 20)
  out <- .generate_weekly(season_mean = 170, team_abbr = "WAS",
                          weekly_team = wt, cv = 0.50)
  expect_length(out$records, 0L)
  expect_equal(out$aleatoric_season_var, 0)
})
