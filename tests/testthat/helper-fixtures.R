# Shared test fixtures. testthat auto-sources helper-*.R before tests run,
# so any function defined here is visible to every test file.

# A roster-shaped fixture used by .project_v0 unit tests.
make_test_rosters <- function() {
  data.frame(
    full_name = c("Josh Allen", "Bijan Robinson", "CeeDee Lamb",
                  "Sam LaPorta", "Patrick Mahomes", "Christian McCaffrey",
                  "Justin Jefferson", "Travis Kelce", "Rookie WR"),
    gsis_id   = paste0("00-", sprintf("%07d", 1:9)),
    team      = c("BUF", "ATL", "DAL", "DET", "KC", "SF", "MIN", "KC", "JAX"),
    position  = c("QB", "RB", "WR", "TE", "QB", "RB", "WR", "TE", "WR"),
    stringsAsFactors = FALSE
  )
}

# Slate-shaped fixture mirroring make_test_rosters() but in the column
# layout load_slate_data() produces. underdog_id is the canonical key;
# full_name/team_abbr/position are used for the two-tier history match
# in .project_slate / .identify_rookies. The 9th row is a rookie — its
# name/team don't appear in make_test_historical().
make_test_slate <- function() {
  data.frame(
    underdog_id      = paste0("ud-", sprintf("%07d", 1:9)),
    first_name       = c("Josh", "Bijan", "CeeDee", "Sam", "Patrick",
                         "Christian", "Justin", "Travis", "Rookie"),
    last_name        = c("Allen", "Robinson", "Lamb", "LaPorta",
                         "Mahomes", "McCaffrey", "Jefferson", "Kelce", "WR"),
    full_name        = c("Josh Allen", "Bijan Robinson", "CeeDee Lamb",
                         "Sam LaPorta", "Patrick Mahomes",
                         "Christian McCaffrey", "Justin Jefferson",
                         "Travis Kelce", "Rookie WR"),
    team_abbr        = c("BUF", "ATL", "DAL", "DET", "KC", "SF",
                         "MIN", "KC", "JAX"),
    position         = c("QB", "RB", "WR", "TE", "QB", "RB", "WR", "TE", "WR"),
    adp              = c(85, 1, 9, 35, 78, 7, 10, 50, 200),
    projected_points = c(350, 290, 220, 180, 310, 260, 200, 165, 80),
    salary           = rep(100, 9),
    position_rank    = c("QB6", "RB1", "WR5", "TE3", "QB7", "RB4",
                         "WR6", "TE6", "WR40"),
    lineup_status    = rep(NA_character_, 9),
    bye_week         = rep(NA_integer_, 9),
    stringsAsFactors = FALSE
  )
}

make_test_historical <- function() {
  # Weekly fixture — one row per player-game. .project_v0 uses each player's
  # most recent season (2025). Three regular-season weeks per player-season,
  # with per-week variation so weekly FP has non-zero spread. Player 1 also
  # gets a huge playoff (POST) game that must be excluded.
  # Every player (ids 1-8) has prior data; roster id 9 (Rookie WR) has none.
  base <- list(
    "00-0000001" = list(pos = "QB", name = "Josh Allen",
                        py = 270, pt = 2, int = 1, ry = 28, rt = 0, rec = 0, recy = 0,  rect = 0),
    "00-0000005" = list(pos = "QB", name = "Patrick Mahomes",
                        py = 300, pt = 2, int = 1, ry = 22, rt = 0, rec = 0, recy = 0,  rect = 0),
    "00-0000002" = list(pos = "RB", name = "Bijan Robinson",
                        py = 0,   pt = 0, int = 0, ry = 95, rt = 1, rec = 4, recy = 32, rect = 0),
    "00-0000006" = list(pos = "RB", name = "Christian McCaffrey",
                        py = 0,   pt = 0, int = 0, ry = 80, rt = 1, rec = 3, recy = 24, rect = 0),
    "00-0000003" = list(pos = "WR", name = "CeeDee Lamb",
                        py = 0,   pt = 0, int = 0, ry = 0,  rt = 0, rec = 7, recy = 95, rect = 1),
    "00-0000007" = list(pos = "WR", name = "Justin Jefferson",
                        py = 0,   pt = 0, int = 0, ry = 0,  rt = 0, rec = 6, recy = 78, rect = 0),
    "00-0000004" = list(pos = "TE", name = "Sam LaPorta",
                        py = 0,   pt = 0, int = 0, ry = 0,  rt = 0, rec = 5, recy = 55, rect = 0),
    "00-0000008" = list(pos = "TE", name = "Travis Kelce",
                        py = 0,   pt = 0, int = 0, ry = 0,  rt = 0, rec = 4, recy = 40, rect = 0)
  )
  team_by_id <- c("00-0000001"="BUF","00-0000002"="ATL","00-0000003"="DAL",
                  "00-0000004"="DET","00-0000005"="KC","00-0000006"="SF",
                  "00-0000007"="MIN","00-0000008"="KC")
  week_mult   <- c(0.7, 1.0, 1.3)               # within-season weekly variation
  season_mult <- c("2024" = 0.85, "2025" = 1.0) # 2025 is the recent season

  rows <- list()
  for (pid in names(base)) {
    b <- base[[pid]]
    for (s in c(2024, 2025)) for (w in seq_along(week_mult)) {
      mlt <- season_mult[[as.character(s)]] * week_mult[w]
      rows[[length(rows) + 1L]] <- data.frame(
        player_id = pid, player_name = b$name,
        recent_team = unname(team_by_id[pid]),
        position = b$pos, season = s, week = w,
        season_type = "REG",
        passing_yards   = b$py   * mlt, passing_tds     = b$pt   * mlt,
        interceptions   = b$int  * mlt,
        rushing_yards   = b$ry   * mlt, rushing_tds     = b$rt   * mlt,
        receptions      = b$rec  * mlt, receiving_yards = b$recy * mlt,
        receiving_tds   = b$rect * mlt,
        stringsAsFactors = FALSE
      )
    }
  }
  # Playoff game for player 1 — must be excluded by .project_v0.
  rows[[length(rows) + 1L]] <- data.frame(
    player_id = "00-0000001", player_name = "Josh Allen",
    recent_team = "BUF",
    position = "QB", season = 2025, week = 19L,
    season_type = "POST",
    passing_yards = 999, passing_tds = 9, interceptions = 0,
    rushing_yards = 99, rushing_tds = 9,
    receptions = 0, receiving_yards = 0, receiving_tds = 0,
    stringsAsFactors = FALSE
  )
  do.call(rbind, rows)
}
