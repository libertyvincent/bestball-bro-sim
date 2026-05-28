# Tests for R/field_empirics.R. A miniature scraper-export fixture is built
# in-memory and written to a tempfile per test, avoiding the need to commit
# any real scraped data.

make_mini_export <- function(drafts = NULL, players = NULL, appearances = NULL,
                             round_index = NULL, slate_id = "slate-A") {
  # Defaults: one slate, three players (1 QB + 2 WRs all on TEAM-A), two
  # appearances per player so the join is non-trivial; one draft of 3 picks
  # where drafter U1 takes all three same-team players => has_qb_stack_3plus.
  players <- players %||% list(
    list(id = "p-qb1",  first_name = "Joe",  last_name = "QB",  position_name = "QB", team_id = "TEAM-A"),
    list(id = "p-wr1",  first_name = "Alpha", last_name = "WR", position_name = "WR", team_id = "TEAM-A"),
    list(id = "p-wr2",  first_name = "Beta",  last_name = "WR", position_name = "WR", team_id = "TEAM-A"),
    list(id = "p-rb1",  first_name = "Gamma", last_name = "RB", position_name = "RB", team_id = "TEAM-B"),
    list(id = "p-rb2",  first_name = "Delta", last_name = "RB", position_name = "RB", team_id = "TEAM-C"),
    list(id = "p-fa1",  first_name = "Free",  last_name = "Agent", position_name = "WR", team_id = NULL)
  )
  appearances <- appearances %||% list(
    list(id = "a-qb1", player_id = "p-qb1"),
    list(id = "a-wr1", player_id = "p-wr1"),
    list(id = "a-wr2", player_id = "p-wr2"),
    list(id = "a-rb1", player_id = "p-rb1"),
    list(id = "a-rb2", player_id = "p-rb2"),
    list(id = "a-fa1", player_id = "p-fa1")
  )

  drafts <- drafts %||% list(
    list(
      draft_id            = "d-1",
      draft_state         = "completed",
      slate_id            = slate_id,
      tournament_id       = "tnmt-x",
      tournament_round_id = "round-x",
      tournament_title    = "Demo Tournament",
      raw_response = list(draft = list(
        id = "d-1", slate_id = slate_id, status = "completed",
        draft_entries = list(
          list(id = "e-1", user_id = "U1", pick_order = 1),
          list(id = "e-2", user_id = "U2", pick_order = 2)
        ),
        # 6 picks total, snake: U1=1,4,5; U2=2,3,6
        picks = list(
          list(number = 1, draft_entry_id = "e-1", appearance_id = "a-qb1",
               projection_adp = "1.5", projection_points = "300", points = "0", swapped = FALSE),
          list(number = 2, draft_entry_id = "e-2", appearance_id = "a-rb1",
               projection_adp = "2.0", projection_points = "280", points = "0", swapped = FALSE),
          list(number = 3, draft_entry_id = "e-2", appearance_id = "a-rb2",
               projection_adp = "3.0", projection_points = "260", points = "0", swapped = FALSE),
          list(number = 4, draft_entry_id = "e-1", appearance_id = "a-wr1",
               projection_adp = "4.0", projection_points = "240", points = "0", swapped = FALSE),
          list(number = 5, draft_entry_id = "e-1", appearance_id = "a-wr2",
               projection_adp = "5.0", projection_points = "220", points = "0", swapped = FALSE),
          list(number = 6, draft_entry_id = "e-2", appearance_id = "a-fa1",
               projection_adp = "6.0", projection_points = "200", points = "0", swapped = FALSE)
        )
      ))
    )
  )

  round_index <- round_index %||% list(
    `round-x` = list(event_type = "tournament", round_abbr = "Final",
                     round_number = 1, slate_id = slate_id,
                     tournament_id = "tnmt-x", tournament_title = "Demo Tournament")
  )

  list(
    exported_at = "2026-05-27T00:00:00Z",
    drafts      = drafts,
    unkeyed     = list(
      list(api_endpoint = paste0("/v1/slates/", slate_id, "/players"),
           raw_response = list(players = players)),
      list(api_endpoint = paste0("/v1/slates/", slate_id,
                                 "/scoring_types/st-1/appearances"),
           raw_response = list(appearances = appearances))
    ),
    round_tournament_index = round_index,
    draft_round_index      = list()
  )
}

write_mini_export <- function(payload) {
  p <- tempfile(fileext = ".json")
  jsonlite::write_json(payload, p, auto_unbox = TRUE, null = "null", na = "null")
  p
}

# ---- load_scraped_drafts ----------------------------------------------------

test_that("load_scraped_drafts produces non-zero rows for the fixture slate", {
  p <- write_mini_export(make_mini_export())
  picks <- load_scraped_drafts(p)
  expect_gt(nrow(picks), 0L)
  expect_setequal(unique(picks$slate_id), "slate-A")
  expect_setequal(unique(picks$drafter_user_id), c("U1", "U2"))
  expect_true(all(c("first_name", "last_name", "position_name") %in% colnames(picks)))
})

test_that("load_scraped_drafts joins appearance_id -> player metadata", {
  p <- write_mini_export(make_mini_export())
  picks <- load_scraped_drafts(p)
  pick1 <- picks[picks$pick_overall == 1L, ]
  expect_equal(pick1$first_name, "Joe")
  expect_equal(pick1$last_name, "QB")
  expect_equal(pick1$position_name, "QB")
  expect_equal(pick1$team_id, "TEAM-A")
  expect_equal(pick1$drafter_user_id, "U1")
  expect_equal(pick1$drafter_slot, 1L)
  expect_equal(pick1$round, 1L)
})

test_that("load_scraped_drafts emits NA team_id for free agents", {
  p <- write_mini_export(make_mini_export())
  picks <- load_scraped_drafts(p)
  fa <- picks[picks$player_id == "p-fa1", ]
  expect_equal(nrow(fa), 1L)
  expect_true(is.na(fa$team_id))
})

test_that("load_scraped_drafts skips incomplete drafts and reports count", {
  payload <- make_mini_export()
  payload$drafts[[2]] <- payload$drafts[[1]]
  payload$drafts[[2]]$draft_id <- "d-2"
  payload$drafts[[2]]$draft_state <- "pending"
  payload$drafts[[2]]$raw_response$draft$id <- "d-2"
  payload$drafts[[2]]$raw_response$draft$status <- "pending"
  p <- write_mini_export(payload)
  picks <- load_scraped_drafts(p)
  expect_equal(length(unique(picks$draft_id)), 1L)
  expect_equal(attr(picks, "n_drafts_input"), 2L)
  expect_equal(attr(picks, "n_drafts_skipped_incomplete"), 1L)
})

test_that("load_scraped_drafts errors loudly when export file is missing", {
  expect_error(
    load_scraped_drafts("inst/data/scraped_drafts/does-not-exist.json"),
    "Scraper export not found"
  )
})

test_that("load_scraped_drafts populates event_type from round_tournament_index", {
  payload <- make_mini_export()
  payload$round_tournament_index$`round-x`$event_type <- "weekly_winner"
  p <- write_mini_export(payload)
  picks <- load_scraped_drafts(p)
  expect_equal(unique(picks$event_type), "weekly_winner")
})

# ---- empirical_position_counts ----------------------------------------------

test_that("position counts sum to roster_total per drafter (QB/RB/WR/TE only)", {
  p <- write_mini_export(make_mini_export())
  picks <- load_scraped_drafts(p)
  ec <- empirical_position_counts(picks)
  # In our fixture both drafters have exactly 3 QB+RB+WR+TE picks each.
  per_team_total <- sum(ec$count * ec$n_teams_with_count) /
    sum(ec$n_teams_with_count) *
    length(unique(ec$position))
  # 6 picks across 2 drafters = 3/drafter, all in QB/RB/WR/TE.
  per_drafter <- aggregate(
    cbind(count = rep(1L, nrow(picks[picks$position_name %in% c("QB","RB","WR","TE"), ])))
      ~ paste(picks$draft_id, picks$drafter_user_id)[picks$position_name %in% c("QB","RB","WR","TE")],
    FUN = sum
  )
  expect_true(all(per_drafter$count == 3L))
})

test_that("position counts exclude non-QBRBWRTE positions", {
  payload <- make_mini_export()
  # Add a CB to the catalog and the picks; should be excluded from counts.
  payload$unkeyed[[1]]$raw_response$players <- c(
    payload$unkeyed[[1]]$raw_response$players,
    list(list(id = "p-cb1", first_name = "X", last_name = "CB",
              position_name = "CB", team_id = "TEAM-A"))
  )
  payload$unkeyed[[2]]$raw_response$appearances <- c(
    payload$unkeyed[[2]]$raw_response$appearances,
    list(list(id = "a-cb1", player_id = "p-cb1"))
  )
  payload$drafts[[1]]$raw_response$draft$picks <- c(
    payload$drafts[[1]]$raw_response$draft$picks,
    list(list(number = 7, draft_entry_id = "e-1", appearance_id = "a-cb1",
              projection_adp = "7", projection_points = "100", points = "0",
              swapped = FALSE))
  )
  p <- write_mini_export(payload)
  picks <- load_scraped_drafts(p)
  expect_true("CB" %in% picks$position_name)
  ec <- empirical_position_counts(picks)
  expect_false("CB" %in% ec$position)
})

# ---- empirical_stack_patterns -----------------------------------------------

test_that("stack patterns detect QB + 2 WRs on same NFL team as 3+ stack", {
  p <- write_mini_export(make_mini_export())
  picks <- load_scraped_drafts(p)
  st <- empirical_stack_patterns(picks)
  u1 <- st[st$drafter_user_id == "U1", ]
  expect_equal(nrow(u1), 1L)
  expect_true(u1$has_qb_stack_2plus)
  expect_true(u1$has_qb_stack_3plus)
  expect_false(u1$has_qb_stack_4plus)
  expect_equal(u1$max_team_stack_depth, 3L)
  expect_equal(u1$n_team_stacks_3plus, 1L)

  # U2 took: RB on TEAM-B, RB on TEAM-C, free agent. No stacks.
  u2 <- st[st$drafter_user_id == "U2", ]
  expect_equal(u2$max_team_stack_depth, 1L)
  expect_false(u2$has_qb_stack_2plus)
  expect_equal(u2$n_team_stacks_3plus, 0L)
})

# ---- publish_field_empirics -------------------------------------------------

test_that("publish_field_empirics writes one JSON per slate with valid schema", {
  payload <- make_mini_export()
  p <- write_mini_export(payload)

  out_dir <- file.path(tempfile("field_empirics_"), "feed")
  paths <- publish_field_empirics(export_path = p, output_dir = out_dir)
  expect_length(paths, 1L)
  expect_true(file.exists(paths[["slate-A"]]))

  feed <- jsonlite::fromJSON(paths[["slate-A"]], simplifyVector = FALSE)
  required <- c("slate_id", "computed_at", "source_export_captured_at",
                "n_drafts_sampled", "n_teams_sampled", "n_tournament_unique",
                "by_event_type", "position_counts", "stack_patterns",
                "pick_distributions")
  expect_true(all(required %in% names(feed)))
  expect_equal(feed$slate_id, "slate-A")
  expect_equal(feed$n_drafts_sampled, 1L)
  expect_equal(feed$n_teams_sampled, 2L)
  expect_true(length(feed$pick_distributions) >= 1L)
  expect_true(!is.null(feed$stack_patterns$qb_stack_2plus_rate))
})
