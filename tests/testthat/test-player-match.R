# Tests for the centralized scraper -> feed bridge key (player_match.R).
# Verifies the 5 specific names the 3b-5 audit caught (Walker III,
# Etienne Jr., Pittman Jr., St. Brown, Hunter CB) all match across the
# scraper-vs-feed sides, plus general normalizer contracts.

# ---- 5 audited names: scraper-form key == feed-form key --------------------

test_that("Travis Etienne Jr. matches across the bridge", {
  # Scraper sees first_name = "Travis", last_name = "Etienne Jr.", pos RB.
  scraper_full <- paste("Travis", "Etienne Jr.")
  # Feed (slate CSV) carries either "Travis Etienne" or "Travis Etienne Jr."
  expect_equal(normalize_player_key(scraper_full, "RB"),
               normalize_player_key("Travis Etienne", "RB"))
  expect_equal(normalize_player_key(scraper_full, "RB"),
               normalize_player_key("Travis Etienne Jr.", "RB"))
})

test_that("Kenneth Walker III matches across the bridge", {
  scraper_full <- paste("Kenneth", "Walker III")
  expect_equal(normalize_player_key(scraper_full, "RB"),
               normalize_player_key("Kenneth Walker", "RB"))
})

test_that("Michael Pittman Jr. matches across the bridge", {
  scraper_full <- paste("Michael", "Pittman Jr.")
  expect_equal(normalize_player_key(scraper_full, "WR"),
               normalize_player_key("Michael Pittman", "WR"))
  expect_equal(normalize_player_key(scraper_full, "WR"),
               normalize_player_key("Michael Pittman Jr.", "WR"))
})

test_that("Amon-Ra St. Brown matches across the bridge (multi-token last name)", {
  scraper_full <- paste("Amon-Ra", "St. Brown")
  # The two main forms the feed might carry.
  expect_equal(normalize_player_key(scraper_full, "WR"),
               normalize_player_key("Amon-Ra St. Brown", "WR"))
  expect_equal(normalize_player_key(scraper_full, "WR"),
               normalize_player_key("Amon-Ra St Brown", "WR"))
})

test_that("Travis Hunter resolves to WR everywhere via the CB->WR remap", {
  scraper_full <- paste("Travis", "Hunter")
  # Scraper labels him CB; feed labels him WR. Same key out the other side.
  expect_equal(normalize_player_key(scraper_full, "CB"),
               normalize_player_key(scraper_full, "WR"))
  # And specifically the key resolves to "...|WR", not "...|CB", since the
  # downstream lineup spec only fills QB/RB/WR/TE slots.
  k <- normalize_player_key(scraper_full, "CB")
  expect_match(k, "\\|WR$")
})

# ---- General normalizer contracts (carried over from the blender suite) ----

test_that("normalize_player_key is vectorized over both args", {
  k <- normalize_player_key(
    c("Travis Etienne Jr.", "Kenneth Walker III", "Travis Hunter"),
    c("RB", "RB", "CB")
  )
  expect_equal(length(k), 3L)
  expect_equal(k[3L], normalize_player_key("Travis Hunter", "WR"))
})

test_that("normalize_player_key returns NA when either input is NA", {
  expect_true(is.na(normalize_player_key(NA_character_, "WR")))
  expect_true(is.na(normalize_player_key("Bijan Robinson", NA_character_)))
})

test_that("normalize_player_key errors on length mismatch", {
  expect_error(
    normalize_player_key(c("Bijan Robinson", "Saquon Barkley"), "RB"),
    "same length"
  )
})

test_that("A.J. (periods + initials) normalizes to AJ", {
  expect_equal(normalize_player_key("A.J. Brown", "WR"),
               normalize_player_key("AJ Brown",   "WR"))
})

test_that("FB position remaps to RB", {
  expect_equal(normalize_player_key("Patrick Ricard", "FB"),
               normalize_player_key("Patrick Ricard", "RB"))
})

test_that("normalizer leaves QB/RB/WR/TE alone", {
  for (p in c("QB", "RB", "WR", "TE")) {
    k <- normalize_player_key("Some Player", p)
    expect_match(k, paste0("\\|", p, "$"))
  }
})

# ---- Existing blender alias still works through the new path --------------

test_that("Ken Walker alias collapses to Kenneth Walker (alias map preserved)", {
  # Both forms (with the alias source and with the canonical) should hit
  # the same key, since .NAME_ALIASES is consulted by .normalize_name() and
  # normalize_player_key() routes through it.
  expect_equal(normalize_player_key("Ken Walker", "RB"),
               normalize_player_key("Kenneth Walker", "RB"))
})
