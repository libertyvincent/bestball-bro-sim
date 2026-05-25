# .normalize_name + .normalize_team_abbr + .NAME_ALIASES.
# Goal: any two source-side names that should match the slate's
# canonical name must produce IDENTICAL output from .normalize_name().

test_that("periods are stripped (A.J. <-> AJ)", {
  expect_equal(.normalize_name("A.J. Brown"), "aj brown")
  expect_equal(.normalize_name("AJ Brown"),   "aj brown")
})

test_that("apostrophes (straight and curly) are stripped", {
  expect_equal(.normalize_name("De'Von Achane"),  "devon achane")
  expect_equal(.normalize_name("De’Von Achane"), "devon achane")
  expect_equal(.normalize_name("Ja'Marr Chase"),  "jamarr chase")
})

test_that("generational suffixes (II..IX, Jr, Sr) are stripped", {
  expect_equal(.normalize_name("Kenneth Walker III"), "kenneth walker")
  expect_equal(.normalize_name("Marvin Harrison Jr"), "marvin harrison")
  expect_equal(.normalize_name("Marvin Harrison Jr."), "marvin harrison")
  expect_equal(.normalize_name("Robert Griffin III"), "robert griffin")
  expect_equal(.normalize_name("Calvin Johnson II"), "calvin johnson")
})

test_that("a player whose first name IS the suffix doesn't lose it mid-name", {
  # "iii" only stripped at end of string after \s+. "iii smith" stays.
  expect_equal(.normalize_name("Iii Smith"), "iii smith")
})

test_that("Ken Walker alias collapses to Kenneth Walker form", {
  slate <- .normalize_name("Kenneth Walker III")  # canonical slate side
  clay  <- .normalize_name("Ken Walker III")      # source-side variant
  expect_equal(slate, clay)
  expect_equal(slate, "kenneth walker")
})

test_that("Marquise Brown / Hollywood Brown alias collapses to one form", {
  slate <- .normalize_name("Hollywood Brown")  # canonical slate side
  clay  <- .normalize_name("Marquise Brown")   # source-side variant
  expect_equal(slate, clay)
})

test_that("whitespace and case variants collapse to the same key", {
  expect_equal(.normalize_name("  Bijan   ROBINSON "),
               .normalize_name("Bijan Robinson"))
})

test_that(".normalize_team_abbr maps Clay/ESPN codes to nflverse convention", {
  expect_equal(.normalize_team_abbr("ARI"), "AZ")
  expect_equal(.normalize_team_abbr("LAR"), "LA")
  expect_equal(.normalize_team_abbr("JAC"), "JAX")
  expect_equal(.normalize_team_abbr("WSH"), "WAS")
})

test_that(".normalize_team_abbr preserves already-canonical codes", {
  for (canon in c("BUF", "ATL", "KC", "LV", "JAX", "AZ", "LA")) {
    expect_equal(.normalize_team_abbr(canon), canon)
  }
})

test_that(".normalize_team_abbr returns NA for unknown codes", {
  expect_true(is.na(.normalize_team_abbr("XYZ")))
  expect_true(is.na(.normalize_team_abbr(NA_character_)))
})

test_that(".normalize_team_abbr vectorizes", {
  out <- .normalize_team_abbr(c("ARI", "BUF", "LAR", "XYZ"))
  expect_equal(out, c("AZ", "BUF", "LA", NA_character_))
})
