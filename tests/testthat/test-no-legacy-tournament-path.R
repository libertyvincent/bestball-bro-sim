# Regression guard: the legacy `inst/tournaments/` folder was removed in
# the chore/remove-legacy-tournament-configs PR. The canonical configs
# live at `inst/data/tournaments/`. This test prevents the legacy path
# from silently returning -- a loader pointed at the wrong directory
# would otherwise let tests pass against stale config.
#
# Scope: code paths that could exercise the path (R/, tests/, .github/).
# Markdown docs are excluded by design (no execution risk; doc drift is a
# lower-stakes concern than code drift).

test_that("no source file references the legacy inst/tournaments/ path", {
  repo_root <- testthat::test_path("..", "..")
  scan_dirs <- c("R", "tests", ".github")

  forbidden <- "inst/tournaments/"
  # This test file itself documents the forbidden string. Exclude it by
  # basename so the guard stays self-documenting without false-positiving.
  self_basename <- "test-no-legacy-tournament-path.R"

  hits <- character(0)
  for (dir in scan_dirs) {
    dir_path <- file.path(repo_root, dir)
    if (!dir.exists(dir_path)) next
    files <- list.files(dir_path, recursive = TRUE, full.names = TRUE,
                        pattern = "\\.(R|yml|yaml)$")
    for (f in files) {
      if (basename(f) == self_basename) next
      lines <- readLines(f, warn = FALSE, encoding = "UTF-8")
      bad <- grep(forbidden, lines, fixed = TRUE)
      if (length(bad) > 0L) {
        hits <- c(hits, sprintf("%s:%d", f, bad))
      }
    }
  }

  expect_equal(
    hits, character(0),
    info = paste0(
      "Legacy path `inst/tournaments/` referenced in: ",
      paste(hits, collapse = "; "),
      ". The canonical path is `inst/data/tournaments/`."
    )
  )
})
