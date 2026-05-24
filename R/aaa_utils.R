#' Null-coalescing operator
#'
#' R 4.4+ has this in base; defined here for compatibility with 4.3.
#'
#' @keywords internal
`%||%` <- function(a, b) if (is.null(a)) b else a

#' Find the package root by walking up from getwd() looking for DESCRIPTION
#'
#' Used to make dev-mode (devtools::load_all() / running tests from any subdir)
#' work regardless of which directory the user happens to be in. Returns NULL
#' if not found.
#'
#' @keywords internal
.find_package_root <- function() {
  d <- normalizePath(getwd(), mustWork = FALSE)
  for (i in 1:20) {  # walk at most 20 levels
    if (file.exists(file.path(d, "DESCRIPTION"))) return(d)
    parent <- dirname(d)
    if (parent == d) return(NULL)  # hit filesystem root
    d <- parent
  }
  NULL
}

#' Resolve an inst/-relative path with dev-mode fallback
#'
#' Wraps `system.file()` with the same package-root walker used elsewhere.
#' Returns "" if the file cannot be found (matching system.file()'s convention).
#'
#' @param subdir Subdirectory under `inst/`, e.g. `"tournaments"` or `"scoring"`.
#' @param filename Filename within `subdir`, e.g. `"bbm_2026.yaml"`.
#' @keywords internal
.inst_path <- function(subdir, filename) {
  pkg_path <- system.file(subdir, filename, package = "bestballBroSim")
  if (pkg_path != "") return(pkg_path)

  root <- .find_package_root()
  if (is.null(root)) return("")

  candidate <- file.path(root, "inst", subdir, filename)
  if (file.exists(candidate)) candidate else ""
}

#' Current NFL season
#'
#' Defers to nflreadr if available; otherwise uses the convention that
#' January–August defaults to prior calendar year (the just-completed season),
#' September–December defaults to current calendar year.
#' @keywords internal
current_season <- function() {
  if (requireNamespace("nflreadr", quietly = TRUE)) {
    return(nflreadr::get_current_season())
  }
  yr <- as.integer(format(Sys.Date(), "%Y"))
  mo <- as.integer(format(Sys.Date(), "%m"))
  if (mo < 9) yr - 1L else yr
}
