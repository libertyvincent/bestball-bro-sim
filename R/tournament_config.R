#' Load a tournament config from YAML
#'
#' Reads a tournament YAML file from `inst/tournaments/` and validates its
#' structure against the stage-abstraction schema defined in LAYER_B.md.
#'
#' @param path Path to a tournament YAML, or just the file stem (without `.yaml`)
#'   for files in `inst/tournaments/`.
#' @return A validated list with class `bbbro_tournament`.
#' @export
#' @examples
#' \dontrun{
#' bbm <- load_tournament_config("bbm_2026")
#' ww  <- load_tournament_config("weekly_winners_2026")
#' }
load_tournament_config <- function(path) {
  if (!grepl("[/\\\\]|\\.ya?ml$", path)) {
    # Treat as file stem in inst/tournaments/
    pkg_path <- system.file("tournaments", paste0(path, ".yaml"),
                            package = "bestballBroSim")
    if (pkg_path == "") {
      # Fall back to dev mode (e.g. devtools::load_all() or running tests)
      # where inst/ is at the package root. Walk up from getwd() to find it.
      root <- .find_package_root()
      if (!is.null(root)) {
        candidate <- file.path(root, "inst", "tournaments", paste0(path, ".yaml"))
        if (file.exists(candidate)) {
          pkg_path <- candidate
        }
      }
      if (pkg_path == "") {
        cli::cli_abort(c(
          "Tournament config not found.",
          i = "Looked for: {.path inst/tournaments/{path}.yaml}",
          i = "Run from the repo root or pass a full path."
        ))
      }
    }
    path <- pkg_path
  }

  cfg <- yaml::read_yaml(path)
  validate_tournament_config(cfg)
  class(cfg) <- c("bbbro_tournament", class(cfg))
  cfg
}

#' Validate a tournament config against the stage-abstraction schema
#'
#' Throws a structured error if the config is malformed. Catches the common
#' mistakes: missing required fields, invalid stage `eligible_from` references,
#' duplicate stage IDs, invalid advancement specs.
#'
#' @param cfg A parsed tournament config (list, not file path).
#' @return Invisibly `TRUE` on success; errors on failure.
#' @export
validate_tournament_config <- function(cfg) {
  required <- c("id", "name", "scoring", "roster_slots", "draft", "stages")
  missing_fields <- setdiff(required, names(cfg))
  if (length(missing_fields) > 0) {
    cli::cli_abort("Tournament config missing required fields: {missing_fields}")
  }

  if (length(cfg$stages) == 0) {
    cli::cli_abort("Tournament config must have at least one stage")
  }

  stage_ids <- vapply(cfg$stages, function(s) s$id, character(1))
  if (anyDuplicated(stage_ids) > 0) {
    dupes <- stage_ids[duplicated(stage_ids)]
    cli::cli_abort("Duplicate stage IDs: {dupes}")
  }

  for (i in seq_along(cfg$stages)) {
    stage <- cfg$stages[[i]]
    validate_stage(stage, prior_stage_ids = stage_ids[seq_len(i - 1)])
  }

  invisible(TRUE)
}

#' Validate a single stage block (internal)
#'
#' @keywords internal
validate_stage <- function(stage, prior_stage_ids) {
  required <- c("id", "weeks", "eligible_from", "pod", "advancement", "payout_pool")
  missing_fields <- setdiff(required, names(stage))
  if (length(missing_fields) > 0) {
    cli::cli_abort(
      "Stage {.val {stage$id %||% '<unnamed>'}} missing required fields: {missing_fields}"
    )
  }

  # eligible_from must be 'all' or refer to a prior stage
  if (stage$eligible_from != "all" &&
      !(stage$eligible_from %in% prior_stage_ids)) {
    cli::cli_abort(c(
      "Stage {.val {stage$id}} has eligible_from={.val {stage$eligible_from}}, \\
      which is not 'all' or a prior stage id.",
      i = "Prior stages: {prior_stage_ids}"
    ))
  }

  # advancement: either the string "none" or a list with type top_n / top_pct
  adv <- stage$advancement
  if (is.character(adv) && length(adv) == 1 && adv == "none") {
    # ok
  } else if (is.list(adv) && !is.null(adv$type) &&
             adv$type %in% c("top_n", "top_pct")) {
    # ok
  } else {
    cli::cli_abort(
      "Stage {.val {stage$id}} has invalid advancement spec — expected \\
      'none' or {{type: top_n|top_pct, ...}}"
    )
  }

  invisible(TRUE)
}

#' List all available tournament configs
#'
#' @return Character vector of tournament IDs found in `inst/tournaments/`.
#' @export
list_tournaments <- function() {
  dir <- system.file("tournaments", package = "bestballBroSim")
  if (dir == "") {
    # Dev-mode fallback: walk up to find package root
    root <- .find_package_root()
    if (is.null(root)) return(character(0))
    dir <- file.path(root, "inst", "tournaments")
    if (!dir.exists(dir)) return(character(0))
  }
  files <- list.files(dir, pattern = "\\.ya?ml$", full.names = FALSE)
  sub("\\.ya?ml$", "", files)
}

