#' Load all tournament definitions
#'
#' Reads every `inst/data/tournaments/*.yaml` (excluding `_common_rules.yaml`),
#' merges in common rules when `inherits_common_rules: true`, cross-references
#' each tournament's `slate_id` against `inst/data/slates/_manifest.yaml`, and
#' validates the result. Returns a list keyed by `tournament_id`.
#'
#' @param dir_path Optional override for the tournaments directory. If omitted,
#'   resolves via `system.file()` with a dev-mode fallback.
#' @param slates_manifest_path Optional override for the slates manifest path.
#' @return Named list of tournament definitions, keyed by `tournament_id`.
#'   Each element is a parsed YAML list with class `bbbro_tournament_def`.
#' @export
load_tournaments <- function(dir_path = NULL, slates_manifest_path = NULL) {
  dir_path <- dir_path %||% .tournaments_dir()
  if (!dir.exists(dir_path)) {
    cli::cli_abort(c(
      "Tournament definitions directory not found.",
      i = "Looked for: {.path {dir_path}}"
    ))
  }

  common_path <- file.path(dir_path, "_common_rules.yaml")
  common_rules <- if (file.exists(common_path)) yaml::read_yaml(common_path) else list()

  slates_manifest_path <- slates_manifest_path %||%
    .inst_path("data/slates", "_manifest.yaml")
  if (slates_manifest_path == "" || !file.exists(slates_manifest_path)) {
    cli::cli_abort(c(
      "Slate manifest not found — required for slate_id cross-referencing.",
      i = "Looked for: {.path inst/data/slates/_manifest.yaml}"
    ))
  }
  slates_manifest <- yaml::read_yaml(slates_manifest_path)

  all_yaml <- list.files(dir_path, pattern = "\\.ya?ml$", full.names = TRUE)
  tnmt_files <- all_yaml[basename(all_yaml) != "_common_rules.yaml"]

  out <- list()
  for (f in tnmt_files) {
    tnmt <- yaml::read_yaml(f)
    if (isTRUE(tnmt$inherits_common_rules)) {
      tnmt <- utils::modifyList(common_rules, tnmt, keep.null = TRUE)
    }
    .validate_tournament(tnmt, slates_manifest, source_file = f)
    class(tnmt) <- c("bbbro_tournament_def", class(tnmt))
    out[[tnmt$tournament_id]] <- tnmt
  }
  out
}

#' Load a single tournament definition by ID
#'
#' Thin wrapper around [load_tournaments()] for ergonomics. Errors if the
#' requested `tournament_id` is not found.
#'
#' @param tournament_id Character ID matching a YAML's `tournament_id` field.
#' @param ... Forwarded to [load_tournaments()].
#' @return One `bbbro_tournament_def` list.
#' @export
load_tournament <- function(tournament_id, ...) {
  all_tnmts <- load_tournaments(...)
  if (!tournament_id %in% names(all_tnmts)) {
    cli::cli_abort(c(
      "Unknown tournament_id: {.val {tournament_id}}",
      i = "Available: {names(all_tnmts)}"
    ))
  }
  all_tnmts[[tournament_id]]
}

#' Resolve an Underdog round_id to its parent tournament_id
#'
#' Used by the extension when only `round_id` is known from the URL. Scans all
#' loaded tournaments' stages for a matching `underdog_round_id`. Returns NULL
#' if no match. Stages with `underdog_round_id == "TBD"` are skipped (so
#' Weekly Winners' placeholder rounds don't false-match each other).
#'
#' @param round_id Character UUID from an Underdog draft URL.
#' @param ... Forwarded to [load_tournaments()].
#' @return The matching `tournament_id` (character), or NULL if no match.
#' @export
resolve_round_to_tournament <- function(round_id, ...) {
  if (is.null(round_id) || length(round_id) == 0 || identical(round_id, "TBD")) {
    return(NULL)
  }
  all_tnmts <- load_tournaments(...)
  for (tnmt in all_tnmts) {
    for (stage in tnmt$stages) {
      rid <- stage$underdog_round_id
      if (!is.null(rid) && !identical(rid, "TBD") && identical(rid, round_id)) {
        return(tnmt$tournament_id)
      }
    }
  }
  NULL
}

# ---- internals ---------------------------------------------------------------

#' Resolve the tournaments directory with dev-mode fallback
#' @keywords internal
.tournaments_dir <- function() {
  pkg <- system.file("data/tournaments", package = "bestballBroSim")
  if (pkg != "" && dir.exists(pkg)) return(pkg)
  root <- .find_package_root()
  if (is.null(root)) return("")
  file.path(root, "inst", "data", "tournaments")
}

#' Validate a tournament definition
#'
#' Checks: required top-level fields, slate cross-reference + position caps,
#' stage seats-entering self-consistency, stage round-ID requirement
#' (relaxed for `structure_type: independent_weekly_pools`), per-table payout
#' tier non-overlap (cross-table overlap is intentional — e.g. BBM7 stacks
#' qualifier and championship payouts).
#'
#' @keywords internal
.validate_tournament <- function(tnmt, slates_manifest, source_file = NA_character_) {
  ctx <- if (is.na(source_file)) "" else sprintf(" (%s)", basename(source_file))

  required <- c("tournament_id", "underdog_tournament_id", "slate_id",
                "scoring", "roster", "stages", "payouts")
  missing_fields <- setdiff(required, names(tnmt))
  if (length(missing_fields) > 0) {
    cli::cli_abort("Tournament definition missing required fields{ctx}: {missing_fields}")
  }

  # Slate cross-reference
  slate_id <- tnmt$slate_id
  if (is.null(slates_manifest$slates[[slate_id]])) {
    cli::cli_abort(c(
      "Tournament {.val {tnmt$tournament_id}} references unknown slate_id {.val {slate_id}}{ctx}.",
      i = "Add it to {.path inst/data/slates/_manifest.yaml} or fix the YAML."
    ))
  }
  slate_def <- slates_manifest$slates[[slate_id]]
  if (is.null(slate_def$position_caps)) {
    cli::cli_abort(c(
      "Slate {.val {slate_id}} (referenced by tournament {.val {tnmt$tournament_id}}) \\
       is missing {.field position_caps}.",
      i = "Add a {.field position_caps} block to that slate in {.path inst/data/slates/_manifest.yaml}."
    ))
  }

  # Stages
  if (length(tnmt$stages) == 0) {
    cli::cli_abort("Tournament {.val {tnmt$tournament_id}} has no stages{ctx}.")
  }
  is_weekly_pools <- identical(tnmt$structure_type, "independent_weekly_pools")

  for (i in seq_along(tnmt$stages)) {
    stage <- tnmt$stages[[i]]
    if (is.null(stage$id)) {
      cli::cli_abort("Stage {i} of {.val {tnmt$tournament_id}} missing {.field id}{ctx}.")
    }
    rid <- stage$underdog_round_id
    if (is.null(rid)) {
      cli::cli_abort(
        "Stage {.val {stage$id}} of {.val {tnmt$tournament_id}} missing {.field underdog_round_id}{ctx}."
      )
    }
    if (identical(rid, "TBD") && !is_weekly_pools) {
      cli::cli_abort(c(
        "Stage {.val {stage$id}} of {.val {tnmt$tournament_id}} has {.val TBD} \\
         underdog_round_id, but tournament is not {.field independent_weekly_pools}{ctx}.",
        i = "Populate the round ID from Underdog's API response."
      ))
    }
  }

  # Seats-entering self-consistency (advancement tournaments only; weekly pools
  # have independent full-field stages, no advancement chain).
  if (!is_weekly_pools) {
    for (i in seq_len(length(tnmt$stages) - 1)) {
      stage <- tnmt$stages[[i]]
      next_stage <- tnmt$stages[[i + 1]]
      adv <- stage$advancement
      seats_in <- stage$seats_entering
      pod_size <- stage$pod_size
      if (is.null(adv) || is.null(seats_in) || is.null(pod_size)) next
      adv_type <- adv$type
      if (identical(adv_type, "top_n_by_pod")) {
        n_per_pod <- adv$n %||% 1L
        expected_next <- (seats_in / pod_size) * n_per_pod
        actual_next <- next_stage$seats_entering
        if (!is.null(actual_next) && !isTRUE(all.equal(expected_next, actual_next))) {
          cli::cli_abort(c(
            "Seats-entering math mismatch in {.val {tnmt$tournament_id}}{ctx}:",
            i = "Stage {.val {stage$id}}: {seats_in} / {pod_size} * {n_per_pod} = {expected_next}",
            x = "Stage {.val {next_stage$id}} declares seats_entering = {actual_next}"
          ))
        }
      }
    }
  }

  # Payouts: per-table tier non-overlap. Cross-table overlap is intentional.
  .validate_payout_tables(tnmt, ctx)

  invisible(TRUE)
}

#' @keywords internal
.validate_payout_tables <- function(tnmt, ctx = "") {
  tables <- list()
  payouts <- tnmt$payouts
  # The payouts block can take a few shapes across our tournaments:
  #   - {tiers: [...]}                                  (Eliminator-style)
  #   - {qualifier_round: {tiers: [...]}, championship_round: {tiers: [...]}}  (BBM7-style)
  #   - {championship_round: {tiers: [...]}}            (Frenchie3 SFLEX)
  #   - {per_week_tiers: [...]}                          (Weekly Winners)
  if (!is.null(payouts$tiers)) tables[["tiers"]] <- payouts$tiers
  if (!is.null(payouts$per_week_tiers)) tables[["per_week_tiers"]] <- payouts$per_week_tiers
  for (sub in c("qualifier_round", "championship_round")) {
    if (!is.null(payouts[[sub]]) && !is.null(payouts[[sub]]$tiers)) {
      tables[[sub]] <- payouts[[sub]]$tiers
    }
  }
  for (table_name in names(tables)) {
    tiers <- tables[[table_name]]
    ranges <- lapply(tiers, function(t) c(t$rank_from, t$rank_to))
    # Reject malformed
    for (i in seq_along(ranges)) {
      r <- ranges[[i]]
      if (length(r) != 2 || any(is.null(r)) || r[1] > r[2]) {
        cli::cli_abort(c(
          "Tournament {.val {tnmt$tournament_id}} payout table {.val {table_name}} \\
           has malformed tier at index {i}{ctx}.",
          i = "Each tier needs {.field rank_from} <= {.field rank_to}."
        ))
      }
    }
    # Check pairwise overlap
    n <- length(ranges)
    if (n < 2) next
    for (i in seq_len(n - 1)) {
      for (j in (i + 1):n) {
        a <- ranges[[i]]; b <- ranges[[j]]
        if (a[1] <= b[2] && b[1] <= a[2]) {
          cli::cli_abort(c(
            "Tournament {.val {tnmt$tournament_id}} payout table {.val {table_name}} \\
             has overlapping tiers{ctx}: ranks {a[1]}-{a[2]} and {b[1]}-{b[2]}."
          ))
        }
      }
    }
  }
  invisible(TRUE)
}
