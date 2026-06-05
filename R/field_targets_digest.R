#' Committed field-targets digest (so CI builds the *validated* field)
#'
#' The synthetic field [generate_field()] draws -- and therefore the
#' Artifact B curves -- depend on the [compute_field_targets()] output
#' (per-position mean counts + per-slot ADP sigma). Those come from the
#' scraped Underdog draft exports, which are large, local-only, and
#' gitignored, so they are absent in CI. Without them CI would fall back to
#' [.default_field_targets()] (ADP-only) and publish curves nothing in the
#' validation chain (the #30 fixture, the oracle re-gate) ever saw -- those
#' were built from the *scraped* field.
#'
#' The digest closes that gap: it commits the **summary stats only** (means
#' / rates / per-slot sigma -- never the raw drafts) so CI can rebuild the
#' exact scraped field. It is a snapshot: refresh it whenever drafts are
#' re-scraped (see [write_field_targets_digest()] /
#' `inst/scripts/refresh_field_targets_digest.R`).
#'
#' Only the finite per-slot sigmas are stored; slots with no estimate are
#' omitted and [generate_field()] applies its global-sigma fallback to them
#' -- byte-for-byte the same behaviour as the NA-carrying scraped targets,
#' since the global sigma is the mean of the finite ones either way.

#' Path to a slate's committed field-targets digest (or `""` if absent).
#' @keywords internal
.field_targets_digest_path <- function(slate_id) {
  .inst_path("data/field_targets", paste0(slate_id, ".json"))
}

#' Write the field-targets digest for a slate from the scraped drafts
#'
#' Computes [compute_field_targets()] from [load_scraped_drafts()] and
#' serializes the summary stats to `inst/data/field_targets/<slate>.json`
#' at full float precision (so the round-trip reproduces the same field).
#'
#' @param slate_id Slate identifier.
#' @param out_dir Output directory (default `inst/data/field_targets`,
#'   relative to the repo root -- run from there).
#' @return Invisibly the written path.
#' @export
write_field_targets_digest <- function(slate_id,
                                       out_dir = file.path("inst", "data", "field_targets")) {
  picks   <- load_scraped_drafts()
  targets <- compute_field_targets(picks, slate_id = slate_id)
  sd_finite <- targets$slot_adp_sd[is.finite(targets$slot_adp_sd)]
  digest <- list(
    slate_id            = slate_id,
    generated_at        = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    n_picks             = nrow(picks),
    position_means      = as.list(targets$position_means),
    qb_stack_2plus_rate = unname(targets$qb_stack_2plus_rate),
    slot_adp_sd         = as.list(sd_finite))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(out_dir, paste0(slate_id, ".json"))
  # digits = 17: the IEEE-754 round-trip guarantee. digits = NA only emits
  # ~15 sig figs, and the tiny target perturbation cascades through the
  # field's per-pick sample() (a few picks flip) -> a measurably different
  # field -> curves off by ~$24 on the convex final ladder. 17 makes
  # generate_field(digest) bit-identical to generate_field(scraped).
  jsonlite::write_json(digest, path, auto_unbox = TRUE, pretty = TRUE,
                       digits = 17, null = "null", na = "null")
  cli::cli_alert_success(
    "Wrote field-targets digest for {.val {slate_id}} ({nrow(picks)} picks) to {.path {path}}")
  invisible(path)
}

#' Load a slate's committed field-targets digest as a `targets` list
#'
#' Round-trips [write_field_targets_digest()] into the shape
#' [generate_field()] consumes (`position_means` / `qb_stack_2plus_rate` /
#' `slot_adp_sd`, the last a named numeric of finite per-slot sigmas).
#'
#' @param slate_id Slate identifier.
#' @return A `targets` list, or `NULL` if no digest is committed.
#' @keywords internal
.load_field_targets_digest <- function(slate_id) {
  path <- .field_targets_digest_path(slate_id)
  if (identical(path, "") || !file.exists(path)) return(NULL)
  d <- jsonlite::fromJSON(path, simplifyVector = TRUE)
  pm <- unlist(d$position_means)
  storage.mode(pm) <- "double"
  sd <- if (length(d$slot_adp_sd) > 0L) {
    v <- unlist(d$slot_adp_sd); storage.mode(v) <- "double"; v
  } else stats::setNames(numeric(0), character(0))
  list(position_means      = pm,
       qb_stack_2plus_rate  = as.numeric(d$qb_stack_2plus_rate %||% NA_real_),
       slot_adp_sd          = sd)
}
