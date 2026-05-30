#' Layer B — stage-based advancement engine
#'
#' Given a validated tournament config and simulation draws, computes
#' per-roster advance probabilities and per-stage payout EV. **Generic over
#' tournament structure** — the same engine handles BBM (multi-stage
#' advancement), Weekly Winners (17 independent stages, no advancement),
#' and any superflex / Big Board / Puppy variant via the tournament config.
#'
#' @param tournament_cfg A `bbbro_tournament_def` from [load_tournament()]
#'   (canonical Sprint 3a loader; reads `inst/data/tournaments/`).
#' @param sim_draws Output of `run_season_sims()`.
#' @param rosters Optional roster matrix to score; if NULL, this function
#'   only computes the per-(stage, week) thresholds needed by callers.
#' @return A list of:
#'   - `stage_thresholds`: per-stage cut points
#'   - `advance_probs`: per-roster per-stage advance probabilities (if rosters given)
#'   - `payout_evs`: per-roster expected dollar payouts (if rosters given)
#' @export
run_stage_engine <- function(tournament_cfg, sim_draws, rosters = NULL) {
  cli::cli_abort(c(
    "Not yet implemented.",
    i = "See LAYER_B.md and the canonical tournament configs in inst/data/tournaments/."
  ))
}

#' Pre-compute Layer B building blocks for a tournament
#'
#' Runs `n_mock_drafts` mock drafts × `n_sims_per_draft` season sims, then
#' computes replacement levels, scarcity curves, reference roster
#' constructions, marginal advance contributions, and leverage scores.
#'
#' Output is the `building_blocks/{tournament_id}.json` file consumed by
#' the extension's live recommendation engine in `dataJoin.js`. See
#' FEED_SPEC.md for the exact output shape.
#'
#' @param tournament_cfg A `bbbro_tournament_def` object from [load_tournament()].
#' @param projections Output of `generate_projections()`.
#' @param sim_draws Output of `run_season_sims()`.
#' @param n_mock_drafts Number of mock drafts to simulate. Default 10000.
#' @param n_sims_per_draft Season sims per drafted roster. Default 1000.
#' @return A list with elements `replacement_levels`, `scarcity_curves`,
#'   `reference_constructions`, `marginal_contributions`, `leverage_scores`,
#'   `payout_curve`, plus a `_meta` block.
#' @export
precompute_building_blocks <- function(tournament_cfg,
                                       projections,
                                       sim_draws,
                                       n_mock_drafts = 10000L,
                                       n_sims_per_draft = 1000L) {
  cli::cli_abort(c(
    "Not yet implemented.",
    i = "See LAYER_B.md sections 'Offline pre-computation' and 'Open decisions'."
  ))
}

#' Simulate the field of opponent drafters
#'
#' Implements the field model (LAYER_B.md DECISION 1). v1 uses ADP +
#' roster construction priors calibrated from BBM3–6 historical pick data.
#'
#' @param tournament_cfg A `bbbro_tournament_def` object from [load_tournament()].
#' @param projections Projection table for ADP and player universe.
#' @param n_drafts Number of mock drafts to run.
#' @return A long tibble of (draft_id, pick_number, drafter_id, player_id).
#' @export
simulate_field_drafts <- function(tournament_cfg, projections, n_drafts) {
  cli::cli_abort("Not yet implemented")
}
