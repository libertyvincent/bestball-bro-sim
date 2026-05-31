# BBM7 thin wrappers over the config-driven engine in R/tournament_ev.R.
#
# The BBM7 EV vertical (#16/#18) was generalized into a config-driven
# engine (`compute_team_ev`, `build_field_payouts`,
# `tournament_field_multiple`, `resolve_tournament_field_size`,
# `simulate_per_stage_scores`). These `*_bbm7*` names are retained as thin
# wrappers so existing callers/tests keep working; each just forwards to
# the generic function with `bbm7.yaml`'s config. There is no BBM7-specific
# logic here.

#' BBM7 money-conservation-valid field base (= [tournament_field_multiple()]).
#' @inheritParams tournament_field_multiple
#' @export
bbm7_field_multiple <- function(tournament_cfg) {
  tournament_field_multiple(tournament_cfg)
}

#' Snap a field size to a BBM7-valid multiple (= [resolve_tournament_field_size()]).
#' @inheritParams resolve_tournament_field_size
#' @export
resolve_bbm7_field_size <- function(tournament_cfg, n_field, snap = TRUE) {
  resolve_tournament_field_size(tournament_cfg, n_field, snap = snap)
}

#' BBM7 field payouts (= [build_field_payouts()]).
#' @inheritParams build_field_payouts
#' @export
build_bbm7_field_payouts <- function(scores, tournament_cfg, seed = NULL) {
  build_field_payouts(scores, tournament_cfg, seed = seed)
}

#' BBM7 single-team EV + per-player attribution (= [compute_team_ev()]).
#'
#' Forwards to [compute_team_ev()] and adds the historical
#' `advance_probs$qf/$sf/$final` aliases (derived from the generic
#' `by_stage` vector) for back-compatibility.
#' @inheritParams compute_team_ev
#' @export
compute_team_bbm7_ev <- function(pod_rosters, team_entry_id, positions,
                                 layerA_draws, schedule, lineup_spec,
                                 tournament_cfg, field_cache,
                                 corr_params = default_corr_params,
                                 n_sims = 10000L, seed = NULL) {
  res <- compute_team_ev(
    pod_rosters = pod_rosters, team_entry_id = team_entry_id,
    positions = positions, layerA_draws = layerA_draws, schedule = schedule,
    lineup_spec = lineup_spec, tournament_cfg = tournament_cfg,
    field_cache = field_cache, corr_params = corr_params,
    n_sims = n_sims, seed = seed)
  bs <- res$advance_probs$by_stage
  if (length(bs) >= 4L) {
    res$advance_probs$qf    <- bs[3L]
    res$advance_probs$sf    <- bs[4L]
    res$advance_probs$final <- bs[4L]
  }
  res
}
