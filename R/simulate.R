#' Run Monte Carlo season simulations
#'
#' Takes a projection table (from `generate_projections()`) and simulates
#' `n_sims` full seasons, sampling each player's weekly outcomes from their
#' distribution and respecting the pairwise correlation structure.
#'
#' Output is the source for the `sim_draws.parquet` file consumed by Layer B
#' offline pre-computation and (eventually) the DFS optimizer.
#'
#' @param projections Tibble from `generate_projections()`.
#' @param n_sims Number of seasons to simulate. Default 10000.
#' @return A long tibble with columns: `sim_id`, `underdog_player_id`,
#'   `week`, `points`. Joint correlations preserved within `sim_id`.
#' @export
run_season_sims <- function(projections, n_sims = 10000L) {
  cli::cli_abort(c(
    "Not yet implemented.",
    i = "See LAYER_A.md DECISIONs 3 + 4 for distribution + correlation methodology."
  ))
}
