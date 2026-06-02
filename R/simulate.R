#' Monte Carlo season simulation for a slate's blended projections
#'
#' Replaces the analytical Normal-distribution percentiles produced by
#' `blend_slate()` with empirical percentiles from `n_sims` simulated
#' 17-active-week seasons per player. Also returns the full per-player
#' per-week draws in long format for downstream Layer B consumption.
#'
#' Two-level model:
#'   1. **Epistemic outer** -- for each sim, draw a `true_season_mean` from
#'      `Normal(season_mean, disagreement_std)`. Reflects "we don't know
#'      the player's true mean because the sources disagree."
#'   2. **Aleatoric inner** -- for each sim and each non-bye week, draw
#'      `weekly_draw[i, w] = max(0, rnorm(1, weekly_mean[w] * scale_i,
#'      weekly_std[w] * scale_i))` where `scale_i = true_means[i] /
#'      season_mean`. Reflects "even knowing the true mean, weekly
#'      outcomes vary." Per-week std scales with the per-week mean so
#'      the position CV is preserved per draw.
#'   3. Bye weeks emit 0; clipping at zero prevents negative points.
#'
#' If `disagreement_std == 0` (rare; e.g. Josh Allen when all three
#' sources agree exactly), the outer draw degenerates to a constant
#' (`scale_i == 1`) and the model reduces to pure aleatoric.
#'
#' @param feed The full feed list from `blend_slate()` (with `_meta`
#'   and `players`). The simulator overwrites `season_percentiles` and
#'   each `weekly[[w]]$percentiles` block in-place with empirical values.
#' @param n_sims Number of season simulations per player (default 10000).
#' @param seed Optional RNG seed for reproducibility. `NULL` = no seed.
#' @return A list with:
#'   - `enriched_feed`: same shape as `feed`, with empirical percentiles
#'     replacing analytical ones.
#'   - `draws`: long-format data.frame with columns
#'     `underdog_id, sim_idx, week, draw_value` -- only players with a
#'     usable projection contribute rows.
#' @export
simulate_slate <- function(feed, n_sims = 10000L, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  n_sims <- as.integer(n_sims)
  if (is.na(n_sims) || n_sims < 1L) {
    cli::cli_abort("n_sims must be a positive integer")
  }

  players <- feed$players
  chunks  <- vector("list", length(players))

  for (i in seq_along(players)) {
    p   <- players[[i]]
    sim <- .simulate_player(p, n_sims)
    if (!is.null(sim)) {
      players[[i]] <- .apply_empirical_percentiles(p, sim)
      chunks[[i]]  <- .draws_long_chunk(p$underdog_id, sim)
    } else {
      players[[i]] <- p
    }
  }
  feed$players <- players

  chunks <- Filter(Negate(is.null), chunks)
  draws  <- if (length(chunks) == 0L) {
    data.frame(underdog_id = character(0), sim_idx = integer(0),
               week = integer(0), draw_value = numeric(0),
               stringsAsFactors = FALSE)
  } else {
    do.call(rbind, chunks)
  }

  list(enriched_feed = feed, draws = draws)
}

#' Simulate one player's season as an n_sims x n_weeks matrix of draws.
#'
#' Returns `NULL` for players with no projection (zero source matches,
#' `season_mean` NULL/NA). For non-bye weeks, draws are clipped at zero.
#' Bye weeks are 0 in every sim.
#'
#' @param player A single player record (one value of `feed$players`).
#' @param n_sims Integer.
#' @keywords internal
.simulate_player <- function(player, n_sims) {
  sm <- player$season_mean
  if (is.null(sm) || is.na(sm)) return(NULL)
  weekly <- player$weekly
  if (length(weekly) == 0L) return(NULL)

  disagreement <- player$disagreement_std %||% 0
  if (is.null(disagreement) || is.na(disagreement) || disagreement < 0) {
    disagreement <- 0
  }

  # Outer: per-sim scale factor from the disagreement Normal. Clip
  # negative scale at 0 -- a negative "true season mean" is non-physical
  # (would also give rnorm a negative sd downstream). For modestly
  # disputed players this never fires; for extreme disputes a small
  # fraction of sims pin at 0 for the week.
  if (disagreement > 0 && sm > 0) {
    true_means <- stats::rnorm(n_sims, mean = sm, sd = disagreement)
    scale      <- pmax(0, true_means / sm)
  } else {
    scale <- rep(1, n_sims)
  }

  n_weeks <- length(weekly)
  draws   <- matrix(0, nrow = n_sims, ncol = n_weeks)

  for (w in seq_len(n_weeks)) {
    wd <- weekly[[w]]
    if (isTRUE(wd$is_bye)) next
    wm <- wd$mean %||% 0
    ws <- wd$std  %||% 0
    if (is.na(wm) || wm <= 0) next
    if (is.na(ws) || ws <= 0) {
      # Zero per-week std (degenerate). Scale-only shift.
      draws[, w] <- pmax(0, wm * scale)
      next
    }
    draws[, w] <- pmax(0, stats::rnorm(n_sims,
                                       mean = wm * scale,
                                       sd   = ws * scale))
  }
  draws
}

#' Overwrite analytical percentiles on a player record with empirical
#' values computed from a draws matrix.
#' @keywords internal
.apply_empirical_percentiles <- function(player, draws_matrix) {
  season_sums <- rowSums(draws_matrix)
  qs <- stats::quantile(season_sums, c(0.10, 0.25, 0.50, 0.75, 0.90),
                        names = FALSE, type = 7)
  player$season_percentiles <- list(
    p10 = round(qs[1], 1), p25 = round(qs[2], 1), p50 = round(qs[3], 1),
    p75 = round(qs[4], 1), p90 = round(qs[5], 1)
  )

  weekly <- player$weekly
  for (w in seq_along(weekly)) {
    if (isTRUE(weekly[[w]]$is_bye)) {
      weekly[[w]]$percentiles <- list(p10 = 0, p50 = 0, p90 = 0)
    } else {
      wq <- stats::quantile(draws_matrix[, w], c(0.10, 0.50, 0.90),
                            names = FALSE, type = 7)
      weekly[[w]]$percentiles <- list(
        p10 = round(wq[1], 2), p50 = round(wq[2], 2), p90 = round(wq[3], 2)
      )
    }
  }
  player$weekly <- weekly
  player
}

#' Long-format chunk for one player's draws matrix.
#' @keywords internal
.draws_long_chunk <- function(underdog_id, draws_matrix) {
  n_sims  <- nrow(draws_matrix)
  n_weeks <- ncol(draws_matrix)
  total   <- n_sims * n_weeks
  data.frame(
    underdog_id = rep(underdog_id, total),
    sim_idx     = rep(seq_len(n_sims), times = n_weeks),
    week        = rep(seq_len(n_weeks), each = n_sims),
    draw_value  = as.vector(draws_matrix),
    stringsAsFactors = FALSE
  )
}

# The run_season_sims() stub that used to live here is gone: the Layer B
# precompute it was reserved for is now the EV building-blocks pipeline
# (R/ev_blocks.R), which consumes simulate_slate()'s draws directly.
