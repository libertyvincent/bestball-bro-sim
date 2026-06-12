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
#' Draw-level availability: for each sim, an independent missed-week mask
#' (iid Bernoulli over active weeks, kept w.p. `q = 1 - availability_p_miss`)
#' zeroes weeks the player misses. Shipped `season_mean`/`season_std`/
#' percentiles, per-week `mean`/`std`/percentiles, and VOR/tier/position_rank
#' are all recomputed from the MASKED draws. The returned `draws` (parquet)
#' stay CONDITIONAL-on-playing -- the mask is re-realized independently in the
#' tensor build so it does not entangle with the copula inverse-CDF. See
#' DRAW_ZEROING_DESIGN.md.
#'
#' @param feed The full feed list from `blend_slate()` (with `_meta`
#'   and `players`). The simulator overwrites the season + weekly stats
#'   in-place with masked empirical values and recomputes position metrics.
#' @param n_sims Number of season simulations per player (default 10000).
#' @param seed Optional RNG seed for reproducibility. `NULL` = no seed.
#' @return A list with:
#'   - `enriched_feed`: same shape as `feed`, with masked empirical stats
#'     replacing the analytical ones and post-sim VOR/tiers.
#'   - `draws`: long-format data.frame with columns
#'     `underdog_id, sim_idx, week, draw_value` -- conditional-on-playing
#'     (0 only on byes); only players with a usable projection contribute rows.
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
    p        <- players[[i]]
    sim_cond <- .simulate_player(p, n_sims)   # conditional-on-playing draws
    if (!is.null(sim_cond)) {
      # Draw-level availability mask (per-player q from the blender's rate),
      # applied to the season-stat recompute. Independent of the value draws.
      q      <- 1 - (p$availability_p_miss %||% 0)
      mask   <- .sample_missed_week_mask(p, n_sims, q)
      masked <- sim_cond * mask
      players[[i]] <- .apply_masked_stats(p, masked)
      # Parquet draws stay CONDITIONAL-on-playing (0 only on byes): the mask is
      # re-realized independently in the tensor build (R/ev_blocks.R). Writing
      # the mask here would entangle it with the copula's inverse-CDF.
      chunks[[i]]  <- .draws_long_chunk(p$underdog_id, sim_cond)
    } else {
      players[[i]] <- p
    }
  }
  feed$players <- players

  # VOR / tier / position_rank are post-sim now: recompute from the MASKED
  # season_mean (RB tiers shift when means drop ~11%). Uses the slate's lineup
  # spec for replacement ranks, mirroring blend_slate's provisional pass.
  feed <- .recompute_position_metrics_post_sim(feed)

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

#' Sample a per-(sim, week) missed-week mask for one player
#'
#' iid Bernoulli over the player's ACTIVE (non-bye) weeks: each active week is
#' kept with probability `q = 1 - availability_p_miss`. Bye weeks keep mask 1
#' (their draw is already 0, so masking is moot) and are never candidates for
#' missing -- a bye is not a missed game. `q >= 1` (no attrition) short-circuits
#' to an all-ones mask. Independent of the value draws.
#'
#' @param player A single player record (needs `weekly`).
#' @param n_sims Integer.
#' @param q Kept probability in (0, 1].
#' @return An `n_sims x n_weeks` 0/1 numeric matrix.
#' @keywords internal
.sample_missed_week_mask <- function(player, n_sims, q) {
  weekly  <- player$weekly
  n_weeks <- length(weekly)
  mask    <- matrix(1, nrow = n_sims, ncol = n_weeks)
  if (is.null(q) || is.na(q) || q >= 1) return(mask)
  for (w in seq_len(n_weeks)) {
    if (isTRUE(weekly[[w]]$is_bye)) next
    mask[, w] <- stats::rbinom(n_sims, 1L, q)
  }
  mask
}

#' Recompute a player's shipped stats from MASKED draws
#'
#' Replaces the pre-sim analytical stats with empirical values from the masked
#' draws matrix. Sets `season_mean`/`season_std` (now post-mask, so the old
#' `season_std^2 = disagreement^2 + aleatoric^2` invariant is RETIRED;
#' `disagreement_std`/`aleatoric_std` are left as the pre-mask conditional
#' components), `season_percentiles`, and per-week `mean`/`std`/`percentiles`.
#' Weekly `mean`/`std` are **unconditional** (include the miss chance), so
#' `Sum_active weekly.mean ~= season_mean` is preserved. For attrition-heavy
#' positions a weekly `p10` may legitimately be 0 (e.g. RB, `p_miss > 0.10`).
#' Bye weeks stay 0.
#' @keywords internal
.apply_masked_stats <- function(player, masked) {
  season_sums <- rowSums(masked)
  player$season_mean <- round(mean(season_sums), 1)
  player$season_std  <- round(stats::sd(season_sums), 1)
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
      next
    }
    col <- masked[, w]
    weekly[[w]]$mean <- round(mean(col), 2)
    weekly[[w]]$std  <- round(stats::sd(col), 2)
    wq <- stats::quantile(col, c(0.10, 0.50, 0.90), names = FALSE, type = 7)
    weekly[[w]]$percentiles <- list(
      p10 = round(wq[1], 2), p50 = round(wq[2], 2), p90 = round(wq[3], 2)
    )
  }
  player$weekly <- weekly
  player
}

#' Recompute position_rank / VOR / tier from the post-sim (masked) season_mean
#'
#' Mirrors blend_slate's provisional pass, but on the masked means so VOR and
#' tiers reflect attrition. No-op when the feed carries no `slate_id` or its
#' lineup spec can't be loaded (the provisional blend-time metrics then stand).
#' @keywords internal
.recompute_position_metrics_post_sim <- function(feed) {
  slate_id <- feed[["_meta"]]$slate_id
  if (is.null(slate_id)) return(feed)
  lineup_spec <- tryCatch(load_slate_lineup_spec(slate_id),
                          error = function(e) NULL)
  if (is.null(lineup_spec)) return(feed)
  feed$players <- .add_position_metrics(
    feed$players,
    replacement_ranks = .replacement_ranks_from_lineup(lineup_spec)
  )
  feed
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
