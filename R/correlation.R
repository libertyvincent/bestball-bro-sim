#' Default correlation parameters (Option A)
#'
#' Flat literature-derived values used until 3b-5 lands position-pair
#' fitting. Nesting requirement: `cross <= game <= team`.
#'
#' - `team  = 0.45`: same-team pairwise correlation. This is the literature
#'   QB <-> pass-catcher value applied flat across all same-team pairs.
#'   Flat application overstates same-team RB <-> RB and QB <-> RB pairs
#'   (realistically <= 0). **KNOWN LIMITATION** -- to be refined into
#'   position-pair correlations alongside the 3b-5 validator.
#' - `game  = 0.25`: same-game, different-team correlation.
#' - `cross = 0.05`: different-game correlation (a tiny league-wide tide
#'   from shared NFL-week conditions).
#'
#' @export
default_corr_params <- list(team = 0.45, game = 0.25, cross = 0.05)

#' Sample correlated weekly draws via a nested variance-components factor copula
#'
#' Takes Layer A's *marginal* per-player weekly draws and produces
#' *correlated* weekly draws that respect team, same-game, and
#' inter-game correlations while preserving each player's marginal
#' distribution exactly.
#'
#' # Method
#'
#' Latent normal per player i, with **independent** factors **per week**:
#' \deqn{Z_i = \lambda_g G + \lambda_G F_{game(i)} + \lambda_T F_{team(i)} +
#'             \lambda_E E_i}
#' with loadings derived from the requested correlation parameters
#' `c = cross`, `g = game`, `t = team` (requires `c <= g <= t`):
#' \preformatted{
#'   lambda_global = sqrt(c)
#'   lambda_game   = sqrt(g - c)
#'   lambda_team   = sqrt(t - g)
#'   lambda_idio   = sqrt(1 - t)
#' }
#' This makes `Var(Z_i) = 1` and yields pairwise correlations: same team
#' (which implies same game) = `t`; same game, different team = `g`;
#' different game = `c`.
#'
#' Each player's correlated latent `Z_i` is then mapped to its marginal
#' via the empirical inverse CDF of Layer A's week-w draws for player
#' i: `x = quantile_type7(sort(layerA[i, w, ]), pnorm(Z_i))`. This
#' preserves the (clipped, two-level) marginal exactly while imposing
#' the requested rank correlation structure.
#'
#' # Weekly-level only
#'
#' Factors are drawn fresh and independently for every week. This is
#' deliberate. Season-mean epistemic teammate correlation (the
#' "they-share-the-same-true-mean" component that the disagreement
#' draw introduces in [simulate_slate()]) is out of scope here.
#'
#' @section TODO:
#' `# TODO(3b-later): season-level epistemic correlation across teammates`
#'   -- would slot in as a per-team outer scale factor applied to all of a
#'   team's weeks consistently within a sim. Requires teammate-aware
#'   reorganization of Layer A's outer epistemic draw, not just a
#'   per-week tweak here.
#'
#' @param player_ids Character vector of `underdog_id` values. The player
#'   set to sample (a roster for 3b-4, a field team for 3b-6). Team/game
#'   grouping is computed per week among these players only -- a single
#'   roster of one player from team X does not group with other rosters'
#'   players from team X.
#' @param layerA_draws Long-format Layer A draws as produced by
#'   [simulate_slate()] / written by [publish_v2()]: a data.frame with
#'   columns `underdog_id` (chr), `sim_idx` (int), `week` (int),
#'   `draw_value` (num). Bye weeks are present as all-zero rows.
#' @param schedule Per-player-per-week schedule data.frame with columns
#'   `underdog_id`, `week`, `team`, `opponent`, `is_bye`. `team` and
#'   `opponent` are 3-letter NFL abbreviations to match the blender
#'   feed; the unordered game key is `sort(c(team, opponent))`. On bye
#'   rows `opponent` may be `NA` and `is_bye` must be `TRUE`.
#' @param corr_params A list with elements `team`, `game`, `cross` in
#'   \eqn{[0, 1]} satisfying `cross <= game <= team`. Defaults to
#'   [default_corr_params].
#' @param n_sims Output sim count (default 10000L). Independent of the
#'   input draws' sim count -- the input only defines each player's
#'   marginal via its empirical CDF.
#' @param seed Optional integer RNG seed. `NULL` (default) =
#'   nondeterministic.
#' @param precomputed_marginals Optional output of
#'   [precompute_layerA_marginals()] -- a `list[player_id][week_str] = sorted
#'   numeric` cache of Layer A's per-player per-week marginals. When
#'   provided, this skips the internal sort, which is the main batch
#'   speedup when 3b-4 / 3b-5 run the sampler over many rosters that share
#'   the same Layer A slate. Lookups for `(player, week)` pairs missing
#'   from the cache fall back to zero, matching the no-cache behavior for
#'   players with no marginal data.
#' @param output_format `"long"` (default) -- returns the long
#'   data.frame matching Layer A. `"matrix_list"` -- returns a named list
#'   keyed by week of `[n_players x n_sims]` matrices with `rownames =
#'   underdog_id`. The matrix-list form skips a 5M-row data.frame
#'   round-trip and is the right shape to feed straight into
#'   [optimize_lineup_totals()]; it's what [simulate_team_season()] uses
#'   internally.
#' @return Depending on `output_format`, either a data.frame
#'   (`underdog_id, sim_idx, week, draw_value`) of `n_sims * length(weeks) *
#'   length(player_ids)` rows, or a named list of `[n_players x n_sims]`
#'   matrices.
#' @export
sample_correlated_draws <- function(player_ids,
                                    layerA_draws,
                                    schedule,
                                    corr_params = default_corr_params,
                                    n_sims = 10000L,
                                    seed = NULL,
                                    precomputed_marginals = NULL,
                                    output_format = c("long", "matrix_list")) {
  output_format <- match.arg(output_format)
  .validate_corr_params(corr_params)

  player_ids <- unique(as.character(player_ids))
  if (length(player_ids) == 0L) {
    cli::cli_abort("`player_ids` is empty.")
  }

  n_sims <- as.integer(n_sims)
  if (is.na(n_sims) || n_sims < 1L) {
    cli::cli_abort("`n_sims` must be a positive integer.")
  }

  .require_cols(layerA_draws,
                c("underdog_id", "sim_idx", "week", "draw_value"),
                "layerA_draws")
  .require_cols(schedule,
                c("underdog_id", "week", "team", "opponent", "is_bye"),
                "schedule")

  if (!is.null(seed)) set.seed(seed)

  layerA_draws <- layerA_draws[layerA_draws$underdog_id %in% player_ids,
                               , drop = FALSE]
  schedule <- schedule[schedule$underdog_id %in% player_ids, , drop = FALSE]

  weeks <- sort(unique(as.integer(schedule$week)))
  if (length(weeks) == 0L) {
    cli::cli_abort("`schedule` has no rows for any of the requested `player_ids`.")
  }
  n_weeks <- length(weeks)

  marginals <- if (is.null(precomputed_marginals)) {
    .build_marginals(layerA_draws, player_ids, weeks)
  } else {
    missing_pids <- setdiff(player_ids, names(precomputed_marginals))
    if (length(missing_pids) > 0L) {
      cli::cli_abort(
        "`precomputed_marginals` is missing entries for player(s): {missing_pids}"
      )
    }
    precomputed_marginals
  }

  cross <- corr_params$cross
  game  <- corr_params$game
  team  <- corr_params$team
  lam_g <- sqrt(cross)
  lam_G <- sqrt(game - cross)
  lam_T <- sqrt(team - game)
  lam_E <- sqrt(1 - team)

  out_mats <- stats::setNames(
    lapply(player_ids, function(p) matrix(0, nrow = n_sims, ncol = n_weeks)),
    player_ids
  )

  sched_by_week <- split(schedule, as.integer(schedule$week))

  for (wi in seq_along(weeks)) {
    w <- weeks[wi]
    sw <- sched_by_week[[as.character(w)]]
    info <- .classify_players_for_week(player_ids, sw)
    active <- info$active
    team_of <- info$team_of
    game_of <- info$game_of
    n_act <- length(active)
    if (n_act == 0L) next  # all on bye; out_mats stays 0

    G <- stats::rnorm(n_sims)
    unique_games <- unique(game_of)
    unique_teams <- unique(team_of)
    F_game <- matrix(stats::rnorm(n_sims * length(unique_games)),
                     nrow = n_sims, ncol = length(unique_games))
    colnames(F_game) <- unique_games
    F_team <- matrix(stats::rnorm(n_sims * length(unique_teams)),
                     nrow = n_sims, ncol = length(unique_teams))
    colnames(F_team) <- unique_teams
    E <- matrix(stats::rnorm(n_sims * n_act), nrow = n_sims, ncol = n_act)

    Z <- lam_E * E +
      lam_g * matrix(G, nrow = n_sims, ncol = n_act) +
      lam_G * F_game[, game_of, drop = FALSE] +
      lam_T * F_team[, team_of, drop = FALSE]

    U <- stats::pnorm(Z)

    for (j in seq_len(n_act)) {
      pid <- active[j]
      sorted <- marginals[[pid]][[as.character(w)]]
      out_mats[[pid]][, wi] <- .inv_cdf_type7(sorted, U[, j])
    }
  }

  if (output_format == "matrix_list") {
    # Per-week [n_players x n_sims] matrix list -- the shape
    # optimize_lineup_totals() accepts directly. Avoids materializing a
    # 5M-row data.frame for the in-package compose path.
    out_list <- vector("list", n_weeks)
    names(out_list) <- as.character(weeks)
    for (wi in seq_along(weeks)) {
      M <- matrix(0, nrow = length(player_ids), ncol = n_sims,
                  dimnames = list(player_ids, NULL))
      for (j in seq_along(player_ids)) {
        M[j, ] <- out_mats[[player_ids[j]]][, wi]
      }
      out_list[[wi]] <- M
    }
    return(out_list)
  }

  chunks <- lapply(player_ids, function(pid) {
    m <- out_mats[[pid]]
    total <- n_sims * n_weeks
    data.frame(
      underdog_id = rep(pid, total),
      sim_idx     = rep(seq_len(n_sims), times = n_weeks),
      week        = rep(weeks, each = n_sims),
      draw_value  = as.vector(m),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, chunks)
}

# ---- helpers ---------------------------------------------------------------

#' Apply a draw-level availability mask to a matrix-list of correlated draws
#'
#' Multiplies each player's week-w draws by an independent `Bernoulli(q_i)`
#' mask (`q_i = 1 - availability_p_miss`), applied **after** the copula
#' inverse-CDF so missingness stays independent of the latent. Bye-week draws
#' are already 0 (masking is moot). Reseeded off `seed` with a fixed offset so
#' the mask is reproducible yet independent of the copula draws (which consumed
#' `seed`). A no-op when every `q_i >= 1`. Shared by [build_ev_draws()] (tensor)
#' and [predict_pod_xadv()] (validator) so both scoring paths price availability
#' identically. See DRAW_ZEROING_DESIGN.md.
#'
#' @param ml Named (by week) list of `[n_players x n_sims]` matrices with
#'   `rownames = underdog_id`.
#' @param q_by_player Named numeric vector `underdog_id -> kept probability`.
#'   Players missing from it default to `q = 1` (no mask).
#' @param seed Optional integer; the copula seed. The mask uses `seed + offset`.
#' @param seed_offset Integer offset (default 7919L).
#' @keywords internal
.mask_matrix_list <- function(ml, q_by_player, seed = NULL,
                              seed_offset = 7919L) {
  if (length(q_by_player) == 0L || all(q_by_player >= 1, na.rm = TRUE)) {
    return(ml)
  }
  if (!is.null(seed)) set.seed(as.integer(seed) + seed_offset)
  for (w in names(ml)) {
    M   <- ml[[w]]
    ids <- rownames(M)
    for (j in seq_along(ids)) {
      qi <- q_by_player[[ids[j]]]
      if (!is.null(qi) && !is.na(qi) && qi < 1) {
        M[j, ] <- M[j, ] * stats::rbinom(ncol(M), 1L, qi)
      }
    }
    ml[[w]] <- M
  }
  ml
}

#' Validate the correlation-parameter list. Errors on missing names,
#' out-of-range values, or nesting violations.
#' @keywords internal
.validate_corr_params <- function(p) {
  if (!is.list(p)) {
    cli::cli_abort("`corr_params` must be a list with elements `team`, `game`, `cross`.")
  }
  needed <- c("team", "game", "cross")
  missing_names <- setdiff(needed, names(p))
  if (length(missing_names) > 0L) {
    cli::cli_abort("`corr_params` is missing element(s): {missing_names}")
  }
  vals <- vapply(p[needed], function(x) {
    if (length(x) != 1L) NA_real_ else as.numeric(x)
  }, numeric(1))
  if (any(is.na(vals))) {
    cli::cli_abort("`corr_params$team/game/cross` must each be a single numeric value.")
  }
  if (any(vals < 0) || any(vals > 1)) {
    cli::cli_abort(c(
      "All `corr_params` values must lie in [0, 1].",
      i = "Got: team={vals['team']}, game={vals['game']}, cross={vals['cross']}"
    ))
  }
  if (!(vals["cross"] <= vals["game"] && vals["game"] <= vals["team"])) {
    cli::cli_abort(c(
      "`corr_params` must satisfy nesting cross <= game <= team.",
      i = "Got: cross={vals['cross']}, game={vals['game']}, team={vals['team']}"
    ))
  }
  invisible(TRUE)
}

#' @keywords internal
.require_cols <- function(df, cols, arg_name) {
  if (!is.data.frame(df)) {
    cli::cli_abort("`{arg_name}` must be a data.frame.")
  }
  missing <- setdiff(cols, colnames(df))
  if (length(missing) > 0L) {
    cli::cli_abort("`{arg_name}` is missing required column(s): {missing}")
  }
  invisible(TRUE)
}

#' Pre-sort Layer A draws into per-player per-week marginals
#'
#' Sorts each `(underdog_id, week)` group of `layerA_draws$draw_value`
#' once and returns a nested lookup `list[player_id][week_str] = sorted
#' numeric`. This is the cache that [sample_correlated_draws()] uses
#' internally for the empirical inverse-CDF mapping; calling it once on
#' the full slate's player set and passing the result as
#' `precomputed_marginals` to many per-roster invocations is the main
#' batch speedup for 3b-4 / 3b-5 (sort happens once instead of once per
#' roster).
#'
#' @param layerA_draws Long Layer A draws (`underdog_id, sim_idx, week,
#'   draw_value`).
#' @param player_ids Optional character vector restricting the cache to a
#'   subset of players. Defaults to all `underdog_id` values in
#'   `layerA_draws`.
#' @param weeks Optional integer vector restricting the cache to a
#'   subset of weeks. Defaults to all weeks present in `layerA_draws`.
#' @return A named list of named lists: `out[[player_id]][[week_str]]` is
#'   a numeric vector sorted ascending. Players or weeks with no draw
#'   rows produce empty lookups (the inverse-CDF then falls back to 0).
#' @export
precompute_layerA_marginals <- function(layerA_draws,
                                        player_ids = NULL,
                                        weeks = NULL) {
  .require_cols(layerA_draws,
                c("underdog_id", "sim_idx", "week", "draw_value"),
                "layerA_draws")
  if (is.null(player_ids)) {
    player_ids <- sort(unique(as.character(layerA_draws$underdog_id)))
  } else {
    player_ids <- unique(as.character(player_ids))
  }
  if (is.null(weeks)) {
    weeks <- sort(unique(as.integer(layerA_draws$week)))
  } else {
    weeks <- sort(unique(as.integer(weeks)))
  }
  .build_marginals(layerA_draws, player_ids, weeks)
}

#' Build a `[player][week] -> sorted-numeric` lookup of week-w marginal
#' draws for empirical inverse-CDF mapping.
#' @keywords internal
.build_marginals <- function(layerA_draws, player_ids, weeks) {
  marginals <- vector("list", length(player_ids))
  names(marginals) <- player_ids
  if (nrow(layerA_draws) == 0L) {
    for (pid in player_ids) marginals[[pid]] <- list()
    return(marginals)
  }
  by_pid <- split(layerA_draws, layerA_draws$underdog_id)
  for (pid in player_ids) {
    pd <- by_pid[[pid]]
    wk_list <- list()
    if (!is.null(pd) && nrow(pd) > 0L) {
      by_wk <- split(pd$draw_value, as.integer(pd$week))
      for (w in weeks) {
        key <- as.character(w)
        v <- by_wk[[key]]
        if (!is.null(v) && length(v) > 0L) {
          wk_list[[key]] <- sort(as.numeric(v))
        }
      }
    }
    marginals[[pid]] <- wk_list
  }
  marginals
}

#' Determine, for one week, which players in `player_ids` are active
#' (with team + game keys, in active-player order) and which are on bye.
#' @keywords internal
.classify_players_for_week <- function(player_ids, sched_w) {
  if (is.null(sched_w) || nrow(sched_w) == 0L) {
    return(list(active = character(0), team_of = character(0),
                game_of = character(0), bye = player_ids))
  }
  by_pid <- split(sched_w, sched_w$underdog_id)
  active <- character(0)
  bye <- character(0)
  team_of <- character(0)
  game_of <- character(0)
  for (pid in player_ids) {
    row <- by_pid[[pid]]
    if (is.null(row) || nrow(row) == 0L || isTRUE(as.logical(row$is_bye[1]))) {
      bye <- c(bye, pid)
      next
    }
    tm <- as.character(row$team[1])
    opp <- as.character(row$opponent[1])
    if (is.na(tm) || is.na(opp)) {
      # Missing team or opponent on a non-bye row -- treat as bye to avoid
      # building a degenerate group with NA in the key.
      bye <- c(bye, pid)
      next
    }
    active <- c(active, pid)
    team_of <- c(team_of, tm)
    game_of <- c(game_of, paste(sort(c(tm, opp)), collapse = "_"))
  }
  list(active = active, team_of = team_of, game_of = game_of, bye = bye)
}

#' Empirical inverse CDF (type-7, linear interpolation between order
#' statistics) over a pre-sorted reference vector.
#'
#' Returns `0` for every element if `sorted` is `NULL` / length 0;
#' returns the singleton for every element if `length(sorted) == 1`.
#' @keywords internal
.inv_cdf_type7 <- function(sorted, u) {
  if (is.null(sorted) || length(sorted) == 0L) {
    return(rep(0, length(u)))
  }
  m <- length(sorted)
  if (m == 1L) {
    return(rep(sorted, length(u)))
  }
  u <- pmin(pmax(u, 0), 1)
  h <- (m - 1) * u + 1
  q <- as.integer(floor(h))
  q <- pmin(pmax(q, 1L), m - 1L)
  fr <- h - q
  sorted[q] + fr * (sorted[q + 1L] - sorted[q])
}
