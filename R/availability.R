# ============================================================================
# Position-level availability: draw-level game-zeroing
# ============================================================================
# Clay projects starter-caliber players at ~17 games regardless of position;
# the market prices injury attrition (steepest at RB). Rather than scale points
# at the mean (PR #34), availability is priced as a per-(path, player, week)
# missed-week MASK driven by the position prior: each active week is kept with
# probability q_i = min(1, expected_games[pos] / clay_games_i). The points that
# enter the blend and the calibration curve are now UNSCALED (conditional-on-
# playing); the prior(/clay_games) only sets the per-player miss rate
# `availability_p_miss` stamped onto each feed record. The mask is then applied
# downstream -- in the Layer A season-stat recompute (R/simulate.R) and in the
# tensor build (R/ev_blocks.R) -- as the same process, independently realized.
# See DRAW_ZEROING_DESIGN.md, inst/adjustments/availability.yaml, and
# inst/scripts/diag_rb_market_gap.R for the diagnosis.

#' Load the per-position availability prior
#'
#' @param path Path to `availability.yaml`. Defaults to the packaged file
#'   under `inst/adjustments/`.
#' @return List with `enabled` (logical) and `expected_games` (named list
#'   QB/RB/WR/TE -> numeric games). A missing/disabled file yields
#'   `enabled = FALSE`, which makes [.apply_availability_to_clay()] a no-op.
#' @keywords internal
.load_availability_prior <- function(
    path = .inst_path("adjustments", "availability.yaml")) {
  if (path == "" || !file.exists(path)) {
    cli::cli_alert_warning(
      "availability prior not found; availability adjustment disabled")
    return(list(enabled = FALSE, expected_games = list()))
  }
  y <- yaml::read_yaml(path)
  list(
    enabled        = isTRUE(y$enabled),
    expected_games = y$expected_games %||% list()
  )
}

#' Per-player missed-week probability
#'
#' `p_miss = max(0, 1 - min(1, expected_games_pos / clay_games))`. The kept
#' fraction `q = min(1, expected_games_pos / clay_games)` is computed RELATIVE
#' to Clay's own per-player games projection, so a player Clay already projects
#' at/below the positional prior gets `q = 1` (`p_miss = 0`) -- no second
#' discount on suspensions/known absences. This carries PR #34's per-player
#' clamp exactly, now as a mask rate instead of a point scale. A missing
#' `clay_games` defaults to a full 17-game season, giving the canonical
#' position rate `1 - prior/17` (the right level for ETR/LegUp-only players,
#' who previously inherited the position-level scaling via the calibration
#' spline). A disabled prior or missing positional prior returns `0` (no mask).
#'
#' @param clay_games Clay's projected games for the player (or `NA`).
#' @param expected_games_pos Positional prior (e.g. RB 15.2).
#' @param enabled Whether the availability prior is enabled.
#' @return Numeric scalar in [0, 1).
#' @keywords internal
.availability_p_miss <- function(clay_games, expected_games_pos,
                                 enabled = TRUE) {
  if (!isTRUE(enabled)) return(0)
  if (is.null(expected_games_pos) || is.na(expected_games_pos)) return(0)
  g <- if (is.null(clay_games) || is.na(clay_games) || clay_games <= 0) {
    17
  } else {
    as.numeric(clay_games)
  }
  q <- min(1, as.numeric(expected_games_pos) / g)
  max(0, 1 - q)
}

#' Build the `_meta.availability_mechanism` marker
#'
#' Replaces PR #34's `availability_adjustment` scaling summary. Describes the
#' draw-zeroing mechanism, the priors, and the canonical (`clay_games = 17`)
#' per-position miss rates. Asserted by markers (not shas) per the process
#' rules. Returns `NULL` when the prior is disabled.
#'
#' @param prior Result of [.load_availability_prior()].
#' @keywords internal
.availability_mechanism_marker <- function(prior) {
  if (!isTRUE(prior$enabled)) return(NULL)
  E <- prior$expected_games
  p_miss_canon <- lapply(E, function(g) round(1 - as.numeric(g) / 17, 4))
  names(p_miss_canon) <- names(E)
  list(
    type                 = "draw_zeroing",
    missed_week_model    = "iid_bernoulli",
    applied_over         = "17 active (non-bye) weeks of the 18-week grid",
    rate                 = "p_miss_i = max(0, 1 - expected_games[pos] / clay_games_i)",
    expected_games       = E,
    p_miss_canonical_17g = p_miss_canon
  )
}

#' Write the Brooks-class availability/role review list
#'
#' Extreme per-player overshoots that the POSITION-level factor intentionally
#' does NOT fix: torn-ACL / buried-handcuff cases where the market prices a
#' near-zero role but the sources project a full season. Surfaced as
#' candidates for manual `adjustments:` entries -- never silently absorbed.
#'
#' Flag criteria, on the shipped (post-availability) `season_mean`:
#'   (a) season_mean / underdog_projected_points > 2  AND  ud >= 5, OR
#'   (b) ud < 20  AND  season_mean > 40   (UD implies ~no role; sources see a
#'       real player)
#'
#' @return List with `count`, `path`, and the `df` of flagged players.
#' @keywords internal
.write_availability_review <- function(slate_id, players_out, out_dir = "build") {
  rows <- list()
  for (p in players_out) {
    sm <- p$season_mean
    ud <- p$underdog_projected_points
    if (is.null(sm) || is.null(ud) || is.na(sm) || is.na(ud) || ud <= 0) next
    ratio <- sm / ud
    near_zero_role <- ud < 20 && sm > 40
    overshoot      <- ratio > 2 && ud >= 5
    if (!near_zero_role && !overshoot) next
    rows[[length(rows) + 1L]] <- data.frame(
      underdog_id               = p$underdog_id %||% NA_character_,
      name                      = p$name %||% NA_character_,
      position                  = p$position %||% NA_character_,
      season_mean               = round(sm, 1),
      underdog_projected_points = round(ud, 1),
      ratio                     = round(ratio, 2),
      reason                    = if (near_zero_role) "near_zero_role" else "overshoot",
      stringsAsFactors = FALSE
    )
  }
  review_df <- if (length(rows)) {
    do.call(rbind, rows)
  } else {
    data.frame(underdog_id = character(0), name = character(0),
               position = character(0), season_mean = numeric(0),
               underdog_projected_points = numeric(0), ratio = numeric(0),
               reason = character(0), stringsAsFactors = FALSE)
  }
  if (nrow(review_df)) review_df <- review_df[order(-review_df$ratio), ]

  path <- file.path(out_dir, sprintf("availability_review_%s.txt", slate_id))
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.table(review_df, path, sep = "\t", row.names = FALSE,
                     quote = FALSE, fileEncoding = "UTF-8")
  list(count = nrow(review_df), path = path, df = review_df)
}
