# ============================================================================
# Position-level availability adjustment
# ============================================================================
# Clay projects starter-caliber players at ~17 games regardless of position;
# the market prices injury attrition (steepest at RB). We scale Clay's points
# by a per-position availability factor BEFORE the calibration curve is built
# and before Clay's points enter the blend, so all three sources -- Clay
# direct and ETR/LegUp via the Clay-fit rank curve -- inherit the corrected
# level. See inst/adjustments/availability.yaml for the prior and its basis,
# and inst/scripts/diag_rb_market_gap.R for the diagnosis.

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

#' Availability factor for one player
#'
#' `min(1, expected_games_pos / clay_games)`. Computed RELATIVE to Clay's
#' own per-player games projection so a player Clay already projects below
#' the positional prior is NOT discounted again -- the factor clamps to 1.
#' Missing/non-positive `clay_games` or a missing prior returns 1 (no-op).
#'
#' @param clay_games Clay's projected games for the player.
#' @param expected_games_pos Positional prior (e.g. RB 15.2).
#' @return Numeric scalar in (0, 1].
#' @keywords internal
.availability_factor <- function(clay_games, expected_games_pos) {
  if (is.null(expected_games_pos) || is.na(expected_games_pos)) return(1)
  if (is.null(clay_games) || is.na(clay_games) || clay_games <= 0) return(1)
  min(1, as.numeric(expected_games_pos) / as.numeric(clay_games))
}

#' Scale Clay's per-player season points by the availability factor
#'
#' Mutates `projected_points_half_ppr` (and `_full_ppr` for parity) on each
#' Clay player record, returning the adjusted `clay_offense` plus a
#' per-position summary for `_meta` logging. MUST run upstream of
#' [build_calibration_curves()] and [.clay_rows_for_slate()] so the whole
#' consensus inherits the corrected level (the calibration curve and Clay's
#' direct blend contribution both read `projected_points_half_ppr`).
#'
#' @param clay_offense Parsed Clay offense feed (`$players` list).
#' @param prior Result of [.load_availability_prior()].
#' @return List with `clay_offense` (possibly adjusted) and `summary` (NULL
#'   when disabled, else a list logged into the feed `_meta`).
#' @keywords internal
.apply_availability_to_clay <- function(clay_offense, prior) {
  if (is.null(clay_offense) || !isTRUE(prior$enabled)) {
    return(list(clay_offense = clay_offense, summary = NULL))
  }
  E       <- prior$expected_games
  players <- clay_offense$players %||% list()
  agg     <- list()  # pos -> c(n, sumf, clamped)

  for (k in seq_along(players)) {
    p   <- players[[k]]
    pos <- p$position %||% NA_character_
    if (is.na(pos) || is.null(E[[pos]])) next

    f <- .availability_factor(p$games, E[[pos]])
    if (!is.null(p$projected_points_half_ppr)) {
      players[[k]]$projected_points_half_ppr <-
        as.numeric(p$projected_points_half_ppr) * f
    }
    if (!is.null(p$projected_points_full_ppr)) {
      players[[k]]$projected_points_full_ppr <-
        as.numeric(p$projected_points_full_ppr) * f
    }

    a <- agg[[pos]] %||% c(n = 0, sumf = 0, clamped = 0)
    a["n"]    <- a["n"] + 1
    a["sumf"] <- a["sumf"] + f
    if (f >= 1) a["clamped"] <- a["clamped"] + 1  # untouched (no double-discount)
    agg[[pos]] <- a
  }
  clay_offense$players <- players

  per_position <- lapply(names(E), function(pos) {
    a <- agg[[pos]]
    if (is.null(a) || a[["n"]] == 0) {
      return(list(expected_games = E[[pos]], players_scaled = 0L,
                  players_clamped = 0L, mean_factor = 1))
    }
    list(
      expected_games  = E[[pos]],
      players_scaled  = as.integer(a[["n"]] - a[["clamped"]]),
      players_clamped = as.integer(a[["clamped"]]),
      mean_factor     = round(a[["sumf"]] / a[["n"]], 4)
    )
  })
  names(per_position) <- names(E)

  list(
    clay_offense = clay_offense,
    summary = list(
      mechanism = paste0(
        "clay points scaled by min(1, expected_games[pos] / clay_games) ",
        "upstream of rank calibration"),
      expected_games = E,
      per_position   = per_position
    )
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
