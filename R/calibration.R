#' Build calibration curves from Clay's projections
#'
#' For each skill position (QB, RB, WR, TE) we treat Clay's per-player
#' half-PPR totals as the rank-to-points reference: sort Clay's players
#' for that position by `projected_points_half_ppr` descending, assign
#' ranks 1..N, and fit a natural cubic spline. The returned closures
#' clamp inputs to Clay's observed rank range -- a rank deeper than
#' Clay covered returns Clay's lowest value (flat extrapolation) rather
#' than letting the spline tail go negative.
#'
#' Each closure has its own captured environment, so the per-position
#' values (`n`, `pts_min`, `pts_max`, `spline`) are NOT shared across
#' positions -- avoids the classic R loop-closure bug.
#'
#' @param clay_players List of player records (jsonlite-parsed
#'   `players` array from `sources/clay_2026_offense.json`). Each entry
#'   needs `position` and `projected_points_half_ppr`.
#' @return Named list (QB, RB, WR, TE). Each entry is a closure that
#'   takes a numeric `rank` vector and returns a numeric points vector
#'   of the same length.
#' @export
build_calibration_curves <- function(clay_players) {
  skill <- c("QB", "RB", "WR", "TE")
  out <- list()
  for (pos in skill) {
    pts <- vapply(clay_players, function(p) {
      if (is.null(p$position) || p$position != pos) return(NA_real_)
      val <- p$projected_points_half_ppr
      if (is.null(val)) NA_real_ else as.numeric(val)
    }, numeric(1))
    pts <- pts[!is.na(pts)]
    pts <- sort(pts, decreasing = TRUE)
    if (length(pts) < 5L) {
      cli::cli_abort(c(
        "Not enough Clay data points to calibrate {.val {pos}}.",
        i = "Got {length(pts)} usable points; need at least 5."
      ))
    }
    out[[pos]] <- .make_calibration_closure(pts)
  }
  out
}

#' Convert a within-position rank to a point-equivalent value
#'
#' @param rank Numeric vector of within-position ranks (1-indexed).
#' @param position One of `"QB"`, `"RB"`, `"WR"`, `"TE"`.
#' @param curves The result of [build_calibration_curves()].
#' @return Numeric vector of points, same length as `rank`.
#' @export
calibrate_rank_to_points <- function(rank, position, curves) {
  if (is.null(curves[[position]])) {
    cli::cli_abort("No calibration curve for position {.val {position}}.")
  }
  curves[[position]](rank)
}

#' Build the per-position calibration closure
#'
#' Factored out so each closure binds its OWN copy of `n`, `pts_min`,
#' `pts_max`, and `spline` -- a closure created inside a `for` loop
#' shares the loop's environment, so subsequent iterations would
#' overwrite these values across positions if we did this inline.
#'
#' @keywords internal
.make_calibration_closure <- function(pts) {
  n       <- length(pts)
  pts_min <- pts[n]
  pts_max <- pts[1]
  ranks   <- seq_along(pts)
  spline  <- stats::splinefun(ranks, pts, method = "natural")
  function(rank) {
    rank    <- as.numeric(rank)
    clamped <- pmin(pmax(rank, 1), n)
    vals    <- spline(clamped)
    vals[rank > n] <- pts_min  # flat extrapolation beyond Clay's coverage
    vals[rank < 1] <- pts_max  # clamp ranks below 1 to the top value
    vals
  }
}
