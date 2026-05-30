#' Best-ball starting-lineup optimizer
#'
#' Given a roster's per-player per-week scores (typically the 3b-2
#' correlated draws) and a slate's structured lineup spec, returns the
#' **optimal starting-lineup total per (sim, week)**.
#'
#' Best-ball lineup totals are separable per (sim, week), so we compute
#' them with vectorized matrix ops -- never loop over sims. For each
#' week we build per-position `[n_pos_players x n_sims]` matrices,
#' column-wise sort them descending, then apply the spec's slots:
#'
#' 1. Pure-position slots take the top-n by score; the remainder of that
#'    position's sorted column becomes a "leftover" available to multi-
#'    position slots.
#' 2. **Season** -- one FLEX slot ({RB,WR,TE}): the leftover pool is
#'    `L = rbind(RB_rest, WR_rest, TE_rest)` and the FLEX contributes
#'    `colMax(L)` per sim.
#' 3. **Superflex** -- one FLEX{RB,WR,TE} + one SFLEX{QB,RB,WR,TE}: the
#'    FLEX always starts the best leftover flex-eligible player; the only
#'    choice is whether SFLEX takes the second-best leftover flex-
#'    eligible (`f2`) or the second-best QB (`qb2`). Closed form:
#'    \preformatted{
#'    Option A: SFLEX = qb2, FLEX = f1   ->  total contribution qb2 + f1
#'    Option B: SFLEX = f2,  FLEX = f1   ->  total contribution f1  + f2
#'    max(A, B) = f1 + max(qb2, f2)
#'    }
#'    so `flex_pair = f1 + pmax(qb2, f2)`.
#'
#' Missing slots (bye / short position / no second QB) substitute 0,
#' which is the right boundary for both the sum and the `pmax` formula.
#' Layer A / 3b-2 already emit zero draws for byes, so byed players
#' simply sort to the bottom of their position column and never get
#' picked when a real scorer is available.
#'
#' @param scores Either (a) a long data.frame with columns `underdog_id`,
#'   `sim_idx`, `week`, `draw_value` -- the 3b-2 output format -- or (b)
#'   a named list keyed by week of `[n_players x n_sims]` matrices with
#'   `rownames = underdog_id`. The latter skips the internal pivot when
#'   3b-4 has already reshaped.
#' @param positions Named character vector mapping `underdog_id` ->
#'   position (`"QB"`, `"RB"`, `"WR"`, `"TE"`). Players whose position
#'   is not referenced by the spec contribute to no slot.
#' @param lineup_spec A list with element `slots` -- a list of slot specs
#'   each with `pos` (label), `n` (slot count), and optional `eligible`
#'   (character vector of position labels; defaults to `c(pos)`). Load
#'   from the slate manifest with [load_slate_lineup_spec()].
#' @param weeks Optional integer vector to restrict the weeks computed
#'   over. Defaults to all weeks present in `scores`.
#' @return A numeric matrix of shape `[n_sims x n_weeks]` with
#'   `colnames` set to the week numbers. 3b-4 consumes this directly --
#'   e.g. `rowSums(weekly_totals[, 1:14])` for BBM's qualifier metric.
#' @export
optimize_lineup_totals <- function(scores, positions, lineup_spec,
                                   weeks = NULL) {
  .validate_lineup_spec(lineup_spec)
  if (!is.character(positions) || is.null(names(positions))) {
    cli::cli_abort("`positions` must be a named character vector (underdog_id -> position).")
  }

  per_week <- .as_per_week_matrices(scores)
  if (length(per_week) == 0L) {
    cli::cli_abort("`scores` contains no weeks.")
  }

  available_weeks <- as.integer(names(per_week))
  if (!is.null(weeks)) {
    weeks <- as.integer(weeks)
    missing <- setdiff(weeks, available_weeks)
    if (length(missing) > 0L) {
      cli::cli_abort("Requested weeks not in `scores`: {missing}")
    }
    per_week <- per_week[as.character(weeks)]
    available_weeks <- weeks
  }

  n_sims <- ncol(per_week[[1L]])
  n_weeks <- length(per_week)
  out <- matrix(0, nrow = n_sims, ncol = n_weeks,
                dimnames = list(NULL, as.character(available_weeks)))

  for (wi in seq_len(n_weeks)) {
    M <- per_week[[wi]]
    if (ncol(M) != n_sims) {
      cli::cli_abort(
        "Inconsistent sim count across weeks: week {available_weeks[wi]} has {ncol(M)} sims, expected {n_sims}."
      )
    }
    out[, wi] <- .week_total(M, positions, lineup_spec, n_sims)
  }
  out
}

#' Load a slate's structured lineup spec from the manifest.
#'
#' Reads `inst/data/slates/_manifest.yaml`, pulls the `starting_lineup`
#' block for the given `slate_id`, and normalizes each slot to ensure
#' `eligible` is always populated (defaults to `c(pos)` for pure slots).
#'
#' @param slate_id Slate ID matching a top-level key under `slates:` in
#'   the manifest.
#' @return A list with element `slate_id` and `slots`, where `slots` is
#'   a list of `list(pos, n, eligible)`.
#' @export
load_slate_lineup_spec <- function(slate_id) {
  manifest <- load_slate_manifest()
  entry <- manifest[[slate_id]]
  if (is.null(entry)) {
    cli::cli_abort("Unknown slate_id: {.val {slate_id}}")
  }
  raw <- entry$starting_lineup
  if (is.null(raw) || is.null(raw$slots)) {
    cli::cli_abort(c(
      "Slate {.val {slate_id}} has no `starting_lineup.slots` block.",
      i = "Add the block to `inst/data/slates/_manifest.yaml`."
    ))
  }
  slots <- lapply(raw$slots, function(s) {
    pos <- as.character(s$pos %||% NA_character_)
    if (is.na(pos) || !nzchar(pos)) {
      cli::cli_abort("A slot in slate {.val {slate_id}} is missing `pos`.")
    }
    n <- as.integer(s$n %||% NA_integer_)
    if (is.na(n) || n < 1L) {
      cli::cli_abort("Slot {pos} in slate {.val {slate_id}} has invalid `n`.")
    }
    eligible <- if (is.null(s$eligible)) pos else as.character(s$eligible)
    list(pos = pos, n = n, eligible = eligible)
  })
  spec <- list(slate_id = slate_id, slots = slots)
  .validate_lineup_spec(spec)
  spec
}

# ---- internals --------------------------------------------------------------

#' Compute the optimal lineup total per sim for one week.
#' @keywords internal
.week_total <- function(M, positions, lineup_spec, n_sims) {
  slots <- lineup_spec$slots
  pure_slots <- Filter(function(s) length(s$eligible) == 1L &&
                                   s$eligible == s$pos, slots)
  multi_slots <- Filter(function(s) !(length(s$eligible) == 1L &&
                                      s$eligible == s$pos), slots)

  # Per-position sorted-descending score matrices, and per-position
  # "rest" matrices (rows beyond what the pure slot consumes).
  positions_in_play <- unique(c(
    vapply(pure_slots, `[[`, character(1), "pos"),
    unlist(lapply(multi_slots, `[[`, "eligible"))
  ))
  pos_state <- list()
  for (p in positions_in_play) {
    pids <- names(positions)[positions == p]
    pids <- intersect(pids, rownames(M))
    if (length(pids) == 0L) {
      sorted <- matrix(0, nrow = 0L, ncol = n_sims)
    } else {
      sub <- M[pids, , drop = FALSE]
      sorted <- if (nrow(sub) == 1L) {
        sub
      } else {
        apply(sub, 2L, sort, decreasing = TRUE, method = "quick")
      }
    }
    pos_state[[p]] <- list(sorted = sorted, consumed = 0L)
  }

  total <- numeric(n_sims)
  for (s in pure_slots) {
    p <- s$pos
    sorted <- pos_state[[p]]$sorted
    take <- min(s$n, nrow(sorted))
    if (take > 0L) {
      total <- total +
        colSums(sorted[seq_len(take), , drop = FALSE])
    }
    pos_state[[p]]$consumed <- pos_state[[p]]$consumed + s$n
  }

  if (length(multi_slots) == 0L) {
    return(total)
  }

  # Apply the closed-form for the two specs in 3b-3 scope.
  total + .multi_slot_contribution(multi_slots, pos_state, n_sims)
}

#' Closed-form contribution from FLEX (and optionally SFLEX) slots.
#'
#' Handles exactly the two cases needed by the slates we ship:
#' - 1 FLEX{RB,WR,TE}: contributes colMax of combined RB/WR/TE leftovers.
#' - 1 FLEX{RB,WR,TE} + 1 SFLEX{QB,RB,WR,TE} (Superflex): contributes
#'   `f1 + pmax(qb2, f2)` where f1/f2 are top-2 of the RB/WR/TE leftover
#'   pool and qb2 is the 2nd-best QB.
#'
#' Errors clearly if the spec falls outside these two cases -- a
#' 3b-later sprint can generalize.
#' @keywords internal
.multi_slot_contribution <- function(multi_slots, pos_state, n_sims) {
  flex_pool_positions <- c("RB", "WR", "TE")
  is_flex   <- function(s) setequal(s$eligible, flex_pool_positions) && s$n == 1L
  is_sflex  <- function(s) setequal(s$eligible, c("QB", flex_pool_positions)) && s$n == 1L

  has_flex  <- any(vapply(multi_slots, is_flex,  logical(1)))
  has_sflex <- any(vapply(multi_slots, is_sflex, logical(1)))
  n_recognized <- sum(vapply(multi_slots,
                             function(s) is_flex(s) || is_sflex(s),
                             logical(1)))
  if (n_recognized != length(multi_slots) || !has_flex) {
    cli::cli_abort(c(
      "Unsupported multi-position slot configuration.",
      i = "3b-3 implements: 1 FLEX{{RB,WR,TE}} (Season) or 1 FLEX + 1 SFLEX{{QB,RB,WR,TE}} (Superflex)."
    ))
  }

  L_rows <- lapply(flex_pool_positions, function(p) {
    st <- pos_state[[p]]
    if (is.null(st)) return(matrix(0, nrow = 0L, ncol = n_sims))
    consumed <- st$consumed
    sorted <- st$sorted
    if (consumed >= nrow(sorted)) {
      matrix(0, nrow = 0L, ncol = n_sims)
    } else {
      sorted[(consumed + 1L):nrow(sorted), , drop = FALSE]
    }
  })
  L <- do.call(rbind, L_rows)
  L_sorted <- if (nrow(L) <= 1L) L else apply(L, 2L, sort,
                                              decreasing = TRUE,
                                              method = "quick")
  f1 <- if (nrow(L_sorted) >= 1L) L_sorted[1L, ] else rep(0, n_sims)

  if (!has_sflex) {
    return(f1)  # Season FLEX
  }

  f2 <- if (nrow(L_sorted) >= 2L) L_sorted[2L, ] else rep(0, n_sims)

  qb_state <- pos_state[["QB"]]
  qb2 <- if (!is.null(qb_state) &&
             nrow(qb_state$sorted) >= qb_state$consumed + 1L) {
    qb_state$sorted[qb_state$consumed + 1L, ]
  } else {
    rep(0, n_sims)
  }

  f1 + pmax(qb2, f2)
}

#' Reshape `scores` into a named list-of-matrices keyed by week.
#' @keywords internal
.as_per_week_matrices <- function(scores) {
  if (is.list(scores) && !is.data.frame(scores) &&
      all(vapply(scores, is.matrix, logical(1)))) {
    if (is.null(names(scores)) || any(!nzchar(names(scores)))) {
      cli::cli_abort("List input must be named by week (e.g. `\"1\"`).")
    }
    return(scores[order(as.integer(names(scores)))])
  }
  if (!is.data.frame(scores)) {
    cli::cli_abort(
      "`scores` must be a long data.frame (underdog_id, sim_idx, week, draw_value) or a named list of [players x sims] matrices."
    )
  }
  required <- c("underdog_id", "sim_idx", "week", "draw_value")
  missing <- setdiff(required, colnames(scores))
  if (length(missing) > 0L) {
    cli::cli_abort("`scores` data.frame is missing column(s): {missing}")
  }

  weeks <- sort(unique(as.integer(scores$week)))
  out <- vector("list", length(weeks))
  names(out) <- as.character(weeks)
  for (w in weeks) {
    sub <- scores[as.integer(scores$week) == w, , drop = FALSE]
    pids <- sort(unique(sub$underdog_id))
    n_sims_w <- max(sub$sim_idx)
    M <- matrix(0, nrow = length(pids), ncol = n_sims_w,
                dimnames = list(pids, NULL))
    idx <- cbind(match(sub$underdog_id, pids), sub$sim_idx)
    M[idx] <- sub$draw_value
    out[[as.character(w)]] <- M
  }
  out
}

#' @keywords internal
.validate_lineup_spec <- function(spec) {
  if (!is.list(spec) || is.null(spec$slots) || !is.list(spec$slots) ||
      length(spec$slots) == 0L) {
    cli::cli_abort("`lineup_spec` must be a list with non-empty `slots`.")
  }
  for (s in spec$slots) {
    if (is.null(s$pos) || is.null(s$n) || is.null(s$eligible)) {
      cli::cli_abort(
        "Each slot needs `pos`, `n`, and `eligible` (the loader populates `eligible` from `pos` if absent)."
      )
    }
    if (!is.numeric(s$n) || length(s$n) != 1L || s$n < 1L) {
      cli::cli_abort("Slot `n` must be a single positive integer (slot pos={s$pos}).")
    }
  }
  invisible(TRUE)
}
