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
#'
#' Hot path: avoid per-column sorts (apply() over 10K columns of small
#' vectors is dominated by R dispatch overhead). Instead, fetch only the
#' specific order statistics we need via [matrixStats::colOrderStats()]
#' -- a single linearized C call per (position, rank).
#' @keywords internal
.week_total <- function(M, positions, lineup_spec, n_sims) {
  slots <- lineup_spec$slots
  pure_slots <- Filter(function(s) length(s$eligible) == 1L &&
                                   s$eligible == s$pos, slots)
  multi_slots <- Filter(function(s) !(length(s$eligible) == 1L &&
                                      s$eligible == s$pos), slots)

  positions_in_play <- unique(c(
    vapply(pure_slots, `[[`, character(1), "pos"),
    unlist(lapply(multi_slots, `[[`, "eligible"))
  ))
  pos_mats <- list()
  for (p in positions_in_play) {
    pids <- names(positions)[positions == p]
    pids <- intersect(pids, rownames(M))
    pos_mats[[p]] <- if (length(pids) == 0L) {
      matrix(0, nrow = 0L, ncol = n_sims)
    } else {
      M[pids, , drop = FALSE]
    }
  }

  consumed <- stats::setNames(integer(length(positions_in_play)),
                              positions_in_play)
  total <- numeric(n_sims)
  for (s in pure_slots) {
    p <- s$pos
    Mp <- pos_mats[[p]]
    take <- min(s$n, nrow(Mp))
    if (take > 0L) {
      total <- total + .top_k_sum(Mp, take)
    }
    consumed[p] <- consumed[p] + s$n
  }

  if (length(multi_slots) == 0L) {
    return(total)
  }
  total + .multi_slot_contribution(multi_slots, pos_mats, consumed, n_sims)
}

#' Sum of the top-K order statistics per column.
#'
#' For small K (1, 2, 3), K separate [matrixStats::colOrderStats()] calls
#' beat a full sort: each call is a single linearized C-level partial
#' selection, no per-column R dispatch.
#' @keywords internal
.top_k_sum <- function(M, k) {
  nr <- nrow(M)
  if (nr == 0L || k <= 0L) return(rep(0, ncol(M)))
  k <- min(k, nr)
  if (nr == 1L) return(M[1L, ])  # colOrderStats requires nrow >= 2
  s <- rep(0, ncol(M))
  for (i in seq_len(k)) {
    s <- s + matrixStats::colOrderStats(M, which = nr - i + 1L)
  }
  s
}

#' k-th largest per column, or 0 if `k > nrow(M)`.
#' @keywords internal
.kth_largest <- function(M, k) {
  nr <- nrow(M)
  if (nr < k || k < 1L) return(rep(0, ncol(M)))
  if (nr == 1L) return(M[1L, ])
  matrixStats::colOrderStats(M, which = nr - k + 1L)
}

#' Closed-form contribution from FLEX (and optionally SFLEX) slots.
#'
#' Identity that lets us skip materializing the leftover pool L:
#' \preformatted{
#'   f1 = max over flex-eligible positions p of the (consumed_p + 1)-th
#'        largest in p's matrix
#'   For Superflex, f2 = max over the same positions, but using the
#'        (consumed_p + 2)-th largest for whichever position contributed
#'        f1 and the (consumed_p + 1)-th largest for the others. Equivalent
#'        to "after pulling f1 from L, the next-best in L is either the
#'        (consumed_p + 2)-th in f1's position, or the (consumed_q + 1)-th
#'        in some other position q."
#' }
#' Then `flex_pair = f1 + pmax(qb2, f2)` per the Season/Superflex derivation
#' in the function docstring.
#' @keywords internal
.multi_slot_contribution <- function(multi_slots, pos_mats, consumed, n_sims) {
  flex_pool_positions <- c("RB", "WR", "TE")
  is_flex  <- function(s) setequal(s$eligible, flex_pool_positions) && s$n == 1L
  is_sflex <- function(s) setequal(s$eligible, c("QB", flex_pool_positions)) && s$n == 1L

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

  firsts <- vapply(flex_pool_positions, function(p) {
    Mp <- pos_mats[[p]]
    if (is.null(Mp)) return(rep(0, n_sims))
    .kth_largest(Mp, (consumed[p] %||% 0L) + 1L)
  }, numeric(n_sims))
  # `firsts` is now [n_sims x 3]; column j == best leftover from position j.
  if (!is.matrix(firsts)) firsts <- matrix(firsts, nrow = n_sims)
  f1 <- matrixStats::rowMaxs(firsts)

  if (!has_sflex) {
    return(f1)  # Season FLEX
  }

  seconds <- vapply(flex_pool_positions, function(p) {
    Mp <- pos_mats[[p]]
    if (is.null(Mp)) return(rep(0, n_sims))
    .kth_largest(Mp, (consumed[p] %||% 0L) + 2L)
  }, numeric(n_sims))
  if (!is.matrix(seconds)) seconds <- matrix(seconds, nrow = n_sims)

  f1_idx <- max.col(firsts, ties.method = "first")
  firsts_after <- firsts
  swap_idx <- cbind(seq_len(n_sims), f1_idx)
  firsts_after[swap_idx] <- seconds[swap_idx]
  f2 <- matrixStats::rowMaxs(firsts_after)

  qb_consumed <- consumed[["QB"]] %||% 0L
  qb2 <- .kth_largest(pos_mats[["QB"]], qb_consumed + 1L)

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
