#' Generate a synthetic field of independent draft entries for a slate
#'
#' Produces a population of synthetic rosters meant to statistically
#' resemble the real Underdog drafter pool for use in 3b-7's later
#' tournament stages (QF/SF/Final on random cross-draft pods) and full
#' tournament EV. Calibrated to 3b-1's `field_empirics` targets:
#' per-position mean counts, `qb_stack_2plus` rate, and per-slot ADP
#' variance.
#'
#' **Independence is the modeling choice.** Real-world the field is
#' millions of separate 12-person snake drafts; from any one drafter's
#' perspective, their slot order is fixed by snake position but the
#' other 11 drafters' picks are independent across drafts. The
#' generator captures that by giving each synthetic team an independent
#' snake position and weighting candidates at each slot by ADP
#' proximity -- so a top-1 player appears on every team whose snake
#' position is `1` (roughly `1/12` of the field), some teams whose
#' position is `2` reach for him, etc. That's exactly the field-level
#' ownership / duplication realism the EV calculations need.
#'
#' **Scope.** 3b-6 is the raw entrant field. Advancement, pod
#' formation, stage filtering, and EV are 3b-7. 3b-6 does **not**
#' validate scoring; matching 3b-1's empirics proves it *looks*
#' right structurally. Whether it *scores* right is for 3b-7 against
#' BBMDB and (later) real outcomes.
#'
#' # Mechanism
#'
#' For each team, draw a snake position uniformly in `1..12`. Walk
#' picks `r = 1..18`. At each pick:
#' \enumerate{
#'   \item Compute the overall slot for `(round, snake_pos)` under
#'     standard 12-person snake (round 1 forward, round 2 backward, ...).
#'   \item Build a weight vector over the player pool:
#'     \itemize{
#'       \item **ADP proximity:** `dnorm((adp - slot) / sigma_slot)` using
#'         the empirical per-slot ADP SD from `picks`.
#'       \item **Soft position steering:** weight modulated by
#'         `(target_count[pos] - current_count[pos]) / remaining_picks`,
#'         clamped at `>= 0.05` -- nudges the roster toward the empirical
#'         per-position means without imposing a hard quota.
#'       \item **Same-team stack multiplier:** once a QB is on the
#'         roster, multiply weights on same-team WR/TE by `stack_alpha`.
#'         This is the lever calibrated to land `qb_stack_2plus` in
#'         the observed band.
#'     }
#'   \item Mask out already-drafted players and any position already at
#'     the slate's cap. Sample one player by the weighted distribution.
#' }
#'
#' Roster size is 18 (Season). Position caps come from the slate
#' manifest. The synthetic roster's `underdog_id` is the slate-CSV id,
#' which is the same key Layer A draws use -- so 3b-2 / 3b-3 / 3b-4
#' score generated rosters with no rejoin.
#'
#' @param slate_id Slate identifier, e.g. `"nfl_2026_season"`. Must have
#'   an entry in the slate manifest with `position_caps`.
#' @param picks Output of [load_scraped_drafts()] -- used to derive the
#'   empirical targets (per-position means, stack rate, per-slot ADP SD).
#'   When `NULL`, the caller must pass `targets` explicitly.
#' @param n_teams Number of synthetic teams to generate (default 10000).
#' @param seed Optional integer RNG seed.
#' @param player_pool Optional pre-loaded slate player pool. When `NULL`
#'   (default), produced via [load_slate_data()]. Only `QB`/`RB`/`WR`/`TE`
#'   rows with a non-NA `adp` are used.
#' @param targets Optional pre-computed targets list (skip empirical
#'   derivation). See [compute_field_targets()] for the shape.
#' @param stack_alpha Multiplier on same-team WR/TE weights after a QB
#'   is rostered (default `25.0`). Calibrated against the validation
#'   sample so the field's `qb_stack_2plus` lands in the empirical
#'   `0.91-0.93` band. The multiplier fires only until the first
#'   same-team stack partner is on the roster -- after that the binary
#'   `qb_stack_2plus` condition is met and the pressure drops to `1.0`
#'   so subsequent picks don't over-pull WR/TE counts.
#' @param pos_alpha Steering strength on position shortfall (default
#'   `2.0`). Higher values pull harder toward the per-position target
#'   means; too high makes the field look quota-driven.
#' @return A list with:
#'   \itemize{
#'     \item `rosters` -- long data.frame of generated picks
#'       (`entry_id`, `pick_overall`, `underdog_id`, `position`,
#'       `team_abbr`, `adp`), one row per pick.
#'     \item `targets` -- the empirical targets used to calibrate.
#'   }
#' @export
generate_field <- function(slate_id,
                           picks = NULL,
                           n_teams = 10000L,
                           seed = NULL,
                           player_pool = NULL,
                           targets = NULL,
                           stack_alpha = 25.0,
                           pos_alpha   = 2.0) {
  if (!is.null(seed)) set.seed(seed)
  n_teams <- as.integer(n_teams)
  if (is.na(n_teams) || n_teams < 1L) {
    cli::cli_abort("`n_teams` must be a positive integer.")
  }
  if (is.null(player_pool)) {
    player_pool <- load_slate_data(slate_id)
  }
  pool <- .prepare_player_pool(player_pool)

  manifest <- load_slate_manifest()
  slate_def <- manifest[[slate_id]]
  if (is.null(slate_def) || is.null(slate_def$position_caps)) {
    cli::cli_abort("Slate {.val {slate_id}} missing `position_caps` in manifest.")
  }
  pos_caps <- unlist(slate_def$position_caps)
  for (p in c("QB", "RB", "WR", "TE")) {
    if (is.null(pos_caps[[p]])) {
      cli::cli_abort("Slate {.val {slate_id}} position_caps missing {.val {p}}.")
    }
  }

  if (is.null(targets)) {
    if (is.null(picks)) {
      cli::cli_abort("Must pass either `targets` or `picks` (to derive targets).")
    }
    targets <- compute_field_targets(picks, slate_id = slate_id)
  }

  ROSTER_SIZE <- 18L
  DRAFT_SIZE  <- 12L
  pool_n  <- nrow(pool)
  positions <- pool$position
  teams     <- pool$team_abbr
  adps      <- pool$adp
  pos_idx_qb <- which(positions == "QB")
  pos_idx_pc <- which(positions %in% c("WR", "TE"))

  target_means <- targets$position_means
  sigma_by_slot <- targets$slot_adp_sd
  global_sigma  <- mean(sigma_by_slot[is.finite(sigma_by_slot)],
                        na.rm = TRUE)
  if (!is.finite(global_sigma) || global_sigma <= 0) global_sigma <- 24

  cap_vec <- as.integer(pos_caps[positions])

  pick_idx_mat <- matrix(NA_integer_, nrow = n_teams, ncol = ROSTER_SIZE)
  pick_slot_mat <- matrix(NA_integer_, nrow = n_teams, ncol = ROSTER_SIZE)
  snake_positions <- sample.int(DRAFT_SIZE, n_teams, replace = TRUE)

  for (t in seq_len(n_teams)) {
    snake_pos <- snake_positions[t]
    drafted <- logical(pool_n)
    pos_count <- c(QB = 0L, RB = 0L, WR = 0L, TE = 0L)
    qb_team <- NA_character_
    have_stack_partner <- FALSE

    for (r in seq_len(ROSTER_SIZE)) {
      slot <- if (r %% 2L == 1L) {
        (r - 1L) * DRAFT_SIZE + snake_pos
      } else {
        r * DRAFT_SIZE - snake_pos + 1L
      }
      sigma <- sigma_by_slot[as.character(slot)]
      if (is.na(sigma) || !is.finite(sigma) || sigma <= 0) sigma <- global_sigma

      adp_w <- stats::dnorm((adps - slot) / sigma)

      remaining <- ROSTER_SIZE - r + 1L
      shortfall <- target_means[positions] - pos_count[positions]
      pos_w <- pmax(0.05, 1 + pos_alpha * shortfall / remaining)

      # Stack multiplier fires only until the binary qb_stack_2plus
      # condition is satisfied (QB + >= 1 same-team WR/TE). After the
      # first stack partner lands, the pressure goes away so subsequent
      # picks don't get over-pulled into QB's team and distort the WR/TE
      # position means.
      stack_w <- rep(1, pool_n)
      if (!is.na(qb_team) && !have_stack_partner) {
        on_stack <- positions %in% c("WR", "TE") & teams == qb_team
        stack_w[on_stack] <- stack_alpha
      }

      cap_ok <- pos_count[positions] < cap_vec
      avail  <- !drafted & cap_ok
      w <- adp_w * pos_w * stack_w
      w[!avail] <- 0

      if (sum(w) <= 0) {
        w <- as.numeric(avail)
      }
      picked <- sample.int(pool_n, 1L, prob = w)
      pick_idx_mat[t, r] <- picked
      pick_slot_mat[t, r] <- slot
      drafted[picked] <- TRUE
      this_pos <- positions[picked]
      pos_count[this_pos] <- pos_count[this_pos] + 1L
      if (this_pos == "QB" && is.na(qb_team)) {
        qb_team <- teams[picked]
      } else if (!is.na(qb_team) && !have_stack_partner &&
                 this_pos %in% c("WR", "TE") &&
                 identical(teams[picked], qb_team)) {
        have_stack_partner <- TRUE
      }
    }
  }

  entry_ids <- sprintf("synth_%05d", seq_len(n_teams))
  rosters <- data.frame(
    entry_id     = rep(entry_ids, each = ROSTER_SIZE),
    pick_overall = as.integer(t(pick_slot_mat)),
    pick_number  = rep(seq_len(ROSTER_SIZE), times = n_teams),
    underdog_id  = pool$underdog_id[as.integer(t(pick_idx_mat))],
    position     = pool$position[as.integer(t(pick_idx_mat))],
    team_abbr    = pool$team_abbr[as.integer(t(pick_idx_mat))],
    adp          = pool$adp[as.integer(t(pick_idx_mat))],
    stringsAsFactors = FALSE
  )

  list(rosters = rosters, targets = targets)
}

#' Compute empirical targets from a scraped-drafts picks tibble
#'
#' Returns the three numbers the field model uses:
#' \itemize{
#'   \item `position_means` -- mean rostered count per position across
#'     teams (QB / RB / WR / TE), weighted by drafter count.
#'   \item `qb_stack_2plus_rate` -- empirical share of teams with
#'     `QB + >= 1` same-team pass-catcher.
#'   \item `slot_adp_sd` -- named numeric, the SD of
#'     `projection_adp_at_pick` per `pick_overall` slot. Sparse slots
#'     are flagged `NA`; the caller falls back to the global mean.
#' }
#'
#' @param picks Output of [load_scraped_drafts()].
#' @param slate_id Optional slate filter. If provided, only picks for
#'   that slate are used. Otherwise all rows.
#' @return Named list with `position_means`, `qb_stack_2plus_rate`,
#'   `slot_adp_sd`.
#' @export
compute_field_targets <- function(picks, slate_id = NULL) {
  if (!is.null(slate_id)) {
    # We don't have direct slate-id -> picks join here without the
    # slate manifest; rely on caller to pass an already-filtered
    # picks tibble (or filter by slate UUID via the manifest).
    manifest <- tryCatch(load_slate_manifest(), error = function(e) list())
    sd_uuid <- manifest[[slate_id]]$underdog_slate_id %||% NA_character_
    if (!is.na(sd_uuid)) {
      picks <- picks[picks$slate_id == sd_uuid, , drop = FALSE]
    }
  }
  picks <- picks[picks$position_name %in% c("QB", "RB", "WR", "TE"), ,
                 drop = FALSE]

  pc <- empirical_position_counts(picks)
  pos_means <- vapply(c("QB", "RB", "WR", "TE"), function(p) {
    rows <- pc[pc$position == p, , drop = FALSE]
    if (nrow(rows) == 0L) return(0)
    sum(rows$count * rows$n_teams_with_count) / sum(rows$n_teams_with_count)
  }, numeric(1))

  sp <- empirical_stack_patterns(picks)
  qb_rate <- mean(sp$has_qb_stack_2plus, na.rm = TRUE)

  sigma_per_slot <- tapply(
    picks$projection_adp_at_pick, picks$pick_overall,
    function(x) {
      x <- x[!is.na(x)]
      if (length(x) < 2L) NA_real_ else stats::sd(x)
    }
  )
  names(sigma_per_slot) <- as.character(names(sigma_per_slot))

  list(
    position_means      = pos_means,
    qb_stack_2plus_rate = qb_rate,
    slot_adp_sd         = sigma_per_slot
  )
}

# ---- internals -------------------------------------------------------------

#' @keywords internal
.prepare_player_pool <- function(pool) {
  required <- c("underdog_id", "position", "team_abbr", "adp")
  missing <- setdiff(required, colnames(pool))
  if (length(missing) > 0L) {
    cli::cli_abort("`player_pool` missing column(s): {missing}")
  }
  ok <- pool$position %in% c("QB", "RB", "WR", "TE") & !is.na(pool$adp) &
        !is.na(pool$team_abbr)
  pool <- pool[ok, , drop = FALSE]
  if (nrow(pool) == 0L) {
    cli::cli_abort("Player pool empty after filtering to QB/RB/WR/TE with adp + team.")
  }
  pool[order(pool$adp), , drop = FALSE]
}
