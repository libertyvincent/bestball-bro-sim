# Ghost-completion validation addendum (no PR).
#
# Question: when a partial roster at an early/mid pick point is ghost-completed
# with ADP-best-available picks, does the resulting roster's curve-based EV land
# in the chalky-accurate band (~$0.75 pod-noise floor, like the gate's chalky
# MAE $0.88 / ~18%) rather than the unique over-priced band ($2.14 / 68%)?
#
# That is the *real* ghost-completion use mode: the extension fills the rest of
# an in-progress roster with ADP-best-available before it can show an EV
# ranking. ADP-best-available completions are chalky by construction, so the
# hypothesis is that they sit in the accurate band even when the partial seed
# came from an otherwise-unique field roster.
#
# Procedure
#   1. Reuse the smoke checkpoint (layerA draws, pool_draws tensor, field_scores)
#      so no heavy sim is repeated. Rebuild the (cheap) Puppy 2 curves + field
#      payout cache from field_scores. Regenerate the field rosters (same seed)
#      to source partial seeds and pod-mates.
#   2. Pick a spread of source rosters (by full-roster ownership) whose first 12
#      picks are all covered by the draws tensor.
#   3. For each source roster x cut in {6, 9, 12}: keep the first `cut` real
#      picks (draft order), then ghost-complete to 18 with ADP-best-available,
#      respecting position caps and starting-lineup minimums, where the field
#      between the drafter's own slots is consumed strictly by ADP.
#   4. For each completed roster: curve EV (evaluate_roster_curve_ev on the
#      shipped paths) vs full-sim EV (compute_team_ev, averaged over pods).
#   5. Report MAE / mean signed gap, per cut and overall, against the bands.
#
# Run from the package root:
#   "<Rscript>" inst/scripts/validate_ghost_completion.R > build/validate_ghost.log 2>&1

suppressMessages(devtools::load_all(".", quiet = TRUE))

# ---- knobs --------------------------------------------------------------------
SLATE_ID     <- "nfl_2026_season"
SEED         <- 1L
FIELD_TEAMS  <- 2700L     # must match the checkpoint's field generation
CUTS         <- c(6L, 9L, 12L)
N_SRC        <- 6L        # source rosters, spread across full-roster ownership
N_PODS       <- 3L        # pods per completed roster (averages out pod luck)
TEAM_EV_SIMS <- 1500L     # match the smoke gate so the noise floor is comparable
GATE_PATHS_N <- 1000L     # paths used for the curve EV (match the gate)
ROSTER_SIZE  <- 18L

POS_CAPS  <- c(QB = 4L, RB = 9L, WR = 10L, TE = 5L)   # season manifest caps
START_MIN <- c(QB = 1L, RB = 2L, WR = 3L, TE = 1L)    # min to field a lineup

t_start <- Sys.time()
stamp <- function(label) cat(sprintf("[%6.1f min] %s\n",
  as.numeric(difftime(Sys.time(), t_start, units = "mins")), label))

# ---- 0. reuse the smoke checkpoint --------------------------------------------
CKPT <- file.path("build", "ev_smoke_ckpt.rds")
if (!file.exists(CKPT)) {
  stop("Checkpoint ", CKPT, " not found -- run inst/scripts/smoke_ev_blocks.R first.")
}
ckpt <- readRDS(CKPT)
layerA       <- ckpt$layerA
pool_draws   <- ckpt$pool_draws
field_scores <- ckpt$field_scores
stopifnot(!is.null(layerA), !is.null(pool_draws), !is.null(field_scores))
stamp("checkpoint loaded (layerA, pool_draws, field_scores)")

# ---- 1. feed / specs / curves / field cache -----------------------------------
feed <- blend_slate(
  slate_id              = SLATE_ID,
  sources_manifest_path = file.path("inst", "data", "sources", "_manifest.yaml"),
  slates_manifest_path  = file.path("inst", "data", "slates", "_manifest.yaml"),
  write_json            = FALSE)
positions   <- positions_from_feed(feed)
schedule    <- schedule_from_feed(feed)
lineup_spec <- load_slate_lineup_spec(SLATE_ID)

pcfg           <- load_tournament("puppy2")
curves_p2      <- build_tournament_curves(pcfg, field_scores, n_grid = 256L, seed = SEED)
field_cache_p2 <- build_field_payouts(field_scores, pcfg, seed = SEED)
stamp("curves + field payout cache rebuilt (puppy2)")

# Regenerate the field (same seed => identical rosters as the checkpoint run).
picks   <- load_scraped_drafts()
pool    <- load_slate_data(SLATE_ID)
targets <- compute_field_targets(picks, slate_id = SLATE_ID)
field   <- generate_field(SLATE_ID, picks = picks, player_pool = pool,
                          targets = targets, n_teams = FIELD_TEAMS, seed = SEED)
field_rosters <- split(field$rosters$underdog_id, field$rosters$entry_id)
# draft order per entry (pick_number 1..18) and its overall snake slots
field_order <- split(
  field$rosters[order(field$rosters$entry_id, field$rosters$pick_number),
                c("underdog_id", "pick_overall", "pick_number")],
  field$rosters$entry_id[order(field$rosters$entry_id, field$rosters$pick_number)])
stamp("field regenerated")

covered_ids <- pool_draws$player_ids
ownership   <- table(field$rosters$underdog_id) / length(field_rosters)
own_of      <- function(ids) mean(ownership[ids], na.rm = TRUE)

# ---- 2. the ADP board + ghost-completion --------------------------------------
# Board = covered, ADP-ranked players (the field drafts strictly by this order
# between the drafter's own slots). Restricting to covered players keeps every
# completed roster scorable by the draws tensor.
adp_vec <- vapply(feed$players, function(p) as.numeric(p$adp %||% NA_real_), numeric(1))
names(adp_vec) <- names(feed$players)
adp_vec <- adp_vec[!is.na(adp_vec) & names(adp_vec) %in% covered_ids]
adp_board <- names(sort(adp_vec))                      # underdog_ids, ADP ascending
board_pos <- positions[adp_board]

#' Ghost-complete a partial roster with ADP-best-available.
#'
#' @param partial_ids the first `cut` real picks (draft order).
#' @param future_slots integer overall snake slots for the drafter's remaining
#'   picks (ascending). The field consumes the board by ADP up to each slot - 1,
#'   then the drafter takes the best available (cap- and need-aware) at the slot.
ghost_complete <- function(partial_ids, future_slots) {
  drafted <- stats::setNames(logical(length(adp_board)), adp_board)
  on_board <- partial_ids[partial_ids %in% adp_board]
  drafted[on_board] <- TRUE
  pos_count <- c(QB = 0L, RB = 0L, WR = 0L, TE = 0L)
  for (id in partial_ids) {
    p <- positions[[id]]
    if (!is.na(p) && p %in% names(pos_count)) pos_count[p] <- pos_count[p] + 1L
  }
  roster <- partial_ids

  consume_field_to <- function(target_total) {
    # Mark lowest-ADP undrafted players as taken until the board has lost
    # `target_total` players total (drafter picks + simulated field picks).
    # `<<-` so the mutation lands on ghost_complete()'s `drafted`, not a
    # copy-on-modify local (the whole field-consumption step depends on it).
    if (sum(drafted) >= target_total) return(invisible())
    for (id in adp_board) {
      if (sum(drafted) >= target_total) break
      if (!drafted[[id]]) drafted[[id]] <<- TRUE
    }
    invisible()
  }
  best_avail <- function(allowed) {
    for (id in adp_board) {
      if (drafted[[id]]) next
      p <- board_pos[[id]]
      if (is.na(p) || !(p %in% allowed)) next
      if (pos_count[[p]] >= POS_CAPS[[p]]) next
      return(id)
    }
    NA_character_
  }

  n_future <- length(future_slots)
  for (i in seq_len(n_future)) {
    slot <- future_slots[i]
    consume_field_to(slot - 1L)                  # field fills everything before us
    picks_left <- n_future - i + 1L              # including this pick
    unmet <- pmax(0L, START_MIN - pos_count[names(START_MIN)])
    if (picks_left <= sum(unmet)) {
      needed <- names(unmet)[unmet > 0L]         # must secure a startable lineup
      pick <- best_avail(needed)
      if (is.na(pick)) pick <- best_avail(names(pos_count))
    } else {
      pick <- best_avail(names(pos_count))
    }
    if (is.na(pick)) pick <- best_avail(names(pos_count))
    if (is.na(pick)) break                       # board exhausted (shouldn't happen)
    drafted[[pick]] <- TRUE
    pp <- board_pos[[pick]]
    pos_count[[pp]] <- pos_count[[pp]] + 1L
    roster <- c(roster, pick)
  }
  roster
}

# ---- 3. select source rosters (covered first-12, spread by ownership) ---------
first12_covered <- vapply(field_order, function(df)
  all(utils::head(df$underdog_id, 12L) %in% covered_ids), logical(1))
eids_cov   <- names(field_order)[first12_covered]
own_full   <- vapply(eids_cov, function(e) own_of(field_rosters[[e]]), numeric(1))
ord        <- order(own_full)
sel_idx    <- unique(round(seq(1, length(ord), length.out = N_SRC)))
src_eids   <- eids_cov[ord[sel_idx]]
cat(sprintf("\nsource rosters: %d covered (first-12), %d selected across ownership %.3f..%.3f\n",
            length(eids_cov), length(src_eids), min(own_full), max(own_full)))

# ---- 4. evaluate: curve EV vs full-sim EV for each (roster, cut) ---------------
rows <- list()
for (e in src_eids) {
  ord_df <- field_order[[e]]
  for (cut in CUTS) {
    partial      <- utils::head(ord_df$underdog_id, cut)
    future_slots <- sort(ord_df$pick_overall[ord_df$pick_number > cut])
    roster <- ghost_complete(partial, future_slots)
    if (length(roster) != ROSTER_SIZE || anyNA(roster) ||
        !all(roster %in% covered_ids)) {
      cat(sprintf("  skip %s cut %d (incomplete/uncovered roster: %d players)\n",
                  e, cut, length(roster)))
      next
    }

    # Curve EV on the shipped paths.
    gate_paths <- pool_draws
    gate_paths$tensor  <- pool_draws$tensor[, , seq_len(GATE_PATHS_N), drop = FALSE]
    gate_paths$n_paths <- GATE_PATHS_N
    cev <- evaluate_roster_curve_ev(roster, gate_paths, curves_p2, lineup_spec)

    # Full-sim EV, averaged over pod draws (pod luck != factorization error).
    sim_evs <- numeric(N_PODS); sim_qs <- numeric(N_PODS)
    for (p in seq_len(N_PODS)) {
      set.seed(SEED + cut * 1000L + p)
      podmates <- sample(setdiff(names(field_rosters), e), 11L)
      pod <- c(field_rosters[podmates], stats::setNames(list(roster), e))
      sev <- compute_team_ev(
        pod_rosters = pod, team_entry_id = e, positions = positions,
        layerA_draws = layerA, schedule = schedule, lineup_spec = lineup_spec,
        tournament_cfg = pcfg, field_cache = field_cache_p2,
        n_sims = TEAM_EV_SIMS, seed = SEED + cut * 1000L + p)
      sim_evs[p] <- sev$team_ev$total_ev
      sim_qs[p]  <- sev$advance_probs$qualifier
    }
    rows[[length(rows) + 1L]] <- data.frame(
      entry_id = e, cut = cut,
      seed_chalk = own_of(partial), roster_chalk = own_of(roster),
      curve_ev = cev$ev, sim_ev = mean(sim_evs), sim_ev_sd = stats::sd(sim_evs),
      curve_q_adv = mean(cev$per_path$g1), sim_q_adv = mean(sim_qs),
      stringsAsFactors = FALSE)
    stamp(sprintf("  %s cut %2d: seed-chalk %.3f -> roster-chalk %.3f | curve $%.3f vs sim $%.3f (sd %.3f)",
                  e, cut, own_of(partial), own_of(roster), cev$ev, mean(sim_evs),
                  stats::sd(sim_evs)))
  }
}

res <- do.call(rbind, rows)
res$signed  <- res$curve_ev - res$sim_ev
res$abs_err <- abs(res$signed)
res$rel_err <- res$abs_err / pmax(res$sim_ev, 0.01)

# ---- 5. report -----------------------------------------------------------------
cat("\n=== GHOST-COMPLETION VALIDATION (Puppy 2, curve EV vs full sim EV) ===\n")
print(res[, c("entry_id", "cut", "seed_chalk", "roster_chalk", "curve_ev",
              "sim_ev", "sim_ev_sd", "signed", "rel_err")],
      row.names = FALSE, digits = 3)

summ <- function(sub, label) cat(sprintf(
  "  %-12s n=%2d  mean curve $%.3f  mean sim $%.3f  mean signed $%+.3f  MAE $%.3f (%.1f%%)  | q_adv curve %.3f vs sim %.3f\n",
  label, nrow(sub), mean(sub$curve_ev), mean(sub$sim_ev), mean(sub$signed),
  mean(sub$abs_err), 100 * mean(sub$abs_err) / mean(sub$sim_ev),
  mean(sub$curve_q_adv), mean(sub$sim_q_adv)))

cat("\nby cut:\n")
for (cut in CUTS) summ(res[res$cut == cut, ], sprintf("cut %d", cut))
cat("overall:\n"); summ(res, "ALL")
cat(sprintf("\n  pod-draw noise floor (mean within-roster sim sd): $%.3f\n",
            mean(res$sim_ev_sd)))
cat("  reference bands -- chalky-accurate: MAE ~$0.88 (~18%), pod-noise floor ~$0.75;\n")
cat("                     unique over-priced: MAE ~$2.14 (~68%), mean signed ~+$1.87\n")

saveRDS(res, file.path("build", "validate_ghost_results.rds"))
stamp("done")
