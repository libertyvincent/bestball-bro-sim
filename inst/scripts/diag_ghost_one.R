# Diagnostic: inspect ONE ghost-completed roster to explain the EV inflation.
suppressMessages(devtools::load_all(".", quiet = TRUE))

SLATE_ID    <- "nfl_2026_season"; SEED <- 1L; FIELD_TEAMS <- 2700L
POS_CAPS    <- c(QB = 4L, RB = 9L, WR = 10L, TE = 5L)
START_MIN   <- c(QB = 1L, RB = 2L, WR = 3L, TE = 1L)
ROSTER_SIZE <- 18L

ckpt <- readRDS(file.path("build", "ev_smoke_ckpt.rds"))
layerA <- ckpt$layerA; pool_draws <- ckpt$pool_draws; field_scores <- ckpt$field_scores

feed <- blend_slate(SLATE_ID,
  sources_manifest_path = file.path("inst", "data", "sources", "_manifest.yaml"),
  slates_manifest_path  = file.path("inst", "data", "slates", "_manifest.yaml"),
  write_json = FALSE)
positions   <- positions_from_feed(feed)
lineup_spec <- load_slate_lineup_spec(SLATE_ID)
pcfg        <- load_tournament("puppy2")
curves_p2   <- build_tournament_curves(pcfg, field_scores, n_grid = 256L, seed = SEED)

picks   <- load_scraped_drafts(); pool <- load_slate_data(SLATE_ID)
targets <- compute_field_targets(picks, slate_id = SLATE_ID)
field   <- generate_field(SLATE_ID, picks = picks, player_pool = pool,
                          targets = targets, n_teams = FIELD_TEAMS, seed = SEED)
field_rosters <- split(field$rosters$underdog_id, field$rosters$entry_id)
field_order <- split(
  field$rosters[order(field$rosters$entry_id, field$rosters$pick_number),
                c("underdog_id", "pick_overall", "pick_number")],
  field$rosters$entry_id[order(field$rosters$entry_id, field$rosters$pick_number)])
covered_ids <- pool_draws$player_ids

adp_vec <- vapply(feed$players, function(p) as.numeric(p$adp %||% NA_real_), numeric(1))
names(adp_vec) <- names(feed$players)
adp_vec <- adp_vec[!is.na(adp_vec) & names(adp_vec) %in% covered_ids]
adp_board <- names(sort(adp_vec)); board_pos <- positions[adp_board]
cat(sprintf("adp_board size (covered, with ADP): %d\n", length(adp_board)))

ghost_complete <- function(partial_ids, future_slots) {
  drafted <- stats::setNames(logical(length(adp_board)), adp_board)
  drafted[partial_ids[partial_ids %in% adp_board]] <- TRUE
  pos_count <- c(QB = 0L, RB = 0L, WR = 0L, TE = 0L)
  for (id in partial_ids) { p <- positions[[id]]
    if (!is.na(p) && p %in% names(pos_count)) pos_count[p] <- pos_count[p] + 1L }
  roster <- partial_ids
  consume_field_to <- function(tt) { if (sum(drafted) >= tt) return(invisible())
    for (id in adp_board) { if (sum(drafted) >= tt) break; if (!drafted[[id]]) drafted[[id]] <<- TRUE }
    invisible() }
  best_avail <- function(allowed) { for (id in adp_board) { if (drafted[[id]]) next
      p <- board_pos[[id]]; if (is.na(p) || !(p %in% allowed)) next
      if (pos_count[[p]] >= POS_CAPS[[p]]) next; return(id) }; NA_character_ }
  n_future <- length(future_slots)
  for (i in seq_len(n_future)) {
    consume_field_to(future_slots[i] - 1L)
    picks_left <- n_future - i + 1L
    unmet <- pmax(0L, START_MIN - pos_count[names(START_MIN)])
    if (picks_left <= sum(unmet)) { needed <- names(unmet)[unmet > 0L]
      pick <- best_avail(needed); if (is.na(pick)) pick <- best_avail(names(pos_count))
    } else pick <- best_avail(names(pos_count))
    if (is.na(pick)) pick <- best_avail(names(pos_count)); if (is.na(pick)) break
    drafted[[pick]] <- TRUE; pp <- board_pos[[pick]]
    pos_count[[pp]] <- pos_count[[pp]] + 1L; roster <- c(roster, pick)
  }
  roster
}

e <- "synth_00950"; cut <- 6L
ord_df <- field_order[[e]]
partial <- utils::head(ord_df$underdog_id, cut)
future_slots <- sort(ord_df$pick_overall[ord_df$pick_number > cut])
cat(sprintf("\nentry %s, cut %d\n", e, cut))
cat("future_slots:", paste(future_slots, collapse = " "), "\n")
roster <- ghost_complete(partial, future_slots)

cat(sprintf("\nroster length: %d   distinct: %d   duplicated: %s\n",
            length(roster), length(unique(roster)),
            paste(roster[duplicated(roster)], collapse = ",")))
cat("positions:", paste(names(table(positions[roster])),
                        table(positions[roster]), sep = ":", collapse = "  "), "\n")
cat("ADP ranks on board:", paste(match(roster, adp_board), collapse = " "), "\n")

# Compare this roster's R1 vs the FULL field roster's R1, vs the field pool.
gate_paths <- pool_draws
gate_paths$tensor <- pool_draws$tensor[, , seq_len(1000L), drop = FALSE]
gate_paths$n_paths <- 1000L
rs_ghost <- roster_round_scores(roster, gate_paths, curves_p2$stage_weeks, lineup_spec)
field_cov <- field_rosters[[e]][field_rosters[[e]] %in% covered_ids]
rs_field <- roster_round_scores(field_cov, gate_paths, curves_p2$stage_weeks, lineup_spec)

# field pool (q_cum_pool) range for g1 context
fp <- build_field_payouts(field_scores, pcfg, seed = SEED)
qp <- fp$pools$q_cum_pool
cat(sprintf("\nq_cum_pool (field R1 metric): min %.0f  median %.0f  max %.0f\n",
            min(qp), stats::median(qp), max(qp)))
cat(sprintf("ghost roster  R1: mean %.0f  (vs field pool pctile %.3f)\n",
            mean(rs_ghost$R1), mean(rs_ghost$R1) > stats::median(qp)))
cat(sprintf("full field roster R1: mean %.0f\n", mean(rs_field$R1)))
cat(sprintf("ghost R1 mean %.0f / field-roster R1 mean %.0f\n",
            mean(rs_ghost$R1), mean(rs_field$R1)))
cat(sprintf("ghost R2/R3/R4 means: %.1f / %.1f / %.1f\n",
            mean(rs_ghost$R2), mean(rs_ghost$R3), mean(rs_ghost$R4)))

cev <- evaluate_roster_curve_ev(roster, gate_paths, curves_p2, lineup_spec)
cat(sprintf("\nghost curve EV $%.3f   mean g1 %.3f  g2 %.3f  g3 %.3f\n",
            cev$ev, mean(cev$per_path$g1), mean(cev$per_path$g2), mean(cev$per_path$g3)))
cevf <- evaluate_roster_curve_ev(field_cov, gate_paths, curves_p2, lineup_spec)
cat(sprintf("field curve EV $%.3f   mean g1 %.3f\n", cevf$ev, mean(cevf$per_path$g1)))
