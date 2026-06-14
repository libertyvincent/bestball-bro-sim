# Phase 1 / PART 1: quantify the full-sim noise floor on the cell-1 marginals.
#
# The gate treated compute_team_ev as "truth", but Puppy 2's EV is dominated by
# the rare, convex final ladder (reach-final ~0.3%, payouts to $100k). The curve
# integrates that analytically per path (low variance); compute_team_ev must
# randomly advance through single-week top-1 pods (high variance). This script
# measures how noisy the full-sim marginals actually are at the gate's 4000 sims
# vs a higher budget, so we know whether cell-1's $3.34 regret is signal or MC
# noise BEFORE attributing it to a curve defect.

suppressMessages(devtools::load_all(".", quiet = TRUE))
SLATE_ID <- "nfl_2026_season"; SEED <- 1L; FIELD_TEAMS <- 2700L
POS_CAPS <- c(QB=4L,RB=9L,WR=10L,TE=5L); START_MIN <- c(QB=1L,RB=2L,WR=3L,TE=1L)
N_SEEDS  <- 6L
SIM_LEVELS <- c(4000L, 16000L)

ckpt <- readRDS(file.path("build","ev_smoke_ckpt.rds"))
layerA <- ckpt$layerA; pool_draws <- ckpt$pool_draws; field_scores <- ckpt$field_scores
feed <- blend_slate(SLATE_ID,
  sources_manifest_path=file.path("inst","data","sources","_manifest.yaml"),
  slates_manifest_path =file.path("inst","data","slates","_manifest.yaml"), write_json=FALSE)
positions <- positions_from_feed(feed); schedule <- schedule_from_feed(feed)
lineup_spec <- load_slate_lineup_spec(SLATE_ID); pcfg <- load_tournament("puppy2")
curves_p2 <- build_tournament_curves(pcfg, field_scores, n_grid=256L, seed=SEED)
field_cache_p2 <- build_field_payouts(field_scores, pcfg, seed=SEED)
picks <- load_scraped_drafts(); pool <- load_slate_data(SLATE_ID)
targets <- compute_field_targets(picks, slate_id=SLATE_ID)
field <- generate_field(SLATE_ID, picks=picks, player_pool=pool, targets=targets,
                        n_teams=FIELD_TEAMS, seed=SEED)
field_rosters <- split(field$rosters$underdog_id, field$rosters$entry_id)
field_order <- split(
  field$rosters[order(field$rosters$entry_id, field$rosters$pick_number),
                c("underdog_id","pick_overall","pick_number")],
  field$rosters$entry_id[order(field$rosters$entry_id, field$rosters$pick_number)])
covered_ids <- pool_draws$player_ids
adp_vec <- vapply(feed$players, function(p) as.numeric(p$adp %||% NA_real_), numeric(1))
names(adp_vec) <- names(feed$players)
adp_vec <- adp_vec[!is.na(adp_vec) & names(adp_vec) %in% covered_ids]
adp_board <- names(sort(adp_vec)); board_pos <- positions[adp_board]

ghost_complete <- function(partial_ids, future_slots) {
  drafted <- stats::setNames(logical(length(adp_board)), adp_board)
  drafted[partial_ids[partial_ids %in% adp_board]] <- TRUE
  pos_count <- c(QB=0L,RB=0L,WR=0L,TE=0L)
  for (id in partial_ids) { p<-positions[[id]]; if(!is.na(p)&&p%in%names(pos_count)) pos_count[p]<-pos_count[p]+1L }
  roster <- partial_ids
  consume_field_to <- function(tt){ if(sum(drafted)>=tt) return(invisible())
    for(id in adp_board){ if(sum(drafted)>=tt) break; if(!drafted[[id]]) drafted[[id]]<<-TRUE }; invisible() }
  best_avail <- function(allowed){ for(id in adp_board){ if(drafted[[id]]) next
    p<-board_pos[[id]]; if(is.na(p)||!(p%in%allowed)) next
    if(pos_count[[p]]>=POS_CAPS[[p]]) next; return(id) }; NA_character_ }
  nf <- length(future_slots)
  for(i in seq_len(nf)){ consume_field_to(future_slots[i]-1L)
    pl <- nf-i+1L; unmet <- pmax(0L, START_MIN-pos_count[names(START_MIN)])
    if(pl<=sum(unmet)){ needed<-names(unmet)[unmet>0L]; pick<-best_avail(needed)
      if(is.na(pick)) pick<-best_avail(names(pos_count)) } else pick<-best_avail(names(pos_count))
    if(is.na(pick)) pick<-best_avail(names(pos_count)); if(is.na(pick)) break
    drafted[[pick]]<-TRUE; pp<-board_pos[[pick]]; pos_count[[pp]]<-pos_count[[pp]]+1L
    roster<-c(roster,pick) }
  roster
}

# ---- reconstruct cell-1 rosters (synth_00950, k=9) ----------------------------
e <- "synth_00950"; k <- 9L
rb_id <- "eb6be1fa-c875-4300-87e2-7809671a5a00"   # curve #1 (variance RB, adp#160)
te_id <- "9077a1e5-ae83-43f2-ad88-8e44df2d6f4d"   # full-sim #1 (scarce TE, adp#126)
ord_df <- field_order[[e]]
partial <- utils::head(ord_df$underdog_id, k)
next_slot <- ord_df$pick_overall[ord_df$pick_number == k+1L]
future_after <- sort(ord_df$pick_overall[ord_df$pick_number > k+1L])
base_roster <- ghost_complete(partial, c(next_slot, future_after))
rb_roster   <- ghost_complete(c(partial, rb_id), future_after)
te_roster   <- ghost_complete(c(partial, te_id), future_after)
rosters <- list(base=base_roster, RB=rb_roster, TE=te_roster)
cat(sprintf("rosters built: base %d / RB %d / TE %d players\n",
            length(base_roster), length(rb_roster), length(te_roster)))

# Fixed pod (same as a gate cell would use), shared across all rosters+seeds.
set.seed(SEED + 1L*17L)
pod_ids <- sample(setdiff(names(field_rosters), e), 11L)

full_ev <- function(roster, n_sims, seed) {
  pod <- c(field_rosters[pod_ids], stats::setNames(list(roster), e))
  compute_team_ev(pod_rosters=pod, team_entry_id=e, positions=positions,
    layerA_draws=layerA, schedule=schedule, lineup_spec=lineup_spec,
    tournament_cfg=pcfg, field_cache=field_cache_p2, n_sims=n_sims, seed=seed)$team_ev$total_ev
}

se <- function(x) stats::sd(x)/sqrt(length(x))
cat(sprintf("\ncurve marginals (gate): RB +$%.3f  TE +$%.3f  (TE-RB = $%.3f)\n", 2.459, 0.662, 0.662-2.459))
cat(sprintf("full-sim marginals (gate, 1 seed @4000): RB +$%.3f  TE +$%.3f  -> regret driver TE-RB = $%.3f\n",
            1.185, 4.522, 4.522-1.185))

for (ns in SIM_LEVELS) {
  evs <- lapply(rosters, function(r) vapply(seq_len(N_SEEDS),
            function(s) full_ev(r, ns, SEED + 1000L*s), numeric(1)))
  cat(sprintf("\n=== n_sims = %d (%d independent seeds) ===\n", ns, N_SEEDS))
  for (nm in names(evs)) cat(sprintf("  %-4s EV: mean $%.3f  sd $%.3f  SE $%.3f   [%s]\n",
      nm, mean(evs[[nm]]), stats::sd(evs[[nm]]), se(evs[[nm]]),
      paste(sprintf("%.2f", evs[[nm]]), collapse=" ")))
  m_rb <- mean(evs$RB) - mean(evs$base); m_te <- mean(evs$TE) - mean(evs$base)
  se_rb <- sqrt(se(evs$RB)^2 + se(evs$base)^2); se_te <- sqrt(se(evs$TE)^2 + se(evs$base)^2)
  cat(sprintf("  MARGINAL RB +$%.3f +/- $%.3f   TE +$%.3f +/- $%.3f\n", m_rb, se_rb, m_te, se_te))
  cat(sprintf("  TE-minus-RB marginal gap: $%.3f +/- $%.3f   (gate saw $%.3f)\n",
              m_te - m_rb, sqrt(se_rb^2+se_te^2), 4.522-1.185))
}
cat("\n[interpretation] if the TE-minus-RB gap is within a few SE of 0, the gate's\n")
cat("  $3.34 cell-1 regret is largely full-sim MC noise, not a curve mis-ranking.\n")
