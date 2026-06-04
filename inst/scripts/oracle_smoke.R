# Fast smoke of the oracle core (small N/F) -- catches math/recycling bugs
# before the full self-validation run.
suppressMessages(devtools::load_all(".", quiet = TRUE))
SLATE_ID <- "nfl_2026_season"; SEED <- 1L; FIELD_TEAMS <- 2700L
N_PATHS <- 300L; MAX_FIELD <- 250L
POS_CAPS <- c(QB=4L,RB=9L,WR=10L,TE=5L); START_MIN <- c(QB=1L,RB=2L,WR=3L,TE=1L)

ckpt <- readRDS(file.path("build","ev_smoke_ckpt.rds"))
layerA <- ckpt$layerA; pool_draws <- ckpt$pool_draws; field_scores <- ckpt$field_scores
feed <- blend_slate(SLATE_ID,
  sources_manifest_path=file.path("inst","data","sources","_manifest.yaml"),
  slates_manifest_path =file.path("inst","data","slates","_manifest.yaml"), write_json=FALSE)
positions <- positions_from_feed(feed); schedule <- schedule_from_feed(feed)
lineup_spec <- load_slate_lineup_spec(SLATE_ID); pcfg <- load_tournament("puppy2")
curves_p2 <- build_tournament_curves(pcfg, field_scores, n_grid=256L, seed=SEED)
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
sw <- curves_p2$stage_weeks; cstruct <- oracle_cfg_struct(pcfg)

ev_draws <- pool_draws
ev_draws$tensor <- pool_draws$tensor[, , seq_len(N_PATHS), drop=FALSE]; ev_draws$n_paths <- N_PATHS

cat("scoring small field...\n")
scored <- oracle_score_field(field_rosters, ev_draws, sw, lineup_spec,
                             min_covered=15L, max_field=MAX_FIELD, seed=SEED)
ofield <- oracle_build_field(scored, cstruct)
cat(sprintf("F=%d N=%d\n", ofield$F, ofield$N))
cat(sprintf("[A] field reach QF %.4f (=%.4f?) SF %.5f (=%.5f?) FIN %.6f (=%.6f?)\n",
  mean(ofield$W[[2]]), cstruct$seats[2]/cstruct$seats[1],
  mean(ofield$W[[3]]), cstruct$seats[3]/cstruct$seats[1],
  mean(ofield$W[[4]]), cstruct$seats[4]/cstruct$seats[1]))

# one ghost roster
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
e <- "synth_00950"; k <- 9L; ord_df <- field_order[[e]]
partial <- utils::head(ord_df$underdog_id, k)
next_slot <- ord_df$pick_overall[ord_df$pick_number==k+1L]
future_after <- sort(ord_df$pick_overall[ord_df$pick_number>k+1L])
r <- ghost_complete(partial, c(next_slot, future_after))
rs <- roster_round_scores(intersect(r,covered_ids), ev_draws, sw, lineup_spec)
v1 <- evaluate_roster_curve_ev(intersect(r,covered_ids), ev_draws, curves_p2, lineup_spec)$ev
for (fm in c("binom","rank")) {
  oc <- oracle_roster_ev(rs, ofield, mode="comove", final_mode=fm)
  os <- oracle_roster_ev(rs, ofield, mode="static", final_mode=fm)
  cat(sprintf("[B/%s] v1 $%.3f | static $%.3f | comove $%.3f | reachF c=%.5f s=%.5f | Efin c=$%.1f\n",
              fm, v1, os$ev, oc$ev, oc$reach["final"], os$reach["final"], oc$parts["Efin"]))
}
cat("smoke OK\n")
