# Phase 1 / PART 3: co-movement test (low variance, no rare-event MC noise).
#
# Isolation: every roster's per-path round scores R1..R4 come from the SAME
# Artifact A tensor (gate paths). We compare two combines that differ in ONE
# thing only -- the reference the advancement percentile is taken against:
#   * STATIC  : percentile of my round score vs the POOLED field distribution
#               (field scores pooled over all paths) -> this is what the v1
#               curve does (field treated as independent of my world).
#   * COMOVE  : percentile of my round score vs the field distribution IN THAT
#               PATH (field scored on the same tensor paths) -> in a boom path
#               the whole field booms, so my high absolute score buys a lower
#               rank/advance. This is the field co-movement the v1 curve omits.
# QF/SF payouts are flat ($5/$25) and h_final is held at the SAME static curve
# for both, so the static-vs-comove EV delta is PURELY advancement co-movement.
#
# Question: does COMOVE push the scarce TE up and the variance RB down (the
# cell-1 direction)? If yes, advancement co-movement is (part of) the root.

suppressMessages(devtools::load_all(".", quiet = TRUE))
SLATE_ID <- "nfl_2026_season"; SEED <- 1L; FIELD_TEAMS <- 2700L
POS_CAPS <- c(QB=4L,RB=9L,WR=10L,TE=5L); START_MIN <- c(QB=1L,RB=2L,WR=3L,TE=1L)
GATE_PATHS_N <- 1000L
N_FIELD      <- 800L     # field rosters scored on the tensor for the per-path field dist

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

gate_paths <- pool_draws
gate_paths$tensor <- pool_draws$tensor[, , seq_len(GATE_PATHS_N), drop=FALSE]
gate_paths$n_paths <- GATE_PATHS_N
sw <- curves_p2$stage_weeks

# round scores per path for any roster (covered subset)
rscore <- function(roster) {
  rc <- intersect(roster, covered_ids)
  roster_round_scores(rc, gate_paths, sw, lineup_spec)   # data.frame R1..R4, n_paths rows
}

# ---- score a field sample on the SAME tensor paths (per-path field dist) ------
cov_count <- vapply(field_rosters, function(r) sum(r %in% covered_ids), integer(1))
elig <- names(field_rosters)[cov_count >= 15L]
set.seed(SEED)
fsamp <- sample(elig, min(N_FIELD, length(elig)))
cat(sprintf("scoring %d field rosters on %d tensor paths...\n", length(fsamp), GATE_PATHS_N))
fR1 <- matrix(0, GATE_PATHS_N, length(fsamp)); fR2 <- fR1; fR3 <- fR1
for (i in seq_along(fsamp)) {
  rs <- rscore(field_rosters[[fsamp[i]]])
  fR1[, i] <- rs$R1; fR2[, i] <- rs$R2; fR3[, i] <- rs$R3
}
# pooled (static) ECDFs
F1 <- stats::ecdf(as.numeric(fR1)); F2 <- stats::ecdf(as.numeric(fR2)); F3 <- stats::ecdf(as.numeric(fR3))
cat("field scored.\n")

# advancement formulas (percentile p = P(opp <= me)):
g1f <- function(p) p^11 + 11*(1-p)*p^10     # top 2 of 12
g2f <- function(p) p^9                       # top 1 of 10
g3f <- function(p) p^4                        # top 1 of 5
# per-path comove percentile of x vs field column-dist in that path
pp_comove <- function(x, FM) rowMeans(FM <= x)   # x length n_paths, FM [n_paths x F]

evaluate_two_ways <- function(roster) {
  rs <- rscore(roster)
  hf <- stats::approx(curves_p2$curves$h_final$x, curves_p2$curves$h_final$y,
                      xout = rs$R4, rule = 2, ties = "ordered")$y   # static finalist ladder, held fixed
  # STATIC percentiles (pooled field == what the curve sees)
  p1s <- F1(rs$R1); p2s <- F2(rs$R2); p3s <- F3(rs$R3)
  g1s <- g1f(p1s); g2s <- g2f(p2s); g3s <- g3f(p3s)
  # COMOVE percentiles (per-path field)
  p1c <- pp_comove(rs$R1, fR1); p2c <- pp_comove(rs$R2, fR2); p3c <- pp_comove(rs$R3, fR3)
  g1c <- g1f(p1c); g2c <- g2f(p2c); g3c <- g3f(p3c)
  combine <- function(g1,g2,g3) g1*((1-g2)*5 + g2*(1-g3)*25 + g2*g3*hf)
  list(
    static = list(ev=mean(combine(g1s,g2s,g3s)), g1=mean(g1s), g2=mean(g2s), g3=mean(g3s),
                  reach_final=mean(g1s*g2s*g3s)),
    comove = list(ev=mean(combine(g1c,g2c,g3c)), g1=mean(g1c), g2=mean(g2c), g3=mean(g3c),
                  reach_final=mean(g1c*g2c*g3c)),
    curve_ev = evaluate_roster_curve_ev(intersect(roster,covered_ids), gate_paths, curves_p2, lineup_spec)$ev)
}

# ---- reconstruct cell-1 rosters ----------------------------------------------
e <- "synth_00950"; k <- 9L
rb_id <- "eb6be1fa-c875-4300-87e2-7809671a5a00"; te_id <- "9077a1e5-ae83-43f2-ad88-8e44df2d6f4d"
ord_df <- field_order[[e]]
partial <- utils::head(ord_df$underdog_id, k)
next_slot <- ord_df$pick_overall[ord_df$pick_number==k+1L]
future_after <- sort(ord_df$pick_overall[ord_df$pick_number>k+1L])
R <- list(base = ghost_complete(partial, c(next_slot, future_after)),
          RB   = ghost_complete(c(partial, rb_id), future_after),
          TE   = ghost_complete(c(partial, te_id), future_after))

res <- lapply(R, evaluate_two_ways)
cat("\n=== STATIC (pooled field == v1 curve) vs COMOVE (per-path field) ===\n")
cat("(my-static vs curve_ev shown to confirm the reimplementation tracks v1)\n")
for (nm in names(res)) { r <- res[[nm]]
  cat(sprintf("\n%-4s  curve_ev $%.3f | my-static EV $%.3f  (g1 %.3f g2 %.3f g3 %.3f, reachF %.5f)\n",
              nm, r$curve_ev, r$static$ev, r$static$g1, r$static$g2, r$static$g3, r$static$reach_final))
  cat(sprintf("      comove   EV $%.3f  (g1 %.3f g2 %.3f g3 %.3f, reachF %.5f)\n",
              r$comove$ev, r$comove$g1, r$comove$g2, r$comove$g3, r$comove$reach_final))
}
mg <- function(w, fld) res[[w]][[fld]]$ev - res$base[[fld]]$ev
cat("\n=== MARGINALS (vs base) ===\n")
cat(sprintf("  STATIC : RB +$%.3f   TE +$%.3f   (TE-RB $%.3f)\n", mg("RB","static"), mg("TE","static"), mg("TE","static")-mg("RB","static")))
cat(sprintf("  COMOVE : RB +$%.3f   TE +$%.3f   (TE-RB $%.3f)\n", mg("RB","comove"), mg("TE","comove"), mg("TE","comove")-mg("RB","comove")))
cat(sprintf("  curve  : RB +$%.3f   TE +$%.3f   (gate full-sim: RB +$1.185 TE +$4.522)\n",
            res$RB$curve_ev-res$base$curve_ev, res$TE$curve_ev-res$base$curve_ev))
cat("\n[interpretation] if COMOVE raises TE-RB toward the full-sim's +$3.34 and the\n")
cat("  RB falls / TE rises, advancement co-movement is a real root. If the static and\n")
cat("  comove marginals are ~equal, co-movement is NOT the driver (look to noise / assembler).\n")
saveRDS(res, file.path("build","diag_ev_p1_comove.rds"))
