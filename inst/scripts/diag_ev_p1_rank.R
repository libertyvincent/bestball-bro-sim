# Phase 1 / final: does the co-movement bias change the RANKING decision?
#
# Rank ALL 22 cell-1 candidates two ways, both noise-free on the same tensor
# paths: STATIC (pooled field == v1 curve) vs COMOVE (per-path field). If the
# top pick and order barely move, the co-movement bias is not decision-relevant
# (v1 ranking is fine despite the absolute over-pricing). If they reorder, the
# bias justifies field-conditioned (v2) curves.

suppressMessages(devtools::load_all(".", quiet = TRUE))
SLATE_ID <- "nfl_2026_season"; SEED <- 1L; FIELD_TEAMS <- 2700L
POS_CAPS <- c(QB=4L,RB=9L,WR=10L,TE=5L); START_MIN <- c(QB=1L,RB=2L,WR=3L,TE=1L)
GATE_PATHS_N <- 1000L; N_FIELD <- 800L

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
gate_paths$tensor <- pool_draws$tensor[, , seq_len(GATE_PATHS_N), drop=FALSE]; gate_paths$n_paths <- GATE_PATHS_N
sw <- curves_p2$stage_weeks
rscore <- function(roster) roster_round_scores(intersect(roster,covered_ids), gate_paths, sw, lineup_spec)

cov_count <- vapply(field_rosters, function(r) sum(r %in% covered_ids), integer(1))
elig <- names(field_rosters)[cov_count >= 15L]; set.seed(SEED); fsamp <- sample(elig, min(N_FIELD,length(elig)))
cat(sprintf("scoring %d field rosters on %d paths...\n", length(fsamp), GATE_PATHS_N))
fR1 <- matrix(0,GATE_PATHS_N,length(fsamp)); fR2<-fR1; fR3<-fR1
for (i in seq_along(fsamp)) { rs<-rscore(field_rosters[[fsamp[i]]]); fR1[,i]<-rs$R1; fR2[,i]<-rs$R2; fR3[,i]<-rs$R3 }
F1<-stats::ecdf(as.numeric(fR1)); F2<-stats::ecdf(as.numeric(fR2)); F3<-stats::ecdf(as.numeric(fR3))
g1f<-function(p) p^11+11*(1-p)*p^10; g2f<-function(p) p^9; g3f<-function(p) p^4
ppc<-function(x,FM) rowMeans(FM<=x)
ev_two <- function(roster) {
  rs<-rscore(roster)
  hf<-stats::approx(curves_p2$curves$h_final$x, curves_p2$curves$h_final$y, xout=rs$R4, rule=2, ties="ordered")$y
  comb<-function(g1,g2,g3) mean(g1*((1-g2)*5 + g2*(1-g3)*25 + g2*g3*hf))
  s<-comb(g1f(F1(rs$R1)), g2f(F2(rs$R2)), g3f(F3(rs$R3)))
  cm<-comb(g1f(ppc(rs$R1,fR1)), g2f(ppc(rs$R2,fR2)), g3f(ppc(rs$R3,fR3)))
  c(static=s, comove=cm)
}

# cell-1 candidates (exact set the gate used) + rosters
cs <- readRDS("build/validate_marginal_results.rds")[[1]]
cands <- cs$df$id; tags <- cs$df$tag; poss <- cs$df$pos
e<-"synth_00950"; k<-9L; ord_df<-field_order[[e]]
partial<-utils::head(ord_df$underdog_id,k)
next_slot<-ord_df$pick_overall[ord_df$pick_number==k+1L]
future_after<-sort(ord_df$pick_overall[ord_df$pick_number>k+1L])
base_ev <- ev_two(ghost_complete(partial, c(next_slot, future_after)))

D <- data.frame(id=cands, pos=poss, tag=tags, s_m=NA_real_, c_m=NA_real_, stringsAsFactors=FALSE)
for (j in seq_along(cands)) {
  ev <- ev_two(ghost_complete(c(partial, cands[j]), future_after))
  D$s_m[j] <- ev["static"] - base_ev["static"]; D$c_m[j] <- ev["comove"] - base_ev["comove"]
}
so <- D$id[order(-D$s_m)]; co <- D$id[order(-D$c_m)]
tau <- suppressWarnings(stats::cor(D$s_m, D$c_m, method="kendall"))
ov <- function(a,b,k) length(intersect(utils::head(a,k),utils::head(b,k)))/k
cat("\n=== cell-1 candidate ranking: STATIC (v1 curve) vs COMOVE (co-moving field) ===\n")
sh<-function(ord,mc,lab){ cat(sprintf(" top-6 by %s:\n",lab))
  for(id in utils::head(ord,6)){ r<-D[D$id==id,]
    cat(sprintf("   %-8s %-3s %-5s  static_m $%+.3f  comove_m $%+.3f\n",substr(id,1,8),r$pos,r$tag,r$s_m,r$c_m)) } }
sh(so,"s_m","STATIC marginal"); sh(co,"c_m","COMOVE marginal")
cat(sprintf("\n STATIC #1 = %s (%s/%s)   COMOVE #1 = %s (%s/%s)\n",
            substr(so[1],1,8),D$pos[D$id==so[1]],D$tag[D$id==so[1]],
            substr(co[1],1,8),D$pos[D$id==co[1]],D$tag[D$id==co[1]]))
reg_static_vs_comove <- max(D$c_m) - D$c_m[D$id==so[1]]   # comove-$ lost by taking the static#1
cat(sprintf(" if you trust COMOVE as truth: top-pick regret of the STATIC(v1) #1 = $%.3f (%.1f%% of best $%.3f)\n",
            reg_static_vs_comove, 100*reg_static_vs_comove/max(max(D$c_m),1e-6), max(D$c_m)))
cat(sprintf(" Kendall tau %.3f   top5 overlap %.2f   top10 overlap %.2f\n", tau, ov(so,co,5), ov(so,co,10)))
saveRDS(D, "build/diag_ev_p1_rank.rds")
