# Part 2 -- re-gate v1 (pooled/static) curves against the co-moving oracle.
#
# For 8-10 (entry, pick-after-k) cells spanning pick points / seeds and
# deliberately including variance-bait and scarce-starting-slot situations,
# rank a realistic pick-(k+1) candidate set two ways on ghost-completed
# states:
#   * v1 marginal      : evaluate_roster_curve_ev(partial+cand) - base   (shipped v1)
#   * oracle marginal  : oracle_roster_ev(comove) ...                     (trusted truth)
# CRN throughout (every candidate scored on the same paths + same field).
#
# Decision metrics per cell (oracle = truth):
#   * top-pick regret  : oracle marginal lost by taking v1's #1 instead of
#                        the oracle's #1.
#   * regret CI        : path-bootstrap of the regret (the rare-event
#                        between-path variance is real; this is the
#                        decision-relevant uncertainty under CRN).
#   * Kendall tau, top5/top10 overlap (v1 order vs oracle order).
#   * oracle-STATIC #1 (== v1-equivalent) is reported too: if it equals
#     v1's #1, any v1-vs-oracle disagreement is genuine co-movement, not
#     oracle reimplementation drift.
#
# Run from the repo root:
#   "<Rscript>" inst/scripts/oracle_regate.R > build/oracle_regate.log 2>&1

suppressMessages(devtools::load_all(".", quiet = TRUE))

# ---- knobs ------------------------------------------------------------------
SLATE_ID    <- "nfl_2026_season"; SEED <- 1L; FIELD_TEAMS <- 2700L
N_PATHS     <- 4000L          # use the full tensor for the rare final
MAX_FIELD   <- 2600L
POS_CAPS    <- c(QB=4L,RB=9L,WR=10L,TE=5L); START_MIN <- c(QB=1L,RB=2L,WR=3L,TE=1L)
N_BASE_ADP  <- 16L; N_VARIANCE <- 6L; N_STACK <- 4L; REACH_ROUNDS <- 4L
ROSTER_SIZE <- 18L; N_BOOT <- 600L
REBUILD     <- FALSE
# entries with covered early picks (verified in prior diags) + spread of k.
CELLS <- list(
  list(e="synth_00950", k=6L),  list(e="synth_00950", k=9L),  list(e="synth_00950", k=12L),
  list(e="synth_00841", k=6L),  list(e="synth_00841", k=9L),  list(e="synth_00841", k=12L),
  list(e="synth_00950", k=14L), list(e="synth_00841", k=14L))

t0 <- Sys.time(); stamp <- function(m) cat(sprintf("[%6.1f min] %s\n",
  as.numeric(difftime(Sys.time(), t0, units="mins")), m))

# ---- setup ------------------------------------------------------------------
ckpt <- readRDS(file.path("build","ev_smoke_ckpt.rds"))
layerA <- ckpt$layerA; pool_draws <- ckpt$pool_draws; field_scores <- ckpt$field_scores
feed <- blend_slate(SLATE_ID,
  sources_manifest_path=file.path("inst","data","sources","_manifest.yaml"),
  slates_manifest_path =file.path("inst","data","slates","_manifest.yaml"), write_json=FALSE)
positions <- positions_from_feed(feed); schedule <- schedule_from_feed(feed)
team_of <- vapply(feed$players, function(p) p$team %||% NA_character_, character(1)); names(team_of) <- names(feed$players)
lineup_spec <- load_slate_lineup_spec(SLATE_ID); pcfg <- load_tournament("puppy2")
curves_p2 <- build_tournament_curves(pcfg, field_scores, n_grid=256L, seed=SEED)
picks <- load_scraped_drafts(); pool <- load_slate_data(SLATE_ID)
targets <- compute_field_targets(picks, slate_id=SLATE_ID)
field <- generate_field(SLATE_ID, picks=picks, player_pool=pool, targets=targets, n_teams=FIELD_TEAMS, seed=SEED)
field_rosters <- split(field$rosters$underdog_id, field$rosters$entry_id)
field_order <- split(
  field$rosters[order(field$rosters$entry_id, field$rosters$pick_number),
                c("underdog_id","pick_overall","pick_number")],
  field$rosters$entry_id[order(field$rosters$entry_id, field$rosters$pick_number)])
covered_ids <- pool_draws$player_ids
adp_vec <- vapply(feed$players, function(p) as.numeric(p$adp %||% NA_real_), numeric(1)); names(adp_vec) <- names(feed$players)
adp_vec <- adp_vec[!is.na(adp_vec) & names(adp_vec) %in% covered_ids]
adp_board <- names(sort(adp_vec)); board_pos <- positions[adp_board]; adp_rank <- stats::setNames(seq_along(adp_board), adp_board)
sw <- curves_p2$stage_weeks; cstruct <- oracle_cfg_struct(pcfg)
season_tot <- apply(pool_draws$tensor, 2L, function(M) colSums(M)) / pool_draws$quant_scale
colnames(season_tot) <- pool_draws$player_ids
s_cv <- matrixStats::colSds(season_tot) / pmax(colMeans(season_tot), 1e-6)
stamp("setup done")

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
    drafted[[pick]]<-TRUE; pp<-board_pos[[pick]]; pos_count[[pp]]<-pos_count[[pp]]+1L; roster<-c(roster,pick) }
  roster
}
available_at <- function(partial_ids, next_slot, pos_count) {
  drafted <- stats::setNames(logical(length(adp_board)), adp_board)
  drafted[partial_ids[partial_ids %in% adp_board]] <- TRUE
  for (id in adp_board) { if (sum(drafted) >= next_slot-1L) break; if(!drafted[[id]]) drafted[[id]]<-TRUE }
  avail <- adp_board[!drafted[adp_board]]
  avail[vapply(avail, function(id){ p<-board_pos[[id]]; !is.na(p)&&pos_count[[p]]<POS_CAPS[[p]] }, logical(1))]
}

ev_draws <- pool_draws; ev_draws$tensor <- pool_draws$tensor[,,seq_len(N_PATHS),drop=FALSE]; ev_draws$n_paths <- N_PATHS

# ---- build oracle field (cached at this N/F) --------------------------------
fcache <- file.path("build","oracle_field_regate.rds")
if (!REBUILD && file.exists(fcache)) {
  ofield <- readRDS(fcache); if (ofield$N != N_PATHS || ofield$F < MAX_FIELD) ofield <- NULL
} else ofield <- NULL
if (is.null(ofield)) {
  stamp(sprintf("scoring field (N=%d F<=%d)...", N_PATHS, MAX_FIELD))
  scored <- oracle_score_field(field_rosters, ev_draws, sw, lineup_spec, min_covered=15L, max_field=MAX_FIELD, seed=SEED)
  ofield <- oracle_build_field(scored, cstruct); saveRDS(ofield, fcache)
}
stamp(sprintf("oracle field ready: N=%d F=%d", ofield$N, ofield$F))

v1_ev <- function(roster) evaluate_roster_curve_ev(intersect(roster,covered_ids), ev_draws, curves_p2, lineup_spec)$ev
oc_pp <- function(roster) oracle_roster_ev(roster_round_scores(intersect(roster,covered_ids), ev_draws, sw, lineup_spec),
                                           ofield, mode="comove", final_mode="binom")$per_path
os_ev <- function(roster) oracle_roster_ev(roster_round_scores(intersect(roster,covered_ids), ev_draws, sw, lineup_spec),
                                           ofield, mode="static", final_mode="binom")$ev
kendall <- function(a,b) suppressWarnings(stats::cor(a,b,method="kendall"))
ov <- function(a,b,k) length(intersect(utils::head(a,k),utils::head(b,k)))/k

# ---- run cells --------------------------------------------------------------
summ <- list()
for (ci in seq_along(CELLS)) {
  e <- CELLS[[ci]]$e; k <- CELLS[[ci]]$k
  ord_df <- field_order[[e]]; partial <- utils::head(ord_df$underdog_id, k)
  if (!all(partial %in% covered_ids)) { cat(sprintf("skip %s k%d (uncovered seed)\n", e, k)); next }
  next_slot <- ord_df$pick_overall[ord_df$pick_number==k+1L]
  future_after <- sort(ord_df$pick_overall[ord_df$pick_number>k+1L])
  pos_count <- c(QB=0L,RB=0L,WR=0L,TE=0L)
  for (id in partial){ p<-positions[[id]]; if(!is.na(p)) pos_count[p]<-pos_count[p]+1L }
  avail <- available_at(partial, next_slot, pos_count)
  in_reach <- avail[adp_rank[avail] <= next_slot + REACH_ROUNDS*12L]
  base_c <- utils::head(avail, N_BASE_ADP)
  var_c  <- utils::head(setdiff(in_reach[order(-s_cv[in_reach])], base_c), N_VARIANCE)
  qb_teams <- unique(team_of[partial[positions[partial]=="QB"]]); qb_teams <- qb_teams[!is.na(qb_teams)]
  stk_c <- in_reach[positions[in_reach]%in%c("WR","TE") & team_of[in_reach]%in%qb_teams]
  stk_c <- utils::head(setdiff(stk_c, c(base_c,var_c)), N_STACK)
  cands <- unique(c(base_c, var_c, stk_c))
  tag <- ifelse(cands%in%stk_c,"stack", ifelse(cands%in%var_c,"var","adp"))
  unmet_start <- pmax(0L, START_MIN - pos_count[names(START_MIN)])
  scarce_pos <- names(unmet_start)[unmet_start > 0L]

  base_roster <- ghost_complete(partial, c(next_slot, future_after))
  base_v1 <- v1_ev(base_roster); base_os <- os_ev(base_roster); base_pp <- oc_pp(base_roster)

  D <- data.frame(id=cands, pos=unname(positions[cands]), adp=unname(adp_rank[cands]),
                  cv=unname(s_cv[cands]), tag=tag, v1_m=NA_real_, oc_m=NA_real_, os_m=NA_real_,
                  stringsAsFactors=FALSE)
  PP <- matrix(NA_real_, N_PATHS, length(cands))   # oracle per-path marginal (cand - base)
  for (j in seq_along(cands)) {
    rc <- ghost_complete(c(partial, cands[j]), future_after)
    if (length(rc)!=ROSTER_SIZE || anyNA(rc)) next
    D$v1_m[j] <- v1_ev(rc) - base_v1
    D$os_m[j] <- os_ev(rc) - base_os
    pp <- oc_pp(rc); PP[,j] <- pp - base_pp; D$oc_m[j] <- mean(PP[,j])
  }
  ok <- !is.na(D$oc_m) & !is.na(D$v1_m); D <- D[ok,]; PP <- PP[,ok,drop=FALSE]
  v1_ord <- D$id[order(-D$v1_m)]; oc_ord <- D$id[order(-D$oc_m)]; os_ord <- D$id[order(-D$os_m)]
  v1_top <- v1_ord[1]; oc_top <- oc_ord[1]; os_top <- os_ord[1]
  jtop_v1 <- which(D$id==v1_top)
  regret <- max(D$oc_m) - D$oc_m[jtop_v1]; best_m <- max(D$oc_m)

  # path-bootstrap of the regret (decision-relevant uncertainty under CRN)
  set.seed(SEED + ci)
  boot <- numeric(N_BOOT)
  for (b in seq_len(N_BOOT)) {
    idx <- sample.int(N_PATHS, N_PATHS, replace=TRUE)
    mb <- colMeans(PP[idx,,drop=FALSE])
    boot[b] <- max(mb) - mb[jtop_v1]
  }
  reg_se <- stats::sd(boot); reg_lo <- stats::quantile(boot,0.05); reg_hi <- stats::quantile(boot,0.95)
  p_big <- mean(boot > 0.50)

  cat(sprintf("\n=== CELL %d: %s pick %d (slot %d), %d cands; scarce-start: %s ===\n",
              ci, e, k+1L, next_slot, nrow(D), if(length(scarce_pos)) paste(scarce_pos,collapse="/") else "none"))
  cat(sprintf("  base EV: v1 $%.3f | oracle-static $%.3f | oracle-comove $%.3f\n", base_v1, base_os, mean(base_pp)))
  shw <- function(ord,lab){ cat(sprintf("  top-5 by %s:\n",lab))
    for(id in utils::head(ord,5)){ r<-D[D$id==id,]
      cat(sprintf("    %-8s %-3s adp#%-3d cv%.2f %-5s  v1 %+.3f  oracle %+.3f\n",
          substr(id,1,8),r$pos,r$adp,r$cv,r$tag,r$v1_m,r$oc_m)) } }
  shw(v1_ord,"v1"); shw(oc_ord,"ORACLE-comove")
  cat(sprintf("  v1 #1 = %s (%s/%s) | oracle #1 = %s (%s/%s) | oracle-static #1 = %s (%s)\n",
      substr(v1_top,1,8),D$pos[jtop_v1],D$tag[jtop_v1],
      substr(oc_top,1,8),D$pos[D$id==oc_top],D$tag[D$id==oc_top],
      substr(os_top,1,8),D$pos[D$id==os_top]))
  cat(sprintf("  TOP-PICK REGRET $%.3f (%.0f%% of best $%.3f)  boot SE $%.3f  90%%CI [$%.3f,$%.3f]  P(reg>$.50)=%.2f\n",
      regret, 100*regret/max(best_m,1e-6), best_m, reg_se, reg_lo, reg_hi, p_big))
  cat(sprintf("  Kendall tau %.3f  top5 %.2f  top10 %.2f   v1#1==oracle-static#1: %s\n",
      kendall(D$v1_m,D$oc_m), ov(v1_ord,oc_ord,5), ov(v1_ord,oc_ord,10), v1_top==os_top))
  summ[[ci]] <- list(ci=ci,e=e,k=k,n=nrow(D),regret=regret,best_m=best_m,reg_se=reg_se,
    reg_lo=reg_lo,reg_hi=reg_hi,p_big=p_big,tau=kendall(D$v1_m,D$oc_m),
    o5=ov(v1_ord,oc_ord,5),o10=ov(v1_ord,oc_ord,10),
    v1_top_tag=D$tag[jtop_v1],oc_top_tag=D$tag[D$id==oc_top],
    v1_eq_static=(v1_top==os_top), scarce=length(scarce_pos)>0)
  stamp(sprintf("  cell %d done: regret $%.3f +-%.3f, tau %.3f", ci, regret, reg_se, kendall(D$v1_m,D$oc_m)))
}

cat("\n=== RE-GATE SUMMARY (oracle = truth) ===\n")
for (s in summ) if(!is.null(s)) cat(sprintf(
  "  %-12s pick%2d %sn=%2d: regret $%.3f (%2.0f%%) +-$%.3f CI[%.2f,%.2f] P>.5=%.2f | tau %.2f top5 %.2f top10 %.2f | v1#1=%s oc#1=%s\n",
  s$e, s$k+1L, ifelse(s$scarce,"*",""), s$n, s$regret, 100*s$regret/max(s$best_m,1e-6),
  s$reg_se, s$reg_lo, s$reg_hi, s$p_big, s$tau, s$o5, s$o10, s$v1_top_tag, s$oc_top_tag))
cat("  (* = scarce starting slot open at this pick)\n")
regs <- vapply(summ[!vapply(summ,is.null,logical(1))], function(s) s$regret, numeric(1))
cat(sprintf("\n  cells: %d | max regret $%.3f | mean regret $%.3f | cells regret>$0.50: %d | cells P(reg>.5)>0.5: %d\n",
    length(regs), max(regs), mean(regs), sum(regs>0.50),
    sum(vapply(summ[!vapply(summ,is.null,logical(1))], function(s) s$p_big>0.5, logical(1)))))
saveRDS(summ, file.path("build","oracle_regate_results.rds"))
stamp("done")
