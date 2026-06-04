# Part 1 self-validation of the co-moving low-variance EV oracle
# (R/ev_oracle.R). Scores the field ONCE (cached) and reports, before the
# re-gate:
#   [A] structural seat-conservation: mean field survival weights must hit
#       the bracket seat ratios (2/12, *1/10, *1/5) -- proves the
#       survivor-carry is self-consistent. Shown vs field size F (subsets)
#       to expose any finite-field bias.
#   [B] co-movement reproduces: static (pooled == v1) vs comove (per-path
#       survivor-weighted) EV + marginals on the cell-1 rosters, with the
#       comove EV shown vs F (convergence) and for both final-integration
#       modes (binom spread vs deterministic rank). oracle-static must
#       track the v1 curve EV.
#   [C] marginal SE: path-MC SE of the CRN marginal (sd/sqrt(N)); plus a
#       field-resample estimate from two disjoint F/2 halves. Must beat the
#       $0.50 bar.
#   [D] bias check vs a hard-bracket co-moving Monte Carlo: analytic
#       (Rao-Blackwellized) reach must match honest hard-podding reach
#       within the hard-MC noise -- the decisive test for Jensen inflation.
#
# Run from the repo root:
#   "<Rscript>" inst/scripts/oracle_selfvalidate.R > build/oracle_selfvalidate.log 2>&1

suppressMessages(devtools::load_all(".", quiet = TRUE))

# ---- knobs ------------------------------------------------------------------
SLATE_ID    <- "nfl_2026_season"; SEED <- 1L; FIELD_TEAMS <- 2700L
N_PATHS     <- 2000L     # oracle paths (tensor has 4000)
MAX_FIELD   <- 2600L     # field rosters scored on the paths
F_SUBSETS   <- c(400L, 800L, 1600L, 2600L)   # convergence probe (column subsets)
POS_CAPS    <- c(QB=4L,RB=9L,WR=10L,TE=5L); START_MIN <- c(QB=1L,RB=2L,WR=3L,TE=1L)
REBUILD     <- FALSE
HB_PATHS    <- 800L      # paths for the hard-bracket bias check
HB_REPS     <- 60L       # hard-podding replicates per path

t0 <- Sys.time()
stamp <- function(m) cat(sprintf("[%6.1f min] %s\n",
  as.numeric(difftime(Sys.time(), t0, units="mins")), m))

# ---- setup (feed / specs / field / curves) ----------------------------------
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
    drafted[[pick]]<-TRUE; pp<-board_pos[[pick]]; pos_count[[pp]]<-pos_count[[pp]]+1L
    roster<-c(roster,pick) }
  roster
}

ev_draws <- pool_draws
ev_draws$tensor  <- pool_draws$tensor[, , seq_len(N_PATHS), drop=FALSE]
ev_draws$n_paths <- N_PATHS

# ---- score the field ONCE (cached) ------------------------------------------
fcache <- file.path("build","oracle_field_scored.rds")
if (!REBUILD && file.exists(fcache)) {
  scored <- readRDS(fcache)
  if (nrow(scored$fR$R1) != N_PATHS || ncol(scored$fR$R1) < MAX_FIELD) scored <- NULL
} else scored <- NULL
if (is.null(scored)) {
  stamp(sprintf("scoring field (N=%d, up to F=%d)...", N_PATHS, MAX_FIELD))
  scored <- oracle_score_field(field_rosters, ev_draws, sw, lineup_spec,
                               min_covered=15L, max_field=MAX_FIELD, seed=SEED)
  saveRDS(scored, fcache)
}
F_have <- ncol(scored$fR$R1)
stamp(sprintf("field scored: F=%d", F_have))

sub_scored <- function(cols) list(fR=lapply(scored$fR, function(M) M[, cols, drop=FALSE]),
                                  entry_ids=scored$entry_ids[cols])
ofield <- oracle_build_field(scored, cstruct)         # full F
stamp("full oracle field built")

# ---- [A] structural seat-conservation vs F ----------------------------------
seat_ratio <- c(qf=cstruct$seats[2]/cstruct$seats[1], sf=cstruct$seats[3]/cstruct$seats[1],
                final=cstruct$seats[4]/cstruct$seats[1])
cat("\n=== [A] STRUCTURAL SEAT-CONSERVATION vs field size F ===\n")
cat(sprintf("  target reach: QF %.5f  SF %.5f  FIN %.6f\n", seat_ratio["qf"], seat_ratio["sf"], seat_ratio["final"]))
for (Fs in F_SUBSETS[F_SUBSETS <= F_have]) {
  of <- oracle_build_field(sub_scored(seq_len(Fs)), cstruct)
  cat(sprintf("  F=%-5d  QF %.5f (%3.0f%%)  SF %.5f (%3.0f%%)  FIN %.6f (%3.0f%%)\n", Fs,
      mean(of$W[[2]]), 100*mean(of$W[[2]])/seat_ratio["qf"],
      mean(of$W[[3]]), 100*mean(of$W[[3]])/seat_ratio["sf"],
      mean(of$W[[4]]), 100*mean(of$W[[4]])/seat_ratio["final"]))
}

# ---- cell-1 rosters (base / variance-RB / scarce-TE) ------------------------
e <- "synth_00950"; k <- 9L
rb_id <- "eb6be1fa-c875-4300-87e2-7809671a5a00"   # variance RB (adp#160, cv0.17)
te_id <- "9077a1e5-ae83-43f2-ad88-8e44df2d6f4d"   # scarce TE (adp#126)
ord_df <- field_order[[e]]
partial <- utils::head(ord_df$underdog_id, k)
next_slot <- ord_df$pick_overall[ord_df$pick_number==k+1L]
future_after <- sort(ord_df$pick_overall[ord_df$pick_number>k+1L])
R <- list(base = ghost_complete(partial, c(next_slot, future_after)),
          RB   = ghost_complete(c(partial, rb_id), future_after),
          TE   = ghost_complete(c(partial, te_id), future_after))
RS <- lapply(R, function(r) roster_round_scores(intersect(r, covered_ids), ev_draws, sw, lineup_spec))
v1 <- lapply(R, function(r) evaluate_roster_curve_ev(intersect(r,covered_ids), ev_draws, curves_p2, lineup_spec)$ev)

# ---- [B] co-movement reproduction (+ F-convergence, + final modes) ----------
cat("\n=== [B] CO-MOVEMENT: static(==v1 pooled) vs comove(per-path survivors) ===\n")
cat("  v1-curve EV:  base $", sprintf("%.3f",v1$base), " RB $", sprintf("%.3f",v1$RB),
    " TE $", sprintf("%.3f",v1$TE), "\n", sep="")
for (fm in c("binom","rank")) {
  cat(sprintf("  -- final_mode = %s --\n", fm))
  os <- lapply(RS, function(rs) oracle_roster_ev(rs, ofield, mode="static", final_mode=fm))
  cat(sprintf("   static F=%d : base $%.3f  RB $%.3f  TE $%.3f\n", F_have, os$base$ev, os$RB$ev, os$TE$ev))
  for (Fs in F_SUBSETS[F_SUBSETS <= F_have]) {
    of <- oracle_build_field(sub_scored(seq_len(Fs)), cstruct)
    oc <- lapply(RS, function(rs) oracle_roster_ev(rs, of, mode="comove", final_mode=fm))
    cat(sprintf("   comove F=%-5d: base $%.3f  RB $%.3f  TE $%.3f   marg RB %+.3f TE %+.3f (TE-RB %+.3f)\n",
        Fs, oc$base$ev, oc$RB$ev, oc$TE$ev,
        oc$RB$ev-oc$base$ev, oc$TE$ev-oc$base$ev, (oc$TE$ev-oc$base$ev)-(oc$RB$ev-oc$base$ev)))
  }
}
# canonical oracle = full F, binom final
oc <- lapply(RS, function(rs) oracle_roster_ev(rs, ofield, mode="comove", final_mode="binom"))
os <- lapply(RS, function(rs) oracle_roster_ev(rs, ofield, mode="static", final_mode="binom"))
cat(sprintf("\n  reachF: base s=%.5f c=%.5f | RB s=%.5f c=%.5f | TE s=%.5f c=%.5f\n",
    os$base$reach["final"], oc$base$reach["final"], os$RB$reach["final"], oc$RB$reach["final"],
    os$TE$reach["final"], oc$TE$reach["final"]))
stamp("co-movement done")

# ---- [C] marginal SE --------------------------------------------------------
cat("\n=== [C] MARGINAL SE (comove binom, full F, CRN) ===\n")
for (nm in c("RB","TE")) {
  d <- oc[[nm]]$per_path - oc[["base"]]$per_path
  cat(sprintf("  %-3s marginal %+.4f  path-MC SE $%.4f  -> %s (bar $0.50)\n",
      nm, mean(d), stats::sd(d)/sqrt(length(d)), ifelse(stats::sd(d)/sqrt(length(d))<0.50,"PASS","FAIL")))
}
if (F_have >= 2L) {
  half <- F_have %/% 2L
  ofa <- oracle_build_field(sub_scored(seq_len(half)), cstruct)
  ofb <- oracle_build_field(sub_scored((half+1L):F_have), cstruct)
  oca <- lapply(RS, function(rs) oracle_roster_ev(rs, ofa, mode="comove", final_mode="binom"))
  ocb <- lapply(RS, function(rs) oracle_roster_ev(rs, ofb, mode="comove", final_mode="binom"))
  mg <- function(L,nm) L[[nm]]$ev - L$base$ev
  cat(sprintf("  field-half A vs B (F=%d each) comove marginals:\n", half))
  cat(sprintf("    RB  A %+.3f  B %+.3f  (|diff| $%.3f)\n", mg(oca,"RB"), mg(ocb,"RB"), abs(mg(oca,"RB")-mg(ocb,"RB"))))
  cat(sprintf("    TE  A %+.3f  B %+.3f  (|diff| $%.3f)\n", mg(oca,"TE"), mg(ocb,"TE"), abs(mg(oca,"TE")-mg(ocb,"TE"))))
}
stamp("SE done")

# ---- [D] bias check vs hard-bracket co-moving Monte Carlo -------------------
fR <- ofield$fR; Fn <- ofield$F
pod_survivors <- function(idx, scores, pod_size, adv_n) {
  idx <- sample(idx); nfit <- (length(idx) %/% pod_size) * pod_size
  if (nfit < pod_size) return(integer(0))
  idx <- idx[seq_len(nfit)]; win <- integer(0)
  for (s in seq.int(1L, nfit-pod_size+1L, by=pod_size)) {
    pod <- idx[s:(s+pod_size-1L)]; win <- c(win, pod[order(-scores[pod])][seq_len(adv_n)]) }
  win
}
reach_hard <- lapply(R, function(.) c(qf=0,sf=0,final=0)); n_acc <- 0L
set.seed(SEED)
np_hb <- min(HB_PATHS, ofield$N)
for (t in seq_len(np_hb)) {
  s1 <- fR$R1[t,]; s2 <- fR$R2[t,]; s3 <- fR$R3[t,]
  myr <- lapply(RS, function(rs) c(rs$R1[t], rs$R2[t], rs$R3[t]))
  for (rep in seq_len(HB_REPS)) {
    S1 <- pod_survivors(seq_len(Fn), s1, cstruct$pod[1], cstruct$advn[1])
    S2 <- if (length(S1) >= cstruct$pod[2]) pod_survivors(S1, s2, cstruct$pod[2], cstruct$advn[2]) else integer(0)
    o1 <- sample(Fn, cstruct$pod[1]-1L)
    o2 <- if (length(S1) >= cstruct$pod[2]-1L) sample(S1, cstruct$pod[2]-1L) else integer(0)
    o3 <- if (length(S2) >= cstruct$pod[3]-1L) sample(S2, cstruct$pod[3]-1L) else integer(0)
    for (nm in names(R)) {
      mr <- myr[[nm]]
      a1 <- sum(s1[o1] > mr[1]) <= (cstruct$advn[1]-1L)
      a2 <- a1 && length(o2)>0 && all(s2[o2] < mr[2])
      a3 <- a2 && length(o3)>0 && all(s3[o3] < mr[3])
      reach_hard[[nm]]["qf"] <- reach_hard[[nm]]["qf"] + a1
      reach_hard[[nm]]["sf"] <- reach_hard[[nm]]["sf"] + a2
      reach_hard[[nm]]["final"] <- reach_hard[[nm]]["final"] + a3
    }
  }
  n_acc <- n_acc + HB_REPS
}
oc_hb <- lapply(RS, function(rs) {
  rs2 <- rs[seq_len(np_hb), , drop=FALSE]
  of2 <- ofield; of2$N <- np_hb
  of2$W  <- lapply(ofield$W,  function(M) M[seq_len(np_hb), , drop=FALSE])
  of2$fR <- lapply(ofield$fR, function(M) M[seq_len(np_hb), , drop=FALSE])
  oracle_roster_ev(rs2, of2, mode="comove", final_mode="binom")$reach
})
cat(sprintf("\n=== [D] BIAS CHECK: analytic vs hard-bracket reach (paths=%d reps=%d -> %d samples) ===\n",
            np_hb, HB_REPS, n_acc))
for (nm in names(R)) {
  hd <- reach_hard[[nm]]/n_acc; an <- oc_hb[[nm]]
  sef <- sqrt(pmax(hd,1e-9)*(1-pmax(hd,1e-9))/n_acc)
  cat(sprintf("  %-4s QF: an %.4f hd %.4f (+-%.4f)  SF: an %.5f hd %.5f (+-%.5f)  FIN: an %.6f hd %.6f (+-%.6f)\n",
      nm, an["qf"],hd["qf"],sef["qf"], an["sf"],hd["sf"],sef["sf"], an["final"],hd["final"],sef["final"]))
}
cat("  [pass if analytic within ~2 SE of hard at each round]\n")
stamp("bias check done")

saveRDS(list(seat_ratio=seat_ratio, v1=v1, static=os, comove=oc,
             reach_hard=lapply(reach_hard,function(x)x/n_acc), analytic_hb=oc_hb, n_hb=n_acc),
        file.path("build","oracle_selfvalidate.rds"))
stamp("done")
