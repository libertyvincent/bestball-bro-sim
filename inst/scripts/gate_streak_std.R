#!/usr/bin/env Rscript
# ============================================================================
# STOP 2 (season_std 3-component reconciliation) + STOP 1 (iid-vs-streaky)
# ============================================================================
# One blend + simulate_slate, then:
#  STOP 2: per-position + spot-player split of the deployed->shipped season_std
#          jump into  rebase (1/q, level) x mask x clip-trim, vs observed.
#  STOP 1: the pre-registered iid-vs-streaky adjudication on an RB-deep roster
#          -- ΔBBP_streak on the RB4's marginal R1-window contribution, against
#          the >=3xMC-SE discriminating gate (DRAW_ZEROING_DESIGN.md §2).
# Run:  "$RS" inst/scripts/gate_streak_std.R > build/gate_streak_std.log 2>&1
suppressMessages(devtools::load_all(".", quiet = TRUE))
suppressWarnings(suppressMessages({ library(httr2); library(jsonlite) }))
`%||%` <- function(a,b) if (is.null(a)) b else a
getn   <- function(x,d=NA_real_) if (is.null(x)) d else as.numeric(x)

SLATE <- "nfl_2026_season"; N_SIMS <- 10000L; SEED <- 20260612L
CACHE <- file.path("~",".bestball-bro","cache")
CDN   <- "https://libertyvincent.github.io/bestball-bro-data"
CV    <- c(QB=0.32, RB=0.50, WR=0.55, TE=0.60)

feed  <- blend_slate(SLATE,
  sources_manifest_path="inst/data/sources/_manifest.yaml",
  slates_manifest_path ="inst/data/slates/_manifest.yaml",
  out_path=tempfile(fileext=".json"), cache_dir=CACHE, write_json=FALSE)
sim   <- simulate_slate(feed, n_sims=N_SIMS, seed=SEED)
efeed <- sim$enriched_feed
dep <- tryCatch({ f<-tempfile(fileext=".json")
  writeBin(resp_body_raw(req_perform(req_timeout(
    request(paste0(CDN,"/v2/projections/",SLATE,".json")),120))),f)
  fromJSON(f,simplifyVector=FALSE) }, error=function(e) NULL)
dep_std <- function(nm){ if(is.null(dep)) return(NA_real_)
  for(p in dep$players) if(identical(tolower(p$name%||%""),tolower(nm))) return(getn(p$season_std)); NA_real_ }

# conditional sum mu_w^2 from PRE-sim feed (conditional weekly means)
sum_mu2_of <- function(uid){ a<-feed$players[[uid]]; s<-0
  for(w in a$weekly%||%list()) if(!isTRUE(w$is_bye)) s<-s+getn(w$mean,0)^2; s }

cat("================ STOP 2: season_std reconciliation ================\n")
cat("deployed sigma -> shipped sigma  =  rebase(1/q) x mask x clip-trim\n\n")

decomp <- function(uid){
  p<-efeed$players[[uid]]; a<-feed$players[[uid]]
  q<-1-getn(p$availability_p_miss,0); d<-getn(p$disagreement_std,0); al<-getn(p$aleatoric_std,0)
  scond<-sqrt(d^2+al^2)
  smask<-sqrt(q^2*d^2 + q*al^2 + q*(1-q)*sum_mu2_of(uid))
  sship<-getn(p$season_std)
  list(q=q, scond=scond, smask=smask, sship=sship)
}

# per-position averages of the three factors (over players with deployed std)
cat(sprintf("%-4s %5s %8s %8s %8s %8s %8s %s\n",
            "pos","q","rebase","mask","clip","prod","obs","note"))
for(pos in c("QB","RB","WR","TE")){
  uids<-vapply(efeed$players,function(p) if(identical(p$position,pos)) p$underdog_id else NA_character_, character(1))
  uids<-uids[!is.na(uids)]
  rb<-c(); mk<-c(); cl<-c(); ob<-c(); qq<-c()
  for(uid in uids){ nm<-efeed$players[[uid]]$name; sd<-dep_std(nm); if(is.na(sd)||sd<=0) next
    D<-decomp(uid); if(!is.finite(D$scond)||D$scond<=0||!is.finite(D$smask)||D$smask<=0) next
    rb<-c(rb,D$scond/sd); mk<-c(mk,D$smask/D$scond); cl<-c(cl,D$sship/D$smask)
    ob<-c(ob,D$sship/sd); qq<-c(qq,D$q) }
  if(!length(rb)) next
  cat(sprintf("%-4s %5.3f %8.3f %8.3f %8.3f %8.3f %8.3f  1/q=%.3f n=%d\n",
      pos, mean(qq), mean(rb), mean(mk), mean(cl),
      mean(rb)*mean(mk)*mean(cl), mean(ob), 1/mean(qq), length(rb)))
}
cat("\n  spot players (rebase x mask x clip = obs jump vs deployed):\n")
cat(sprintf("  %-20s %-3s %7s %7s %6s %6s %6s %7s %7s\n",
            "name","pos","dep","ship","reb","mask","clip","prod","obs"))
for(nm in c("Bijan Robinson","Christian McCaffrey","Josh Allen","CeeDee Lamb","Trey McBride")){
  uid<-NA; for(p in efeed$players) if(identical(tolower(p$name%||%""),tolower(nm))){uid<-p$underdog_id;break}
  if(is.na(uid)) next; D<-decomp(uid); sd<-dep_std(nm)
  reb<-D$scond/sd; mask<-D$smask/D$scond; clip<-D$sship/D$smask
  cat(sprintf("  %-20s %-3s %7.1f %7.1f %6.3f %6.3f %6.3f %7.3f %7.3f\n",
      substr(nm,1,20), efeed$players[[uid]]$position, sd, D$sship,
      reb, mask, clip, reb*mask*clip, D$sship/sd))
}
cat("\n  Interpretation: rebase ~ 1/q (deployed sigma used scaled mu_w);\n")
cat("  mask raises played-week spread (largest for high-mu studs); clip trims.\n\n")

# ============================================================================
# STOP 1 — iid vs streaky adjudication on an RB-deep roster
# ============================================================================
cat("================ STOP 1: iid vs streaky (RB-deep roster) ================\n")
set.seed(SEED)
lspec <- load_slate_lineup_spec(SLATE)
posmap <- positions_from_feed(efeed)
sched  <- schedule_from_feed(efeed)

# Build an RB-deep roster: 4 RBs by position_rank (RB1/RB2/RB3/RB4=insurance),
# plus a legal complement so weekly best-ball optimization is realistic.
by_rank <- function(pos, k){
  ids<-vapply(efeed$players,function(p) if(identical(p$position,pos)) p$underdog_id else NA_character_,character(1))
  ids<-ids[!is.na(ids)]
  pr <-vapply(ids,function(id) getn(efeed$players[[id]]$position_rank,1e9),numeric(1))
  ids[order(pr)][k] }
RB4_INS <- by_rank("RB", 40)             # the pure-insurance deep RB
roster <- c(by_rank("RB", c(4,10,22)), RB4_INS,
            by_rank("QB", c(3,16)), by_rank("WR", c(5,12,20,30)), by_rank("TE", c(4,11)))
roster <- unique(roster[!is.na(roster)])
cat(sprintf("  roster (%d): RB4_INS=%s (rank %s, p_miss %.3f)\n", length(roster),
    efeed$players[[RB4_INS]]$name, efeed$players[[RB4_INS]]$position_rank,
    getn(efeed$players[[RB4_INS]]$availability_p_miss)))

WK <- 1:14
ml <- sample_correlated_draws(player_ids=roster, layerA_draws=sim$draws,
        schedule=sched, n_sims=500L, seed=SEED, output_format="matrix_list")
ml <- ml[as.character(WK)]
q_of <- setNames(1-vapply(roster,function(id) getn(efeed$players[[id]]$availability_p_miss,0),numeric(1)), roster)

# iid mask: independent Bernoulli(q) per (player, week, path)
mask_iid <- function(){ lapply(WK, function(w){
  M<-ml[[as.character(w)]]; ids<-rownames(M)
  t(vapply(ids,function(id){ q<-q_of[[id]]; if(q>=1) rep(1,ncol(M)) else stats::rbinom(ncol(M),1L,q) }, numeric(ncol(M)))) }) }
# streaky mask: per (player,path) 2-state Markov, mean out-run L, stationary P(out)=p_miss
L_RUN <- 3
mask_streak <- function(){ np<-500L
  lapply(seq_along(WK), function(wi) NULL) -> out
  states <- lapply(rownames(ml[[1]]), function(id){
    q<-q_of[[id]]; pmiss<-1-q
    if(pmiss<=0) return(matrix(FALSE, length(WK), np))
    p_in <- 1/L_RUN; p_out <- p_in*pmiss/(1-pmiss)
    st <- matrix(FALSE, length(WK), np)
    st[1,] <- stats::runif(np) < pmiss                 # stationary init
    for(wi in 2:length(WK)){
      prev<-st[wi-1,]; u<-stats::runif(np)
      st[wi,] <- ifelse(prev, u > p_in, u < p_out)     # out->stay-out unless recover; in->go-out
    }
    st })
  names(states)<-rownames(ml[[1]])
  lapply(seq_along(WK), function(wi){
    M<-ml[[wi]]; ids<-rownames(M)
    t(vapply(ids, function(id) as.numeric(!states[[id]][wi,]), numeric(np))) }) }

apply_mask <- function(masks){ lapply(seq_along(WK), function(wi){
  M<-ml[[wi]]; M*masks[[wi]] }) }

r1_marginal <- function(masks){
  mm <- apply_mask(masks)
  named <- setNames(mm, as.character(WK))
  for(wi in seq_along(WK)) rownames(named[[wi]]) <- rownames(ml[[wi]])
  full <- optimize_lineup_totals(named, posmap[roster], lspec, weeks=WK)
  ro2  <- setdiff(roster, RB4_INS)
  wo_named <- lapply(named, function(M) M[ro2,,drop=FALSE])
  names(wo_named)<-as.character(WK)
  wo <- optimize_lineup_totals(wo_named, posmap[ro2], lspec, weeks=WK)
  list(full=rowSums(full), wo=rowSums(wo)) }

set.seed(SEED+1L); iid <- r1_marginal(mask_iid())
set.seed(SEED+2L); stk <- r1_marginal(mask_streak())
marg_iid <- iid$full - iid$wo
marg_stk <- stk$full - stk$wo
dpath <- marg_stk - marg_iid
dbbp  <- mean(dpath); se <- stats::sd(dpath)/sqrt(length(dpath))

cat(sprintf("\n  RB4 marginal R1 contribution:  iid=%.3f  streaky=%.3f (L=%d)\n",
            mean(marg_iid), mean(marg_stk), L_RUN))
cat(sprintf("  ΔBBP_streak = %.4f   MC-SE = %.4f   ratio = %.2f x SE\n",
            dbbp, se, abs(dbbp)/se))
cat(sprintf("  discriminating gate (>=3xSE): %s\n",
            if(abs(dbbp) >= 3*se) "**TRIPS -> hand ΔBBP to hub for EV conversion**"
            else "NOT tripped -> SHIP iid (streakiness not decision-relevant)"))
cat(sprintf("\n  supporting tail (R1-window full roster): iid std=%.2f p90=%.1f | streaky std=%.2f p90=%.1f\n",
            stats::sd(iid$full), quantile(iid$full,.9),
            stats::sd(stk$full), quantile(stk$full,.9)))
cat("DONE.\n")
