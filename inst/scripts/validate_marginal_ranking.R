# Marginal-ranking gate (no PR): does the curve over-pricing CANCEL in the
# next-pick ranking, or does it reorder the top picks toward variance/stacks?
#
# Decision-quality framing, same as the N=500 path-count regret protocol, but
# curve-vs-full-sim instead of curve-vs-curve-resample.
#
# For a couple of chalky ghost-completion states (a real seed roster kept to
# pick k), we rank a realistic candidate set for pick k+1 -- ~24 options that
# DELIBERATELY include high-variance/boom-bust and same-team stack picks
# alongside the ADP-consensus options -- by:
#   * curve marginal EV   : EV(ghost-complete(partial + cand)) - EV(baseline)
#   * full-sim marginal EV : same two rosters scored by compute_team_ev
# Each candidate is ghost-completed (ADP-best-available for the remaining
# slots, the field consuming the board by ADP) so the roster sits in the curve
# support -- this is the real use mode, not the degenerate partial-roster tail.
#
# Within a cell, every candidate shares ONE fixed pod and ONE compute_team_ev
# seed, so pod luck + bracket-draw noise are COMMON across candidates and
# cancel in the ranking (the comparison is within-cell). We report:
#   * top-pick $-regret : full-sim marginal EV lost by taking the curve's #1
#                         pick instead of full-sim's #1 (valued on full-sim).
#   * top-k overlap + Kendall tau over the shared candidate set.
#   * whether the curve's #1 is a variance/stack pick when full-sim's is not.
#
# Low regret + high overlap  -> the bias is common-mode, cancels in ranking -> ship v1.
# High regret / curve tilts to variance -> need v2 uniqueness-conditioning first.
#
# Run from the package root:
#   "<Rscript>" inst/scripts/validate_marginal_ranking.R > build/validate_marginal.log 2>&1

suppressMessages(devtools::load_all(".", quiet = TRUE))

# ---- knobs --------------------------------------------------------------------
SLATE_ID     <- "nfl_2026_season"
SEED         <- 1L
FIELD_TEAMS  <- 2700L
# (entry, pick-after-k) cells: rank the pick k+1 candidates on that seed.
CELLS        <- list(list(e = "synth_00950", k = 9L),
                     list(e = "synth_00841", k = 9L),
                     list(e = "synth_00950", k = 6L))
N_BASE_ADP   <- 16L       # ADP-consensus candidates (best available by ADP)
N_VARIANCE   <- 6L        # highest-CV (boom/bust) candidates within reach
N_STACK      <- 4L        # same-team-as-QB stack candidates within reach
REACH_ROUNDS <- 4L        # variance/stack candidates may reach up to this many rounds
FULL_SIMS    <- 4000L     # compute_team_ev sims per candidate (1 shared pod)
GATE_PATHS_N <- 1000L
ROSTER_SIZE  <- 18L
POS_CAPS     <- c(QB = 4L, RB = 9L, WR = 10L, TE = 5L)
START_MIN    <- c(QB = 1L, RB = 2L, WR = 3L, TE = 1L)

t_start <- Sys.time()
stamp <- function(label) cat(sprintf("[%6.1f min] %s\n",
  as.numeric(difftime(Sys.time(), t_start, units = "mins")), label))

# ---- 0/1. checkpoint + feed/specs/curves/field -------------------------------
ckpt <- readRDS(file.path("build", "ev_smoke_ckpt.rds"))
layerA <- ckpt$layerA; pool_draws <- ckpt$pool_draws; field_scores <- ckpt$field_scores
stamp("checkpoint loaded")

feed <- blend_slate(SLATE_ID,
  sources_manifest_path = file.path("inst", "data", "sources", "_manifest.yaml"),
  slates_manifest_path  = file.path("inst", "data", "slates", "_manifest.yaml"),
  write_json = FALSE)
positions   <- positions_from_feed(feed)
schedule    <- schedule_from_feed(feed)
team_of     <- vapply(feed$players, function(p) p$team %||% NA_character_, character(1))
names(team_of) <- names(feed$players)
lineup_spec <- load_slate_lineup_spec(SLATE_ID)
pcfg           <- load_tournament("puppy2")
curves_p2      <- build_tournament_curves(pcfg, field_scores, n_grid = 256L, seed = SEED)
field_cache_p2 <- build_field_payouts(field_scores, pcfg, seed = SEED)

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
stamp("curves + field ready")

# ---- ADP board + per-player season variance ----------------------------------
adp_vec <- vapply(feed$players, function(p) as.numeric(p$adp %||% NA_real_), numeric(1))
names(adp_vec) <- names(feed$players)
adp_vec <- adp_vec[!is.na(adp_vec) & names(adp_vec) %in% covered_ids]
adp_board <- names(sort(adp_vec)); board_pos <- positions[adp_board]
adp_rank  <- stats::setNames(seq_along(adp_board), adp_board)

# Season-total mean / SD / CV per player from the full 4000-path pool tensor
# (boom/bust = high coefficient of variation).
season_tot <- apply(pool_draws$tensor, 2L, function(M) colSums(M)) / pool_draws$quant_scale
# season_tot: [n_paths x n_players]
colnames(season_tot) <- pool_draws$player_ids
s_mean <- colMeans(season_tot)
s_sd   <- matrixStats::colSds(season_tot)
s_cv   <- s_sd / pmax(s_mean, 1e-6)
stamp("season variance computed")

# ---- ghost completion (shared with the absolute-EV addendum) ------------------
ghost_complete <- function(partial_ids, future_slots) {
  drafted <- stats::setNames(logical(length(adp_board)), adp_board)
  drafted[partial_ids[partial_ids %in% adp_board]] <- TRUE
  pos_count <- c(QB = 0L, RB = 0L, WR = 0L, TE = 0L)
  for (id in partial_ids) { p <- positions[[id]]
    if (!is.na(p) && p %in% names(pos_count)) pos_count[p] <- pos_count[p] + 1L }
  roster <- partial_ids
  consume_field_to <- function(tt) { if (sum(drafted) >= tt) return(invisible())
    for (id in adp_board) { if (sum(drafted) >= tt) break
      if (!drafted[[id]]) drafted[[id]] <<- TRUE }; invisible() }
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

# Board state just before the drafter's (k+1)th pick: field has consumed the
# board by ADP up to `next_slot - 1` (partial picks included). Returns the set
# of still-available board ids (cap-valid given the partial roster).
available_at <- function(partial_ids, next_slot, pos_count) {
  drafted <- stats::setNames(logical(length(adp_board)), adp_board)
  drafted[partial_ids[partial_ids %in% adp_board]] <- TRUE
  for (id in adp_board) { if (sum(drafted) >= next_slot - 1L) break
    if (!drafted[[id]]) drafted[[id]] <- TRUE }
  avail <- adp_board[!drafted[adp_board]]
  keep <- vapply(avail, function(id) {
    p <- board_pos[[id]]; !is.na(p) && pos_count[[p]] < POS_CAPS[[p]] }, logical(1))
  avail[keep]
}

gate_paths <- pool_draws
gate_paths$tensor  <- pool_draws$tensor[, , seq_len(GATE_PATHS_N), drop = FALSE]
gate_paths$n_paths <- GATE_PATHS_N

curve_ev  <- function(roster) evaluate_roster_curve_ev(roster, gate_paths, curves_p2, lineup_spec)$ev
full_ev   <- function(roster, e, pod_ids, seed) {
  pod <- c(field_rosters[pod_ids], stats::setNames(list(roster), e))
  compute_team_ev(pod_rosters = pod, team_entry_id = e, positions = positions,
    layerA_draws = layerA, schedule = schedule, lineup_spec = lineup_spec,
    tournament_cfg = pcfg, field_cache = field_cache_p2,
    n_sims = FULL_SIMS, seed = seed)$team_ev$total_ev
}

kendall <- function(a, b) suppressWarnings(stats::cor(a, b, method = "kendall"))
topk_overlap <- function(r1, r2, k) length(intersect(utils::head(r1, k), utils::head(r2, k))) / k

# ---- run each cell -------------------------------------------------------------
cell_summ <- list()
for (ci in seq_along(CELLS)) {
  e <- CELLS[[ci]]$e; k <- CELLS[[ci]]$k
  ord_df <- field_order[[e]]
  partial <- utils::head(ord_df$underdog_id, k)
  if (!all(partial %in% covered_ids)) { cat(sprintf("skip %s k%d (uncovered seed)\n", e, k)); next }
  next_slot   <- ord_df$pick_overall[ord_df$pick_number == k + 1L]
  future_after <- sort(ord_df$pick_overall[ord_df$pick_number > k + 1L])
  pos_count <- c(QB = 0L, RB = 0L, WR = 0L, TE = 0L)
  for (id in partial) { p <- positions[[id]]; if (!is.na(p)) pos_count[p] <- pos_count[p] + 1L }

  avail <- available_at(partial, next_slot, pos_count)
  reach_cut <- next_slot + REACH_ROUNDS * 12L
  in_reach  <- avail[adp_rank[avail] <= reach_cut]

  base_cands <- utils::head(avail, N_BASE_ADP)
  var_cands  <- in_reach[order(-s_cv[in_reach])]
  var_cands  <- utils::head(setdiff(var_cands, base_cands), N_VARIANCE)
  qb_teams   <- unique(team_of[partial[positions[partial] == "QB"]])
  qb_teams   <- qb_teams[!is.na(qb_teams)]
  stack_cands <- in_reach[positions[in_reach] %in% c("WR", "TE") &
                          team_of[in_reach] %in% qb_teams]
  stack_cands <- utils::head(setdiff(stack_cands, c(base_cands, var_cands)), N_STACK)
  cands <- unique(c(base_cands, var_cands, stack_cands))
  tag <- ifelse(cands %in% stack_cands, "stack",
         ifelse(cands %in% var_cands, "var", "adp"))

  set.seed(SEED + ci * 17L)
  pod_ids <- sample(setdiff(names(field_rosters), e), 11L)
  cell_seed <- SEED + ci * 17L

  base_roster <- ghost_complete(partial, c(next_slot, future_after))
  base_curve  <- curve_ev(base_roster)
  base_full   <- full_ev(base_roster, e, pod_ids, cell_seed)

  df <- data.frame(id = cands, pos = unname(positions[cands]),
                   adp_rank = unname(adp_rank[cands]), cv = unname(s_cv[cands]),
                   tag = tag, curve_m = NA_real_, full_m = NA_real_,
                   stringsAsFactors = FALSE)
  for (j in seq_along(cands)) {
    rc <- ghost_complete(c(partial, cands[j]), future_after)
    if (length(rc) != ROSTER_SIZE || anyNA(rc)) next
    df$curve_m[j] <- curve_ev(rc) - base_curve
    df$full_m[j]  <- full_ev(rc, e, pod_ids, cell_seed) - base_full
  }
  df <- df[!is.na(df$full_m) & !is.na(df$curve_m), , drop = FALSE]
  curve_ord <- df$id[order(-df$curve_m)]
  full_ord  <- df$id[order(-df$full_m)]
  curve_top <- curve_ord[1L]; full_top <- full_ord[1L]
  regret <- max(df$full_m) - df$full_m[df$id == curve_top]
  best_m <- max(df$full_m)
  tau <- kendall(df$curve_m, df$full_m)

  cat(sprintf("\n=== CELL %d: %s, rank pick %d (overall slot %d), %d candidates ===\n",
              ci, e, k + 1L, next_slot, nrow(df)))
  cat(sprintf("baseline ghost-complete EV: curve $%.3f  full-sim $%.3f  (pod fixed, seed %d)\n",
              base_curve, base_full, cell_seed))
  show <- function(ord, mcol, lab) {
    cat(sprintf("  top-6 by %s:\n", lab))
    for (id in utils::head(ord, 6L)) {
      r <- df[df$id == id, ]
      cat(sprintf("    %-8s %-3s adp#%-3d cv%.2f %-5s  curve_m $%+.3f  full_m $%+.3f\n",
                  substr(id, 1, 8), r$pos, r$adp_rank, r$cv, r$tag, r$curve_m, r$full_m))
    }
  }
  show(curve_ord, "curve_m", "CURVE marginal EV")
  show(full_ord,  "full_m",  "FULL-SIM marginal EV")
  cat(sprintf("  curve #1 = %s (%s/%s)   full-sim #1 = %s (%s/%s)\n",
              substr(curve_top, 1, 8), df$pos[df$id == curve_top], df$tag[df$id == curve_top],
              substr(full_top, 1, 8), df$pos[df$id == full_top], df$tag[df$id == full_top]))
  cat(sprintf("  TOP-PICK REGRET: $%.3f  (curve #1 full-sim value $%.3f vs best $%.3f; %.1f%% of best)\n",
              regret, df$full_m[df$id == curve_top], best_m, 100 * regret / max(best_m, 1e-6)))
  cat(sprintf("  overlap top5 %.2f  top10 %.2f   Kendall tau %.3f\n",
              topk_overlap(curve_ord, full_ord, 5L),
              topk_overlap(curve_ord, full_ord, 10L), tau))
  cat(sprintf("  curve #1 is variance/stack: %s   full-sim #1 is variance/stack: %s\n",
              df$tag[df$id == curve_top] != "adp", df$tag[df$id == full_top] != "adp"))
  cell_summ[[ci]] <- list(cell = ci, e = e, k = k, df = df, regret = regret,
    best_m = best_m, tau = tau, curve_top = curve_top, full_top = full_top,
    o5 = topk_overlap(curve_ord, full_ord, 5L), o10 = topk_overlap(curve_ord, full_ord, 10L))
  stamp(sprintf("  cell %d done: regret $%.3f, tau %.3f, top10 %.2f", ci, regret, tau,
                topk_overlap(curve_ord, full_ord, 10L)))
}

cat("\n=== MARGINAL-RANKING GATE SUMMARY ===\n")
for (s in cell_summ) if (!is.null(s)) cat(sprintf(
  "  %-12s pick%2d: regret $%.3f (%.1f%% of best $%.3f)  tau %.3f  top5 %.2f top10 %.2f  curve#1 %s\n",
  s$e, s$k + 1L, s$regret, 100 * s$regret / max(s$best_m, 1e-6), s$best_m, s$tau,
  s$o5, s$o10, s$df$tag[s$df$id == s$curve_top]))
saveRDS(cell_summ, file.path("build", "validate_marginal_results.rds"))
stamp("done")
