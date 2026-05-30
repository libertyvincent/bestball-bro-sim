# 3b-5 xAdv bias audit (throwaway).
#
# Goal: decide whether 3b-5's +0.108 xAdv optimism is a player-matching
# artifact (opponents fail the bridge more than our teams -> opponent
# scores deflated -> we rank higher) or a genuine projection-spread
# difference (we rate strong teams higher than BBMDB does).
#
# Method: reproduce the validator's bridge over all 20 validation pods,
# split match outcomes into (our 20 teams) vs (220 opponent teams),
# dump unmatched players, look at projection quality of unmatched, and
# regress the per-team signed xAdv error against the two candidate
# explanatory channels.

cat("== Loading data ==\n")
picks <- load_scraped_drafts()
bbmdb_path <- "C:/Users/vince/Desktop/bbmdb_scraper/data/bbmdb_teams.parquet"
bbmdb <- arrow::read_parquet(bbmdb_path)

# Re-resolve which BBMDB rows map to which scraper drafts + tournaments,
# mirroring the validator's logic.
all_tnmts <- load_tournaments()
by_uuid <- list()
for (t in all_tnmts) by_uuid[[t$underdog_tournament_id]] <- t

apples <- bbmdb[bbmdb$tournament_corpus == "post_nfl_draft" &
                bbmdb$underdog_entry_id %in% picks$draft_entry_id, ,
                drop = FALSE]
apples$draft_id        <- NA_character_
apples$tournament_id   <- NA_character_
apples$slate_id        <- NA_character_
for (i in seq_len(nrow(apples))) {
  eid <- apples$underdog_entry_id[i]
  rows <- picks[picks$draft_entry_id == eid, , drop = FALSE]
  if (nrow(rows) == 0L) next
  apples$draft_id[i]      <- rows$draft_id[1L]
  uuid <- rows$tournament_id[1L]
  cfg <- by_uuid[[uuid]]
  if (!is.null(cfg)) {
    apples$tournament_id[i] <- cfg$tournament_id
    apples$slate_id[i]      <- cfg$slate_id
  }
}
ready <- apples[!is.na(apples$slate_id) &
                apples$slate_id == "nfl_2026_season", , drop = FALSE]
cat("Validatable rows:", nrow(ready), "\n")
cat("Distinct drafts (pods):", length(unique(ready$draft_id)), "\n")

cat("\n== Blending feed (slate nfl_2026_season) ==\n")
sources_path <- file.path("inst", "data", "sources", "_manifest.yaml")
slates_path  <- file.path("inst", "data", "slates",  "_manifest.yaml")
feed <- blend_slate(slate_id = "nfl_2026_season",
                    sources_manifest_path = sources_path,
                    slates_manifest_path  = slates_path,
                    write_json = FALSE)
cat("feed$players count:", length(feed$players), "\n")

# Replicate the validator's bridge.
bridge <- bestballBroSim:::.scraper_to_feed_id_map(picks, feed)
cat("Bridge resolved", length(bridge), "of",
    length(unique(picks$underdog_id)), "unique scraper player UUIDs\n")

# ---------------------------------------------------------------------------
# Part 1 -- per-pod, per-team match outcomes (ours vs opponents)
# ---------------------------------------------------------------------------
cat("\n== Part 1: match completeness per pod ==\n")

# Build a frame of every (draft_entry_id, draft_id, underdog_id, name, pos,
# adp_at_pick, projection_points, matched, side).
pod_drafts <- unique(ready$draft_id)
pick_roster <- picks[picks$draft_id %in% pod_drafts, , drop = FALSE]
pick_roster$matched <- !is.na(bridge[pick_roster$underdog_id])
pick_roster$feed_uid <- bridge[pick_roster$underdog_id]

# Which entries are "ours" (in the BBMDB validation set) vs opponents
our_eids <- ready$underdog_entry_id
pick_roster$is_ours <- pick_roster$draft_entry_id %in% our_eids

cat(sprintf("Total picks in scope: %d (across %d pods)\n",
            nrow(pick_roster), length(pod_drafts)))
cat(sprintf("Overall match rate: %.3f%%\n",
            100 * mean(pick_roster$matched)))
cat(sprintf("  - our %d teams: %.3f%%\n",
            length(our_eids),
            100 * mean(pick_roster$matched[pick_roster$is_ours])))
opp_picks <- pick_roster[!pick_roster$is_ours & pick_roster$draft_id %in% pod_drafts, ]
cat(sprintf("  - %d opponent teams: %.3f%%\n",
            length(unique(opp_picks$draft_entry_id)),
            100 * mean(opp_picks$matched)))

# Per-team match-fail counts to feed Part 2 regressions.
per_team <- aggregate(
  matched ~ draft_entry_id + draft_id,
  data = pick_roster,
  FUN = function(x) c(n = length(x), miss = sum(!x))
)
per_team <- do.call(data.frame, per_team)
names(per_team) <- c("draft_entry_id", "draft_id", "n_picks", "n_miss")
per_team$miss_rate <- per_team$n_miss / per_team$n_picks
per_team$is_ours <- per_team$draft_entry_id %in% our_eids

# Per-pod summary: our team's miss rate vs mean opponent miss rate.
per_pod <- list()
for (d in pod_drafts) {
  this <- per_team[per_team$draft_id == d, ]
  ours <- this[this$is_ours, ]
  opp  <- this[!this$is_ours, ]
  per_pod[[length(per_pod) + 1L]] <- data.frame(
    draft_id = d,
    n_our_teams = nrow(ours),
    our_miss_rate = if (nrow(ours)) mean(ours$miss_rate) else NA_real_,
    opp_miss_rate_mean = mean(opp$miss_rate),
    opp_miss_rate_max  = max(opp$miss_rate),
    asymmetry = mean(opp$miss_rate) - mean(ours$miss_rate)
  )
}
pod_summary <- do.call(rbind, per_pod)
cat("\nPer-pod asymmetry (opp_miss - our_miss):\n")
print(summary(pod_summary$asymmetry))
cat("\nFraction of pods where opponents miss MORE than our team:",
    round(mean(pod_summary$asymmetry > 0), 3), "\n")
cat("Mean per-pod asymmetry (positive => opponent deflation):",
    round(mean(pod_summary$asymmetry), 4), "\n")

# ---------------------------------------------------------------------------
# Part 1.3 -- unmatched-player list with patterns
# ---------------------------------------------------------------------------
cat("\n== Unmatched player list (with side, projection, pick context) ==\n")
unmatched <- pick_roster[!pick_roster$matched, ]
cat("Unique unmatched (name, position):\n")
uniq_unmatched <- unique(unmatched[, c("first_name", "last_name",
                                        "position_name")])
cat("Total distinct unmatched players:", nrow(uniq_unmatched), "\n")

# Add side breakdown per distinct player.
uniq_unmatched$n_picks_in_scope <- NA_integer_
uniq_unmatched$n_picks_on_ours  <- NA_integer_
uniq_unmatched$avg_adp_at_pick  <- NA_real_
uniq_unmatched$avg_proj_pts     <- NA_real_
for (i in seq_len(nrow(uniq_unmatched))) {
  m <- pick_roster$first_name == uniq_unmatched$first_name[i] &
       pick_roster$last_name  == uniq_unmatched$last_name[i] &
       pick_roster$position_name == uniq_unmatched$position_name[i]
  uniq_unmatched$n_picks_in_scope[i] <- sum(m & !pick_roster$matched)
  uniq_unmatched$n_picks_on_ours[i]  <- sum(m & !pick_roster$matched & pick_roster$is_ours)
  uniq_unmatched$avg_adp_at_pick[i]  <- mean(pick_roster$projection_adp_at_pick[m & !pick_roster$matched], na.rm = TRUE)
  uniq_unmatched$avg_proj_pts[i]     <- mean(pick_roster$projection_points[m & !pick_roster$matched], na.rm = TRUE)
}
uniq_unmatched <- uniq_unmatched[order(uniq_unmatched$avg_adp_at_pick), ]
cat("\nFull unmatched table (sorted by avg ADP at pick):\n")
print(uniq_unmatched, row.names = FALSE)

# Look for surface patterns: suffixes, apostrophes, periods, hyphens.
flag_pattern <- function(nm) {
  has <- list(
    suffix_jr   = grepl("(Jr\\.?|II|III|IV)$", nm, perl = TRUE),
    apostrophe  = grepl("'", nm, fixed = TRUE),
    period      = grepl("\\.", nm, perl = TRUE),
    hyphen      = grepl("-", nm, fixed = TRUE)
  )
  has
}
nms_full <- paste(uniq_unmatched$first_name, uniq_unmatched$last_name)
flags <- flag_pattern(nms_full)
cat("\nUnmatched names with surface-pattern flags:\n")
for (k in names(flags)) {
  n <- sum(flags[[k]])
  if (n > 0L) cat(sprintf("  %-12s n=%d  | %s\n", k, n,
                          paste(nms_full[flags[[k]]], collapse = ", ")))
}

# ---------------------------------------------------------------------------
# Part 1.5 -- quality skew (where are unmatched in the draft / projection?)
# ---------------------------------------------------------------------------
cat("\n== Quality skew of unmatched players ==\n")
matched_picks <- pick_roster[pick_roster$matched, ]
cat(sprintf("Matched picks: avg ADP at pick   = %.1f, avg proj pts = %.1f\n",
            mean(matched_picks$projection_adp_at_pick, na.rm = TRUE),
            mean(matched_picks$projection_points,      na.rm = TRUE)))
cat(sprintf("Unmatched picks: avg ADP at pick = %.1f, avg proj pts = %.1f\n",
            mean(unmatched$projection_adp_at_pick, na.rm = TRUE),
            mean(unmatched$projection_points,      na.rm = TRUE)))

# Round-bucket: how many of the unmatched were taken in rounds 1-12 (starters
# /flex pool) vs 13-18 (deep bench)?
cat("\nUnmatched picks by round bucket:\n")
print(table(cut(unmatched$round,
                breaks = c(0, 6, 12, 18, Inf),
                labels = c("R1-6", "R7-12", "R13-18", ">18"))))

# ---------------------------------------------------------------------------
# Part 2 -- decompose the signed xAdv error
# ---------------------------------------------------------------------------
cat("\n== Part 2: decompose signed xAdv error ==\n")

# Run the validator (faster, no markdown report) to get per-team predicted_xadv.
result <- validate_xadv_against_bbmdb(
  bbmdb_path    = bbmdb_path,
  scraper_path  = file.path("inst", "data", "scraped_drafts",
                            "udbb-scraper-latest.json"),
  layerA_n_sims = 5000L,
  n_sims        = 5000L,
  base_seed     = 1L,
  verbose       = FALSE
)
per_team_xadv <- result$per_team
per_team_xadv <- per_team_xadv[per_team_xadv$status == "ready", ]
per_team_xadv$signed_err <- per_team_xadv$predicted_xadv -
  per_team_xadv$bbmdb_xadv

# Join asymmetry numbers per pod.
per_team_xadv <- merge(per_team_xadv,
                       pod_summary[, c("draft_id", "asymmetry",
                                       "our_miss_rate",
                                       "opp_miss_rate_mean")],
                       by = "draft_id", all.x = TRUE)
# Projection delta (ours - bbmdb).
per_team_xadv$proj_delta <- per_team_xadv$our_team_projection -
  per_team_xadv$bbmdb_team_projection

cat("\nPer-team signed err (predicted - bbmdb) summary:\n")
print(summary(per_team_xadv$signed_err))
cat("\nPer-team signed err vs pod asymmetry (opp_miss - our_miss):\n")
print(stats::cor.test(per_team_xadv$signed_err,
                      per_team_xadv$asymmetry,
                      method = "pearson"))
cat("\nPer-team signed err vs projection delta (ours - bbmdb):\n")
print(stats::cor.test(per_team_xadv$signed_err,
                      per_team_xadv$proj_delta,
                      method = "pearson"))

cat("\nPer-team table (signed err, asymmetry, proj delta):\n")
show_cols <- c("entry_id", "tournament_id", "bbmdb_xadv",
               "predicted_xadv", "signed_err", "our_miss_rate",
               "opp_miss_rate_mean", "asymmetry",
               "our_team_projection", "bbmdb_team_projection",
               "proj_delta")
print(per_team_xadv[, show_cols], row.names = FALSE)
