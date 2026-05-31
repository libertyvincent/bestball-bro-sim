# Headline BBM7 EV run (Sprint 3b-7, head 5).
#
#   1. Money conservation at the recommended stable field size n_field =
#      10,080 (a clean multiple of 672,336 / 667 = 1,008).
#   2. A per-player additive EV attribution for one real scraped draft pod,
#      eyeballed against the team EV (per-player EVs must sum to it).
#
# Run from the package root with `devtools::load_all()` already on the path,
# or `Rscript -e 'devtools::load_all(); source("inst/scripts/smoke_bbm7_ev.R")'`.

picks <- load_scraped_drafts()
pool  <- load_slate_data("nfl_2026_season")
targets <- compute_field_targets(picks, slate_id = "nfl_2026_season")
tcfg <- load_tournament("bbm7")

# Clean multiple guardrail: snap the requested size to a money-conservation-
# valid one (derived from config, not hardcoded).
n_field <- resolve_bbm7_field_size(tcfg, 10080L)
cat("n_field (snapped):", n_field, " base:", bbm7_field_multiple(tcfg), "\n")

field <- generate_field("nfl_2026_season", picks = picks, player_pool = pool,
                        targets = targets, n_teams = n_field, seed = 1L)
field_rosters <- split(field$rosters$underdog_id, field$rosters$entry_id)

# Layer A draws via the blender + simulate_slate (slate-shared, run once).
sources_path <- file.path("inst", "data", "sources", "_manifest.yaml")
slates_path  <- file.path("inst", "data", "slates",  "_manifest.yaml")
feed <- blend_slate(slate_id = "nfl_2026_season",
                    sources_manifest_path = sources_path,
                    slates_manifest_path  = slates_path,
                    write_json = FALSE)
sim <- simulate_slate(feed, n_sims = 1000L, seed = 1L)
layerA <- sim$draws

# Positions + schedule from the enriched feed.
positions <- vapply(names(feed$players),
                    function(uid) feed$players[[uid]]$position %||% NA_character_,
                    character(1))
names(positions) <- names(feed$players)
positions <- positions[!is.na(positions)]

sched_chunks <- list()
for (uid in names(feed$players)) {
  pl <- feed$players[[uid]]
  for (wk in pl$weekly %||% list()) {
    w <- as.integer(wk$week %||% NA_integer_); if (is.na(w)) next
    sched_chunks[[length(sched_chunks)+1L]] <- data.frame(
      underdog_id = uid, week = w, team = pl$team %||% NA_character_,
      opponent = if (isTRUE(wk$is_bye)) NA_character_ else (wk$opponent %||% NA_character_),
      is_bye = isTRUE(wk$is_bye), stringsAsFactors = FALSE)
  }
}
schedule <- do.call(rbind, sched_chunks)

lineup_spec <- load_slate_lineup_spec("nfl_2026_season")

# 1. Score the field at every stage + money conservation.
cat("Scoring field (", n_field, " teams)...\n", sep = "")
t0 <- Sys.time()
field_scores <- simulate_per_stage_scores(
  rosters = field_rosters, positions = positions,
  layerA_draws = layerA, schedule = schedule,
  lineup_spec = lineup_spec, n_sims = 500L, seed = 1L
)
cat(sprintf("  wall %.1fs\n", as.numeric(difftime(Sys.time(), t0, units="secs"))))

cat("Computing field payouts...\n")
t0 <- Sys.time()
fp <- build_bbm7_field_payouts(field_scores, tcfg, seed = 1L)
cat(sprintf("  wall %.1fs\n", as.numeric(difftime(Sys.time(), t0, units="secs"))))

expected <- 15000010 / tcfg$total_field_size
cat("\n=== Money conservation (n_field = ", n_field, ") ===\n", sep = "")
cat("Field-mean total EV (gross): $", round(fp$field_mean_total_ev, 2), "\n", sep="")
cat("Expected (prize_pool / full_field): $", round(expected, 2), "\n", sep="")
cat("Gap: $", round(fp$field_mean_total_ev - expected, 2),
    sprintf("  (%.2f%%)\n", 100 * abs(fp$field_mean_total_ev - expected) / expected),
    sep = "")
cat("Net of $25 entry: $", round(fp$field_mean_total_ev - tcfg$entry_fee_usd, 2), "\n", sep="")

# Cache the field artifacts so per-player exploration can be re-run without
# re-scoring the 10,080-team field.
saveRDS(list(fp = fp, positions = positions, schedule = schedule,
             lineup_spec = lineup_spec),
        file.path(tempdir(), "bbm7_headline_cache.rds"))

# 2. Per-player attribution for one real scraped BBM7 draft pod. Pick a pod
# with full feed coverage, evaluate every entry, and show the strongest team
# (a representative eyeball, not a near-empty roster).
cat("\n=== Per-player EV attribution (one real draft pod) ===\n")
bridge <- bestballBroSim:::.scraper_to_feed_id_map(picks, feed)
picks$feed_uid <- bridge[picks$underdog_id]
cov <- tapply(picks$feed_uid, picks$draft_id,
              function(v) mean(!is.na(v)))
draft_pick <- names(sort(cov[cov >= 0.95], decreasing = TRUE))[1L]
if (is.na(draft_pick)) draft_pick <- names(which.max(cov))
pod_rows <- picks[picks$draft_id == draft_pick & !is.na(picks$feed_uid), ]
pod_rosters <- lapply(split(pod_rows$feed_uid, pod_rows$draft_entry_id), unique)
cat(sprintf("draft %s: %d entries, roster sizes %s\n", draft_pick,
            length(pod_rosters),
            paste(range(lengths(pod_rosters)), collapse = "-")))

# Evaluate every entry in the pod against the same field cache; show the best.
evals <- lapply(names(pod_rosters), function(eid) {
  compute_team_bbm7_ev(
    pod_rosters = pod_rosters, team_entry_id = eid,
    positions = positions, layerA_draws = layerA,
    schedule = schedule, lineup_spec = lineup_spec,
    tournament_cfg = tcfg, field_cache = fp, n_sims = 1000L, seed = 1L)
})
names(evals) <- names(pod_rosters)
team_evs <- vapply(evals, function(e) e$team_ev$total_ev, numeric(1))
cat("Pod team EVs ($): ",
    paste(sprintf("%.2f", sort(team_evs, decreasing = TRUE)), collapse = ", "),
    "\n", sep = "")
eid <- names(which.max(team_evs))
res <- evals[[eid]]

cat("\nShowing entry ", eid, " -- Team EV: $", round(res$team_ev$total_ev, 2),
    "  (qual $", round(res$team_ev$qualifier_round_ev, 2),
    " + champ $", round(res$team_ev$championship_round_ev, 2), ")\n", sep = "")
pp <- res$per_player_ev
pp$name <- vapply(pp$underdog_id,
                  function(u) feed$players[[u]]$name %||% u, character(1))
pp$pos  <- positions[pp$underdog_id]
print(pp[, c("name", "pos", "qualifier_round_ev", "championship_round_ev", "total_ev")],
      row.names = FALSE)
cat("\nSum of per-player total EV: $", round(sum(pp$total_ev), 4),
    "  (team EV: $", round(res$team_ev$total_ev, 4), ")\n", sep = "")
cat("Metric: ", attr(pp, "metric"), "\n", sep = "")
