# Smoke run: synthetic small-BBM7 field to verify money conservation.

# 1. Generate small field via 3b-6.
picks <- load_scraped_drafts()
pool  <- load_slate_data("nfl_2026_season")
targets <- compute_field_targets(picks, slate_id = "nfl_2026_season")

n_field <- 1200L   # 100 qualifier pods of 12
field <- generate_field("nfl_2026_season", picks = picks, player_pool = pool,
                        targets = targets, n_teams = n_field, seed = 1L)
field_rosters <- split(field$rosters$underdog_id, field$rosters$entry_id)

# 2. Layer A draws via the blender + simulate_slate (slate-shared, run once).
sources_path <- file.path("inst", "data", "sources", "_manifest.yaml")
slates_path  <- file.path("inst", "data", "slates",  "_manifest.yaml")
feed <- blend_slate(slate_id = "nfl_2026_season",
                    sources_manifest_path = sources_path,
                    slates_manifest_path  = slates_path,
                    write_json = FALSE)
sim <- simulate_slate(feed, n_sims = 1000L, seed = 1L)
layerA <- sim$draws

# 3. Positions + schedule from the enriched feed.
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
tcfg <- load_tournament("bbm7")

# 4. Score the field at every stage.
cat("Scoring field (", n_field, " teams)...\n", sep = "")
t0 <- Sys.time()
field_scores <- simulate_per_stage_scores(
  rosters = field_rosters, positions = positions,
  layerA_draws = layerA, schedule = schedule,
  lineup_spec = lineup_spec, n_sims = 500L, seed = 1L
)
cat(sprintf("  wall %.1fs\n", as.numeric(difftime(Sys.time(), t0, units="secs"))))

# 5. Compute field payouts.
cat("Computing field payouts...\n")
t0 <- Sys.time()
fp <- build_bbm7_field_payouts(field_scores, tcfg, seed = 1L)
cat(sprintf("  wall %.1fs\n", as.numeric(difftime(Sys.time(), t0, units="secs"))))

cat("\nField-mean total EV: $", round(fp$field_mean_total_ev, 2), "\n", sep="")
expected <- 15000010 / tcfg$total_field_size
cat("Expected (prize_pool / full_field): $", round(expected, 2), "\n", sep="")
cat("Money conservation gap: $",
    round(fp$field_mean_total_ev - expected, 2), "\n", sep="")
