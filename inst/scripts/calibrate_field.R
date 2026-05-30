# Calibrate stack_alpha + pos_alpha to land the four validation
# numbers in the right bands. Throwaway tuning script.

picks <- load_scraped_drafts()
pool  <- load_slate_data("nfl_2026_season")
targets <- compute_field_targets(picks, slate_id = "nfl_2026_season")
cat("Empirical targets:\n")
cat(sprintf("  Position means: QB=%.2f RB=%.2f WR=%.2f TE=%.2f\n",
            targets$position_means["QB"],
            targets$position_means["RB"],
            targets$position_means["WR"],
            targets$position_means["TE"]))
cat(sprintf("  qb_stack_2plus_rate: %.3f\n", targets$qb_stack_2plus_rate))

run <- function(stack_alpha, pos_alpha, n_teams = 2000L, seed = 1L) {
  t0 <- Sys.time()
  out <- generate_field(
    slate_id    = "nfl_2026_season",
    picks       = picks,
    player_pool = pool,
    targets     = targets,
    n_teams     = n_teams,
    seed        = seed,
    stack_alpha = stack_alpha,
    pos_alpha   = pos_alpha
  )
  wall <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  per_team <- aggregate(
    underdog_id ~ entry_id + position,
    data = out$rosters,
    FUN = length
  )
  names(per_team)[3] <- "n"
  pos_means <- tapply(per_team$n, per_team$position, mean)

  # Stack rate: per entry, does it have a QB and >=1 same-team WR/TE?
  stack_flag <- tapply(seq_len(nrow(out$rosters)), out$rosters$entry_id,
    function(idx) {
      r <- out$rosters[idx, , drop = FALSE]
      qbs <- r[r$position == "QB", "team_abbr"]
      pcs <- r[r$position %in% c("WR", "TE"), "team_abbr"]
      any(pcs %in% qbs)
    }
  )
  cat(sprintf(
    "stack_alpha=%-4s pos_alpha=%-4s | QB=%.2f RB=%.2f WR=%.2f TE=%.2f | stack2+=%.3f | %.1fs\n",
    stack_alpha, pos_alpha,
    pos_means["QB"], pos_means["RB"], pos_means["WR"], pos_means["TE"],
    mean(stack_flag), wall
  ))
  invisible(list(pos_means = pos_means, stack_rate = mean(stack_flag)))
}

cat("\nPost-fix sweep (stack pressure now drops after first partner):\n")
for (stack_alpha in c(4.0, 8.0, 15.0, 25.0, 40.0)) {
  for (pos_alpha in c(1.0, 2.0, 4.0)) {
    run(stack_alpha, pos_alpha)
  }
}
