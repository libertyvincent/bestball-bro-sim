# Measure the field_targets digest inputs from the CURRENT load_scraped_drafts
# source (whatever .default_field_corpus_path resolves). Run once with the
# remap line in load_scraped_drafts disabled, once enabled, to isolate the
# CB->WR effect on the new boards corpus. Read-only.
suppressMessages(devtools::load_all(".", quiet = TRUE))
picks <- load_scraped_drafts()
cat(sprintf("corpus n_picks: %d\n", nrow(picks)))
cat("non-QRWT position_name (as returned by load_scraped_drafts):\n")
stray <- picks[!picks$position_name %in% c("QB","RB","WR","TE"), ]
print(table(stray$position_name))
if (nrow(stray)) print(unique(paste(stray$first_name, stray$last_name, "|", stray$position_name)))

tb <- compute_field_targets(picks, slate_id = "nfl_2026_season")
cat("\nposition_means:\n")
for (p in c("QB","RB","WR","TE")) cat(sprintf("  %-2s %.6f\n", p, tb$position_means[[p]]))
cat(sprintf("qb_stack_2plus_rate: %.6f\n", tb$qb_stack_2plus_rate))
cat(sprintf("n finite slot_adp_sd: %d   sum: %.6f\n",
            sum(is.finite(tb$slot_adp_sd)), sum(tb$slot_adp_sd[is.finite(tb$slot_adp_sd)])))
cat("DONE.\n")
