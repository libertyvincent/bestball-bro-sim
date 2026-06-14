# Read-only preview of the CB->WR / FB->RB source remap, pre-edit.
# Computes the before/after the remap WOULD produce, without changing any
# shipped file. Two data sources:
#   (1) udbb-scraper-latest.json  -> field_targets digest (compute_field_targets)
#   (2) boards_2026-06-13.json    -> drafter-table integral
suppressMessages(devtools::load_all(".", quiet = TRUE))

remap <- function(df) { df$position_name <- .normalize_fantasy_position(df$position_name); df }

cat("=================== (1) field_targets source: udbb-scraper-latest ===================\n")
picks <- load_scraped_drafts()
cat(sprintf("total picks: %d\n", nrow(picks)))
stray <- picks[!picks$position_name %in% c("QB","RB","WR","TE"), ]
cat("non-QRWT position_name counts (raw):\n"); print(table(stray$position_name))
cat("affected players (name|pos):\n")
if (nrow(stray)) print(unique(paste(stray$first_name, stray$last_name, "|", stray$position_name)))

tb_raw <- compute_field_targets(picks, slate_id = "nfl_2026_season")
tb_new <- compute_field_targets(remap(picks), slate_id = "nfl_2026_season")
cat("\nposition_means  RAW -> REMAP (delta):\n")
for (p in c("QB","RB","WR","TE"))
  cat(sprintf("  %-2s  %.6f -> %.6f  (%+.6f)\n", p,
              tb_raw$position_means[[p]], tb_new$position_means[[p]],
              tb_new$position_means[[p]] - tb_raw$position_means[[p]]))
cat(sprintf("qb_stack_2plus_rate  %.6f -> %.6f  (%+.6f)\n",
            tb_raw$qb_stack_2plus_rate, tb_new$qb_stack_2plus_rate,
            tb_new$qb_stack_2plus_rate - tb_raw$qb_stack_2plus_rate))
cat(sprintf("slot_adp_sd identical? %s\n",
            isTRUE(all.equal(tb_raw$slot_adp_sd, tb_new$slot_adp_sd))))

cat("\n=================== (2) drafter-table source: boards_2026-06-13 ===================\n")
SEASON <- "a9c04e81-1ace-4b16-a31d-4c725a47f16f"; OWNER <- "208206fa9882"
bp <- load_scraped_drafts("../bestball-bro-data/sources/field/boards_2026-06-13.json")
b  <- bp[bp$slate_id == SEASON & bp$drafter_user_id != OWNER, ]
strayb <- b[!b$position_name %in% c("QB","RB","WR","TE"), ]
cat("non-QRWT position_name counts (raw, opponent Season):\n"); print(table(strayb$position_name))
if (nrow(strayb)) print(unique(paste(strayb$first_name, strayb$last_name, "|", strayb$position_name)))

integral <- function(df) {
  d <- df[df$position_name %in% c("QB","RB","WR","TE"), ]
  tab <- prop.table(table(round = d$round,
                          pos = factor(d$position_name, c("QB","RB","WR","TE"))), margin = 1)
  colSums(tab)
}
ir <- integral(b); inew <- integral(remap(b))
cat("\ndrafter-table integral  RAW -> REMAP (delta):\n")
for (p in c("QB","RB","WR","TE"))
  cat(sprintf("  %-2s  %.4f -> %.4f  (%+.4f)\n", p, ir[[p]], inew[[p]], inew[[p]] - ir[[p]]))
cat("DONE.\n")
