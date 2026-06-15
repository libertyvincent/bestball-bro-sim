# Derive the sim-side opponent drafter table: P(pos | round) for the Season
# slate, opponent-only (owner excluded), on the privacy-stripped field corpus.
# This is the SIM-side benchmark the field-construction validation measures
# against; it is regenerated (report-only) by the corpus-refresh automation
# (bestball-bro-data/scripts/refresh_corpus.py). It never writes the installed
# *extension* table -- that benchmark move is human-routed.
#
# Promoted from build/ (gitignored scratch) to the tracked inst/scripts/ so the
# refresh pipeline's cross-repo dependency is versioned and reproducible.
#
# Usage (run from the repo root so devtools::load_all(".") finds the package):
#   Rscript inst/scripts/derive_drafter_table.R [corpus_boards.json]
# With no arg, resolves the newest committed boards_*.json via the canonical
# resolver. Prints the per-round table plus exact INTEGRAL / ROUND_SUM lines.
suppressMessages(devtools::load_all(".", quiet=TRUE))
SEASON <- "a9c04e81-1ace-4b16-a31d-4c725a47f16f"; OWNER <- "208206fa9882"
# Corpus path: first CLI arg, else the newest committed boards_*.json (the
# stripped-corpus superset) via the canonical resolver -- so the table follows
# the current corpus rather than a pinned date.
.args <- commandArgs(trailingOnly = TRUE)
corpus <- if (length(.args) >= 1L) .args[[1L]] else .default_field_corpus_path()
picks <- load_scraped_drafts(corpus)
s <- picks[picks$slate_id == SEASON, ]
cat(sprintf("corpus: %s\n", corpus))
cat(sprintf("Season drafts (a9c04e81, completed): %d\n", length(unique(s$draft_id))))
opp <- s[s$drafter_user_id != OWNER & s$position_name %in% c("QB","RB","WR","TE"), ]
cat(sprintf("opponent picks (QB/RB/WR/TE): %d  | owner picks excluded: %d\n",
            nrow(opp), sum(s$drafter_user_id == OWNER)))
# P(pos | round): rows = round 1..18, cols = QB/RB/WR/TE (fractions)
tab <- prop.table(table(round = opp$round, pos = factor(opp$position_name, c("QB","RB","WR","TE"))), margin = 1)
cat("\nP(pos | round) — opponent-only, deduped combined Season (newest superset corpus):\n")
cat(sprintf("%-5s %6s %6s %6s %6s %6s\n","rd","QB","RB","WR","TE","n"))
n_by_round <- table(opp$round)
for (r in as.character(1:18)) {
  if (!r %in% rownames(tab)) next
  cat(sprintf("%-5s %6.3f %6.3f %6.3f %6.3f %6d\n", r,
              tab[r,"QB"], tab[r,"RB"], tab[r,"WR"], tab[r,"TE"], n_by_round[r]))
}
# Integral = expected count per opponent team = colSums of the per-round
# conditional table (each round's P(pos|round) sums to 1 by construction).
# Printed at full precision so downstream tooling reads exact values.
integral <- colSums(tab)
cat(sprintf("\nINTEGRAL QB=%.6f RB=%.6f WR=%.6f TE=%.6f SUM=%.6f\n",
            integral[["QB"]], integral[["RB"]], integral[["WR"]], integral[["TE"]],
            sum(integral)))
cat(sprintf("ROUND_SUM_MAXDEV %.3e (n_rounds=%d)\n",
            max(abs(rowSums(tab) - 1)), nrow(tab)))
