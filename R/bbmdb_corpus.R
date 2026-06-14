#' Pinned BBMDB validation-corpus snapshot
#'
#' The BBMDB validation corpus is a dated, immutable series -- one parquet per
#' scrape vintage under `.../data/snapshots/bbmdb_<date>.parquet`. The simulator
#' validates against a single *pinned* vintage so that G2 and the test suite are
#' reproducible: pointing at the dated snapshot (never the mutable, undated
#' `bbmdb_teams.parquet`) means the next scrape can no longer silently change
#' what the gate, diagnostics, and tests read.
#'
#' This is the single source of truth for the corpus path. All call sites (the
#' G2 gate, the BBMDB diagnostics, and the gated real-set tests) resolve the
#' path through this function, so **advancing the pin is a one-line edit**:
#' change the default `snapshot` date here and every consumer re-points together
#' -- they can no longer diverge.
#'
#' @param snapshot Snapshot vintage as a `"YYYY-MM-DD"` date string. Defaults to
#'   the pinned vintage `"2026-06-14"` (636 teams; advanced from the original
#'   `"2026-05-28"` baseline of 406 teams).
#' @return Absolute path to `.../data/snapshots/bbmdb_<snapshot>.parquet`.
#' @keywords internal
bbmdb_corpus_path <- function(snapshot = "2026-06-14") {
  file.path(
    "C:/Users/vince/Desktop/bbmdb_scraper/data/snapshots",
    sprintf("bbmdb_%s.parquet", snapshot)
  )
}
