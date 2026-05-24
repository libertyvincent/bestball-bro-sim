#' Identify rookies inside a slate via the "no historical stats" gate
#'
#' v1 / slate-architecture identification: the slate CSV is the canonical
#' player universe, so we only ever flag people who actually appear in the
#' slate. Within that universe, a "rookie" is any skill player whose name
#' (with team + position) does not match a row in `historical_stats_df`
#' (typically `pull_player_stats(season - 5 : season - 1)`).
#'
#' Why stats-absence rather than `entry_year`: in the offseason
#' `nflreadr::load_rosters()` lags badly (the freshly-drafted class is on
#' team rosters but their `entry_year`/`gsis_id` are still NA), and
#' `load_draft_picks()` returns PFR-placeholder IDs that don't link back
#' to anything we can join on. The slate CSV sidesteps both problems
#' because Underdog has already curated the player set and assigned them
#' stable UUIDs.
#'
#' Known limitation: a player who was drafted in a prior year but logged
#' zero NFL stats (cut, IR all year — e.g. Jonathan Brooks 2024) will be
#' classified as a rookie here and projected via the comparables module
#' rather than as a returning veteran. Acceptable for v1; revisit if the
#' false-rookie population grows.
#'
#' Matching is two-tier: (1) `(normalized full name, team_abbr,
#' position)` exact match, (2) for slate rows still unmatched,
#' `(normalized full name, position)` when that combination is unique
#' across history (catches players whose team changed since their last
#' season — they're not rookies).
#'
#' @param slate_df Slate player universe from [load_slate_data()]. Must
#'   include `underdog_id`, `full_name`, `team_abbr`, `position`.
#' @param historical_stats_df Weekly player stats from
#'   [pull_player_stats()]. Must include `player_id`, `position`, and a
#'   name column (`player_display_name` or `player_name`).
#' @return The rookie subset of `slate_df` — skill positions only,
#'   deduped on `underdog_id`.
#' @keywords internal
.identify_rookies <- function(slate_df, historical_stats_df) {
  skill <- c("QB", "RB", "WR", "TE")
  s <- slate_df[slate_df$position %in% skill &
                  !is.na(slate_df$position) &
                  !is.na(slate_df$underdog_id), , drop = FALSE]
  s <- s[!duplicated(s$underdog_id), , drop = FALSE]
  if (nrow(s) == 0L) return(s)

  matched_ids <- .match_slate_to_history(s, historical_stats_df)
  s[is.na(matched_ids), , drop = FALSE]
}

#' Match slate players to historical-stats player_ids
#'
#' Two-tier match: tier 1 is (norm name, team_abbr, position); tier 2 is
#' (norm name, position) restricted to unambiguous keys. Returns a
#' character vector of `player_id` (gsis_id) aligned to rows of
#' `slate_df`; NA where no match.
#'
#' Exposed (internal) so the slate projection orchestrator can use the
#' same lookup to pair veterans to their history.
#'
#' @keywords internal
.match_slate_to_history <- function(slate_df, historical_stats_df) {
  hist <- historical_stats_df
  if (is.null(hist) || nrow(hist) == 0L) {
    return(rep(NA_character_, nrow(slate_df)))
  }

  name_col <- if ("player_display_name" %in% names(hist)) "player_display_name"
              else if ("player_name"        %in% names(hist)) "player_name"
              else NA_character_
  if (is.na(name_col)) {
    return(rep(NA_character_, nrow(slate_df)))
  }
  team_col <- if ("recent_team" %in% names(hist)) "recent_team"
              else if ("team"   %in% names(hist)) "team"
              else NA_character_

  hist <- hist[!is.na(hist$player_id) &
                 !is.na(hist[[name_col]]) &
                 !is.na(hist$position), , drop = FALSE]
  if (nrow(hist) == 0L) return(rep(NA_character_, nrow(slate_df)))

  # One row per player: their most-recent observed (team, position).
  hist <- hist[order(-as.integer(hist$season)), , drop = FALSE]
  hist <- hist[!duplicated(hist$player_id), , drop = FALSE]

  hist_norm_name <- .norm_name(hist[[name_col]])
  hist_team_abbr <- if (!is.na(team_col)) .nflverse_team(hist[[team_col]])
                    else rep(NA_character_, nrow(hist))

  slate_norm_name <- .norm_name(slate_df$full_name)

  key_tier1_hist  <- paste(hist_norm_name,  hist_team_abbr,     hist$position, sep = "|")
  key_tier1_slate <- paste(slate_norm_name, slate_df$team_abbr, slate_df$position, sep = "|")
  out <- hist$player_id[match(key_tier1_slate, key_tier1_hist)]

  unmatched <- which(is.na(out))
  if (length(unmatched) > 0L) {
    key_tier2_hist  <- paste(hist_norm_name,  hist$position,     sep = "|")
    key_tier2_slate <- paste(slate_norm_name, slate_df$position, sep = "|")
    counts <- table(key_tier2_hist)
    unique_keys <- names(counts)[counts == 1L]
    for (i in unmatched) {
      k <- key_tier2_slate[i]
      if (k %in% unique_keys) {
        idx <- which(key_tier2_hist == k)[1]
        out[i] <- hist$player_id[idx]
      }
    }
  }

  out
}

#' Project slate rookies via historical-draft-capital comparables
#'
#' Layer A v1 module — slate-aware. For each rookie row in `rookies_df`,
#' looks up their draft round in `drafts_df` (by normalized name +
#' nflverse team abbreviation + position), bins by `(position,
#' draft_capital)`, and projects from the empirical distribution of
#' historical comparables' rookie-year season totals.
#'
#' Bins: `"1st"`, `"2nd-3rd"`, `"4th-5th"`, `"6th-7th"`, `"UDFA"`.
#' If a `(position, bin)` cell has fewer than 10 historical comparables
#' it falls back to a position-only sample and emits a single batched
#' warning listing the bins that fell back.
#'
#' Drafted comparables who never logged a regular-season game in their
#' rookie year (cut, IR all season, practice squad) are included with
#' `season_total = 0` — honest signal about the bin's NFL stickiness.
#'
#' Current-year UDFAs always trigger the position-only fallback (the
#' comparable table only contains drafted players). Acceptable for v1;
#' revisit in v1.5+ if UDFA projections look materially off.
#'
#' @param rookies_df Slate rookies (one row each). Required columns:
#'   `underdog_id`, `full_name`, `team_abbr`, `position`. Caller filters
#'   to skill positions and dedupes — this function applies the same
#'   filter defensively.
#' @param drafts_df Draft picks from [pull_draft_picks()]. Must include
#'   `season`, `round`, `position`, `pfr_player_name`, `team`. Covers
#'   the current year (for round lookup) plus the historical comparable
#'   window.
#' @param historical_stats_df Weekly player stats from
#'   [pull_player_stats()] — the same data frame the veteran pipeline
#'   uses.
#' @param scoring_cfg `bbbro_scoring` object from
#'   [load_scoring_config()].
#' @param season Current NFL season (the rookies' year), e.g. `2026`.
#' @return Data frame: `underdog_id`, `full_name`, `team_abbr`,
#'   `position`, `season_mean`, `season_std`, `season_p10..p95`.
#'   Rank and VOR are computed downstream by the slate orchestrator so
#'   rookies and veterans rank against the same in-slate pool.
#' @export
project_rookies <- function(rookies_df, drafts_df, historical_stats_df,
                            scoring_cfg, season) {
  skill <- c("QB", "RB", "WR", "TE")
  rookies_df <- rookies_df[rookies_df$position %in% skill &
                             !is.na(rookies_df$underdog_id), , drop = FALSE]
  rookies_df <- rookies_df[!duplicated(rookies_df$underdog_id), , drop = FALSE]
  if (nrow(rookies_df) == 0L) return(.empty_rookie_proj_df())

  comp <- .build_rookie_comparables(drafts_df, historical_stats_df,
                                    scoring_cfg, season)

  current_draft <- drafts_df[!is.na(drafts_df$season) &
                               drafts_df$season == season, , drop = FALSE]
  cd_keys <- character(0)
  if (nrow(current_draft) > 0L) {
    cd_keys <- paste(.norm_name(current_draft$pfr_player_name),
                     .nflverse_team(current_draft$team),
                     current_draft$position, sep = "|")
  }

  fallback_seen <- character(0)
  out_rows <- vector("list", nrow(rookies_df))

  for (i in seq_len(nrow(rookies_df))) {
    r <- rookies_df[i, ]
    key <- paste(.norm_name(r$full_name), r$team_abbr, r$position, sep = "|")
    round <- NA_integer_
    if (length(cd_keys) > 0L) {
      hit <- which(cd_keys == key)
      if (length(hit) > 0L) round <- as.integer(current_draft$round[hit[1]])
    }

    bin <- .draft_capital_bin(round)

    samples <- comp$season_total[comp$position == r$position &
                                   comp$bin == bin]
    if (length(samples) < 10L) {
      combo <- paste0(r$position, "/", bin)
      if (!(combo %in% fallback_seen)) {
        fallback_seen <- c(fallback_seen, combo)
      }
      samples <- comp$season_total[comp$position == r$position]
    }

    stats_row <- .rookie_row_stats(samples)
    out_rows[[i]] <- cbind(
      data.frame(
        underdog_id = r$underdog_id,
        full_name   = r$full_name,
        team_abbr   = r$team_abbr,
        position    = r$position,
        stringsAsFactors = FALSE
      ),
      stats_row
    )
  }

  if (length(fallback_seen) > 0L) {
    cli::cli_warn(c(
      "Rookie comparable bin(s) below 10-sample floor; used position-only fallback:",
      i = "{paste(fallback_seen, collapse = ', ')}"
    ))
  }

  do.call(rbind, out_rows)
}

#' Build the rookie comparables table
#'
#' For every drafted player whose draft year falls in the historical
#' window, joins to their rookie-year regular-season fantasy points. A
#' draftee who never appeared in player_stats their rookie year gets
#' `season_total = 0`.
#'
#' @keywords internal
.build_rookie_comparables <- function(drafts_df, historical_stats_df,
                                      scoring_cfg, season) {
  skill <- c("QB", "RB", "WR", "TE")
  hist <- historical_stats_df
  hist <- hist[hist$position %in% skill & !is.na(hist$player_id), ,
               drop = FALSE]
  if ("season_type" %in% names(hist)) {
    hist <- hist[is.na(hist$season_type) | hist$season_type == "REG", ,
                 drop = FALSE]
  } else if ("week" %in% names(hist)) {
    hist <- hist[is.na(hist$week) | hist$week <= 18L, , drop = FALSE]
  }
  if (nrow(hist) == 0L) return(.empty_comp_df())
  hist$fp <- compute_fantasy_points(hist, scoring_cfg)
  hist_seasons <- sort(unique(hist$season))

  comp_draft <- drafts_df[
    drafts_df$season %in% hist_seasons &
      drafts_df$season < season &
      drafts_df$position %in% skill &
      !is.na(drafts_df$gsis_id), , drop = FALSE]
  if (nrow(comp_draft) == 0L) return(.empty_comp_df())

  per_player_season <- aggregate(fp ~ player_id + season,
                                 data = hist, FUN = sum)
  names(per_player_season) <- c("gsis_id", "season", "season_total")

  joined <- merge(
    comp_draft[, c("gsis_id", "season", "position", "round")],
    per_player_season,
    by = c("gsis_id", "season"), all.x = TRUE, sort = FALSE)
  joined$season_total[is.na(joined$season_total)] <- 0
  joined$bin <- vapply(joined$round, .draft_capital_bin, character(1))

  data.frame(
    gsis_id      = joined$gsis_id,
    position     = joined$position,
    bin          = joined$bin,
    season_total = joined$season_total,
    stringsAsFactors = FALSE
  )
}

#' Map a draft round to a capital bin
#'
#' Returns one of `"1st"`, `"2nd-3rd"`, `"4th-5th"`, `"6th-7th"`, `"UDFA"`.
#' Vectorize via `vapply()`; this function takes a scalar.
#'
#' @keywords internal
.draft_capital_bin <- function(round) {
  if (length(round) == 0L || is.na(round)) return("UDFA")
  r <- as.integer(round)
  if (r == 1L)            "1st"
  else if (r %in% 2:3)    "2nd-3rd"
  else if (r %in% 4:5)    "4th-5th"
  else if (r %in% 6:7)    "6th-7th"
  else                    "UDFA"
}

#' Empirical stats for a rookie's comparable sample
#'
#' Returns a one-row data frame with the projection numerics only — the
#' caller cbinds identifying columns (`underdog_id`, `full_name`, etc.).
#'
#' Empirical quantiles via `stats::quantile(type = 7)`; `season_mean` is
#' the sample median (matches `season_p50`). Empty samples yield a zero
#' row (defensive — should not happen with the position-only fallback in
#' place).
#'
#' @keywords internal
.rookie_row_stats <- function(samples) {
  if (length(samples) == 0L) {
    return(data.frame(
      season_mean   = 0, season_std = 0,
      season_p10    = 0, season_p25 = 0, season_p50 = 0,
      season_p75    = 0, season_p90 = 0, season_p95 = 0,
      stringsAsFactors = FALSE
    ))
  }
  q <- stats::quantile(samples,
                       probs = c(0.10, 0.25, 0.50, 0.75, 0.90, 0.95),
                       type = 7, names = FALSE, na.rm = TRUE)
  std_val <- if (length(samples) >= 2L) stats::sd(samples) else 0
  data.frame(
    season_mean   = stats::median(samples, na.rm = TRUE),
    season_std    = std_val,
    season_p10    = q[1], season_p25 = q[2], season_p50 = q[3],
    season_p75    = q[4], season_p90 = q[5], season_p95 = q[6],
    stringsAsFactors = FALSE
  )
}

#' Empty rookie-projection data frame (shape only) — used when there are no rookies
#' @keywords internal
.empty_rookie_proj_df <- function() {
  data.frame(
    underdog_id = character(0),
    full_name   = character(0),
    team_abbr   = character(0),
    position    = character(0),
    season_mean = numeric(0), season_std = numeric(0),
    season_p10  = numeric(0), season_p25 = numeric(0), season_p50 = numeric(0),
    season_p75  = numeric(0), season_p90 = numeric(0), season_p95 = numeric(0),
    stringsAsFactors = FALSE
  )
}

#' Empty comparable table (shape only)
#' @keywords internal
.empty_comp_df <- function() {
  data.frame(
    gsis_id = character(0), position = character(0),
    bin = character(0), season_total = numeric(0),
    stringsAsFactors = FALSE
  )
}

#' Normalize a player name for fuzzy joining
#'
#' Lowercases, strips apostrophes / periods / smart-quotes, swaps hyphens
#' for spaces, drops trailing Jr/Sr/II/III/IV/V suffixes, collapses runs
#' of whitespace. Vectorized.
#'
#' @keywords internal
.norm_name <- function(x) {
  if (length(x) == 0L) return(character(0))
  y <- tolower(x)
  y <- gsub("[`'’.]", "", y)
  y <- gsub("-", " ", y, fixed = TRUE)
  y <- gsub("\\s+(jr|sr|ii|iii|iv|v)$", "", y, perl = TRUE)
  y <- gsub("\\s+", " ", y)
  trimws(y)
}

#' Normalize NFL team codes to the nflverse roster convention
#'
#' Vectorized. `load_draft_picks()` uses legacy three-letter codes
#' (`ARI`, `LAR`, `GNB`, `KAN`, `NOR`, `NWE`, `LVR`, `SFO`, `TAM`); rosters
#' and player_stats use the modern two-letter / shortened codes (`AZ`,
#' `LA`, `GB`, `KC`, `NO`, `NE`, `LV`, `SF`, `TB`). Everything else passes
#' through unchanged.
#'
#' @keywords internal
.nflverse_team <- function(team) {
  if (length(team) == 0L) return(character(0))
  map <- c(ARI = "AZ", KAN = "KC", GNB = "GB", NWE = "NE", NOR = "NO",
           LVR = "LV", LAR = "LA", SFO = "SF", TAM = "TB", JAC = "JAX",
           OAK = "LV", SD = "LAC", STL = "LA")
  ifelse(team %in% names(map), unname(map[team]), team)
}
