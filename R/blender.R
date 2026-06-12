#' Blend source projections for a single slate
#'
#' Layer A v2 entry point. Consumes the normalized source feeds
#' published to gh-pages by bestball-bro-data, weights them per
#' [inst/data/sources/_manifest.yaml], converts rank-only sources
#' (ETR, LegUp) to point-equivalents via a Clay-fit calibration curve,
#' and emits a single v2 projection JSON for the slate.
#'
#' Sprint 2 scope: cross-source consensus mean, analytical disagreement
#' std, position-CV aleatoric std, per-week distribution derived from
#' Clay's weekly team scoring. Monte Carlo + draws.parquet come in
#' Sprint 2.5.
#'
#' @param slate_id Slate identifier from `inst/data/slates/_manifest.yaml`.
#' @param sources_manifest_path Path to sources/_manifest.yaml.
#' @param slates_manifest_path  Path to slates/_manifest.yaml.
#' @param out_path Output path for `<slate_id>.json`.
#' @param cache_dir HTTP cache directory; default `~/.bestball-bro/cache`.
#' @param write_json If `TRUE` (default), writes the feed to `out_path`.
#'   `publish_v2()` sets this to `FALSE` so the simulator can enrich the
#'   feed (replacing analytical percentiles with empirical ones) before
#'   the single final write.
#' @return Invisibly returns the feed list (post-jsonlite shape).
#' @export
blend_slate <- function(slate_id,
                        sources_manifest_path,
                        slates_manifest_path,
                        out_path,
                        cache_dir = file.path("~", ".bestball-bro", "cache"),
                        write_json = TRUE) {

  cli::cli_alert_info("blender [{slate_id}]: starting")

  sources  <- .load_sources_manifest(sources_manifest_path)
  slate    <- load_slate_data(slate_id)
  base_url <- sources$data_repo_base_url

  per_source_weights <- sources$slate_weights[[slate_id]] %||% list()
  if (length(per_source_weights) == 0L) {
    cli::cli_abort(c(
      "No slate_weights entry for {.val {slate_id}}.",
      i = "Add an entry under `slate_weights:` in sources/_manifest.yaml."
    ))
  }

  # --- Fetch sources -------------------------------------------------------
  clay_offense        <- .fetch_clay_offense(sources, base_url, cache_dir)
  clay_weekly_team    <- .fetch_clay_weekly_team_scoring(sources, base_url, cache_dir)
  etr_rows            <- .fetch_etr_for_slate(sources,  base_url, slate_id, cache_dir)
  legup_rows          <- .fetch_legup_for_slate(sources, base_url, slate_id, cache_dir)

  # --- Position-level availability (draw-level game-zeroing) ---------------
  # Availability is NOT scaled into the points here (PR #34's mean-level
  # mechanism is retired). Clay's points -- and therefore the calibration
  # curve and the ETR/LegUp calibrated values -- stay UNSCALED (conditional-
  # on-playing). The prior only sets a per-player miss rate, stamped onto each
  # feed record as `availability_p_miss`, that the Layer A stat recompute
  # (R/simulate.R) and the tensor build (R/ev_blocks.R) apply as a draw-level
  # mask. See DRAW_ZEROING_DESIGN.md and R/availability.R.
  avail_prior <- .load_availability_prior()

  # --- Build Clay's calibration curve (on UNSCALED points) -----------------
  cli::cli_alert("Building Clay calibration curves")
  curves <- build_calibration_curves(clay_offense$players)

  # --- Per-source player tables, all keyed by underdog_id ------------------
  clay_rows  <- .clay_rows_for_slate(clay_offense, slate)
  etr_rows   <- .annotate_pos_rank(etr_rows,   rank_col = "overall_rank")
  legup_rows <- .annotate_pos_rank(legup_rows, rank_col = "overall_rank")

  cli::cli_alert("Clay rows matched to slate: {nrow(clay_rows)}")
  cli::cli_alert("ETR rows in slate universe: {nrow(etr_rows)}")
  cli::cli_alert("LegUp rows in slate universe: {nrow(legup_rows)}")

  n_opp_norm <- attr(clay_weekly_team, "opponent_normalizations") %||% 0L
  cli::cli_alert(
    "blender [{slate_id}]: opponent normalization applied to {n_opp_norm} rows")

  audit <- .write_match_audit(slate_id, slate, etr_rows, legup_rows, clay_rows)
  cli::cli_alert("Audit: {audit$count} slate players in ETR/LegUp but not Clay -> {.path {audit$audit_path}}")

  # --- Per-player blend ----------------------------------------------------
  cli::cli_alert("Blending across sources")
  players_out <- .blend_each_player(
    slate         = slate,
    clay          = clay_rows,
    etr           = etr_rows,
    legup         = legup_rows,
    curves        = curves,
    weights       = per_source_weights,
    aleatoric_cv  = sources$aleatoric_cv,
    weekly_team   = clay_weekly_team,
    avail_prior   = avail_prior
  )

  # --- Within-slate position_rank, VOR, tier ------------------------------
  # Replacement ranks are derived from this slate's starting lineup so
  # that e.g. superflex values QBs against a ~2-starters-per-team
  # baseline instead of the standard 1-QB baseline.
  lineup_spec <- load_slate_lineup_spec(slate_id)
  players_out <- .add_position_metrics(
    players_out,
    replacement_ranks = .replacement_ranks_from_lineup(lineup_spec)
  )

  # --- Brooks-class review list (no auto-correction) ----------------------
  # Extreme per-player overshoots the position factor intentionally does NOT
  # fix (torn ACLs, buried handcuffs): surfaced for manual `adjustments:`.
  review <- .write_availability_review(slate_id, players_out)
  cli::cli_alert(
    "Availability review: {review$count} Brooks-class outlier(s) -> {.path {review$path}}")

  # --- Write JSON ----------------------------------------------------------
  feed <- .build_feed(
    slate_id       = slate_id,
    sources_used   = names(per_source_weights),
    source_weights = per_source_weights,
    aleatoric_cv   = sources$aleatoric_cv,
    players        = players_out,
    availability   = .availability_mechanism_marker(avail_prior)
  )

  if (isTRUE(write_json)) {
    .write_projection_feed(feed, out_path)
  }
  invisible(feed)
}

#' Write a v2 projection feed to JSON
#'
#' Factored out of `blend_slate()` so `publish_v2()` can defer the write
#' until after the simulator has enriched percentiles.
#' @keywords internal
.write_projection_feed <- function(feed, out_path) {
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(feed, out_path, auto_unbox = TRUE, pretty = TRUE,
                       null = "null", na = "null")
  cli::cli_alert_success(
    "Wrote {length(feed$players)} players to {.path {out_path}}")
  invisible(out_path)
}

# ============================================================================
# Manifest loading
# ============================================================================

#' @keywords internal
.load_sources_manifest <- function(path) {
  if (path == "" || !file.exists(path)) {
    cli::cli_abort("sources manifest not found at {.path {path}}")
  }
  yaml::read_yaml(path)
}

# ============================================================================
# HTTP fetching with a tiny per-URL cache
# ============================================================================

#' Fetch a source feed (JSON), caching the parsed result by URL hash
#'
#' Cache filename = sha256(url) under `cache_dir`. Hit -> return cached
#' parse. Miss -> GET via httr2, write to cache, return parse. To force a
#' refresh, delete the cache file (or pass `force_refresh = TRUE`).
#'
#' @keywords internal
.fetch_source_feed <- function(url, cache_dir, force_refresh = FALSE) {
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  key  <- digest::digest(url, algo = "sha256", serialize = FALSE)
  path <- file.path(cache_dir, paste0(key, ".json"))

  if (!force_refresh && file.exists(path)) {
    return(jsonlite::fromJSON(path, simplifyVector = FALSE))
  }

  cli::cli_alert("Fetching {url}")
  req  <- httr2::request(url) |> httr2::req_timeout(60)
  resp <- httr2::req_perform(req)
  body <- httr2::resp_body_string(resp)
  writeLines(body, path, useBytes = TRUE)
  jsonlite::fromJSON(body, simplifyVector = FALSE)
}

# ============================================================================
# Source-specific fetchers -> uniform row shape
# ============================================================================

#' @keywords internal
.fetch_clay_offense <- function(sources, base_url, cache_dir) {
  if (!isTRUE(sources$sources$clay$enabled)) return(NULL)
  url <- paste0(base_url, "/", sources$sources$clay$feed_url)
  .fetch_source_feed(url, cache_dir)
}

#' @keywords internal
.fetch_clay_weekly_team_scoring <- function(sources, base_url, cache_dir) {
  if (!isTRUE(sources$sources$clay$enabled)) return(NULL)
  url <- paste0(base_url, "/", sources$sources$clay$weekly_team_scoring_url)
  raw <- .fetch_source_feed(url, cache_dir)
  if (is.null(raw$teams)) return(raw)

  # Top-level team keys -> nflverse convention (ARI -> AZ, LAR -> LA, ...)
  names(raw$teams) <- vapply(names(raw$teams), .normalize_team_abbr,
                              character(1))

  # Per-week opponent codes -> same normalization. Count rows that
  # actually changed so the blender can log the deviation explicitly.
  norm_count <- 0L
  for (t in names(raw$teams)) {
    weeks_data <- raw$teams[[t]]$weeks
    if (is.null(weeks_data)) next
    for (i in seq_along(weeks_data)) {
      raw_opp <- weeks_data[[i]]$opponent
      if (is.null(raw_opp) || is.na(raw_opp)) next
      norm_opp <- .normalize_team_abbr(raw_opp)
      if (!identical(norm_opp, raw_opp)) {
        raw$teams[[t]]$weeks[[i]]$opponent <- norm_opp
        norm_count <- norm_count + 1L
      }
    }
  }
  attr(raw, "opponent_normalizations") <- norm_count
  raw
}

#' @keywords internal
.fetch_etr_for_slate <- function(sources, base_url, slate_id, cache_dir) {
  src <- sources$sources$etr
  if (!isTRUE(src$enabled)) return(.empty_source_rows())
  feed_url <- src$feeds[[slate_id]]
  if (is.null(feed_url)) {
    cli::cli_alert_warning("ETR has no feed for slate {.val {slate_id}}; skipping")
    return(.empty_source_rows())
  }
  raw <- .fetch_source_feed(paste0(base_url, "/", feed_url), cache_dir)
  rows <- raw$players %||% list()
  if (length(rows) == 0L) return(.empty_source_rows())

  data.frame(
    underdog_id  = vapply(rows, function(r) r$underdog_id %||% NA_character_,
                          character(1)),
    name         = vapply(rows, function(r) r$player_name %||% NA_character_,
                          character(1)),
    team         = vapply(rows, function(r) .normalize_team_abbr(r$team %||% NA_character_),
                          character(1)),
    position     = vapply(rows, function(r) r$position %||% NA_character_,
                          character(1)),
    overall_rank = vapply(rows, function(r) as.integer(r$etr_rank %||% NA),
                          integer(1)),
    pos_rank     = NA_integer_,  # filled in .annotate_pos_rank
    points       = NA_real_,
    stringsAsFactors = FALSE
  )
}

#' @keywords internal
.fetch_legup_for_slate <- function(sources, base_url, slate_id, cache_dir) {
  src <- sources$sources$legup
  if (!isTRUE(src$enabled)) return(.empty_source_rows())
  feed_url <- src$feeds[[slate_id]]
  if (is.null(feed_url)) {
    cli::cli_alert_warning("LegUp has no feed for slate {.val {slate_id}}; skipping")
    return(.empty_source_rows())
  }
  raw <- .fetch_source_feed(paste0(base_url, "/", feed_url), cache_dir)
  rows <- raw$players %||% list()
  if (length(rows) == 0L) return(.empty_source_rows())

  data.frame(
    underdog_id  = vapply(rows, function(r) r$underdog_id %||% NA_character_,
                          character(1)),
    name         = vapply(rows, function(r) r$player_name %||% NA_character_,
                          character(1)),
    team         = vapply(rows, function(r) .normalize_team_abbr(r$team %||% NA_character_),
                          character(1)),
    position     = vapply(rows, function(r) r$position %||% NA_character_,
                          character(1)),
    overall_rank = vapply(rows, function(r) as.integer(r$legup_rank %||% NA),
                          integer(1)),
    pos_rank     = vapply(rows, function(r) {
                          v <- r$legup_pos_rank
                          if (is.null(v) || is.na(v)) NA_integer_ else as.integer(v)
                        }, integer(1)),
    points       = NA_real_,
    stringsAsFactors = FALSE
  )
}

#' @keywords internal
.empty_source_rows <- function() {
  data.frame(
    underdog_id  = character(0),
    name         = character(0),
    team         = character(0),
    position     = character(0),
    overall_rank = integer(0),
    pos_rank     = integer(0),
    points       = numeric(0),
    stringsAsFactors = FALSE
  )
}

#' For each row whose pos_rank is NA, derive it from overall_rank within
#' position (rows already sorted by overall_rank get sequential ranks).
#' @keywords internal
.annotate_pos_rank <- function(df, rank_col) {
  if (nrow(df) == 0L) return(df)
  missing_pr <- is.na(df$pos_rank)
  if (!any(missing_pr)) return(df)

  # Within each position, sort by overall_rank ascending; the kth row
  # gets pos_rank k. Rows with overall_rank=NA fall to the bottom.
  for (pos in unique(df$position)) {
    idx <- which(df$position == pos & missing_pr)
    if (!length(idx)) next
    o <- order(df[[rank_col]][idx])
    df$pos_rank[idx[o]] <- seq_along(idx)
  }
  df
}

# ============================================================================
# Clay matching: name+team+pos -> slate underdog_id
# ============================================================================

#' Build a data.frame of Clay rows keyed by slate underdog_id
#'
#' Clay has no Underdog UUID. We match by normalized name + nflverse team
#' abbreviation + position, then carry the slate's underdog_id forward.
#' Slate rows that don't match a Clay row are simply absent from the
#' returned frame.
#' @keywords internal
.clay_rows_for_slate <- function(clay_offense, slate) {
  rows <- clay_offense$players %||% list()
  if (length(rows) == 0L) return(.empty_source_rows())

  clay_df <- data.frame(
    name         = vapply(rows, function(r) r$name %||% NA_character_, character(1)),
    team         = vapply(rows, function(r) .normalize_team_abbr(r$team %||% NA_character_),
                          character(1)),
    position     = vapply(rows, function(r) r$position %||% NA_character_, character(1)),
    overall_rank = vapply(rows, function(r) as.integer(r$rank_overall %||% NA), integer(1)),
    pos_rank     = vapply(rows, function(r) as.integer(r$rank_position %||% NA), integer(1)),
    points       = vapply(rows, function(r) as.numeric(r$projected_points_half_ppr %||% NA),
                          numeric(1)),
    games        = vapply(rows, function(r) as.numeric(r$games %||% NA), numeric(1)),
    stringsAsFactors = FALSE
  )

  clay_df$match_key <- paste(.normalize_name(clay_df$name),
                             clay_df$team, clay_df$position, sep = "|")
  slate_key         <- paste(.normalize_name(slate$full_name),
                             slate$team_abbr, slate$position, sep = "|")

  m <- match(slate_key, clay_df$match_key)
  matched <- !is.na(m)
  data.frame(
    underdog_id  = slate$underdog_id[matched],
    name         = clay_df$name[m[matched]],
    team         = clay_df$team[m[matched]],
    position     = clay_df$position[m[matched]],
    overall_rank = clay_df$overall_rank[m[matched]],
    pos_rank     = clay_df$pos_rank[m[matched]],
    points       = clay_df$points[m[matched]],
    games        = clay_df$games[m[matched]],
    stringsAsFactors = FALSE
  )
}

# ============================================================================
# Normalization helpers
# ============================================================================
# `.normalize_name()` and `.NAME_ALIASES` were consolidated into
# R/player_match.R so the blender's Clay join and the scraper -> feed
# bridge share one canonical implementation. The blender still calls
# `.normalize_name()` here; it's the same function, just in a shared file.

#' Normalize a team abbreviation to nflverse convention
#'
#' Source feeds use AP/ESPN conventions (`ARI`, `LAR`); the package's
#' canonical form is the nflverse roster convention (`AZ`, `LA`).
#' Validates the result against the 32-team allowlist; unknown values
#' return NA (caller decides whether to abort).
#' @keywords internal
.normalize_team_abbr <- function(abbr) {
  if (length(abbr) == 0L) return(character(0))
  if (length(abbr) > 1L) {
    return(vapply(abbr, .normalize_team_abbr, character(1),
                  USE.NAMES = FALSE))
  }
  if (is.na(abbr) || is.null(abbr)) return(NA_character_)
  fix <- c(ARI = "AZ", LAR = "LA", JAC = "JAX",
           WSH = "WAS", CLV = "CLE", BLT = "BAL",
           ARZ = "AZ",  HST = "HOU", SD  = "LAC")
  if (abbr %in% names(fix)) abbr <- unname(fix[abbr])
  canonical <- c("AZ","ATL","BAL","BUF","CAR","CHI","CIN","CLE","DAL","DEN",
                 "DET","GB","HOU","IND","JAX","KC","LV","LAC","LA","MIA",
                 "MIN","NE","NO","NYG","NYJ","PHI","PIT","SF","SEA","TB",
                 "TEN","WAS")
  if (abbr %in% canonical) abbr else NA_character_
}

# ============================================================================
# Per-player blend
# ============================================================================

#' @keywords internal
.blend_each_player <- function(slate, clay, etr, legup,
                                curves, weights, aleatoric_cv, weekly_team,
                                avail_prior = list(enabled = FALSE,
                                                   expected_games = list())) {
  skill <- c("QB", "RB", "WR", "TE")
  # Slate restricted to skill positions with a usable underdog_id
  s <- slate[slate$position %in% skill & !is.na(slate$underdog_id), ,
             drop = FALSE]
  s <- s[!duplicated(s$underdog_id), , drop = FALSE]

  # Index source rows by underdog_id for O(1) lookup (match() returns
  # NA for missing keys rather than erroring as `[[` would).
  clay_match  <- match(s$underdog_id, clay$underdog_id)
  etr_match   <- match(s$underdog_id, etr$underdog_id)
  legup_match <- match(s$underdog_id, legup$underdog_id)

  out <- vector("list", nrow(s))
  for (i in seq_len(nrow(s))) {
    p     <- s[i, ]
    ud    <- p$underdog_id
    pos   <- p$position
    team  <- p$team_abbr  # already nflverse convention
    src_breakdown <- list()
    points_by_src <- list()
    weights_by_src <- list()

    # --- Clay ----
    ci <- clay_match[i]
    clay_games_i <- if (!is.na(ci)) clay$games[ci] else NA_real_
    if (!is.na(ci)) {
      pts <- clay$points[ci]
      if (!is.na(pts) && !is.null(weights$clay)) {
        points_by_src$clay  <- pts
        weights_by_src$clay <- weights$clay
        src_breakdown$clay  <- list(
          raw_points        = round(pts, 1),
          weight            = NA_real_  # set below post-renorm
        )
      }
    }

    # --- ETR ----
    ei <- etr_match[i]
    if (!is.na(ei) && !is.null(weights$etr)) {
      pr <- etr$pos_rank[ei]
      src_pos <- etr$position[ei]
      if (!is.na(pr) && !is.na(src_pos) && src_pos %in% skill) {
        calibrated <- as.numeric(calibrate_rank_to_points(pr, src_pos, curves))
        points_by_src$etr  <- calibrated
        weights_by_src$etr <- weights$etr
        src_breakdown$etr  <- list(
          raw_rank          = as.integer(etr$overall_rank[ei]),
          raw_pos_rank      = as.integer(pr),
          calibrated_points = round(calibrated, 1),
          weight            = NA_real_
        )
      }
    }

    # --- LegUp ----
    li <- legup_match[i]
    if (!is.na(li) && !is.null(weights$legup)) {
      pr <- legup$pos_rank[li]
      src_pos <- legup$position[li]
      if (!is.na(pr) && !is.na(src_pos) && src_pos %in% skill) {
        calibrated <- as.numeric(calibrate_rank_to_points(pr, src_pos, curves))
        points_by_src$legup  <- calibrated
        weights_by_src$legup <- weights$legup
        src_breakdown$legup  <- list(
          raw_rank          = as.integer(legup$overall_rank[li]),
          raw_pos_rank      = as.integer(pr),
          calibrated_points = round(calibrated, 1),
          weight            = NA_real_
        )
      }
    }

    consensus <- .compute_consensus(points_by_src, weights_by_src)

    # Stamp renormalized weights back into source_breakdown for debug.
    for (nm in names(src_breakdown)) {
      src_breakdown[[nm]]$weight <- round(consensus$norm_weights[[nm]], 3)
    }

    cv <- as.numeric(aleatoric_cv[[pos]] %||% NA_real_)
    if (is.null(consensus$mean) || is.na(consensus$mean)) {
      season_mean      <- NA_real_
      season_std       <- NA_real_
      disagreement_std <- NA_real_
      aleatoric_std    <- NA_real_
      weekly           <- list()
    } else {
      season_mean <- consensus$mean
      # Compute weekly first; per-week std = cv * weekly_mean. Season
      # aleatoric variance = sum(weekly_std^2) over active weeks.
      wk <- .generate_weekly(season_mean, team, weekly_team, cv)
      disagreement_std <- consensus$disagreement_std
      aleatoric_std    <- sqrt(wk$aleatoric_season_var)
      season_std       <- sqrt(disagreement_std^2 + aleatoric_std^2)
      weekly           <- wk$records
    }

    # Per-player draw-zeroing miss rate (carries PR #34's clamp). Missing
    # clay_games defaults to a full 17-game season -> canonical 1 - prior/17.
    p_miss <- .availability_p_miss(clay_games_i,
                                   avail_prior$expected_games[[pos]],
                                   avail_prior$enabled)

    out[[i]] <- list(
      underdog_id                = ud,
      name                       = p$full_name,
      team                       = team,
      position                   = pos,
      availability_p_miss        = round(p_miss, 4),
      underdog_projected_points  = if (is.na(p$projected_points)) NULL
                                   else round(p$projected_points, 1),
      season_mean                = if (is.na(season_mean)) NULL
                                   else round(season_mean, 1),
      season_std                 = if (is.na(season_std)) NULL
                                   else round(season_std, 1),
      # Components of season_std exposed for the Monte Carlo simulator
      # (two-level draw: disagreement_std outer, aleatoric inner) and
      # for debuggability. Invariant: season_std^2 == disagreement_std^2
      # + aleatoric_std^2.
      disagreement_std           = if (is.na(disagreement_std)) NULL
                                   else round(disagreement_std, 2),
      aleatoric_std              = if (is.na(aleatoric_std)) NULL
                                   else round(aleatoric_std, 2),
      season_percentiles         = if (is.na(season_mean)) NULL
                                   else .season_percentiles(season_mean, season_std),
      sources_used               = names(points_by_src),
      source_breakdown           = src_breakdown,
      weekly                     = weekly,
      adp                        = if (is.na(p$adp)) NULL else p$adp
    )
  }
  out
}

#' @keywords internal
.season_percentiles <- function(mu, sd) {
  if (is.na(mu)) return(NULL)
  if (is.na(sd) || sd <= 0) {
    return(list(p10 = round(mu, 1), p25 = round(mu, 1), p50 = round(mu, 1),
                p75 = round(mu, 1), p90 = round(mu, 1)))
  }
  list(
    p10 = round(stats::qnorm(0.10, mu, sd), 1),
    p25 = round(stats::qnorm(0.25, mu, sd), 1),
    p50 = round(mu, 1),
    p75 = round(stats::qnorm(0.75, mu, sd), 1),
    p90 = round(stats::qnorm(0.90, mu, sd), 1)
  )
}

#' @keywords internal
.compute_consensus <- function(points_by_src, weights_by_src) {
  if (length(points_by_src) == 0L) {
    return(list(mean = NA_real_, disagreement_std = NA_real_,
                norm_weights = list()))
  }
  pts <- unlist(points_by_src, use.names = TRUE)
  w   <- unlist(weights_by_src[names(pts)], use.names = TRUE)
  w   <- w / sum(w)  # renormalize over present sources
  mu  <- sum(w * pts)
  # Weighted variance around the mean.
  v   <- sum(w * (pts - mu)^2)
  list(
    mean             = mu,
    disagreement_std = sqrt(v),
    norm_weights     = as.list(w)
  )
}

# ============================================================================
# Weekly distribution from Clay's team scoring
# ============================================================================

#' Generate per-week records + season aleatoric variance
#'
#' For each non-bye week, the player's expected fantasy mean is
#' `(season_mean / active_weeks) * (team_nfl_score[w] / mean(team_nfl_score over active weeks))`.
#' Per-week std is `cv * weekly_mean` (position-default CV from the
#' manifest). The season aleatoric variance is `sum(weekly_std^2)`
#' over active weeks (treats weeks as independent draws), returned so
#' the caller can combine it with cross-source disagreement variance
#' into the total season std.
#'
#' Opponent and home/away come from Clay's per-week record (V -> "away",
#' H -> "home"). Bye weeks return mean = 0 and std = 0.
#' @keywords internal
.generate_weekly <- function(season_mean, team_abbr, weekly_team, cv) {
  empty <- list(records = list(), aleatoric_season_var = 0)
  if (is.null(weekly_team) || is.null(weekly_team$teams) ||
      is.na(team_abbr) || is.null(weekly_team$teams[[team_abbr]])) {
    return(empty)
  }
  weeks_data <- weekly_team$teams[[team_abbr]]$weeks
  if (is.null(weeks_data) || length(weeks_data) == 0L) return(empty)

  scores <- vapply(weeks_data, function(w) as.numeric(w$team_nfl_score %||% NA),
                   numeric(1))
  is_bye <- vapply(weeks_data, function(w) isTRUE(w$is_bye), logical(1))
  active <- !is_bye & !is.na(scores) & scores > 0
  if (!any(active)) return(empty)

  team_avg <- mean(scores[active])
  per_act  <- season_mean / sum(active)
  records  <- vector("list", length(weeks_data))
  weekly_vars <- numeric(length(weeks_data))

  for (i in seq_along(weeks_data)) {
    wd <- weeks_data[[i]]
    if (is_bye[i]) {
      records[[i]] <- list(
        week = as.integer(wd$week %||% i),
        opponent = NULL, home_away = NULL,
        is_bye = TRUE, mean = 0, std = 0,
        percentiles = list(p10 = 0, p50 = 0, p90 = 0)
      )
      next
    }
    mult   <- scores[i] / team_avg
    w_mean <- per_act * mult
    w_std  <- if (is.na(cv) || cv <= 0) 0 else cv * w_mean
    weekly_vars[i] <- w_std^2

    loc <- wd$location %||% NA_character_
    home_away <- if (identical(loc, "V")) "away"
                 else if (identical(loc, "H")) "home"
                 else NA_character_
    opp_raw <- wd$opponent
    records[[i]] <- list(
      week        = as.integer(wd$week %||% i),
      opponent    = if (is.null(opp_raw) || is.na(opp_raw)) NULL else opp_raw,
      home_away   = if (is.na(home_away)) NULL else home_away,
      is_bye      = FALSE,
      mean        = round(w_mean, 2),
      std         = round(w_std, 2),
      percentiles = list(
        p10 = round(stats::qnorm(0.10, w_mean, max(w_std, 1e-6)), 2),
        p50 = round(w_mean, 2),
        p90 = round(stats::qnorm(0.90, w_mean, max(w_std, 1e-6)), 2)
      )
    )
  }
  list(records = records, aleatoric_season_var = sum(weekly_vars))
}

# ============================================================================
# Within-slate position metrics
# ============================================================================

#' Derive per-position VOR replacement ranks from a slate's starting lineup
#'
#' `replacement_rank[pos] = startable_slots[pos] * pod_size`, where
#' startable slots are counted with a deliberate, legible flex-allocation
#' heuristic:
#'
#' * Dedicated slots (QB/RB/WR/TE) count fully toward their position.
#' * A flex slot whose eligible pool includes QB (superflex) counts fully
#'   toward QB -- the second QB is the reason that slot exists, and in
#'   practice it is filled by one.
#' * RB/WR/TE flex slots are NOT allocated to any position. This keeps a
#'   standard 1-QB lineup (QB1/RB2/WR3/TE1/FLEX) at exactly the v1
#'   convention QB12/RB24/WR36/TE12 -- i.e. dedicated starters x pod --
#'   treating thin, spread-out flex demand as already absorbed by those
#'   baselines rather than pushing every flex-eligible position a tier down.
#'
#' This is the lineup-rule version of replacement level; an empirical
#' (field-derived) refinement can replace the heuristic later.
#'
#' @param lineup_spec Lineup spec from [load_slate_lineup_spec()].
#' @param pod_size Teams per draft pod (Underdog best ball default: 12).
#' @return Named integer vector of replacement ranks for QB/RB/WR/TE.
#' @keywords internal
.replacement_ranks_from_lineup <- function(lineup_spec, pod_size = 12L) {
  startable <- c(QB = 0L, RB = 0L, WR = 0L, TE = 0L)
  for (s in lineup_spec$slots) {
    n <- as.integer(s$n)
    if (length(s$eligible) == 1L && s$eligible %in% names(startable)) {
      # Dedicated positional slot
      startable[[s$eligible]] <- startable[[s$eligible]] + n
    } else if ("QB" %in% s$eligible) {
      # Superflex-style slot -> QB
      startable[["QB"]] <- startable[["QB"]] + n
    }
    # else: RB/WR/TE flex -> unallocated (see heuristic above)
  }
  startable * pod_size
}

#' @keywords internal
.add_position_metrics <- function(players_out, replacement_ranks) {
  # Build a flat data.frame for sorting.
  df <- data.frame(
    idx         = seq_along(players_out),
    position    = vapply(players_out, function(p) p$position %||% NA_character_,
                          character(1)),
    season_mean = vapply(players_out, function(p) {
                          v <- p$season_mean
                          if (is.null(v) || is.na(v)) NA_real_ else as.numeric(v)
                        }, numeric(1)),
    stringsAsFactors = FALSE
  )

  # Per-position rank by season_mean (NA pushed to the bottom)
  df$position_rank <- NA_integer_
  for (pos in unique(df$position)) {
    sub <- which(df$position == pos)
    if (!length(sub)) next
    o <- order(-df$season_mean[sub], na.last = TRUE)
    df$position_rank[sub[o]] <- seq_along(sub)
  }

  # Replacement value per position = season_mean at the slate's
  # replacement rank (derived from its starting lineup by
  # .replacement_ranks_from_lineup; standard 1-QB lineups reproduce the
  # v1 convention QB12 / RB24 / WR36 / TE12).
  repl_value <- vapply(names(replacement_ranks), function(p) {
    pts <- sort(df$season_mean[df$position == p], decreasing = TRUE,
                na.last = NA)
    rk  <- replacement_ranks[[p]]
    if (rk >= 1L && length(pts) >= rk) pts[rk] else 0
  }, numeric(1))

  for (i in seq_along(players_out)) {
    pos <- df$position[i]
    pr  <- df$position_rank[i]
    sm  <- df$season_mean[i]

    players_out[[i]]$position_rank <- if (is.na(pr)) NULL else as.integer(pr)
    players_out[[i]]$vor <- if (is.na(sm)) NULL
                            else round(max(0, sm - (repl_value[[pos]] %||% 0)), 1)
    players_out[[i]]$tier <- if (is.na(pr)) NULL else .compute_tier(pr, pos)
  }
  players_out
}

#' Write the slate's blend-match audit: skill players in slate AND in
#' ETR or LegUp BUT not matched by Clay. Empty file (just the header)
#' is fine. Returns count + path so the caller can log.
#' @keywords internal
.write_match_audit <- function(slate_id, slate, etr_rows, legup_rows,
                                clay_rows, out_dir = "build") {
  skill   <- c("QB", "RB", "WR", "TE")
  in_clay  <- !is.na(match(slate$underdog_id, clay_rows$underdog_id))
  in_etr   <- !is.na(match(slate$underdog_id, etr_rows$underdog_id))
  in_legup <- !is.na(match(slate$underdog_id, legup_rows$underdog_id))

  keep <- (in_etr | in_legup) & !in_clay &
    !is.na(slate$position) & slate$position %in% skill

  audit_df <- data.frame(
    underdog_id = slate$underdog_id[keep],
    name        = slate$full_name[keep],
    team        = slate$team_abbr[keep],
    position    = slate$position[keep],
    in_etr      = in_etr[keep],
    in_legup    = in_legup[keep],
    in_clay     = FALSE,
    stringsAsFactors = FALSE
  )

  path <- file.path(out_dir, sprintf("blender_audit_%s.txt", slate_id))
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.table(audit_df, path, sep = "\t", row.names = FALSE,
                     quote = FALSE, fileEncoding = "UTF-8")

  list(count = nrow(audit_df), audit_path = path, audit_df = audit_df)
}

#' Simple position-banded tier (1 = elite, 5 = deep replacement-level).
#' @keywords internal
.compute_tier <- function(pos_rank, position) {
  thresholds <- list(
    QB = c(3L,  9L, 15L, 24L),
    RB = c(6L, 18L, 30L, 48L),
    WR = c(6L, 18L, 36L, 60L),
    TE = c(3L,  8L, 15L, 24L)
  )[[position]]
  if (is.null(thresholds)) return(NA_integer_)
  for (i in seq_along(thresholds)) {
    if (pos_rank <= thresholds[i]) return(i)
  }
  length(thresholds) + 1L
}

# ============================================================================
# Feed assembly
# ============================================================================

#' @keywords internal
.build_feed <- function(slate_id, sources_used, source_weights,
                        aleatoric_cv, players, availability = NULL) {
  by_id <- list()
  for (p in players) by_id[[p$underdog_id]] <- p

  meta <- list(
    slate_id      = slate_id,
    version       = format(Sys.Date(), "%Y-%m-%d"),
    generated_at  = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    methodology   = "blended_consensus_v2",
    sources_used  = sources_used,
    source_weights = source_weights,
    aleatoric_cv  = aleatoric_cv,
    player_count  = length(by_id)
  )
  # Transparency: log the availability MECHANISM marker (draw-zeroing type,
  # priors, canonical per-position miss rates). Replaces PR #34's
  # availability_adjustment scaling summary.
  if (!is.null(availability)) meta$availability_mechanism <- availability

  list(`_meta` = meta, players = by_id)
}
