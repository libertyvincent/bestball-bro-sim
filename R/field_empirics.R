#' Load scraped Underdog drafts and resolve picks to player metadata
#'
#' Reads the Chrome-extension scraper export and emits a long-form tibble with
#' one row per pick. Performs the two-hop join required to turn an
#' `appearance_id` (the pick's foreign key) into player metadata:
#'
#'   pick.appearance_id -> appearances[].id -> appearances[].player_id ->
#'   players[].id -> {first_name, last_name, position_name, team_id}
#'
#' Player catalogs (`/v1/slates/<slate_id>/players`) and appearance catalogs
#' (`/v1/slates/<slate_id>/scoring_types/<scoring_type_id>/appearances`) both
#' live in the export's `unkeyed[]` array, keyed by api_endpoint URL. Each
#' unkeyed entry has `raw_response.players[]` or `raw_response.appearances[]`
#' depending on the endpoint.
#'
#' Note: `team_id` lives on the player record, NOT the appearance record. For
#' stacking analysis the join must run through `players[].team_id`. Free
#' agents have `team_id = NULL` and are emitted as `NA_character_` --
#' they contribute to position counts but not to team-stack analyses.
#'
#' Tournament resolution: the scraper hoists `tournament_id`,
#' `tournament_round_id`, and `tournament_title` onto every draft, so this
#' loader trusts those directly. The hoisted `tournament_id` is Underdog's
#' UUID -- a different namespace from Sprint 3a's internal slug returned by
#' [resolve_round_to_tournament()] (e.g. `"bbm7"`). We don't cross-check the
#' two here; Sprint 3b-7 will add a `resolve_underdog_uuid_to_tournament()`
#' bridge in `tournament_loader.R` (backlog item, not in scope here).
#'
#' Weekly Winners note: for slate
#' `d9fd5f58-393f-400c-a010-3bf79b822b48` the scraper hoists the
#' weekly_winner pool's id into the `tournament_id` slot -- it is NOT an
#' actual tournament UUID in the Sprint 3a sense. The empirics code treats
#' it as an opaque grouping key, which is fine; just don't pass it to
#' Sprint 3a's tournament loader expecting a match.
#'
#' `event_type` (`"tournament"` vs `"weekly_winner"`) is pulled from
#' `round_tournament_index` keyed by `tournament_round_id`.
#'
#' @param export_path Path to the scraper export JSON. Defaults to the
#'   canonical local copy.
#' @return A tibble with one row per pick and the columns:
#'   `draft_id, draft_state, slate_id, tournament_id, tournament_round_id,
#'   tournament_title, event_type, drafter_slot, drafter_user_id,
#'   draft_entry_id, round, pick_overall, appearance_id, underdog_id,
#'   first_name, last_name, position_name, team_id, projection_adp_at_pick,
#'   projection_points, actual_points`.
#'   The `underdog_id` column carries the Underdog player UUID (the value
#'   stored on `appearance.player_id` upstream); we standardize on the
#'   `underdog_id` name across the sim repo to match Layer A's draws and
#'   3b-2's correlated output.
#'   Attributes:
#'     - `exported_at` (chr scalar from the export header)
#'     - `n_drafts_input` (total drafts in the export, including skipped)
#'     - `n_drafts_skipped_incomplete` (count filtered out by draft_state)
#'
#' @export
load_scraped_drafts <- function(
  export_path = "inst/data/scraped_drafts/udbb-scraper-latest.json"
) {
  if (!file.exists(export_path)) {
    cli::cli_abort(c(
      "Scraper export not found.",
      i = "Expected: {.path {export_path}}",
      i = "Place the latest Chrome-extension export at that path \\
           (typical name: {.val udbb-scraper-<timestamp>.json}, then copy to \\
           {.val udbb-scraper-latest.json})."
    ))
  }

  export <- jsonlite::fromJSON(export_path, simplifyVector = FALSE)

  required_top <- c("drafts", "unkeyed", "round_tournament_index")
  missing_top <- setdiff(required_top, names(export))
  if (length(missing_top) > 0) {
    cli::cli_abort(c(
      "Scraper export missing required top-level keys: {missing_top}",
      i = "File: {.path {export_path}}"
    ))
  }

  catalogs    <- .build_slate_catalogs(export$unkeyed)
  round_index <- .build_round_event_index(export$round_tournament_index)

  n_input <- length(export$drafts)
  per_draft_rows <- list()
  n_skipped <- 0L

  for (i in seq_along(export$drafts)) {
    draft <- export$drafts[[i]]
    state <- draft$draft_state %||% draft$raw_response$draft$status %||% NA_character_
    if (!identical(state, "completed")) {
      n_skipped <- n_skipped + 1L
      next
    }

    rows <- .pick_rows_for_draft(draft, catalogs, round_index)
    if (!is.null(rows) && nrow(rows) > 0) {
      per_draft_rows[[length(per_draft_rows) + 1]] <- rows
    }
  }

  if (n_skipped > 0) {
    cli::cli_alert_info(
      "Skipped {n_skipped} non-completed draft{?s} (draft_state != \"completed\")"
    )
  }

  out <- if (length(per_draft_rows) == 0) {
    .empty_picks_tibble()
  } else {
    do.call(rbind, per_draft_rows)
  }

  attr(out, "exported_at") <- export$exported_at %||% NA_character_
  attr(out, "n_drafts_input") <- n_input
  attr(out, "n_drafts_skipped_incomplete") <- n_skipped
  out
}

# ---- helpers ----------------------------------------------------------------

#' Build per-slate catalog maps for appearance_id->player_id and
#' player_id->player_meta from the export's `unkeyed[]` array.
#'
#' Returns a list:
#'   list(
#'     app_to_player = list(<slate_id> = list(<appearance_id> = <player_id>)),
#'     player_meta   = list(<slate_id> = list(<player_id> = list(first_name,
#'                                                               last_name,
#'                                                               position_name,
#'                                                               team_id)))
#'   )
#'
#' @keywords internal
.build_slate_catalogs <- function(unkeyed) {
  app_to_player <- list()
  player_meta   <- list()

  re_players <- "/v1/slates/([^/]+)/players$"
  re_apps    <- "/v1/slates/([^/]+)/scoring_types/[^/]+/appearances$"

  for (u in unkeyed) {
    ep <- u$api_endpoint %||% ""
    if (grepl(re_players, ep)) {
      slate_id <- sub(re_players, "\\1", ep)
      players  <- u$raw_response$players %||% list()
      meta_map <- vector("list", length(players))
      keys     <- character(length(players))
      for (i in seq_along(players)) {
        p <- players[[i]]
        keys[i] <- p$id %||% NA_character_
        meta_map[[i]] <- list(
          first_name    = p$first_name %||% NA_character_,
          last_name     = p$last_name  %||% NA_character_,
          position_name = p$position_name %||% NA_character_,
          team_id       = if (is.null(p$team_id)) NA_character_ else as.character(p$team_id)
        )
      }
      names(meta_map) <- keys
      player_meta[[slate_id]] <- meta_map
    } else if (grepl(re_apps, ep)) {
      slate_id <- sub(re_apps, "\\1", ep)
      apps     <- u$raw_response$appearances %||% list()
      ids   <- character(length(apps))
      pids  <- character(length(apps))
      for (i in seq_along(apps)) {
        a <- apps[[i]]
        ids[i]  <- a$id %||% NA_character_
        pids[i] <- a$player_id %||% NA_character_
      }
      m <- as.list(pids)
      names(m) <- ids
      app_to_player[[slate_id]] <- m
    }
  }
  list(app_to_player = app_to_player, player_meta = player_meta)
}

#' Build a round_id -> event_type map from `round_tournament_index`.
#'
#' `round_tournament_index` is a named list keyed by round_id. Each entry
#' has fields including `event_type` (`"tournament"` or `"weekly_winner"`).
#'
#' @keywords internal
.build_round_event_index <- function(round_tournament_index) {
  if (length(round_tournament_index) == 0) return(list())
  out <- lapply(round_tournament_index, function(x) x$event_type %||% NA_character_)
  out
}

#' Convert one draft into a per-pick tibble.
#' @keywords internal
.pick_rows_for_draft <- function(draft, catalogs, round_index) {
  rd <- draft$raw_response$draft
  picks   <- rd$picks %||% list()
  entries <- rd$draft_entries %||% list()
  if (length(picks) == 0 || length(entries) == 0) return(NULL)

  slate_id <- draft$slate_id %||% rd$slate_id %||% NA_character_
  if (is.na(slate_id) || nzchar(slate_id) == FALSE) return(NULL)

  app_map  <- catalogs$app_to_player[[slate_id]] %||% list()
  meta_map <- catalogs$player_meta[[slate_id]]   %||% list()

  entry_lookup <- list()
  for (e in entries) {
    eid <- e$id %||% NA_character_
    if (is.na(eid)) next
    entry_lookup[[eid]] <- list(
      user_id    = e$user_id    %||% NA_character_,
      pick_order = e$pick_order %||% NA_integer_
    )
  }
  n_drafters <- length(entries)

  trid <- draft$tournament_round_id %||% rd$tournament_round_id %||% NA_character_
  event_type <- if (!is.na(trid)) (round_index[[trid]] %||% NA_character_) else NA_character_

  draft_id        <- draft$draft_id        %||% rd$id %||% NA_character_
  draft_state     <- draft$draft_state     %||% rd$status %||% NA_character_
  tournament_id   <- draft$tournament_id   %||% NA_character_
  tournament_ttl  <- draft$tournament_title %||% NA_character_

  n <- length(picks)
  draft_entry_id          <- character(n)
  appearance_id           <- character(n)
  underdog_id             <- character(n)
  first_name              <- character(n)
  last_name               <- character(n)
  position_name           <- character(n)
  team_id                 <- character(n)
  pick_overall            <- integer(n)
  drafter_slot            <- integer(n)
  drafter_user_id         <- character(n)
  projection_adp_at_pick  <- numeric(n)
  projection_points       <- numeric(n)
  actual_points           <- numeric(n)

  for (i in seq_len(n)) {
    p   <- picks[[i]]
    eid <- p$draft_entry_id %||% NA_character_
    aid <- p$appearance_id  %||% NA_character_
    pid <- if (!is.na(aid)) (app_map[[aid]] %||% NA_character_) else NA_character_
    pm  <- if (!is.na(pid)) (meta_map[[pid]] %||% NULL) else NULL
    e   <- if (!is.na(eid)) (entry_lookup[[eid]] %||% NULL) else NULL

    draft_entry_id[i]         <- eid
    appearance_id[i]          <- aid
    underdog_id[i]            <- pid
    first_name[i]             <- if (is.null(pm)) NA_character_ else pm$first_name
    last_name[i]              <- if (is.null(pm)) NA_character_ else pm$last_name
    position_name[i]          <- if (is.null(pm)) NA_character_ else pm$position_name
    team_id[i]                <- if (is.null(pm)) NA_character_ else pm$team_id
    pick_overall[i]           <- as.integer(p$number %||% NA_integer_)
    drafter_slot[i]           <- if (is.null(e)) NA_integer_ else as.integer(e$pick_order)
    drafter_user_id[i]        <- if (is.null(e)) NA_character_ else e$user_id
    projection_adp_at_pick[i] <- suppressWarnings(as.numeric(p$projection_adp    %||% NA))
    projection_points[i]      <- suppressWarnings(as.numeric(p$projection_points %||% NA))
    actual_points[i]          <- suppressWarnings(as.numeric(p$points            %||% NA))
  }

  round <- if (n_drafters > 0) {
    as.integer(ceiling(pick_overall / n_drafters))
  } else {
    rep(NA_integer_, n)
  }

  tibble::tibble(
    draft_id               = draft_id,
    draft_state            = draft_state,
    slate_id               = slate_id,
    tournament_id          = tournament_id,
    tournament_round_id    = trid,
    tournament_title       = tournament_ttl,
    event_type             = event_type,
    drafter_slot           = drafter_slot,
    drafter_user_id        = drafter_user_id,
    draft_entry_id         = draft_entry_id,
    round                  = round,
    pick_overall           = pick_overall,
    appearance_id          = appearance_id,
    underdog_id            = underdog_id,
    first_name             = first_name,
    last_name              = last_name,
    position_name          = position_name,
    team_id                = team_id,
    projection_adp_at_pick = projection_adp_at_pick,
    projection_points      = projection_points,
    actual_points          = actual_points
  )
}

#' Positions kept by the empirics functions.
#'
#' Underdog's player catalog occasionally classifies a handful of players as
#' `CB`/`FB` and they get drafted in some leagues. Every slate we support
#' starts only QB/RB/WR/TE, so these stray rows are real picks of real
#' players but irrelevant for field-team construction. Filtered out at the
#' top of [empirical_position_counts()] and [empirical_stack_patterns()];
#' kept on the raw `picks` tibble for users who want to inspect them.
#' @keywords internal
.FIELD_EMPIRIC_POSITIONS <- c("QB", "RB", "WR", "TE")

#' Per-team position counts
#'
#' For each (slate, drafter), counts roster slots by position, then
#' aggregates to a frequency table: "how many teams in the sample rostered
#' exactly N at position P?". Intended for sampling synthetic field teams.
#'
#' Only QB/RB/WR/TE rows are counted; see [.FIELD_EMPIRIC_POSITIONS].
#'
#' @param picks Output of [load_scraped_drafts()].
#' @param stratify_by Grouping columns. Default `c("slate_id")`. Other useful
#'   values: `c("slate_id", "tournament_id")`, `c("slate_id", "event_type")`.
#' @return tibble with columns: all `stratify_by` cols, plus `position`,
#'   `count`, `n_teams_with_count`. Example row: `(slate, RB, 4, 12)` means
#'   12 teams in the sample rostered exactly 4 RBs.
#' @export
empirical_position_counts <- function(picks, stratify_by = c("slate_id")) {
  .check_picks(picks)
  .check_stratify_cols(picks, stratify_by)

  picks <- picks[picks$position_name %in% .FIELD_EMPIRIC_POSITIONS, , drop = FALSE]

  group_cols <- unique(c(stratify_by, "draft_id", "drafter_user_id",
                         "position_name"))
  per_team <- as.data.frame(
    dplyr::summarise(
      dplyr::group_by(picks, dplyr::across(dplyr::all_of(group_cols))),
      count = dplyr::n(),
      .groups = "drop"
    )
  )

  agg_cols <- unique(c(stratify_by, "position_name", "count"))
  agg <- as.data.frame(
    dplyr::summarise(
      dplyr::group_by(per_team, dplyr::across(dplyr::all_of(agg_cols))),
      n_teams_with_count = dplyr::n(),
      .groups = "drop"
    )
  )

  tibble::as_tibble(
    dplyr::arrange(
      dplyr::rename(agg, position = "position_name"),
      dplyr::across(dplyr::all_of(stratify_by)),
      .data$position,
      .data$count
    )
  )
}

#' Stack patterns per team
#'
#' For each (slate, drafter, draft), computes:
#'   - `max_team_stack_depth`: the largest number of players the drafter rostered
#'      from any single NFL team (free agents excluded).
#'   - `has_qb_stack_2plus/3plus/4plus`: whether the drafter rostered a QB plus
#'      1/2/3+ pass-catchers (WR or TE) from the SAME NFL team. The "N" in the
#'      column name counts the QB itself, so `2plus` = QB + 1 PC, `3plus` =
#'      QB + 2 PCs, etc.
#'   - `n_team_stacks_3plus`: count of distinct NFL teams from which the
#'      drafter rostered 3+ players (any positions).
#'
#' Only QB/RB/WR/TE rows are considered; see [.FIELD_EMPIRIC_POSITIONS].
#' Players with `team_id = NA` (free agents) are excluded from the stack
#' joins -- they contribute to position counts but not to team-stack metrics.
#'
#' Playoff-week game stacks (drafter rostered players from BOTH sides of an
#' NFL game in weeks 15-17) need schedule data and are deferred to Sprint
#' 3b-6 when the field model needs them.
#'
#' @param picks Output of [load_scraped_drafts()].
#' @param stratify_by Grouping columns. Default `c("slate_id")`.
#' @return tibble with one row per (slate, draft, drafter), plus columns:
#'   `draft_id, drafter_user_id, max_team_stack_depth, has_qb_stack_2plus,
#'   has_qb_stack_3plus, has_qb_stack_4plus, n_team_stacks_3plus`.
#' @export
empirical_stack_patterns <- function(picks, stratify_by = c("slate_id")) {
  .check_picks(picks)
  .check_stratify_cols(picks, stratify_by)
  required_extra <- c("team_id", "draft_id", "drafter_user_id", "position_name")
  missing <- setdiff(required_extra, colnames(picks))
  if (length(missing) > 0) {
    cli::cli_abort("`picks` missing columns for stack analysis: {missing}")
  }

  picks <- picks[picks$position_name %in% .FIELD_EMPIRIC_POSITIONS, , drop = FALSE]

  team_key_cols <- unique(c(stratify_by, "draft_id", "drafter_user_id"))
  all_teams <- dplyr::distinct(picks, dplyr::across(dplyr::all_of(team_key_cols)))

  with_team <- picks[!is.na(picks$team_id) & nzchar(picks$team_id), , drop = FALSE]

  per_drafter_team <- as.data.frame(
    dplyr::summarise(
      dplyr::group_by(
        with_team,
        dplyr::across(dplyr::all_of(c(team_key_cols, "team_id")))
      ),
      n_players = dplyr::n(),
      n_qb      = sum(.data$position_name == "QB"),
      n_pc      = sum(.data$position_name %in% c("WR", "TE")),
      .groups   = "drop"
    )
  )

  per_drafter <- as.data.frame(
    dplyr::summarise(
      dplyr::group_by(per_drafter_team,
                      dplyr::across(dplyr::all_of(team_key_cols))),
      max_team_stack_depth = max(.data$n_players),
      has_qb_stack_2plus   = any(.data$n_qb >= 1 & .data$n_pc >= 1),
      has_qb_stack_3plus   = any(.data$n_qb >= 1 & .data$n_pc >= 2),
      has_qb_stack_4plus   = any(.data$n_qb >= 1 & .data$n_pc >= 3),
      n_team_stacks_3plus  = sum(.data$n_players >= 3),
      .groups = "drop"
    )
  )

  # Re-attach teams that had no players with team_id (extremely rare but
  # possible if an entire roster is free agents). Default zeros.
  out <- dplyr::left_join(all_teams, per_drafter, by = team_key_cols)
  out$max_team_stack_depth <- tidyr::replace_na(out$max_team_stack_depth, 0L)
  out$has_qb_stack_2plus   <- tidyr::replace_na(out$has_qb_stack_2plus,   FALSE)
  out$has_qb_stack_3plus   <- tidyr::replace_na(out$has_qb_stack_3plus,   FALSE)
  out$has_qb_stack_4plus   <- tidyr::replace_na(out$has_qb_stack_4plus,   FALSE)
  out$n_team_stacks_3plus  <- tidyr::replace_na(out$n_team_stacks_3plus,  0L)
  tibble::as_tibble(out)
}

#' ADP variance per pick slot
#'
#' For each (slate, `pick_overall`), shows the distribution of players who
#' actually got drafted across the captured drafts. Used by Sprint 3b-6 to
#' model "given pick N, what players might the field take with what
#' probability?".
#'
#' Uses pick-level `projection_adp_at_pick`, not slate-level appearance ADP.
#' The pick-time signal is what the field actually responded to at that
#' moment.
#'
#' @param picks Output of [load_scraped_drafts()].
#' @param stratify_by Grouping columns. Default `c("slate_id")`.
#' @return tibble: all `stratify_by` cols + `pick_overall, underdog_id,
#'   first_name, last_name, position_name, n_times_drafted,
#'   mean_adp_at_pick, sd_adp_at_pick`.
#' @export
empirical_pick_distributions <- function(picks, stratify_by = c("slate_id")) {
  .check_picks(picks)
  .check_stratify_cols(picks, stratify_by)

  group_cols <- unique(c(stratify_by, "pick_overall", "underdog_id",
                         "first_name", "last_name", "position_name"))
  out <- as.data.frame(
    dplyr::summarise(
      dplyr::group_by(picks, dplyr::across(dplyr::all_of(group_cols))),
      n_times_drafted  = dplyr::n(),
      mean_adp_at_pick = mean(.data$projection_adp_at_pick, na.rm = TRUE),
      sd_adp_at_pick   = stats::sd(.data$projection_adp_at_pick, na.rm = TRUE),
      .groups = "drop"
    )
  )
  tibble::as_tibble(
    dplyr::arrange(out,
                   dplyr::across(dplyr::all_of(stratify_by)),
                   .data$pick_overall,
                   dplyr::desc(.data$n_times_drafted))
  )
}

#' Drafter-level summary stats
#'
#' One row per (slate, draft, drafter). Useful for quick diagnostics: who
#' drafted how many of each position and what their stack signature looks
#' like.
#'
#' @param picks Output of [load_scraped_drafts()].
#' @return tibble: `slate_id, draft_id, drafter_user_id, drafter_slot, n_qb,
#'   n_rb, n_wr, n_te, max_team_stack_depth, has_qb_stack_2plus,
#'   has_qb_stack_3plus, has_qb_stack_4plus, n_team_stacks_3plus`.
#' @export
drafter_team_summary <- function(picks) {
  .check_picks(picks)
  pos_filtered <- picks[picks$position_name %in% .FIELD_EMPIRIC_POSITIONS, ,
                        drop = FALSE]

  per_team <- as.data.frame(
    dplyr::summarise(
      dplyr::group_by(pos_filtered, .data$slate_id, .data$draft_id,
                      .data$drafter_user_id, .data$drafter_slot),
      n_qb = sum(.data$position_name == "QB"),
      n_rb = sum(.data$position_name == "RB"),
      n_wr = sum(.data$position_name == "WR"),
      n_te = sum(.data$position_name == "TE"),
      .groups = "drop"
    )
  )

  stacks <- empirical_stack_patterns(picks)
  out <- dplyr::left_join(per_team, stacks,
    by = c("slate_id", "draft_id", "drafter_user_id"))
  tibble::as_tibble(out)
}

#' @keywords internal
.check_picks <- function(picks) {
  if (!is.data.frame(picks)) {
    cli::cli_abort("`picks` must be a data frame (output of load_scraped_drafts()).")
  }
  required <- c("draft_id", "drafter_user_id", "position_name", "slate_id")
  missing  <- setdiff(required, colnames(picks))
  if (length(missing) > 0) {
    cli::cli_abort("`picks` is missing required columns: {missing}")
  }
}

#' @keywords internal
.check_stratify_cols <- function(picks, stratify_by) {
  missing <- setdiff(stratify_by, colnames(picks))
  if (length(missing) > 0) {
    cli::cli_abort(c(
      "`stratify_by` references unknown column(s): {missing}",
      i = "Available columns: {colnames(picks)}"
    ))
  }
}

#' Compute and persist all field empirics as a JSON feed
#'
#' Reads the scraped drafts export, computes all empirical distributions,
#' writes one JSON file per slate to
#' `<output_dir>/<slate_id>.json`.
#'
#' Schema per file:
#' \preformatted{
#' {
#'   "slate_id": "...",
#'   "computed_at": "2026-...",
#'   "source_export_captured_at": "2026-...",
#'   "n_drafts_sampled": 20,
#'   "n_teams_sampled": 240,
#'   "n_tournament_unique": 5,
#'   "by_event_type": {
#'     "tournament":    { "n_drafts": 20, "n_teams": 240 },
#'     "weekly_winner": { "n_drafts": 0,  "n_teams": 0   }
#'   },
#'   "position_counts": {
#'     "QB": { "1": 3, "2": 98, "3": 133, "4": 6 },
#'     "RB": { ... }, ...
#'   },
#'   "stack_patterns": {
#'     "mean_max_team_stack_depth": 3.05,
#'     "qb_stack_2plus_rate": 0.91,
#'     "qb_stack_3plus_rate": 0.56,
#'     "qb_stack_4plus_rate": 0.08,
#'     "mean_n_team_stacks_3plus": 1.21
#'   },
#'   "pick_distributions": [
#'     {"pick_overall": 1, "underdog_id": "...", "first_name": "Jahmyr",
#'      "last_name": "Gibbs", "position_name": "RB",
#'      "n_times_drafted": 4, "mean_adp_at_pick": 1.7, "sd_adp_at_pick": 0.2},
#'     ...
#'   ]
#' }
#' }
#'
#' @param export_path Path to the scraper export JSON.
#' @param output_dir Output directory; created if missing.
#' @return Invisible named list of `slate_id -> file_path`.
#' @export
publish_field_empirics <- function(
  export_path = "inst/data/scraped_drafts/udbb-scraper-latest.json",
  output_dir  = "build/feed/v2/field_empirics"
) {
  picks <- load_scraped_drafts(export_path)
  if (nrow(picks) == 0) {
    cli::cli_abort("No picks loaded from {.path {export_path}} -- nothing to publish.")
  }

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  pos_counts   <- empirical_position_counts(picks)
  stacks       <- empirical_stack_patterns(picks)
  pick_dists   <- empirical_pick_distributions(picks)

  source_captured_at <- attr(picks, "exported_at") %||% NA_character_
  computed_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  out_paths <- list()
  for (sid in unique(picks$slate_id)) {
    slate_picks <- picks[picks$slate_id == sid, , drop = FALSE]
    n_drafts <- length(unique(slate_picks$draft_id))
    n_teams  <- nrow(unique(slate_picks[, c("draft_id", "drafter_user_id")]))
    n_tnmt_unique <- length(setdiff(unique(slate_picks$tournament_id), NA))

    by_event_type <- lapply(
      split(slate_picks, slate_picks$event_type, drop = TRUE),
      function(sub) list(
        n_drafts = length(unique(sub$draft_id)),
        n_teams  = nrow(unique(sub[, c("draft_id", "drafter_user_id")]))
      )
    )

    pc_slate <- pos_counts[pos_counts$slate_id == sid, , drop = FALSE]
    position_counts <- list()
    for (pos in unique(pc_slate$position)) {
      rows <- pc_slate[pc_slate$position == pos, , drop = FALSE]
      m <- as.list(rows$n_teams_with_count)
      names(m) <- as.character(rows$count)
      position_counts[[pos]] <- m
    }

    st_slate <- stacks[stacks$slate_id == sid, , drop = FALSE]
    stack_patterns <- list(
      mean_max_team_stack_depth = round(mean(st_slate$max_team_stack_depth), 3),
      qb_stack_2plus_rate       = round(mean(st_slate$has_qb_stack_2plus), 3),
      qb_stack_3plus_rate       = round(mean(st_slate$has_qb_stack_3plus), 3),
      qb_stack_4plus_rate       = round(mean(st_slate$has_qb_stack_4plus), 3),
      mean_n_team_stacks_3plus  = round(mean(st_slate$n_team_stacks_3plus), 3)
    )

    pd_slate <- pick_dists[pick_dists$slate_id == sid, , drop = FALSE]
    pd_slate <- pd_slate[, c("pick_overall", "underdog_id", "first_name",
                             "last_name", "position_name", "n_times_drafted",
                             "mean_adp_at_pick", "sd_adp_at_pick")]

    feed <- list(
      slate_id                  = sid,
      computed_at               = computed_at,
      source_export_captured_at = source_captured_at,
      n_drafts_sampled          = n_drafts,
      n_teams_sampled           = n_teams,
      n_tournament_unique       = n_tnmt_unique,
      by_event_type             = by_event_type,
      position_counts           = position_counts,
      stack_patterns            = stack_patterns,
      pick_distributions        = pd_slate
    )

    fp <- file.path(output_dir, paste0(sid, ".json"))
    jsonlite::write_json(feed, fp, auto_unbox = TRUE, pretty = TRUE,
                         null = "null", na = "null", dataframe = "rows")
    cli::cli_alert_success(
      "publish_field_empirics [{sid}]: wrote {.path {fp}} ({n_drafts} drafts, {n_teams} teams)"
    )
    out_paths[[sid]] <- fp
  }

  invisible(out_paths)
}

#' @keywords internal
.empty_picks_tibble <- function() {
  tibble::tibble(
    draft_id               = character(0),
    draft_state            = character(0),
    slate_id               = character(0),
    tournament_id          = character(0),
    tournament_round_id    = character(0),
    tournament_title       = character(0),
    event_type             = character(0),
    drafter_slot           = integer(0),
    drafter_user_id        = character(0),
    draft_entry_id         = character(0),
    round                  = integer(0),
    pick_overall           = integer(0),
    appearance_id          = character(0),
    underdog_id            = character(0),
    first_name             = character(0),
    last_name              = character(0),
    position_name          = character(0),
    team_id                = character(0),
    projection_adp_at_pick = numeric(0),
    projection_points      = numeric(0),
    actual_points          = numeric(0)
  )
}
