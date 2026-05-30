#' Generate canonical tournament configs from a compact shorthand input
#'
#' Stops the hand-writing of per-tournament YAMLs. Takes a compact
#' specification (display name, slate, advancement shorthand, final
#' seats), optionally enriches it from the udbb-scraper export
#' (real per-round `underdog_round_id`s, current `entry_count`,
#' `description`, `image_url`, `rules_url`, `cutoff_at`), and emits a
#' YAML file matching the canonical [bbm7.yaml] schema.
#'
#' Scope: the 4-stage **Season-slate** structure (1 qualifier round on
#' cumulative weeks 1-14, then per-week QF/SF/Final on 15/16/17). This is
#' the regular shape every Season-slate BBM-style tournament shares;
#' only the per-stage `(advance, pod)` ratios and the final seat count
#' vary, so a shorthand like `2/12 - 1/10 - 1/6 - 625` fully specifies
#' the advancement. Eliminator (17-round H2H) and Weekly Winners
#' (independent weekly pools) use different structures and stay
#' hand-written.
#'
#' # Shorthand
#'
#' The `stages` field is a 3-element character vector, one entry per
#' advancement stage:
#' - First entry "A/B" -> qualifier round: A advance per pod of B, weeks 1-14.
#' - Second "C/D"      -> quarterfinals:   C advance per pod of D, week 15.
#' - Third  "E/F"      -> semifinals:      E advance per pod of F, week 16.
#' - `final_seats`     -> championship single-pod size, week 17.
#'
#' Seats entering each stage are computed backward from `final_seats`;
#' `total_field_size` is the result for stage 1. The generator errors if
#' the shorthand doesn't divide evenly.
#'
#' # Auto-derivation from the scraper
#'
#' If the spec carries an `underdog_tournament_id` AND the udbb-scraper
#' export has captured the matching `/v1/tournaments/<uuid>` endpoint,
#' the generator merges in:
#' - real per-round `underdog_round_id` values (from `tournament_rounds`),
#' - missing `description` / `image_url` / `rules_url` / `cutoff_at` /
#'   `max_entries_per_user` from the endpoint payload.
#'
#' For tournaments without a captured endpoint, round IDs default to
#' `"TBD"` and the output carries `auto_generated: true` so the
#' canonical validator accepts the placeholder (see [.validate_tournament]).
#' Spec-provided values always win over scraper values; scraper only
#' fills holes.
#'
#' @param spec A list with required fields `id`, `display_name`,
#'   `underdog_tournament_id`, `slate_id`, `stages`, `final_seats`, plus
#'   any optional Underdog-side metadata
#'   (`description`, `rules_url`, `image_url`, `cutoff_at`,
#'   `entry_fee_usd`, `max_entries_per_user`, `rake_pct`).
#' @param scraper_export Optional list (parsed udbb-scraper JSON). When
#'   present, the generator enriches `spec` with matching scraper data.
#' @param slates_manifest Optional pre-loaded slate manifest (`list`).
#'   Defaults to [load_slate_manifest()]; takes a manifest only to
#'   make tests independent of the on-disk file.
#' @return A list representing the canonical tournament definition --
#'   same shape [load_tournament()] returns, ready to either pass to
#'   [render_tournament_yaml()] for disk-writing or compare against an
#'   existing hand-written file.
#' @export
generate_tournament_config <- function(spec,
                                       scraper_export = NULL,
                                       slates_manifest = NULL) {
  .require_spec_fields(spec)
  if (is.null(slates_manifest)) {
    slates_manifest <- load_slate_manifest()
  }
  slate_def <- slates_manifest[[spec$slate_id]]
  if (is.null(slate_def)) {
    cli::cli_abort(c(
      "Unknown slate_id {.val {spec$slate_id}} for tournament {.val {spec$id}}.",
      i = "Add it to {.path inst/data/slates/_manifest.yaml}."
    ))
  }

  stage_info <- lapply(spec$stages, .parse_stage_shorthand)
  final_seats <- as.integer(spec$final_seats)
  seats <- .compute_seats_entering(stage_info, final_seats)

  scraper_meta <- if (!is.null(scraper_export) &&
                      !is.null(spec$underdog_tournament_id)) {
    .scraper_lookup_tournament(scraper_export, spec$underdog_tournament_id)
  } else {
    list()
  }

  round_ids <- if (!is.null(scraper_meta$round_ids) &&
                   length(scraper_meta$round_ids) >= 4L) {
    scraper_meta$round_ids[1:4]
  } else if (!is.null(scraper_meta$round_ids) &&
             length(scraper_meta$round_ids) == 1L) {
    # Only the qualifier round is in the index (common -- comes from
    # captured drafts when /v1/tournaments/<uuid> isn't in unkeyed[]).
    c(scraper_meta$round_ids, rep("TBD", 3L))
  } else {
    rep("TBD", 4L)
  }
  any_tbd <- any(round_ids == "TBD")

  list(
    tournament_id          = spec$id,
    underdog_tournament_id = spec$underdog_tournament_id,
    display_name           = spec$display_name,
    description            = spec$description %||% scraper_meta$description %||% "",
    rules_url              = spec$rules_url %||% scraper_meta$rules_url %||% "",
    image_url              = spec$image_url %||% scraper_meta$image_url %||% "",
    slate_id               = spec$slate_id,
    underdog_slate_id      = slate_def$underdog_slate_id,
    entry_fee_usd          = spec$entry_fee_usd %||% 0L,
    total_field_size       = as.integer(seats[1L]),
    max_entries_per_user   = spec$max_entries_per_user %||%
                             scraper_meta$max_entries %||% 150L,
    draft_size             = 12L,
    draft_rounds           = .roster_block_from_slate(slate_def)$total_slots,
    pick_clock_seconds     = 20L,
    rake_pct               = spec$rake_pct,
    cutoff_at              = spec$cutoff_at %||% scraper_meta$cutoff_at,
    # `auto_generated: true` lets the canonical validator accept TBD
    # round IDs (only when present). For fully-resolved configs (all 4
    # round IDs known) we omit the flag so a subsequent hand-edit can't
    # silently drift back to TBDs.
    auto_generated         = if (any_tbd) TRUE else NULL,
    inherits_common_rules  = TRUE,
    scoring                = .STANDARD_HALF_PPR_UD_SCORING,
    roster                 = .roster_block_from_slate(slate_def),
    best_ball_lineup_rule  = .best_ball_lineup_rule_for_slate(slate_def),
    simulator_output_schema = list(
      primary_unit       = "per_player_total_ev",
      per_week_breakdown = "diagnostic_only",
      rationale          = paste(
        "Player value depends on advancement through multi-week structure.",
        "Per-week breakdowns derivable but not the natural unit."
      )
    ),
    stages = list(
      .stage_block(1L, round_ids[1L], stage_info[[1L]], seats[1L]),
      .stage_block(2L, round_ids[2L], stage_info[[2L]], seats[2L]),
      .stage_block(3L, round_ids[3L], stage_info[[3L]], seats[3L]),
      .championship_block(round_ids[4L], seats[4L])
    ),
    payouts = .placeholder_payouts(final_seats)
  )
}

#' Render a generated tournament-config list as a canonical YAML string
#'
#' Hand-rolled instead of [yaml::as.yaml()] for one important reason: YAML
#' 1.1 treats the unquoted key `n` as the boolean `FALSE`. The
#' canonical config has `advancement."n":` as an explicitly quoted key,
#' and the loader depends on it. [yaml::as.yaml()] won't reliably emit the
#' quotes around `n`, so we render manually.
#'
#' The output mirrors `bbm7.yaml`'s ordering and indentation so the
#' generator-produced config is human-comparable with any hand-written
#' canonical config.
#'
#' @param cfg A list from [generate_tournament_config()].
#' @return Character scalar -- the rendered YAML text.
#' @export
render_tournament_yaml <- function(cfg) {
  out <- character(0)
  add <- function(...) out[[length(out) + 1L]] <<- paste0(..., collapse = "")

  add("# ", cfg$display_name)
  add("# Auto-generated by generate_tournament_config().")
  if (isTRUE(cfg$auto_generated)) {
    add("# Some round IDs are TBD; re-run the generator once Underdog publishes them.")
  }
  add("")

  add("tournament_id: ", cfg$tournament_id)
  add("underdog_tournament_id: ", cfg$underdog_tournament_id)
  add("display_name: ", .yaml_str(cfg$display_name))
  add("description: ", .yaml_str(cfg$description))
  add("rules_url: ", .yaml_str(cfg$rules_url))
  add("image_url: ", .yaml_str(cfg$image_url))
  add("")
  add("slate_id: ", cfg$slate_id)
  add("underdog_slate_id: ", cfg$underdog_slate_id)
  add("")
  add("entry_fee_usd: ", cfg$entry_fee_usd)
  add("total_field_size: ", cfg$total_field_size)
  add("max_entries_per_user: ", cfg$max_entries_per_user)
  add("draft_size: ", cfg$draft_size)
  add("draft_rounds: ", cfg$draft_rounds)
  add("pick_clock_seconds: ", cfg$pick_clock_seconds)
  if (!is.null(cfg$rake_pct)) add("rake_pct: ", cfg$rake_pct)
  if (!is.null(cfg$cutoff_at)) add("cutoff_at: ", .yaml_str(cfg$cutoff_at))
  add("")
  if (isTRUE(cfg$auto_generated)) {
    add("auto_generated: true")
    add("")
  }
  add("inherits_common_rules: true")
  add("")

  add("scoring:")
  for (k in names(cfg$scoring)) {
    add("  ", k, ": ", cfg$scoring[[k]])
  }
  add("")

  add("roster:")
  add("  total_slots: ", cfg$roster$total_slots)
  add("  starting_lineup:")
  for (k in names(cfg$roster$starting_lineup)) {
    add("    ", k, ": ", cfg$roster$starting_lineup[[k]])
  }
  add("  bench: ", cfg$roster$bench)
  add("")

  add("best_ball_lineup_rule:")
  add("  flex_eligible: [", paste(cfg$best_ball_lineup_rule$flex_eligible,
                                  collapse = ", "), "]")
  add("  flex_selection: ", cfg$best_ball_lineup_rule$flex_selection)
  add("  description: |")
  for (ln in strsplit(cfg$best_ball_lineup_rule$description, "\n",
                      fixed = TRUE)[[1]]) {
    add("    ", ln)
  }
  add("")

  add("simulator_output_schema:")
  add("  primary_unit: ", cfg$simulator_output_schema$primary_unit)
  add("  per_week_breakdown: ", cfg$simulator_output_schema$per_week_breakdown)
  add("  rationale: |")
  for (ln in strsplit(cfg$simulator_output_schema$rationale, "\n",
                      fixed = TRUE)[[1]]) {
    add("    ", ln)
  }
  add("")

  add("stages:")
  for (i in seq_along(cfg$stages)) {
    s <- cfg$stages[[i]]
    add("  - id: ", s$id)
    add("    abbreviation: ", s$abbreviation)
    add("    underdog_round_id: ", s$underdog_round_id)
    add("    round_number: ", s$round_number)
    add("    weeks: [", paste(s$weeks, collapse = ", "), "]")
    add("    pod_structure: ", s$pod_structure)
    add("    pod_size: ", s$pod_size)
    add("    seats_entering: ", s$seats_entering)
    add("    advancement:")
    add("      type: ", s$advancement$type)
    if (!is.null(s$advancement$n)) {
      # YAML 1.1 boolean alias `n` requires quoting on read AND write so
      # the loader sees a string key, not the boolean FALSE.
      add("      \"n\": ", s$advancement$n)
    }
    add("      ranking_metric: ", s$advancement$ranking_metric)
    if (i < length(cfg$stages)) add("")
  }
  add("")

  add("# Payouts placeholder -- the generator emits a minimal tier block so")
  add("# the canonical loader's payout validation passes. Replace with the real")
  add("# tier-by-tier prize structure from the Underdog rules page.")
  add("payouts:")
  add("  championship_round:")
  add("    tiers:")
  for (t in cfg$payouts$championship_round$tiers) {
    add(sprintf("      - {rank_from: %d, rank_to: %d, usd: %d}",
                t$rank_from, t$rank_to, t$usd))
  }

  paste(unlist(out), collapse = "\n")
}

#' Write a generated tournament config to disk
#'
#' @inheritParams generate_tournament_config
#' @param output_dir Directory to write `<spec$id>.yaml` into. Created if
#'   missing.
#' @return Invisibly, the absolute output path.
#' @export
write_tournament_config <- function(spec,
                                    output_dir,
                                    scraper_export = NULL,
                                    slates_manifest = NULL) {
  cfg <- generate_tournament_config(spec, scraper_export, slates_manifest)
  yaml_str <- render_tournament_yaml(cfg)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(output_dir, paste0(spec$id, ".yaml"))
  writeLines(yaml_str, out_path, useBytes = TRUE)
  invisible(normalizePath(out_path, winslash = "/"))
}

#' Generate every tournament listed in the generator input table
#'
#' Reads `inst/data/tournaments/_generator_input.yaml` (or the override
#' `input_path`), filters by `only_ids` if given, and writes the
#' generated YAMLs under `output_dir`. Tournaments whose spec carries
#' `write_to_disk: false` are skipped -- useful for entries that exist
#' only to support round-trip tests against richer hand-written files
#' (e.g. BBM7).
#'
#' @param input_path Path to the generator-input YAML. Defaults to
#'   `inst/data/tournaments/_generator_input.yaml`.
#' @param output_dir Where to write generated configs. Defaults to
#'   `inst/data/tournaments/`.
#' @param scraper_export Optional parsed scraper export. Defaults to
#'   reading `inst/data/scraped_drafts/udbb-scraper-latest.json` if it
#'   exists.
#' @param only_ids Optional character vector of tournament IDs to filter to.
#' @return Invisibly, a named character vector of `id -> output_path` for
#'   the tournaments that were written.
#' @export
generate_all_tournament_configs <- function(input_path = NULL,
                                            output_dir = NULL,
                                            scraper_export = NULL,
                                            only_ids = NULL) {
  if (is.null(input_path)) {
    input_path <- .inst_path("data/tournaments", "_generator_input.yaml")
  }
  if (input_path == "" || !file.exists(input_path)) {
    cli::cli_abort(c(
      "Generator input not found.",
      i = "Expected: {.path inst/data/tournaments/_generator_input.yaml}"
    ))
  }
  if (is.null(output_dir)) {
    root <- .find_package_root()
    if (is.null(root)) {
      cli::cli_abort("Cannot resolve package root for default output_dir.")
    }
    output_dir <- file.path(root, "inst", "data", "tournaments")
  }
  input <- yaml::read_yaml(input_path)
  specs <- input$tournaments %||% list()

  if (is.null(scraper_export)) {
    scraper_path <- .inst_path("data/scraped_drafts", "udbb-scraper-latest.json")
    if (scraper_path != "" && file.exists(scraper_path)) {
      scraper_export <- jsonlite::fromJSON(scraper_path, simplifyVector = FALSE)
    }
  }
  slates_manifest <- load_slate_manifest()

  out_paths <- character(0)
  for (s in specs) {
    if (!is.null(only_ids) && !(s$id %in% only_ids)) next
    if (isFALSE(s$write_to_disk)) next
    p <- write_tournament_config(s, output_dir, scraper_export, slates_manifest)
    out_paths[[s$id]] <- p
    cli::cli_alert_success("Wrote {.path {p}}")
  }
  invisible(out_paths)
}

# ---- internals --------------------------------------------------------------

#' Standard half-PPR Underdog scoring (matches `bbm7.yaml`).
#' @keywords internal
.STANDARD_HALF_PPR_UD_SCORING <- list(
  receptions       = 0.5,
  receiving_td     = 6,
  receiving_yard   = 0.1,
  rushing_td       = 6,
  rushing_yard     = 0.1,
  passing_yard     = 0.04,
  passing_td       = 4,
  interception     = -1,
  two_pt_conversion = 2,
  fumble_lost      = -2
)

#' @keywords internal
.require_spec_fields <- function(spec) {
  needed <- c("id", "display_name", "underdog_tournament_id", "slate_id",
              "stages", "final_seats")
  missing <- setdiff(needed, names(spec))
  if (length(missing) > 0L) {
    cli::cli_abort("Tournament spec is missing required field(s): {missing}")
  }
  if (!is.character(spec$stages) || length(spec$stages) != 3L) {
    cli::cli_abort(c(
      "`stages` must be a 3-element character vector of `N/POD` strings.",
      i = "Got: {.val {spec$stages}}"
    ))
  }
}

#' Parse one stage shorthand like "2/12" -> list(n = 2L, pod_size = 12L).
#' @keywords internal
.parse_stage_shorthand <- function(s) {
  parts <- strsplit(trimws(s), "/", fixed = TRUE)[[1]]
  if (length(parts) != 2L) {
    cli::cli_abort("Bad stage shorthand {.val {s}}: expected `N/POD`.")
  }
  n <- suppressWarnings(as.integer(parts[1L]))
  pod <- suppressWarnings(as.integer(parts[2L]))
  if (is.na(n) || is.na(pod) || n < 1L || pod < 1L) {
    cli::cli_abort("Bad stage shorthand {.val {s}}: N and POD must be positive integers.")
  }
  list(n = n, pod_size = pod)
}

#' Compute seats_entering for the 4 stages by working backward from
#' final_seats. Errors if the shorthand doesn't divide evenly.
#' @keywords internal
.compute_seats_entering <- function(stage_info, final_seats) {
  seats <- numeric(4L)
  seats[4L] <- final_seats
  for (i in 3:1) {
    s <- stage_info[[i]]
    raw <- seats[i + 1L] * s$pod_size / s$n
    if (!isTRUE(all.equal(raw, round(raw)))) {
      cli::cli_abort(c(
        "Advancement shorthand doesn't divide evenly at stage {i}.",
        i = "Stage {i+1} entering = {seats[i + 1L]}, pod_size = {s$pod_size}, n = {s$n}",
        x = "Backward-derived seats_entering = {raw} (must be integral)."
      ))
    }
    seats[i] <- raw
  }
  as.integer(seats)
}

#' Stage block for round 1/2/3 (qualifiers / QF / SF).
#' @keywords internal
.stage_block <- function(round_number, round_id, sh_info, seats_entering) {
  ids       <- c("qualifiers", "quarterfinals", "semifinals")
  abbrs     <- c("Qual", "QF", "SF")
  weeks_lst <- list(1:14, 15L, 16L)
  pod_st    <- c("within_draft", "random", "random")
  rank_met  <- c(
    "cumulative_starting_lineup_points_weeks_1_14",
    "week_15_starting_lineup_points",
    "week_16_starting_lineup_points"
  )
  list(
    id                = ids[round_number],
    abbreviation      = abbrs[round_number],
    underdog_round_id = round_id,
    round_number      = as.integer(round_number),
    weeks             = as.integer(weeks_lst[[round_number]]),
    pod_structure     = pod_st[round_number],
    pod_size          = as.integer(sh_info$pod_size),
    seats_entering    = as.integer(seats_entering),
    advancement       = list(
      type           = "top_n_by_pod",
      n              = as.integer(sh_info$n),
      ranking_metric = rank_met[round_number]
    )
  )
}

#' Championship stage block.
#' @keywords internal
.championship_block <- function(round_id, final_seats) {
  list(
    id                = "championship",
    abbreviation      = "Final",
    underdog_round_id = round_id,
    round_number      = 4L,
    weeks             = 17L,
    pod_structure     = "single_pod",
    pod_size          = as.integer(final_seats),
    seats_entering    = as.integer(final_seats),
    advancement       = list(
      type           = "rank_all",
      ranking_metric = "week_17_starting_lineup_points"
    )
  )
}

#' Build the `roster` block from the slate's `starting_lineup`.
#' @keywords internal
.roster_block_from_slate <- function(slate_def) {
  slots <- slate_def$starting_lineup$slots
  if (is.null(slots)) {
    cli::cli_abort("Slate is missing `starting_lineup.slots`.")
  }
  starting <- list()
  for (slot in slots) {
    # The slate manifest quotes "n": so yaml::read_yaml gives us a string key
    # "n". A future un-quoting would parse as YAML 1.1 boolean FALSE; the
    # fallback to slot[["FALSE"]] handles that case defensively.
    n_val <- slot$n %||% slot[["n"]] %||% slot[["FALSE"]]
    if (is.null(n_val)) {
      cli::cli_abort("Slot {.val {slot$pos}} in slate is missing `n` count.")
    }
    starting[[slot$pos]] <- as.integer(n_val)
  }
  # Total slots: 18 for Season-family slates, 20 for Superflex (matches
  # the slate's documented bench size). Derive from starting + bench.
  total_starting <- sum(unlist(starting))
  bench <- 10L  # Season default. Slate could override if needed.
  if (identical(slate_def$display_name, "NFL 2026 Superflex Season")) {
    bench <- 12L
  }
  list(
    total_slots     = total_starting + bench,
    starting_lineup = starting,
    bench           = bench
  )
}

#' Best-ball lineup rule block. The Season-family has flex {RB,WR,TE}; the
#' Superflex slate adds a SFLEX. The generator's 4-stage shape is currently
#' only used for Season-family tournaments, so we emit the FLEX-only rule.
#' @keywords internal
.best_ball_lineup_rule_for_slate <- function(slate_def) {
  list(
    flex_eligible  = c("RB", "WR", "TE"),
    flex_selection = "highest_weekly_score_among_overflow_RB_WR_TE",
    description    = paste(
      "Each week, lineup auto-set to maximize total points. FLEX filled by",
      "highest weekly score among: (3rd-best RB on roster) OR (4th-best WR",
      "on roster) OR (2nd-best TE on roster).",
      sep = "\n"
    )
  )
}

#' Minimal championship payouts block -- placeholder that satisfies the
#' canonical validator's per-table non-overlap check. Replace by hand
#' with the real Underdog payout schedule.
#' @keywords internal
.placeholder_payouts <- function(final_seats) {
  list(
    championship_round = list(
      tiers = list(
        list(rank_from = 1L,            rank_to = 1L,             usd = 0L),
        list(rank_from = 2L,            rank_to = as.integer(final_seats), usd = 0L)
      )
    )
  )
}

#' Look up a tournament in the udbb-scraper export by Underdog UUID.
#'
#' Returns a list with whatever fields the scraper exposed:
#' - `round_ids`: character vector of `tournament_rounds[].id`, ordered by
#'   `number`, when `/v1/tournaments/<uuid>` was captured in `unkeyed[]`.
#' - `description`, `image_url`, `rules_url`, `cutoff_at`, `max_entries`:
#'   from the same endpoint when present.
#' - Falls back to a single `round_ids` entry from `round_tournament_index`
#'   (the qualifier round, available whenever drafts were captured).
#' @keywords internal
.scraper_lookup_tournament <- function(scraper, uuid) {
  if (is.null(scraper) || is.null(uuid)) return(list())
  out <- list()

  target_ep <- paste0("/v1/tournaments/", uuid)
  for (u in scraper$unkeyed %||% list()) {
    if (identical(u$api_endpoint %||% "", target_ep)) {
      t <- u$raw_response$tournament %||% list()
      out$description    <- t$description
      out$image_url      <- t$image_url
      out$rules_url      <- t$rules_url
      out$cutoff_at      <- t$cutoff_at
      out$max_entries    <- t$max_entries
      rounds <- t$tournament_rounds %||% list()
      if (length(rounds) > 0L) {
        ords <- vapply(rounds, function(r) as.integer(r$number %||% NA_integer_),
                       integer(1))
        ids  <- vapply(rounds, function(r) as.character(r$id %||% NA_character_),
                       character(1))
        out$round_ids <- ids[order(ords)]
      }
      break
    }
  }

  if (is.null(out$round_ids)) {
    # Fall back to whatever round_tournament_index has for this tournament.
    rti <- scraper$round_tournament_index %||% list()
    matches <- character(0)
    for (rid in names(rti)) {
      r <- rti[[rid]]
      if (identical(r$tournament_id, uuid)) {
        matches <- c(matches, rid)
      }
    }
    if (length(matches) > 0L) out$round_ids <- matches
  }

  out
}

#' Quote a YAML string value. We always quote -- the canonical files
#' do the same for descriptions / URLs that may contain `: ` or similar.
#' @keywords internal
.yaml_str <- function(s) {
  if (is.null(s) || is.na(s)) return('""')
  s <- as.character(s)
  # Escape backslashes and double quotes for double-quoted YAML.
  s <- gsub("\\\\", "\\\\\\\\", s)
  s <- gsub('"', '\\\\"', s)
  paste0('"', s, '"')
}
