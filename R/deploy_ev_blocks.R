#' Deploy pipeline for the EV building-block artifacts (Artifact A + B)
#'
#' Wires [publish_ev_blocks()] into the feed deploy. For each configured
#' slate it (1) reblends the feed, (2) reuses the Layer A draws
#' [publish_v2()] already wrote to `<out_dir>/v2/draws/<slate>.parquet`
#' (the contract: reuse, never regenerate), (3) builds Artifact A (the
#' path-aligned int16 joint-draws tensor at `n_paths`), (4) scores a
#' synthetic field once and builds the per-tournament curves (Artifact B),
#' then (5) calls [publish_ev_blocks()], which writes `v2/ev/*` and merges
#' the `v2_draws_*` / `tournaments.<id>.curves_*` keys into the existing
#' `_meta.json` in place (preserving the projection keys [publish_v2()]
#' wrote).
#'
#' Run it AFTER [publish_v2()] and BEFORE the CI step that moves the
#' parquet out of the publish dir -- it reads that parquet. The EV
#' artifacts land in `v2/ev/`, which the parquet move (`v2/draws/*.parquet`)
#' never touches, so the ~5 MB/slate tensor stays on gh-pages.
#'
#' Adding a slate or tournament later is a one-line edit to
#' [ev_blocks_publish_config()] -- no pipeline/workflow change.

#' Which slates/tournaments get EV blocks published, and at what scale.
#'
#' Each entry: `slate_id`, the `tournament_ids` whose curves to ship, and
#' the build knobs (`n_paths` = shipped path count per the contract's
#' ranking-stability protocol; `top_n` draftable players; `field_teams`
#' synthetic field size -- must be a money-conservation-valid multiple for
#' EVERY listed tournament, see [tournament_field_multiple()];
#' `field_sims` curve-field sim count). Only `puppy2` + `dachshund` ship
#' today (the curves that exist); Mini Golden / Frenchie 3 / $25 Puppy are
#' future config swaps -- add them here once their configs land.
#' @export
ev_blocks_publish_config <- function() {
  list(
    list(
      slate_id       = "nfl_2026_season",
      tournament_ids = c("puppy2", "dachshund"),
      n_paths        = 500L,    # chosen N (smoke_ev_blocks.R path-count protocol)
      top_n          = 300L,    # -> ~295 covered players after intersect
      field_teams    = 2700L,   # 9*300 (puppy2) AND 15*180 (dachshund)
      field_sims     = 400L
    )
  )
}

#' ADP-only default field targets (for CI, where scraped drafts are absent)
#'
#' The scraped Underdog draft exports that [compute_field_targets()] needs
#' are large local-only inputs (gitignored), so they are NOT present in CI.
#' The synthetic field [generate_field()] draws is driven almost entirely by
#' ADP (`dnorm((adp - slot)/sigma)`) plus a positional-balance nudge and the
#' stack multiplier; the scraped targets only supply (a) the per-position
#' mean counts that set the balance nudge and (b) per-slot ADP sigma (which
#' already falls back to a global 24 when missing). So an ADP-driven field
#' with typical best-ball position means is a faithful, reproducible basis
#' for the curves -- the contract does not require scraped-draft fidelity in
#' Artifact B. `position_means` are typical 18-pick UD best-ball construction
#' (sum to the 18-man roster); `slot_adp_sd` is left empty so generate_field
#' uses its global sigma.
#' @keywords internal
.default_field_targets <- function() {
  list(
    position_means = c(QB = 1.8, RB = 5.8, WR = 8.2, TE = 2.2),  # sums to 18
    qb_stack_2plus_rate = 0.5,
    slot_adp_sd = stats::setNames(numeric(0), character(0)))     # -> global sigma 24
}

#' Resolve field targets: real scraped-draft targets when available,
#' otherwise the ADP-only defaults (the CI path).
#' @keywords internal
.resolve_field_targets <- function(slate_id) {
  tryCatch({
    picks <- load_scraped_drafts()
    tg <- compute_field_targets(picks, slate_id = slate_id)
    cli::cli_alert_info("publish_ev_blocks [{slate_id}]: field targets from scraped drafts")
    tg
  }, error = function(e) {
    cli::cli_alert_warning(
      "publish_ev_blocks [{slate_id}]: scraped drafts unavailable -- ADP-default field targets")
    .default_field_targets()
  })
}

#' Read the Layer A draws back, capped to `max_sims` simulations
#'
#' Reuses the parquet [publish_v2()] wrote. The marginals only need a few
#' thousand sims to be well-resolved, so the cap bounds CI memory/time
#' without a separate sim (the 10K-sim production parquet would otherwise
#' pull ~50M rows into memory). The filter is pushed into Arrow so only the
#' kept sims are read.
#' @keywords internal
.read_layerA_capped <- function(parquet_path, max_sims = 2000L) {
  if (!file.exists(parquet_path)) {
    cli::cli_abort(c(
      "Layer A draws parquet not found: {.path {parquet_path}}.",
      i = "Run publish_v2() first (it writes v2/draws/<slate>.parquet)."))
  }
  ds <- arrow::open_dataset(parquet_path, format = "parquet")
  max_sims <- as.integer(max_sims)
  df <- tryCatch(
    dplyr::collect(dplyr::filter(ds, .data$sim_idx <= max_sims)),
    error = function(e) as.data.frame(arrow::read_parquet(parquet_path)))
  as.data.frame(df)
}

#' Build + publish EV-blocks artifacts for every configured slate
#'
#' @param out_dir Feed root (the peaceiris publish dir, e.g.
#'   `"build/deploy"`). The parquet from [publish_v2()] is expected under
#'   `<out_dir>/v2/draws/` unless `draws_dir` overrides it.
#' @param config Slate/tournament config; see [ev_blocks_publish_config()].
#' @param draws_dir Directory holding `<slate>.parquet` (default
#'   `<out_dir>/v2/draws`).
#' @param marginal_sims Cap on Layer A sims used for the marginals.
#' @param seed RNG seed for the joint draw + field sim (reproducible build).
#' @param cache_dir HTTP cache dir for [blend_slate()] (shared with
#'   publish_v2's cache so the reblend hits warm sources).
#' @return Invisibly `TRUE`.
#' @export
publish_ev_blocks_pipeline <- function(
    out_dir,
    config        = ev_blocks_publish_config(),
    draws_dir     = file.path(out_dir, "v2", "draws"),
    marginal_sims = 2000L,
    seed          = 1L,
    cache_dir     = file.path("~", ".bestball-bro", "cache")) {

  sources_path <- .inst_path("data/sources", "_manifest.yaml")
  slates_path  <- .inst_path("data/slates",  "_manifest.yaml")

  for (cfg in config) {
    sid <- cfg$slate_id
    parquet_path <- file.path(draws_dir, paste0(sid, ".parquet"))
    cli::cli_alert_info("publish_ev_blocks [{sid}]: blending feed")
    feed <- blend_slate(
      slate_id              = sid,
      sources_manifest_path = sources_path,
      slates_manifest_path  = slates_path,
      cache_dir             = cache_dir,
      write_json            = FALSE)
    positions   <- positions_from_feed(feed)
    schedule    <- schedule_from_feed(feed)
    lineup_spec <- load_slate_lineup_spec(sid)

    cli::cli_alert_info("publish_ev_blocks [{sid}]: reading Layer A draws (<= {marginal_sims} sims)")
    layerA <- .read_layerA_capped(parquet_path, max_sims = marginal_sims)

    # ---- Artifact A: path-aligned int16 joint-draws tensor ----
    cli::cli_alert_info(
      "publish_ev_blocks [{sid}]: building Artifact A (N={cfg$n_paths} paths, top {cfg$top_n})")
    ev_draws <- build_ev_draws(feed, layerA, slate_id = sid,
                               n_paths = cfg$n_paths, top_n = cfg$top_n, seed = seed)

    # ---- Artifact B: synthetic field scored once -> per-tournament curves ----
    # Field targets: real scraped-draft targets when present (local/dev),
    # else ADP-only defaults (CI). A config entry may override via
    # `field_targets` (used by the integration test to pin the CI path).
    targets <- cfg$field_targets %||% .resolve_field_targets(sid)
    pool    <- load_slate_data(sid)
    cli::cli_alert_info(
      "publish_ev_blocks [{sid}]: scoring field ({cfg$field_teams} teams x {cfg$field_sims} sims)")
    field   <- generate_field(sid, player_pool = pool, targets = targets,
                              n_teams = cfg$field_teams, seed = seed)
    field_rosters <- split(field$rosters$underdog_id, field$rosters$entry_id)
    field_scores  <- simulate_per_stage_scores(
      rosters = field_rosters, positions = positions, layerA_draws = layerA,
      schedule = schedule, lineup_spec = lineup_spec,
      n_sims = cfg$field_sims, seed = seed)

    curves_list <- lapply(cfg$tournament_ids, function(tid) {
      cli::cli_alert_info("publish_ev_blocks [{sid}]: curves [{tid}]")
      build_tournament_curves(load_tournament(tid), field_scores,
                              n_grid = 256L, seed = seed)
    })

    # ---- publish: writes v2/ev/* and merges EV keys into _meta.json ----
    publish_ev_blocks(out_dir, ev_draws, curves_list)
  }
  invisible(TRUE)
}
