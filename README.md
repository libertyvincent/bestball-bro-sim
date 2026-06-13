# bestball-bro-sim

R sim project producing the JSON projection + tournament-building-block feed consumed by the **BestBall Bro** Chrome extension during live Underdog best ball drafts.

## Three related repos

- **bestball-bro-sim** (this repo) — Layer A (projections + season sims) and Layer B (tournament pre-compute). Publishes to `bestball-bro-data` via GitHub Actions.
- **[bestball-bro-data](https://libertyvincent.github.io/bestball-bro-data/)** — GitHub Pages JSON feed served to the extension.
- **bbbro** (Chrome extension) — consumes the feed via `projectionFetch.js`, joins to live Underdog appearances in `matchPlayer.js`, computes live pick EV in `dataJoin.js`.

## Design docs

The design lives in four docs at the repo root:

- [ARCHITECTURE.md](ARCHITECTURE.md) — cross-repo map: how this repo, `bestball-bro-data`, and the extension fit together
- [LAYER_A.md](LAYER_A.md) — projection + season simulation methodology
- [LAYER_B.md](LAYER_B.md) — tournament simulation engine (stage-based, config-driven)
- [FEED_SPEC.md](FEED_SPEC.md) — wire format between this project and the extension

Read these before changing anything structural.

## Status

**Layer A and Layer B are implemented and tested.** Layer A: the v2 blended-consensus projections (`R/blender.R` + `R/calibration.R`), the Monte Carlo season simulator with empirical percentiles (`R/simulate.R`), and the v2 feed publisher (`R/publish_v2.R`). Layer B: field empirics from scraped Underdog drafts (`R/field_empirics.R`), the synthetic opponent-field model (`R/field_model.R`), correlated cross-player draws (`R/correlation.R`), the best-ball lineup optimizer (`R/lineup_optimizer.R`), the team season simulator (`R/math_simulator.R`), and the config-driven multi-stage tournament EV engine with per-player EV attribution (`R/tournament_ev.R`, with BBM7 wrappers in `R/tournament_ev_bbm7.R`), validated against BBMDB historical outcomes (`R/bbmdb_validator.R`). Tournament configs load, validate, and generate via `R/tournament_loader.R` / `R/tournament_config_generator.R`. Every implemented module has a testthat suite under `tests/testthat/`.

The EV building-blocks contract (sim ↔ extension interface) is implemented in `R/ev_blocks.R`: Artifact A (per-slate path-aligned `int16` joint-draws tensor + sidecar), Artifact B (per-tournament advancement/payout curves), the reference eval loop (curve-based marginal EV with common random numbers), and `publish_ev_blocks()` which registers everything in `_meta.json`. The frozen interface spec lives in [docs/ev_building_blocks_contract.md](docs/ev_building_blocks_contract.md). The old `stage_engine.R` / `publish.R` / `run_season_sims()` stubs that reserved this slot are gone.

The v1 nflverse projection pipeline (`projections.R`, `rookies.R`) was retired — its output had no consumers. The extension reads the Clay legacy feed (built in `bestball-bro-data`) and Layer B is fed by v2's draws; see [ARCHITECTURE.md](ARCHITECTURE.md) for the cross-repo picture.

## Layout

```
R/
  # Layer A — projections + season sims
  data_pull.R            # slate manifest + CSV loaders
  blender.R              # v2 blended-consensus projections (Clay/ETR/LegUp)
  calibration.R          # rank -> points calibration curves
  simulate.R             # Monte Carlo season sims, empirical percentiles
  scoring.R              # scoring-config loader + fantasy-point computation
  player_match.R         # cross-source player name normalization
  publish.R              # _meta.json manifest writer
  publish_v2.R           # v2 feed + parquet draws writer (authors _meta.json)

  # Layer B — tournament pre-compute
  field_empirics.R       # empirical distributions from scraped Underdog drafts
  field_model.R          # synthetic opponent-field generator
  correlation.R          # correlated cross-player weekly draws
  lineup_optimizer.R     # best-ball optimal-lineup kernel
  math_simulator.R       # per-team season score simulator
  tournament_ev.R        # config-driven multi-stage tournament EV engine
  tournament_ev_bbm7.R   # BBM7 back-compat wrappers
  bbmdb_validator.R      # xAdv validation against BBMDB historical data
  tournament_loader.R    # tournament YAML loader + validator
  tournament_config_generator.R  # YAML generator from spec shorthand
  ev_blocks.R            # EV building-blocks artifacts (draws tensor + curves + eval loop)

inst/
  data/
    tournaments/         # Per-tournament configs (Sprint 3a schema)
      _common_rules.yaml
      _generator_input.yaml
      bbm7.yaml, puppy.yaml, puppy2.yaml, dachshund.yaml, mini_golden.yaml,
      eliminator_2026.yaml, frenchie3.yaml, frenchie3_superflex.yaml,
      weekly_winners_2026.yaml
    slates/              # Per-slate manifest + CSV universes
    sources/             # v2 blender source-feed manifest
  scoring/               # Per-scoring-system definitions
    half_ppr_underdog.yaml
  adjustments/           # User "knowing ball" overrides
    2026.yaml
  anchors/               # Anchor players for validation
    2026.yaml
  scripts/               # Standalone diagnostic / smoke-test runners

tests/                   # testthat tests
.github/workflows/       # CI: scheduled feed rebuilds + publish to bestball-bro-data
```

## Blender (v2) — `R/blender.R`

The blender builds a **blended consensus** from three published source feeds (it replaced the retired v1 nflverse weekly→season retrofit). Per slate, the blender:

1. Loads the slate's player universe from `inst/data/slates/<slate_id>.csv` (canonical UUIDs).
2. Loads three source feeds from `https://libertyvincent.github.io/bestball-bro-data/sources/` (cached by URL sha256 under `~/.bestball-bro/cache`):
   - **Clay** — half-PPR point projections with full stats (`clay_2026_offense.json`)
   - **ETR** — Underdog-slate rankings, 300 players (rank-only)
   - **LegUp** — Underdog-slate rankings (rank-only)
3. Fits a per-position **calibration curve** (`R/calibration.R`) — natural spline through Clay's `(rank, half_ppr_points)` so rank-only sources convert to point-equivalents.
4. For each slate player, joins by Underdog UUID (ETR / LegUp) or by normalized name + nflverse team + position (Clay), computes a **weighted consensus mean**, **cross-source disagreement std**, and **aleatoric per-week std** (`cv * weekly_mean`).
5. Generates per-week mean + std + percentiles by applying Clay's `weekly_team_scoring.json` as a per-week team-output multiplier. Opponent and home/away come from the same file (Clay carries the schedule).
6. Writes `v2/projections/<slate_id>.json`.

Top-level entry: `publish_v2()`. It writes the per-slate feed JSON + parquet draws and authors the feed root's `_meta.json`. The extension cutover to this feed (`use_newfeed: true`) is a separate, coordinated sprint.

### Sources manifest

`inst/data/sources/_manifest.yaml` declares the three sources, their per-slate feed URLs, the per-slate source weights, and the position aleatoric CV values. The blender re-normalizes weights per player over whichever sources are actually present.

### Aleatoric CV and the season-std formula

```
weekly_std        = cv * weekly_mean
aleatoric_var     = sum(weekly_std^2)   # over active (non-bye) weeks
disagreement_std  = sqrt(weighted variance across sources around consensus mean)
season_std        = sqrt(disagreement_std^2 + aleatoric_var)
```

Each player record also exposes `disagreement_std` and `aleatoric_std` as separate fields. Invariant: `season_std² = disagreement_std² + aleatoric_std²`. The Sprint 2.5 Monte Carlo simulator consumes both components for two-level draws (see below).

Percentiles were analytical (`qnorm(p, season_mean, season_std)`) through Sprint 2; Sprint 2.5 replaced them with empirical percentiles from 10K simulated seasons. The Normal-distribution formula above no longer drives the `season_percentiles` or `weekly[].percentiles` fields in the published feed.

The per-position weekly CV values in the manifest:

| Position | CV   | Rationale |
|---|---|---|
| QB | 0.32 | Elite QB ~22 ppg, weekly std ~7 |
| RB | 0.50 | High variance, TD-dependent |
| WR | 0.55 | Highest mid-season variance, target-dependent |
| TE | 0.60 | Boom/bust position |

These are **weekly** CVs, not season — adjust here if backtests suggest different values.

### Name aliases

Some players have canonical-name disagreements between sources and the slate CSV (e.g. Clay's `"Ken Walker III"` vs the slate's `"Kenneth Walker III"`). Add a one-line entry to `.NAME_ALIASES` in `R/blender.R`:

```r
.NAME_ALIASES <- c(
  "ken walker"     = "kenneth walker",
  "marquise brown" = "hollywood brown"
)
```

Keys are the source's variant **after** lowercasing, period-stripping, and generational-suffix-stripping (so `"Marquise Brown" → "marquise brown"`, `"Ken Walker III" → "ken walker"`). Values are the slate CSV's canonical form, same normalization. Add new entries as they're discovered via the audit file (below).

### Audit file (`build/blender_audit_<slate_id>.txt`)

After each `blend_slate()` run, the blender writes a tab-separated audit listing slate players that show up in ETR or LegUp but **not** Clay. Most rows are explainable (free agents with `team = NA`, rookies Clay didn't include, recent trades Clay's PDF predates); a few are name-alias cases worth fixing. The blender logs the audit count at the end of each run; if it exceeds your tolerance (we've been using ~20), open the file and check for systematic issues.

Categories you'll typically see:

| Category | Action |
|---|---|
| Free agent (team=NA in slate) | Expected; blend on ETR/LegUp only |
| Rookie / UDFA Clay didn't list | Expected; blend on ETR/LegUp only |
| Slate team ≠ Clay's PDF team | Slate is canonical; 2-source blend is fine |
| Name disagreement (e.g. Ken/Kenneth) | Add to `.NAME_ALIASES` |

## Monte Carlo (Sprint 2.5) — `R/simulate.R`

The blender's analytical percentiles are replaced by **empirical percentiles** from 10K simulated 17-active-week seasons per player. Top-level entry: `publish_v2(n_sims = 10000L, seed = NULL)`. `simulate_slate()` does the work; it takes the full feed from `blend_slate()` and returns `enriched_feed` + a long-format draws data.frame written to parquet.

### Two-level model

For each player, each of the `n_sims` simulated seasons goes through two stochastic layers:

1. **Epistemic outer**. Draw `true_season_mean ~ Normal(season_mean, disagreement_std)`. Reflects "we don't know the player's true mean because the sources disagree." A per-sim `scale = true_season_mean / season_mean` (clipped at 0) is propagated to the weekly layer.
2. **Aleatoric inner**. For each non-bye week, draw `weekly_value ~ Normal(weekly_mean * scale, weekly_std * scale)`, clipped at 0 (fantasy points can't go negative). Bye weeks emit exactly 0. Weeks are drawn **iid** within a sim — no inter-week correlation in Sprint 2.5; revisit in Sprint 3+.

If `disagreement_std = 0` (rare; only when all three sources project identical points), the outer draw degenerates to a constant and the model reduces to pure aleatoric. The two-level decomposition holds the invariant `var(season_sum) ≈ disagreement_std² + aleatoric_std²` modulo the small left-tail shrinkage from clipping.

### Empirical vs analytical sanity check

For Bijan Robinson (unanimous-ish RB1, `season_mean = 318.3, season_std = 39.0`):

```
                analytical  empirical  delta
season p10          268.30     270.80  +2.50   <- clip-at-zero shifts low tail up
season p50          318.30     318.70  +0.40
season p90          368.30     368.70  +0.40
empirical mean      318.30     319.12  +0.82   <- 0.3% Monte Carlo noise at n=10K
empirical sd         39.00      38.27  -0.73   <- 1.9% lower from clipping
```

p90 delta is sub-1 point. Anything > 30 points off would indicate a structural bug.

### Where the draws live

The full per-player per-week draws (long-format: `underdog_id, sim_idx, week, draw_value`) are written to `build/deploy/v2/draws/<slate>.parquet` by `publish_v2()`. In CI the workflow then **moves them out of the gh-pages publish directory** into `build/artifacts/v2-draws/` and uploads them as `actions/upload-artifact@v4` (90-day retention).

Parquet draws are **server-side artifacts only** — they do NOT go to GitHub Pages (Pages has a 1 GB site soft limit; four slates of 10K-sim draws total several GB). Layer B's CI in Sprint 3 will fetch them via the artifacts API.

### Performance

Single-slate benchmark on the GitHub Actions runner equivalent (`n_sims = 10000`, ~450 active players, 17 active weeks): **~77 seconds**. Four slates: ~5 minutes wall time in CI. Peak memory: ~2 GB per slate (long-format draws). The 10K knob trades sim accuracy for speed — drop to `n_sims = 100` for fast local iteration.

## Tournaments (Sprint 3a)

Per-tournament definitions live under `inst/data/tournaments/`, one YAML per tournament plus a shared `_common_rules.yaml`. These are pure-data definitions consumed by Layer B's simulator (Sprint 3b+) and by the extension's `round_id → tournament_id` resolver.

Loader API (`R/tournament_loader.R`):

```r
load_tournaments()                                     # all definitions, keyed by tournament_id
load_tournament("bbm7")                                # one tournament
resolve_round_to_tournament("8674b7f5-...")            # round_id (UUID) -> "bbm7" or NULL
```

### Common-rules inheritance

Each tournament YAML opts in via `inherits_common_rules: true`. At load time, fields from `_common_rules.yaml` (`draft_mechanics`, `lineup_mechanics`, `roster_lock`, `advancement_pod_formation`, `wildcard_advancement`, `tie_breaking`, `adp_pick_blocking`, `slow_draft_clock_progression`) are merged in non-destructively: the tournament's own keys always win.

### Adding a new tournament

1. Drop a new YAML in `inst/data/tournaments/<tournament_id>.yaml`.
2. Confirm its `slate_id` exists in `inst/data/slates/_manifest.yaml` and the slate has a `position_caps` block.
3. Run `devtools::test(filter = "tournament-loader")` — the validator checks required fields, slate cross-reference, stage seats-entering math, and per-table payout non-overlap.
4. Commit.

### Round-ID → tournament-ID mapping

Each stage carries an `underdog_round_id` (UUID from Underdog's network responses). The extension reads `round_id` from the draft URL and calls `resolve_round_to_tournament()` to find the parent tournament definition. `underdog_round_id: TBD` is permitted **only** for tournaments marked `structure_type: independent_weekly_pools` (Weekly Winners — Underdog only exposes the current week's round UUID; future weeks' IDs don't exist until they go live). The extension is expected to resolve TBD rounds at runtime via Underdog's API.

### Payout-table structure

The validator flags overlapping tiers **within** a single payout table but **not across** tables. BBM7 intentionally stacks two tables (`qualifier_round` + `championship_round`) — a top-10 weeks 1–14 finish wins a qualifier-round prize *and* can win a championship-round prize. That's by design.

### Superflex tournaments

Underdog runs a family of Superflex tournaments with the same general 4-stage structure but different field sizes, entry caps, and payouts. We model **one** representative (`frenchie3_superflex_double_entry`) and rely on the extension's closest-match fallback (by field size + entry fee) for unmodeled variants. If a Superflex variant becomes important enough to model explicitly, add a new YAML.

## Field Empirics (Sprint 3b-1) — `R/field_empirics.R`

Layer B foundation. Reads scraped Underdog drafts (captured by the bbbrotk Chrome extension) and produces empirical distributions that Sprint 3b-6's synthetic field model will sample from: per-team position counts, stack patterns, and per-pick-slot ADP distributions. No simulation, no correlations — pure analysis of real picks.

### Inputs

`load_scraped_drafts()` reads the **privacy-stripped field corpus** committed in the `bestball-bro-data` repo:

```
bestball-bro-data/sources/field/boards_<date>.json
```

`.default_field_corpus_path()` resolves the newest `boards_*.json` (preferring an in-repo `inst/data/field_corpus/` if vendored, then the `bestball-bro-data` sibling repo); pass `export_path` to override. The corpus is opponent-only (hashed `user_id`, `is_owner` flag, `/v1/user` dropped) but shares the raw scraper schema, so the parser is unchanged. The current sample is ~51 drafts across Season / Weekly Winners / Eliminator / Superflex. The old raw `inst/data/scraped_drafts/udbb-scraper-*.json` export and its manual `-latest` copy step are **retired** (see the lineage note below).

> **One field-data ingestion path (the old raw lineage is retired, PR #38).** `load_scraped_drafts()` now resolves the **privacy-stripped** field corpus `bestball-bro-data/sources/field/boards_<date>.json` (opponent-only, hashed `user_id`, `is_owner` flag, `/v1/user` dropped), via `.default_field_corpus_path()`. It shares the raw scraper schema, so the parser is unchanged. The **old raw** `inst/data/scraped_drafts/udbb-scraper-*.json` export — un-stripped, **git-ignored, never committed** — is no longer referenced by any default and is slated for local deletion. Production was always on the committed clean digest `inst/data/field_targets/<slate>.json` (summary stats, no identifiers) via `deploy_ev_blocks.R::.resolve_field_targets`. See [ARCHITECTURE.md](ARCHITECTURE.md) ("Two scraper-ingestion lineages").

### API

```r
picks  <- load_scraped_drafts()                            # 1 row per pick
counts <- empirical_position_counts(picks)                 # per-slate position freqs
stacks <- empirical_stack_patterns(picks)                  # per-team stack signature
pdists <- empirical_pick_distributions(picks)              # per-pick-slot draftee distribution
summ   <- drafter_team_summary(picks)                      # 1 row per drafter
publish_field_empirics()                                   # writes 1 JSON per slate to build/feed/v2/field_empirics/
```

`load_scraped_drafts()` performs the two-hop join `pick.appearance_id → appearance.player_id → player.{first_name, last_name, position_name, team_id}` against the four slate catalogs hoisted into the export's `unkeyed[]` array. Free agents (`team_id = NULL`) come through as `NA` and are excluded from stack-pattern joins; they still count for position counts. Non-completed drafts (`draft_state != "completed"`) are skipped with a printed count.

The empirics filter to **QB/RB/WR/TE only** — Underdog's player catalog includes a handful of `CB`/`FB` rows that occasionally get drafted, but those four positions are the only ones any slate we model actually starts.

### Tournament-ID namespace caveat

The scraper hoists Underdog's UUID `tournament_id` onto every draft. Sprint 3a's `resolve_round_to_tournament()` returns this repo's internal slug (e.g. `"bbm7"`) — different namespace. We don't cross-check the two in 3b-1. Sprint 3b-7 will add a `resolve_underdog_uuid_to_tournament()` bridge in `tournament_loader.R`.

For the Weekly Winners slate, the scraper hoists the weekly-winner pool's id into the `tournament_id` slot — it is **not** a tournament UUID in the Sprint 3a sense. The empirics treat it as an opaque grouping key, which is fine; just don't pass it to the Sprint 3a tournament loader expecting a match.

### Output schema

`publish_field_empirics()` writes `build/feed/v2/field_empirics/<slate_id>.json` per slate. Local-only for now; CI integration comes when Layer B's daily pipeline lands.

```jsonc
{
  "slate_id": "a9c04e81-1ace-4b16-a31d-4c725a47f16f",
  "computed_at": "2026-05-28T01:23:45Z",
  "source_export_captured_at": "2026-05-27T22:43:46.468Z",
  "n_drafts_sampled": 20,
  "n_teams_sampled": 240,
  "n_tournament_unique": 5,
  "by_event_type": {
    "tournament": {"n_drafts": 20, "n_teams": 240}
  },
  "position_counts": {
    "QB": {"1": 3, "2": 98, "3": 133, "4": 6},
    "RB": {"4": 17, "5": 121, "6": 90, "7": 10, "8": 1, "9": 1},
    "WR": {"4": 1, "5": 5, "6": 27, "7": 133, "8": 62, "9": 10, "10": 2},
    "TE": {"1": 1, "2": 78, "3": 151, "4": 10}
  },
  "stack_patterns": {
    "mean_max_team_stack_depth": 3.05,
    "qb_stack_2plus_rate": 0.912,
    "qb_stack_3plus_rate": 0.562,
    "qb_stack_4plus_rate": 0.083,
    "mean_n_team_stacks_3plus": 1.21
  },
  "pick_distributions": [
    {"pick_overall": 1, "underdog_id": "...", "first_name": "Bijan",
     "last_name": "Robinson", "position_name": "RB",
     "n_times_drafted": 9, "mean_adp_at_pick": 1.5, "sd_adp_at_pick": 0.0},
    ...
  ]
}
```

QB-stack columns count the QB itself: `qb_stack_2plus` = QB + 1 PC, `qb_stack_3plus` = QB + 2 PCs, etc. "PC" means WR or TE on the same NFL team as the QB. Playoff game stacks (drafter rostered both sides of a Week 15–17 NFL game) need schedule data and are deferred to Sprint 3b-6.

## Install (dev)

```r
# In R, from the repo root
install.packages(c("devtools", "remotes"))
remotes::install_deps(dependencies = TRUE)
devtools::load_all()

# Try the working piece
bbm <- load_tournament("bbm7")
ww  <- load_tournament("weekly_winners_2026")

# Run tests
devtools::test()
```

## CI / publishing

`.github/workflows/build_feed.yml` runs on a schedule (daily during draft season, weekly during regular season) and on manual dispatch. On each run it:

1. Runs `publish_v2()` — blender + Monte Carlo simulator at `n_sims = 10000` — writing `build/deploy/v2/projections/<slate>.json` and `build/deploy/v2/draws/<slate>.parquet` and authoring `build/deploy/_meta.json` (season header + per-slate `v2_path` / `v2_sha256` / `v2_generated_at`).
2. Moves the parquet draws out of `build/deploy/v2/draws/` to `build/artifacts/v2-draws/` so they don't get pushed to gh-pages.
3. Pushes `build/deploy/` (only `v2/` + `_meta.json`) to the `gh-pages` branch of `bestball-bro-data` via `peaceiris/actions-gh-pages@v4` (`keep_files: true` to coexist with Clay/ETR/LegUp source feeds on the same branch).
4. Uploads `build/artifacts/v2-draws/*.parquet` as `v2-draws-<run-id>` via `actions/upload-artifact@v4` (90-day retention). Layer B's CI in Sprint 3 will fetch these.

Required secret: `BESTBALL_BRO_DATA_DEPLOY_KEY` in repo Settings → Secrets and variables → Actions. Generate a deploy key with write access to `bestball-bro-data` and paste the private half here.

## License

MIT.
