# bestball-bro-sim

R sim project producing the JSON projection + tournament-building-block feed consumed by the **BestBall Bro** Chrome extension during live Underdog best ball drafts.

## Three related repos

- **bestball-bro-sim** (this repo) — Layer A (projections + season sims) and Layer B (tournament pre-compute). Publishes to `bestball-bro-data` via GitHub Actions.
- **[bestball-bro-data](https://libertyvincent.github.io/bestball-bro-data/)** — GitHub Pages JSON feed served to the extension.
- **bbbro** (Chrome extension) — consumes the feed via `projectionFetch.js`, joins to live Underdog appearances in `matchPlayer.js`, computes live pick EV in `dataJoin.js`.

## Design docs

The design lives in three docs at the repo root:

- [LAYER_A.md](LAYER_A.md) — projection + season simulation methodology
- [LAYER_B.md](LAYER_B.md) — tournament simulation engine (stage-based, config-driven)
- [FEED_SPEC.md](FEED_SPEC.md) — wire format between this project and the extension

Read these before changing anything structural.

## Status

**Pre-alpha.** Working today: tournament config loading + validation (`R/tournament_config.R` + tests). Valid YAML configs for BBM and Weekly Winners under `inst/tournaments/`. Everything else is stubbed pending implementation per LAYER_A.md / LAYER_B.md decisions.

Implementation milestones in roughly the right order:

1. `R/data_pull.R` — wrap `nflreadr` loaders with caching ✓ (skeleton)
2. `R/projections.R` v0 — naive consensus-only projections (ffanalytics passthrough) to ship a feed shape
3. `R/publish.R` — JSON writer matching FEED_SPEC, push to bestball-bro-data via CI
4. Extension changes to consume the new feed (separate repo)
5. `R/projections.R` v1 — real Layer A methodology (top-down + component + comparables)
6. `R/simulate.R` — Monte Carlo season sims with correlation structure
7. `R/stage_engine.R` — Layer B implementation
8. Backtest pipeline against BBM3–6
9. Production scheduling

## Layout

```
R/
  tournament_config.R    # YAML loader + validator (WORKING)
  data_pull.R            # nflverse + Underdog ADP loaders (partial)
  projections.R          # Layer A (stub)
  simulate.R             # Monte Carlo season sim (stub)
  stage_engine.R         # Layer B (stub)
  publish.R              # JSON + parquet feed writer (stub)

inst/
  tournaments/           # Per-tournament stage configs
    bbm_2026.yaml
    weekly_winners_2026.yaml
  scoring/               # Per-scoring-system definitions
    half_ppr_underdog.yaml
  adjustments/           # User "knowing ball" overrides
    2026.yaml
  anchors/               # Anchor players for validation
    2026.yaml

tests/                   # testthat tests
.github/workflows/       # CI: scheduled feed rebuilds + publish to bestball-bro-data
```

## Blender (v2) — `R/blender.R`

v2 replaces the v1 nflverse weekly→season retrofit with a **blended consensus** built from three published source feeds. Per slate, the blender:

1. Loads the slate's player universe from `inst/data/slates/<slate_id>.csv` (canonical UUIDs).
2. Loads three source feeds from `https://libertyvincent.github.io/bestball-bro-data/sources/` (cached by URL sha256 under `~/.bestball-bro/cache`):
   - **Clay** — half-PPR point projections with full stats (`clay_2026_offense.json`)
   - **ETR** — Underdog-slate rankings, 300 players (rank-only)
   - **LegUp** — Underdog-slate rankings (rank-only)
3. Fits a per-position **calibration curve** (`R/calibration.R`) — natural spline through Clay's `(rank, half_ppr_points)` so rank-only sources convert to point-equivalents.
4. For each slate player, joins by Underdog UUID (ETR / LegUp) or by normalized name + nflverse team + position (Clay), computes a **weighted consensus mean**, **cross-source disagreement std**, and **aleatoric per-week std** (`cv * weekly_mean`).
5. Generates per-week mean + std + percentiles by applying Clay's `weekly_team_scoring.json` as a per-week team-output multiplier. Opponent and home/away come from the same file (Clay carries the schedule).
6. Writes `v2/projections/<slate_id>.json`.

Top-level entry: `publish_v2()`. v1 (`publish_projections()` / the existing CI flow) keeps running in parallel — the extension switches over in a later sprint.

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

## Install (dev)

```r
# In R, from the repo root
install.packages(c("devtools", "remotes"))
remotes::install_deps(dependencies = TRUE)
devtools::load_all()

# Try the working piece
bbm <- load_tournament_config("bbm_2026")
ww  <- load_tournament_config("weekly_winners_2026")

# Run tests
devtools::test()
```

## CI / publishing

`.github/workflows/build_feed.yml` runs on a schedule (daily during draft season, weekly during regular season) and on manual dispatch. On each run it:

1. Builds v1 projections (`generate_projections` per slate → `publish_manifest`) and stages them under `build/deploy/v1/`.
2. Runs `publish_v2()` — blender + Monte Carlo simulator at `n_sims = 10000` — writing `build/deploy/v2/projections/<slate>.json` and `build/deploy/v2/draws/<slate>.parquet` and updating `build/deploy/_meta.json`.
3. Moves the parquet draws out of `build/deploy/v2/draws/` to `build/artifacts/v2-draws/` so they don't get pushed to gh-pages.
4. Pushes `build/deploy/` to the `gh-pages` branch of `bestball-bro-data` via `peaceiris/actions-gh-pages@v4` (`keep_files: true` to coexist with Clay/ETR/LegUp source feeds on the same branch).
5. Uploads `build/artifacts/v2-draws/*.parquet` as `v2-draws-<run-id>` via `actions/upload-artifact@v4` (90-day retention). Layer B's CI in Sprint 3 will fetch these.

Required secret: `BESTBALL_BRO_DATA_DEPLOY_KEY` in repo Settings → Secrets and variables → Actions. Generate a deploy key with write access to `bestball-bro-data` and paste the private half here.

## License

MIT.
