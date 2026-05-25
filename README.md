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
season_percentiles = qnorm(p, mean = season_mean, sd = season_std)
```

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

`.github/workflows/build_feed.yml` runs on a schedule (daily during draft season, weekly during regular season) and on manual dispatch. On success it commits regenerated JSON/parquet artifacts to `bestball-bro-data` via a deploy key.

Required secret: `BESTBALL_BRO_DATA_DEPLOY_KEY` in repo Settings → Secrets and variables → Actions. Generate a deploy key with write access to `bestball-bro-data` and paste the private half here.

## License

MIT.
