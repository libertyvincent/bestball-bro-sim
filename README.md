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
