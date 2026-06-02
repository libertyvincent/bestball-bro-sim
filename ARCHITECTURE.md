# BestBall Bro — Architecture Map (two repos, three feed generations)

_Companion to HANDOFF.md. Written to untangle how `bestball-bro-sim`, `bestball-bro-data`, and the extension fit together._

## The two repos, in one line each

- **bestball-bro-data** (Python + static JSON) — **ingestion.** Parses Mike Clay's ESPN PDF and emits both the legacy extension feed and the source feeds the sim consumes. Served via GitHub Pages / raw / jsDelivr.
- **bestball-bro-sim** (R) — **modeling.** Reads the data repo's source feeds, produces blended projections + Monte Carlo draws + tournament EV, and deploys its outputs back to the data repo's `gh-pages` branch.

So: **data ingests, sim models, sim deploys back into data.**

## The three feed generations

| Gen | Built by | Lives at | Consumed by | Status |
|---|---|---|---|---|
| **v0** — Clay legacy | `build.py` (Python, **data** repo) | `projections/nfl_2026.json` on **main** | **The extension** (`use_newfeed:false`) | Working, in production |
| **v1** — nflverse | `R/projections.R` + `R/rookies.R` (**sim** repo) | `v1/projections/*.json` on `gh-pages` (+ stale copies on data's `main`) | Nobody | Broken, dead → **delete** |
| **v2** — blended consensus | `R/blender.R` + `R/simulate.R` (**sim** repo) | `v2/projections/*.json` on `gh-pages`; parquet draws as GitHub Actions artifacts (too large for Pages) | Layer B (draws); intended extension feed | The keeper |

### A terminology trap: "v0" means two different things

Watch this, because it's the main source of confusion. "v0" is overloaded:

- **Cross-repo (this map's usage):** v0 = the Clay `build.py` legacy feed in the data repo — the live one the extension consumes.
- **Sim-repo-internal history:** "v0" was an early, now-dead stage of `R/projections.R` (a naive prior-season passthrough). Its fingerprint survives in a stray file on the data repo's `main`: `v1/projections/nfl_2026.json` carries `methodology: "v0_prior_season_passthrough"` — an old sim-v0 artifact sitting in the v1 folder.

So when talking cross-repo, "v0" = Clay/`build.py`. When reading the sim repo's own README/history, "v0" = the dead passthrough. Don't let the two collide. (In current sim terms there is no live "v0" — the sim's pipelines are v1 nflverse and v2 blended.)

## Data flow

```
bestball-bro-data (Python = ingestion)            bestball-bro-sim (R = modeling)
  build.py ─► projections/nfl_2026.json (v0) ───────────────────────► extension   [LIVE now, use_newfeed:false]
  build.py ─► sources/clay_2026_*.json ─┐
  build_etr.py  ─► sources/etr ─────────┼─► blender ─► v2/projections + draws ─► Layer B ─► extension  [target]
  build_legup.py─► sources/legup ───────┘
  (sim CI) ──────────────────────────────► v1/projections (nflverse) ─► nobody   [DELETE]
```

## Couplings worth burning into memory

- **v2 depends on v0's `build.py`.** The blender eats the Clay/ETR/LegUp **source feeds** that `build.py` + `build_etr.py` + `build_legup.py` produce. Killing or breaking `build.py` starves v2. This is why "preserve v0 **and** v2, delete v1" is the correct framing — v0 isn't only the legacy extension feed, it's also v2's raw material.
- **The sim deploys to data's `gh-pages`** (peaceiris, `keep_files:true`), not `main`. The extension reads v0 from **`main`**. So `main` carries v0 + some stale committed v1 artifacts; `gh-pages` carries the sim's v1/v2 projection JSON + the source feeds. The v2 **parquet draws** are the exception — too large for Pages, so CI moves them to GitHub Actions artifacts rather than gh-pages.
- **`_meta.json` is the new-feed index.** `use_newfeed:false` → the extension reads v0 directly. `use_newfeed:true` → the extension walks `_meta.json`, which currently points at **v1** (the broken feed) — almost certainly why the flag is off. Target cutover: `_meta` registers **v2** → flip `use_newfeed:true` → extension consumes v2, with v0 as the fallback until v2 is proven.

## Intended end-state (after v1 retirement)

One clean line: **Clay/ETR/LegUp sources → v2 blend → extension**, with v0 as backup. The current mess is sediment from three generations layered over time; retiring v1 removes the dead middle layer and collapses the picture.
