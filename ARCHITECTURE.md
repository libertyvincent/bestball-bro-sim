# BestBall Bro — Architecture Map (two repos, three feed generations)

_Written to untangle how `bestball-bro-sim`, `bestball-bro-data`, and the extension fit together._

## The two repos, in one line each

- **bestball-bro-data** (Python + static JSON) — **ingestion.** Parses Mike Clay's ESPN PDF and emits both the legacy extension feed and the source feeds the sim consumes. Served via GitHub Pages / raw / jsDelivr.
- **bestball-bro-sim** (R) — **modeling.** Reads the data repo's source feeds, produces blended projections + Monte Carlo draws + tournament EV, and deploys its outputs back to the data repo's `gh-pages` branch.

So: **data ingests, sim models, sim deploys back into data.**

## The three feed generations

| Gen | Built by | Lives at | Consumed by | Status |
|---|---|---|---|---|
| **v0** — Clay legacy | `build.py` (Python, **data** repo) | `projections/nfl_2026.json` on **main** | **The extension** (`use_newfeed:false`) | Working, in production |
| **v1** — nflverse | `R/projections.R` + `R/rookies.R` (**sim** repo) | — | Nobody | **Retired & deleted** (sim PR #23); stale copies may linger on data's `main` |
| **v2** — blended consensus | `R/blender.R` + `R/simulate.R` (**sim** repo) | `v2/projections/*.json` on `gh-pages`; parquet draws as GitHub Actions artifacts (too large for Pages); EV building-block artifacts (`v2/ev/*`) small enough for Pages | Layer B (draws); EV artifacts → the extension; intended extension feed | The keeper |

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
  build_etr.py  ─► sources/etr ─────────┼─► blender ─► v2/projections + draws ─► Layer B ─► v2/ev artifacts ─► extension  [target]
  build_legup.py─► sources/legup ───────┘
```

Layer B's outputs for the extension are the **EV building-block artifacts** (per-slate
path-aligned draws tensor + per-tournament advancement/payout curves) registered in
`_meta.json` — see `docs/ev_building_blocks_contract.md` and `FEED_SPEC.md`.

## Couplings worth burning into memory

- **v2 depends on v0's `build.py`.** The blender eats the Clay/ETR/LegUp **source feeds** that `build.py` + `build_etr.py` + `build_legup.py` produce. Killing or breaking `build.py` starves v2. This is why "preserve v0 **and** v2, delete v1" is the correct framing — v0 isn't only the legacy extension feed, it's also v2's raw material.
- **The sim deploys to data's `gh-pages`** (peaceiris, `keep_files:true`), not `main`. The extension reads v0 from **`main`**. So `main` carries v0 + some stale committed v1 artifacts; `gh-pages` carries the sim's v1/v2 projection JSON + the source feeds. The v2 **parquet draws** are the exception — too large for Pages, so CI moves them to GitHub Actions artifacts rather than gh-pages.
- **`_meta.json` is the new-feed index, and it is v2-authored.** `publish_v2()` creates/updates it with per-slate `v2_path` / `v2_sha256`; `publish_ev_blocks()` adds the EV artifact keys (`v2_draws_*`, per-tournament `curves_*`). `use_newfeed:false` → the extension reads v0 directly. Target cutover: flip `use_newfeed:true` → extension consumes v2 + the EV artifacts, with v0 as the fallback until v2 is proven.

## Current end-state (v1 retired)

One clean line: **Clay/ETR/LegUp sources → v2 blend → sim Layer B → v2/ev artifacts → extension**, with v0 as backup. v1 was retired and deleted from the sim repo (PR #23); the remaining cleanup is scrubbing its stale artifacts from the data repo's `main`.

## Availability model — draw-level game-zeroing (v2, PR #36)

Position-level injury attrition is priced as a **per-(path, player, week) missed-week mask**, not a points-level scale. This replaced PR #34's mean-level `availability_adjustment` (see `DRAW_ZEROING_DESIGN.md` for the design record; `FEED_SPEC.md` for the wire contract).

- **Rate (per player):** `availability_p_miss = max(0, 1 − expected_games[pos] / clay_games_i)`; each active week is kept with `q_i = 1 − p_miss`. Priors live in `inst/adjustments/availability.yaml` (QB 16.8 / RB 15.2 / WR 16.5 / TE 16.6 games of 17). The clamp carries PR #34's no-double-discount (a player Clay already projects below the prior gets `p_miss = 0`). `_meta.availability_mechanism = {type: "draw_zeroing", missed_week_model: "iid_bernoulli", expected_games, p_miss_canonical_17g}`.
- **Mask applied post-inverse-CDF, from one source.** The blended points and calibration curve stay **unscaled (conditional-on-playing)**; the parquet draws stay conditional (0 only on byes). The mask is realized **independently** in every scoring consumer, all reading the same per-player `availability_p_miss`: (a) the projections season-stat recompute (`R/simulate.R`), (b) the Artifact A tensor build (`R/ev_blocks.R`), (c) the shipped tournament-curve field build (`R/tournament_ev.R` via `deploy_ev_blocks.R`), and (d) the BBMDB/xAdv validator (`R/bbmdb_validator.R`). The shared masker is `R/correlation.R::.mask_matrix_list`. **Primitives are mask-exempt by contract** — `sample_correlated_draws` / `optimize_lineup_totals` / `precompute_layerA_marginals` operate on conditional draws; the mask is applied to their inputs/outputs by callers (this is what keeps it off the parquet and out of the copula's inverse-CDF map). The consumer audit (PR #36 review) confirmed all four scoring paths mask-consistent from one source; `simulate_team_season`/`simulate_teams` carry the opt-in param but have no shipped caller (dormant).
- **`_meta` key rename propagated to readers.** The `availability_adjustment` → `availability_mechanism` rename was carried into the one downstream reader of that key: `diagnostics/te_correlation_check.R` (its `availability_on` build flag now reads `availability_mechanism`), fixed alongside the rename so no reader is left pointing at the dead key.

## Settled findings (don't re-litigate)

- **TE cross-correlation implemented to intent (PR #35).** The live tensor's copula carries Option A (team 0.45 / game 0.25 / cross 0.05) to within `|Spearman − target| ≤ 0.004` in every bucket; TE×TE cross = 0.047. Decision-robust. Source: `diagnostics/te_correlation_findings.md`.
- **§9 4th-RB verdict: RB-light/TE-heavy stands.** On the honest (attrition-priced) feed, the 4th RB does **not** beat its TE/WR-depth alternative at the contested slot — clears at **0/3 seats** on both puppy2 and dachshund, robust across **both** build archetypes (Robust-RB and balanced). Draw-zeroing's insurance channel demonstrably fires (RB4 leads the room more when a starter is zeroed) but isn't large enough to move the pick; the beating alternative is TE/WR-**depth as a class**, not specific players. **Why RB4 is inert:** best-ball's max-of-room already harvests the RB4 ceiling automatically in the weeks it matters (there is no separate "start him" decision to capture — the optimizer takes the room's weekly max), and the RB4's lineup start-frequency does not diverge enough from the alternative's to add EV. So pricing the insurance channel makes RB4's value *visible* but confirms it is already absorbed; the marginal goes to the thinner TE/WR room. Diagnose-only harnesses: `build/adjudicate_rb4*.R`.
- **Honest empirical edge (hub field-edge analysis):** the model's optimal construction is reported to beat a human-like field by **~79–101 BBP/roster**. _Recorded per the hub; the in-repo artifact for this figure was not located during the docs pass — treat as a hub finding pending a cite._

## Two scraper-ingestion lineages (do not conflate)

- **New (stripped) — the field-calibration corpus:** `bestball-bro-data/sources/field/boards_<date>.json`, produced by that repo's `strip_field_export.py` (opponent-only; `user_id` hashed; `is_owner` flag; `/v1/user/*` account envelopes dropped). Per the data-repo pipeline it also applies a CB→WR position remap, carries a June-snapshot caveat, and splits ADP σ by slate. _(These specifics live in `bestball-bro-data`, not this repo — described per that pipeline's contract; verify there if precise behavior matters.)_ In **this** repo, the CI-safe summary of the validated field is the committed digest `inst/data/field_targets/<slate>.json` (position means, stack rate, per-slot ADP σ — no raw identifiers).
- **Old (raw) — the `inst/data` scraper export: RETIRED (PR #38).** `inst/data/scraped_drafts/udbb-scraper-*.json` was un-stripped (raw `user_id` UUIDs + `/v1/user` envelopes), **git-ignored** (`.gitignore:51`), never committed/in history. `load_scraped_drafts()` no longer references it — it now resolves the **stripped corpus** (`.default_field_corpus_path()` → newest `boards_*.json`), which shares the raw scraper schema so the parser is unchanged. Production was always on the committed digest, so nothing shipped changed. The orphaned raw working-tree files are slated for local deletion (gitignore stays; no history remediation — never tracked). **One ingestion path, the stripped corpus.**
