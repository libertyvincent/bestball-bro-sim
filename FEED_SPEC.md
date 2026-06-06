# Feed Specification

## Purpose

The Feed Specification defines the wire format that bridges Layer A's projection output and Layer B's offline pre-computed building blocks to Layer C's consumption in the BestBall Bro Chrome extension.

The feed is published to `https://libertyvincent.github.io/bestball-bro-data/`, fetched by `projectionFetch.js` daily (24h IndexedDB cache), and consumed downstream by `matchPlayer.js` → `dataJoin.js`.

This document specifies file structure, schemas, versioning, joining strategy, and migration path.

---

## File structure

The feed is a directory tree at the GitHub Pages root. Files are versioned by season and (where appropriate) by tournament.

What the sim publishes today (to the data repo's `gh-pages` branch):

```
bestball-bro-data/  (gh-pages)
├── _meta.json                              # Multi-slate manifest — discovered first, drives everything else
├── sources/                                # Clay/ETR/LegUp source feeds (built by the data repo; the blender's input)
└── v2/
    ├── projections/
    │   ├── nfl_2026_season.json            # Layer A — one file per Underdog slate (slate CSV is canonical universe)
    │   ├── nfl_2026_eliminator.json
    │   ├── nfl_2026_weekly_winners.json
    │   └── nfl_2026_superflex.json
    └── ev/                                 # Layer B — EV building-block artifacts (per the frozen contract)
        ├── nfl_2026_season_draws.bin       #   Artifact A: path-aligned int16 joint-draws tensor (~5 MB at N=500)
        ├── nfl_2026_season_draws.json      #   Artifact A sidecar (player index, axis metadata, lineup_spec)
        ├── puppy2_curves.json              #   Artifact B: one curves file per tournament on the slate
        └── dachshund_curves.json
```

The v2 parquet draws are deliberately **not** here — they ship as GitHub Actions artifacts (see "v2 outputs" below).

The extension fetches `_meta.json` first to discover available files and detect updates, then fetches the file paths listed there. This avoids hardcoding paths in the extension. EV artifacts are looked up per active draft straight from `_meta.json`: the slate entry carries the draws paths, and its `tournaments` map (keyed by `tournament_id`, resolvable from the draft's `round_id` via the tournament configs) carries each tournament's curves path. Adding a new tournament is publishing one curves file and re-running the publisher — no extension change.

`[DECISION 1]` Single combined feed vs. multiple files?

- **Single combined file:** Easier consumption (one fetch). Larger payload (~30–50MB combined once all tournaments are included).
- **Multiple files:** Smaller individual fetches, can update independently (projections daily, per-tournament building blocks weekly, tournament configs rarely). More fetches but only loads what's needed for active drafts.

*Recommendation:* **Multiple files.** Update cadence differs per file type; the extension only needs to fetch building_blocks for tournaments the user is actually drafting; the 24h IDB cache handles bandwidth.

---

## Versioning

Each file carries `_meta.model_version` (semver). The extension surfaces a warning in DevTools console (not user-facing UI) if `projections.model_version` and `building_blocks.model_version` are incompatible per the version matrix below.

Compatibility:

| Projections | Building Blocks | Status |
|-------------|-----------------|--------|
| 0.x.y       | 0.x.z           | Compatible (pre-release, expect breakage) |
| 1.x.y       | 1.x.z           | Compatible |
| 1.x.y       | 2.x.z           | Incompatible — extension warns, falls back to projections-only mode |

`_meta.json` at root is the multi-slate manifest, authored by `publish_v2()` (via `.update_root_meta_for_v2()`). Each slate entry registers the v2 projection feed:

```jsonc
{
  "season": 2026,
  "generated_at": "2026-08-15T03:00:00Z",
  "slates": {
    "nfl_2026_season": {
      "underdog_slate_id": "a9c04e81-1ace-4b16-a31d-4c725a47f16f",
      // Layer A (written by publish_v2)
      "v2_path":           "v2/projections/nfl_2026_season.json",
      "v2_sha256":         "...",
      "v2_generated_at":   "2026-08-15T03:05:00Z",
      // Layer B EV building blocks (written by publish_ev_blocks)
      "v2_draws_path":           "v2/ev/nfl_2026_season_draws.bin",
      "v2_draws_sha256":         "...",
      "v2_draws_sidecar_path":   "v2/ev/nfl_2026_season_draws.json",
      "v2_draws_sidecar_sha256": "...",
      "v2_draws_generated_at":   "2026-08-15T03:20:00Z",
      "tournaments": {
        // underdog_tournament_id == the UUID a live draft exposes as `source_id`,
        // so the extension maps source_id -> tid -> curves deterministically.
        "puppy2":    { "underdog_tournament_id": "e9f88543-f815-4db2-a076-1271fb35160c", "title": "The Puppy 2",  "curves_path": "v2/ev/puppy2_curves.json",    "curves_sha256": "..." },
        "dachshund": { "underdog_tournament_id": "1f35c88b-e5c4-4b8c-b42a-db74f55d7a18", "title": "The Dachshund", "curves_path": "v2/ev/dachshund_curves.json", "curves_sha256": "..." }
      }
    }
    // one entry per slate enabled in inst/data/slates/_manifest.yaml
  }
}
```

`.update_root_meta_for_v2()` creates `_meta.json` from scratch when it doesn't exist (filling `season` from the slate manifest) and updates it in place otherwise, preserving fields written by other publishers. The extension reads the file blind and picks the feed by which fields exist; entries on the live `gh-pages` `_meta.json` may also carry legacy v1 fields (`path` / `version` / `sha256`) from before the v1 pipeline was retired — those go away on the next data-repo cleanup.

The `slates` map is open-ended — adding a new slate means dropping its CSV in `inst/data/slates/`, flipping `enabled: true` in the manifest YAML, and the next build produces a new entry here automatically.

### v2 outputs (settled)

`publish_v2()` writes, per enabled slate:

- **`v2/projections/<slate_id>.json`** — blended-consensus projections (Clay/ETR/LegUp via `blend_slate()`) with **empirical percentiles** from the Monte Carlo season simulator (`simulate_slate()`, default 10,000 sims/player). Published to gh-pages and registered in `_meta.json` as above.
- **`v2/draws/<slate_id>.parquet`** — the full per-player per-week sim draws (long format: `underdog_id, sim_idx, week, draw_value`). **Server-side artifact only, never on gh-pages**: CI moves these out of the publish directory and uploads them as a GitHub Actions artifact (`v2-draws-<run-id>`, 90-day retention). They are not registered in `_meta.json`.

Separately, `publish_field_empirics()` writes **`v2/field_empirics/<slate_id>.json`** per slate (position-count, stack-pattern, and pick-slot distributions from scraped Underdog drafts) under the local build tree. These are local-only for now — not yet pushed to gh-pages and not registered in `_meta.json`.

Layer B's outputs ARE represented in `_meta.json`: `publish_ev_blocks()` (R/ev_blocks.R) writes the per-slate `v2_draws_*` keys and the per-tournament `tournaments.<id>.curves_*` keys shown above. Their wire format is frozen in [docs/ev_building_blocks_contract.md](docs/ev_building_blocks_contract.md); the "EV building-block artifacts" section below summarizes it.

---

## Projections schema

**The v1 projection feed (`v1/projections/<slate_id>.json`) is retired** — its nflverse pipeline was removed from the sim repo and CI no longer publishes it. The live projection feed is **v2** (`v2/projections/<slate_id>.json`), produced by the blender + Monte Carlo simulator; see "v2 outputs (settled)" above and the Blender / Monte Carlo sections of the sim repo's README for what it contains.

The formal v2 consumption contract (required vs optional fields from the extension's point of view) is specified as part of the extension cutover sprint, not here.

The slate CSV remains the canonical player universe — every UUID in the CSV gets a row in the feed.

### Correlation representation

`[DECISION 2]` How do we represent correlations?

- **Sparse list per player (above):** each player carries a list of pairs they correlate with. Symmetric data duplicated. Easy to consume.
- **Sparse matrix in `_meta`:** single global sparse correlation matrix, players reference by index. No duplication.
- **Type-based defaults + overrides:** for each `correlation_type` ("same_team_qb_wr"), a global default rho; per-player overrides only when meaningful.

*Recommendation:* **Sparse list per player.** Duplicated data is fine at this scale (~300 players × ~5 meaningful correlations × small payload). Direct consumption — no joins, no indexes. The extension's existing `dataJoin.js` pattern handles it naturally.

---

## Sim draws schema (`v2/draws/<slate_id>.parquet`)

Parquet, written by `publish_v2()`, one file per slate. **Server-side artifact only** (GitHub Actions artifact, never gh-pages — see "v2 outputs" above).

Schema (long format, the actual columns):

| Column | Type | Notes |
|--------|------|-------|
| `underdog_id` | string | join key |
| `sim_idx` | int | 1 .. n_sims |
| `week` | int | 1 .. 18 |
| `draw_value` | double | half-PPR points for that (player, sim, week); 0 on byes |

These are **Layer A (per-player independent) draws** — cross-player correlation is induced downstream by `sample_correlated_draws()` (R/correlation.R). The `sim_idx` axis is NOT a shared "world" across players; do not treat it as one. The path-aligned, correlated representation the extension consumes is Artifact A below.

Consumers: Layer B offline (R) only. The extension does **not** fetch this file.

---

## EV building-block artifacts (`v2/ev/*`)

> The original "building blocks" design (replacement levels / scarcity curves /
> leverage scores / payout curve, under a `v1/building_blocks/` namespace) was
> **superseded** by the EV building-blocks contract before it ever shipped. The
> frozen interface is [docs/ev_building_blocks_contract.md](docs/ev_building_blocks_contract.md);
> the implementation is `R/ev_blocks.R` (publisher: `publish_ev_blocks()`).
> Summary of what's actually on the wire:

**Artifact A — joint draws (per slate).** Two files:

- `v2/ev/<slate_id>_draws.bin` — raw little-endian `int16` tensor, axis order
  `[path][player][week]` (path-major; a path's full board is contiguous),
  scores ×10 (0.1-pt resolution). ~5 MB at the shipped N = 500 paths.
- `v2/ev/<slate_id>_draws.json` — sidecar:

```jsonc
{
  "slate_id": "nfl_2026_season",
  "dtype": "int16", "endianness": "little",
  "axis_order": ["path", "player", "week"],
  "n_paths": 500, "n_players": 295, "n_weeks": 17,
  "weeks": [1, 2, ..., 17],
  "quant_scale": 10,
  "player_index": { "<underdog_id>": 0, ... },   // 0-based tensor row index
  "positions":    { "<underdog_id>": "RB", ... },
  "lineup_spec": {                               // the slate's starting lineup
    "slate_id": "nfl_2026_season",
    "slots": [
      { "pos": "QB", "n": 1, "eligible": "QB" },
      { "pos": "RB", "n": 2, "eligible": "RB" },
      { "pos": "WR", "n": 3, "eligible": "WR" },
      { "pos": "TE", "n": 1, "eligible": "TE" },
      { "pos": "FLEX", "n": 1, "eligible": ["RB", "WR", "TE"] }
    ]
  },
  "generated_at": "..."
}
```

The sidecar's `lineup_spec` (slate starting lineup) plus the curves file's
`stage_weeks` are exactly what the extension's round-score assembler consumes to
turn the tensor into per-round scores — the feed is self-describing. Single-slot
`eligible` is a scalar, multi-position (FLEX) an array; the shape equals
`load_slate_lineup_spec()` and the golden combine fixture's `inputs.lineup_spec`.

**Hard invariant:** path *i* is the same simulated world for every player — the
shared path axis is the correlation mechanism. The sim guarantees it at build
time (one joint correlated draw) and tests it (stack correlation + structural
no-reshuffle); the extension must never reorder paths per player.

**Artifact B — curves (per tournament).** `v2/ev/<tournament_id>_curves.json`:

```jsonc
{
  "tournament_id": "puppy2",
  "slate_id": "nfl_2026_season",
  "stage_weeks": { "r1": [1, ..., 14], "r2": [15], "r3": [16], "r4": [17] },
  "structure":   { "pod_sizes": [12, 10, 5, 750], "seats": [225000, 37500, 3750, 750], "advance_n": [2, 1, 1] },
  "built_from":  { "n_field": 2700, "n_sims": 400 },
  "curves": {
    "g1":        { "x": [...], "y": [...] },   // P(advance qualifier | R1)
    "g2":        { "x": [...], "y": [...] },   // P(advance QF | R2)
    "g3":        { "x": [...], "y": [...] },   // P(advance SF | R3)
    "payout_qf": { "x": [...], "y": [...] },   // E[$ | exit at QF with R2]
    "payout_sf": { "x": [...], "y": [...] },   // E[$ | exit at SF with R3]
    "h_final":   { "x": [...], "y": [...] }    // E[$ | finalist with R4]
  },
  "generated_at": "..."
}
```

Curves are lookup tables — evaluate with linear interpolation, clamped at the
endpoints. The extension's per-path combine is:

```
$ = g1(R1) · [ (1−g2(R2))·payout_qf(R2) + g2(R2)·(1−g3(R3))·payout_sf(R3) + g2(R2)·g3(R3)·h_final(R4) ]
```

EV = mean over paths; marginal EV of a candidate = EV(roster + X) − EV(roster)
on the **same paths** (common random numbers). The R reference implementation
(`roster_round_scores()` / `evaluate_roster_curve_ev()` / `rank_marginal_ev()`)
is the executable spec.

**Known caveats for the extension build** (from the Track-1 validation, sim PR #27):

1. Naive marginal EV is degenerate on partial rosters (early/mid picks) — the
   extension needs ghost-pick roster completion before the EV ranking replaces VOR.
2. The curves over-price high-variance/unique rosters (+$1.87 mean on a $4.44
   field-mean EV); chalky rosters are priced to within pod noise. v1 ships with
   this caveat; uniqueness-conditioned curves are the v2 remedy.

### Curve provenance: the field-targets digest

The Artifact B curves are built from a synthetic field ([generate_field()]),
whose shape depends on the scraped Underdog draft exports
(`compute_field_targets()` → per-position mean counts + per-slot ADP sigma).
Those exports are large, local-only, and gitignored, so they are **absent in
CI**. To keep the curves CI publishes equal to the ones validated locally (the
#30 fixture, the oracle re-gate — both built from the scraped field), the
summary stats (means/rates/per-slot sigma, **never the raw drafts**) are
committed as a **field-targets digest** at
`inst/data/field_targets/<slate_id>.json`. The publisher resolves field
targets **scraped drafts (local) → committed digest (CI) →
`.default_field_targets()` (ADP-only, last resort)**; with the digest present,
CI rebuilds the scraped field bit-for-bit (curves identical to within 0;
verified by `inst/scripts/gate_curve_fidelity.R` and
`tests/testthat/test-field-targets-digest.R`).

The digest is a **snapshot** — regenerate it whenever drafts are re-scraped:

```
Rscript inst/scripts/refresh_field_targets_digest.R   # then commit inst/data/field_targets/*.json
```

(Longer term the extension/scraper-observes-drafts loop could publish the
digest automatically.)

---

## Tournament configs (sim-side YAML, not published)

Tournament rules (stages, pods, payouts, round UUIDs) live as YAML under the sim
repo's `inst/data/tournaments/`, loaded and validated by `R/tournament_loader.R`.
They are **not published to the feed**: everything the extension needs from a
tournament's structure is baked into its Artifact B curves file
(`stage_weeks`, `structure`, and the six curves — see "EV building-block
artifacts" above). Round-ID → tournament resolution also happens sim-side at
publish time; the extension only needs the `tournaments` map in `_meta.json`.

---

## Scoring schema (`v1/scoring/half_ppr_underdog.json`)

```jsonc
{
  "name": "half_ppr_underdog",
  "version": "1.0.0",
  "rules": {
    "passing_yards":     0.04,
    "passing_tds":       4.0,
    "interceptions":    -1.0,
    "rushing_yards":     0.1,
    "rushing_tds":       6.0,
    "receptions":        0.5,
    "receiving_yards":   0.1,
    "receiving_tds":     6.0,
    "fumbles_lost":     -2.0,
    "two_point":         2.0,
    "te_premium":        0.0
  }
}
```

Verified against the extension's `scoring_type_id` mapping at runtime. Mismatch → extension logs warning and uses Underdog's scoring per `appearances.projection.points`.

---

## Joining to Underdog appearances

The extension's existing `BBBRO_MATCH` module joins feed records to Underdog appearance records. Today the join is `name|TEAM|pos` with `OVERRIDES` for known mismatches. With the new feed, we gain `underdog_id` as a direct join key.

**New join order in `matchPlayer.js`:**

1. **Primary:** `appearance.player_id === player.underdog_id` — direct UUID match, perfect when both sides have it.
2. **Secondary:** `name|TEAM|pos` — current Phase 8 logic, used when `underdog_id` is missing from the feed (early-season rookies, mid-season trades the feed hasn't picked up yet).
3. **Tertiary:** name-only + position uniqueness — current fallback.
4. **Quaternary:** lastname + position + team disambiguation — current fallback.
5. **OVERRIDES table** — true name-spelling mismatches (Hollywood/Marquise, etc.).

`underdog_id` becoming the primary key eliminates ~80% of the OVERRIDES entries over time. We keep the existing fallback chain because Underdog occasionally assigns new UUIDs (re-signed players, rookies pre-rookie-camp, etc.) and the feed might lag.

---

## Tournament resolution (via `_meta.json`)

> The `v1/tournaments_index.json` + `building_blocks_url` + contest-title-matching
> design originally specced here was superseded along with the rest of the v1
> building-blocks namespace. Resolution now runs off identifiers, not titles.

The extension knows two identifiers for a live draft: the **slate UUID** and the
draft's **round UUID** (from the Underdog draft URL / API). Resolution:

1. **Slate:** match the draft's slate UUID against `slates.<slate_id>.underdog_slate_id`
   in `_meta.json` → that slate's `v2_path` (projections) and `v2_draws_*` (Artifact A).
2. **Tournament:** the sim-side configs map every tournament's per-stage
   `underdog_round_id` to its `tournament_id` (`resolve_round_to_tournament()`).
   The published curves files carry `tournament_id`, and `_meta.json`'s
   `slates.<slate_id>.tournaments` map is keyed by it. The extension ships (or
   fetches) the small round-UUID → tournament_id mapping and then loads
   `tournaments.<tournament_id>.curves_path`.
3. **No match:** log a warning, fall back to projection-only ranking (VOR +
   position rank) with no EV column.

### Adding new tournaments mid-season

1. Sim repo: add the tournament's YAML config (`inst/data/tournaments/<id>.yaml`,
   real fee/payouts/round UUIDs — see the Dachshund/Puppy 2 pattern).
2. Re-run the EV publisher → new `v2/ev/<id>_curves.json` + `_meta.json` entry.
3. Extension picks it up on the next `_meta.json` poll. No extension version bump.

---

## Update detection in extension

`projectionFetch.js` polls `_meta.json` on each draft-board load. If `_meta.generated_at` is newer than the cached version, it refetches the changed files (compared by `sha256`).

`[DECISION 5]` Polling cadence in the extension?

- **On each navigation to a draft URL:** Realistic; matches when freshness matters. Could hammer GitHub Pages if a user opens 10 drafts in 30 seconds.
- **24h TTL (current):** Predictable load. Misses same-day projection updates if the user's session is long.
- **Hybrid:** 24h TTL on file content; `_meta.json` always fetched (it's tiny). If meta changed, fetch the new files.

*Recommendation:* **Hybrid.** `_meta.json` is a few KB, polling it on every draft-board load is fine. Heavy file fetches only happen when meta indicates a change.

---

## Backward compatibility / migration

The current feed `libertyvincent.github.io/bestball-bro-data` is the Mike Clay half-PPR projections in some legacy shape. The new feed format is a breaking change.

`[DECISION 6]` Migration strategy?

- **Hard cutover:** publish new feed, ship new extension version, drop legacy.
- **Versioned URLs:** legacy at `bestball-bro-data/legacy/`, new at `bestball-bro-data/v1/`. Extension reads from `_meta.json` which lives at root and points to current version.
- **Polyfill:** publish *both* formats during a transition window; new extension reads new, old extension keeps working.

*Recommendation:* **Versioned URLs.** Cheapest version control, and the `_meta.json` at root means the extension's only hardcoded URL is the manifest. Everything else is discovered.

---

## Open decisions summary

| # | Decision | My pick | Outcome |
|---|----------|---------|---------|
| 1 | Feed file structure | Multiple files | Built that way (v2 projections + per-slate/per-tournament EV artifacts) |
| 2 | Correlation representation | Sparse list per player | Superseded: correlation ships implicitly via Artifact A's shared path axis |
| 3 | Publish sim_draws | Yes, for advanced consumers | Parquet draws are CI-artifacts only; the published correlated form is Artifact A |
| 4 | Marginal contribution granularity | Every 3 picks + interpolation | Superseded with the building-blocks design (curves + live combine instead) |
| 5 | Extension update detection | Hybrid: _meta on each load, files on sha change | Still the plan |
| 6 | Migration from current feed | Versioned URLs | Built that way (`v2/` namespace, `_meta.json` at root) |

---

## What this unlocks

Once these three docs (Layer A, Layer B, Feed Spec) are signed off, the code work splits cleanly:

- **R sim project (`bestball-bro-sim`):** implements Layer A + Layer B offline. Publishes to `bestball-bro-data`. Self-contained.
- **`bestball-bro-data` repo:** receives daily/weekly/triggered publishes from CI. GitHub Pages serves the files. No logic.
- **BestBall Bro extension:** `projectionFetch.js` grows to handle the new feed; `matchPlayer.js` adds the UUID-primary join; `dataJoin.js` implements live Layer B (the recommendation engine). Combo popup grows new columns. Nothing rebuilt — only extended.

That's the whole system.
