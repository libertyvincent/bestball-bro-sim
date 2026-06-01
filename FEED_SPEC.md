# Feed Specification

## Purpose

The Feed Specification defines the wire format that bridges Layer A's projection output and Layer B's offline pre-computed building blocks to Layer C's consumption in the BestBall Bro Chrome extension.

The feed is published to `https://libertyvincent.github.io/bestball-bro-data/`, fetched by `projectionFetch.js` daily (24h IndexedDB cache), and consumed downstream by `matchPlayer.js` → `dataJoin.js`.

This document specifies file structure, schemas, versioning, joining strategy, and migration path.

---

## File structure

The feed is a directory tree at the GitHub Pages root. Files are versioned by season and (where appropriate) by tournament.

```
bestball-bro-data/
├── _meta.json                              # Multi-slate manifest — discovered first, drives everything else
└── v1/
    ├── tournaments_index.json              # Underdog contest title → tournament_id mapping
    ├── projections/
    │   ├── nfl_2026_season.json            # Layer A — one file per Underdog slate (slate CSV is canonical universe)
    │   ├── nfl_2026_eliminator.json        # (added when the Eliminator slate is enabled in inst/data/slates/_manifest.yaml)
    │   ├── nfl_2026_weekly_winners.json
    │   └── nfl_2026_superflex.json
    ├── sim_draws/
    │   └── nfl_2026.parquet                # Layer A full sim draws (optional consumer; cross-slate for now)
    ├── tournaments/
    │   ├── bbm_2026.json                   # BBM stage config
    │   ├── bbm_superflex_2026.json
    │   ├── big_board_2026.json
    │   ├── puppy_2026.json
    │   └── weekly_winners_2026.json
    ├── building_blocks/
    │   ├── bbm_2026.json                   # Layer B output per tournament
    │   ├── bbm_superflex_2026.json
    │   ├── big_board_2026.json
    │   ├── puppy_2026.json
    │   └── weekly_winners_2026.json
    ├── teams/
    │   └── nfl_2026.json                   # 32 NFL teams (existing, unchanged)
    ├── scoring/
    │   └── half_ppr_underdog.json          # Scoring definition (referenced by tournament configs)
    └── payouts/
        ├── bbm_2026_finals.csv             # Detailed payout distributions
        └── ww_week_1.csv                   # (referenced from tournament configs by URL)
```

The extension fetches `_meta.json` first to discover available files and detect updates, then fetches the file paths listed there. This avoids hardcoding paths in the extension. Tournament configs and building blocks are looked up dynamically per active draft via `tournaments_index.json` — adding a new tournament is publishing two files and updating the index, no extension change.

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

`_meta.json` at root is the multi-slate manifest. Each slate entry carries the v1 fields (written by `publish_manifest()`) and, once `publish_v2()` has run, a v2 triple registered by `.update_root_meta_for_v2()`:

```jsonc
{
  "season": 2026,
  "generated_at": "2026-08-15T03:00:00Z",
  "slates": {
    "nfl_2026_season": {
      // v1 (publish_manifest)
      "underdog_slate_id": "a9c04e81-1ace-4b16-a31d-4c725a47f16f",
      "path":              "v1/projections/nfl_2026_season.json",
      "version":           "1.0.0",
      "sha256":            "...",
      // v2 (.update_root_meta_for_v2 — added per slate after publish_v2())
      "v2_path":           "v2/projections/nfl_2026_season.json",
      "v2_sha256":         "...",
      "v2_generated_at":   "2026-08-15T03:05:00Z"
    }
    // future slates (nfl_2026_eliminator, nfl_2026_weekly_winners,
    // nfl_2026_superflex) appear here once enabled in
    // inst/data/slates/_manifest.yaml.
  }
}
```

The v2 registration is additive — `.update_root_meta_for_v2()` preserves whatever the v1 path already wrote and only adds the `v2_path` / `v2_sha256` / `v2_generated_at` triple. The extension can read `_meta.json` blind and pick v1 or v2 by which fields exist.

The `slates` map is open-ended — adding a new slate means dropping its CSV in `inst/data/slates/`, flipping `enabled: true` in the manifest YAML, and the next build produces a new entry here automatically.

### v2 outputs (settled)

`publish_v2()` writes, per enabled slate:

- **`v2/projections/<slate_id>.json`** — blended-consensus projections (Clay/ETR/LegUp via `blend_slate()`) with **empirical percentiles** from the Monte Carlo season simulator (`simulate_slate()`, default 10,000 sims/player). Published to gh-pages and registered in `_meta.json` as above.
- **`v2/draws/<slate_id>.parquet`** — the full per-player per-week sim draws (long format: `underdog_id, sim_idx, week, draw_value`). **Server-side artifact only, never on gh-pages**: CI moves these out of the publish directory and uploads them as a GitHub Actions artifact (`v2-draws-<run-id>`, 90-day retention). They are not registered in `_meta.json`.

Separately, `publish_field_empirics()` writes **`v2/field_empirics/<slate_id>.json`** per slate (position-count, stack-pattern, and pick-slot distributions from scraped Underdog drafts) under the local build tree. These are local-only for now — not yet pushed to gh-pages and not registered in `_meta.json`.

Tournament configs and building-blocks files (Layer B output) are not represented in the current `_meta.json` schema — `publish_building_blocks()` is still a stub, and they re-enter when the Layer B building-block precompute ships. Their wire format is finalized in the EV-brain contract sprint, not here.

---

## Projections schema (`v1/projections/<slate_id>.json`)

One file per Underdog slate. `_meta` carries the slate identity; every
row in `players` is a row in the slate's Underdog CSV, projected by our
methodology — veterans via the nflverse weekly→season pipeline, rookies
via historical-draft-capital comparables. `underdog_projected_points`
is preserved as a reference / traceability field; it does NOT feed our
`season.mean`, `season.std`, `vor`, or anything else we compute.

```jsonc
{
  "_meta": {
    "slate_id":          "nfl_2026_season",
    "underdog_slate_id": "a9c04e81-1ace-4b16-a31d-4c725a47f16f",
    "display_name":      "NFL 2026 Season",
    "season":            2026,
    "scoring_id":        "half_ppr_underdog",
    "methodology":       "v1_nflverse_veterans_comparables_rookies",
    "generated_at":      "2026-08-15T03:00:00Z",
    "model_version":     "1.0.0",
    "player_count":      1449
  },
  "players": [
    {
      "underdog_id": "25cbfc55-cb8a-4589-85de-e91870f65952",
      "gsis_id":     "00-0037077",
      "name":        "Bijan Robinson",
      "team":        "ATL",
      "position":    "RB",
      "season": {
        "mean":   285.4,
        "std":    62.5,
        "median": 285.4,
        "percentiles": { "p10": 205.3, "p25": 243.1, "p50": 285.4, "p75": 327.7, "p90": 365.5, "p95": 387.9 }
      },
      "vor":                       84.2,
      "position_rank":             "RB1",
      "adp":                       1.5,
      "underdog_projected_points": 294.9
    }
    // ... player_count entries (~1449 for the Season slate)
  ]
}
```

### Required vs optional fields

**Required (extension parses these every load):**
- `underdog_id` — canonical primary key
- `name`, `team`, `position`
- `season.mean`, `season.std`
- `position_rank`, `vor`

**Optional (used by richer recommendation logic; absent = feature degrades gracefully):**
- `gsis_id` — present for veterans; **null for rookies** (no historical-stats match available). Extension can fall back to name-based joins.
- `season.percentiles` — full empirical / parametric distribution; falls back to Normal(mean, std) if absent.
- `adp` — taken from the slate CSV; null for free-agent rows Underdog hasn't priced yet.
- `underdog_projected_points` — Underdog's own projection for the slate, kept as a reference field for sanity comparison and divergence reporting. **Do NOT feed this back into ranking** — it is not our methodology and treating it as such would silently overwrite our projection.

The slate CSV is the canonical player universe — every UUID in the CSV gets a row in the feed, in the order yielded by our VOR sort.

### Correlation representation

`[DECISION 2]` How do we represent correlations?

- **Sparse list per player (above):** each player carries a list of pairs they correlate with. Symmetric data duplicated. Easy to consume.
- **Sparse matrix in `_meta`:** single global sparse correlation matrix, players reference by index. No duplication.
- **Type-based defaults + overrides:** for each `correlation_type` ("same_team_qb_wr"), a global default rho; per-player overrides only when meaningful.

*Recommendation:* **Sparse list per player.** Duplicated data is fine at this scale (~300 players × ~5 meaningful correlations × small payload). Direct consumption — no joins, no indexes. The extension's existing `dataJoin.js` pattern handles it naturally.

---

## Sim draws schema (`v1/sim_draws/nfl_2026.parquet`)

Parquet for efficiency (~80KB per player × 300 players × 10k draws would be ~240MB as JSON; ~20MB as Parquet).

Schema:

| Column | Type | Notes |
|--------|------|-------|
| `underdog_id` | string | join key |
| `sim_id` | int | 0 .. n_sims-1 |
| `season_points` | float | half-PPR season total |
| `games_played` | int | |
| `weekly_points` | list[float] | length 17 |

Joint correlations are preserved by `sim_id` — sim_id=0 across all players represents one self-consistent simulated season.

Consumers: Layer B offline (R), future DFS optimizer (whatever language). Extension does **not** fetch this file — it's for advanced tooling.

`[DECISION 3]` Publish sim_draws at all for v1, or defer?

*Recommendation:* **Publish.** It's the only way Layer B's offline pipeline can do anything sophisticated with correlations, and it costs almost nothing to write once the sim already ran.

---

## Building blocks schema (`v1/building_blocks/bbm_2026.json`)

Layer B's offline pre-computed outputs:

```jsonc
{
  "_meta": {
    "season": 2026,
    "tournament": "bbm_2026",
    "projections_version": "0.3.0",
    "model_version": "0.3.0",
    "generated_at": "2026-08-15T05:00:00Z",
    "n_mock_drafts": 10000,
    "n_season_sims_per_draft": 1000
  },
  "replacement_levels": {
    // position × roster_archetype × week → fantasy point threshold
    "QB":   { "zero_qb":     { "1": 12.4, "2": 13.1, ...}, "modal": { ... } },
    "RB":   { "zero_rb":     { "1": 7.8,  ...}, "anchor_rb": { ... }, "modal": { ... } },
    "WR":   { ... },
    "TE":   { ... }
  },
  "scarcity_curves": {
    // position × pick_number → marginal value vs. waiting
    "QB":   [ 0.0, 0.0, ..., 4.2, 5.1, ... ],   // index = pick_number (1..216)
    "RB":   [ ... ],
    "WR":   [ ... ],
    "TE":   [ ... ]
  },
  "reference_constructions": [
    {
      "id": "zero_rb_slot_3",
      "slot": 3,
      "round_position_priors": ["WR","WR","WR","RB","WR","TE","RB","QB", ...],
      "historical_advance_rate": 0.043,
      "weight": 0.18
    }
    // ... ~50 constructions (12 slots × ~4 archetypes each)
  ],
  "marginal_contributions": {
    // Keyed by (underdog_id, construction_id, pick_number)
    // For each cell: marginal advance probability of adding this player
    //                to this construction at this pick number
    "2b0cbe83-48a2-...": {
      "zero_rb_slot_3":   { "1": 0.0042, "13": 0.0031, "25": 0.0019, ...},
      "anchor_rb_slot_3": { "1": 0.0061, "13": 0.0044, ...}
    }
    // ... per player × construction × ~18 pick checkpoints
  },
  "leverage_scores": {
    // Per player: leverage score (0-1, higher = more uniqueness EV)
    "2b0cbe83-48a2-...": 0.32,
    // ...
  },
  "payout_curve": {
    // advance_probability → expected_dollar_payout
    "0.001": 12.50, "0.005": 24.10, "0.01": 41.20, ...
  }
}
```

The extension consumes `marginal_contributions` + `leverage_scores` + `payout_curve` to compute live pick EV.

`[DECISION 4]` Granularity of `marginal_contributions` table?

Coarse (every 3 picks: 1, 4, 7, ..., 217) keeps the table small (~300 players × 50 constructions × 72 picks = ~1M entries, ~30MB JSON). Fine (every pick: 1..216) is 3× larger.

*Recommendation:* **Every 3 picks**, with linear interpolation in the extension when the user's actual pick falls between checkpoints. The marginal contribution surface is smooth — interpolation is fine.

---

## Tournament schema (`v1/tournaments/bbm_2026.json`)

The Layer B tournament YAML config, serialized to JSON for the extension to consume. The extension uses it for:
- Decomposing BRO score (what payout structure does the advance prob convert against?)
- Displaying tournament-specific UI elements (round-by-round advancement targets)

Structure mirrors the YAML in `LAYER_B.md`.

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

## Tournament resolution (`v1/tournaments_index.json`)

The extension's existing `_resolveContestTitle()` returns a contest title from one of four sources (`draft.title`, `tournament_rounds`, `weekly_winners`, `tournaments` cache). Matching that title to one of our tournament configs is a lookup in `tournaments_index.json`.

### Schema

```jsonc
{
  "_meta": { "version": "1.0.0", "generated_at": "2026-08-15T03:00:00Z" },
  "tournaments": {
    "bbm_2026": {
      "name": "Best Ball Mania VII",
      "season": 2026,
      "underdog_contest_titles": [
        "Best Ball Mania",
        "Best Ball Mania VII",
        "BBM VII",
        "BBM7"
      ],
      "config_url": "v1/tournaments/bbm_2026.json",
      "building_blocks_url": "v1/building_blocks/bbm_2026.json"
    },
    "bbm_superflex_2026": {
      "name": "Best Ball Mania Superflex",
      "season": 2026,
      "underdog_contest_titles": [
        "Best Ball Mania Superflex",
        "BBM Superflex"
      ],
      "config_url": "v1/tournaments/bbm_superflex_2026.json",
      "building_blocks_url": "v1/building_blocks/bbm_superflex_2026.json"
    },
    "big_board_2026": {
      "name": "The Big Board",
      "season": 2026,
      "underdog_contest_titles": ["The Big Board", "Big Board"],
      "config_url": "v1/tournaments/big_board_2026.json",
      "building_blocks_url": "v1/building_blocks/big_board_2026.json"
    },
    "puppy_2026":          { "name": "The Puppy",         "season": 2026, "underdog_contest_titles": ["The Puppy", "Puppy"],                       "config_url": "v1/tournaments/puppy_2026.json",          "building_blocks_url": "v1/building_blocks/puppy_2026.json" },
    "weekly_winners_2026": { "name": "Weekly Winners",    "season": 2026, "underdog_contest_titles": ["Weekly Winners", "Weekly Winner"],          "config_url": "v1/tournaments/weekly_winners_2026.json", "building_blocks_url": "v1/building_blocks/weekly_winners_2026.json" }
  }
}
```

### Resolution algorithm (`dataJoin.js`)

1. Get contest title from `_resolveContestTitle()` for the current draft.
2. For each entry in `tournaments_index.tournaments`, check if any of its `underdog_contest_titles` matches (case-insensitive exact match; substring fallback for noisy titles like "Best Ball Mania VII — Week 1 Slate").
3. On match: load the tournament's `building_blocks_url` from IDB cache (or fetch + cache if stale per `_meta.json`). Use its `marginal_contributions` + `leverage_scores` + `payout_curve` to compute live pick EV.
4. On no match: log a warning via `console.warn`, fall back to "no tournament-specific EV — show projection-only ranking (VOR + position rank), no BRO score column."

### Adding new tournaments mid-season

When Underdog launches a new format (or names a satellite contest oddly), the workflow is:

1. R sim project generates new `v1/tournaments/{id}.json` + `v1/building_blocks/{id}.json`
2. New entry in `tournaments_index.json` with the Underdog contest title alias(es)
3. New entry in `_meta.json` under `tournaments`
4. All three published to `bestball-bro-data`
5. Extension picks up the new tournament on next `_meta.json` poll (within the hour for active users)

No extension version bump required. New title aliases for existing tournaments are even cheaper — just `tournaments_index.json` edits.

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

| # | Decision | My pick |
|---|----------|---------|
| 1 | Feed file structure | Multiple files |
| 2 | Correlation representation | Sparse list per player |
| 3 | Publish sim_draws | Yes, for advanced consumers |
| 4 | Marginal contribution granularity | Every 3 picks + linear interpolation |
| 5 | Extension update detection | Hybrid: _meta on each load, files on sha change |
| 6 | Migration from current feed | Versioned URLs |

---

## What this unlocks

Once these three docs (Layer A, Layer B, Feed Spec) are signed off, the code work splits cleanly:

- **R sim project (`bestball-bro-sim`):** implements Layer A + Layer B offline. Publishes to `bestball-bro-data`. Self-contained.
- **`bestball-bro-data` repo:** receives daily/weekly/triggered publishes from CI. GitHub Pages serves the files. No logic.
- **BestBall Bro extension:** `projectionFetch.js` grows to handle the new feed; `matchPlayer.js` adds the UUID-primary join; `dataJoin.js` implements live Layer B (the recommendation engine). Combo popup grows new columns. Nothing rebuilt — only extended.

That's the whole system.
