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
├── _meta.json                          # Manifest — discovered first, drives everything else
├── tournaments_index.json              # Underdog contest title → tournament_id mapping
├── v1/
│   └── projections/
│       └── nfl_2026.json               # Layer A — per-player projections (shared across tournaments)
├── sim_draws/
│   └── nfl_2026.parquet                # Layer A full sim draws (optional consumer)
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

`_meta.json` at root is the manifest:

```jsonc
{
  "season": 2026,
  "generated_at": "2026-08-15T03:00:00Z",
  "files": {
    "projections":       { "path": "v1/projections/nfl_2026.json",    "version": "0.3.0", "sha256": "..." },
    "sim_draws":         { "path": "sim_draws/nfl_2026.parquet",      "version": "0.3.0", "sha256": "..." },
    "tournaments_index": { "path": "tournaments_index.json",          "version": "1.0.0", "sha256": "..." },
    "teams":             { "path": "teams/nfl_2026.json",             "version": "1.0.0", "sha256": "..." }
  },
  "tournaments": {
    "bbm_2026":            { "config": "tournaments/bbm_2026.json",            "building_blocks": "building_blocks/bbm_2026.json",            "version": "0.3.0", "sha256": "..." },
    "bbm_superflex_2026":  { "config": "tournaments/bbm_superflex_2026.json",  "building_blocks": "building_blocks/bbm_superflex_2026.json",  "version": "0.3.0", "sha256": "..." },
    "big_board_2026":      { "config": "tournaments/big_board_2026.json",      "building_blocks": "building_blocks/big_board_2026.json",      "version": "0.3.0", "sha256": "..." },
    "puppy_2026":          { "config": "tournaments/puppy_2026.json",          "building_blocks": "building_blocks/puppy_2026.json",          "version": "0.3.0", "sha256": "..." },
    "weekly_winners_2026": { "config": "tournaments/weekly_winners_2026.json", "building_blocks": "building_blocks/weekly_winners_2026.json", "version": "0.3.0", "sha256": "..." }
  },
  "scoring_systems": {
    "half_ppr_underdog":   { "path": "scoring/half_ppr_underdog.json", "version": "1.0.0", "sha256": "..." }
  }
}
```

The `tournaments` map is open-ended — adding a new tournament means adding an entry here plus publishing the two referenced files (`config` and `building_blocks`).

---

## Projections schema (`v1/projections/nfl_2026.json`)

```jsonc
{
  "_meta": {
    "season": 2026,
    "scoring": "half_ppr_underdog",
    "generated_at": "2026-08-15T03:00:00Z",
    "n_sims": 10000,
    "model_version": "0.3.0",
    "data_through_date": "2026-08-14",
    "user_adjustments_version": "2026-08-14a"
  },
  "players": [
    {
      "name": "Bijan Robinson",
      "team": "ATL",
      "position": "RB",
      "gsis_id": "00-0037077",
      "underdog_player_id": "2b0cbe83-48a2-41b0-b03f-42180cfc4b77",
      "season": {
        "mean": 287.4, "median": 281.2, "std": 58.3,
        "percentiles": { "p10": 215.0, "p25": 248.5, "p50": 281.2, "p75": 322.1, "p90": 363.8, "p95": 388.5 },
        "games_played": { "mean": 15.8, "p10": 13, "p50": 16, "p90": 17 }
      },
      "weekly": [
        { "week": 1, "mean": 17.5, "std": 8.2, "p90": 32.0, "p10": 4.5 }
        // ... weeks 2-17
      ],
      "stats": {
        "rush_att":   { "mean": 245,  "std": 35  },
        "rush_yards": { "mean": 1180, "std": 250 },
        "rush_tds":   { "mean": 11,   "std": 4   },
        "targets":    { "mean": 85,   "std": 18  },
        "rec":        { "mean": 65,   "std": 14  },
        "rec_yards":  { "mean": 520,  "std": 130 },
        "rec_tds":    { "mean": 3,    "std": 1.8 }
      },
      "correlations": [
        { "with_underdog_id": "...uuid...", "rho":  0.42, "type": "same_team_qb_rb" },
        { "with_underdog_id": "...uuid...", "rho": -0.15, "type": "same_team_rb_rb" }
      ],
      "vor": 84.2,
      "position_rank": "RB1"
    }
    // ... ~300 players
  ]
}
```

### Required vs optional fields

**Required (extension parses these every load):**
- `name`, `team`, `position`, `underdog_player_id`
- `season.mean`, `season.std`
- `position_rank`, `vor`

**Optional (used by richer recommendation logic; absent = feature degrades gracefully):**
- `gsis_id` — falls back to name-based matching
- `season.percentiles` — falls back to assuming normal distribution
- `weekly` — falls back to season/17 with position-specific weekly variance
- `stats` — purely informational, not used by current B-side
- `correlations` — falls back to zero correlations (loses stack EV)
- `season.games_played` — falls back to 17 expected

This gradual-degradation pattern lets the feed evolve. Adding new fields never breaks the extension.

### Correlation representation

`[DECISION 2]` How do we represent correlations?

- **Sparse list per player (above):** each player carries a list of pairs they correlate with. Symmetric data duplicated. Easy to consume.
- **Sparse matrix in `_meta`:** single global sparse correlation matrix, players reference by index. No duplication.
- **Type-based defaults + overrides:** for each `correlation_type` ("same_team_qb_wr"), a global default rho; per-player overrides only when meaningful.

*Recommendation:* **Sparse list per player.** Duplicated data is fine at this scale (~300 players × ~5 meaningful correlations × small payload). Direct consumption — no joins, no indexes. The extension's existing `dataJoin.js` pattern handles it naturally.

---

## Sim draws schema (`sim_draws/nfl_2026.parquet`)

Parquet for efficiency (~80KB per player × 300 players × 10k draws would be ~240MB as JSON; ~20MB as Parquet).

Schema:

| Column | Type | Notes |
|--------|------|-------|
| `underdog_player_id` | string | join key |
| `sim_id` | int | 0 .. n_sims-1 |
| `season_points` | float | half-PPR season total |
| `games_played` | int | |
| `weekly_points` | list[float] | length 17 |

Joint correlations are preserved by `sim_id` — sim_id=0 across all players represents one self-consistent simulated season.

Consumers: Layer B offline (R), future DFS optimizer (whatever language). Extension does **not** fetch this file — it's for advanced tooling.

`[DECISION 3]` Publish sim_draws at all for v1, or defer?

*Recommendation:* **Publish.** It's the only way Layer B's offline pipeline can do anything sophisticated with correlations, and it costs almost nothing to write once the sim already ran.

---

## Building blocks schema (`building_blocks/bbm_2026.json`)

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
    // Keyed by (underdog_player_id, construction_id, pick_number)
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

## Tournament schema (`tournaments/bbm_2026.json`)

The Layer B tournament YAML config, serialized to JSON for the extension to consume. The extension uses it for:
- Decomposing BRO score (what payout structure does the advance prob convert against?)
- Displaying tournament-specific UI elements (round-by-round advancement targets)

Structure mirrors the YAML in `LAYER_B.md`.

---

## Scoring schema (`scoring/half_ppr_underdog.json`)

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

The extension's existing `BBBRO_MATCH` module joins feed records to Underdog appearance records. Today the join is `name|TEAM|pos` with `OVERRIDES` for known mismatches. With the new feed, we gain `underdog_player_id` as a direct join key.

**New join order in `matchPlayer.js`:**

1. **Primary:** `appearance.player_id === player.underdog_player_id` — direct UUID match, perfect when both sides have it.
2. **Secondary:** `name|TEAM|pos` — current Phase 8 logic, used when `underdog_player_id` is missing from the feed (early-season rookies, mid-season trades the feed hasn't picked up yet).
3. **Tertiary:** name-only + position uniqueness — current fallback.
4. **Quaternary:** lastname + position + team disambiguation — current fallback.
5. **OVERRIDES table** — true name-spelling mismatches (Hollywood/Marquise, etc.).

`underdog_player_id` becoming the primary key eliminates ~80% of the OVERRIDES entries over time. We keep the existing fallback chain because Underdog occasionally assigns new UUIDs (re-signed players, rookies pre-rookie-camp, etc.) and the feed might lag.

---

## Tournament resolution (`tournaments_index.json`)

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
      "config_url": "tournaments/bbm_2026.json",
      "building_blocks_url": "building_blocks/bbm_2026.json"
    },
    "bbm_superflex_2026": {
      "name": "Best Ball Mania Superflex",
      "season": 2026,
      "underdog_contest_titles": [
        "Best Ball Mania Superflex",
        "BBM Superflex"
      ],
      "config_url": "tournaments/bbm_superflex_2026.json",
      "building_blocks_url": "building_blocks/bbm_superflex_2026.json"
    },
    "big_board_2026": {
      "name": "The Big Board",
      "season": 2026,
      "underdog_contest_titles": ["The Big Board", "Big Board"],
      "config_url": "tournaments/big_board_2026.json",
      "building_blocks_url": "building_blocks/big_board_2026.json"
    },
    "puppy_2026":          { "name": "The Puppy",         "season": 2026, "underdog_contest_titles": ["The Puppy", "Puppy"],                       "config_url": "tournaments/puppy_2026.json",          "building_blocks_url": "building_blocks/puppy_2026.json" },
    "weekly_winners_2026": { "name": "Weekly Winners",    "season": 2026, "underdog_contest_titles": ["Weekly Winners", "Weekly Winner"],          "config_url": "tournaments/weekly_winners_2026.json", "building_blocks_url": "building_blocks/weekly_winners_2026.json" }
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

1. R sim project generates new `tournaments/{id}.json` + `building_blocks/{id}.json`
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
