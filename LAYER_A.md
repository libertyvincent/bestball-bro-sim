# Layer A — Projections & Player Simulations

## Purpose

Layer A is the foundation of the BestBall Bro analytical stack. It produces per-player season and weekly fantasy point distributions, plus the correlation structure between players, that downstream layers (offline tournament EV pre-computation, live draft recommendation engine, future DFS lineup optimizer) consume.

This document defines the methodology, inputs, outputs, and validation approach for Layer A. Open methodology decisions are flagged inline with `[DECISION]` and require resolution before any code is written.

**Critical design constraint:** Outputs must be *distributional*, not point estimates. Best ball tournament EV is fundamentally about ceilings, advance probabilities, and joint outcomes — not mean projected points. A 12 PPG WR who hits 22 PPG in 4 games beats a steady 15 PPG WR in a best ball context. This same property makes the outputs reusable for DFS without rework.

---

## v1 scope (current iteration)

v1 is the next iteration after the v0 baseline (prior-season passthrough). It addresses the two biggest v0 gaps — rookies and missed games — while deferring deeper component modeling.

**In scope for v1:**

- **Rookie projections via college / draft-capital comparables** — replaces the v0 behavior where rookies project to ~zero because they have no prior-season stats.
- **Games-played normalization** — project per-game and multiply by expected games played. Fixes the v0 bug where players who missed time the prior season (e.g., Lamar Jackson at 13 games) are systematically undervalued because their season totals were depressed.
- **Veterans keep the v0 weekly → season methodology** — sum of weekly fantasy points from the prior season, with season variance derived from `weekly_std × sqrt(games)`. Good enough as a v1 baseline; component-level modeling is deferred.

**Explicitly deferred to v1.5+:**

- Component stat models (per-attempt rates, target shares, etc.)
- Aging curves
- Team-context blending for players who changed teams
- Workload / depth-chart awareness
- Refined variance modeling beyond `weekly_std × sqrt(games)`
- Player correlations (these belong to Layer B's sim engine)

**Follow-up:** once v1 ships, update the `_meta.methodology` field in the projections feed from `"v0_prior_season_passthrough"` to `"v1_rookies_and_games_normalized"` (set in `R/publish.R`, function `.projections_to_feed()`).

---

## Scope

### In scope
- NFL only (other sports deferred)
- Regular season (Weeks 1–17)
- Positions: QB, RB, WR, TE
- Underdog half-PPR scoring as primary; engine extensible to other formats
- Player universe: all players appearing on Underdog NFL draft boards (~300–350 typical)

### Out of scope (v1)
- DST / K
- College football
- DFS-specific extras (ownership projections, slate-specific opponent modeling) — outputs are *designed* for DFS reuse but the DFS-specific layer is its own project

Note: Best Bowl Mania playoff weeks (15–17) are covered — Layer A projects all regular season weeks, and the playoff bracket structure is just a Layer B stage config.

---

## Inputs

### 1. nflverse data (R, `nflreadr` / `nflfastR`)

- `load_player_stats(seasons = 2010:2025)` — weekly fantasy-relevant stats
- `load_pbp(seasons = recent)` — snap share, route participation, target share, aDOT, red-zone usage, air yards
- `load_rosters_weekly()` — depth chart context, injury status flags
- `load_schedules()` — schedules, byes, home/away, opponent strength
- `load_injuries()` — practice participation, game status
- `load_ff_opportunity()` — ffverse-modeled expected fantasy points (feature + benchmark)
- `load_ff_rankings()` — FantasyPros consensus rankings (market prior)

Pulled from `nflverse-data` release artifacts. `stats_player` release tag is the current canonical source — the older `player_stats_{season}.csv` paths are deprecated per Discord.

### 2. Consensus projection prior (R, `ffanalytics`)

Aggregates ESPN, FantasyPros, NumberFire, CBS, Yahoo, etc., applied with half-PPR scoring. Used as **one input** to our model, not the final output. The point is to know what the market thinks so we can identify and justify deviation.

### 3. Underdog ADP (daily snapshot)

Live ADP from `stats.underdogfantasy.com/v1/slates/{slate_id}/scoring_types/{scoring_type_id}/appearances`. The extension already captures this for live use; Layer A needs a daily snapshot stored historically so we can:
- Study how ADP moves through the offseason
- Identify where our projections diverge from market

### 4. User "knowing ball" adjustments

`[DECISION 1]` Where do manual projection adjustments live?

- **A.** YAML in the sim repo (`inst/adjustments/2026.yaml`), player-keyed deltas. Version-controlled, simple. Cons: not editable inside the draft tool.
- **B.** Separate JSON file published alongside the feed, fetched by both sim and extension. Single source. More moving parts.
- **C.** UI in the extension writes to `chrome.storage.local`, exported as JSON for sim ingest on next run. Edits from inside the draft. Most complex; round-trip is awkward.

*Recommendation:* **A** for v1, migrate later if it becomes friction.

---

## Methodology

### Projection architecture

`[DECISION 2]` Three reasonable architectures, choose or hybridize:

**Option 1: Component-based.** Project each underlying stat (rush attempts, YPC, rush TDs, targets, catch rate, YPR, rec TDs, etc.) separately, multiply out to fantasy points. *Pros:* interpretable, each component has a clear historical anchor, handles change-of-team well. *Cons:* most parts, intra-player component correlations are tricky.

**Option 2: Comparables / historical regression.** Fit a model that maps current-situation features (projected snap share, depth chart, scheme, team, age, prior production) to season fantasy points using historical examples. *Pros:* fewer parameters, captures interactions, robust. *Cons:* less interpretable, surgical adjustments harder.

**Option 3: Top-down team allocation.** Project team points / plays / pass rate first. Allocate team stats to players based on role priors (target share, rush share). *Pros:* enforces consistency — player projections sum to team totals; naturally captures QB-WR correlation. *Cons:* requires good team projections (Vegas helps); allocation brittle for ambiguous role situations.

*Recommendation:* **Hybrid.** Top-down team allocation provides the constraint layer, component models provide the per-player skeleton, comparables inform variance estimates by archetype. Roughly what good shops do, including Pat's process per the transcript.

### Variance / distribution modeling

`[DECISION 3]` How do we go from a mean projection to a full distribution?

**Option 1: Empirical bootstrap.** For each player, find N closest historical comparables (by position, age, projected role, team context); use their actual realized distributions as the empirical distribution. (This is what `ffsimulator` does.)

**Option 2: Parametric.** Assume a distribution shape (right-skewed: shifted log-normal or gamma — fantasy points are right-skewed) with mean from the projection, variance from position-level historical variance, adjusted by archetype (high-volume floor vs. TD-dependent ceiling).

**Option 3: Hybrid.** Parametric shape, bootstrap-derived shape parameters by archetype.

*Recommendation:* Start with **Option 2** for v1 — simpler, faster to validate. Move to **3** if variance estimates feel off. Pure bootstrap conflates "this player's outcome variance" with "the historical noise of his role type," which we can do better than.

### Correlation structure

This is the piece most homemade sims get wrong. Minimum correlations to model:

1. **Same-team QB ↔ pass-catchers** — strong positive (.2–.5 depending on share)
2. **Same-team WRs** — mild negative (–.1 to –.2) — compete for targets, but a high-volume passing game lifts everyone
3. **Same-team QB ↔ same-team RB** — mild negative or near zero — pass-script hurts RB rushing, helps RB receiving
4. **Opposing QBs** — mild positive (.1–.2) — shootouts
5. **Player ↔ own DST** — negative
6. **Player ↔ opposing DST** — negative

`[DECISION 4]` How do we estimate these?

**Option 1: Empirical from play-by-play.** Compute historical correlations between same-team and same-game pairings from `load_pbp()`. Data-driven. Noisy for specific pairs, requires aggregation by archetype.

**Option 2: Theoretical / prior-based.** Set correlations by structural rules (same-team WR1-QB = .35, same-team WR1-WR2 = –.15, etc.).

**Option 3: Hybrid.** Theoretical priors with empirical adjustments where data is sufficient.

*Recommendation:* **Option 3.** Hardcode structural priors for v1, refine empirically once we have a backtesting loop.

### Games played / availability model

`[DECISION 5]` Probability each player misses each week.

**Option 1: Position + age priors.** Base rates by archetype. Simple, defensible, doesn't try to predict specific injuries.

**Option 2: Per-player injury history.** Players with recent injuries get higher miss-game probability. More personalized; rewards small samples.

**Option 3: Hybrid.** Base rate by position/age, per-player flags for documented chronic issues.

*Recommendation:* **Option 3.** Start with base rates; allow manual flags via the adjustments file.

### Scoring

Scoring lives in the tournament config, not in Layer A. Layer A is scoring-agnostic: it projects raw stats (rush attempts, targets, YPC, catch rate, etc.) and derives fantasy-point distributions at output time for each scoring system in use.

Layer A publishes:
- Raw stat distributions per player — the foundation, never changes per tournament
- Fantasy-point distributions per scoring system as derived outputs

Today's set covers at least half-PPR (BBM, Big Board, Puppy, Weekly Winners) — verify against the extension's `scoring_type_id` map for canonical definitions. TE premium, full PPR, or any other variant is a trivial addition once the corresponding scoring file is added to the feed.

`[DECISION 6]` Resolved: raw stats with derived per-scoring outputs. Lets us swap scoring without re-projecting.

---

## Outputs

The Layer A output is a per-player object that becomes the foundation of the JSON feed. Exact wire format is the Feed Spec (separate doc), but conceptually:

```jsonc
{
  // Identity (joins to Underdog appearances + nflverse player IDs)
  "name": "Bijan Robinson",
  "team": "ATL",
  "position": "RB",
  "gsis_id": "00-0037077",
  "underdog_player_id": "2b0cbe83-48a2-41b0-b03f-42180cfc4b77",

  // Season-level distributional projection (half-PPR)
  "season": {
    "mean": 287.4,
    "median": 281.2,
    "std": 58.3,
    "percentiles": {
      "p10": 215.0, "p25": 248.5, "p50": 281.2,
      "p75": 322.1, "p90": 363.8, "p95": 388.5
    },
    "games_played": { "mean": 15.8, "p10": 13, "p50": 16, "p90": 17 }
  },

  // Weekly distributional projection
  "weekly": [
    { "week": 1, "mean": 17.5, "std": 8.2, "p90": 32.0, "p10": 4.5 },
    // ... weeks 2-17
  ],

  // Raw stat projections (so consumers can apply different scoring)
  "stats": {
    "rush_att":   { "mean": 245,  "std": 35  },
    "rush_yards": { "mean": 1180, "std": 250 },
    "rush_tds":   { "mean": 11,   "std": 4   },
    "targets":    { "mean": 85,   "std": 18  },
    "rec":        { "mean": 65,   "std": 14  },
    "rec_yards":  { "mean": 520,  "std": 130 },
    "rec_tds":    { "mean": 3,    "std": 1.8 }
  },

  // Sparse correlation list — only meaningful pairs
  "correlations": [
    { "with_underdog_id": "...", "rho":  0.42, "type": "same_team_qb_rb" },
    { "with_underdog_id": "...", "rho": -0.15, "type": "same_team_rb_rb" }
  ],

  "meta": {
    "generated_at": "2026-08-15T03:00:00Z",
    "model_version": "0.3.0",
    "data_through_date": "2026-08-14",
    "user_adjustments_applied": ["games_played: +0.5"]
  }
}
```

Whole-feed metadata (sibling to the players array):

```jsonc
{
  "_meta": {
    "season": 2026,
    "scoring": "half_ppr_underdog",
    "generated_at": "2026-08-15T03:00:00Z",
    "n_sims": 10000,
    "model_version": "0.3.0",
    "data_through_date": "2026-08-14"
  }
}
```

`[DECISION 7]` Distribution representation: percentiles + moments (as above), raw sim draws (an array of N season totals per player), or both?

- **Percentiles + moments:** small payload (few KB per player), easy to consume in the extension, loses joint distribution info.
- **Sim draws:** large payload (~80 KB per player at 10k draws), preserves joint distribution exactly when keyed by player ID — Layer B can do its own correlated sampling.
- **Hybrid:** percentiles in main feed; full sim draws in a separate file for advanced uses.

*Recommendation:* **Hybrid.** Main feed = percentiles + moments (extension consumes this). Companion `sim_draws.parquet` published alongside for Layer B offline pre-computation and future DFS optimizer.

---

## Update cadence

`[DECISION 8]`

- **Daily** — freshest, most CI cost.
- **Weekly** (Tuesday rebuilds during regular season) — standard cadence for projection products.
- **News-triggered** — requires news API.
- **Hybrid** — weekly full rebuild + on-demand for major news.

*Recommendation:* **Daily during draft season** (April–August best ball drafts run any day), **weekly during regular season** (Tuesday after MNF), **on-demand** for major injury/trade news. Matches when the user actually drafts or makes decisions.

---

## Validation

Every feed publish runs:

1. **Team-total reconciliation.** Player projections by team sum approximately to projected team points. Catches allocation bugs.
2. **Position-total reconciliation.** Top-12 QBs sum near historical top-12 QB totals; same for RB1–24, WR1–36, TE1–12.
3. **Anchor players.** Small set of "anchor" players manually sanity-checked each run (top-3 at each position).
4. **ADP divergence flags.** Players whose rank diverges sharply from Underdog ADP get flagged for review. Genuine edge shows up here — but so do bugs.
5. **Variance plausibility.** Per-position std devs look like historical per-position std devs.

### Backtesting

`[DECISION 9]` Backtest the model on 2023/2024/2025 before deploying for 2026?

*Recommendation:* **Yes.** Build the pipeline to support "as-of-date" projection generation (project 2023 with only pre-2023 data), run for each of the last three years, compare to actual outcomes vs. ADP. **This is the single highest-leverage validation step — and it's what Sack keeps banging on about in the transcript.** Without it we don't actually know if we're beating market.

---

## Proposed R project layout

```
bestball-bro-sim/
├── DESCRIPTION
├── R/
│   ├── data_pull.R          # nflreadr loaders, caching
│   ├── ffanalytics_prior.R  # consensus projection ingest
│   ├── team_model.R         # team-level projections (points, pace, pass rate)
│   ├── allocation.R         # team → player allocation
│   ├── component_model.R    # per-component projection
│   ├── variance.R           # distribution shapes by archetype
│   ├── correlations.R       # pairwise correlation construction
│   ├── injury_model.R       # games-played probability
│   ├── simulate.R           # Monte Carlo season simulator
│   ├── validate.R           # team-total / position-total / anchor checks
│   ├── backtest.R           # as-of-date projection generation
│   └── publish.R            # JSON feed + sim_draws.parquet writer
├── inst/
│   ├── adjustments/2026.yaml   # user "knowing ball" overrides
│   └── anchors/2026.yaml       # anchor players for validation
├── tests/
├── .github/workflows/build_feed.yml  # daily/weekly regeneration → bestball-bro-data
└── LAYER_A.md               # this document
```

---

## Open decisions summary

For your call before code:

| # | Decision | My pick |
|---|----------|---------|
| 1 | User adjustments location | YAML in sim repo (A) |
| 2 | Projection architecture | Hybrid (top-down + component + comparables) |
| 3 | Variance modeling | Parametric for v1 (Option 2), revisit |
| 4 | Correlation estimation | Theoretical priors + empirical refinement (3) |
| 5 | Injury model | Base rate + manual chronic flags (3) |
| 6 | Project in raw stats or FP | Raw stats |
| 7 | Distribution representation | Hybrid: percentiles in feed + sim_draws companion |
| 8 | Update cadence | Daily draft season / weekly regular season / on-demand |
| 9 | Backtest | Yes, last 3 seasons |

If you sign off on these (or push back), Layer B doc + Feed Spec come next. If anything here feels wrong, the place to argue is *before* it gets baked into code.
