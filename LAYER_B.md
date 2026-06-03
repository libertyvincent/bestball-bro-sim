# Layer B — Tournament Simulation & Live Recommendation

> **Status note (June 2026):** Layer B is implemented, but not exactly as this
> document designed it. What carried forward: the engine + config split (the
> config-driven tournament EV engine is `R/tournament_ev.R`, configs are YAML
> under `inst/data/tournaments/` via `R/tournament_loader.R`), the field model
> (`R/field_empirics.R` + `R/field_model.R`), correlated draws
> (`R/correlation.R`), and the best-ball lineup kernel (`R/lineup_optimizer.R`).
> What was **superseded**: the building-block precompute designed below
> (replacement levels / scarcity curves / leverage scores / the BRO score and
> its multipliers). Its replacement is the **EV building-blocks contract** —
> per-slate path-aligned joint-draws tensor + per-tournament advancement/payout
> curves + a curve-based marginal-EV combine — frozen in
> [docs/ev_building_blocks_contract.md](docs/ev_building_blocks_contract.md) and
> implemented in `R/ev_blocks.R` (artifacts registered in `_meta.json`; see
> [FEED_SPEC.md](FEED_SPEC.md)). This document is kept as the design record for
> the decisions that led there.

## Purpose

Layer B bridges projections (Layer A) and the live draft experience (Layer C). It answers: **"Given my current roster, the picks already made, and the players still available, what is the expected payout of picking each available player right now?"**

Layer B is **engine + config**. The engine is tournament-agnostic — it takes a tournament config as input and sims whatever rules it's fed. Tournament rules (roster slots, advancement structure, payouts, field size, scoring) live in YAML configs that the engine consumes, not in code. Adding a new tournament format is a config change, not an engine change.

Layer B splits across two execution contexts:

- **R-side (offline)** pre-computes building blocks that don't depend on the user's specific draft — field-behavior priors, replacement levels, position scarcity curves, marginal advance contributions under reference roster constructions, payout-weighted leverage scores. **Precomputed per tournament config** and shipped in the JSON feed alongside Layer A outputs.

- **Extension-side (live)** combines those building blocks with the user's actual roster and the live draft state to produce per-pick EV during the pick clock. Lightweight math (lookups + weighted combinations), runs comfortably in JavaScript.

This mirrors how production systems work — Pat's Sidekick, Solver, etc. The expensive sim ran offline; the live tool does fast inference against pre-computed quantities. Anyone trying to run full Monte Carlo during a fast draft is going to lose to the clock.

---

## Scope

### In scope (v1)
- **Stage-based advancement engine** — handles any tournament expressible as scoring + roster slots + a list of stages (scoring window, eligibility, pod definition, advancement rule, payout pool). See *Tournament config schema* below.
- Tournament configs for BBM, Big Board, Puppy, Weekly Winners, and superflex variants of each as launch coverage
- Live per-pick EV during the user's draft

### Deferred
- DFS lineup EV — parallel project; reuses Layer A but separate Layer B logic (different optimization target — salary cap + ownership leverage, not advancement EV)
- Any tournament format the stage abstraction can't express (none known today; flag if discovered)

### Out of scope
- Auto-drafting (Layer B *informs* picks, never makes them)
- Cross-portfolio optimization (managing exposure across drafts is Layer C / extension work, fed by Layer B per-pick scores)

The v1 question of "which tournaments do we support" is dissolved by the stage abstraction. Anything Underdog runs that fits the schema is supported by writing a config — no engine work needed.

---

## Tournament config schema

Each tournament is a YAML config the engine ingests. The schema centers on **stages** — every tournament is a list of stages, where each stage has its own scoring window, eligibility rule, pod definition, advancement rule, and payout pool. This single abstraction covers BBM-style multi-stage advancement, Weekly Winners-style independent weekly contests, and anything between.

### Example: Best Ball Mania

```yaml
# Historical example (pre-Sprint 3a stage-abstraction schema). The
# canonical configs now live at inst/data/tournaments/<id>.yaml with the
# richer Sprint 3a schema (tournament_id, underdog_tournament_id,
# inherits_common_rules, tiered payouts, etc.) -- see bbm7.yaml.
id: bbm_2026
name: "Best Ball Mania VII"
entry_fee: 25
field_size: 750000
scoring: half_ppr_underdog          # references scoring/{id}.json
roster_slots:
  QB:    { min_drafted: 1, max_drafted: 3, scoring_starts: 1 }
  RB:    { min_drafted: 2, max_drafted: 6, scoring_starts: 2 }
  WR:    { min_drafted: 3, max_drafted: 7, scoring_starts: 3 }
  TE:    { min_drafted: 1, max_drafted: 3, scoring_starts: 1 }
  FLEX:  { eligible: [RB, WR, TE], scoring_starts: 1 }
  # Superflex variant adds: SFLEX: { eligible: [QB, RB, WR, TE], scoring_starts: 1 }
draft:
  teams_per_pod: 12
  rounds: 18
  format: snake
stages:
  - id: regular_season
    weeks: { from: 1, to: 14 }
    eligible_from: all
    pod: { type: draft_pod, size: 12 }
    advancement: { type: top_n, count: 2 }
    payout_pool: 0
  - id: quarterfinals
    weeks: [15]
    eligible_from: regular_season         # advancers from prior stage
    pod: { type: round_robin_field, size: 470 }
    advancement: { type: top_pct, pct: 0.10 }
    payout_pool: 0
  - id: semifinals
    weeks: [16]
    eligible_from: quarterfinals
    pod: { type: round_robin_field, size: 470 }
    advancement: { type: top_pct, pct: 0.20 }
    payout_pool: 0
  - id: finals
    weeks: [17]
    eligible_from: semifinals
    pod: { type: round_robin_field, size: 470 }
    advancement: none
    payout_pool:
      total: 15_000_000
      distribution_url: payouts/bbm_2026_finals.csv
```

### Example: Weekly Winners

Same engine, different stages — no advancement, weekly payouts:

```yaml
# Historical example -- canonical config now at
# inst/data/tournaments/weekly_winners_2026.yaml under the Sprint 3a schema.
id: weekly_winners_2026
name: "Weekly Winners NFL 2026"
scoring: half_ppr_underdog
roster_slots: { ... }                   # same shape as BBM
draft: { teams_per_pod: 12, rounds: 18, format: snake }
stages:
  - id: week_1
    weeks: [1]
    eligible_from: all
    pod: { type: draft_pod, size: 12 }
    advancement: none
    payout_pool: { total: 50_000, distribution_url: payouts/ww_week_1.csv }
  - id: week_2
    weeks: [2]
    eligible_from: all                  # every team plays every week
    pod: { type: draft_pod, size: 12 }
    advancement: none
    payout_pool: { total: 50_000, distribution_url: payouts/ww_week_2.csv }
  # ... 17 stages total
```

### Schema reference

**Stage components:**

- `weeks` — NFL week numbers contributing to this stage's scoring. Range syntax `{ from: 1, to: 14 }` or explicit list `[15]`.
- `eligible_from` — `all` (every team in tournament) or `<stage_id>` (advancers from prior stage).
- `pod` — how teams are grouped for ranking:
  - `{ type: draft_pod, size: N }` — your original draft cohort
  - `{ type: round_robin_field, size: N }` — entire surviving field ranked together
  - `{ type: random_pod, size: N }` — randomly grouped surviving teams (rare)
- `advancement`:
  - `{ type: top_n, count: K }` — top K per pod advance
  - `{ type: top_pct, pct: P }` — top P fraction per pod advance
  - `none` — no advancement (terminal stage or non-advancement tournament)
- `payout_pool`:
  - `0` — no stage payout (typical for non-terminal advancement stages)
  - `{ total: N, distribution_url: ... }` — total pot and distribution rule, referenced by URL to keep configs compact

**Roster slots:**

- `min_drafted` / `max_drafted` — bounds on draft picks at this position
- `scoring_starts` — how many at this position auto-fill weekly scoring (best ball)
- `eligible: [...]` for FLEX-like slots — list of positions that can fill this slot

Adding a new tournament — Big Board, Puppy, Best Bowl Mania, anything Underdog launches — is a YAML, not a code change.

---

## Field modeling

To compute pick EV we need to simulate the rest of the draft — what opponents pick. Layer B's field model is how we do that.

`[DECISION 1]` How do we model opponent drafters?

**Option 1: Pure ADP + noise.** Each opponent picks player nearest to current ADP with Gaussian noise (std proportional to round). *Pros:* Trivial. *Cons:* Ignores roster construction — opponents would draft 7 QBs in Round 1.

**Option 2: ADP + roster construction priors.** Same as Option 1 but with a per-round per-position prior (e.g., Round 1 picks are ~95% RB/WR; Round 17 is ~40% QB, 25% TE, 35% RB/WR depth). *Pros:* Realistic distributions, simple to fit from BBM historical data. *Cons:* Treats all opponents identically.

**Option 3: Heterogeneous field — sharps + casuals.** Model X% "sharp" drafters (advance-rate-optimized, use Layer B against themselves) and (1–X)% "casual" drafters (pure ADP + light roster priors). *Pros:* More accurate; BBM fields are heterogeneous. *Cons:* Requires choosing X (the sharp fraction), adds complexity.

**Option 4: BBM historical sampling.** For each pick slot, sample from the actual distribution of picks made at that slot in prior BBMs from the Best Ball Data Bowl + Underdog's published pick-by-pick CSVs (BBM4/5/6). *Pros:* Empirical. *Cons:* Player pool changes year-to-year — sampling 2024 picks at slot 47 doesn't map cleanly to 2026 players.

*Recommendation:* **Option 2 for v1**, calibrated from BBM3–6 data. **Option 3 once we have a working pipeline** — heterogeneity matters more for stake/leverage calculations than for advance probability.

The Best Ball Data Bowl repo plus BBM4–6 downloads from underdognetwork.com give us 4 years of pick-by-pick training data for whichever option we choose.

---

## Offline pre-computation (R-side)

For each tournament config × Layer A projection feed combination, Layer B's offline pipeline produces:

### 1. Field draft simulations

Run M (M ≈ 10,000) full mock drafts using the field model. For each, run K (K ≈ 1,000) season simulations using Layer A's distributions and correlations. The result: a large empirical distribution of `(roster_construction, season_outcome, advance_stage_reached)` tuples.

This is the expensive step. It's what makes Layer B "offline."

### 2. Replacement-level thresholds

For each position × week, the fantasy-point threshold below which a player doesn't enter scoring (because a better-scoring roster slot fills first). This is *dynamic per roster construction* — a roster with 3 starting WRs has a higher WR replacement level than one with 5.

Output: replacement level lookup table by position × roster construction archetype × week.

### 3. Position scarcity curves

For each position × pick slot in the draft, the marginal value of taking the next player at that position vs. waiting one round. Captures "the WR run is starting" dynamics.

Output: 2D table of scarcity curves indexed by (position, pick_number).

### 4. Reference roster construction EVs

`[DECISION 2]` Which "reference" roster constructions do we pre-compute against?

**Option A: Modal constructions.** Top 5–10 most common BBM3–6 winning roster builds (Zero RB, Hero RB, Anchor RB, balanced, etc.). *Pros:* Realistic. *Cons:* Constructions evolve year-over-year; modal may not be optimal.

**Option B: Advance-rate-weighted constructions.** Sample roster constructions weighted by their historical advance rate, not their frequency. *Pros:* Aligned with what we actually care about. *Cons:* Smaller sample per construction.

**Option C: Per-slot priors.** For each draft slot (1–12), pre-compute EV under the modal construction *for that slot* (slot 1 plays differently than slot 12). *Pros:* Slot-aware. *Cons:* 12× the precompute cost.

*Recommendation:* **Hybrid B + C.** Pre-compute advance-rate-weighted constructions per slot. The compute cost is real but bounded — 12 slots × ~10 constructions × M × K sims = manageable overnight on commodity hardware.

### 5. Marginal advance contributions

For each player × roster construction archetype × pick number, the marginal advance probability of adding that player to the roster, vs. taking the next-best alternative. This is the *atomic unit* that live B-side uses.

Output: 3D table of marginal contributions, queried at live time by (player, current_roster_archetype, pick_number).

### 6. Leverage / uniqueness scores

`[DECISION 3]` Do we model uniqueness/leverage for advancement EV?

BBM advancement is rank-based, not absolute-score based. Players uniquely owned by you (low field exposure) give *leverage* — when they hit, you advance disproportionately because the rest of the field doesn't have them.

**Option A: Skip for v1.** Treat all players equally on EV.
**Option B: Compute leverage from BBM historical exposure data.** Players with low historical exposure but high ceilings get bonus EV.
**Option C: Compute leverage from Underdog live exposure data.** If Underdog publishes (or we scrape) current draft exposure %, that's the live signal.

*Recommendation:* **Option B for v1.** Cheap to compute, real edge, matches what shops actually do. Option C is the long-term aspiration.

### 7. Payout-weighted EV conversion

For each (advance probability, advance stage), the expected dollar payout given the tournament's payout structure. This is just arithmetic once advance probabilities are computed.

---

## Live recommendation (extension-side)

During the user's draft, when it's their pick:

```
For each available player on the board:
  1. Look up player's marginal advance contribution from feed building blocks,
     keyed by (player_id, current_roster_archetype, pick_number)
  2. Adjust for the user's specific roster context:
     a. Apply Layer A correlation adjustments for already-drafted teammates
        (positive stack bonus, negative same-position-same-team penalty)
     b. Apply position-fill penalty if a position is already maxed
     c. Apply user's custom rankings delta (if provided)
  3. Convert adjusted advance probability → expected dollar payout via payout table
  4. Apply leverage adjustment from feed building blocks
  5. Output: pick_EV in dollars, plus decomposition (base_EV, stack_bonus, position_penalty, leverage_adj)
Sort descending by pick_EV.
```

The user's current roster gets classified into the nearest pre-computed roster archetype each pick (it can change as more picks are made). This is what makes the live computation cheap — most of the heavy lifting is just lookups.

`[DECISION 4]` Live recommendation latency budget?

Fast best ball drafts give 20 seconds per pick. Live computation needs to complete in <500ms to feel instant. *Recommendation:* hard budget 250ms for the EV computation; if we ever exceed it, push more pre-computation to offline.

---

## The BRO score (Layer B's output to Layer C)

Today's BRO score is a heuristic blend of VOR + positional/exposure signals. Post-Layer-B, the BRO score becomes:

```
BRO = pick_EV_dollars
```

That's it. Pick EV in expected dollars is the single best summary statistic — it already incorporates VOR (via projection), advance probability, stack synergies, position scarcity, leverage, and tournament payout structure. The user sorts by BRO descending and the top row is, by our model, the highest-EV pick.

Decomposition columns (ceiling, advance_prob, stack_bonus, leverage) are available for users who want to understand *why* BRO ranks a player high — but BRO itself is the summary.

`[DECISION 5]` Should BRO score be normalized (e.g., 0–100 scaled) or kept as raw expected dollars?

*Recommendation:* **Raw dollars.** Loses a little visual cleanliness but is interpretable — "this pick is worth $4.20 in expected payout vs. $3.80 for the alternative" is more useful than "BRO score 87 vs 84."

---

## Update cadence

Layer B offline pre-computation is more expensive than Layer A's projections. Full M=10,000 mock drafts × K=1,000 season sims per draft is ~hours, not minutes, even with parallelization.

`[DECISION 6]` Layer B update cadence?

- **Match Layer A** (daily during draft season): expensive but always fresh.
- **Slower than Layer A** (weekly): cheaper, slightly stale building blocks.
- **Triggered on Layer A change exceeding threshold**: rebuild Layer B only when projections move enough to matter.

*Recommendation:* **Weekly during draft season**, **daily during the final 2 weeks before the season starts**, **after each weekly projection update during the regular season**. Layer B building blocks are robust to small Layer A perturbations; we don't need to rebuild on every minor news update.

---

## Validation

Layer B validation is meaningfully harder than Layer A because the ground truth is "did rosters built using Layer B advance and cash?"

1. **Backtest on BBM3–6.** Run Layer A as-of pre-season for each year, run Layer B against the actual BBM3–6 field, simulate "drafting with BRO recommendations" against the actual field. Did simulated entries advance / cash above field average?
2. **Self-consistency.** Layer B's predicted advance probabilities for the user's roster, summed across all simulated paths, should match the empirical advance rate of similar rosters in BBM3–6.
3. **Sanity anchors.** A known elite construction (e.g., advance-rate top 5% from BBM6) should score in the top decile of pick-EV decomposition.
4. **Live calibration.** During the actual season, log every recommended pick and its actual outcome. Build a calibration plot — predicted EV vs. realized payout share.

---

## Proposed R project layout (Layer B additions)

Extends the Layer A project layout:

```
bestball-bro-sim/
├── R/
│   ├── ...                          # Layer A files
│   ├── field_model.R                # opponent drafter simulation
│   ├── mock_drafts.R                # full mock draft sim engine
│   ├── replacement_levels.R         # threshold computation
│   ├── scarcity_curves.R            # position scarcity by pick
│   ├── reference_rosters.R          # construction archetype precompute
│   ├── marginal_contributions.R     # the 3D MC lookup table
│   ├── leverage.R                   # uniqueness/exposure adjustments
│   ├── payout_ev.R                  # advance prob → $ conversion
│   └── publish.R                    # extended to write building_blocks.json
├── inst/
│   ├── data/
│   │   └── tournaments/                # canonical Sprint 3a configs
│   │       ├── bbm7.yaml
│   │       └── best_bowl_mania_2026.yaml # deferred
│   └── ...
```

---

## Open decisions summary

| # | Decision | My pick |
|---|----------|---------|
| 1 | Field model | ADP + roster construction priors (Option 2) for v1, heterogeneous later. **Calibrated per tournament** — Puppy fields are sharper than BBM, Weekly Winners drafts differently than advancement formats. |
| 2 | Reference roster constructions | Advance-rate-weighted × per-slot (Hybrid B+C). **Per-tournament necessarily** — optimal construction varies by format (Zero RB in BBM ≠ in superflex ≠ N/A in Weekly Winners). |
| 3 | Leverage / uniqueness | Yes, from BBM historical exposure (Option B) |
| 4 | Live latency budget | 250ms hard, 500ms soft ceiling |
| 5 | BRO normalization | Raw expected dollars |
| 6 | Update cadence | Weekly draft season / daily final 2 weeks / weekly regular season |
| 7 | Reference construction precompute vs runtime | **Precompute.** Keeps the engine fast at runtime; per-tournament necessarily (see #2) |

The v1 question of "which tournaments do we support" is dissolved by the stage abstraction — any Underdog format expressible as scoring + roster + stages is supported by writing a config. Launch coverage: BBM, Big Board, Puppy, Weekly Winners, plus superflex variants. New formats post-launch are config additions, no engine change.

Sign off or push back. Once Layer B is settled, the Feed Spec (next doc) is mostly mechanical — it serializes Layer A + Layer B outputs into the wire format the extension consumes.
