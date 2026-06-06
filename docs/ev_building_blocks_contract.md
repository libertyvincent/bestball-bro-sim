# EV Building-Blocks Contract (sim ↔ extension)

The interface for turning per-player projections into a live, roster-conditional draft recommendation. Spawns two implementation tracks (a sim sprint that produces the artifacts; an extension build that consumes them). This doc is the agreement between them — freeze it here, derive the prompts from it.

## What we're computing
At each pick, rank available players by **marginal EV**: `EV(roster + X) − EV(roster)` in a specific tournament's field/payout structure. This is roster-conditional, so it can't be precomputed across all rosters — the *combination* is inherently live. The sim's job is to precompute primitives; the extension combines them at pick time.

## Division of labor
- **Sim precomputes** (per slate): a sample of correlated season paths — the joint draws. (Per tournament): the advancement + payout curves that encapsulate the field, pods, survivor-selection, and ladder.
- **Extension computes live**: its own roster's per-round score distribution from the draws (the one genuinely roster-dependent step), then cheap curve lookups + a combine.

## Artifact A — joint draws (per slate)
The roster-dependent input. The sim already generates 10K correlated paths/slate; ship a downsampled, path-aligned slice.

- **Shape:** one `int16` tensor, axis order `[path][player][week]` (path-major so a path's board is contiguous for the eval loop). Scores stored ×10 (0.1-pt resolution; max ~3276 pts ≫ any real week).
- **Players:** the top ~300 realistically-draftable (not all ~457).
- **Weeks:** all 17 (Weeks 1–14 individually — the qualifier score is a sum of *weekly* best-ball lineups, so per-week is required — plus 15/16/17).
- **Paths (N):** set empirically by the **ranking-stability protocol** (below), not guessed. Expect 250–500. Payload at 300×17×N×2B ≈ 2.5 MB (N=250) / 5 MB (N=500), less gzipped.
- **Sidecar (small JSON):** `{ appearance_id → player_index, positions, weeks, n_paths, quant_scale, dtype, axis_order, lineup_spec }`. `lineup_spec` is the slate's starting lineup (`{ slate_id, slots:[{ pos, n, eligible }] }`, single-slot `eligible` a scalar / FLEX an array) — the round-score assembler needs it together with the curves file's `stage_weeks` to turn the tensor into per-round scores, so the feed is self-describing without the extension hard-coding the lineup.
- **Registration:** per-slate `_meta` keys `v2_draws_path` / `v2_draws_sha256`. Fetched slate-aware; cached + sha-gated like the v2 feed (and `_meta` itself stays fetched-fresh per the freshness fix).
- **HARD INVARIANT — path alignment.** Path *i* must be the *same simulated world* for every player; that shared path axis is the entire correlation mechanism. If draws are sampled/sorted per-player independently, stacking silently vanishes. First test of the artifact: a known stack (QB + his WR) shows the expected positive cross-path correlation, and a structural assert that no per-player reshuffle happened during serialization.

## Artifact B — curves (per tournament)
Roster-independent; encapsulate the entire field/bracket/ladder. Built from the **same sim run** as the draws (same player + field models) or calibration won't match. Lookup tables + linear interpolation, a few hundred points each — tiny.

Per tournament, as functions of the relevant round score:
- **Advancement:** `g₁(R1)`, `g₂(R2)`, `g₃(R3)` = P(advance that round | my round score), each vs the correct (survivor-selected) field for that round.
- **Payout-by-exit-bucket:** `payout_QF(R2)`, `payout_SF(R3)`, `h_final(R4)` = expected $ given you exit (or finish) at that round with that score. These can be flat (Puppy 2: QF=$5, SF=$25) or rank-dependent (Dachshund SF: $100/$24 by Week-16 rank) — the sim emits whatever the ladder requires.

- **Registration:** per-tournament `_meta.tournaments.<tid>` keys `curves_path` / `curves_sha256` (sha-gated like the draws) plus the **live-draft bridge** `underdog_tournament_id` (the UUID a live Underdog draft exposes as `draft.source_id`) and `title`. The extension maps `source_id → tid → curves` deterministically — no title-normalization. Sourced from each tournament config's `underdog_tournament_id` / `display_name` (`inst/data/tournaments/<tid>.yaml`).

## Extension eval loop
Per available player X, over the shipped paths:
1. **Assemble round-scores** (the roster-dependent step): for each path, take the roster's weekly best-ball lineup (top QB / 2 RB / 3 WR / TE / FLEX from the rostered players that week) for Weeks 1–14 → sum = R1; Weeks 15/16/17 = R2/R3/R4. Adding X only re-opens the weeks where X cracks the optimal lineup, so the marginal recompute is cheap.
2. **Combine, per path:**
   `$ = g₁·(1−g₂)·payout_QF(R2) + g₁·g₂·(1−g₃)·payout_SF(R3) + g₁·g₂·g₃·h_final(R4)`
   (with the qual-exit bucket contributing $0).
3. **EV** = mean of `$` over paths. **Marginal EV(X)** = `EV(roster+X) − EV(roster)`, evaluated on the **same path set** — common random numbers, so per-path differences mostly cancel and the marginal is far less noisy than either EV alone. Rank available players by marginal EV; that replaces broScore's heuristic multipliers.

## Validation gate (makes the contract trustworthy)
**Curve-based EV must reproduce the full Layer-B sim EV** for a set of test rosters. If it matches, the round-factorization is sound and the curves are sufficient. The expected place it can drift is **leverage/duplication**: the g-curves treat the field as independent of your roster, so they capture your *ceiling* (internal stacking — carried by the joint draws) but not the fact that a chalky roster co-booms with the field and advances *less*. The gate quantifies that error on chalky vs unique test rosters. v1-acceptable if small; if material, condition the curves on a roster-uniqueness statistic. Ceiling is the dominant effect, so v1 ships pending this check.

## Path-count protocol (how N gets set)
Don't converge absolute EV — converge the **ranking**. At N = 250 / 500 / 1000, draw independent path samples and measure churn in the top-~20 available-player ordering (Kendall-τ / top-k overlap) across resamples. Smallest N where recommendations stop reordering wins. Becomes a standing test so a future slate that needs more paths is flagged. Curves use the full 10K sim-side regardless (free); only the shipped draws cost payload.

## Two implementation tracks (derive prompts next)
1. **Sim:** emit Artifact A (path-aligned draws, with the alignment test) + Artifact B (the six curves per tournament), register in `_meta`, and run the validation gate + path-count protocol. Anchor on Puppy 2 (Season, live, configured).
2. **Extension:** fetch draws + curves slate/tournament-aware (reusing the freshness-fixed `_meta` path), build the per-path roster round-score assembler (weekly best-ball), the combine, and the CRN marginal ranking. Replaces broScore's multipliers behind a flag, v0/v2-VOR retained as fallback.
