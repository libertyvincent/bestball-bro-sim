# Phase A Design — Draw-level availability (game-zeroing)

**Status:** DESIGN ONLY. No code until approved. PAUSE in force at end of this doc.
**Supersedes the mean-level mechanism of PR #34** (the upstream Clay-points scaling in
`R/availability.R` / `R/blender.R:52–64`). Replaces it with a per-(path, player, week)
missed-week mask driven by the same position-level priors
(`inst/adjustments/availability.yaml`: QB 16.8 / RB 15.2 / WR 16.5 / TE 16.6 games out of 17).

This doc answers the prompt's 7 items and the hub's 4 binding additions, each with a
recommendation + rationale, and pre-registers every decision rule and tolerance **before**
the measurement it governs.

---

## 0. Grid reading (confirmed) and its tensor consequence

The Layer A draw grid is **18 weeks** (NFL weeks 1–18). Each player has exactly **one** bye,
present as a `is_bye=TRUE`, value-0 column (`R/blender.R:600,611–618`; `R/simulate.R:107`;
`FEED_SPEC.md:139–140`). So each player has **17 active (non-bye) weeks** = Clay's 17 games;
the priors are out of these 17.

The mask is sampled **among each player's 17 active weeks only**, independent of the bye
column. The bye stays 0; week-18 trim to the 17-week tensor (`R/ev_blocks.R:128`,
`weeks = 1:17`) is unchanged downstream.

**Consequence to state explicitly (hub-required):** every team's bye falls in weeks 5–14, so
within the *shipped* tensor (weeks 1–17) each player has 16 active weeks + 1 bye. The tensor
therefore carries **16 × (prior/17) expected played weeks + 1 bye** per player. This is
consistent because Clay's 17th game (NFL week 18) is dropped from the tensor for **everyone
alike** — the full-season `season_mean` in the projections file still prices all 17 games; the
tensor prices the 16 of them that land in weeks 1–17. The reconciliation gate (§8) is what
ties these two views together.

Per-position miss probabilities (used throughout):

| pos | prior | `p_miss = 1 − prior/17` | `q = prior/17` (kept) |
|-----|-------|-------------------------|------------------------|
| QB  | 16.8  | 0.01176                 | 0.98824 |
| RB  | 15.2  | 0.10588                 | 0.89412 |
| WR  | 16.5  | 0.02941                 | 0.97059 |
| TE  | 16.6  | 0.02353                 | 0.97647 |

These are the **canonical `clay_games = 17`** rates. The mask rate is actually **per-player**,
`p_miss_i = max(0, 1 − prior_pos/clay_games_i)`, which equals the table value for a 17-game
starter and is **0** for players Clay projects at/below the prior — see §3 for why (it carries
PR #34's clamp exactly and prevents double-discounting suspensions/known absences).

---

## 1. Where zeroing enters — **Decision #1**

### The two-stage topology (from Step 0)

There is a **parquet boundary** between two independent draw stages, and — per FEED_SPEC —
`sim_idx` in the parquet and `path` in the tensor are **not a shared world** (different RNG,
re-drawn from marginals):

```
 Stage 1  R/simulate.R  ──► v2/draws/<slate>.parquet (conditional marginals)
   .simulate_player()         (underdog_id, sim_idx, week, draw_value)
        │                                  │
        │ (also: stats recompute)          ▼
        ▼                         Stage 2  R/correlation.R via R/ev_blocks.R
  season_mean/std/...            sample_correlated_draws() → copula → tensor 500×295×17
  (projections JSON)             .inv_cdf_type7() maps U=pnorm(Z) through each
                                 player's empirical marginal
```

### Options considered

**Option A — zero inside the Stage-1 marginal** (my Step-0 entanglement flag). Zeroed weeks
become a 0-atom in the parquet marginal. Stage 2's `.inv_cdf_type7` (`R/correlation.R:199–203`)
then maps the bottom `p_miss` of `U = pnorm(Z)` onto that 0-atom. Because `Z` carries the
team/game factor (`R/correlation.R:192–195`, team loading 0.45), **"player missed this week"
becomes correlated with the team's bad weeks**, and teammates miss *together* on low-factor
weeks. That silently manufactures (a) availability↔performance coupling and (b) handcuff-style
availability contagion — **both explicitly out of scope** for v1. It also corrupts the copula's
marginal-preservation guarantee (it would now preserve a played/missed *mixture*, not the
played distribution, changing the rank-correlation semantics). **Rejected.**

**Option B — independent missed-week mask as a first-class process** (the hub's prior).
- Stage-1 marginal stays **conditional-on-playing** (0 only on byes); the parquet is unchanged
  in shape *and* semantics.
- A mask `M[path, player, week] ∈ {0,1}`, drawn from the position prior **independently of the
  value draws and of the copula latent `Z`**, is applied:
  - **Stage 2:** *post* `.inv_cdf_type7` — `tensor_value = played_value × M`. The copula still
    sees the clean conditional marginal, so correlation among *played* values is untouched and
    missingness is independent of `Z` (no artifact).
  - **Stage 1:** applied to season sums for the stats recompute (§4).
- Because the two stages do not share `sim_idx`/`path`, they use the **same process** (same
  `p_miss`, same model), **not the same realizations** — exactly the FEED_SPEC constraint.

### Recommendation: **Option B (accept the hub's prior).**

**Rationale.** Option B is the only one that keeps v1 zeroing genuinely independent (the stated
scope: no contagion, no handcuff coupling, no availability↔value coupling) while leaving the
copula's marginal-preservation intact. The cost — the parquet marginal does not itself embody
attrition — is acceptable and documented: the parquet was *always* conditional-on-playing (its
only zeros are byes), so its contract is unchanged; attrition is a separate process that both
downstream consumers (stats recompute and tensor build) apply identically. We do **not** reject
the hub's prior.

**Confirmation (hub-requested):** the Stage-2 mask draws its RNG from `build_ev_draws`'s
**existing `seed` parameter** (threaded through to `sample_correlated_draws`), so gate runs are
reproducible — a fixed `seed` yields a fixed mask realization and a byte-stable tensor for the G5
reconciliation and the bucket-table check.

**Correlation artifacts that independent zeroing still introduces (must be stated, and the
magnitude reported — not just the direction):** Even in Option B, multiplying by an independent
Bernoulli mask *dilutes* the realized pairwise correlation measured on the shipped (mixture)
tensor. The per-week correlation is computed over active-both weeks (PR #35 restricted to
non-bye/positive-variance weeks), so the mask thins it whenever the mask's zero-atom enters the
measured sample. The direction is *safe* (it never manufactures spurious positive correlation),
but the magnitude must be quantified against the verified targets.

**Reported-not-gated, with a required Phase-B artifact:** Phase B must reproduce the **PR #35
bucket table** on the post-mask tensor — same format and the same Spearman targets
(team `0.45→0.433`, game `0.25→0.239`, cross `0.05→0.048`; `±0.05` flag on Spearman):

| kind | bucket | n_pairs | Pearson | Spearman | Sp target | gap | flag |
|------|--------|--------:|--------:|---------:|----------:|----:|:---:|
| RB×RB | cross / same_game / same_team | … | … | … | 0.048/0.239/0.433 | … | |
| TE×TE | … | | | | | | |
| WR×WR | … | | | | | | |

Plus a **one-line statement** of (a) whether every bucket stays inside the `±0.05` flag after
masking, and (b) whether the residual dilution, converted by the hub, stays inside the PR #35
**~1.1 mean-EV no-flip margin** (it ran 99.1% of the zero-correlation ideal pre-mask, so the
TE-heavy verdict had comfortable headroom; masking moves correlation *down*, which can only
*increase* harvest value — the safe direction — so the no-flip verdict is not at risk, but the
table makes that explicit rather than assumed). We do **not** gate on the bucket values; the
gate is the §8 reconciliation, not the correlation level.

---

## 2. Missed-week process — **Decision #2** (pre-registered)

### Candidates
- **iid Bernoulli** per active week: each of the 17 active weeks independently kept w.p.
  `q = prior/17`. `E[games] = 17q = prior`. Trivially exact in the mean.
- **Streaky** (geometric run length): injuries cluster into consecutive-week runs with the same
  marginal `p_miss`.

### Why the mean and the per-week cross-section are identical either way
`E[season] = Σ_w E[weekly_max]`, and with the mask independent of values and of other players,
the *per-week* joint distribution of "who is available" is identical under iid and streaky
(both stationary at marginal `p_miss`, both independent across players in v1). So **season mean
and every single-week round window (R2/R3/R4 = weeks 15/16/17) are identical**. Streakiness can
only change the **across-week dependence of one player's availability**, which affects the
*variance* of multi-week windows (R1 = weeks 1–14) and the lumpiness of a backup's fill-in
contribution (the insurance tail).

### Pre-registered decision rule (declared before measuring)
Default: **ship iid.** Adopt streaky only if it (a) is statistically distinguishable from noise
**and** (b) is decision-relevant in EV terms — both, mirroring the PR #35 verdict structure
(measure a BBP-space effect; hand it to the hub for EV conversion against the tie band).

**Why a discriminating threshold is needed.** The earlier "> 1.0×MC-SE" criterion was
underpowered: under the null (iid is correct) a 1-SE band trips ~32% of the time, so it is not a
discriminating rule. The threshold below is set so the false-positive rate under the null is
~0.3%.

**Adjudication measurement (Phase B, no draft sims — scope §9).** Build one RB-deep roster
(stud RB1/RB2 + an RB4 whose value is pure insurance) and one control. For each, compute the
**RB4's marginal contribution to the R1 (weeks 1–14) round-window score** — i.e. the
insurance-channel quantity this whole sprint exists to price — under (i) iid and (ii) a
geometric-run streak model matched to the same marginal `p_miss`. Report it as a BBP-point delta
`ΔBBP_streak = |R1_RB4(streaky) − R1_RB4(iid)|`, with its 500-path MC-SE.

**Two-part decision rule (pre-registered):**
1. **Discriminating gate (model-side):** consider streaky a real effect **iff
   `ΔBBP_streak ≥ 3 × MC-SE`** of that statistic (the 500-path SE; `≈ 1/√500` on a correlation-
   like quantity, computed exactly at runtime for the actual statistic). Below 3×SE → noise →
   **ship iid, full stop.**
2. **EV materiality (hub-converted):** if it clears the discriminating gate, hand `ΔBBP_streak`
   to the hub exactly as PR #35 Part 2 handed its shrinkage curve. **Ship streaky only if the
   hub's EV conversion of `ΔBBP_streak` exceeds the ~1.1 mean-EV near-tie margin** used for the
   depth-piece-vs-alternative choice. This sprint produces the BBP delta; the EV conversion and
   the flip decision are the hub's, because the EV brain / harness is out of scope here (§9).

**Prior belief (to be tested, not assumed):** iid passes both — best-ball value lives in the
per-week lineup mechanic, which is mask-marginal-identical between iid and streaky; streakiness
only re-shapes the across-week dependence of one player, a second-order effect on the R1 sum.

---

## 3. Mean-preservation mechanics — **Decision #3** (no double discount)

### What reverts, what stays (exact trace)

**REVERTS (the PR #34 upstream factor is removed where the mask replaces it):**
- `R/blender.R:59–64` — `.apply_availability_to_clay()` no longer scales Clay's
  `projected_points_half_ppr`/`_full_ppr`. It becomes a no-op for points (the loader/prior is
  retained only to *feed the priors into the mask* and the `_meta` marker; see §4).
- `build_calibration_curves()` (`R/blender.R:68`) is therefore fit on **unscaled** Clay points.
- `.clay_rows_for_slate()` (`R/blender.R:71`, line 346) carries **unscaled** Clay points.
- Consequence: the blender consensus mean (`R/blender.R:500`) and the per-week conditional means
  from `.generate_weekly()` (`per_act = season_mean / active_weeks`, `R/blender.R:605`) are now
  **unscaled, conditional-on-playing rates** — i.e. "points per active week if the player plays."

**STAYS:**
- `inst/adjustments/availability.yaml` is still read; the priors now parameterize `p_miss`.
- ETR/LegUp are still calibrated onto Clay's spline — but now the **unscaled** spline. They
  inherit the unscaled conditional level, exactly as they previously inherited the scaled level.
  (Rank-only sources track whatever level Clay's curve carries; removing the scale moves all
  three sources up together, preserving cross-source consensus shape.)

### The no-double-discount identity
Let `u_i` = unscaled conditional season rate (blender consensus, post-revert). Then:
```
E[season_i]  =  u_i × E[fraction of 17 active weeks played]  =  u_i × (prior_pos / 17)
```
PR #34's deployed mean was `u_i × (prior_pos / 17)` applied at the season level (for a player
Clay projects at 17 games). **The factor `prior/17` appears exactly once** — via the mask —
because it has been removed upstream. Applying it in both places (the failure mode) would give
`u_i × (prior/17)²`; the test below rules that out.

### Per-player `clay_games < 17` — the mask rate carries PR #34's clamp (not a deviation)
PR #34 used `f = min(1, prior/clay_games_i)` — a clamp that protected players Clay already
projects below the positional prior (suspensions, known absences, returning-from-injury, buried
handcuffs: `f = 1`, no discount). A **uniform** `p_miss = 1 − prior/17` would **double-discount
exactly those players** (Clay has already priced their reduced season, and the mask would cut it
again). Flagging that is not handling it. **The mask rate is therefore per-player, set to
reproduce PR #34's clamp exactly:**

```
p_miss_i = max(0, 1 − prior_pos / clay_games_i)        q_i = min(1, prior_pos / clay_games_i)
```

- `clay_games_i ≥ prior` (the overwhelming majority; Clay projects ~17): `q_i = prior/clay_games_i`,
  which for the canonical 17-game starter equals the §0 table value `prior/17`.
- `clay_games_i ≤ prior` (sub-prior players): `p_miss_i = 0` — **zeroing is skipped**, Clay's own
  reduced projection stands untouched. No second discount.

This is **not a new per-player injury-probability layer** (which the decision declines): it reuses
the *same* `clay_games_i` PR #34 already used, only as a mask rate instead of a point scale. The
prior remains the sole tuning input. Result: `E[season_i] = u_i × q_i = u_i × min(1, prior/clay_games_i)`
= **PR #34's deployed level, per player, exactly** — so this is preservation, not a bounded
deviation. (Note: the implied expected *games* is `17·q_i`; for a sub-17 player whose zeroing is
skipped the points level is exactly Clay's, which is the quantity the gates preserve.) The
Brooks-class review (`R/availability.R:137–173`) still surfaces extreme overshoots for manual
`adjustments:`, unchanged.

### Pre-registered no-double-discount unit test
Two synthetic RBs (`prior = 15.2`):
1. **17-game RB, `u = 200`, `clay_games = 17`:**
   - *Upstream removed:* the blended `season_mean` equals the no-availability baseline `u`
     (value with the prior `enabled: false`), within `1e-6` — proves the factor is gone from the
     level entering the draws.
   - *Mask applies once:* `mean(masked_season_sums)` over a large fixed-seed sim → `u·q = 178.8`
     (`q = 15.2/17`), **not** `u·q² = 159.9` and **not** `u = 200`. The three candidates are far
     apart → unambiguous.
2. **Sub-prior RB (clamp), `u = 120`, `clay_games = 12`:** `q_i = min(1, 15.2/12) = 1`, so
   `p_miss_i = 0`. Assert `mean(masked_season_sums) → u = 120` within MC tol — **no second
   discount** on a player Clay already projects below the prior. (A uniform mask would wrongly
   give `120 × 15.2/17 = 107.3`; the test rules that out.)
3. **QB-rate, `u = 300`, `clay_games = 17` (`p_miss = 0.0118`):** the low-rate regime where G5's
   per-player tol is blind (§8). Assert `mean(masked_season_sums) → u·q = 300 × 16.8/17 ≈ 296.5`,
   **not** `u = 300`. Confirms the mask mechanism fires correctly at small `p_miss` — the
   player-scoped check G5 cannot make.

### Pre-registered MC mean-drift tolerance (Stage 1, `n_sims = 10000`)
The projections `season_mean` is produced by **Stage 1** at the production
`n_sims = 10000` (`.github/workflows/build_feed.yml:68`), **not** 500. The drift compares the
current deployed `season_mean` (analytic consensus — effectively MC-noise-free) against the new
**masked** `season_mean` (Stage-1 MC).

**Season CV after masking — full variance decomposition** (representative mid-RB: conditional
`u = 200`, per-week `cv = 0.50` → `μ_w = u/17 = 11.76`, `σ_w = cv·μ_w = 5.88`; `q = 15.2/17 =
0.894`; representative `disagreement_std D = 12`). Three independent components of the *masked*
season total `S' = s·Σ_w X_w M_w`, evaluated around the masked mean `E[S'] = q·u = 178.8`:

| component | formula | value |
|-----------|---------|------:|
| disagreement (epistemic, scaled) | `q²·D²` = `0.799 × 144` | 115.1 |
| aleatoric on played weeks | `q·A²` = `q·Σσ_w²` = `0.894 × 588` | 525.7 |
| **availability (mask)** | `Σ q(1−q)μ_w²` = `17 × 0.0948 × 138.3` | 222.9 |
| **Var(S')** | sum | **863.7** |

`SD(S') = 29.4` → `CV_season = 29.4 / 178.8 ≈ 0.164`.

**Stage-1 MC SE of the mean:** `SE₁ = CV_season · E[S'] / √n_sims = 0.164 × 178.8 / √10000 =
0.293 pts = 0.164%` of the mean.

So MC noise alone is ~0.16% — the **2% tolerance is deliberately ~12×SE₁ because the dominant
drift contributor is not MC noise, it is the spline re-fit**: PR #34 scaled Clay points *then*
fit the calibration spline; the new design fits on *unscaled* points. Since the per-player clamp
makes `f = min(1,prior/clay_games)` **non-uniform** across a position, `fit(scaled) ≠ f·fit(unscaled)`
near the ranks of sub-prior Clay players, shifting ETR/LegUp calibrated values by up to ~1%.
The 2% budget = spline-refit (~≤1%) + `season_mean` rounding to 0.1 + MC (0.16%), with margin.

- **Per-player mean drift** (deployed vs new masked `season_mean`): **`|Δ| ≤ 2%`** — pre-registered.
- **Per-position aggregate**: **`|Δ| ≤ 0.5%`**. Pooling over `N_pos` players shrinks the *MC*
  component by `√N_pos` (RB `N≈83` → `0.164%/9.1 ≈ 0.018%`) but **not** the spline-refit shift,
  which is correlated within position — hence 0.5%, set above the pooled-MC floor to admit the
  systematic while still catching a position-wide structural error (a double-apply would be
  `~p_miss ≈ 10%` for RB, far outside 0.5%).

These tolerances catch structural failure (factor applied twice → ~10% RB offset; not applied →
+12%) with wide margin, and never trip on MC noise or the legitimate spline re-fit.

---

## 4. Downstream recompute plan — **Decision #4**

### Pipeline ordering change
`.add_position_metrics()` (`R/blender.R:104,691–735`) currently runs **inside `blend_slate()`
pre-sim**, off the consensus `season_mean`. It must move to **post-sim**, off the masked
`season_mean`, because VOR/tiers/position_rank must reflect attrition (RB tiers in particular
shift when RB means drop ~11%). Proposed: `simulate_slate()` (or a thin post-sim step in
`publish_v2()`) recomputes the masked season stats, then calls `.add_position_metrics()` on the
enriched feed. No change to the *function*; only **when** it runs.

### Producer flips (pre-sim → post-sim, from masked Stage-1 sums)

| Field | Today | After | Notes |
|-------|-------|-------|-------|
| `season_mean` | pre-sim consensus (`blender.R:500`) | **post-sim** mean of masked season sums | now `≈ u×q` |
| `season_std` | pre-sim `√(disagr²+aleat²)` (`blender.R:503`) | **post-sim** empirical std of masked sums | now includes mask variance; **invariant retired** (below) |
| `position_rank`,`vor`,`tier` | pre-sim (`blender.R:691–735`) | **post-sim** from masked `season_mean` | function unchanged, runs later |
| `season_percentiles` | already post-sim (`simulate.R:130`) | post-sim from masked sums | mechanically same path, masked draws |
| `weekly[[w]].percentiles` | already post-sim (`simulate.R:142`) | post-sim, see §7 | semantics decision in §7 |
| `weekly[[w]].mean/std` | pre-sim (`blender.R:635`) | see §7 | conditional vs unconditional decision |

**`season_std` invariant retired.** The documented invariant `season_std² = disagreement_std²
+ aleatoric_std²` (`R/blender.R:519–521`) **no longer holds** once mask variance enters. Plan:
ship `season_std` as the **empirical post-mask std**; keep `disagreement_std`/`aleatoric_std` as
the **pre-mask, conditional-on-playing analytical components** (documented as such), and add a
reported (not shipped-as-invariant) `availability_std` decomposition note in `_meta`. **Test
impact:** `tests/testthat/test-simulate.R:53–59` ("empirical season std matches analytical
season_std within 5%") asserts the *unmasked* `.simulate_player` output against the analytical
value — it stays valid as a unit test of the **conditional** model (which is unchanged), because
the mask is a separate function. A **new** test covers the masked shipped `season_std`.

### Byte-identical-shape artifacts (must NOT change)
- **Tensor:** `500×295×17` `int16` LE, value ×10, C-order `[path][player][week]`, decode spec
  unchanged (`R/ev_blocks.R:172–227`, `FEED_SPEC.md:160–170`). The mask multiplies values to 0
  *before* quantization → still `int16`, same dims, same decode. Contract test (§6) stays green.
- **Draws sidecar JSON:** same fields/axis_order/shape.
- **Parquet draws:** same 4 columns, still 18 weeks, **still conditional-on-playing** (0 only on
  byes). Per Option B the mask is *not* written into the parquet (writing it would re-introduce
  the §1 entanglement). **Documentation note for FEED_SPEC:** make explicit that
  `draws.parquet` is the conditional-on-playing marginal and that availability is a separate
  process embodied in the tensor and the projections season stats — not in the parquet.

### `source_breakdown` — meaning change (must be flagged)
`source_breakdown` (`R/blender.R:439–479`) reports each source's `raw_points` /
`calibrated_points`. After the revert these become **unscaled, conditional-on-playing** values
(higher than today's scaled numbers), and they **no longer reconcile to `season_mean`** (which
is now masked/lower). Recommendation: keep `source_breakdown` as the honest **conditional**
inputs and document in `_meta` that `season_mean = E[conditional × availability]`. No shape
change; a semantic clarification consumers should be told about.

### `_meta` mechanism marker — replace
Replace the PR #34 scaling summary (`_meta.availability_adjustment` with `mechanism = "clay
points scaled by …"`, `per_position.mean_factor`, `players_scaled`; `R/availability.R:111–120`,
`R/blender.R:806–808`) with a **mechanism marker** (the prompt's suggestion):
```jsonc
"availability_mechanism": {
  "type": "draw_zeroing",
  "missed_week_model": "iid_bernoulli",      // or "geometric_run" if §2 flips
  "applied_over": "17 active (non-bye) weeks of the 18-week grid",
  "rate": "p_miss_i = max(0, 1 - expected_games[pos] / clay_games_i)",  // per-player; carries PR #34's clamp
  "expected_games": { "QB":16.8, "RB":15.2, "WR":16.5, "TE":16.6 },
  "p_miss_canonical_17g": { "QB":0.0118, "RB":0.1059, "WR":0.0294, "TE":0.0235 }
}
```
Asserted by markers (not shas), per the process rules.

---

## 5. Bye handling — **Decision #5**

The mask is sampled **only over each player's active (`is_bye=FALSE`) weeks**; bye weeks are
left at 0 by the existing `is_bye` path and are **never** candidates for masking. So a bye is
not a "missed game," and `p_miss` cannot stack on top of a bye. `E[games_played] = 17 × q =
prior` over the 17 active weeks, exactly. Implementation reads `is_bye` from the same weekly
records the schedule is built from (`R/ev_blocks.R:39–46`), so Stage 1 and Stage 2 agree on
which week is the bye for each player.

---

## 6. Gates (proposed numbers)

| # | Gate | Threshold (pre-registered) |
|---|------|----------------------------|
| G1 | Per-position gap vs UD market reproduces PR #34 levels (RB +2.1%, QB −2.3%, WR −0.3%, TE −3.0%), **uniform across tiers** | each position within **±0.6 pp** of the PR #34 number; tier-by-tier spread of the gap **≤ 1.0 pp** within a position |
| G2 | bbmdb points-space ratio ≈ 1.008; MAE ≤ 0.124; Spearman > 0.3 | **unchanged existing floors — not retuned** (per scope) |
| G3 | No-double-discount unit test (§3) | green |
| G4 | Tensor decode contract test (existing decode spec) | green, unchanged |
| G5 | **Feed-internal consistency** (hub-required, §8) | per-player `|Δ| ≤ 3%`, position-aggregate `≤ 0.7%`. Low-`p_miss` positions (QB/WR/TE) rely on the aggregate row + **G1**+**G3** for one-stage / player-scoped mismatches (§8) |
| — | **Directional sanity (reported, NOT gated):** `season_std` rises at attrition-heavy positions | RB `season_std` up; expected magnitude below |

G1 rationale: PR #34's per-position gaps were the *level* target; the mask reproduces the same
expected level **per player** (`u_i × q_i = u_i × min(1, prior/clay_games_i)`, §3), so the
aggregate gap should land within position-aggregate MC SE (§3, ≤0.02% for RB) plus the
spline-refit shift (≤~1% per player, largely averaging out in aggregate). ±0.6 pp covers that
with margin. The tier-spread sub-check enforces "uniform across tiers": because `clay_games_i ≈ 17`
for essentially all drafted starters across every tier, the per-player rate is effectively the
position-uniform `prior/17` across tiers; the only sub-prior exceptions are the Brooks-class
outliers (suspensions/handcuffs), which are not tier-systematic. A gap that tilted by tier would
signal an unintended interaction and fail the sub-check.

**Expected `season_std` rise (reported):** masking adds per-week variance
`(1−q)·E[played]²`. As a fraction of season variance the RB rise is the largest (`p_miss`
≈ 0.106). Rough estimate: RB `season_std` up **~3–6%**, QB/WR/TE up **<1%**. Reported with
actual numbers in Phase B, not gated.

---

## 7. Weekly mean/std semantics — **Decision (hub-required)**

**The question:** after masking, `weekly[[w]].mean` can be *conditional-on-playing*
(`per_act × mult`, the played rate) or *unconditional* (`q × played rate`, includes the miss
chance). Today they coincide because `p_play = 1`.

**Consumer audit (hub-required):**
- `schedule_from_feed()` (`R/ev_blocks.R:25–46`) reads weekly only for
  `week/team/opponent/is_bye` — it does **not** read `mean`/`std`. **Indifferent** to the choice.
- **Extension display** — out of scope to modify and not in this repo, so its assumption cannot
  be read here. **This is a required pre-Phase-B check** (see below). The one fact we *do* know:
  in today's feed `Σ_active weekly.mean = season_mean` (because `.generate_weekly` derives
  weekly means from `season_mean`). Any consumer that relies on that aggregation identity will
  silently break if we change weekly semantics without preserving it.

**Recommendation: ship `weekly.mean/std` as UNCONDITIONAL** (`q × conditional rate`), so the
existing identity `Σ_active weekly.mean ≈ season_mean` is **preserved** (within MC tol), and
ship `weekly.percentiles` from the masked draws (coherent: for RB, `p10` may be 0 since
`p_miss ≈ 0.106`). Rationale: this protects any consumer that aggregates weekly→season (the
identity that holds today), and the per-week value becomes "expected points including
availability," which is defensible for a projection. The cost is a small downward shift in the
displayed single-week number (material only for RB, ~11%).

**Pre-registered semantics + gate:** ship `weekly.mean/std = unconditional` and verify
`|Σ_active weekly.mean − season_mean| ≤ 1%` per player.

**RESOLVED (hub audit complete).** The extension audit found that **nothing in the shipped
extension or popup reads `weekly[].mean`/`std` (or indexes `weekly[]` at all — it is dead-carried
via spread).** So there is no consumer to break: we **ship unconditional** as recommended and the
`|Σ weekly.mean − season_mean| ≤ 1%` identity gate stands. **Expected, not a bug:** RB
`weekly.percentiles.p10` will legitimately be **0**, because `p_miss ≈ 0.1059 > 0.10` — more than
10% of paths zero that week, so the 10th percentile lands on the mask atom. Phase B notes this in
the spot checks rather than flagging it.

---

## 8. Feed-internal consistency / reconciliation gate — **Decision (hub-required)**

**Requirement:** the projections-file season stats and the tensor must embody the **same**
availability process (same `q`), even though they are independent MC realizations.

**Gate G5.** For each player `i`:
```
E_tensor[i]      = mean over 500 paths of Σ_{w=1..17} decode(tensor[path, i, w])     # weeks 1–17
proj_W1to17[i]   = season_mean[i] − E[week-18 contribution]
                 = season_mean[i] − ( q_i · weekly_conditional_mean(week 18, i) )    # 0 if no wk-18 game
assert |E_tensor[i] − proj_W1to17[i]| / proj_W1to17[i] ≤ tol_player
assert position-aggregate |Δ| ≤ tol_pos
```
**Confirmation (hub-requested):** the week-18 adjustment uses the **per-player `q_i =
min(1, prior/clay_games_i)`** (§3), *not* the position scalar `prior/17` — consistent with the
per-player mask rate, so a sub-prior player (whose `q_i = 1`) is adjusted by his full conditional
week-18 mean. The adjustment uses the projections' own weekly records (per-week conditional mean
+ `is_bye`), so the comparison is apples-to-apples on weeks 1–17.

**Pre-registered tolerances — full MC-SE derivation.** The two sides are **independent** MC
estimates of (essentially) the same per-player quantity, at **different sim counts**:

- **Stage 2 (tensor), `n_paths = 500`** (`build_ev_draws` default; confirmed in PR #35 Step 0):
  `SE_tensor = CV_{W1-17} · mean / √500`. Using the §3 representative `CV ≈ 0.164` (weeks 1–17
  carry ~16/17 of the mass, CV essentially unchanged): `SE_tensor = 0.164 / 22.36 = 0.733%` of
  the mean. This is the looser side.
- **Stage 1 (projections), `n_sims = 10000`** (`build_feed.yml:68`):
  `SE_proj = 0.164 / 100 = 0.164%` of the mean (= §3 `SE₁`). ~4.5× tighter.
- **Combined difference** (independent → variances add):
  `SE_Δ = √(SE_tensor² + SE_proj²) = √(0.733%² + 0.164%²) = √(0.537% + 0.027%) = √0.564% =
  0.751%` of the mean.

- **`tol_player = 3%`** ≈ **4.0 × SE_Δ** — never trips on MC noise, yet catches a *process
  mismatch*: mask applied in only one stage → offset ≈ full `p_miss` (`~10.6%` RB / `~2.9%` WR);
  double-apply in one stage → another `~p_miss`. For RB this is far outside 3%. **Coverage of the
  low-`p_miss` positions, precisely:** the per-player 3% tol has **no power** against one-stage-
  only masking at QB/WR/TE (`p_miss` 1.2–2.9% all `< 3%`). That failure is instead caught **at
  every position** by the *aggregate* gate — a position-wide one-stage offset is systematic
  (`≥ 1.18%` even for QB) and so exceeds `tol_pos = 0.7%` — and corroborated by the **G1**
  per-position level gate. The only residual uncovered case is a *player-scoped* process mismatch
  at a low-`p_miss` position; that is a mechanism bug, not a level error, and is **G3's** job (the
  no-double-discount unit test, which now includes a QB-rate case, §3). G5's row therefore
  cross-references G1+G3.
- **`tol_pos = 0.7%`** (position-aggregate). The MC component pools by `√N_pos`
  (RB `N≈83`: `SE_Δ/9.1 ≈ 0.082%`); since both stages use the *same* mask process there is no
  systematic offset to survive pooling, so 0.7% is ~8.5× the pooled-MC SE — generous against
  noise, decisive against a process mismatch.

This gate is the safety net that makes "same process, not same realizations" *verifiable*.

---

## 9. What this sprint does NOT adjudicate

The 4th-RB construction question is **not** answered here. It is answered later, at the harness
level, against the deployed feed — by the hub, not in this sprint. **No draft simulations are
run in Phase B.** This sprint ships the model-side channel (draw zeroing) that *makes depth-as-
insurance priceable*; whether the RB4 clears its ADP is a separate, downstream measurement.

---

## 10. Build order (for Phase B, after approval)

1. Revert the upstream scaling in `.apply_availability_to_clay` (points become a no-op); retain
   prior load + `_meta` marker rewrite (§3, §4).
2. Add the mask process (one function, position `p_miss`, active-weeks-only) — Decision #1
   Option B. Apply post-inverse-CDF in `R/ev_blocks.R`/`R/correlation.R` (tensor) and to season
   sums in `R/simulate.R` (stats).
3. Move `.add_position_metrics` post-sim (§4 ordering).
4. Recompute season_mean/std/percentiles/VOR/tiers/weekly from masked draws; set weekly
   semantics per §7.
5. Run §2 iid-vs-streaky adjudication; lock the model per the pre-registered rule.
6. Run all gates G1–G5 + report the directional `season_std` rise and before/after spot checks
   (Gibbs, CMC, Chase, one mid-tier TE).

---

## Pre-registration summary (declared before measurement)

- **Mask rate:** per-player `p_miss_i = max(0, 1 − prior_pos/clay_games_i)` (carries PR #34's
  clamp; zeroing skipped for sub-prior players) over the 17 active weeks (§3).
- **Model:** iid Bernoulli; flip to geometric-run **iff** the RB4-insurance R1-window ΔBBP
  clears **≥ 3×MC-SE** *and* the hub's EV conversion exceeds the ~1.1-EV no-flip margin (§2).
- **No-double-discount:** 17-game RB masked mean → `u×q` (not `u×q²`, not `u`); sub-prior RB
  (`clay_games=12`) masked mean → `u` (no second discount); upstream factor removed asserted at
  `1e-6` (§3).
- **Mean drift:** per-player ≤ 2%, per-position ≤ 0.5% (§3; Stage 1 `n_sims=10000`, SE₁≈0.16%).
- **G1 level:** per-position gap within ±0.6 pp of PR #34; tier-spread ≤ 1.0 pp (§6).
- **G5 reconciliation:** per-player ≤ 3%, per-position ≤ 0.7% (§8).
- **Weekly semantics:** unconditional; `|Σ weekly.mean − season_mean| ≤ 1%`; extension audit is a
  blocking pre-Phase-B check (§7).

**PAUSE.** No build until this doc is approved.
