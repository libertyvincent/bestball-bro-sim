# TE cross-correlation — verifying the tensor's load-bearing assumption

**Diagnose-only.** No changes to tensor generation, projections, curves, or any
`src`/`R` package code. Reproduce with `Rscript diagnostics/te_correlation_check.R`
(reads only published CDN artifacts).

## The question

The EV brain's TE-heavy construction is a slot-structural edge **only if the
premise holds: your TEs don't boom and bust together.** The single TE slot
harvests the weekly max of the TE room, and the max-of-N value collapses as
cross-player correlation rises. Means (−7% vs market) and weekly variance are
already verified; the **correlation structure** never was. The thesis rides on
ρ_cross ≈ 0.05 being (a) actually implemented and (b) approximately true of
reality. This report settles **(a)** (Part 1) and quantifies the stakes of
**(b)** (Part 2). **(b)** itself needs weekly actuals that don't exist in-repo
(Part 3 descoped).

## Build pinned at runtime

The feed rebuilds daily from source, so the artifact is pinned at decode time,
not pre-committed. Part 1 ran entirely against:

| field | value |
|---|---|
| draws `generated_at` | `2026-06-10T07:11:21Z` |
| bin sha256 | `293696027b3e9c02471a552d54a26e32afb40db1fe3d7e4b20d030527f2145df` |
| sidecar sha256 | `36d85ec6d461ff73e5d0add30b69085c331268aee102590725b28fe1c430f6a8` |
| projection feed `generated_at` | `2026-06-10T07:07:08Z` (schedule source; same build) |
| availability adjustment | present (RB factor 0.8951) — post-PR #34 |

(Hub's prompt pinned an earlier 06:46:44Z build; a source-driven rebuild replaced
it ~24 min later. Accepted by hub — the diagnostic's subject is the correlation
structure, which is byte-invariant across the rebuild: identical copula params,
#34 touches no correlation file.)

## Step 0 — where correlation lives (confirmed against code)

- **Path:** `deploy_ev_blocks.R::publish_ev_blocks_pipeline` →
  `ev_blocks.R::build_ev_draws` → **one joint**
  `correlation.R::sample_correlated_draws(corr_params = default_corr_params,
  n_sims = 500, output_format = "matrix_list")` over the top ~295 players. The
  shared 500-path axis **is** the correlation mechanism.
- **Mechanism:** nested variance-components **Gaussian factor copula**, applied
  **per week with independent factors each week**:
  `Z_i = √cross·G + √(game−cross)·F_game(i) + √(team−game)·F_team(i) + √(1−team)·E_i`,
  then mapped to each player's Layer-A marginal by empirical inverse-CDF
  (type-7). Marginals preserved exactly; rank structure imposed. Season-level
  epistemic teammate correlation is explicitly **out of scope** (documented
  TODO) — so this is purely within-week co-movement, which is what the
  max-harvest cares about.
- **Parameters:** `default_corr_params = list(team = 0.45, game = 0.25,
  cross = 0.05)` — exactly the remembered Option A triple. **Uniform /
  position-blind**: a TE×TE cross pair gets `cross = 0.05`, same as any pair.
  The code flags this as a KNOWN LIMITATION (flat same-team value overstates
  RB↔RB / QB↔RB).
- **#34 impact:** none on correlation. The #34 diff touched only
  `R/blender.R`, `R/availability.R`, the yaml, and tests. Availability changes
  feed **marginals** (which flow into the tensor's draws) but not the copula or
  its parameters.
- **In-tensor counts** (live sidecar): QB 39, RB 83, **TE 46**, WR 127 = 295.
- **Weekly actuals in-repo:** none. `bestball-bro-sim`, `bestball-bro-data`, and
  `bbmdb_scraper` hold only projections / simulated draws / season-total labels;
  the v1 nflverse `load_player_stats` pipeline was retired. **Part 3 descoped.**

## Part 1 — measured correlation in the live tensor

Per-week Pearson **and** Spearman across the 500 paths, averaged over weeks where
both players are non-bye/active (positive variance). Restricted to **top-40 per
position by in-tensor season mean** (fantasy-relevant; dead-roster noise removed).
Two statistics, distinct jobs (per hub):

- **Spearman** = implementation fidelity (monotone-invariant; survives the
  inverse-CDF). Target = latent Gaussian Spearman `(6/π)·arcsin(ρ/2)`: team
  0.45→**0.433**, game 0.25→**0.239**, cross 0.05→**0.048**. The **±0.05 flag**
  applies to this comparison.
- **Pearson** = the economic stat the harvest math feels.

| kind | bucket | n_pairs | Pearson | Spearman | Sp target | gap | flag |
|---|---|--:|--:|--:|--:|--:|:--:|
| **TE×TE** | **cross** | 772 | **0.047** | **0.046** | 0.048 | −0.002 | |
| TE×TE | same_game | 351 | 0.245 | 0.236 | 0.239 | −0.003 | |
| TE×TE | same_team | 8 | 0.440 | 0.429 | 0.433 | −0.004 | |
| RB×RB | cross | 772 | 0.049 | 0.047 | 0.048 | −0.001 | |
| RB×RB | same_game | 349 | 0.248 | 0.239 | 0.239 | −0.001 | |
| RB×RB | same_team | 8 | 0.445 | 0.430 | 0.433 | −0.003 | |
| WR×WR | cross | 766 | 0.047 | 0.046 | 0.048 | −0.002 | |
| WR×WR | same_game | 349 | 0.247 | 0.238 | 0.239 | −0.001 | |
| WR×WR | same_team | 14 | 0.447 | 0.433 | 0.433 | −0.001 | |
| TE×WR | same_team (stack) | 50 | 0.443 | 0.431 | 0.433 | −0.002 | |

Per-week mean; per-cell sd ≈ 0.045 in the cross buckets is exactly the sampling
SE of one 500-path correlation (1/√500 = 0.0447) — i.e. no heterogeneity beyond
noise; the bucket means are resolved to ≈±0.001.

**Pooled-across-weeks robustness** (mixes within-week co-move with schedule-level
mean co-move): TE×TE cross 0.051 / WR×WR cross 0.051 / RB×RB cross 0.052 — within
0.004 of the per-week values. No material divergence; no hidden schedule-level
co-boom.

### Part 1 findings

1. **Implementation fidelity CONFIRMED — premise (a) holds.** Every bucket
   matches its latent target to within **|gap| ≤ 0.004**, far inside the ±0.05
   flag. The Option A copula (team 0.45 / game 0.25 / cross 0.05) is correctly
   implemented in the live tensor. **TE×TE cross = 0.047 Pearson / 0.046
   Spearman** — the TE room is, as designed, essentially uncorrelated across
   teams.
2. **Position-blind, confirmed empirically.** TE, RB, and WR are statistically
   identical in every bucket (cross ≈0.047, same_game ≈0.239 Spearman, same_team
   ≈0.43). TE is not special in the tensor — it carries the same 0.05 cross as
   everyone.
3. **Attenuation is negligible at the top-40 level.** Pearson ≈ Spearman in every
   bucket (cross 0.047 vs 0.046), so the harvest math feels essentially the full
   latent 0.05 — the inverse-CDF onto top-40 marginals barely attenuates.
4. **The open risk is entirely premise (b):** the model *assumes* 0.05; whether
   real TEs co-move at 0.05 vs something higher is unverified (no weekly actuals
   in-repo). Part 2 quantifies how much the TE-room harvest value would shrink if
   reality is higher than 0.05 — that is the decision-relevant question.

**Checkpoint 2 — measured ρ matches intent (no mismatch), so no re-scope.**

## Part 2 — sensitivity: TE max-of-N harvest value vs ρ_cross

Same live build. Per hub, attenuation is negligible at top-40, so latent ρ ≈
realized ρ in the sweep — no correction. Representative rooms by in-tensor
season-mean rank:

- **TE room** (ranks 8/15/22/28/35): means 138.7 / 121.2 / 104.0 / 85.1 / 61.0
- **RB contrast** (ranks 8/22): means 227.6 / 173.5

### Shuffle test (model-free, real tensor) — correlation drag already priced

E[season-summed weekly max] of the room from the actual joint draws, vs the same
after destroying cross-player dependence (a fresh independent path permutation
for every player×week cell, averaged over 200 replicates).

| room | real (as-shipped) | shuffled (independent) | gap = drag |
|---|--:|--:|--:|
| TE room (4) | 185.1 | 186.8 (sd 0.23) | **1.71 pts (0.92%)** |
| RB room (2) | 253.5 | 255.0 (sd 0.29) | 1.42 pts (0.56%) |

The tensor's TE-room value is **99.1% of the zero-correlation ideal** — at the
shipped ρ≈0.05, co-movement removes <1% of the harvest. The RB contrast is
similar (position-blind, as expected).

### Synthetic ρ sweep — equicorrelation copula, tensor marginals

All-cross TE room (different teams/games), latent equicorrelation ρ swept, fresh
factors per week, mapped to each TE's week-w empirical marginal.
**Cross-check:** synthetic V(4) @ ρ=0.05 = 184.9 vs the real tensor 4-TE room
185.1 — the synthetic reproduces the live tensor to 0.1%, so the sweep is
anchored to reality at the verified ρ.

**Marginal value of the m-th TE (Δ E[weekly max], season-summed, pts):**

| | ρ=0.05 | ρ=0.15 | ρ=0.30 | ρ=0.50 |
|---|--:|--:|--:|--:|
| TE3 | 13.05 | 12.24 | 10.71 | 8.34 |
| TE4 | 3.85 | 3.54 | 2.95 | 2.20 |
| TE5 | 0.51 | 0.49 | 0.45 | 0.32 |

**Absolute shrinkage vs ρ=0.05 (pts) — the Δ of the Δ, for EV conversion:**

| | ρ=0.15 | ρ=0.30 | ρ=0.50 |
|---|--:|--:|--:|
| TE3 | 0.81 | 2.33 | 4.71 |
| TE4 | 0.31 | 0.91 | 1.66 |
| TE5 | 0.02 | 0.07 | 0.19 |

**% shrinkage vs ρ=0.05:**

| | ρ=0.15 | ρ=0.30 | ρ=0.50 |
|---|--:|--:|--:|
| TE3 | 6.2% | 17.9% | 36.1% |
| TE4 | 8.1% | 23.5% | 42.9% |
| TE5 | 4.3% | 12.8% | 37.1% |

### Part 2 observations (curve only — the verdict is hub's)

1. **Room depth decays far faster than correlation does.** Even at the verified
   ρ=0.05, the marginals fall off a cliff with depth: TE3 = 13.0 pts, TE4 = 3.9,
   **TE5 = 0.5 pts** (season-summed). The TE-heavy edge lives in TE3 and a
   shrinking TE4; the 5th TE is nearly valueless *regardless* of correlation.
2. **The decision-relevant marginals (TE4/TE5) shrink moderately with ρ in %,
   but the absolute points are small.** Worst-case ρ=0.50: TE4 loses 1.66 pts,
   TE5 loses 0.19 pts. At a pessimistic-but-plausible ρ=0.30 (6× the assumption):
   TE4 loses 0.91 pts, TE5 loses 0.07 pts. TE3 is the largest absolute mover
   (2.33 pts at ρ=0.30, 4.71 at ρ=0.50).
3. **Hand-off to hub's pre-registered rule:** the threshold is the ~1.1 mean-EV
   near-tie margin on the TE4/TE5-vs-alternatives choice; hub converts these BBP
   point shrinkages to EV against the extension's ΔBBP magnitudes. The numbers
   above are the conversion inputs. Whether the recommendation flips is hub's
   call — this report ends at the shrinkage curve.

**Part 3 (empirical 2025 anchor): out of scope — no weekly actuals in-repo.** If
the sweep proves decision-relevant after hub's EV conversion, the empirical
anchor for premise (b) becomes a separate small data-sourcing task.

## Summary

- **Premise (a) — "ρ_cross ≈ 0.05 is implemented" — VERIFIED.** The live tensor
  carries the Option A copula to within |Spearman−target| ≤ 0.004 in every
  bucket; TE×TE cross = 0.047. Implementation is correct and position-blind.
- **Correlation drag on the TE room is <1% at the shipped ρ** (shuffle test).
- **The open question is premise (b)** — whether reality is 0.05 or higher — and
  Part 2 prices it: the TE4/TE5 marginals shrink 8–43% / 4–37% across
  ρ ∈ {0.15, 0.30, 0.50}, in absolute terms ≤1.7 pts. Hub's EV conversion against
  the ~1.1 EV near-tie margin decides whether the TE-heavy recommendation holds.
