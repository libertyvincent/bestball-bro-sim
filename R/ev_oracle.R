#' Co-moving low-variance EV oracle (sprint reference truth)
#'
#' A reference EV simulator built to re-gate the v1 (pooled/static) curves.
#' It fixes the two defects of [compute_team_ev()] as a ground truth:
#'
#' 1. **Co-moves at every round.** The field is scored on the *same* joint
#'    Artifact-A draws as the evaluated roster, and survivor selection is
#'    carried round-to-round, so the QF/SF/final fields are the actual
#'    co-moving survivors of each path -- not iid static draws
#'    ([compute_team_ev()] co-moves only at the qualifier) and not the
#'    full pre-tournament field.
#' 2. **Resolves the rare-event final EV with low variance.** Pod
#'    assignment is integrated out analytically: each field roster carries
#'    a *survival weight* (its marginal advance probability on that path),
#'    so the survivor pool of each round is the field re-weighted -- the
#'    Rao-Blackwellized survivor field, with zero pod-draw Monte-Carlo
#'    variance. The convex final ladder is integrated analytically per
#'    path via a binomial over the realized co-moving finalist field.
#'
#' The only Monte Carlo left is over the `N` correlated paths (and the `F`
#' field sample); both are cheap to scale on the tensor. CRN is automatic
#' (every candidate is scored on the same paths and same field), so
#' marginals cancel common path noise.
#'
#' It is **independent of the v2 fix's compression**: it uses the full
#' per-path field score matrices, never the compact quantile grid, so it
#' remains a valid truth for validating any compact fix later.
#'
#' The roster being evaluated is treated as a *test particle*: one entry
#' among `total_field_size` does not move the field bracket, so the field
#' survival weights are roster-independent and built once
#' ([oracle_build_field()]), then every candidate roster is scored against
#' them cheaply ([oracle_roster_ev()]).

# ---- structural readers -----------------------------------------------------

#' Pull the structural constants the oracle needs from a tournament config.
#'
#' @param tournament_cfg Parsed config from [load_tournament()].
#' @return List: `pod` / `advn` / `seats` (per stage), `n_stage`,
#'   `finalist_tiers` (list of `{from,to,usd}` over the final pod),
#'   `qf_usd` / `sf_usd` (flat first-/second-elimination loser payouts).
#' @export
oracle_cfg_struct <- function(tournament_cfg) {
  st <- .stage_structure(tournament_cfg)
  if (st$n_stage != 4L) {
    cli::cli_abort(c(
      "oracle supports the 4-stage Season shape only.",
      i = "This tournament has {st$n_stage} stages."))
  }
  tiers <- tournament_cfg$payouts$championship_round$tiers
  if (is.null(tiers)) cli::cli_abort("Config has no championship_round payout table.")
  fin <- list(); qf_usd <- 0; sf_usd <- 0
  for (t in tiers) {
    elig <- t$eligibility %||% NA_character_
    if (identical(elig, "finalist")) {
      fin[[length(fin) + 1L]] <- list(from = as.integer(t$rank_from),
                                      to = as.integer(t$rank_to),
                                      usd = as.numeric(t$usd))
    } else if (identical(elig, "semifinals_loser")) {
      sf_usd <- as.numeric(t$usd)
    } else if (identical(elig, "quarterfinals_loser")) {
      qf_usd <- as.numeric(t$usd)
    }
  }
  list(pod = st$pod, advn = st$advn, seats = st$seats, n_stage = st$n_stage,
       finalist_tiers = fin, qf_usd = qf_usd, sf_usd = sf_usd)
}

#' Generic pod-advancement probability: P(advance | percentile `p`).
#'
#' `p = P(opponent score <= my score)` against the round's opponent pool.
#' For a pod of `pod_size` with `advance_n` advancing, I advance iff at
#' most `advance_n - 1` of my `pod_size - 1` opponents beat me:
#' `sum_{j=0}^{advance_n-1} C(n_opp, j) (1-p)^j p^(n_opp-j)`.
#' Vectorized over `p`. (Mirrors the binomial in [.advance_prob_curve()].)
#' @keywords internal
.oracle_adv_prob <- function(p, pod_size, advance_n) {
  n_opp <- pod_size - 1L
  s <- numeric(length(p))
  for (j in 0:(advance_n - 1L)) {
    s <- s + choose(n_opp, j) * (1 - p)^j * p^(n_opp - j)
  }
  s
}

# ---- field survival weights (roster-independent, built once) -----------------

#' Per-path round scores for a sample of field rosters, on the tensor paths
#'
#' Scores every eligible field roster (>= `min_covered` covered players)
#' through [roster_round_scores()] on the SAME `ev_draws` paths the
#' evaluated rosters use -- the entire co-movement mechanism.
#'
#' @param field_rosters Named list `entry_id -> chr underdog_id`.
#' @param ev_draws A `bbbro_ev_draws` (path-aligned tensor).
#' @param stage_weeks Named list `r1`..`r4` (from a curves object).
#' @param lineup_spec Slate lineup spec.
#' @param min_covered Minimum tensor-covered players to include a roster.
#' @param max_field Optional cap on the number of field rosters scored.
#' @param seed Optional seed for the (capped) field subsample.
#' @return List with `fR` (list `R1`..`R4`, each `[N_path x F]`) and the
#'   `entry_ids` kept.
#' @export
oracle_score_field <- function(field_rosters, ev_draws, stage_weeks, lineup_spec,
                               min_covered = 15L, max_field = NULL, seed = NULL) {
  covered <- ev_draws$player_ids
  cov_n <- vapply(field_rosters, function(r) sum(r %in% covered), integer(1))
  elig <- names(field_rosters)[cov_n >= min_covered]
  if (!is.null(max_field) && length(elig) > max_field) {
    if (!is.null(seed)) set.seed(seed)
    elig <- sample(elig, max_field)
  }
  N <- ev_draws$n_paths; F <- length(elig)
  fR <- list(R1 = matrix(0, N, F), R2 = matrix(0, N, F),
             R3 = matrix(0, N, F), R4 = matrix(0, N, F))
  for (i in seq_along(elig)) {
    rc <- intersect(field_rosters[[elig[i]]], covered)
    rs <- roster_round_scores(rc, ev_draws, stage_weeks, lineup_spec)
    fR$R1[, i] <- rs$R1; fR$R2[, i] <- rs$R2
    fR$R3[, i] <- rs$R3; fR$R4[, i] <- rs$R4
  }
  list(fR = fR, entry_ids = elig)
}

#' Build the roster-independent field survival weights (the oracle's field)
#'
#' Carries survivor selection round-to-round as marginal advance
#' probabilities. After round k the field's *entering weight* for round
#' k+1 is `w_{k+1}[t,i] = w_k[t,i] * advance_prob(percentile of i among the
#' w_k-weighted field on path t)`. `w_1 = 1` (everyone enters). The
#' weighted field IS the Rao-Blackwellized survivor pool: pod-assignment
#' randomness integrated out, zero bracket Monte-Carlo variance.
#'
#' @param scored Output of [oracle_score_field()] (or a `list(fR=...)`).
#' @param cfg_struct Output of [oracle_cfg_struct()].
#' @return A `bbbro_ev_oracle_field`: `fR`, `W` (entering weights `[N x F]`
#'   for rounds 1..4; `W[[4]]` = finalist weights), `N`, `F`, `cfg_struct`.
#' @export
oracle_build_field <- function(scored, cfg_struct) {
  fR <- scored$fR
  N <- nrow(fR$R1); F <- ncol(fR$R1)
  pod <- cfg_struct$pod; advn <- cfg_struct$advn
  W <- vector("list", 4L)
  w <- matrix(1, N, F)                       # round-1 entering weight: all enter
  for (k in 1:3) {
    W[[k]] <- w
    Rk <- fR[[k]]
    p <- matrix(0, N, F)                     # weighted percentile of each member
    for (t in seq_len(N)) {
      o <- order(Rk[t, ])
      w_o <- w[t, o]
      cw <- cumsum(w_o)
      tot <- cw[F]
      # Leave-one-out: P(OTHER entrant <= me), weighted -- a roster never
      # competes against itself. Without this, self-weight inflates each
      # percentile by w_self/tot, which compounds round-to-round and breaks
      # seat-conservation at the deep rounds (where tot is small).
      loo_num <- cw - w_o
      loo_den <- tot - w_o
      pt <- numeric(F)
      pt[o] <- ifelse(loo_den > 0, loo_num / loo_den, 0)
      p[t, ] <- pt
    }
    a <- .oracle_adv_prob(p, pod[k], advn[k])
    w <- w * a                               # survival so far = next entering weight
  }
  W[[4]] <- w                                # finalist (entering-final) weights
  structure(list(fR = fR, W = W, N = N, F = F, cfg_struct = cfg_struct),
            class = "bbbro_ev_oracle_field")
}

# ---- single-roster EV against the oracle field -------------------------------

#' Weighted-ECDF percentile of `x` in a pooled, weighted value set.
#' `p = (sum of weights with value <= x) / (total weight)`. Used by the
#' static (pooled-field) mode. @keywords internal
.oracle_pooled_pct <- function(x, vals, wts) {
  o <- order(vals); sv <- vals[o]; scw <- cumsum(wts[o]); tot <- scw[length(scw)]
  idx <- findInterval(x, sv)                 # # of sorted vals <= x
  out <- numeric(length(x))
  pos <- idx > 0L
  out[pos] <- scw[idx[pos]] / tot
  out
}

#' Oracle EV for one roster against the co-moving field
#'
#' @param rs Per-path round scores (`data.frame` R1..R4) from
#'   [roster_round_scores()] -- on the SAME `ev_draws` as the field.
#' @param ofield A `bbbro_ev_oracle_field` from [oracle_build_field()].
#' @param mode `"comove"` (per-path percentiles vs the survivor-weighted
#'   field -- the oracle truth) or `"static"` (percentiles vs the pooled
#'   survivor-weighted field -- the v1-equivalent, for the co-movement
#'   contrast).
#' @param final_mode How to integrate the final ladder over my rank among
#'   the realized co-moving finalist field. `"binom"`: rank ~
#'   Binomial(seats_final - 1, 1 - p4) -- captures the real spread of which
#'   finalists materialize (convex, but sensitive to p4 estimation noise
#'   when the field sample is small). `"rank"`: deterministic expected rank
#'   `1 + round((seats_final - 1)(1 - p4))` -- robust to a sparse field at
#'   the cost of the top-tier convexity spread.
#' @return List: `ev` (mean $), `per_path` ($ per path, for CRN marginals
#'   and split-half SE), `reach` (mean P(reach QF/SF/final)),
#'   `parts` (mean a1/a2/a3 and mean E_final).
#' @export
oracle_roster_ev <- function(rs, ofield, mode = c("comove", "static"),
                             final_mode = c("binom", "rank")) {
  mode <- match.arg(mode)
  final_mode <- match.arg(final_mode)
  cs <- ofield$cfg_struct
  fR <- ofield$fR; W <- ofield$W; N <- ofield$N
  pod <- cs$pod; advn <- cs$advn
  r <- list(rs$R1, rs$R2, rs$R3, rs$R4)

  p_me <- vector("list", 4L)
  if (mode == "comove") {
    for (k in 1:4) {
      num <- rowSums(W[[k]] * (fR[[k]] <= r[[k]]))
      den <- rowSums(W[[k]])
      p_me[[k]] <- ifelse(den > 0, num / den, 0)
    }
  } else {
    for (k in 1:4) {
      p_me[[k]] <- .oracle_pooled_pct(r[[k]], as.numeric(fR[[k]]), as.numeric(W[[k]]))
    }
  }

  a1 <- .oracle_adv_prob(p_me[[1]], pod[1], advn[1])
  a2 <- .oracle_adv_prob(p_me[[2]], pod[2], advn[2])
  a3 <- .oracle_adv_prob(p_me[[3]], pod[3], advn[3])

  # Analytic final ladder: my rank among the realized co-moving finalist
  # field is Binomial -- # of the (seats_final - 1) other finalists that
  # beat me, each with prob q = 1 - p4. Integrate the step ladder over it.
  q <- 1 - p_me[[4]]
  n_other <- cs$seats[4] - 1L
  if (final_mode == "binom") {
    Efin <- numeric(N)
    for (tier in cs$finalist_tiers) {
      pr <- stats::pbinom(tier$to - 1L, n_other, q) -
            stats::pbinom(tier$from - 2L, n_other, q)
      Efin <- Efin + tier$usd * pr
    }
  } else {
    rank_me <- 1L + round(n_other * q)       # deterministic expected rank
    # step-ladder lookup: $ for each finalist rank
    Efin <- numeric(N)
    for (tier in cs$finalist_tiers) {
      hit <- rank_me >= tier$from & rank_me <= tier$to
      Efin[hit] <- tier$usd
    }
  }

  ev_path <- a1 * ((1 - a2) * cs$qf_usd + a2 * ((1 - a3) * cs$sf_usd + a3 * Efin))
  list(ev = mean(ev_path), per_path = ev_path,
       reach = c(qf = mean(a1), sf = mean(a1 * a2), final = mean(a1 * a2 * a3)),
       parts = c(a1 = mean(a1), a2 = mean(a2), a3 = mean(a3), Efin = mean(Efin)))
}

#' Marginal EV of adding each candidate to a roster, under the oracle (CRN)
#'
#' `marginal(X) = oracle_EV(roster + X) - oracle_EV(roster)`, both on the
#' same paths/field so per-path noise cancels. Mirrors [rank_marginal_ev()]
#' but with the oracle as the EV.
#'
#' @param base_rs Round scores of the base roster.
#' @param cand_rs_list Named list of candidate round-score data.frames.
#' @param oield A `bbbro_ev_oracle_field`.
#' @param mode `"comove"` or `"static"`.
#' @return data.frame `id`, `marginal_ev`, sorted descending.
#' @export
oracle_rank_marginal <- function(base_rs, cand_rs_list, oield,
                                 mode = c("comove", "static")) {
  mode <- match.arg(mode)
  base <- oracle_roster_ev(base_rs, oield, mode = mode)$ev
  m <- vapply(cand_rs_list, function(rs)
    oracle_roster_ev(rs, oield, mode = mode)$ev - base, numeric(1))
  out <- data.frame(id = names(cand_rs_list), marginal_ev = unname(m),
                    stringsAsFactors = FALSE)
  out[order(-out$marginal_ev), , drop = FALSE]
}
