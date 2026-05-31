#' Simulate per-stage scores for a population of rosters
#'
#' Runs a single joint correlated draw over the union of all `rosters`
#' players (so cross-roster NFL-game/team correlation is preserved
#' within each sim) and computes each roster's weekly totals via 3b-3's
#' optimizer, then sums weeks 1-14 cumulatively for the qualifier
#' total and keeps weeks 15 / 16 / 17 separately for the QF / SF /
#' Final stages.
#'
#' Used by [build_bbm7_field_payouts()] to score the synthetic field
#' for the money-conservation check and to build the conditional
#' advancer-stage pools the team-EV machinery samples opponents from.
#' Also usable on a small specific pod if a caller wants per-team
#' stage breakdowns without going through the full BBM7 EV plumbing.
#'
#' @param rosters Named list of `entry_id -> character vector of
#'   underdog_id`. Each entry's roster.
#' @param positions Named character vector `underdog_id -> position`.
#' @param layerA_draws Long Layer A draws data.frame.
#' @param schedule Per-player-per-week schedule data.frame.
#' @param lineup_spec Slate lineup spec (from [load_slate_lineup_spec()]).
#' @param corr_params Correlation parameters; default [default_corr_params].
#' @param n_sims Output sim count (default 10000).
#' @param seed Optional integer seed.
#' @param precomputed_marginals Optional shared marginal cache
#'   from [precompute_layerA_marginals()].
#' @param contrib_entry_id Optional entry_id. When supplied, per-player
#'   contributions to that team's optimal lineups are computed from the
#'   same joint draw and returned in `contrib` (used by head-5
#'   attribution). Cheap -- only the one team is decomposed.
#' @return Named list with four numeric matrices keyed by entry_id (as
#'   rownames) and columns = sims:
#'   `q_cum` (weeks 1-14 cumulative), `w15`, `w16`, `w17`. When
#'   `contrib_entry_id` is set, also `contrib` -- a list of per-player
#'   `[n_team_players x n_sims]` matrices `q` (weeks 1-14 summed), `w15`,
#'   `w16`, `w17` (rownames = the team's underdog_ids); otherwise `NULL`.
#' @export
simulate_per_stage_scores <- function(rosters,
                                      positions,
                                      layerA_draws,
                                      schedule,
                                      lineup_spec,
                                      corr_params = default_corr_params,
                                      n_sims = 10000L,
                                      seed = NULL,
                                      precomputed_marginals = NULL,
                                      contrib_entry_id = NULL) {
  if (!is.list(rosters) || length(rosters) == 0L) {
    cli::cli_abort("`rosters` must be a non-empty list of entry_id -> roster.")
  }
  union_ids <- unique(unlist(rosters, use.names = FALSE))

  ml <- sample_correlated_draws(
    player_ids            = union_ids,
    layerA_draws          = layerA_draws,
    schedule              = schedule,
    corr_params           = corr_params,
    n_sims                = n_sims,
    seed                  = seed,
    output_format         = "matrix_list",
    precomputed_marginals = precomputed_marginals
  )

  weeks_qual <- as.character(1:14)
  weeks_qual <- intersect(weeks_qual, names(ml))
  stages_kept <- intersect(c("15", "16", "17"), names(ml))

  n_teams <- length(rosters)
  q_cum <- matrix(0, nrow = n_teams, ncol = n_sims,
                  dimnames = list(names(rosters), NULL))
  w15 <- matrix(0, nrow = n_teams, ncol = n_sims,
                dimnames = list(names(rosters), NULL))
  w16 <- matrix(0, nrow = n_teams, ncol = n_sims,
                dimnames = list(names(rosters), NULL))
  w17 <- matrix(0, nrow = n_teams, ncol = n_sims,
                dimnames = list(names(rosters), NULL))

  contrib_target <- if (!is.null(contrib_entry_id)) {
    idx <- match(contrib_entry_id, names(rosters))
    if (is.na(idx)) {
      cli::cli_abort("`contrib_entry_id` {.val {contrib_entry_id}} not found in `rosters`.")
    }
    idx
  } else NA_integer_
  contrib_out <- NULL

  for (i in seq_len(n_teams)) {
    team_pids <- intersect(rosters[[i]], union_ids)
    team_pos  <- positions[team_pids]
    team_pos  <- team_pos[!is.na(team_pos)]
    team_pids <- intersect(team_pids, names(team_pos))
    if (length(team_pids) == 0L) next
    team_ml <- lapply(ml, function(M) {
      keep <- intersect(rownames(M), team_pids)
      M[keep, , drop = FALSE]
    })
    wt <- optimize_lineup_totals(
      scores      = team_ml,
      positions   = team_pos,
      lineup_spec = lineup_spec
    )
    # `wt` is [n_sims x n_weeks_present] with column names = week numbers.
    week_cols <- colnames(wt)
    qual_cols <- intersect(weeks_qual, week_cols)
    if (length(qual_cols) > 0L) {
      q_cum[i, ] <- rowSums(wt[, qual_cols, drop = FALSE])
    }
    if ("15" %in% week_cols) w15[i, ] <- wt[, "15"]
    if ("16" %in% week_cols) w16[i, ] <- wt[, "16"]
    if ("17" %in% week_cols) w17[i, ] <- wt[, "17"]

    if (!is.na(contrib_target) && i == contrib_target) {
      cw <- optimize_lineup_contributions(
        scores      = team_ml,
        positions   = team_pos,
        lineup_spec = lineup_spec
      )
      mk <- function() matrix(0, length(team_pids), n_sims,
                              dimnames = list(team_pids, NULL))
      q_c <- mk(); c15 <- mk(); c16 <- mk(); c17 <- mk()
      for (wk in names(cw)) {
        Cm <- cw[[wk]]
        rws <- rownames(Cm)
        wnum <- as.integer(wk)
        if (wnum >= 1L && wnum <= 14L) q_c[rws, ] <- q_c[rws, ] + Cm
        else if (wnum == 15L) c15[rws, ] <- c15[rws, ] + Cm
        else if (wnum == 16L) c16[rws, ] <- c16[rws, ] + Cm
        else if (wnum == 17L) c17[rws, ] <- c17[rws, ] + Cm
      }
      contrib_out <- list(q = q_c, w15 = c15, w16 = c16, w17 = c17)
    }
  }

  list(q_cum = q_cum, w15 = w15, w16 = w16, w17 = w17,
       contrib = contrib_out)
}

#' Compute BBM7 payouts for every (team, sim) cell of a field-sample
#' run and surface the advancer-conditional pools the team-EV
#' machinery uses
#'
#' For each `sim_idx`, randomly groups the field into qualifier pods of
#' `qual_pod_size` (default 12), advances the top `qual_advance_n`
#' (default 2) by `q_cum`, then forward-simulates QF (pods of
#' `qf_pod_size`, top 1), SF (pods of `sf_pod_size`, top 1), Final
#' (single pod, rank all by `w17`). Each team's payout is the sum of
#' (qualifier-round-tier-by-rank-in-field) + (championship-round-tier-
#' by-progression-and-within-stage-rank).
#'
#' Tier ranks are scaled from the field-sample size to the
#' `tournament_cfg`'s `total_field_size` (672,336 for BBM7) so that
#' top-10K qualifier-round payouts and the 667/667/667/6003 championship
#' brackets land in the right proportion. Field-mean payout converges
#' to `prize_pool / total_field_size` by construction -- this is the
#' money-conservation check.
#'
#' @param scores Output of [simulate_per_stage_scores()].
#' @param tournament_cfg Parsed BBM7 config from [load_tournament()].
#' @param seed Optional integer seed for pod assignment.
#' @return A list with:
#'   \itemize{
#'     \item `per_team_ev` -- data.frame, one row per field entry,
#'       columns `entry_id, qualifier_round_ev, championship_round_ev,
#'       total_ev`.
#'     \item `field_mean_total_ev` -- numeric scalar.
#'     \item `pools` -- list of empirical distributions for the team-EV
#'       evaluator (`q_cum_pool`, `qualifier_advancer_w15`,
#'       `qf_advancer_w16`, `sf_advancer_w17`) and the per-team rank
#'       distribution for percentile-based qualifier-round lookups.
#'   }
#' @export
build_bbm7_field_payouts <- function(scores,
                                     tournament_cfg,
                                     seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  q_cum <- scores$q_cum
  w15   <- scores$w15
  w16   <- scores$w16
  w17   <- scores$w17
  n_teams <- nrow(q_cum)
  n_sims  <- ncol(q_cum)
  entry_ids <- rownames(q_cum)
  if (is.null(entry_ids)) entry_ids <- as.character(seq_len(n_teams))

  # Money conservation only holds when the field is a clean multiple of the
  # per-finalist field share (see `bbm7_field_multiple()`). The scores are
  # already computed here so we can't snap -- error with a clear message and
  # point the caller at `resolve_bbm7_field_size()`, which snaps at
  # field-generation time.
  base <- bbm7_field_multiple(tournament_cfg)
  if (n_teams %% base != 0L) {
    lo <- max(base, (n_teams %/% base) * base)
    hi <- lo + base
    cli::cli_abort(c(
      "Field sample has {n_teams} teams, not a multiple of {base}.",
      x = "Money conservation requires a field that is a multiple of {base} \\
           ({.code total_field_size / final-stage seats}); {n_teams} biases \\
           the field-mean EV by several percent.",
      i = "Regenerate the field at {lo} or {hi} (see {.fn resolve_bbm7_field_size})."
    ))
  }

  stages <- tournament_cfg$stages
  qual_pod    <- as.integer(stages[[1L]]$pod_size %||% 12L)
  qual_adv_n  <- as.integer(stages[[1L]]$advancement$n %||% 2L)
  qf_pod      <- as.integer(stages[[2L]]$pod_size %||% 14L)
  sf_pod      <- as.integer(stages[[3L]]$pod_size %||% 12L)
  full_field  <- as.integer(tournament_cfg$total_field_size %||%
                              max(n_teams * 100L, n_teams + 1L))
  q_tiers     <- tournament_cfg$payouts$qualifier_round$tiers
  c_tiers     <- tournament_cfg$payouts$championship_round$tiers

  qualifier_pay_per <- matrix(0, n_teams, n_sims,
                              dimnames = list(entry_ids, NULL))
  champ_pay_per     <- matrix(0, n_teams, n_sims,
                              dimnames = list(entry_ids, NULL))

  qualifier_advancers_w15 <- numeric(0)
  qf_advancers_w16        <- numeric(0)
  sf_advancers_w17        <- numeric(0)

  n_finalist_full <- as.integer(stages[[4L]]$seats_entering %||% 667L)
  n_sf_entering   <- as.integer(stages[[3L]]$seats_entering %||% 8004L)
  n_qf_entering   <- as.integer(stages[[2L]]$seats_entering %||% 112056L)
  n_sf_lost_full  <- n_sf_entering - n_finalist_full
  n_qf_lost_full  <- n_qf_entering - n_sf_entering
  n_q_advancers_full <- n_qf_entering  # everyone who advanced qualifier

  # Pre-build the per-bucket tier-cap tables (ordered list of
  # `n_slots / usd` segments) so the range-averaging payout fn doesn't
  # rescan the YAML tiers each sim/team.
  qual_round_caps <- .tier_caps_from_yaml(q_tiers, eligibility = NULL,
                                          total_slots = full_field)
  finalist_caps   <- .tier_caps_from_yaml(c_tiers, eligibility = "finalist",
                                          total_slots = n_finalist_full)
  sf_loser_caps   <- .tier_caps_combined(
    c_tiers, primary_eligibility = "semifinals_loser",
    fallback_eligibility = "qualifier_advancer",
    primary_slots = 667L, total_slots = n_sf_lost_full)
  qf_loser_caps <- .tier_caps_combined(
    c_tiers, primary_eligibility = c("quarterfinals_loser",
                                     "quarterfinals_loser_lower"),
    fallback_eligibility = "qualifier_advancer",
    primary_slots = 667L + 6003L, total_slots = n_qf_lost_full)

  for (j in seq_len(n_sims)) {
    q_j   <- q_cum[, j]
    w15_j <- w15[, j]
    w16_j <- w16[, j]
    w17_j <- w17[, j]

    # ---- Qualifier-round payout: per-team representative range avg ---
    rk_q <- rank(-q_j, ties.method = "first")
    qualifier_pay_per[, j] <- vapply(rk_q, function(k)
      .range_avg_payout(k, n_teams, full_field, qual_round_caps),
      numeric(1))

    # ---- Qualifier pod advancement (random pods of 12) ----
    pod_assign <- sample.int(n_teams, n_teams)
    q_advanced_idx <- integer(0)
    for (s in seq.int(1L, n_teams - qual_pod + 1L, by = qual_pod)) {
      pod_idx <- pod_assign[s:(s + qual_pod - 1L)]
      top <- pod_idx[order(-q_j[pod_idx])][seq_len(qual_adv_n)]
      q_advanced_idx <- c(q_advanced_idx, top)
    }
    qualifier_advancers_w15 <- c(qualifier_advancers_w15,
                                  w15_j[q_advanced_idx])

    # ---- QF (pods of qf_pod, top 1) ----
    qf_advanced_idx <- integer(0)
    qf_shuf <- sample(q_advanced_idx)
    for (s in seq.int(1L, length(qf_shuf) - qf_pod + 1L, by = qf_pod)) {
      pod_idx <- qf_shuf[s:(s + qf_pod - 1L)]
      qf_advanced_idx <- c(qf_advanced_idx, pod_idx[which.max(w15_j[pod_idx])])
    }
    qf_advancers_w16 <- c(qf_advancers_w16, w16_j[qf_advanced_idx])

    # ---- SF (pods of sf_pod, top 1) ----
    sf_advanced_idx <- integer(0)
    sf_shuf <- sample(qf_advanced_idx)
    for (s in seq.int(1L, length(sf_shuf) - sf_pod + 1L, by = sf_pod)) {
      pod_idx <- sf_shuf[s:(s + sf_pod - 1L)]
      sf_advanced_idx <- c(sf_advanced_idx, pod_idx[which.max(w16_j[pod_idx])])
    }
    sf_advancers_w17 <- c(sf_advancers_w17, w17_j[sf_advanced_idx])

    # ---- Championship payouts ----
    # Each progression bucket gets its own range-averaged payout assignment.

    # Finalists, ranked by w17 within sim's sample finalists.
    if (length(sf_advanced_idx) > 0L) {
      f_order <- sf_advanced_idx[order(-w17_j[sf_advanced_idx])]
      for (k in seq_along(f_order)) {
        champ_pay_per[f_order[k], j] <-
          .range_avg_payout(k, length(f_order), n_finalist_full,
                            finalist_caps)
      }
    }

    # SF losers (qf_advanced \ sf_advanced) ranked by w16.
    sf_losers <- setdiff(qf_advanced_idx, sf_advanced_idx)
    if (length(sf_losers) > 0L) {
      sl_order <- sf_losers[order(-w16_j[sf_losers])]
      for (k in seq_along(sl_order)) {
        champ_pay_per[sl_order[k], j] <-
          .range_avg_payout(k, length(sl_order), n_sf_lost_full,
                            sf_loser_caps)
      }
    }

    # QF losers (q_advanced \ qf_advanced) ranked by w15.
    qf_losers <- setdiff(q_advanced_idx, qf_advanced_idx)
    if (length(qf_losers) > 0L) {
      ql_order <- qf_losers[order(-w15_j[qf_losers])]
      for (k in seq_along(ql_order)) {
        champ_pay_per[ql_order[k], j] <-
          .range_avg_payout(k, length(ql_order), n_qf_lost_full,
                            qf_loser_caps)
      }
    }
  }

  per_team_q_ev <- rowMeans(qualifier_pay_per)
  per_team_c_ev <- rowMeans(champ_pay_per)
  per_team_total_ev <- per_team_q_ev + per_team_c_ev
  per_team_ev <- data.frame(
    entry_id              = entry_ids,
    qualifier_round_ev    = per_team_q_ev,
    championship_round_ev = per_team_c_ev,
    total_ev              = per_team_total_ev,
    stringsAsFactors = FALSE
  )

  list(
    per_team_ev         = per_team_ev,
    field_mean_total_ev = mean(per_team_total_ev),
    pools = list(
      q_cum_pool              = as.numeric(q_cum),
      qualifier_advancer_w15  = qualifier_advancers_w15,
      qf_advancer_w16         = qf_advancers_w16,
      sf_advancer_w17         = sf_advancers_w17
    )
  )
}

#' Compute BBM7 expected value for one team
#'
#' Heads 1-4 of the prompt: per-team qualifier advance probability
#' (3b-5 pod machinery), qualifier-round expected payout (field-rank
#' percentile from `pools`), and championship-round expected payout
#' (forward-simulate QF/SF/Final against advancer-conditional opponent
#' pools sampled from `pools`).
#'
#' Head 5 adds per-player EV **attribution**: each sim's realized
#' winnings are split across the roster in proportion to the points each
#' player contributed to the optimal lineups in the weeks that determined
#' that payout -- qualifier-round $ across the weeks-1-14 lineup
#' contributions, championship $ across the progression-path weeks the
#' team actually played (week 15 for a QF loser; 15-16 for an SF loser;
#' 15-17 for a finalist). Summed over sims, the per-player EVs add up to
#' the team EV exactly. This is an attribution of *where realized value
#' flowed*, NOT a marginal / value-over-replacement number -- it is not
#' "EV lost if you drop this player".
#'
#' @param pod_rosters Named list (entry_id -> chr underdog_ids), the
#'   evaluated team's actual draft pod. Same shape 3b-5 consumes.
#' @param team_entry_id The entry_id within `pod_rosters` that is our
#'   team. Must match one of `names(pod_rosters)`.
#' @param positions Named character vector `underdog_id -> position`.
#' @param layerA_draws Long Layer A draws data.frame.
#' @param schedule Per-player-per-week schedule data.frame.
#' @param lineup_spec Slate lineup spec.
#' @param tournament_cfg BBM7 config from [load_tournament()].
#' @param field_cache Output of [build_bbm7_field_payouts()].
#' @param corr_params Correlation parameters; default
#'   [default_corr_params].
#' @param n_sims Output sim count (default 10000).
#' @param seed Optional integer seed.
#' @return A list with:
#'   \itemize{
#'     \item `team_ev` -- list with `qualifier_round_ev`,
#'       `championship_round_ev`, `total_ev`.
#'     \item `advance_probs` -- list with `qualifier`, `qf`, `sf`,
#'       `final` (= `sf`); per-sim mean indicators.
#'     \item `per_sim` -- data.frame with one row per sim:
#'       `q_total, w15, w16, w17, qualifier_round_pay, champ_pay,
#'       progression`.
#'     \item `per_player_ev` -- data.frame, one row per evaluated-team
#'       player: `underdog_id, qualifier_round_ev, championship_round_ev,
#'       total_ev`. Additive attribution (sums to `team_ev`); carries a
#'       `metric` attribute flagging it is not a marginal metric.
#'   }
#' @export
compute_team_bbm7_ev <- function(pod_rosters,
                                 team_entry_id,
                                 positions,
                                 layerA_draws,
                                 schedule,
                                 lineup_spec,
                                 tournament_cfg,
                                 field_cache,
                                 corr_params = default_corr_params,
                                 n_sims = 10000L,
                                 seed = NULL) {
  if (!(team_entry_id %in% names(pod_rosters))) {
    cli::cli_abort("`team_entry_id` {.val {team_entry_id}} not found in `pod_rosters`.")
  }
  stages <- tournament_cfg$stages
  qual_adv_n <- as.integer(stages[[1L]]$advancement$n %||% 2L)
  qf_pod     <- as.integer(stages[[2L]]$pod_size %||% 14L)
  sf_pod     <- as.integer(stages[[3L]]$pod_size %||% 12L)
  n_finalist <- as.integer(stages[[4L]]$seats_entering %||% 667L)
  n_sf_lost  <- as.integer(stages[[3L]]$seats_entering %||% 8004L) - n_finalist
  full_field <- as.integer(tournament_cfg$total_field_size %||% 672336L)

  pod_scores <- simulate_per_stage_scores(
    rosters         = pod_rosters,
    positions       = positions,
    layerA_draws    = layerA_draws,
    schedule        = schedule,
    lineup_spec     = lineup_spec,
    corr_params     = corr_params,
    n_sims          = n_sims,
    seed            = seed,
    contrib_entry_id = team_entry_id
  )

  team_q   <- pod_scores$q_cum[team_entry_id, ]
  team_w15 <- pod_scores$w15[team_entry_id, ]
  team_w16 <- pod_scores$w16[team_entry_id, ]
  team_w17 <- pod_scores$w17[team_entry_id, ]

  # Qualifier-round payout per sim: rank team's q_cum in the full field
  # via empirical CDF of `q_cum_pool`.
  pool_q <- field_cache$pools$q_cum_pool
  q_pct  <- vapply(team_q, function(x) mean(pool_q >= x), numeric(1))
  # rank ~ ceil(q_pct * full_field)
  rk_full <- pmax(1L, as.integer(ceiling(q_pct * full_field)))
  q_pay <- .payout_lookup(rk_full,
                           tournament_cfg$payouts$qualifier_round$tiers,
                           eligibility = NULL)

  # Qualifier advance: top-`qual_adv_n` of the pod by q_cum.
  pod_q <- pod_scores$q_cum  # rows = pod entries
  q_ranks_in_pod <- apply(pod_q, 2L,
                          function(x) rank(-x, ties.method = "first"))
  advance_q <- q_ranks_in_pod[team_entry_id, ] <= qual_adv_n

  # Championship bracket per sim.
  c_pay <- numeric(n_sims)
  progression <- rep("qualifier_loser", n_sims)

  pool_w15 <- field_cache$pools$qualifier_advancer_w15
  pool_w16 <- field_cache$pools$qf_advancer_w16
  pool_w17 <- field_cache$pools$sf_advancer_w17

  if (length(pool_w15) < 13L || length(pool_w16) < 11L ||
      length(pool_w17) < 1L) {
    cli::cli_warn(
      "Field-cache pools are sparse; advancer-conditional opponent sampling may be biased. \\
       Consider raising n_field_sims in build_bbm7_field_payouts()."
    )
  }

  for (j in seq_len(n_sims)) {
    if (!advance_q[j]) {
      progression[j] <- "qualifier_loser"
      next
    }
    progression[j] <- "qualifier_advancer"
    opp_w15 <- sample(pool_w15, qf_pod - 1L, replace = TRUE)
    if (team_w15[j] <= max(opp_w15)) {
      # QF loser; rank among QF losers by w15.
      qf_loser_rank_in_field <- max(1L, as.integer(
        ceiling(mean(pool_w15 >= team_w15[j]) * (full_field -
                  as.integer(stages[[2L]]$advancement$n %||% 1L) *
                  as.integer(stages[[2L]]$seats_entering %||% 1L) /
                  qf_pod))
      ))
      rk <- n_finalist + n_sf_lost + qf_loser_rank_in_field
      c_pay[j] <- .payout_lookup(rk,
                                  tournament_cfg$payouts$championship_round$tiers,
                                  eligibility = c("quarterfinals_loser",
                                                  "quarterfinals_loser_lower",
                                                  "qualifier_advancer"))
      next
    }
    progression[j] <- "qf_advancer"
    opp_w16 <- sample(pool_w16, sf_pod - 1L, replace = TRUE)
    if (team_w16[j] <= max(opp_w16)) {
      sf_loser_rank <- max(1L, as.integer(
        ceiling(mean(pool_w16 >= team_w16[j]) * n_sf_lost)
      ))
      rk <- n_finalist + sf_loser_rank
      c_pay[j] <- .payout_lookup(rk,
                                  tournament_cfg$payouts$championship_round$tiers,
                                  eligibility = c("semifinals_loser",
                                                  "qualifier_advancer"))
      next
    }
    progression[j] <- "sf_advancer"
    # Finalist: rank among finalists by w17.
    finalist_rank <- max(1L, as.integer(
      ceiling(mean(pool_w17 >= team_w17[j]) * n_finalist)
    ))
    c_pay[j] <- .payout_lookup(finalist_rank,
                                tournament_cfg$payouts$championship_round$tiers,
                                eligibility = "finalist")
  }

  per_player_ev <- .attribute_player_ev(
    contrib     = pod_scores$contrib,
    q_pay       = q_pay,
    c_pay       = c_pay,
    progression = progression
  )

  list(
    team_ev = list(
      qualifier_round_ev    = mean(q_pay),
      championship_round_ev = mean(c_pay),
      total_ev              = mean(q_pay) + mean(c_pay)
    ),
    per_player_ev = per_player_ev,
    advance_probs = list(
      qualifier = mean(advance_q),
      qf  = mean(progression == "qf_advancer" | progression == "sf_advancer"),
      sf  = mean(progression == "sf_advancer"),
      final = mean(progression == "sf_advancer")
    ),
    per_sim = data.frame(
      sim_idx             = seq_len(n_sims),
      q_total             = team_q,
      w15                 = team_w15,
      w16                 = team_w16,
      w17                 = team_w17,
      qualifier_round_pay = q_pay,
      champ_pay           = c_pay,
      progression         = progression,
      stringsAsFactors = FALSE
    )
  )
}

# ---- per-player attribution (head 5) ----------------------------------------

#' Additively attribute a team's realized per-sim winnings across its
#' roster, given per-player lineup contributions.
#'
#' For each sim the qualifier-round payout is split across the players'
#' weeks-1-14 lineup contributions and the championship payout across the
#' progression-path weeks the team actually played (week 15 for a QF
#' loser; 15-16 for an SF loser; 15-17 for a finalist; nothing for a
#' qualifier loser). Splitting each payout proportionally to a player's
#' share of the contributing points makes the per-player EVs sum to the
#' team EV exactly (the additivity gate) -- the per-sim column sums of the
#' attribution recover `q_pay` / `c_pay` whenever the team scored, and a
#' team that scored nothing earns nothing to attribute.
#' @keywords internal
.attribute_player_ev <- function(contrib, q_pay, c_pay, progression) {
  if (is.null(contrib)) {
    cli::cli_abort(
      "Per-player contributions missing; call simulate_per_stage_scores(contrib_entry_id=).")
  }
  players <- rownames(contrib$q)
  n_sims  <- length(q_pay)

  # Qualifier round: split across weeks-1-14 contributions.
  q_c     <- contrib$q
  denom_q <- colSums(q_c)
  fac_q   <- ifelse(denom_q > 0, q_pay / denom_q, 0)
  q_attr  <- sweep(q_c, 2L, fac_q, `*`)

  # Championship round: split across the progression-path weeks.
  champ_c <- matrix(0, nrow = length(players), ncol = n_sims,
                    dimnames = list(players, NULL))
  is_qadv <- progression == "qualifier_advancer"   # QF loser: week 15
  is_qfa  <- progression == "qf_advancer"           # SF loser: weeks 15-16
  is_sfa  <- progression == "sf_advancer"           # finalist: weeks 15-17
  if (any(is_qadv)) champ_c[, is_qadv] <- contrib$w15[, is_qadv, drop = FALSE]
  if (any(is_qfa)) {
    champ_c[, is_qfa] <- contrib$w15[, is_qfa, drop = FALSE] +
      contrib$w16[, is_qfa, drop = FALSE]
  }
  if (any(is_sfa)) {
    champ_c[, is_sfa] <- contrib$w15[, is_sfa, drop = FALSE] +
      contrib$w16[, is_sfa, drop = FALSE] + contrib$w17[, is_sfa, drop = FALSE]
  }
  denom_c <- colSums(champ_c)
  fac_c   <- ifelse(denom_c > 0, c_pay / denom_c, 0)
  c_attr  <- sweep(champ_c, 2L, fac_c, `*`)

  player_q_ev <- rowMeans(q_attr)
  player_c_ev <- rowMeans(c_attr)
  out <- data.frame(
    underdog_id           = players,
    qualifier_round_ev    = player_q_ev,
    championship_round_ev = player_c_ev,
    total_ev              = player_q_ev + player_c_ev,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
  out <- out[order(-out$total_ev), , drop = FALSE]
  rownames(out) <- NULL
  attr(out, "metric") <- paste(
    "additive EV attribution (realized value flow);",
    "NOT marginal / value-over-replacement -- not 'EV lost if dropped'")
  out
}

# ---- field-size guardrail ---------------------------------------------------

#' Money-conservation-valid synthetic-field base for a tournament
#'
#' [build_bbm7_field_payouts()] reproduces the prize pool only when the
#' synthetic field size is an exact multiple of `total_field_size`
#' divided by the final-stage seat count -- the number of full-field
#' entries each finalist "represents". For BBM7 that is
#' 672,336 / 667 = 1,008. A field that is *not* a multiple of it (e.g.
#' 1,200) splits unevenly through the qualifier -> QF -> SF -> Final pod
#' bracket and biases the field-mean EV by several percent. The base is
#' derived from config, not hardcoded, so Puppy / Dachshund / etc. each
#' get their own denominator.
#'
#' @param tournament_cfg Parsed tournament config from [load_tournament()].
#' @return Integer base; valid field sizes are positive multiples of it.
#' @export
bbm7_field_multiple <- function(tournament_cfg) {
  full_field <- as.integer(tournament_cfg$total_field_size %||% NA_integer_)
  stages <- tournament_cfg$stages
  final_seats <- if (length(stages) > 0L) {
    as.integer(stages[[length(stages)]]$seats_entering %||% NA_integer_)
  } else NA_integer_
  if (is.na(full_field) || is.na(final_seats) || final_seats <= 0L) {
    cli::cli_abort(
      "Cannot derive field multiple: config missing `total_field_size` or final-stage `seats_entering`.")
  }
  if (full_field %% final_seats != 0L) {
    cli::cli_abort(c(
      "`total_field_size` ({full_field}) is not an exact multiple of the \\
       final-stage seat count ({final_seats}).",
      i = "The money-conservation guarantee needs a clean per-finalist field share."
    ))
  }
  as.integer(full_field / final_seats)
}

#' Snap a requested synthetic-field size to a money-conservation-valid one
#'
#' Use at field-generation time (before [generate_field()] /
#' [build_bbm7_field_payouts()]) so the field is a clean multiple of
#' [bbm7_field_multiple()].
#'
#' @param tournament_cfg Parsed tournament config from [load_tournament()].
#' @param n_field Requested field size.
#' @param snap If `TRUE` (default) returns the nearest positive multiple of
#'   the base; if `FALSE`, errors when `n_field` is not already a valid
#'   multiple, naming the nearby valid sizes.
#' @return Integer valid field size (a positive multiple of the base).
#' @export
resolve_bbm7_field_size <- function(tournament_cfg, n_field, snap = TRUE) {
  base <- bbm7_field_multiple(tournament_cfg)
  n_field <- as.integer(n_field)
  if (!is.na(n_field) && n_field > 0L && n_field %% base == 0L) {
    return(n_field)
  }
  if (!isTRUE(snap)) {
    lo <- max(base, (as.integer(n_field) %/% base) * base)
    hi <- lo + base
    cli::cli_abort(c(
      "n_field = {n_field} is not a multiple of {base} (the money-conservation base).",
      i = "Nearest valid sizes: {lo} or {hi}. Pass a multiple of {base} \\
           (e.g. {base}, {2L * base}, {10L * base})."
    ))
  }
  mult <- max(1L, as.integer(round(as.numeric(n_field) / base)))
  mult * base
}

# ---- helpers ----------------------------------------------------------------

#' Build a "tier-cap" table from a YAML tiers list filtered by
#' eligibility. Returns a list of `{n_slots, usd}` segments in rank
#' order; the catch-all final segment pads to `total_slots`.
#' @keywords internal
.tier_caps_from_yaml <- function(tiers, eligibility = NULL,
                                  total_slots = NULL) {
  rows <- list()
  for (t in tiers) {
    if (!is.null(eligibility)) {
      etag <- t$eligibility %||% NA_character_
      if (!isTRUE(etag %in% eligibility)) next
    }
    rows[[length(rows) + 1L]] <- list(
      rank_from = as.integer(t$rank_from),
      rank_to   = as.integer(t$rank_to),
      usd       = as.numeric(t$usd)
    )
  }
  rows <- rows[order(vapply(rows, `[[`, integer(1), "rank_from"))]
  caps <- list()
  cursor <- 1L
  for (r in rows) {
    if (r$rank_from > cursor) {
      caps[[length(caps) + 1L]] <- list(
        n_slots = r$rank_from - cursor, usd = 0)
    }
    caps[[length(caps) + 1L]] <- list(
      n_slots = r$rank_to - r$rank_from + 1L, usd = r$usd)
    cursor <- r$rank_to + 1L
  }
  if (!is.null(total_slots) && cursor <= total_slots) {
    caps[[length(caps) + 1L]] <- list(
      n_slots = total_slots - cursor + 1L, usd = 0)
  }
  caps
}

#' Build tier caps for the SF-loser / QF-loser buckets: primary
#' eligibility tier(s) get their $$ for the top `primary_slots`; the
#' rest of the bucket falls back to the `fallback_eligibility` tier's
#' $$ (its first segment).
#' @keywords internal
.tier_caps_combined <- function(tiers, primary_eligibility,
                                 fallback_eligibility,
                                 primary_slots, total_slots) {
  primary_caps <- .tier_caps_from_yaml(
    tiers, eligibility = primary_eligibility, total_slots = NULL)
  # Sum slots in the primary -- if it's less than primary_slots, top up.
  primary_n <- sum(vapply(primary_caps, `[[`, integer(1), "n_slots"))
  if (primary_n < primary_slots && length(primary_caps) > 0L) {
    # last tier's usd extends to primary_slots
    last_usd <- primary_caps[[length(primary_caps)]]$usd
    primary_caps[[length(primary_caps) + 1L]] <- list(
      n_slots = primary_slots - primary_n, usd = last_usd)
  }
  # Fallback: take the first segment matching fallback_eligibility and
  # pad to total_slots.
  fallback_usd <- 0
  for (t in tiers) {
    if (identical(t$eligibility %||% NA_character_, fallback_eligibility)) {
      fallback_usd <- as.numeric(t$usd)
      break
    }
  }
  remaining <- total_slots - primary_slots
  if (remaining > 0L) {
    primary_caps[[length(primary_caps) + 1L]] <- list(
      n_slots = remaining, usd = fallback_usd)
  }
  primary_caps
}

#' Expected payout for a sample team's representative range over a
#' bucket. Sample rank `k` of `n_sample` covers full-field bucket
#' ranks `((k-1)*n_full/n_sample, k*n_full/n_sample]`. Integrates the
#' tier caps over that range and divides by the range width.
#' @keywords internal
.range_avg_payout <- function(rank_in_sample, n_sample, n_full, caps) {
  if (n_sample <= 0L) return(0)
  # `(rank_in_sample - 1L) * n_full` overflows 32-bit integer arithmetic once
  # the product exceeds 2^31 (n_full = 672,336 for BBM7, so any rank above
  # ~3,194 trips it -- and the recommended stable n_field >= 10,080 always
  # does). Force the rank-scaling into double precision before multiplying.
  rk_low  <- floor((as.numeric(rank_in_sample) - 1) * as.numeric(n_full) /
                     n_sample) + 1
  rk_high <- floor(as.numeric(rank_in_sample) * as.numeric(n_full) / n_sample)
  if (rk_high < rk_low) rk_high <- rk_low
  total <- 0
  cursor <- 1L
  for (tc in caps) {
    tier_end <- cursor + tc$n_slots - 1L
    overlap_low  <- max(cursor, rk_low)
    overlap_high <- min(tier_end, rk_high)
    if (overlap_high >= overlap_low) {
      total <- total + (overlap_high - overlap_low + 1L) * tc$usd
    }
    cursor <- tier_end + 1L
    if (cursor > rk_high) break
  }
  total / (rk_high - rk_low + 1L)
}

#' Look up payout $ for a vector of full-field ranks against a tier
#' table, restricted (optionally) to a set of accepted `eligibility`
#' tags. Used by [compute_team_bbm7_ev()] for the EVALUATED team where
#' percentile lookups give a single rank rather than a representative
#' range.
#' @keywords internal
.payout_lookup <- function(ranks, tiers, eligibility = NULL) {
  out <- numeric(length(ranks))
  if (length(tiers) == 0L) return(out)
  starts <- vapply(tiers, function(t) as.integer(t$rank_from %||% NA_integer_),
                   integer(1))
  ends   <- vapply(tiers, function(t) as.integer(t$rank_to %||% NA_integer_),
                   integer(1))
  usds   <- vapply(tiers, function(t) as.numeric(t$usd %||% 0), numeric(1))
  elig   <- vapply(tiers, function(t) as.character(t$eligibility %||% NA_character_),
                   character(1))
  for (i in seq_along(ranks)) {
    r <- ranks[i]
    if (is.na(r)) next
    matches <- which(starts <= r & ends >= r)
    if (length(matches) == 0L) next
    if (!is.null(eligibility)) {
      keep <- which(is.na(elig[matches]) | elig[matches] %in% eligibility)
      matches <- matches[keep]
      if (length(matches) == 0L) next
    }
    out[i] <- usds[matches[1L]]
  }
  out
}
