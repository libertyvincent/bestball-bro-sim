# 3b-7 money-conservation gap localizer (throwaway).
#
# Part 1: re-implements build_bbm7_field_payouts's bracket assignment
# with per-(head, bucket) accounting so we can compare assigned-$ to
# configured-$ tier by tier.
# Part 2: runs the field-mean gross EV at several sample sizes to see
# whether the gap shrinks (sampling) or plateaus (structural).

`%||%` <- function(a, b) if (is.null(a)) b else a

# ---------------------------------------------------------------------------
# Setup -- shared across both parts.
# ---------------------------------------------------------------------------

picks <- load_scraped_drafts()
pool  <- load_slate_data("nfl_2026_season")
targets <- compute_field_targets(picks, slate_id = "nfl_2026_season")

sources_path <- file.path("inst", "data", "sources", "_manifest.yaml")
slates_path  <- file.path("inst", "data", "slates",  "_manifest.yaml")
feed <- blend_slate(slate_id = "nfl_2026_season",
                    sources_manifest_path = sources_path,
                    slates_manifest_path  = slates_path,
                    write_json = FALSE)

positions <- vapply(names(feed$players),
                    function(uid) feed$players[[uid]]$position %||% NA_character_,
                    character(1))
names(positions) <- names(feed$players)
positions <- positions[!is.na(positions)]
sched_chunks <- list()
for (uid in names(feed$players)) {
  pl <- feed$players[[uid]]
  for (wk in pl$weekly %||% list()) {
    w <- as.integer(wk$week %||% NA_integer_); if (is.na(w)) next
    sched_chunks[[length(sched_chunks)+1L]] <- data.frame(
      underdog_id = uid, week = w, team = pl$team %||% NA_character_,
      opponent = if (isTRUE(wk$is_bye)) NA_character_ else (wk$opponent %||% NA_character_),
      is_bye = isTRUE(wk$is_bye), stringsAsFactors = FALSE)
  }
}
schedule <- do.call(rbind, sched_chunks)
lineup_spec <- load_slate_lineup_spec("nfl_2026_season")
tcfg <- load_tournament("bbm7")

# Configured per-entry expected $ from each tier of BBM7.
.configured_per_entry <- function(tcfg) {
  full <- as.integer(tcfg$total_field_size)
  out <- list()
  # Qualifier round tiers.
  for (t in tcfg$payouts$qualifier_round$tiers) {
    n <- as.integer(t$rank_to - t$rank_from + 1L)
    label <- sprintf("Q[%d-%d]@$%d", t$rank_from, t$rank_to, t$usd)
    out[[label]] <- list(head = "qualifier",
                         configured_per_entry = n * t$usd / full,
                         configured_total = n * t$usd,
                         eligibility = NA)
  }
  # Championship tiers.
  for (t in tcfg$payouts$championship_round$tiers) {
    n <- as.integer(t$rank_to - t$rank_from + 1L)
    label <- sprintf("C[%d-%d]@$%d:%s", t$rank_from, t$rank_to, t$usd, t$eligibility)
    out[[label]] <- list(head = "championship",
                         configured_per_entry = n * t$usd / full,
                         configured_total = n * t$usd,
                         eligibility = t$eligibility)
  }
  out
}

# Re-run of the bracket assignment, instrumented to track per-(head, tier)
# assigned $$$. Mirrors build_bbm7_field_payouts but doesn't return per-team
# numbers -- only the per-tier accounting and grand totals.
.bbm7_payouts_with_breakdown <- function(scores, tcfg, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  q_cum <- scores$q_cum; w15 <- scores$w15; w16 <- scores$w16; w17 <- scores$w17
  n_teams <- nrow(q_cum); n_sims <- ncol(q_cum)
  stages <- tcfg$stages
  qual_pod <- as.integer(stages[[1L]]$pod_size)
  qual_adv_n <- as.integer(stages[[1L]]$advancement$n)
  qf_pod <- as.integer(stages[[2L]]$pod_size)
  sf_pod <- as.integer(stages[[3L]]$pod_size)
  full <- as.integer(tcfg$total_field_size)
  n_fin <- as.integer(stages[[4L]]$seats_entering)
  n_sf_lost <- as.integer(stages[[3L]]$seats_entering) - n_fin
  n_qf_entering <- as.integer(stages[[2L]]$seats_entering)
  n_qf_lost <- n_qf_entering - as.integer(stages[[3L]]$seats_entering)

  q_tiers <- tcfg$payouts$qualifier_round$tiers
  c_tiers <- tcfg$payouts$championship_round$tiers
  q_caps <- bestballBroSim:::.tier_caps_from_yaml(q_tiers, NULL, full)
  fin_caps <- bestballBroSim:::.tier_caps_from_yaml(
    c_tiers, eligibility = "finalist", total_slots = n_fin)
  sl_caps <- bestballBroSim:::.tier_caps_combined(
    c_tiers, primary_eligibility = "semifinals_loser",
    fallback_eligibility = "qualifier_advancer",
    primary_slots = 667L, total_slots = n_sf_lost)
  ql_caps <- bestballBroSim:::.tier_caps_combined(
    c_tiers, primary_eligibility = c("quarterfinals_loser",
                                     "quarterfinals_loser_lower"),
    fallback_eligibility = "qualifier_advancer",
    primary_slots = 667L + 6003L, total_slots = n_qf_lost)

  # Assigned-$ counters per tier label.
  tier_totals <- list()
  bump <- function(label, head, x) {
    if (is.null(tier_totals[[label]])) {
      tier_totals[[label]] <<- list(head = head, total = 0)
    }
    tier_totals[[label]]$total <<- tier_totals[[label]]$total + x
  }

  # For per-bucket accumulation: track contribution from caps row.
  # Caps row has n_slots/usd. Each sample team's contribution to row r:
  #   n_slots_in_overlap * usd / range_width
  # Aggregate by index in caps (= label).
  attribute <- function(rank_in_sample, n_sample, n_full, caps,
                        head, labels) {
    if (n_sample <= 0L || length(caps) == 0L) return(invisible())
    rk_low  <- floor((rank_in_sample - 1L) * n_full / n_sample) + 1L
    rk_high <- floor(rank_in_sample      * n_full / n_sample)
    if (rk_high < rk_low) rk_high <- rk_low
    width <- rk_high - rk_low + 1L
    cursor <- 1L
    for (i in seq_along(caps)) {
      tc <- caps[[i]]
      tier_end <- cursor + tc$n_slots - 1L
      lo <- max(cursor, rk_low); hi <- min(tier_end, rk_high)
      if (hi >= lo) {
        bump(labels[i], head, (hi - lo + 1L) * tc$usd / width)
      }
      cursor <- tier_end + 1L
      if (cursor > rk_high) break
    }
  }

  # Build labels for each caps row -- matched to YAML tier semantics where
  # possible; catch-all rows labeled "...|pad".
  q_labels <- character(length(q_caps))
  cursor <- 1L
  for (i in seq_along(q_caps)) {
    tc <- q_caps[[i]]
    rng <- sprintf("%d-%d", cursor, cursor + tc$n_slots - 1L)
    matched_tier <- NULL
    for (t in q_tiers) {
      if (t$rank_from == cursor && t$rank_to == cursor + tc$n_slots - 1L) {
        matched_tier <- t; break
      }
    }
    q_labels[i] <- if (!is.null(matched_tier)) {
      sprintf("Q[%s]@$%d", rng, matched_tier$usd)
    } else sprintf("Q[%s]@$%g(pad)", rng, tc$usd)
    cursor <- cursor + tc$n_slots
  }
  # Finalist caps labels keyed to YAML tier list.
  fin_labels <- character(length(fin_caps))
  cursor <- 1L
  for (i in seq_along(fin_caps)) {
    tc <- fin_caps[[i]]
    rng <- sprintf("%d-%d", cursor, cursor + tc$n_slots - 1L)
    matched_tier <- NULL
    for (t in c_tiers) {
      if (identical(t$eligibility %||% NA_character_, "finalist") &&
          t$rank_from == cursor && t$rank_to == cursor + tc$n_slots - 1L) {
        matched_tier <- t; break
      }
    }
    fin_labels[i] <- if (!is.null(matched_tier)) {
      sprintf("C-fin[%s]@$%d", rng, matched_tier$usd)
    } else sprintf("C-fin[%s]@$%g(pad)", rng, tc$usd)
    cursor <- cursor + tc$n_slots
  }
  # SL labels: top 667 are $1000, rest are $25 fallback.
  sl_labels <- character(length(sl_caps))
  cursor <- 1L
  for (i in seq_along(sl_caps)) {
    tc <- sl_caps[[i]]
    rng <- sprintf("%d-%d", cursor, cursor + tc$n_slots - 1L)
    sl_labels[i] <- sprintf("C-sl[%s]@$%g", rng, tc$usd)
    cursor <- cursor + tc$n_slots
  }
  # QL labels.
  ql_labels <- character(length(ql_caps))
  cursor <- 1L
  for (i in seq_along(ql_caps)) {
    tc <- ql_caps[[i]]
    rng <- sprintf("%d-%d", cursor, cursor + tc$n_slots - 1L)
    ql_labels[i] <- sprintf("C-ql[%s]@$%g", rng, tc$usd)
    cursor <- cursor + tc$n_slots
  }

  for (j in seq_len(n_sims)) {
    q_j <- q_cum[, j]; w15_j <- w15[, j]; w16_j <- w16[, j]; w17_j <- w17[, j]

    # Qualifier-round payouts.
    rk_q <- rank(-q_j, ties.method = "first")
    for (k in seq_len(n_teams)) {
      attribute(rk_q[k], n_teams, full, q_caps, "qualifier", q_labels)
    }

    # Pod assignments.
    pod_assign <- sample.int(n_teams, n_teams)
    q_adv <- integer(0)
    for (s in seq.int(1L, n_teams - qual_pod + 1L, by = qual_pod)) {
      idx <- pod_assign[s:(s + qual_pod - 1L)]
      top <- idx[order(-q_j[idx])][seq_len(qual_adv_n)]
      q_adv <- c(q_adv, top)
    }
    qf_adv <- integer(0); qf_shuf <- sample(q_adv)
    for (s in seq.int(1L, length(qf_shuf) - qf_pod + 1L, by = qf_pod)) {
      idx <- qf_shuf[s:(s + qf_pod - 1L)]
      qf_adv <- c(qf_adv, idx[which.max(w15_j[idx])])
    }
    sf_adv <- integer(0); sf_shuf <- sample(qf_adv)
    for (s in seq.int(1L, length(sf_shuf) - sf_pod + 1L, by = sf_pod)) {
      idx <- sf_shuf[s:(s + sf_pod - 1L)]
      sf_adv <- c(sf_adv, idx[which.max(w16_j[idx])])
    }

    if (length(sf_adv) > 0L) {
      f_order <- sf_adv[order(-w17_j[sf_adv])]
      for (k in seq_along(f_order))
        attribute(k, length(f_order), n_fin, fin_caps, "championship",
                  fin_labels)
    }
    sl <- setdiff(qf_adv, sf_adv)
    if (length(sl) > 0L) {
      sl_order <- sl[order(-w16_j[sl])]
      for (k in seq_along(sl_order))
        attribute(k, length(sl_order), n_sf_lost, sl_caps, "championship",
                  sl_labels)
    }
    ql <- setdiff(q_adv, qf_adv)
    if (length(ql) > 0L) {
      ql_order <- ql[order(-w15_j[ql])]
      for (k in seq_along(ql_order))
        attribute(k, length(ql_order), n_qf_lost, ql_caps, "championship",
                  ql_labels)
    }
  }
  list(tier_totals = tier_totals, n_teams = n_teams, n_sims = n_sims)
}

# Score the field, run the breakdown, print the per-tier accounting.
score_field <- function(n_teams, layerA, n_sims = 500L, seed = 1L) {
  field <- generate_field("nfl_2026_season", picks = picks, player_pool = pool,
                          targets = targets, n_teams = n_teams, seed = seed)
  rosters <- split(field$rosters$underdog_id, field$rosters$entry_id)
  simulate_per_stage_scores(
    rosters = rosters, positions = positions,
    layerA_draws = layerA, schedule = schedule,
    lineup_spec = lineup_spec, n_sims = n_sims, seed = seed)
}

cat("\n== Part 1: per-tier accounting at n_field=1200, n_sims=500 ==\n")
sim <- simulate_slate(feed, n_sims = 1000L, seed = 1L)
scores_1200 <- score_field(1200L, sim$draws, n_sims = 500L, seed = 1L)
res_1200 <- .bbm7_payouts_with_breakdown(scores_1200, tcfg, seed = 1L)

per_entry_assigned <- vapply(res_1200$tier_totals,
  function(x) x$total / (res_1200$n_teams * res_1200$n_sims), numeric(1))
per_entry_head <- vapply(res_1200$tier_totals, `[[`, character(1), "head")

cfg <- .configured_per_entry(tcfg)
# Match labels by best-effort string compare. Build the comparison table.
rows <- list()
for (lab in names(res_1200$tier_totals)) {
  assigned <- per_entry_assigned[[lab]]
  head     <- per_entry_head[[lab]]
  # Find configured row by matching rank_range substring.
  cfg_match <- NA_character_
  for (cfg_lab in names(cfg)) {
    if (substr(cfg_lab, 1, 1) == toupper(substr(head, 1, 1)) &&
        sub(".*\\[(.*)\\].*", "\\1", lab, perl = TRUE) ==
        sub(".*\\[(.*)\\].*", "\\1", cfg_lab, perl = TRUE)) {
      cfg_match <- cfg_lab; break
    }
  }
  cfg_val <- if (!is.na(cfg_match)) cfg[[cfg_match]]$configured_per_entry else NA_real_
  rows[[length(rows) + 1L]] <- data.frame(
    label = lab,
    head = head,
    assigned_per_entry = round(assigned, 4),
    configured_per_entry = round(cfg_val, 4),
    delta = round(assigned - cfg_val, 4),
    stringsAsFactors = FALSE
  )
}
tab <- do.call(rbind, rows)
tab <- tab[order(tab$head, -abs(tab$delta)), ]
cat("Per-tier assigned vs configured (per-entry $):\n")
print(tab, row.names = FALSE)

cat(sprintf("\nQualifier head total assigned: $%.4f  configured: $%.4f  delta: $%.4f\n",
            sum(tab$assigned_per_entry[tab$head == "qualifier"]),
            sum(tab$configured_per_entry[tab$head == "qualifier"], na.rm = TRUE),
            sum(tab$delta[tab$head == "qualifier"], na.rm = TRUE)))
cat(sprintf("Championship head total assigned: $%.4f  configured: $%.4f  delta: $%.4f\n",
            sum(tab$assigned_per_entry[tab$head == "championship"]),
            sum(tab$configured_per_entry[tab$head == "championship"], na.rm = TRUE),
            sum(tab$delta[tab$head == "championship"], na.rm = TRUE)))
cat(sprintf("\nTotal field-mean assigned EV: $%.2f\n",
            sum(tab$assigned_per_entry)))
cat(sprintf("Expected (configured / full field): $%.2f\n",
            15000010 / tcfg$total_field_size))

# ---------------------------------------------------------------------------
# Part 2: size convergence.
# ---------------------------------------------------------------------------

cat("\n== Part 2: size convergence ==\n")
# 1008 = 672336 / 667 is the cleanest cell -- 1 finalist per sim is the real
# rate. Try non-clean sizes too.
sizes <- c(1008L, 1200L, 2016L, 3024L, 5040L)
expected <- 15000010 / tcfg$total_field_size
results <- list()
for (n in sizes) {
  scores <- score_field(n, sim$draws, n_sims = 300L, seed = 1L)
  fp <- build_bbm7_field_payouts(scores, tcfg, seed = 1L)
  results[[length(results) + 1L]] <- data.frame(
    n_field = n,
    n_sims = 300L,
    field_mean_total_ev = round(fp$field_mean_total_ev, 2),
    expected = round(expected, 2),
    gap = round(fp$field_mean_total_ev - expected, 2),
    gap_rel = round((fp$field_mean_total_ev - expected) / expected, 4)
  )
  cat(sprintf("  n_field=%d  EV=%.2f  gap=%+.2f (%+.2f%%)\n",
              n, fp$field_mean_total_ev,
              fp$field_mean_total_ev - expected,
              100 * (fp$field_mean_total_ev - expected) / expected))
}
convergence <- do.call(rbind, results)
cat("\nConvergence table:\n")
print(convergence, row.names = FALSE)
