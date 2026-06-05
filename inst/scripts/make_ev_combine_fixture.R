# Emit the language-independent golden fixture for the extension's JS EV
# combine. Self-contained: embedded inputs (a 64-path draws subset + the
# published Puppy 2 curves + lineup_spec + stage_weeks) and the expected
# outputs computed by the R reference (#27) on EXACTLY those embedded inputs
# (roster_round_scores -> evaluate_roster_curve_ev -> rank_marginal_ev).
#
# The extension validates its JS pipeline against `expected` using only
# `inputs`, offline, independent of feed redeploys. The companion test
# (tests/testthat/test-ev-combine-fixture.R) regenerates `expected` from the
# embedded `inputs` and asserts it matches -- so the fixture also pins the R
# reference.
#
# Run from the repo root:
#   "<Rscript>" inst/scripts/make_ev_combine_fixture.R

suppressMessages(devtools::load_all(".", quiet = TRUE))

SLATE_ID  <- "nfl_2026_season"
TOURN     <- "puppy2"
N_PATHS   <- 64L
SEED      <- 1L
FIELD_TEAMS <- 2700L
OUT       <- file.path("inst", "fixtures", "ev_combine_fixture_puppy2.json")
CURVES_SRC <- file.path("build", "ev_smoke")   # published Artifact B lives here

# ---- load reference inputs --------------------------------------------------
ckpt <- readRDS(file.path("build", "ev_smoke_ckpt.rds"))
pool_draws <- ckpt$pool_draws
feed <- blend_slate(SLATE_ID,
  sources_manifest_path = file.path("inst","data","sources","_manifest.yaml"),
  slates_manifest_path  = file.path("inst","data","slates","_manifest.yaml"),
  write_json = FALSE)
positions   <- positions_from_feed(feed)
lineup_spec <- load_slate_lineup_spec(SLATE_ID)
curves <- read_tournament_curves(CURVES_SRC, TOURN)   # exactly as published
sw <- curves$stage_weeks
covered_ids <- pool_draws$player_ids

# season-total coefficient of variation per covered player (boom/bust metric)
season_tot <- apply(pool_draws$tensor, 2L, function(M) colSums(M)) / pool_draws$quant_scale
colnames(season_tot) <- covered_ids
s_cv <- matrixStats::colSds(season_tot) / pmax(colMeans(season_tot), 1e-6)

adp_vec <- vapply(feed$players, function(p) as.numeric(p$adp %||% NA_real_), numeric(1))
names(adp_vec) <- names(feed$players)

# ---- pick a covered, lineup-valid test roster -------------------------------
# A real field roster (realistic shape) whose 18 ids are all in the tensor.
picks   <- load_scraped_drafts(); pool <- load_slate_data(SLATE_ID)
targets <- compute_field_targets(picks, slate_id = SLATE_ID)
field   <- generate_field(SLATE_ID, picks = picks, player_pool = pool,
                          targets = targets, n_teams = FIELD_TEAMS, seed = SEED)
field_rosters <- split(field$rosters$underdog_id, field$rosters$entry_id)
valid_roster <- function(r) {
  if (length(r) != 18L || !all(r %in% covered_ids)) return(FALSE)
  pc <- table(factor(positions[r], levels = c("QB","RB","WR","TE")))
  pc[["QB"]] >= 1L && pc[["RB"]] >= 2L && pc[["WR"]] >= 3L && pc[["TE"]] >= 1L
}
roster <- NULL
for (e in names(field_rosters)) if (valid_roster(field_rosters[[e]])) { roster <- field_rosters[[e]]; break }
if (is.null(roster)) cli::cli_abort("No fully-covered, lineup-valid field roster found.")
cat(sprintf("test roster: %s (entry %s)\n  pos: %s\n", e,
            paste(table(factor(positions[roster], levels=c("QB","RB","WR","TE"))), collapse="/"),
            paste(positions[roster], collapse=",")))

# ---- build the candidate set (spanning chalk / boom-bust / scarce / uncovered)
avail <- setdiff(covered_ids, roster)
avail_adp <- sort(adp_vec[intersect(names(adp_vec), avail)])   # ascending ADP
chalk <- names(utils::head(avail_adp, 4L))                      # best ADP available
boom  <- names(utils::head(sort(s_cv[avail], decreasing = TRUE), 2L))  # highest CV
te_av <- avail[positions[avail] == "TE"]; scarce_te <- te_av[which.min(adp_vec[te_av])]
qb_av <- avail[positions[avail] == "QB"]; scarce_qb <- qb_av[which.min(adp_vec[qb_av])]
mid   <- names(avail_adp[seq(40L, by = 25L, length.out = 3L)])  # a few mid-ADP
cov_cands <- unique(c(chalk, boom, scarce_te, scarce_qb, mid))
cov_cands <- cov_cands[!is.na(cov_cands)]
# one real-but-UNCOVERED id: an ADP'd feed player outside the tensor universe.
uncovered_pool <- setdiff(names(adp_vec)[!is.na(adp_vec)], covered_ids)
uncovered_id <- names(sort(adp_vec[uncovered_pool]))[1L]        # most-drafted uncovered
if (is.na(uncovered_id)) cli::cli_abort("No uncovered ADP player available for the fixture.")

tag_of <- function(id) {
  if (identical(id, uncovered_id)) return("uncovered")
  if (id %in% c(scarce_te, scarce_qb)) return("scarce")
  if (id %in% boom) return("boom_bust")
  if (id %in% chalk) return("chalk")
  "mid"
}
all_cands <- c(cov_cands, uncovered_id)
cat(sprintf("candidates: %d covered + 1 uncovered\n", length(cov_cands)))

# ---- subset the tensor: union(roster, covered candidates), first N_PATHS ------
embed_ids <- unique(c(roster, cov_cands))
col_idx <- match(embed_ids, pool_draws$player_ids)
stopifnot(!anyNA(col_idx))
sub <- pool_draws
sub$tensor      <- pool_draws$tensor[, col_idx, seq_len(N_PATHS), drop = FALSE]
sub$player_ids  <- embed_ids
sub$positions   <- pool_draws$positions[embed_ids]
sub$n_paths     <- N_PATHS
# sub is a valid bbbro_ev_draws over the embedded players/paths.

# ---- compute the expected outputs on the EMBEDDED inputs ---------------------
rs <- roster_round_scores(roster, sub, sw, lineup_spec)
roster_ev <- evaluate_roster_curve_ev(roster, sub, curves, lineup_spec)$ev
rk <- rank_marginal_ev(roster, cov_cands, sub, curves, lineup_spec)   # covered only
cat(sprintf("roster EV $%.4f | top candidate %s $%+.4f\n",
            roster_ev, substr(rk$underdog_id[1],1,8), rk$marginal_ev[1]))

# ---- assemble the fixture JSON ----------------------------------------------
# Flat int16 array in [path][player][week] C-order == as.integer() of the
# R [week,player,path] column-major tensor (documented in the schema header).
values <- as.integer(sub$tensor)
player_index <- stats::setNames(as.list(seq_along(embed_ids) - 1L), embed_ids)  # 0-based

candidates_meta <- lapply(all_cands, function(id) list(
  id = id, position = unname(positions[id] %||% NA_character_),
  tag = tag_of(id), covered = id %in% covered_ids))

fixture <- list(
  `_schema` = list(
    description = paste(
      "Golden fixture for the Chrome-extension JS EV combine, computed by the R",
      "reference (bestballBroSim R/ev_blocks.R). Self-contained: validate the JS",
      "pipeline offline against `expected` using only `inputs`."),
    generated_by = "inst/scripts/make_ev_combine_fixture.R",
    reference_loop = "roster_round_scores -> evaluate_roster_curve_ev (combine) -> rank_marginal_ev",
    combine_formula = paste(
      "dollars(path) = g1(R1) * ( (1-g2(R2))*payout_qf(R2)",
      "+ g2(R2)*(1-g3(R3))*payout_sf(R3) + g2(R2)*g3(R3)*h_final(R4) );",
      "roster_ev = mean over paths. marginal_ev(cand) = EV(roster+cand) - EV(roster), same paths (CRN)."),
    draws = list(
      dtype = "int16", quant_scale_note = "score = value / quant_scale",
      axis_order = c("path","player","week"),
      layout = "flat C-order array; value index = ((path*n_players)+player)*n_weeks + week (all 0-based)",
      player_index = "appearance_id -> 0-based player index; weeks[k] = NFL week for week-index k"),
    curves = "each curve is an {x,y} grid; evaluate by linear interpolation clamped at the ends (R approx rule=2)",
    round_scores = "per path, R[r] = sum over stage_weeks[r] of the best-ball optimal weekly lineup total (FLEX = best overflow of RB/WR/TE per lineup_spec)",
    uncovered = "candidate ids NOT in draws.player_index are uncovered: exclude from ranking and flag (the reference would abort if asked to score one) -- see expected.uncovered_candidates",
    tolerance = "doubles; assert |js - r| <= 1e-6 absolute for scores, <= 1e-6 relative for EV/marginals"),
  inputs = list(
    slate = SLATE_ID, tournament = TOURN, n_paths = N_PATHS,
    quant_scale = pool_draws$quant_scale, weeks = as.integer(sub$weeks),
    lineup_spec = lineup_spec, stage_weeks = sw,
    draws = list(
      n_paths = N_PATHS, n_players = length(embed_ids), n_weeks = length(sub$weeks),
      player_index = player_index, positions = as.list(sub$positions),
      values = values),
    curves = curves$curves,
    test_roster = roster,
    candidates = candidates_meta),
  expected = list(
    round_scores = list(R1 = rs$R1, R2 = rs$R2, R3 = rs$R3, R4 = rs$R4),
    roster_ev = roster_ev,
    marginal_ranking = lapply(seq_len(nrow(rk)), function(i) list(
      id = rk$underdog_id[i], position = rk$position[i], marginal_ev = rk$marginal_ev[i])),
    uncovered_candidates = list(uncovered_id)))

dir.create(dirname(OUT), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(fixture, OUT, auto_unbox = TRUE, pretty = TRUE,
                     digits = NA, null = "null", na = "null")
cat(sprintf("wrote %s (%.0f KB)\n", OUT, file.size(OUT) / 1024))
