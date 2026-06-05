# HARD GATE: the curves CI publishes (built from the committed field-targets
# digest, no scraped drafts) must EQUAL the curves built from the live
# scraped drafts -- i.e. the digest reproduces the *validated* scraped field.
#
# We compare curves rebuilt NOW, holding everything but the targets source
# constant (ckpt$layerA, n_teams=2700, field_sims=400, seed=1, n_grid=256):
#   * scraped : compute_field_targets(scraped drafts)  -- the validation field
#   * digest  : the committed field-targets digest     -- the CI path
#   * default : ADP-only .default_field_targets        -- the old CI fallback
# HARD GATE: digest == scraped (the fix). GAP: default != scraped (the bug
# #31 would have shipped). This is the sprint's "equivalently, the
# locally-built scraped-field curves" check -- robust to code/draw drift
# since the frozen ckpt (which the fixture comparison below is NOT: the #30
# fixture embeds curves from the Jun-2 ckpt$field_scores, so rebuilding with
# today's field/draw code can differ from it for reasons orthogonal to this
# sprint; reported as informational).
#
# Local gate: needs build/ev_smoke_ckpt.rds (the validation layerA) and the
# scraped drafts. Run from the repo root:
#   "<Rscript>" inst/scripts/gate_curve_fidelity.R

suppressMessages(devtools::load_all(".", quiet = TRUE))
SLATE <- "nfl_2026_season"; SEED <- 1L; FIELD_TEAMS <- 2700L; FIELD_SIMS <- 400L; NGRID <- 256L
FIX <- "inst/fixtures/ev_combine_fixture_puppy2.json"
TOL <- 1e-6

ckpt <- readRDS(file.path("build", "ev_smoke_ckpt.rds"))
layerA <- ckpt$layerA
feed <- blend_slate(SLATE,
  sources_manifest_path = file.path("inst","data","sources","_manifest.yaml"),
  slates_manifest_path  = file.path("inst","data","slates","_manifest.yaml"), write_json = FALSE)
positions <- positions_from_feed(feed); schedule <- schedule_from_feed(feed)
lineup_spec <- load_slate_lineup_spec(SLATE); pcfg <- load_tournament("puppy2")
pool <- load_slate_data(SLATE)
fix_curves <- jsonlite::fromJSON(FIX, simplifyVector = TRUE)$inputs$curves

curve_maxdiff <- function(a, b) {
  max(vapply(names(a), function(nm)
    max(abs(as.numeric(a[[nm]]$y) - as.numeric(b[[nm]]$y)),
        abs(as.numeric(a[[nm]]$x) - as.numeric(b[[nm]]$x))), numeric(1)))
}
build_curves <- function(targets, label) {
  field <- generate_field(SLATE, player_pool = pool, targets = targets,
                          n_teams = FIELD_TEAMS, seed = SEED)
  fr <- split(field$rosters$underdog_id, field$rosters$entry_id)
  fs <- simulate_per_stage_scores(fr, positions, layerA, schedule, lineup_spec,
                                  n_sims = FIELD_SIMS, seed = SEED)
  cat(sprintf("  built %s field+curves\n", label))
  build_tournament_curves(pcfg, fs, n_grid = NGRID, seed = SEED)$curves
}

scraped_t <- compute_field_targets(load_scraped_drafts(), slate_id = SLATE)
digest_t  <- bestballBroSim:::.load_field_targets_digest(SLATE)
default_t <- bestballBroSim:::.default_field_targets()

cat("== rebuilding curves three ways (this runs 3 field sims) ==\n")
c_scraped <- build_curves(scraped_t, "scraped (validation field)")
c_digest  <- build_curves(digest_t,  "digest (CI path)")
c_default <- build_curves(default_t, "default (ADP-only)")

d_digest_vs_scraped  <- curve_maxdiff(c_digest,  c_scraped)
d_default_vs_scraped <- curve_maxdiff(c_default, c_scraped)
d_scraped_vs_fixture <- curve_maxdiff(c_scraped, fix_curves)   # informational (drift)

cat("\n================= CURVE-FIDELITY GATE =================\n")
cat(sprintf("  digest  vs scraped (rebuilt today): max|diff| = %.3e   [the FIX -- must be ~0]\n", d_digest_vs_scraped))
cat(sprintf("  default vs scraped (rebuilt today): max|diff| = %.3e   [the GAP -- must be large]\n", d_default_vs_scraped))
cat(sprintf("  scraped(today) vs #30 fixture:      max|diff| = %.3e   [informational: ckpt/code drift]\n", d_scraped_vs_fixture))

pass <- TRUE
chk <- function(ok, m) { cat(sprintf("  [%s] %s\n", if (ok) "PASS" else "FAIL", m)); if (!ok) pass <<- FALSE }
cat("\n")
chk(d_digest_vs_scraped  < TOL,  sprintf("digest (CI) curves == scraped (validated) curves (< %.0e)  <-- HARD GATE", TOL))
chk(d_default_vs_scraped > 0.01, "ADP-only curves DIFFER from validated scraped curves (the gap #31 would ship)")
cat(sprintf("\n================= %s =================\n", if (pass) "CURVE FIDELITY OK" else "GATE FAILED"))
if (!pass) quit(status = 1L)
