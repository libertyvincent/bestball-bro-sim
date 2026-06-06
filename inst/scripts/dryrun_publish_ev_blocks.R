# Local end-to-end dry-run of the EV-blocks publish step (the sprint gate).
#
# Mimics the CI state right after publish_v2(): a _meta.json carrying the
# projection keys (+ a sentinel to prove merge-without-clobber) and the
# Layer A draws parquet under v2/draws/. Then runs
# publish_ev_blocks_pipeline() and asserts every gate item.
#
# Uses the checkpointed Layer A draws (build/ev_smoke_ckpt.rds) as the
# parquet source, and a FAST field config (the publish PLUMBING is what's
# under test, not curve quality). Run from the repo root:
#   "<Rscript>" inst/scripts/dryrun_publish_ev_blocks.R

suppressMessages(devtools::load_all(".", quiet = TRUE))
SLATE <- "nfl_2026_season"
DEPLOY <- file.path("build", "dry_deploy")
unlink(DEPLOY, recursive = TRUE); dir.create(file.path(DEPLOY, "v2", "draws"),
                                             recursive = TRUE, showWarnings = FALSE)

# ---- mimic publish_v2 output ------------------------------------------------
ckpt <- readRDS(file.path("build", "ev_smoke_ckpt.rds"))
parquet <- file.path(DEPLOY, "v2", "draws", paste0(SLATE, ".parquet"))
arrow::write_parquet(ckpt$layerA, parquet)
cat(sprintf("wrote Layer A parquet: %d rows (%d sims)\n",
            nrow(ckpt$layerA), length(unique(ckpt$layerA$sim_idx))))

# a projections JSON (so v2_path points at a real file) + _meta with a
# sentinel key that MUST survive the EV merge.
dir.create(file.path(DEPLOY, "v2", "projections"), recursive = TRUE, showWarnings = FALSE)
proj <- file.path(DEPLOY, "v2", "projections", paste0(SLATE, ".json"))
writeLines('{"slate":"nfl_2026_season"}', proj)
meta0 <- list(
  season = 2026L, generated_at = "2026-08-15T03:00:00Z",
  slates = list(`nfl_2026_season` = list(
    underdog_slate_id = "a9c04e81-1ace-4b16-a31d-4c725a47f16f",
    v2_path = "v2/projections/nfl_2026_season.json",
    v2_sha256 = bestballBroSim:::.file_sha256(proj),
    v2_generated_at = "2026-08-15T03:00:00Z",
    sentinel_keep = "DO_NOT_CLOBBER")))
jsonlite::write_json(meta0, file.path(DEPLOY, "_meta.json"),
                     auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null")

# ---- run the pipeline (fast field; real N=500/top_n=300 for the size gate) ---
# field_targets pins the ADP-only CI path (no scraped drafts) so this dry-run
# faithfully mirrors production CI.
fast_cfg <- list(list(slate_id = SLATE, tournament_ids = c("puppy2", "dachshund"),
                      n_paths = 500L, top_n = 300L, field_teams = 900L, field_sims = 50L,
                      field_targets = bestballBroSim:::.default_field_targets()))
publish_ev_blocks_pipeline(DEPLOY, config = fast_cfg, marginal_sims = 1500L, seed = 1L)

# ---- GATE -------------------------------------------------------------------
cat("\n================= GATE =================\n")
pass <- TRUE
chk <- function(ok, msg) { cat(sprintf("  [%s] %s\n", if (ok) "PASS" else "FAIL", msg)); if (!ok) pass <<- FALSE }

files <- c("v2/ev/nfl_2026_season_draws.bin", "v2/ev/nfl_2026_season_draws.json",
           "v2/ev/puppy2_curves.json", "v2/ev/dachshund_curves.json")
for (f in files) chk(file.exists(file.path(DEPLOY, f)), sprintf("present: %s", f))

meta <- jsonlite::fromJSON(file.path(DEPLOY, "_meta.json"), simplifyVector = FALSE)
e <- meta$slates[[SLATE]]
chk(!is.null(e$v2_draws_path) && e$v2_draws_path == "v2/ev/nfl_2026_season_draws.bin", "_meta v2_draws_path")
chk(!is.null(e$v2_draws_sidecar_path), "_meta v2_draws_sidecar_path")
chk(!is.null(e$v2_draws_sha256) && !is.null(e$v2_draws_sidecar_sha256), "_meta draws sha256s")
chk(!is.null(e$v2_draws_generated_at), "_meta v2_draws_generated_at")
chk(!is.null(e$tournaments$puppy2$curves_path), "_meta tournaments.puppy2.curves_path")
chk(!is.null(e$tournaments$dachshund$curves_path), "_meta tournaments.dachshund.curves_path")
chk(!is.null(e$tournaments$puppy2$curves_sha256) && !is.null(e$tournaments$dachshund$curves_sha256),
    "_meta tournaments curves sha256s")
# merge-without-clobber
chk(identical(e$v2_path, "v2/projections/nfl_2026_season.json"), "v2_path intact")
chk(identical(e$sentinel_keep, "DO_NOT_CLOBBER"), "sentinel projection key NOT clobbered")
chk(identical(meta$season, 2026L), "season header intact")

# sidecar player_index ~ 295 ; .bin size ~ N*players*17*2
sc <- jsonlite::fromJSON(file.path(DEPLOY, "v2/ev/nfl_2026_season_draws.json"), simplifyVector = TRUE)
np <- length(sc$player_index)
chk(np >= 280 && np <= 300, sprintf("sidecar player_index count = %d (~295)", np))
bin_sz <- file.size(file.path(DEPLOY, "v2/ev/nfl_2026_season_draws.bin"))
expect_sz <- 500L * np * 17L * 2L
chk(bin_sz == expect_sz, sprintf(".bin size = %d bytes (expect N*players*17*2 = %d; %.2f MB)",
                                 bin_sz, expect_sz, bin_sz/1e6))

# self-describing feed: sidecar carries the Season lineup_spec (QB1/RB2/WR3/
# TE1/FLEX1[RB,WR,TE]) in the #30 fixture shape, == load_slate_lineup_spec().
ls_live <- sc$lineup_spec
ls_ref  <- jsonlite::fromJSON(jsonlite::toJSON(load_slate_lineup_spec(SLATE),
                              auto_unbox = TRUE), simplifyVector = TRUE)
chk(!is.null(ls_live) && identical(ls_live$slate_id, SLATE), "sidecar carries lineup_spec.slate_id")
chk(identical(ls_live$slots$pos, c("QB","RB","WR","TE","FLEX")) &&
    identical(as.integer(ls_live$slots$n), c(1L,2L,3L,1L,1L)) &&
    identical(ls_live$slots$eligible[[5L]], c("RB","WR","TE")),
    "sidecar lineup_spec == Season lineup (QB1/RB2/WR3/TE1/FLEX1[RB,WR,TE])")
chk(isTRUE(all.equal(ls_live, ls_ref)), "sidecar lineup_spec == load_slate_lineup_spec()")

# shas in _meta match deployed files (the extension's freshness gate)
sha_ok <- function(rel, sha) identical(bestballBroSim:::.file_sha256(file.path(DEPLOY, rel)), sha)
chk(sha_ok(e$v2_draws_path, e$v2_draws_sha256), "draws .bin sha matches file")
chk(sha_ok(e$v2_draws_sidecar_path, e$v2_draws_sidecar_sha256), "sidecar sha matches file")
chk(sha_ok(e$tournaments$puppy2$curves_path, e$tournaments$puppy2$curves_sha256), "puppy2 curves sha matches file")
chk(sha_ok(e$tournaments$dachshund$curves_path, e$tournaments$dachshund$curves_sha256), "dachshund curves sha matches file")

cat(sprintf("\n================= %s =================\n", if (pass) "ALL GATE CHECKS PASS" else "GATE FAILED"))
if (!pass) quit(status = 1L)
