# Tests for the committed field-targets digest (R/field_targets_digest.R),
# which lets CI rebuild the *validated* scraped field without the gitignored
# raw drafts -- closing the gap where CI would otherwise publish ADP-only
# curves nothing in the validation chain saw.
#
# The headline live==validated check (digest curves == #30 fixture curves,
# within float tol) is inst/scripts/gate_curve_fidelity.R (needs the local
# validation layerA). Here we pin the rigorous, fast proxy: the digest
# round-trips to the same targets as the live scraped drafts and therefore
# generate_field() produces a BIT-IDENTICAL field at the fixture's config --
# and curves are a deterministic function of the field, so identical field =>
# identical curves.

SLATE <- "nfl_2026_season"

test_that("the committed digest is present, parses, and is usable by generate_field", {
  d <- bestballBroSim:::.load_field_targets_digest(SLATE)
  expect_false(is.null(d))
  expect_setequal(names(d$position_means), c("QB", "RB", "WR", "TE"))
  expect_true(all(is.finite(d$position_means)))
  expect_true(is.numeric(d$slot_adp_sd) && length(d$slot_adp_sd) > 0L &&
              all(is.finite(d$slot_adp_sd)))
  expect_true(is.numeric(d$qb_stack_2plus_rate))

  # generate_field consumes it without scraped drafts (the CI path).
  pool <- load_slate_data(SLATE)
  field <- generate_field(SLATE, player_pool = pool, targets = d,
                          n_teams = 400L, seed = 7L)
  ids <- split(field$rosters$underdog_id, field$rosters$entry_id)
  expect_equal(length(ids), 400L)
  expect_true(all(lengths(ids) == 18L))
  expect_true(all(field$rosters$underdog_id %in% pool$underdog_id))
  # the field reflects the digest: per-position MEAN counts track the
  # digest's position_means (generate_field targets means, not per-roster
  # minimums, so this is the meaningful "digest drives the field" check).
  pos <- stats::setNames(pool$position, pool$underdog_id)
  field_means <- colMeans(do.call(rbind, lapply(ids, function(r)
    table(factor(pos[r], levels = c("QB","RB","WR","TE"))))))
  expect_equal(unname(field_means[c("QB","RB","WR","TE")]),
               unname(d$position_means[c("QB","RB","WR","TE")]), tolerance = 0.4)
})

# Path to the field corpus (the digest's source of truth) for the fidelity
# proof. The digest is written from load_scraped_drafts() with no arg, which
# resolves the newest privacy-stripped corpus via .default_field_corpus_path()
# (the udbb-scraper-latest lineage is retired). Resolve the same way here so the
# round-trip compares the digest against exactly the source it was built from.
# Returns "" when no corpus is present (CI), so the skip guard fires.
.scraped_path <- function() {
  # testthat runs in tests/testthat, so .default_field_corpus_path()'s
  # wd-relative `../bestball-bro-data` lookup misses. Resolve the sibling repo
  # from the package root instead (newest boards_*.json, matching the default
  # resolver the digest was built through).
  root <- bestballBroSim:::.find_package_root()
  if (is.null(root)) return("")
  hits <- sort(Sys.glob(file.path(dirname(root), "bestball-bro-data",
                                  "sources", "field", "boards_*.json")),
               decreasing = TRUE)
  if (length(hits) > 0L) hits[[1L]] else ""
}

test_that("digest round-trips the scraped targets exactly (numeric identity)", {
  skip_if_not(file.exists(.scraped_path()), "Scraped drafts absent -- digest-source comparison skipped.")
  scraped <- compute_field_targets(load_scraped_drafts(.scraped_path()), slate_id = SLATE)
  d <- bestballBroSim:::.load_field_targets_digest(SLATE)
  ord <- c("QB", "RB", "WR", "TE")
  # position means + qb stack rate: exact (compare values, ignoring the
  # array/dimnames attributes tapply/vapply attach upstream).
  expect_equal(unname(d$position_means[ord]),
               unname(as.numeric(scraped$position_means[ord])), tolerance = 1e-12)
  expect_equal(d$qb_stack_2plus_rate, as.numeric(scraped$qb_stack_2plus_rate), tolerance = 1e-12)
  # the finite per-slot sigmas the digest stores match the scraped ones exactly;
  # slots it omits are exactly the non-finite scraped ones (generate_field's
  # global-sigma fallback handles them identically either way).
  sc_finite <- scraped$slot_adp_sd[is.finite(scraped$slot_adp_sd)]
  expect_setequal(names(d$slot_adp_sd), names(sc_finite))
  expect_equal(unname(d$slot_adp_sd[names(sc_finite)]),
               unname(as.numeric(sc_finite)), tolerance = 1e-12)
})

test_that("generate_field(digest) == generate_field(scraped) at the fixture config (CI builds the validated field)", {
  skip_if_not(file.exists(.scraped_path()), "Scraped drafts absent -- field-equality proof skipped.")
  scraped <- compute_field_targets(load_scraped_drafts(.scraped_path()), slate_id = SLATE)
  digest  <- bestballBroSim:::.load_field_targets_digest(SLATE)
  pool <- load_slate_data(SLATE)
  # the #30 fixture / oracle re-gate field config: n_teams = 2700, seed = 1.
  f_scraped <- generate_field(SLATE, player_pool = pool, targets = scraped, n_teams = 2700L, seed = 1L)
  f_digest  <- generate_field(SLATE, player_pool = pool, targets = digest,  n_teams = 2700L, seed = 1L)
  # bit-identical rosters => identical field_scores => identical curves.
  expect_identical(f_digest$rosters$underdog_id, f_scraped$rosters$underdog_id)
  expect_identical(f_digest$rosters$pick_overall, f_scraped$rosters$pick_overall)
})
