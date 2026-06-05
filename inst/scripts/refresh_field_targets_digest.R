# Refresh the committed field-targets digests from the local scraped drafts.
#
# The digest (inst/data/field_targets/<slate>.json) is a SNAPSHOT of
# compute_field_targets() so CI can rebuild the validated scraped field
# without the gitignored raw drafts. Re-run this whenever drafts are
# re-scraped (inst/data/scraped_drafts/udbb-scraper-latest.json updated):
#
#   "<Rscript>" inst/scripts/refresh_field_targets_digest.R
#
# Then commit the updated inst/data/field_targets/*.json. (Longer term the
# scraper-observes-drafts loop could publish this automatically.)

suppressMessages(devtools::load_all(".", quiet = TRUE))
slates <- unique(vapply(ev_blocks_publish_config(), function(c) c$slate_id, character(1)))
for (sid in slates) write_field_targets_digest(sid)
cli::cli_alert_success("Refreshed field-targets digest(s): {slates}")
