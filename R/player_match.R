#' Normalize a player match key (name + fantasy position) for cross-source joins
#'
#' The single bridge between Underdog's live-API player records (used by
#' [load_scraped_drafts()]) and the slate-CSV / blended-feed player
#' records (used by [blend_slate()] / Layer A). The two sources publish
#' the same player under two different Underdog UUIDs, so every
#' scraper-to-feed join in this package -- the 3b-5 validator's bridge,
#' Clay's name-side fallback inside the blender, and the
#' 3b-6 / 3b-7 field-model sites that will land next -- routes through
#' this one function.
#'
#' Pass **combined** names (e.g. `"Travis Etienne Jr."`,
#' `"Amon-Ra St. Brown"`), not pre-split first/last pairs: the canonical
#' name normalizer correctly handles multi-token last names and
#' generational suffixes only when it sees the whole string.
#'
#' @section Position remap:
#' Underdog's live API occasionally labels two-way players by their
#' defensive position (Travis Hunter as `"CB"`, the odd ex-fullback as
#' `"FB"`). They only score fantasy points at their offensive position,
#' so [.normalize_fantasy_position()] remaps `CB -> WR` and `FB -> RB`.
#' The same key applied to the slate CSV's `"WR"` then matches.
#'
#' @param full_name Character vector of full player names.
#' @param position Character vector of position labels. Must be the same
#'   length as `full_name`.
#' @return Character vector of `"<normalized name>|<normalized position>"`
#'   keys, with `NA` propagated where any input is `NA`.
#' @export
normalize_player_key <- function(full_name, position) {
  if (length(full_name) != length(position)) {
    cli::cli_abort("`full_name` and `position` must be the same length.")
  }
  if (length(full_name) == 0L) return(character(0))
  nm  <- .normalize_name(full_name)
  pos <- .normalize_fantasy_position(position)
  out <- paste(nm, pos, sep = "|")
  bad <- is.na(full_name) | is.na(position) |
    is.na(nm) | is.na(pos) | !nzchar(nm) | !nzchar(pos)
  out[bad] <- NA_character_
  out
}

#' Player-name aliases used by `.normalize_name()`
#'
#' Keys are the *non-canonical* form (lowercased, after period strip
#' and suffix strip); values are the canonical form (as the slate CSV
#' carries it, also post-suffix-strip). Applied AFTER the regex-driven
#' normalization steps in `.normalize_name()`. Add entries here when
#' the per-slate audit file at `build/blender_audit_<slate_id>.txt`
#' surfaces a player who shows up in ETR/LegUp but not Clay because
#' Clay's PDF lists them under a different name.
#'
#' Conventions:
#'   - Lowercase
#'   - No periods
#'   - No trailing generational suffix (it's already stripped by the
#'     time the alias map is consulted, so "Ken Walker III" -> "ken walker")
#'   - Map TO the slate CSV's canonical name, post-normalize
#'
#' Document any addition in README.md (Name aliases section).
#' @keywords internal
.NAME_ALIASES <- c(
  "ken walker"     = "kenneth walker",   # Clay says "Ken Walker", slate says "Kenneth Walker"
  "marquise brown" = "hollywood brown"   # Clay says "Marquise Brown", slate says "Hollywood Brown"
)

#' Normalize a player name for cross-source matching
#'
#' Lowercases, strips periods, normalizes unicode apostrophes/hyphens
#' to ASCII variants, collapses whitespace, strips a trailing
#' generational suffix token (II..IX, Jr, Sr -- with or without a
#' period), and then applies the [.NAME_ALIASES] map for known
#' canonical-name disagreements between sources and the slate CSV.
#'
#' Used only for cross-source matching; the canonical output name
#' preserves the original suffix and full first name.
#' @keywords internal
.normalize_name <- function(x) {
  if (length(x) == 0L) return(character(0))
  x <- tolower(x)
  x <- gsub("[‘’']", "", x, perl = TRUE)   # curly/straight apostrophes
  x <- gsub("[–—]", "-", x, perl = TRUE)   # en/em-dash -> hyphen
  x <- gsub("\\.", "", x)                            # A.J. -> AJ; Jr. -> Jr
  x <- gsub("\\s+", " ", x, perl = TRUE)
  x <- trimws(x)
  # Strip trailing generational suffix (after period strip so "Jr." == "jr")
  x <- gsub("\\s+(ii|iii|iv|v|vi|vii|viii|ix|jr|sr)$", "", x, perl = TRUE)
  x <- trimws(x)
  # Alias map for known cross-source canonical-name disagreements.
  hit <- match(x, names(.NAME_ALIASES))
  if (any(!is.na(hit))) {
    fix <- !is.na(hit)
    x[fix] <- unname(.NAME_ALIASES[hit[fix]])
  }
  x
}

#' Normalize a fantasy position label for cross-source matching
#'
#' Underdog's live API sometimes labels two-way players by their
#' defensive position (Travis Hunter as `CB`); the slate CSV / feed
#' uses the fantasy-scoring position (`WR`). Remap so the two match.
#' Same idea for the rare ex-fullback case (`FB -> RB`).
#' @keywords internal
.normalize_fantasy_position <- function(pos) {
  if (length(pos) == 0L) return(character(0))
  pos <- toupper(trimws(as.character(pos)))
  pos[pos == "CB"] <- "WR"
  pos[pos == "FB"] <- "RB"
  pos
}
