#!/usr/bin/env Rscript
# =============================================================================
# check_version_consistency.R
#
# Guards against two related classes of release-hygiene bug that have bitten
# this package before:
#
#   1. DESCRIPTION's Version advances (a new release, or a new dev version
#      like X.Y.Z.9000) with no corresponding NEWS.md entry -- so the
#      changelog silently falls behind the code.
#
#   2. _pkgdown.yml carries dormant configuration that only misbehaves once
#      some other, unrelated condition becomes true. Concretely:
#      `development: mode: auto` sat inert for the entire life of this
#      project, then activated the moment Version gained a 4th component,
#      silently splitting all future site builds into docs/dev/ instead of
#      docs/ -- the directory GitHub Pages actually serves -- so the live
#      site quietly stopped updating. Fail loudly if this reappears instead
#      of relying on someone noticing a stale site.
#
# Usage:
#   Rscript tools/check_version_consistency.R
#
# Exit status: 0 if clean, 1 if a problem is found.
# =============================================================================

pkg_root <- getwd()
desc_path <- file.path(pkg_root, "DESCRIPTION")
news_path <- file.path(pkg_root, "NEWS.md")
pkgdown_path <- file.path(pkg_root, "_pkgdown.yml")

if (!file.exists(desc_path)) {
  stop("Could not find DESCRIPTION in '", pkg_root, "'. Run this script from the package root.", call. = FALSE)
}

problems <- character(0)

# ---- 1. NEWS.md has an entry matching (the release form of) Version -------

desc <- read.dcf(desc_path)
version <- desc[1, "Version"]
# A dev version like 3.0.0.9000 documents the *upcoming* 3.0.0 release; only
# the first three components should appear in NEWS.md.
release_version <- paste(strsplit(version, "[.]")[[1]][1:3], collapse = ".")

cat("DESCRIPTION Version:", version, "\n")
cat("Expected top NEWS.md entry: concurve", release_version, "\n\n")

if (!file.exists(news_path)) {
  problems <- c(problems, sprintf(
    "NEWS.md does not exist, but DESCRIPTION Version is %s.", version
  ))
} else {
  news_lines <- readLines(news_path, warn = FALSE)
  first_heading <- news_lines[grepl("^# ", news_lines)][1]

  if (is.na(first_heading)) {
    problems <- c(problems, "NEWS.md has no top-level '# ' heading at all.")
  } else if (!grepl(release_version, first_heading, fixed = TRUE)) {
    problems <- c(problems, sprintf(
      "NEWS.md's top entry is %s, but DESCRIPTION Version implies the next release is %s.",
      shQuote(trimws(first_heading)), release_version
    ))
  }
}

# ---- 2. _pkgdown.yml: guard the specific dormant-config trap --------------

if (file.exists(pkgdown_path)) {
  pd <- tryCatch(yaml::yaml.load_file(pkgdown_path), error = function(e) NULL)

  if (is.null(pd)) {
    problems <- c(problems, "_pkgdown.yml could not be parsed as YAML.")
  } else {
    dev_mode <- pd$development$mode
    if (!is.null(dev_mode) && identical(dev_mode, "auto")) {
      problems <- c(problems, paste(
        "_pkgdown.yml has development: mode: auto. This is dormant until",
        "DESCRIPTION's Version gains a 4th component (e.g. a .9000 dev",
        "suffix), at which point pkgdown starts building the site to",
        "docs/dev/ instead of docs/ -- the directory GitHub Pages actually",
        "serves -- so the live site silently stops updating. Use",
        "'mode: release' (always build to docs/) or 'mode: devel' (always",
        "build to docs/dev/) so behavior doesn't depend on the current",
        "version number, or set this deliberately with a comment explaining",
        "the split is wanted and something (README, navbar, CI) actually",
        "links to docs/dev/."
      ))
    }
  }
} else {
  cat("(No _pkgdown.yml; skipping pkgdown config checks.)\n")
}

# ---- Report -----------------------------------------------------------------

if (length(problems) == 0) {
  cat("OK: NEWS.md matches DESCRIPTION Version, and no dormant pkgdown config found.\n")
  quit(status = 0, save = "no")
}

cat("Found", length(problems), "problem(s):\n\n")
for (p in problems) {
  cat("- ", paste(strwrap(p, width = 76, exdent = 2), collapse = "\n"), "\n\n", sep = "")
}
quit(status = 1, save = "no")
