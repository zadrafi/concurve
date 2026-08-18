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
#   3. _pkgdown.yml's `url:` silently drifting out of sync with
#      DESCRIPTION's `URL:` field. pkgdown requires its site url to appear
#      among DESCRIPTION's URL entries (checked by pkgdown::check_pkgdown()
#      itself, via check_urls()) -- hit earlier this session as "URL is
#      missing package url" when _pkgdown.yml's url held a comma-separated
#      list instead of a single site URL. Checked here directly (fast, no
#      pkgdown dependency) in addition to via check_pkgdown() below.
#
#   4. Anything else pkgdown's own check_pkgdown() validates -- reference
#      index completeness, articles index completeness, and whatever
#      DESCRIPTION-coupled checks future pkgdown versions add -- run as a
#      catch-all if the pkgdown package is available, rather than
#      re-implementing pkgdown's own validation logic by hand.
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
  # ---- 3. url: in _pkgdown.yml matches a DESCRIPTION URL entry ------------

  if (!is.null(pd) && !is.null(pd$url)) {
    desc_urls <- if ("URL" %in% colnames(desc)) {
      trimws(strsplit(desc[1, "URL"], ",")[[1]])
    } else {
      character(0)
    }
    norm_url <- function(u) sub("/+$", "", trimws(u))

    if (length(desc_urls) == 0) {
      problems <- c(problems, paste(
        "_pkgdown.yml has a url:, but DESCRIPTION has no URL: field at all.",
        "pkgdown::check_pkgdown() requires the site url to be listed there."
      ))
    } else if (!norm_url(pd$url) %in% norm_url(desc_urls)) {
      problems <- c(problems, sprintf(
        paste(
          "_pkgdown.yml's url (%s) does not match any entry in DESCRIPTION's",
          "URL field (%s). pkgdown::check_pkgdown() fails on this mismatch,",
          "and a wrong url also breaks canonical-link and opengraph metadata",
          "on every page of the built site."
        ),
        pd$url, paste(desc_urls, collapse = ", ")
      ))
    }
  }
} else {
  cat("(No _pkgdown.yml; skipping pkgdown config checks.)\n")
}

# ---- 4. Catch-all: pkgdown's own validation, if pkgdown is installed ------

if (file.exists(pkgdown_path)) {
  if (requireNamespace("pkgdown", quietly = TRUE)) {
    tryCatch(
      {
        suppressMessages(pkgdown::check_pkgdown(pkg_root))
        cat("pkgdown::check_pkgdown(): no problems found.\n")
      },
      error = function(e) {
        problems <<- c(problems, paste0(
          "pkgdown::check_pkgdown() failed: ", conditionMessage(e)
        ))
      }
    )
  } else {
    cat("(pkgdown package not installed; skipping pkgdown::check_pkgdown().)\n")
  }
}

# ---- Report -----------------------------------------------------------------

if (length(problems) == 0) {
  cat("OK: NEWS.md matches DESCRIPTION Version, and no pkgdown config drift found.\n")
  quit(status = 0, save = "no")
}

cat("Found", length(problems), "problem(s):\n\n")
for (p in problems) {
  cat("- ", paste(strwrap(p, width = 76, exdent = 2), collapse = "\n"), "\n\n", sep = "")
}
quit(status = 1, save = "no")
