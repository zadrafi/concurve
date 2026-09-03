# dev-check.R — concurve development checks
# Run from the package root (the folder that contains DESCRIPTION).
# Two clearly separated concerns: routine checking (run often) and
# one-time scaffolding (run once, ever). No tryCatch swallowing errors,
# no library() of the package under test, no redundant check passes.

# ---- 0. Sanity: are we actually in the package root? -------------------
if (!file.exists("DESCRIPTION")) {
  stop("No DESCRIPTION here. setwd() to the concurve package root first — ",
       "usethis/devtools act on the working directory / active project.")
}

# ---- 1. ROUTINE: this is the whole check ------------------------------
# devtools::check() already: documents (roxygen -> Rd + NAMESPACE),
# builds the source tarball, builds the PDF manual, and runs
# R CMD check --as-cran. You do not need rcmdcheck + check_man +
# roxygenize + a manual Rd2pdf on top of it.
devtools::check(
  document  = TRUE,           # regenerate Rd/NAMESPACE from roxygen first
  cran      = TRUE,           # adds --as-cran
  manual    = TRUE,           # build the PDF manual (needs LaTeX; see note)
  vignettes = TRUE,
  error_on  = "warning"       # ONE value. Fail on error/warning; NOTEs are fine.
)

# Equivalent one-liner if you prefer rcmdcheck directly:
# rcmdcheck::rcmdcheck(args = "--as-cran", error_on = "warning")

# ---- 2. OPTIONAL: spelling, when you want it --------------------------
# One value flagged the CRAN false positives (Hjort, Rafi, Schweder,
# Surprisal, ...). Keep them in inst/WORDLIST so this stays quiet.
# spelling::spell_check_package(".")            # report only
# spelling::update_wordlist(".")                # fold new false positives in

# ---- 3. OPTIONAL: see the CRAN incoming queue -------------------------
# Do NOT install packages inside a check script. Install once, separately.
# foghorn::cran_incoming()   # whole incoming dashboard; no per-pkg arg


## =======================================================================
## ONE-TIME SETUP — you already did this; do NOT re-run on every check.
## Left here only as a record. Each of these edits DESCRIPTION/.Rbuildignore
## and several ERROR if the target already exists (e.g. use_cran_comments()).
## =======================================================================
if (FALSE) {

  # Dependencies: concurve's DESCRIPTION already declares these. Only run
  # use_package() when ADDING a new one — not as a batch on every build.
  # usethis::use_package("survival", "Imports")
  # usethis::use_package("testthat", "Suggests")

  # Build-ignore: paths are RELATIVE to the package root, and the default
  # escapes regex metacharacters. No "~/concurve/..." absolute paths.
  # usethis::use_build_ignore(c(
  #   "cran-comments.md", "_pkgdown.yml", "codemeta.json",
  #   "README.Rmd", ".github", "revdep", "docs"
  # ))

  # These each error if the file/setup already exists:
  # usethis::use_spell_check(lang = "en-US")   # adds spelling to Suggests + a test
  # usethis::use_cran_comments()               # ERRORS if cran-comments.md exists
  # codemetar::write_codemeta()                # regenerate only when metadata changes
}
