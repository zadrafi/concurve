## =====================================================================
## concurve package-maintenance helpers
##
## DEFINITIONS ONLY. Sourcing this file defines functions and does
## nothing else -- no library() calls, no workflow invocation. Call the
## functions you want, deliberately:
##
##   source("dev/usethis.R")
##   manage_build_ignores()
##
## Moved here from the package root on 2026-09-04. History: the root
## copy carried top-level library() calls and a trailing
## execute_package_workflow(), so pasting or sourcing it re-ran the whole
## workflow and repeatedly reverted DESCRIPTION, .Rbuildignore,
## cran-comments.md, tests/spelling.R, and codemeta.json. `dev/` is
## .Rbuildignore'd via `^dev$`, so this file never enters the tarball.
##
## All functions use pkg::fun() so nothing needs to be attached first.
## =====================================================================


## ---------------------------------------------------------------------
## One-time project scaffolding
##
## These were run once when the project was created. Re-running them on
## an established package is almost always wrong -- several error if the
## target already exists, and use_mit_license() would contradict the
## GPL-3 license concurve actually ships under.
## ---------------------------------------------------------------------

create_project_structure <- function() {
  usethis::use_blank_slate()
  usethis::use_git()
  usethis::use_github()
}

setup_package_infrastructure <- function() {
  # NOTE: concurve is GPL-3. Do not call use_mit_license() here.
  usethis::use_readme_rmd()
  usethis::use_news_md()
  usethis::use_lifecycle_badge("experimental")
  usethis::use_code_of_conduct()
}

configure_ci <- function() {
  usethis::use_github_action_check_standard()
  usethis::use_github_action("test-coverage")
  usethis::use_codecov()
  usethis::use_github_action("pkgdown")
}

add_dev_dependencies <- function() {
  usethis::use_dev_package("testthat")
  usethis::use_dev_package("knitr")
  usethis::use_dev_package("rmarkdown")
  usethis::use_package_doc()
}

update_documentation <- function() {
  devtools::document()
  pkgdown::build_site()
  usethis::use_vignette("intro")
}


## ---------------------------------------------------------------------
## Dependency declarations
##
## FIXED 2026-09-04: survival, survminer, ProfileLikelihood, officer, and
## pbmcapply were previously added to Imports. Nothing in R/ calls them --
## they appear only in vignettes, examples, and tests -- so Imports
## produces the "All declared Imports should be used" NOTE. They belong
## in Suggests, and this function now puts them there.
## ---------------------------------------------------------------------

manage_package_dependencies <- function() {
  # Packages actually called from R/.
  core_imports <- c(
    "lme4", "parallel", "boot", "bcaboot", "ggplot2", "metafor",
    "dplyr", "tidyr", "flextable", "knitr", "tibble", "scales",
    "colorspace", "numDeriv", "rlang", "methods"
  )

  # Used only conditionally (requireNamespace-guarded), or only in
  # vignettes / examples / tests. rstan backs curve_stan_fit() and is
  # deliberately Suggests-only: see AGENTS.md, "Stan integration".
  dev_suggests <- c(
    "testthat", "covr", "spelling", "Lock5Data", "rmarkdown",
    "roxygen2md", "survival", "survminer", "ProfileLikelihood",
    "officer", "pbmcapply", "likelihoodAsy", "rstan"
  )

  for (pkg in core_imports) {
    tryCatch(
      usethis::use_package(pkg, "Imports"),
      error = function(e) message("Error adding import for ", pkg, ": ", conditionMessage(e))
    )
  }

  for (pkg in dev_suggests) {
    tryCatch(
      usethis::use_package(pkg, "Suggests"),
      error = function(e) message("Error adding suggest for ", pkg, ": ", conditionMessage(e))
    )
  }

  invisible(TRUE)
}


## ---------------------------------------------------------------------
## .Rbuildignore
##
## FIXED 2026-09-04, two separate bugs:
##
##  1. escape = FALSE wrote unanchored patterns. "references.bib" and
##     "american-medical-association.csl" then matched
##     vignettes/references.bib and the CSL as well, hiding them from the
##     tarball -- so every vignette failed to re-build under R CMD check
##     with "File 'references.bib' not found in resource path". Those two
##     entries are gone and escape is left at its default (TRUE), which
##     anchors each pattern as ^...$.
##
##  2. Entries like "~/concurve/vignettes/bootstrap.Rmd" were absolute
##     paths. use_build_ignore() expects paths relative to the package
##     root; the absolute forms produced patterns that match nothing.
##
## The function is additive and idempotent: usethis skips entries that
## are already present.
## ---------------------------------------------------------------------

manage_build_ignores <- function() {
  ignore_files <- c(
    # Development and configuration files
    "usethis.R", "dev", "tools", "Manuscripts", "CRAN-RELEASE",
    "CRAN-SUBMISSION", "cran-comments.md", "docs", "examples",
    "pkgdown", "revdep", "Makefile", "README.Rmd", "_pkgdown.yml",
    "codecov.yml", "codemeta.json", ".covrignore", ".travis.yml",
    ".circleci", ".here", ".github", "AGENTS.md",

    # Vignettes excluded from the build, by path relative to the root
    "vignettes/bayes.Rmd", "vignettes/variancecomponents.Rmd",
    "vignettes/casestudies.Rmd", "vignettes/wishlist.Rmd",
    "vignettes/supported.Rmd",

    # Local artifacts and scratch output
    "stata", "SECURITY.md", "CODE_OF_CONDUCT.md", "concurve.pdf",
    ".venv", "rstanlm", "AB4607D1", "shared"

    # DO NOT add "references.bib" or "american-medical-association.csl".
    # The vignettes cite them at build time and they must ship.
  )

  for (file in ignore_files) {
    tryCatch(
      usethis::use_build_ignore(file),
      error = function(e) message("Error ignoring file ", file, ": ", conditionMessage(e))
    )
  }

  invisible(TRUE)
}


## ---------------------------------------------------------------------
## Checking
##
## FIXED 2026-09-04:
##
##  * use_spell_check(), use_cran_comments(), and write_codemeta()
##    overwrite tests/spelling.R, cran-comments.md, and codemeta.json.
##    cran-comments.md in particular gets replaced by the "This is a new
##    release." template, which is wrong -- concurve is a *resubmission*
##    of a package archived in 2022. They are now behind `scaffold`,
##    which defaults to FALSE.
##
##  * The Rd2pdf block called find.package("concurve"), which errors with
##    "invalid 'path' argument" when the package is not installed in the
##    current library. devtools::check(manual = TRUE) builds the manual
##    anyway, so the block is gone.
##
##  * error_on was passed the whole vector c("never", "error", "warning",
##    "note"); it takes ONE value. Now "warning".
##
##  * rcmdcheck() + check_man() + roxygenize() + check() re-ran the same
##    work three times. devtools::check() documents, builds, and runs
##    R CMD check on its own.
## ---------------------------------------------------------------------

comprehensive_package_check <- function(scaffold = FALSE, manual = TRUE) {
  if (isTRUE(scaffold)) {
    message("Regenerating spelling test, cran-comments.md, and codemeta.json.")
    message("This OVERWRITES cran-comments.md -- restore the resubmission text after.")
    usethis::use_spell_check(vignettes = TRUE, lang = "en-US", error = TRUE)
    usethis::use_cran_comments(open = interactive())
    codemetar::write_codemeta()
  }

  devtools::check(
    document  = TRUE,
    cran      = TRUE,
    manual    = manual,
    vignettes = TRUE,
    error_on  = "warning"
  )
}


## ---------------------------------------------------------------------
## pkgdown
##
## FIXED 2026-09-04: this called install.packages("foghorn") as a side
## effect. Installing packages from inside a maintenance function is a
## surprise at best; the queue check is now guarded and optional.
## ---------------------------------------------------------------------

manage_pkgdown_site <- function(check_queue = FALSE) {
  pkgdown::build_site()

  if (isTRUE(check_queue)) {
    if (requireNamespace("foghorn", quietly = TRUE)) {
      # cran_incoming() takes no per-package argument.
      print(foghorn::cran_incoming())
    } else {
      message("Install 'foghorn' to see the CRAN incoming queue.")
    }
  }

  invisible(TRUE)
}


## ---------------------------------------------------------------------
## Deliberately NOT defined: execute_package_workflow()
##
## Chaining these together is what caused the repeated damage. Each
## function has different preconditions and blast radius; run them one at
## a time and inspect `git status` in between. For a routine check, use
## dev_check.R instead.
## ---------------------------------------------------------------------
