# Comprehensive R Package Development Script for concurve

# Load essential development libraries
library(usethis)
library(devtools)
library(roxygen2)
library(rcmdcheck)
library(pkgdown)
library(codemetar)

library(tidyverse)
library(concurve)
library(flextable)
library(boot)
library(bcaboot)

# Package Dependency Management Function
manage_package_dependencies <- function() {
  # Core Imports
  core_imports <- c(
    "lme4", "parallel", "pbmcapply", "boot", "bcaboot",
    "ProfileLikelihood", "ggplot2", "metafor", "dplyr",
    "tidyr", "flextable", "officer", "knitr", "tibble",
    "survival", "survminer", "scales"
  )

  # Development Suggests
  dev_suggests <- c(
    "testthat", "covr", "spelling", "Lock5Data",
    "rmarkdown", "knitr", "roxygen2md"
  )

  # Add Imports
  lapply(core_imports, function(pkg) {
    tryCatch(
      {
        use_package(pkg, "Imports", min_version = NULL)
      },
      error = function(e) {
        message(paste("Error adding import for", pkg, ":", e$message))
      }
    )
  })

  # Add Suggests
  lapply(dev_suggests, function(pkg) {
    tryCatch(
      {
        use_package(pkg, "Suggests", min_version = NULL)
      },
      error = function(e) {
        message(paste("Error adding suggest for", pkg, ":", e$message))
      }
    )
  })
}

# Build Ignore Management Function
manage_build_ignores <- function() {
  ignore_files <- c(
    # Development and Configuration Files
    "usethis.R", "Manuscripts", "CRAN-RELEASE", "cran-comments.md",
    "docs", "examples", "pkgdown", "revdep", "Makefile",
    "README.Rmd", "_pkgdown.yml", "codecov.yml", "codemeta.json",
    ".covrignore", ".travis.yml", ".circleci", ".here", ".github",

    # Documentation and Vignette Related
    "references.bib", "american-medical-association.csl",
    "bayes.Rmd", "variancecomponents.Rmd", "casestudies.Rmd",
    "wishlist.Rmd", "stata", "SECURITY.md", "CODE_OF_CONDUCT.md",
    "concurve.pdf", "~/concurve/vignettes/R&G2020.Rmd",
    "supported.Rmd", "~/concurve/vignettes/bootstrap.Rmd"
  )

  lapply(ignore_files, function(file) {
    tryCatch(
      {
        use_build_ignore(file, escape = FALSE)
      },
      error = function(e) {
        message(paste("Error ignoring file", file, ":", e$message))
      }
    )
  })
}

# Comprehensive Package Check Function
comprehensive_package_check <- function() {
  # Spell Check
  use_spell_check(vignettes = TRUE, lang = "en-US", error = TRUE)

  # Generate CRAN Comments
  use_cran_comments(open = interactive())


  # Generate Codemeta
  codemetar::write_codemeta()

  # Check Manuals
  check_man(pkg = ".")

  # Roxygenize
  roxygenize(
    package.dir = ".",
    roclets = c("collate", "rd"),
    load_code = NULL,
    clean = FALSE
  )

  # Generate PDF Documentation
  pack <- "concurve"
  path <- find.package(pack)
  system(paste(
    shQuote(file.path(R.home("bin"), "R")),
    "CMD", "Rd2pdf", shQuote(path)
  ))

  # Comprehensive R CMD Check
  chk <- rcmdcheck(
    path = ".",
    quiet = FALSE,
    args = character(),
    build_args = character(),
    check_dir = FALSE,
    libpath = .libPaths(),
    repos = getOption("repos"),
    timeout = Inf,
    error_on = c("never", "error", "warning", "note")
  )

  # Display Check Details
  check_details(chk)
  parse_check(chk)

  # Comprehensive Check
  check(
    pkg = ".",
    document = TRUE,
    clean_doc = TRUE,
    manual = TRUE,
    cran = TRUE,
    remote = TRUE,
    incoming = TRUE,
    force_suggests = FALSE,
    run_dont_test = FALSE,
    args = c("--as-cran", "--timings"),
    build_args = c("--compact-vignettes"),
    quiet = FALSE,
    check_dir = tempdir(),
    vignettes = TRUE,
    error_on = c("never", "error", "warning", "note")
  )
}

# Pkgdown Site Management
manage_pkgdown_site <- function() {

  # Rebuild site
  pkgdown::build_site()

  # Preview site
  # shows which CRAN queue your package is sitting in
  install.packages("foghorn")
  foghorn::cran_incoming(pkg = "concurve")}

# Main Execution Function
execute_package_workflow <- function() {
  tryCatch(
    {
      manage_package_dependencies()
      manage_build_ignores()
      comprehensive_package_check()
      manage_pkgdown_site()
    },
    error = function(e) {
      message("Workflow encountered an error: ", e$message)
    }
  )
}

# Run the workflow
execute_package_workflow()
