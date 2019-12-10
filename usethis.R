library(usethis)
library(roxygen2)
library(roxygen2md)
library(devtools)
library(concurve)

# Importing other packages

use_package("parallel", "Imports", min_version = NULL)
use_package("pbmcapply", "Imports", min_version = NULL)
use_package("boot", "Imports", min_version = NULL)
use_package("bcaboot", "Imports", min_version = NULL)
use_package("ProfileLikelihood", "Imports", min_version = NULL)
use_package("ggplot2", "Imports", min_version = NULL)
use_package("metafor", "Imports", min_version = NULL)
use_package("dplyr", "Imports", min_version = NULL)
use_package("tidyr", "Imports", min_version = NULL)
use_package("flextable", "Imports", min_version = NULL)
use_package("officer", "Imports", min_version = NULL)
use_package("knitr", "Imports", min_version = NULL)
use_package("tibble", "Imports", min_version = NULL)
use_package("survival", "Imports", min_version = NULL)
use_package("survminer", "Imports", min_version = NULL)
use_package("scales", "Imports", min_version = NULL)

# Suggest other packages
use_package("testthat", "Suggests", min_version = NULL)
use_package("covr", "Suggests", min_version = NULL)
use_package("spelling", "Suggests", min_version = NULL)
use_package("Lock5Data", "Suggests", min_version = NULL)

# Other helper functions

use_build_ignore("usethis.R", escape = TRUE)
use_build_ignore("references.bib", escape = TRUE)
use_build_ignore("american-medical-association.csl", escape = TRUE)

use_spell_check(vignettes = TRUE, lang = "en-US", error = FALSE)
use_cran_comments(open = interactive())
use_tidy_style()
use_revdep()
codemetar::write_codemeta()

build(pkg = ".", path = NULL, binary = FALSE, vignettes = TRUE,
      manual = TRUE, args = NULL, quiet = FALSE)

devtools::document()
roxygen2md(scope = "full")
check_man(pkg = ".")


check(
  pkg = ".", document = TRUE, build_args = NULL,
  manual = TRUE, cran = TRUE, remote = TRUE, incoming = TRUE,
  force_suggests = TRUE, run_dont_test = TRUE, args = "--timings",
  env_vars = NULL, quiet = FALSE, check_dir = tempdir(),
  cleanup = TRUE, error_on = c("never", "error", "warning", "note")
)


check_rhub(
  pkg = ".", platforms = NULL, email = NULL,
  interactive = TRUE, build_args = NULL
)

rhub::check_for_cran(".")
