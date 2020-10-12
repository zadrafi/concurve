library(usethis)
library(roxygen2)
library(roxygen2md)
library(devtools)
library(concurve)
library(rcmdcheck)
library(pkgdown)
library(revdepcheck)
library(codemetar)

# Importing other packages

use_package("lme4", "Imports", min_version = NULL)
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
use_build_ignore("Manuscripts", escape = TRUE)
use_build_ignore("CRAN-RELEASE", escape = TRUE)
use_build_ignore("cran-comments.md", escape = TRUE)
use_build_ignore("docs", escape = TRUE)
use_build_ignore("examples", escape = TRUE)
use_build_ignore("pkgdown", escape = TRUE)
use_build_ignore("revdep", escape = TRUE)
use_build_ignore("Makefile", escape = TRUE)
use_build_ignore("README.Rmd", escape = TRUE)
use_build_ignore("_pkgdown.yml", escape = TRUE)
use_build_ignore("codecov.yml", escape = TRUE)
use_build_ignore("codemeta.json", escape = TRUE)
use_build_ignore(".covrignore", escape = TRUE)
use_build_ignore(".travis.yml", escape = TRUE)
use_build_ignore(".circleci", escape = TRUE)
use_build_ignore(".here", escape = TRUE)
use_build_ignore(".github", escape = TRUE)
use_build_ignore("references.bib", escape = TRUE)
use_build_ignore("american-medical-association.csl", escape = TRUE)
use_build_ignore("bayes.Rmd", escape = TRUE)
use_build_ignore("variancecomponents.Rmd", escape = TRUE)
use_build_ignore("casestudies.Rmd", escape = TRUE)
use_build_ignore("wishlist.Rmd", escape = TRUE)
use_build_ignore("stata", escape = TRUE)
use_build_ignore("SECURITY.md", escape = TRUE)
use_build_ignore("CODE_OF_CONDUCT.md", escape = TRUE)
use_build_ignore("concurve.pdf", escape = TRUE)
use_build_ignore("~/concurve/vignettes/R&G2020.Rmd", escape = TRUE)
use_build_ignore("supported.Rmd", escape = TRUE)
use_build_ignore("~/concurve/vignettes/supported.Rmd", escape = TRUE)

use_spell_check(vignettes = TRUE, lang = "en-US", error = FALSE)
use_cran_comments(open = interactive())
use_tidy_style()
use_revdep()
revdepcheck::revdep_check(num_workers = 4)
codemetar::write_codemeta()

revdepcheck::revdep_reset()

check_man(pkg = ".")
roxygenize(package.dir = ".", roclets = c("collate",  "rd"), load_code = NULL, clean = TRUE)




pack <- "concurve"
path <- find.package(pack)
system(paste(shQuote(file.path(R.home("bin"), "R")),
             "CMD", "Rd2pdf", shQuote(path)))

rcmdcheck()

chk <- rcmdcheck(path = ".", quiet = FALSE, args = character(),
          build_args = character(), check_dir = NULL, libpath = .libPaths(),
          repos = getOption("repos"), timeout = Inf, error_on = c("never",
                                                                  "error", "warning", "note"))

check_details(chk)[]

parse_check(chk)


check(
  pkg = ".", document = NA, clean_doc = TRUE, 
  manual = TRUE, cran = TRUE, remote = TRUE, incoming = TRUE,
  force_suggests = FALSE, run_dont_test = FALSE, args = c('--as-cran','--timings'),
  build_args = c('--compact-vignettes'),
  quiet = FALSE, check_dir = tempdir(),
  vignettes = TRUE, error_on = c("never", "error", "warning", "note")
)


pkgdown::clean_site()
pkgdown::build_site()

