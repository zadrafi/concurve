## Resubmission

This package was previously on CRAN and was archived on 2022-10-03
("check issues were not corrected despite reminders"). This is a
resubmission (3.0.1) that additionally addresses the automated
incoming-check feedback on the 3.0.0 submission of 2026-08-25:

- All flagged redirected/shortDOI URLs have been replaced with their
  canonical targets (doi.org/10.1214/ss/1028905930,
  doi.org/10.1214/aoms/1177706815, mc-stan.org/rstanarm/,
  tidyeval.tidyverse.org/).
- "Powerpoint" in DESCRIPTION corrected to "PowerPoint".
- The remaining words flagged as possibly misspelled in DESCRIPTION are
  proper names and technical terms: Schweder, Hjort, and NL are the
  cited authors "Schweder T, Hjort NL"; Rafi is the maintainer's
  surname; Surprisal is a standard information-theoretic term;
  PowerPoint is the Microsoft product.

A full `R CMD check --as-cran` run against the current source, including
vignette rebuilding, completes locally with 0 errors, 0 warnings, and 0
notes. The maintainer will address any outstanding check issues promptly
if this submission has problems.

## R CMD check results

0 errors \| 0 warnings \| 0 notes

- `checking CRAN incoming feasibility` (run only on CRAN infrastructure,
  not in local checks) is expected to report the prior archival; this is
  expected for a resubmission.

## Test environments

- local: macOS, R 4.6.1
- GitHub Actions: ubuntu-latest (release, devel, oldrel-1),
  windows-latest (release), macos-latest (release)
