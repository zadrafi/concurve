## Resubmission

This package was previously on CRAN and was archived on 2022-10-03
("check issues were not corrected despite reminders"). This is a
resubmission. A full `R CMD check --as-cran` run against the current
source now completes with 0 errors and 0 warnings; the maintainer is
reachable at the address in DESCRIPTION and will address any
outstanding check issues promptly if this submission has problems.

## R CMD check results

0 errors | 0 warnings | 2 notes

* `checking CRAN incoming feasibility`: reports the prior archival
  (expected for a resubmission) and a handful of documented URLs that
  return redirects (HTTP 301) or that could not be resolved from this
  build environment's network. All were checked manually and resolve
  correctly.
* `checking HTML version of manual`: skipped locally because the
  `tidy` binary available in the build environment is older than
  HTML Tidy requires; this is a limitation of the local check
  environment, not of the package.

## Test environments

* local: macOS, R 4.6.1
* GitHub Actions: ubuntu-latest (release, devel, oldrel-1),
  windows-latest (release), macos-latest (release)
