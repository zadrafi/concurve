## Resubmission

This package was previously on CRAN and was archived on 2022-10-03
("check issues were not corrected despite reminders"). This is a
resubmission (3.0.2) that addresses the automated incoming-check
feedback on the 3.0.1 submission of 2026-08-25:

- The remaining shortDOI link (doi.org/10/gg9s2f) now uses its canonical
  DOI (doi.org/10.1111/insr.12007), and the defunct
  tidyeval.tidyverse.org URL in man/tidyeval.Rd has been replaced with
  <https://rlang.r-lib.org/>.
- README.md: the redirected lifecycle badge URL, a malformed
  double-slash URL, a relative file URI, and a local file path have all
  been replaced with canonical absolute URLs.
- The invalid `width="50px"` image attribute in concurve-package.Rd is
  now `width="50"`.
- The semanticscholar.org citation URL (status 202) has been removed
  from the vignette references.
- The stat.lesslikely.com URLs flagged as unresolvable on the Debian
  pretest machine belong to this package's own documentation website,
  which is online and resolves normally (the Windows pretest of the same
  tarball resolved them without error); this appears to have been a
  transient DNS failure on the check machine.
- The words flagged as possibly misspelled in DESCRIPTION are proper
  names and technical terms: Schweder, Hjort, and NL are the cited
  authors "Schweder T, Hjort NL"; Rafi is the maintainer's surname;
  Surprisal is a standard information-theoretic term.

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
