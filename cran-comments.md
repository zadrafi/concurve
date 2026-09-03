## Resubmission

This package was previously on CRAN and was archived on 2022-10-03
("check issues were not corrected despite reminders"). This is a
resubmission (3.0.3) that corrects the problems reported by the
automated incoming pretest of the 3.0.2 submission (1 ERROR, 1 WARNING,
1 NOTE):

- **PDF manual ERROR/WARNING ("Illegal unit of measure (pt inserted)").**
  The package help page (`man/concurve-package.Rd`, generated from
  `R/concurve-package.R`) carried a hand-maintained `\tabular{}` block
  containing `\figure{logo.png}{options: width="50"}`. That directive
  emits `\includegraphics[width="50"]` into the LaTeX manual, i.e. a
  width with no valid LaTeX unit, which broke the PDF build. The block
  also duplicated the package version, date, and license (with a
  "GLP-3" typo). The entire block has been removed; version, date, and
  license are taken from `DESCRIPTION`. The reference manual now builds
  cleanly.

- **Tarball size NOTE (~13 MB).** The source tarball has been reduced to
  under 5 MB by removing image assets that were not referenced by any
  help page, vignette, or the README: a 7 MB animated logo
  (`man/figures/HomeLogo.gif`), several large curve PDFs/SVGs, stray
  files (`.DS_Store`, an empty `checkmark.png`), and five uncited SVGs
  in `vignettes/`.

- **Unused Imports NOTE.** `survival`, `survminer`, `ProfileLikelihood`,
  and `officer` were declared in `Imports` but are only used in vignettes,
  examples, and tests; they are now in `Suggests`.

## Expected NOTEs

- **`checking CRAN incoming feasibility` ... New submission / Package was
  archived on CRAN.** Expected: this is a resubmission of a previously
  archived package.

- **Possibly misspelled words in DESCRIPTION** (Schweder, Hjort, NL,
  Rafi, Surprisal). These are not misspellings: "Schweder T, Hjort NL"
  are cited authors, Rafi is the maintainer's surname, and "surprisal"
  is a standard information-theoretic term (the S-value / surprisal is
  central to this package).

## R CMD check results

Local `R CMD check --as-cran` on the current source completes with
0 errors and 0 warnings. The only NOTEs are the two expected items
above, which are raised only on CRAN incoming infrastructure.

## Test environments

- local: macOS, R 4.6.1
- GitHub Actions: ubuntu-latest (release, devel, oldrel-1),
  windows-latest (release), macos-latest (release)
