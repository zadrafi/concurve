## Submission

concurve 3.0.4 is an update to 3.0.3, which returned the package to CRAN
after it was archived on 2022-10-03 ("check issues were not corrected
despite reminders"). The check problems behind that archival — a
`LazyData` field with no `data/` directory, an invalid image width in
the HTML manual, and an undeclared `rlang` in Rd cross-references — are
all resolved; the last of them, the stray `LazyData` field, is removed
in this release.

## What is new in 3.0.4

- **`curve_stan()`, `curve_stan_fit()`, `concurve_stan_file()`** build
  consonance functions from Monte Carlo draws of a confidence
  distribution. See the note on `inst/stan/` below.
- **Eleven defunct functions are now exported.** They never were, so old
  code calling `plotpint()` and friends raised "could not find function"
  instead of the message naming the current replacement. They now signal
  a `defunctError` that names the function to use.
- **The examples for the eight most-used functions now run** rather than
  sitting in `\dontrun{}`, so `R CMD check` executes them. This
  uncovered two example blocks that had never been valid.
- Bug fixes in `curve_lik_glm()` (dispersion handling for families with
  a free dispersion parameter, and convergence in the tails of
  inverse-link models) and `curve_rev()` (a stray debugging `print()`).

## Note on `inst/stan/`

The three `.stan` files in `inst/stan/` are shipped as plain text only:
the package has no `src/`, `configure`, `LinkingTo`, or
`SystemRequirements`, does not use **rstantools**, and compiles nothing
at install time. `curve_stan_fit()` compiles a model on demand with
**rstan**, which is in `Suggests` only and guarded by
`requireNamespace()`; the package installs, loads, and passes its tests
and examples without rstan. Those tests are skipped when rstan is
absent, and the `stanc()` parse test is additionally `skip_on_cran()`.

## Expected NOTEs

- **Possibly misspelled words in DESCRIPTION** (Schweder, Hjort, NL,
  Rafi, Surprisal). These are not misspellings: "Schweder T, Hjort NL"
  are cited authors, Rafi is the maintainer's surname, and "surprisal"
  is a standard information-theoretic term central to this package.

- **Possibly invalid URLs at `stat.lesslikely.com`.** The incoming
  pretest for 3.0.3 reported that this host could not be resolved. The
  site is the package's pkgdown documentation, served by GitHub Pages;
  it resolves (CNAME to `zadrafi.github.io`) and returns HTTP 200 when
  checked from outside the pretest machine. We believe this is a DNS
  limitation of the check environment rather than a broken link, but we
  are happy to substitute different URLs if CRAN prefers.

## R CMD check results

0 errors, 0 warnings, 0 notes locally, other than the expected items
above.

HTML validation was run with HTML Tidy 5.8.0. Note that on macOS the
system `/usr/bin/tidy` is a 2006 build, which causes `R CMD check` to
*skip* HTML validation and emit a NOTE saying so; with a current Tidy on
`PATH` the manual validates cleanly.

## Test environments

- local: macOS 15 (arm64), R 4.6.1, with HTML Tidy 5.8.0
- GitHub Actions: ubuntu-latest (r-devel, release, oldrel-1),
  windows-latest (release), macos-latest (release) — all passing
- win-builder: r-devel and r-release
