# concurve, 2026-09-04 — record of the day

39 commits across two branches. Written at the end of the session; the
correction in the last section matters more than the list.

## Where things stand

| | |
|----|----|
| CRAN holds | `concurve_3.0.3.tar.gz`, built from `d543516`, submitted 2026-09-01, sitting in `incoming/newbies/` awaiting manual review |
| `master` | 3.0.3 in DESCRIPTION, but **20 commits past the submitted tarball** |
| `release/3.0.4` | 3.0.4, 19 further commits, draft PR #60 |
| Exports | 48 → 62 (all on `master`) |
| Check | `R CMD check --as-cran`, 1 expected NOTE (archival), with HTML Tidy 5.8.0 |
| CI | green on ubuntu devel/release/oldrel-1, windows, macos |
| Blocked on | the withdrawal email, which only the maintainer can send |

## The bug that drove the day

`curve_lik_glm()`, new in 3.0.3 and never on CRAN before, profiled the
unscaled deviance for families with a free dispersion parameter. Support
intervals came out wrong by `1/sqrt(dispersion)` — so they **depended on
the units of the response**. Measured against `confint()`:

| model | dispersion | interval width |
|----|----|----|
| gaussian, residual SD 0.4 | 0.21 | 219% of correct |
| gaussian, residual SD 5.0 | 27.08 | **19%** of correct |
| Gamma(log), shape 2 | 0.52 | 138% of correct |
| binomial | 1.00 | correct |

`gaussian` is `glm()`'s default family, and the 19% case understates
uncertainty. Fixed on `master` (`1f2509f`, before the branch). A
withdrawal request for 3.0.3 is drafted at `dev/cran-withdraw-3.0.3.md`
and `.eml`.

## Work on `master` (20 commits)

- **Stan support as Suggests-only.** `curve_stan()`, `curve_stan_fit()`,
  `concurve_stan_file()`; `.stan` files ship as plain text, nothing
  compiles at install. Deliberately *not* rstantools — an earlier
  `use_rstan()` run had added Rcpp/LinkingTo/`src/` scaffolding, which
  was reverted. Validated the generalized fiducial distribution against
  the exact Student-t.
- **Eleven defunct stubs exported.** They had never been exported in any
  NAMESPACE in the repository's history, so `plotpint()` gave "could not
  find function" instead of the redirect. `defunct.R` rewritten with
  roxygen and `.Defunct()`; `man/defunct.Rd` had been hand-maintained
  since 2019 and carried an alias for `likintervals`, which existed
  nowhere.
- **`usethis.R` neutralised.** Sourcing it kept reverting DESCRIPTION,
  `.Rbuildignore`, and cran-comments.md — twice in this session. Moved to
  `dev/`, definitions-only, destructive defaults corrected. Nothing
  automated was ever running it; it was always a console `source()`.
- **Examples made runnable** for the eight core functions, which exposed
  two blocks that had never worked: `plot_multi()` documented
  `curve_from_se(point =, se =, df =)`, arguments it does not have.
- `plot_multi()` moved to the `.data` pronoun. Most `globalVariables()`
  calls in the package turned out to be dead code, not NSE suppression.
- `LazyData: true` removed — one of the three 2022 archival NOTEs, still
  present.
- Tarball 13 MB → 3.4 MB.

## Work on `release/3.0.4` (19 commits)

- **Version bump, and a NEWS correction.** The 3.0.3 heading had
  accumulated work done *after* the submission. The submitted tarball
  contains only the LaTeX fix, the size reduction, and the Suggests
  reclassification; the `curve_lik_glm()` and `curve_rev()` fixes landed
  a day later and are **not in CRAN's copy**.
- **`deviancestat` standardised** to `D = -2 log(L/Lmax)` across all five
  constructors. `curve_rev()` was right and four others reported half the
  value. Breaking for `ggcurve(type = "d")` and `plot_compare(type = "d")`,
  whose y-values double. `plot_compare()` had always labelled that axis
  `2ln(MLR)`, so this restores the documented intent.
- **Three metadata tables in `supported.Rmd` now generated, not restated.**
  They had drifted: Imports listed four packages that had moved to
  Suggests, the version table stopped at 3.0.0 and linked to four dead
  anchors, and "New Functions" named 10 of 27.
- README article index: five existing vignettes were missing.
- `tools/cran-queue.sh` plus an optional launchd agent that notifies only
  on change.

## The correction

I spent much of the session describing `master` as frozen at the tarball
CRAN is reviewing. **That is wrong.** `master` is 20 commits past
`d543516`, including the Stan feature and 14 new exports. What is frozen
is the *version number*, not the tree.

This matters for the release decision: merging `release/3.0.4` does not
add the Stan work to a pristine 3.0.3 — that work is already on `master`
and will ship in whatever goes out next, whether that is 3.0.4 or a
re-cut 3.0.3.

## Recurring theme

Nearly every defect found today was **silence read as success**:

- `\dontrun{}` hid two examples that could never have run;
- `skip_on_cran = TRUE` made a *skipped* spelling test print `OK`, so a
  British spelling reached CI;
- a 2006 HTML Tidy made *skipped* HTML validation look like a pass — and
  HTML validation was one of the three 2022 archival NOTEs;
- `installed.packages()[pkgs, ]` erroring on a missing Suggests, having
  been papered over by adding two genuine typos to `inst/WORDLIST`;
- and my own `tools/cran-queue.sh`, where an unreachable CRAN would have
  printed as "nothing queued" — advice to submit on top of a pending
  submission.

## Still outstanding

1. **Send `dev/cran-withdraw-3.0.3.eml`.** Only the maintainer can; CRAN
   authenticates on the From: header.
2. Two unsaved `Untitled` buffers in RStudio, contents unknown.
3. Four stale editor tabs pointing at deleted files.
4. `variancecomponents.Rmd` deferred, not ruled out — measured at 53 s,
   41 s of it a single `curve_lmer(method = "profile")` call.
