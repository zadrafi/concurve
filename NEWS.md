# concurve 3.0.3

## New features

- Consonance functions from Monte Carlo confidence distributions. The new
  `curve_stan()` builds a standard `concurve` object from draws of a
  confidence distribution -- for example a generalized fiducial
  distribution sampled with Stan, or a bootstrap distribution -- by reading
  the interval limits at every level off the empirical quantiles. The
  output works with `ggcurve()`, `plot_compare()`, and `curve_table()`
  unchanged, and requires no Stan installation.
- `curve_stan_fit()` compiles and samples a Stan program with **rstan**,
  extracts one scalar parameter, and passes the draws to `curve_stan()`.
  Compiled models are cached for the session. **rstan** is a `Suggests`
  dependency only: the package installs and works without it, and the
  function stops with an informative message if it is unavailable.
- `concurve_stan_file()` locates three Stan programs shipped as plain text
  in `inst/stan/`: `normal_gfd` (Hannig's generalized fiducial
  distribution for the normal location-scale model, whose marginal for
  the mean is exactly Student-t), `normal_profile` (for profiling over
  the scale with `rstan::optimizing()`), and `normal_mle`. Nothing is
  compiled at install time.
- Note that `curve_compare()` integrates an interpolant of the interval
  endpoints and can fail on Monte Carlo curves; use `plot_compare()` to
  compare `curve_stan()` output graphically.

## Bug fixes

- Fixed a fatal LaTeX error when building the PDF reference manual
  ("Illegal unit of measure (pt inserted)"). The package help page no
  longer carries a hand-maintained `\tabular{}` block containing a
  `\figure{}{options: width="50"}` directive, which produced an
  `\includegraphics` width with no valid LaTeX unit. Package version,
  date, and license are taken from `DESCRIPTION` as usual.

- `curve_lik_glm()` now divides the profile deviance by the model's
  estimated dispersion for families with a free dispersion parameter
  (`gaussian`, `Gamma`, `inverse.gaussian`, `quasi*`), as `confint()` does.
  Previously the curve for these families was too flat by a factor of the
  dispersion, so its support intervals disagreed with the profile
  likelihood CI (e.g. ~50% too wide for a Gamma model with shape 2).
  `binomial` and `poisson` results are unchanged.
- `curve_lik_glm()` starts each constrained refit from the full model's
  fitted means and drops (with a message) grid points where the refit
  does not converge, instead of failing outright far in the tails of an
  inverse-link model.
- `curve_rev()` no longer prints the reconstructed standard error to the
  console for `measure = "ratio"` (a leftover debugging `print()`).

## Minor changes

- Reduced the source tarball from ~13 MB to under 5 MB by removing
  unused image assets from `man/figures/` (a 7 MB animated logo, several
  large curve PDFs/SVGs, and stray files) and uncited SVGs from
  `vignettes/`. None were referenced by any help page, vignette, or
  README.
- `survival`, `survminer`, `ProfileLikelihood`, and `officer` are declared
  in `Suggests` rather than `Imports`: no function in `R/` calls them; they
  are used only in vignettes, examples, and tests (which already guard
  with `requireNamespace()` / `skip_if_not_installed()`). This clears the
  "All declared Imports should be used" NOTE.

# concurve 3.0.2

## Minor changes

- Replaced a remaining shortDOI link and a defunct tidy evaluation URL
  with their canonical targets, per CRAN incoming checks.
- Fixed redirected and malformed links and a local file path in the
  README; removed an unstable citation URL from the vignette
  references.
- Corrected an invalid image width attribute in the package help page.

# concurve 3.0.1

## Minor changes

- Replaced shortDOI links in vignette references with their canonical
  full DOIs, and updated two redirected URLs (rstanarm, tidy evaluation)
  to their canonical forms, per CRAN incoming checks.
- Corrected the spelling of 'PowerPoint' in DESCRIPTION.

# concurve 3.0.0

## Major changes

- New functions for analytic and native likelihood-based inference:
  - `curve_analytic()` computes consonance intervals directly from
    closed-form quantile functions (z, t, Fisher-z correlation,
    chi-squared variance, Wilson-score proportion) instead of
    numerically inverting `confint()` thousands of times, and is
    typically orders of magnitude faster.
  - `curve_region()` computes the confidence-distribution probability
    that a parameter lies in an arbitrary region, from any `concurve`
    intervals data frame, along with the counternull value for a
    supplied null.
  - `construct_likelihood()` builds likelihood, log-likelihood, score,
    information, and profile-likelihood functions directly from
    `lm()`/`glm()` objects (or from scratch), with `coef()`, `vcov()`,
    `logLik()`, `confint()`, `summary()`, and `plot()` methods.
  - `as_curve_lik()`, `curve_lik_glm()`, and `curve_lik_exact()`
    construct native likelihood functions -- from any grid of parameter
    values and log-likelihood, by direct profiling of a model
    coefficient, or exactly for common designs (proportions, odds
    ratios, rate ratios, means, variances, correlations) -- with no
    dependency beyond base R and `stats`.
  - `curve_support()` computes likelihood/support intervals at arbitrary
    relative-likelihood cutoffs.
  - `ggplot_likelihood()`, `plotly_likelihood()`,
    `plot_all_parameters()`, `plot_ci_levels()`, and
    `plot_profile_vs_wald()` add ggplot2 and interactive plotly output,
    multi-parameter panels, and profile-vs-Wald comparison plots for
    `construct_likelihood()` objects.
  - `curve_wrap()` is a generic wrapper that constructs consonance
    functions from any function that produces confidence intervals.
  - `curve_from_ratio()` and `curve_from_se()` construct consonance
    functions from a published ratio estimate, or from a point estimate
    and its standard error.
  - `curve_overlap()` quantifies the area of overlap between two
    consonance functions.
  - `curve_summary()` summarizes a consonance function at a set of
    confidence levels.
  - `plot_multi()` overlays consonance or surprisal functions from
    several analyses on one plot.
- Four new articles: "Constructing Valid Likelihood Functions",
  "Likelihoods with Existing R Tools", "Correlation: Likelihood and
  P-value Functions", and "Count and Rate Models: Likelihood and P-value
  Functions".
- Removed the `pbmcapply` dependency, replaced with base R.

## Bug fixes

- `curve_gen()` was only ever defined when installed on Windows or
  macOS, due to a check of the installing machine's OS; on Linux the
  function did not exist at all and the package could not be installed.
  It is now a single function that dispatches at call time instead.
- `plot_compare()` was missing its `@export` tag and so was not actually
  callable after a normal installation, despite being documented and
  used in several articles.
- Several functions called functions from `boot`, `dplyr`, `tibble`,
  `lme4`, `metafor`, `flextable`, `ggplot2`, and `scales` without
  namespace-qualifying them; depending on which packages happened to
  already be loaded in a session, these could fail with "could not find
  function" after a clean install. All such calls are now
  namespace-qualified.
- `rlang` and `numDeriv` were used internally but not declared as
  dependencies; `plotly` is now declared as a Suggested package.

## Minor changes

- Fixed several broken or misplaced articles (including one that had
  ended up outside the package entirely) and cleaned up missing alt-text
  on images throughout the documentation site for accessibility; the
  "Supported Versions" table now indicates support status with text
  instead of images.
- Modernized the continuous-integration workflows and fixed the
  configuration that had been preventing the documentation site from
  deploying.

# concurve 2.7.7

## Major changes

- `log` option added to `curve_gen()` to exponentiate the coefficients.
  - Thanks to [Isabella Ghement](https://twitter.com/IsabellaGhement)
    for finding the issue and notifying us about it.
- customization option for `title` fully added to `ggcurve()`

# concurve 2.7.5

## Major changes

- Substantial revisions to the documents and website
- Now, far fewer dependencies to avoid potential conflicts in the future
- Included lots of resources for individuals to better learn the
  concepts
- Far better improvement in documentation and in continous integration
- Added several messages that accompany functions from the program to
  aid researchers
- Larger and improved tutorial on how to construct these functions using
  Stata

# concurve 2.7.0

## Major changes

- `curve_gen()` now includes options to adjust CIs and P-values for
  multiple comparisons.
- `curve_gen()` can now accept inputs from the `rms` package's `ols()`
  function.
  - This can be done using the same `lm` option that is typically used.
- disabled likelihood function computations for `curve_rev()` for
  continuous variables due to instability.

## Minor changes

- improvements to documentation all around.
- new article on estimating variance components using `lme4`.
- [new
  article](https://stat.lesslikely.com/concurve/articles/tables.html) on
  using `curve_table()` to produce outputs from the functions.
- updates to article on troubleshooting and parallel computing.

# concurve 2.6.0

## Major changes

- An error in `curve_rev()` was fixed where for the default measure, the
  computations were calculated as if measure was set to ratio.
  - Thank you to [Aaron Caldwell](https://twitter.com/ExPhysStudent) for
    his generous help in fixing this bug.
- `curve_rev()` can now take summary statistics such as the point
  estimate and it's standard error to back calculate the function.
  - Also thanks to [Aaron Caldwell](https://twitter.com/ExPhysStudent)
    for this added feature.
- `curve_boot()` now takes an option allowing users to specify the
  number of cores to be used.
- Graphing functions such as `ggcurve()` and `plot_compare()` now have
  the options to customize the second y-axis and also change the color
  of the outline of the functions.
  - Thus, the argument `yaxis` has been replaced by `yaxis1` and
    `yaxis2`.
  - For `plot_compare()` color has been replaced by `color1` and
    `color2`

# concurve 2.5.0

## Major changes

- `curve_meta()` can now handle complex data structures from `metafor`
  with clustered data.
- `curve_gen()` can now handle ANOVAs and robust linear regressions from
  the `MASS` package.
- `concurve` is now intergrated with the `cowplot` package for easier
  graphing.
- removed `tibble::tibble()` integration due to breakage in code.
- `expand_scale()` has been replaced with `expansion()` within the inner
  workings of `ggcurve()` and `plot_compare()` due to the former
  function (`expand_scale()`) being deprecated.

# concurve 2.4.1

## Major changes

- New function `curve_lmer()` for mixed-effects models
- Fixed a bug where `curve_meta()` could not utilize `rma.mh()` or
  `rma.peto()` from `metafor`.

# concurve 2.4.0

## Major changes

- `curve_boot()` can utilize parametric Bca bootstrap methods to compute
  functions.
- Corrected error where order of labels in columns for `curve_boot()`
  tables was incorrect.
- Corrected error where order of labels in columns for `curve_meta()`
  tables was incorrect.
- Set minimum version of `R` to 3.5.0.
- included `install.packages("concurve", dep = TRUE)` as solution to
  installation problems for some individuals.
- Removed `MASS`, `compiler`, and `Rlang` from `DESCRIPTION` `IMPORTS`,
  since these weren't used.
- Wrote new unit tests examing the class of each of the objects created
  from the functions.

## Minor changes

- `ggcurve()` theme has been changed from `theme_bw()` to
  `theme_minimal()`.
- Several new examples in the "[Examples in
  R](https://stat.lesslikely.com/concurve/articles/examples.html)"
  article.

# concurve 2.3.0

## Major changes

- `ggconcurve()` is now `ggcurve()`.
- `ggcurve()` plots confidence (consonance) distributions, densities,
  likelihood, and deviance functions.
- `plot_curve()` is now deprecated. Please use `ggcurve()` instead.
- `curve_compare()` compares two functions and calculates the area
  between the curve.
- `plot_compare()`allows two separate functions to be plotted and
  compared simultaneously.
- `curve_table()` produces publication-ready tables of relevant
  statistics.
- `curve_boot()` uses bootstrapping to approximate the consonance
  functions via the [`boot`](https://cran.r-project.org/package=boot)
  and [`bcaboot`](https://cran.r-project.org/package=bcaboot) packages.
- `curve_lik()` produces likelihood functions by transforming the
  objects from the
  [`ProfileLikelihood`](https://cran.r-project.org/package=ProfileLikelihood)
  package.

## Minor changes

- All functions now provide progress on how long it will take to
  complete the task.
- Interval widths are now provided as measures of precision.

# concurve 2.1.0

## Major changes

- `ggconcurve()` now plots both the P-values and CI level using both
  y-axes when the type = "consonance". Previously, this was only
  possible via `plot_concurve()` (which uses base R graphics) because
  `ggplot2` had a bug in its last few versions, which inhibited proper
  transformations in the y-axis.

# concurve 2.0.1

## Major changes

- `plot_concurve()` now has "measure" as an item which allows for ratio
  measures to be logarithmically scaled on the x axis. There are two
  options, "default", which is set as the default option and is for mean
  differences, and "ratio", which will result in the axis being
  logarithmically scaled.
- `plot_concurve()` also now has a "fill" option which will allow users
  to choose the color of the plot.

# concurve 2.0

## Major changes

- The `plotpint()` function which plotted consonance functions has been
  repackaged into `ggconcurve()`.
- The `plotsint()` function which plotted surprisal functions has been
  repackaged into `ggconcurve()`.
- Functions can now also be plotted with base R via the
  `plot_concurve()` function.
- Consonance functions can be plotted as a pyramid (right side up) or
  inverted (upside down) via the "position" item in `ggconcurve()`.
- Null values (for means & ratios) can be plotted via the `ggconcurve()`
  function to show how much of the interval surrounds it.
- Log transformations included in all the plotting functions for ratio
  measures.
- Parallel programming has now been implemented into the computations
  via the `mclapply()` function from the *parallel* package.

# concurve 1.08

## Major changes

- Can produce consonance and surprisal functions for correlations via
  the `corrintervals()` function.
- Now able to construct consonance and surprisal functions from the
  point estiate, and confidence limits via the `rev_eng()` function.
- Graphs produced via the `plotpint()` or `plotsint()` function now able
  to take custom titles, subtitles, x-axis titles, and captions.

# concurve 1.07

## Major changes

- Can now produce consonance and surprisal functions for survival data
  produced with the `survival` package.

# concurve 1.06

## Major changes

- Now contains
  [documentation](https://stat.lesslikely.com/concurve/articles/stata.html)
  for producing interval functons in `Stata`.

## Minor changes

- Default plots now contain grids, title, subtitle, and a caption.
- Updated figures in README and the 'Examples in R' vignette/article.
