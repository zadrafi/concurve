# Changelog

## concurve 2.7.7

CRAN release: 2020-10-12

### Major changes

- `log` option added to `curve_gen()` to exponentiate the coefficients.
  - Thanks to [Isabella Ghement](https://twitter.com/IsabellaGhement)
    for finding the issue and notifying us about it.
- customization option for `title` fully added to
  [`ggcurve()`](reference/ggcurve.md)

## concurve 2.7.5

### Major changes

- Substantial revisions to the documents and website
- Now, far fewer dependencies to avoid potential conflicts in the future
- Included lots of resources for individuals to better learn the
  concepts
- Far better improvement in documentation and in continous integration
- Added several messages that accompany functions from the program to
  aid researchers
- Larger and improved tutorial on how to construct these functions using
  Stata

## concurve 2.7.0

### Major changes

- `curve_gen()` now includes options to adjust CIs and P-values for
  multiple comparisons.
- `curve_gen()` can now accept inputs from the `rms` package’s `ols()`
  function.
  - This can be done using the same `lm` option that is typically used.
- disabled likelihood function computations for
  [`curve_rev()`](reference/curve_rev.md) for continuous variables due
  to instability.

### Minor changes

- improvements to documentation all around.
- new article on estimating variance components using `lme4`.
- [new
  article](https://data.lesslikely.com/concurve/articles/tables.html) on
  using [`curve_table()`](reference/curve_table.md) to produce outputs
  from the functions.
- updates to article on troubleshooting and parallel computing.

## concurve 2.6.0

### Major changes

- An error in [`curve_rev()`](reference/curve_rev.md) was fixed where
  for the default measure, the computations were calculated as if
  measure was set to ratio.
  - Thank you to [Aaron Caldwell](https://twitter.com/ExPhysStudent) for
    his generous help in fixing this bug.
- [`curve_rev()`](reference/curve_rev.md) can now take summary
  statistics such as the point estimate and it’s standard error to back
  calculate the function.
  - Also thanks to [Aaron Caldwell](https://twitter.com/ExPhysStudent)
    for this added feature.
- [`curve_boot()`](reference/curve_boot.md) now takes an option allowing
  users to specify the number of cores to be used.
- Graphing functions such as [`ggcurve()`](reference/ggcurve.md) and
  [`plot_compare()`](reference/plot_compare.md) now have the options to
  customize the second y-axis and also change the color of the outline
  of the functions.
  - Thus, the argument `yaxis` has been replaced by `yaxis1` and
    `yaxis2`.
  - For [`plot_compare()`](reference/plot_compare.md) color has been
    replaced by `color1` and `color2`

## concurve 2.5.0

CRAN release: 2020-04-20

### Major changes

- [`curve_meta()`](reference/curve_meta.md) can now handle complex data
  structures from `metafor` with clustered data.
- `curve_gen()` can now handle ANOVAs and robust linear regressions from
  the `MASS` package.
- `concurve` is now intergrated with the `cowplot` package for easier
  graphing.
- removed
  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
  integration due to breakage in code.
- [`expand_scale()`](https://ggplot2.tidyverse.org/reference/expansion.html)
  has been replaced with
  [`expansion()`](https://ggplot2.tidyverse.org/reference/expansion.html)
  within the inner workings of [`ggcurve()`](reference/ggcurve.md) and
  [`plot_compare()`](reference/plot_compare.md) due to the former
  function
  ([`expand_scale()`](https://ggplot2.tidyverse.org/reference/expansion.html))
  being deprecated.

## concurve 2.4.1

### Major changes

- New function [`curve_lmer()`](reference/curve_lmer.md) for
  mixed-effects models
- Fixed a bug where [`curve_meta()`](reference/curve_meta.md) could not
  utilize
  [`rma.mh()`](https://wviechtb.github.io/metafor/reference/rma.mh.html)
  or
  [`rma.peto()`](https://wviechtb.github.io/metafor/reference/rma.peto.html)
  from `metafor`.

## concurve 2.4.0

### Major changes

- [`curve_boot()`](reference/curve_boot.md) can utilize parametric Bca
  bootstrap methods to compute functions.
- Corrected error where order of labels in columns for
  [`curve_boot()`](reference/curve_boot.md) tables was incorrect.
- Corrected error where order of labels in columns for
  [`curve_meta()`](reference/curve_meta.md) tables was incorrect.
- Set minimum version of `R` to 3.5.0.
- included `install.packages("concurve", dep = TRUE)` as solution to
  installation problems for some individuals.
- Removed `MASS`, `compiler`, and `Rlang` from `DESCRIPTION` `IMPORTS`,
  since these weren’t used.
- Wrote new unit tests examing the class of each of the objects created
  from the functions.

### Minor changes

- [`ggcurve()`](reference/ggcurve.md) theme has been changed from
  [`theme_bw()`](https://ggplot2.tidyverse.org/reference/ggtheme.html)
  to
  [`theme_minimal()`](https://ggplot2.tidyverse.org/reference/ggtheme.html).
- Several new examples in the “[Examples in
  R](https://data.lesslikely.com/concurve/articles/examples.html)”
  article.

## concurve 2.3.0

CRAN release: 2019-12-04

### Major changes

- [`ggconcurve()`](reference/defunct.md) is now
  [`ggcurve()`](reference/ggcurve.md).
- [`ggcurve()`](reference/ggcurve.md) plots confidence (consonance)
  distributions, densities, likelihood, and deviance functions.
- `plot_curve()` is now deprecated. Please use
  [`ggcurve()`](reference/ggcurve.md) instead.
- [`curve_compare()`](reference/curve_compare.md) compares two functions
  and calculates the area between the curve.
- [`plot_compare()`](reference/plot_compare.md)allows two separate
  functions to be plotted and compared simultaneously.
- [`curve_table()`](reference/curve_table.md) produces publication-ready
  tables of relevant statistics.
- [`curve_boot()`](reference/curve_boot.md) uses bootstrapping to
  approximate the consonance functions via the
  [`boot`](https://cran.r-project.org/package=boot) and
  [`bcaboot`](https://cran.r-project.org/package=bcaboot) packages.
- [`curve_lik()`](reference/curve_lik.md) produces likelihood functions
  by transforming the objects from the
  [`ProfileLikelihood`](https://cran.r-project.org/package=ProfileLikelihood)
  package.

### Minor changes

- All functions now provide progress on how long it will take to
  complete the task.
- Interval widths are now provided as measures of precision.

## concurve 2.1.0

CRAN release: 2019-09-19

### Major changes

- [`ggconcurve()`](reference/defunct.md) now plots both the P-values and
  CI level using both y-axes when the type = “consonance”. Previously,
  this was only possible via [`plot_concurve()`](reference/defunct.md)
  (which uses base R graphics) because `ggplot2` had a bug in its last
  few versions, which inhibited proper transformations in the y-axis.

## concurve 2.0.1

### Major changes

- [`plot_concurve()`](reference/defunct.md) now has “measure” as an item
  which allows for ratio measures to be logarithmically scaled on the x
  axis. There are two options, “default”, which is set as the default
  option and is for mean differences, and “ratio”, which will result in
  the axis being logarithmically scaled.
- [`plot_concurve()`](reference/defunct.md) also now has a “fill” option
  which will allow users to choose the color of the plot.

## concurve 2.0

CRAN release: 2019-07-10

### Major changes

- The [`plotpint()`](reference/defunct.md) function which plotted
  consonance functions has been repackaged into
  [`ggconcurve()`](reference/defunct.md).
- The [`plotsint()`](reference/defunct.md) function which plotted
  surprisal functions has been repackaged into
  [`ggconcurve()`](reference/defunct.md).
- Functions can now also be plotted with base R via the
  [`plot_concurve()`](reference/defunct.md) function.
- Consonance functions can be plotted as a pyramid (right side up) or
  inverted (upside down) via the “position” item in
  [`ggconcurve()`](reference/defunct.md).
- Null values (for means & ratios) can be plotted via the
  [`ggconcurve()`](reference/defunct.md) function to show how much of
  the interval surrounds it.
- Log transformations included in all the plotting functions for ratio
  measures.
- Parallel programming has now been implemented into the computations
  via the [`mclapply()`](https://rdrr.io/r/parallel/mclapply.html)
  function from the *parallel* package.

## concurve 1.08

### Major changes

- Can produce consonance and surprisal functions for correlations via
  the [`corrintervals()`](reference/defunct.md) function.
- Now able to construct consonance and surprisal functions from the
  point estiate, and confidence limits via the
  [`rev_eng()`](reference/defunct.md) function.
- Graphs produced via the [`plotpint()`](reference/defunct.md) or
  [`plotsint()`](reference/defunct.md) function now able to take custom
  titles, subtitles, x-axis titles, and captions.

## concurve 1.07

### Major changes

- Can now produce consonance and surprisal functions for survival data
  produced with the `survival` package.

## concurve 1.06

### Major changes

- Now contains
  [documentation](https://data.lesslikely.com/concurve/articles/stata.html)
  for producing interval functons in `Stata`.

### Minor changes

- Default plots now contain grids, title, subtitle, and a caption.
- Updated figures in README and the ‘Examples in R’ vignette/article.
