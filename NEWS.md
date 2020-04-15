# concurve 2.5.0

## Major changes 
* `curve_meta()` can now handle complex data structures from `metafor` with clustered data.
* `curve_gen()` can now handle ANOVAs and robust linear regressions from the `MASS` package.
* `concurve` is now intergrated with the `cowplot` package for easier graphing. 
* removed `tibble::tibble()` integration due to breakage in code. 
* `expand_scale()` has been replaced with `expansion()` within the inner workings of `ggcurve()` and `plot_compare()` due to the former function (`expand_scale()`) being deprecated.


# concurve 2.4.1

## Major changes 
* New function `curve_lmer()` for mixed-effects models 
* Fixed a bug where `curve_meta()` could not utilize `rma.mh()` or `rma.peto()` from `metafor`.

# concurve 2.4.0

## Major changes 
* `curve_boot()` can utilize parametric Bca bootstrap methods to compute functions.
* Corrected error where order of labels in columns for `curve_boot()` tables was incorrect.
* Corrected error where order of labels in columns for `curve_meta()` tables was incorrect.
* Set minimum version of `R` to 3.5.0.
* included `install.packages("concurve", dep = TRUE)` as solution to installation problems for some individuals. 
* Removed `MASS`, `compiler`, and `Rlang` from `DESCRIPTION` `IMPORTS`, since these weren't used.
* Wrote new unit tests examing the class of each of the objects created from the functions. 


## Minor changes 
* `ggcurve()` theme has been changed from `theme_bw()` to `theme_minimal()`.
* Several new examples in the "[Examples in R](https://data.lesslikely.com/concurve/articles/examples.html)" article.


# concurve 2.3.0

## Major changes 
* `ggconcurve()` is now `ggcurve()`.
* `ggcurve()` plots confidence (consonance) distributions, densities, likelihood, and deviance functions. 
* `plot_curve()` is now deprecated. Please use `ggcurve()` instead. 
* `curve_compare()` compares two functions and calculates the area between the curve. 
* `plot_compare() `allows two separate functions to be plotted and compared simultaneously.
* `curve_table()` produces publication-ready tables of relevant statistics.
* `curve_boot()` uses bootstrapping to approximate the consonance functions via the [`boot`](https://cran.r-project.org/package=boot) and [`bcaboot`](https://cran.r-project.org/package=bcaboot) packages. 
* `curve_lik()` produces likelihood functions by transforming the objects from the [`ProfileLikelihood`](https://cran.r-project.org/package=ProfileLikelihood) package.

## Minor changes 

* All functions now provide progress on how long it will take to complete the task.
* Interval widths are now provided as measures of precision. 

# concurve 2.1.0

## Major changes 
* `ggconcurve()` now plots both the P-values and CI level using both y-axes when the type = "consonance". Previously, this was only possible via `plot_concurve()` (which uses base R graphics) because `ggplot2` had a bug in its last few versions, which inhibited proper transformations in the y-axis. 

# concurve 2.0.1

## Major changes 

* `plot_concurve()` now has "measure" as an item which allows for ratio measures to be logarithmically scaled on the x axis. There are two options, "default", which is set as the default option and is for mean differences, and "ratio", which will result in the axis being logarithmically scaled. 
* `plot_concurve()` also now has a "fill" option which will allow users to choose the color of the plot.


# concurve 2.0

## Major changes 

* The `plotpint()` function which plotted consonance functions has been repackaged into `ggconcurve()`.
* The `plotsint()` function which plotted surprisal functions has been repackaged into `ggconcurve()`.
* Functions can now also be plotted with base R via the `plot_concurve()` function.
* Consonance functions can be plotted as a pyramid (right side up) or inverted (upside down) via the "position" item in `ggconcurve()`.
* Null values (for means & ratios) can be plotted via the `ggconcurve()` function to show how much of the interval surrounds it.
* Log transformations included in all the plotting functions for ratio measures. 
* Parallel programming has now been implemented into the computations via the `mclapply()` function from the *parallel* package.  


# concurve 1.08

## Major changes

* Can produce consonance and surprisal functions for correlations via the `corrintervals()` function.
* Now able to construct consonance and surprisal functions from the point estiate, and confidence limits via the `rev_eng()` function.
* Graphs produced via the `plotpint()` or `plotsint()` function now able to take custom titles, subtitles, x-axis titles, and captions.

# concurve 1.07

## Major changes

* Can now produce consonance and surprisal functions for survival data produced with the `survival` package.

# concurve 1.06

## Major changes

* Now contains [documentation](https://data.lesslikely.com/concurve/articles/stata.html) for producing interval functons in `Stata`.

## Minor changes

* Default plots now contain grids, title, subtitle, and a caption. 
* Updated figures in README and the 'Examples in R' vignette/article.
