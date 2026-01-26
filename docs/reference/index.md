# Package index

- [`concurve-package`](reference/concurve-package.md)
  [`concurve`](reference/concurve-package.md) :

  A description of the `concurve` `R` package

## Statistical Computations

Compute consonance and surprisal distributions for a wide range of
scenarios, along with likelihood functions.

- [`curve_boot()`](reference/curve_boot.md) : Generate Consonance
  Functions via Bootstrapping
- [`curve_corr()`](reference/curve_corr.md) : Consonance Functions for
  Correlations
- [`curve_from_ratio()`](reference/curve_from_ratio.md) : Construct
  Consonance Function for Ratio Measures
- [`curve_from_se()`](reference/curve_from_se.md) : Construct Consonance
  Function from Point Estimate and Standard Error
- [`curve_gen()`](reference/curve_gen.md) : Consonance Functions For
  Linear Models, Generalized Linear Models, and Robust Linear Models
- [`curve_lik()`](reference/curve_lik.md) : Compute Profile Likelihood
  Functions
- [`curve_lmer()`](reference/curve_lmer.md) : Consonance Functions For
  Linear & Non-Linear Mixed-Effects Models.
- [`curve_mean()`](reference/curve_mean.md) : Consonance Functions For
  Mean Differences
- [`curve_meta()`](reference/curve_meta.md) : Consonance Functions For
  Meta-Analytic Data
- [`curve_rev()`](reference/curve_rev.md) : Reverse Engineer Consonance
  / Likelihood Functions Using the Point Estimate and Confidence Limits
- [`curve_surv()`](reference/curve_surv.md) : Consonance Functions For
  Survival Data

## Statistical Graphics

Plot the overall functions that were computed such as the consonance,
surprisal, and likelihood functions.

- [`ggcurve()`](reference/ggcurve.md) : Plots Consonance, Surprisal, and
  Likelihood Functions
- [`curve_compare()`](reference/curve_compare.md) : Compare Two
  Functions and Produces An AUC Score
- [`curve_overlap()`](reference/curve_overlap.md) : Compute Overlap
  Statistics Between Two Consonance Functions
- [`plot_compare()`](reference/plot_compare.md) : Graph and Compare
  Consonance, Surprisal, and Likelihood Functions
- [`plot_multi()`](reference/plot_multi.md) : Plot Multiple Consonance
  Functions for Comparison

## Statistical Reporting

Display the tables showing relevant statistics from the initial
computations.

- [`curve_summary()`](reference/curve_summary.md) : Generate Summary
  Statistics for Multiple Curves
- [`curve_table()`](reference/curve_table.md) : Produce Tables For
  concurve Functions

## Miscellaneous Functions

Some internal helper functions.

- [`curve_wrap()`](reference/curve_wrap.md) : Generic Wrapper for Any
  CI-Producing Function
- [`RobustMin()`](reference/RobustMin.md) : Robust Min, an alternative
  to max() that doesn't throw a warning
- [`RobustMax()`](reference/RobustMax.md) : Robust Max, an alternative
  to max() that doesn't throw a warning
