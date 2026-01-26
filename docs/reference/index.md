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
- [`curve_lik()`](reference/curve_lik.md) : Compute Profile Likelihood
  Functions
- [`curve_lmer()`](reference/curve_lmer.md) : Consonance Functions For
  Linear & Non-Linear Mixed-Effects Models.
- [`curve_mean()`](reference/curve_mean.md) : Consonance Functions For
  Mean Differences
- [`curve_meta()`](reference/curve_meta.md) : Consonance Functions For
  Meta-Analytic Data
- [`curve_model()`](reference/curve_model.md) : Construct Consonance
  Functions from Fitted Models
- [`curve_rev()`](reference/curve_rev.md) : Reverse Engineer Consonance
  / Likelihood Functions Using the Point Estimate and Confidence Limits
- [`curve_surv()`](reference/curve_surv.md) : Consonance Functions For
  Survival Data
- [`curve_wrap()`](reference/curve_wrap.md) : Construct Consonance
  Functions from Any CI-Producing Function
- [`curve_from_ratio()`](reference/curve_from_ratio.md) : Construct
  Consonance Function from Ratio Estimate
- [`curve_from_se()`](reference/curve_from_se.md) : Construct Consonance
  Function from Standard Error

## Database Integration

Connect to databases and construct consonance functions from query
results.

- [`curve_snowflake()`](reference/curve_snowflake.md) : Construct
  Consonance Functions from Snowflake Query Results
- [`curve_snowflake_batch()`](reference/curve_snowflake_batch.md) :
  Batch Process Multiple Snowflake Queries for Consonance Functions

## Statistical Graphics

Plot the overall functions that were computed such as the consonance,
surprisal, and likelihood functions.

- [`ggcurve()`](reference/ggcurve.md) : Plots Consonance, Surprisal, and
  Likelihood Functions
- [`curve_compare()`](reference/curve_compare.md) : Compare Two
  Functions and Produces An AUC Score
- [`plot_compare()`](reference/plot_compare.md) : Graph and Compare
  Consonance, Surprisal, and Likelihood Functions
- [`curve_overlap()`](reference/curve_overlap.md) : Calculate Overlap
  Between Consonance Functions
- [`plot_multi()`](reference/plot_multi.md) : Plot Multiple Consonance
  Functions

## Statistical Reporting

Display the tables showing relevant statistics from the initial
computations.

- [`curve_table()`](reference/curve_table.md) : Produce Tables For
  concurve Functions
- [`curve_summary()`](reference/curve_summary.md) : Generate Summary
  Statistics for Consonance Objects

## Data Export

Export consonance data to other tools and formats.

- [`export_for_powerbi()`](reference/export_for_powerbi.md) : Export
  Consonance Data for Power BI

## Miscellaneous Functions

Some internal helper functions.

- [`RobustMin()`](reference/RobustMin.md) : Robust Min, an alternative
  to max() that doesn't throw a warning
- [`RobustMax()`](reference/RobustMax.md) : Robust Max, an alternative
  to max() that doesn't throw a warning
