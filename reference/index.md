# Package index

- [`concurve`](https://stat.lesslikely.com/concurve/reference/concurve-package.md)
  [`concurve-package`](https://stat.lesslikely.com/concurve/reference/concurve-package.md)
  :

  A description of the `concurve` `R` package

## Statistical Computations

Compute consonance and surprisal distributions for a wide range of
scenarios, along with likelihood functions.

- [`curve_boot()`](https://stat.lesslikely.com/concurve/reference/curve_boot.md)
  : Generate Consonance Functions via Bootstrapping
- [`curve_corr()`](https://stat.lesslikely.com/concurve/reference/curve_corr.md)
  : Consonance Functions for Correlations
- [`curve_gen()`](https://stat.lesslikely.com/concurve/reference/curve_gen.md)
  : Consonance Functions For Linear Models, Generalized Linear Models,
  and Robust Linear Models
- [`curve_lik()`](https://stat.lesslikely.com/concurve/reference/curve_lik.md)
  : Compute Profile Likelihood Functions
- [`curve_lmer()`](https://stat.lesslikely.com/concurve/reference/curve_lmer.md)
  : Consonance Functions For Linear & Non-Linear Mixed-Effects Models.
- [`curve_mean()`](https://stat.lesslikely.com/concurve/reference/curve_mean.md)
  : Consonance Functions For Mean Differences
- [`curve_meta()`](https://stat.lesslikely.com/concurve/reference/curve_meta.md)
  : Consonance Functions For Meta-Analytic Data
- [`curve_model()`](https://stat.lesslikely.com/concurve/reference/curve_model.md)
  : Construct Consonance Functions from Fitted Models
- [`curve_rev()`](https://stat.lesslikely.com/concurve/reference/curve_rev.md)
  : Reverse Engineer Consonance / Likelihood Functions Using the Point
  Estimate and Confidence Limits
- [`curve_surv()`](https://stat.lesslikely.com/concurve/reference/curve_surv.md)
  : Consonance Functions For Survival Data
- [`curve_wrap()`](https://stat.lesslikely.com/concurve/reference/curve_wrap.md)
  : Construct Consonance Functions from Any CI-Producing Function
- [`curve_from_ratio()`](https://stat.lesslikely.com/concurve/reference/curve_from_ratio.md)
  : Construct Consonance Function from Ratio Estimate
- [`curve_from_se()`](https://stat.lesslikely.com/concurve/reference/curve_from_se.md)
  : Construct Consonance Function from Standard Error

## Database Integration

Connect to databases and construct consonance functions from query
results.

- [`curve_snowflake()`](https://stat.lesslikely.com/concurve/reference/curve_snowflake.md)
  : Construct Consonance Functions from Snowflake Query Results
- [`curve_snowflake_batch()`](https://stat.lesslikely.com/concurve/reference/curve_snowflake_batch.md)
  : Batch Process Multiple Snowflake Queries for Consonance Functions

## Statistical Graphics

Plot the overall functions that were computed such as the consonance,
surprisal, and likelihood functions.

- [`ggcurve()`](https://stat.lesslikely.com/concurve/reference/ggcurve.md)
  : Plots Consonance, Surprisal, and Likelihood Functions
- [`curve_compare()`](https://stat.lesslikely.com/concurve/reference/curve_compare.md)
  : Compare Two Functions and Produces An AUC Score
- [`plot_compare()`](https://stat.lesslikely.com/concurve/reference/plot_compare.md)
  : Graph and Compare Consonance, Surprisal, and Likelihood Functions
- [`curve_overlap()`](https://stat.lesslikely.com/concurve/reference/curve_overlap.md)
  : Calculate Overlap Between Consonance Functions
- [`plot_multi()`](https://stat.lesslikely.com/concurve/reference/plot_multi.md)
  : Plot Multiple Consonance Functions

## Statistical Reporting

Display the tables showing relevant statistics from the initial
computations.

- [`curve_table()`](https://stat.lesslikely.com/concurve/reference/curve_table.md)
  : Produce Tables For concurve Functions
- [`curve_summary()`](https://stat.lesslikely.com/concurve/reference/curve_summary.md)
  : Generate Summary Statistics for Consonance Objects

## Data Export

Export consonance data to other tools and formats.

- [`export_for_powerbi()`](https://stat.lesslikely.com/concurve/reference/export_for_powerbi.md)
  : Export Consonance Data for Power BI

## Likelihood Functions

Construct likelihood, log-likelihood, profile likelihood, and deviance
functions from fitted models, and plot them.

- [`construct_likelihood()`](https://stat.lesslikely.com/concurve/reference/construct_likelihood.md)
  : Construct Likelihood Function for Statistical Models
- [`plot(`*`<likelihood_function>`*`)`](https://stat.lesslikely.com/concurve/reference/plot.likelihood_function.md)
  : Plot likelihood function
- [`ggplot_likelihood()`](https://stat.lesslikely.com/concurve/reference/ggplot_likelihood.md)
  : Plot likelihood using ggplot2
- [`plotly_likelihood()`](https://stat.lesslikely.com/concurve/reference/plotly_likelihood.md)
  : Interactive likelihood plot
- [`plot_all_parameters()`](https://stat.lesslikely.com/concurve/reference/plot_all_parameters.md)
  : Plot likelihood for all parameters
- [`plot_ci_levels()`](https://stat.lesslikely.com/concurve/reference/plot_ci_levels.md)
  : Plot likelihood with multiple confidence levels
- [`plot_profile_vs_wald()`](https://stat.lesslikely.com/concurve/reference/plot_profile_vs_wald.md)
  : Compare profile and Wald confidence intervals

## Likelihood Function Methods

S3 methods for objects returned by construct_likelihood().

- [`coef(`*`<likelihood_function>`*`)`](https://stat.lesslikely.com/concurve/reference/coef.likelihood_function.md)
  : Extract coefficients from a likelihood function
- [`confint(`*`<likelihood_function>`*`)`](https://stat.lesslikely.com/concurve/reference/confint.likelihood_function.md)
  : Confidence intervals for likelihood function parameters
- [`logLik(`*`<likelihood_function>`*`)`](https://stat.lesslikely.com/concurve/reference/logLik.likelihood_function.md)
  : Extract log-likelihood from a likelihood function
- [`print(`*`<likelihood_function>`*`)`](https://stat.lesslikely.com/concurve/reference/print.likelihood_function.md)
  : Print a likelihood function object
- [`print(`*`<summary.likelihood_function>`*`)`](https://stat.lesslikely.com/concurve/reference/print.summary.likelihood_function.md)
  : Print a likelihood function summary
- [`summary(`*`<likelihood_function>`*`)`](https://stat.lesslikely.com/concurve/reference/summary.likelihood_function.md)
  : Summarize a likelihood function object
- [`vcov(`*`<likelihood_function>`*`)`](https://stat.lesslikely.com/concurve/reference/vcov.likelihood_function.md)
  : Extract variance-covariance matrix from a likelihood function

## Miscellaneous Functions

Some internal helper functions.

- [`RobustMin()`](https://stat.lesslikely.com/concurve/reference/RobustMin.md)
  : Robust Min, an alternative to max() that doesn't throw a warning
- [`RobustMax()`](https://stat.lesslikely.com/concurve/reference/RobustMax.md)
  : Robust Max, an alternative to max() that doesn't throw a warning
