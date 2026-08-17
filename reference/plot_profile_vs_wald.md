# Compare profile and Wald confidence intervals

Compare profile and Wald confidence intervals

## Usage

``` r
plot_profile_vs_wald(x, parameter = NULL, ci_level = 0.95,
  n_points = 200, ...)
```

## Arguments

- x:

  A `likelihood_function` object

- parameter:

  Name of the parameter to plot. If `NULL`, uses the first parameter.

- ci_level:

  Confidence level (default: 0.95)

- n_points:

  Number of parameter values for profile likelihood evaluation

- ...:

  Additional arguments passed to
  [`plot`](https://rdrr.io/r/graphics/plot.default.html)

## Value

Invisibly returns a list with elements `profile` and `wald` containing
the respective confidence interval bounds

## Details

Plots the profile likelihood with both profile-based and Wald (normal
approximation) confidence interval bounds overlaid for comparison.
Differences between the two methods indicate departures from normality
or asymmetry in the likelihood.
