# Plot likelihood function

Plot a likelihood function showing relative likelihood or deviance, with
optional confidence intervals and reference lines.

## Usage

``` r
# S3 method for class 'likelihood_function'
plot(x, parameter = NULL,
  type = c("likelihood", "deviance", "both"), n_points = 200,
  interval = NULL, add_ci = TRUE, ci_level = 0.95, add_mle = TRUE,
  relative = TRUE, main = NULL, xlab = NULL, ylab = NULL,
  col = "black", lwd = 2, ...)

# S3 method for class 'likelihood_function'
plot(x, parameter = NULL,
  type = c("likelihood", "deviance", "both"), n_points = 200,
  interval = NULL, add_ci = TRUE, ci_level = 0.95, add_mle = TRUE,
  relative = TRUE, main = NULL, xlab = NULL, ylab = NULL,
  col = "black", lwd = 2, ...)
```

## Arguments

- x:

  A `likelihood_function` object

- parameter:

  Name of the parameter to plot. If `NULL`, plots the first parameter
  and displays a message.

- type:

  Type of plot: `"likelihood"` (relative likelihood), `"deviance"`
  (profile deviance), or `"both"` (two-panel plot)

- n_points:

  Number of parameter values at which to evaluate the profile likelihood

- interval:

  A length-2 numeric vector specifying the range of parameter values to
  plot. If `NULL`, automatically determined as ±5 standard errors from
  the MLE.

- add_ci:

  Logical; if `TRUE`, adds confidence interval bounds to the plot

- ci_level:

  Confidence level for the intervals (default: 0.95)

- add_mle:

  Logical; if `TRUE`, adds a vertical line at the MLE

- relative:

  Logical; if `TRUE`, plots relative likelihood; if `FALSE`, plots
  absolute likelihood. Only applies to likelihood plots.

- main:

  Main title for the plot. If `NULL`, automatically generated.

- xlab:

  X-axis label. If `NULL`, uses the parameter name.

- ylab:

  Y-axis label. If `NULL`, automatically determined by plot type.

- col:

  Color for the main likelihood/deviance curve

- lwd:

  Line width for the main curve

- ...:

  Additional arguments passed to
  [`plot`](https://rdrr.io/r/graphics/plot.default.html)

## Value

Invisibly returns the profile likelihood data frame

## Details

Creates publication-quality plots of likelihood and deviance functions.
Includes optional confidence intervals and reference lines. For
two-panel plots, the layout is automatically managed.

## See also

[`ggplot_likelihood`](https://stat.lesslikely.com/concurve/reference/ggplot_likelihood.md)
for ggplot2 version,
[`plotly_likelihood`](https://stat.lesslikely.com/concurve/reference/plotly_likelihood.md)
for interactive plots
