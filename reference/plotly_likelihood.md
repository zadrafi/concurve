# Interactive likelihood plot

Interactive likelihood plot

## Usage

``` r
plotly_likelihood(x, parameter = NULL, ci_level = 0.95, n_points = 200)
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

## Value

A plotly object with interactive hover tooltips

## Details

Creates an interactive plot using plotly, allowing users to hover over
the likelihood curve to see exact values. Includes confidence interval
bounds and MLE reference line.

## See also

[`plot`](https://stat.lesslikely.com/concurve/reference/plot.likelihood_function.md)
for base graphics version,
[`ggplot_likelihood`](https://stat.lesslikely.com/concurve/reference/ggplot_likelihood.md)
for ggplot2 version
