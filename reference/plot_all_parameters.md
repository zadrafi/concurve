# Plot likelihood for all parameters

Plot likelihood for all parameters

## Usage

``` r
plot_all_parameters(x, type = c("likelihood", "deviance"), ci_level = 0.95,
  n_points = 100, ncol = NULL, ...)
```

## Arguments

- x:

  A `likelihood_function` object

- type:

  Type of plot: `"likelihood"` or `"deviance"`

- ci_level:

  Confidence level for intervals (default: 0.95)

- n_points:

  Number of parameter values for profile likelihood evaluation

- ncol:

  Number of columns in the plot grid. If `NULL`, automatically
  determined as `ceiling(sqrt(n_parameters))`

- ...:

  Additional arguments passed to
  [`plot`](https://stat.lesslikely.com/concurve/reference/plot.likelihood_function.md)

## Value

Invisibly returns `NULL`

## Details

Creates a multi-panel plot with one panel per parameter. Useful for
visualizing all likelihood functions simultaneously for comparison.
