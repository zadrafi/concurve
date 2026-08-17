# Plot likelihood with multiple confidence levels

Plot likelihood with multiple confidence levels

## Usage

``` r
plot_ci_levels(x, parameter = NULL, ci_levels = c(0.5, 0.68, 0.9, 0.95,
  0.99), n_points = 200, colors = NULL, ...)
```

## Arguments

- x:

  A `likelihood_function` object

- parameter:

  Name of the parameter to plot. If `NULL`, uses the first parameter.

- ci_levels:

  Numeric vector of confidence levels to display (default: 50%, 68%,
  90%, 95%, 99%)

- n_points:

  Number of parameter values for profile likelihood evaluation

- colors:

  Character vector of colors for each confidence level. If `NULL`,
  automatically generated as a gradient from blue to red.

- ...:

  Additional arguments passed to
  [`plot`](https://rdrr.io/r/graphics/plot.default.html)

## Value

Invisibly returns `NULL`

## Details

Plots a single likelihood function with colored interval bands at
multiple confidence levels, allowing visual comparison of interval
widths and how confidence intervals change with level.
