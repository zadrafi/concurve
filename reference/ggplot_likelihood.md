# Plot likelihood using ggplot2

Plot likelihood using ggplot2

## Usage

``` r
ggplot_likelihood(x, parameter = NULL, type = c("likelihood", "deviance",
  "both"), ci_level = 0.95, n_points = 200, theme = "minimal")
```

## Arguments

- x:

  A `likelihood_function` object

- parameter:

  Name of the parameter to plot. If `NULL`, uses the first parameter.

- type:

  Type of plot: `"likelihood"`, `"deviance"`, or `"both"`

- ci_level:

  Confidence level (default: 0.95)

- n_points:

  Number of parameter values for profile likelihood evaluation

- theme:

  ggplot2 theme to apply. Options: `"minimal"` (default), `"classic"`,
  `"bw"`, or any other ggplot2 theme object.

## Value

A ggplot object (or patchwork composition for `type = "both"`)

## Details

Creates publication-quality plots using ggplot2 with subtitle showing
the confidence interval. For `type = "both"`, requires the patchwork
package for combining plots vertically. If patchwork is not installed,
only the likelihood plot is returned with a message.

## See also

[`plot`](https://stat.lesslikely.com/concurve/reference/plot.likelihood_function.md)
for base graphics version,
[`plotly_likelihood`](https://stat.lesslikely.com/concurve/reference/plotly_likelihood.md)
for interactive plots
