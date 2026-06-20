# Plot Multiple Consonance Functions

Creates a combined plot showing multiple consonance functions for
comparison.

Overlays multiple consonance or surprisal functions on a single plot for
direct visual comparison of effect estimates across groups, time
periods, or studies.

## Usage

``` r
plot_multi(..., type = "c", measure = "default", nullvalue = NULL,
  position = "pyramid", title = "Comparison of Consonance Functions",
  subtitle = "Functions display intervals at every level.",
  xaxis = expression(theta == ~"Effect Size"),
  yaxis1 = expression(paste(italic(p), "-value")),
  yaxis2 = "Confidence Level (%)", colors = NULL, alpha = 0.15,
  legend.position = "bottom")

plot_multi(..., type = "c", measure = "default", nullvalue = NULL,
  position = "pyramid", title = "Comparison of Consonance Functions",
  subtitle = "Functions display intervals at every level.",
  xaxis = expression(theta == ~"Effect Size"),
  yaxis1 = expression(paste(italic(p), "-value")),
  yaxis2 = "Confidence Level (%)", colors = NULL, alpha = 0.15,
  legend.position = "bottom")
```

## Arguments

- ...:

  Named concurve dataframes to compare. Names become legend labels.
  Alternatively, pass a single named list of dataframes.

- type:

  Character. Type of function to plot: "c" for consonance (default), "s"
  for surprisal.

- measure:

  Character. Scale type: "default" for linear, "ratio" for log scale
  (odds ratios, hazard ratios, etc.).

- nullvalue:

  Numeric. Value(s) to mark with vertical reference line(s). Use single
  value (e.g., 0) or vector for range (e.g., c(-0.5, 0.5)).

- position:

  Character. Orientation of consonance function: "pyramid" (default) or
  "inverted".

- title:

  Character. Plot title.

- subtitle:

  Character. Plot subtitle.

- xaxis:

  Character or expression. X-axis label.

- yaxis1:

  Character or expression. Primary y-axis label.

- yaxis2:

  Character. Secondary y-axis label (for CI levels).

- colors:

  Character vector. Colors for each curve. If NULL, uses
  colorblind-friendly palette.

- alpha:

  Numeric. Transparency for ribbon fill (0-1). Default is 0.15.

- legend.position:

  Character or numeric vector. Legend position. Default is "bottom".

- labels:

  Character vector of labels for each curve. If NULL, uses names from
  input or generates sequential labels.

## Value

A ggplot object.

A ggplot2 object.

## Details

This function overlays multiple consonance or surprisal functions on a
single plot for easy comparison. Each curve is distinguished by color.

## See also

[`ggcurve()`](reference/ggcurve.md) for single curve plotting

[`curve_overlap()`](reference/curve_overlap.md) for quantifying overlap

[`ggcurve()`](reference/ggcurve.md),
[`plot_compare()`](reference/plot_compare.md),
[`curve_compare()`](reference/curve_compare.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Compare three analyses
result1 <- curve_from_ratio(1.5, 1.1, 2.0)
result2 <- curve_from_ratio(1.3, 0.9, 1.8)
result3 <- curve_from_ratio(1.8, 1.2, 2.5)

plot_multi(result1[[1]], result2[[1]], result3[[1]],
  labels = c("Study A", "Study B", "Study C"),
  nullvalue = 1,
  title = "Comparison of Odds Ratios"
)
} # }

if (FALSE) { # \dontrun{
# Compare two studies
study1 <- curve_from_se(point = 0.5, se = 0.2, df = 50)
study2 <- curve_from_se(point = 0.8, se = 0.25, df = 80)

plot_multi(
  "Study A" = study1[[1]],
  "Study B" = study2[[1]],
  nullvalue = 0,
  title = "Comparison of Treatment Effects"
)

# Using a list
curves_list <- list("Group 1" = study1[[1]], "Group 2" = study2[[1]])
plot_multi(curves_list, type = "s", nullvalue = 0)
} # }
```
