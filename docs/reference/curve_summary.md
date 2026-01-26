# Generate Summary Statistics for Multiple Curves

Creates a comparison table with point estimates, confidence intervals,
and S-values at a specified null hypothesis for multiple consonance
functions.

## Usage

``` r
curve_summary(curves_list, nullvalue = 0, ci_levels = 0.95)
```

## Arguments

- curves_list:

  A named list of concurve dataframes.

- nullvalue:

  Numeric. The null hypothesis value for S-value calculation. Default is
  0.

- ci_levels:

  Numeric vector. Confidence levels to include in summary. Default is
  0.95.

## Value

A data frame with columns:

- Group: Name of the curve

- Estimate: Point estimate (midpoint of narrowest interval)

- CI_Lower, CI_Upper: Bounds at requested confidence level(s)

- CI_Width: Width of interval

- Svalue_at_Null: S-value at the null hypothesis

- Evidence: Plain-language interpretation of evidence strength

## See also

[`plot_multi()`](reference/plot_multi.md),
[`curve_overlap()`](reference/curve_overlap.md),
[`curve_table()`](reference/curve_table.md)

## Examples

``` r
if (FALSE) { # \dontrun{
curves <- list(
  "Group A" = curve_from_se(1.2, 0.3)[[1]],
  "Group B" = curve_from_se(0.8, 0.4)[[1]],
  "Group C" = curve_from_se(1.5, 0.2)[[1]]
)
curve_summary(curves, nullvalue = 0)
} # }
```
