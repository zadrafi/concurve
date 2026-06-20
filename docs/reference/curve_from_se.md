# Construct Consonance Function from Standard Error

Convenience function to construct consonance functions given a point
estimate and its standard error.

## Usage

``` r
curve_from_se(estimate, se, measure = "mean", steps = 1000,
  cores = getOption("mc.cores", 1L), table = TRUE)
```

## Arguments

- estimate:

  Point estimate.

- se:

  Standard error of the estimate.

- measure:

  Type of measure: "mean" for differences (default), "ratio" for ratio
  measures (estimate should be on original scale).

- steps:

  Number of consonance levels to compute. Default is 1000.

- cores:

  Number of cores for parallel computation.

- table:

  Logical. If TRUE (default), includes a summary table.

## Value

A list with class "concurve" containing the intervals dataframe, density
dataframe, and optionally a summary table.

## Details

This function constructs consonance intervals using the normal
approximation: `estimate +/- z * se` where z varies across confidence
levels.

For ratio measures, the function handles the log transformation
internally.

## See also

[`curve_from_ratio()`](reference/curve_from_ratio.md) for constructing
from CI bounds

[`curve_rev()`](reference/curve_rev.md) for the underlying function

## Examples

``` r
if (FALSE) { # \dontrun{
# Mean difference with SE
result <- curve_from_se(estimate = 2.5, se = 0.8, measure = "mean")
ggcurve(result[[1]], type = "c", nullvalue = TRUE)

# Odds ratio with SE (on log scale internally)
result <- curve_from_se(estimate = 1.5, se = 0.2, measure = "ratio")
ggcurve(result[[1]], type = "c", nullvalue = TRUE)
} # }
```
