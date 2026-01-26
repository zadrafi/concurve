# Construct Consonance Function from Ratio Estimate

Convenience function to construct consonance functions from ratio
measures (odds ratios, hazard ratios, risk ratios) given a point
estimate and confidence interval bounds.

## Usage

``` r
curve_from_ratio(ratio, lower, upper, conf.level = 0.95, steps = 1000,
  cores = getOption("mc.cores", 1L), table = TRUE)
```

## Arguments

- ratio:

  Point estimate of the ratio.

- lower:

  Lower bound of the confidence interval.

- upper:

  Upper bound of the confidence interval.

- conf.level:

  Confidence level of the provided interval. Default is 0.95.

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

This is a convenience wrapper around
[`curve_rev`](reference/curve_rev.md) specifically for ratio measures.
It automatically handles the log transformation and sets appropriate
measure type.

## See also

[`curve_from_se()`](reference/curve_from_se.md) for constructing from
standard error

[`curve_rev()`](reference/curve_rev.md) for the underlying function

## Examples

``` r
if (FALSE) { # \dontrun{
# From a published odds ratio: OR = 1.5, 95% CI [1.1, 2.0]
result <- curve_from_ratio(ratio = 1.5, lower = 1.1, upper = 2.0)
ggcurve(result[[1]], type = "c", nullvalue = TRUE)

# Hazard ratio from survival analysis: HR = 0.75, 95% CI [0.60, 0.95]
result <- curve_from_ratio(ratio = 0.75, lower = 0.60, upper = 0.95)
ggcurve(result[[1]], type = "c", nullvalue = TRUE)
} # }
```
