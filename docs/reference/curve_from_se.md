# Construct Consonance Function from Point Estimate and Standard Error

Builds a consonance distribution when you only have a point estimate and
its standard error. Common when working with database query results,
published statistics, or external model outputs.

## Usage

``` r
curve_from_se(point, se, df = Inf, steps = 1000,
  cores = getOption("mc.cores", 1L), table = TRUE)
```

## Arguments

- point:

  Numeric. The point estimate (e.g., mean difference, coefficient).

- se:

  Numeric. The standard error of the point estimate.

- df:

  Numeric. Degrees of freedom for t-distribution. Use `Inf` for z-based
  (normal) intervals. Default is `Inf`.

- steps:

  Indicates how many consonance intervals are to be calculated. By
  default, it is set to 1000.

- cores:

  Number of cores for parallel computation. Default is
  `getOption("mc.cores", 1L)`.

- table:

  Logical. If TRUE (default), includes a summary table.

## Value

A list with class "concurve" containing the intervals dataframe, density
dataframe, and optionally a summary table.

## Details

The function computes confidence intervals using: \$\$CI = \hat{\theta}
\pm t\_{1-\alpha/2, df} \times SE\$\$

where \\t\\ is the critical value from the t-distribution (or z if df =
Inf).

This is useful when:

- Working with regression outputs from databases (Snowflake, SQL Server)

- Reconstructing curves from published point estimates and CIs

- Building curves from custom estimators

## See also

[`curve_from_ratio()`](reference/curve_from_ratio.md),
[`curve_rev()`](reference/curve_rev.md),
[`curve_wrap()`](reference/curve_wrap.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# From a regression coefficient
result <- curve_from_se(point = 2.5, se = 0.8, df = 98)
ggcurve(result[[1]], nullvalue = 0)

# From published study: effect = 0.35, 95% CI [0.12, 0.58]
# Back-calculate SE: (0.58 - 0.12) / (2 * 1.96) = 0.117
result <- curve_from_se(point = 0.35, se = 0.117)
ggcurve(result[[1]], type = "s", nullvalue = 0)
} # }
```
