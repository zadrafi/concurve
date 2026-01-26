# Construct Consonance Function for Ratio Measures

Builds a consonance distribution for ratio measures (odds ratios, risk
ratios, hazard ratios) from a point estimate and confidence interval
bounds. Computations are performed on the log scale internally.

## Usage

``` r
curve_from_ratio(point, lower_ci, upper_ci, ci_level = 0.95, steps = 1000,
  cores = getOption("mc.cores", 1L), table = TRUE)
```

## Arguments

- point:

  Numeric. The point estimate (e.g., OR = 1.5, HR = 2.1).

- lower_ci:

  Numeric. Lower bound of a reference confidence interval.

- upper_ci:

  Numeric. Upper bound of a reference confidence interval.

- ci_level:

  Numeric. The confidence level of the provided interval. Default is
  0.95.

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
dataframe, and optionally a summary table. Values are on the original
(exponentiated) scale.

## Details

The function back-calculates the standard error on the log scale:
\$\$SE\_{log} = \frac{\log(upper) - \log(lower)}{2 \times
z\_{1-\alpha/2}}\$\$

Then computes intervals at all levels and exponentiates the results.

## See also

[`curve_from_se()`](reference/curve_from_se.md),
[`curve_rev()`](reference/curve_rev.md),
[`ggcurve()`](reference/ggcurve.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# From a published odds ratio: OR = 1.61, 95% CI [0.997, 2.59]
result <- curve_from_ratio(point = 1.61, lower_ci = 0.997, upper_ci = 2.59)
ggcurve(result[[1]], measure = "ratio", nullvalue = 1)

# Hazard ratio with 90% CI
result <- curve_from_ratio(
  point = 0.72,
  lower_ci = 0.58,
  upper_ci = 0.89,
  ci_level = 0.90
)
ggcurve(result[[1]],
  measure = "ratio", nullvalue = 1,
  title = "Hazard Ratio for Treatment Effect"
)
} # }
```
