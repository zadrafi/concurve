# Generate Summary Statistics for Consonance Objects

Produces a summary of key statistics from a consonance function.

## Usage

``` r
curve_summary(data, levels = c(0.5, 0.9, 0.95, 0.99), null_value = NULL,
  digits = 4)
```

## Arguments

- data:

  A concurve object or intervals data frame.

- levels:

  Confidence levels to include in summary. Default is c(0.50, 0.90,
  0.95, 0.99).

- null_value:

  Reference value for compatibility assessment. Default is 0 for
  differences, 1 for ratios.

- digits:

  Number of decimal places in output. Default is 4.

## Value

A data frame with summary statistics.

## Details

This function provides a summary of a consonance function including:

- Point estimate (value at maximum consonance)

- Confidence intervals at specified levels

- P-value and S-value at the null hypothesis

- Interval width as a measure of precision

## See also

[`curve_table()`](reference/curve_table.md) for formatted interval
tables

## Examples

``` r
if (FALSE) { # \dontrun{
model <- lm(mpg ~ wt, data = mtcars)
result <- curve_gen(model, "wt")

# Get summary
curve_summary(result[[1]])

# Custom levels
curve_summary(result[[1]], levels = c(0.80, 0.95))
} # }
```
