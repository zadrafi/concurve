# Export Consonance Data for Power BI

Exports consonance function data in formats optimized for Power BI
visualization and analysis.

## Usage

``` r
export_for_powerbi(data, file, format = NULL, include_metadata = TRUE,
  pivot = FALSE)
```

## Arguments

- data:

  A concurve object or intervals data frame.

- file:

  Output file path. Supports .csv, .xlsx, or .json extensions.

- format:

  Output format: "csv" (default), "excel", or "json". If NULL, inferred
  from file extension.

- include_metadata:

  Logical. If TRUE, includes additional metadata columns useful for
  Power BI DAX measures. Default is TRUE.

- pivot:

  Logical. If TRUE, creates a pivoted format with separate columns for
  lower and upper bounds at each confidence level. Default is FALSE.

## Value

Invisibly returns the exported data frame.

## Details

This function prepares consonance function data for import into Power
BI. The exported data includes:

- Confidence levels and corresponding intervals

- P-values and S-values

- Interval widths for precision analysis

When `include_metadata = TRUE`, adds columns for:

- `ci_label`: Human-readable confidence level (e.g., "95\\

- `significance`: Whether interval excludes null value

## See also

[`curve_table()`](reference/curve_table.md) for formatted interval
tables

[`curve_snowflake()`](reference/curve_snowflake.md) for Snowflake
database integration

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate consonance data
model <- lm(mpg ~ wt, data = mtcars)
result <- curve_gen(model, "wt")

# Export to CSV for Power BI
export_for_powerbi(result[[1]], "consonance_data.csv")

# Export with metadata
export_for_powerbi(result[[1]], "consonance_data.csv", include_metadata = TRUE)

# Export to Excel
export_for_powerbi(result[[1]], "consonance_data.xlsx", format = "excel")
} # }
```
