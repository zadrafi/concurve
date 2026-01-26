# Construct Consonance Functions from Snowflake Query Results

Connects to a Snowflake database and constructs consonance distributions
from query results containing point estimates and confidence intervals.

## Usage

``` r
curve_snowflake(conn, query, estimate_col = "estimate",
  lower_col = "lower", upper_col = "upper", conf.level = 0.95,
  steps = 1000, cores = getOption("mc.cores", 1L), table = TRUE)
```

## Arguments

- conn:

  A DBI connection object to Snowflake database.

- query:

  SQL query string that returns columns for estimate, lower, and upper
  bounds.

- estimate_col:

  Name of the column containing point estimates. Default is "estimate".

- lower_col:

  Name of the column containing lower bounds. Default is "lower".

- upper_col:

  Name of the column containing upper bounds. Default is "upper".

- conf.level:

  Confidence level of the input intervals. Default is 0.95.

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

This function requires the DBI and odbc packages to be installed. It
connects to Snowflake, executes the provided query, and constructs
consonance functions from the results.

## See also

[`curve_snowflake_batch()`](reference/curve_snowflake_batch.md) for
batch processing

[`curve_rev()`](reference/curve_rev.md) for constructing curves from
published intervals

[`export_for_powerbi()`](reference/export_for_powerbi.md) for exporting
results

## Examples

``` r
if (FALSE) { # \dontrun{
library(DBI)
library(odbc)

# Connect to Snowflake
conn <- dbConnect(odbc::odbc(),
  Driver = "Snowflake",
  Server = "your_account.snowflakecomputing.com",
  Database = "your_database",
  Schema = "your_schema",
  UID = "your_username",
  PWD = "your_password"
)

# Query with estimate and CI columns
query <- "SELECT estimate, lower_ci, upper_ci FROM results WHERE analysis_id = 1"
result <- curve_snowflake(conn, query,
  estimate_col = "estimate",
  lower_col = "lower_ci",
  upper_col = "upper_ci"
)

ggcurve(result[[1]], type = "c")

dbDisconnect(conn)
} # }
```
