# Batch Process Multiple Snowflake Queries for Consonance Functions

Executes multiple queries against a Snowflake database and constructs
consonance distributions for each result set.

## Usage

``` r
curve_snowflake_batch(conn, queries, estimate_col = "estimate",
  lower_col = "lower", upper_col = "upper", conf.level = 0.95,
  steps = 1000, cores = getOption("mc.cores", 1L), table = TRUE)
```

## Arguments

- conn:

  A DBI connection object to Snowflake database.

- queries:

  A named list of SQL query strings.

- estimate_col:

  Name of the column containing point estimates.

- lower_col:

  Name of the column containing lower bounds.

- upper_col:

  Name of the column containing upper bounds.

- conf.level:

  Confidence level of the input intervals. Default is 0.95.

- steps:

  Number of consonance levels to compute. Default is 1000.

- cores:

  Number of cores for parallel computation.

- table:

  Logical. If TRUE (default), includes summary tables.

## Value

A named list of concurve objects, one for each query.

## See also

[`curve_snowflake()`](reference/curve_snowflake.md) for single query
processing

[`plot_compare()`](reference/plot_compare.md) for comparing results

## Examples

``` r
if (FALSE) { # \dontrun{
queries <- list(
  "Model A" = "SELECT estimate, lower_ci, upper_ci FROM results WHERE model = 'A'",
  "Model B" = "SELECT estimate, lower_ci, upper_ci FROM results WHERE model = 'B'"
)

results <- curve_snowflake_batch(conn, queries,
  estimate_col = "estimate",
  lower_col = "lower_ci",
  upper_col = "upper_ci"
)

# Plot comparison
plot_compare(results[["Model A"]][[1]], results[["Model B"]][[1]])
} # }
```
