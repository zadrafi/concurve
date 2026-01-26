# Snowflake Integration

![](https://res.cloudinary.com/less-likely/image/upload/v1575441662/Site/Logo2.jpg)

## Overview

This vignette demonstrates how to construct consonance (confidence)
distributions directly from Snowflake query results. This workflow is
particularly useful when:

- Model results are stored in data warehouse tables
- A/B test statistics are computed in SQL
- You want to visualize uncertainty from production analytics pipelines
- Integration with Power BI or other BI tools is required

## Prerequisites

Install the required packages:

``` r

# Core package
install.packages("concurve")

# Database connectivity
install.packages("DBI")
install.packages("odbc")

# For Snowflake specifically
# Option 1: ODBC driver (requires Snowflake ODBC driver installed)
# Option 2: Native R driver
install.packages("RSnowflake")  # if available
```

## Connecting to Snowflake

## Basic Usage: Point Estimate with Standard Error

The most common case is when you have a point estimate and its standard
error stored in a table.

### Example: Pre-computed Model Results

### Custom Column Names

If your table uses different column names:

``` r

sql <- "
  SELECT
    beta_hat,
    se_beta,
    degrees_freedom
  FROM regression_outputs
  WHERE model_id = 123
"

result <- curve_snowflake(
  con, sql,
  type = "se",
  point_col = "beta_hat",
  se_col = "se_beta",
  df_col = "degrees_freedom"
)
```

## Computing Regression Statistics in Snowflake

Snowflake provides `REGR_*` aggregate functions that can compute
regression statistics directly in SQL.

### Single Regression

``` r

sql <- "
  SELECT
    REGR_SLOPE(sales, price) AS slope,
    REGR_COUNT(sales, price) AS n_obs,
    -- Compute SE: sqrt(MSE / SXX)
    SQRT(
      REGR_SYYX(sales, price) / NULLIF(REGR_COUNT(sales, price) - 2, 0)
      / NULLIF(REGR_SXX(sales, price), 0)
    ) AS slope_se
  FROM sales_data
  WHERE category = 'Electronics'
    AND sale_date >= '2024-01-01'
"

result <- curve_snowflake(con, sql, type = "regression")
ggcurve(result[[1]], nullvalue = 0, title = "Price Effect on Electronics Sales")
```

### Explanation of REGR\_\* Functions

| Function               | Description              | Formula         |
|------------------------|--------------------------|-----------------|
| `REGR_SLOPE(y, x)`     | Slope coefficient        | β₁ = Sxy / Sxx  |
| `REGR_INTERCEPT(y, x)` | Intercept                | β₀ = ȳ - β₁x̄    |
| `REGR_COUNT(y, x)`     | Number of non-null pairs | n               |
| `REGR_SXX(y, x)`       | Sum of squares for x     | Σ(x - x̄)²       |
| `REGR_SYY(y, x)`       | Sum of squares for y     | Σ(y - ȳ)²       |
| `REGR_SXY(y, x)`       | Sum of cross-products    | Σ(x - x̄)(y - ȳ) |
| `REGR_SYYX(y, x)`      | Sum of squared residuals | SSR = Σ(y - ŷ)² |
| `REGR_R2(y, x)`        | R-squared                | 1 - SSR/SYY     |

The standard error of the slope is: \\SE(\hat{\beta}\_1) =
\sqrt{\frac{MSE}{S\_{XX}}} = \sqrt{\frac{SSR/(n-2)}{S\_{XX}}}\\

\## A/B Test Results

For conversion rate experiments:

``` r

sql <- "
  SELECT
    COUNT_IF(variant = 'control') AS n_control,
    COUNT_IF(variant = 'control' AND converted) AS x_control,
    COUNT_IF(variant = 'treatment') AS n_treatment,
    COUNT_IF(variant = 'treatment' AND converted) AS x_treatment
  FROM experiment_events
  WHERE experiment_id = 'checkout_v2'
    AND event_date BETWEEN '2024-06-01' AND '2024-06-30'
"

result <- curve_snowflake(con, sql, type = "abtest")

# The curve shows the difference in conversion rates (treatment - control)
ggcurve(
  result[[1]],
  nullvalue = 0,
  title = "Checkout Redesign: Conversion Rate Lift",
  xaxis = "Difference in Conversion Rate"
)

# Check S-value at null
svalue_at_null(result[[1]], null = 0)
```

## Ratio Measures (Odds Ratios, Hazard Ratios)

When working with ratio measures that have asymmetric confidence
intervals:

``` r

sql <- "
  SELECT
    odds_ratio AS estimate,
    or_lower_95 AS lower_ci,
    or_upper_95 AS upper_ci
  FROM survival_analysis_results
  WHERE cohort = '2024_q1'
    AND outcome = 'churn_90d'
"

result <- curve_snowflake(
  con, sql,
  type = "ratio",
  ci_level = 0.95
)

ggcurve(
  result[[1]],
  measure = "ratio",
  nullvalue = 1,
  title = "Churn Risk: Treatment vs Control"
)
```

## Batch Processing Multiple Groups

Often you need to compare effects across segments, regions, or time
periods.

### Regional Comparison

``` r

sql <- "
  SELECT
    region AS group_name,
    avg_treatment_effect AS estimate,
    effect_std_error AS std_error,
    n_units - 1 AS df
  FROM analytics.regional_treatment_effects
  WHERE campaign = 'summer_promo_2024'
"

# Returns a named list of concurve objects
curves <- curve_snowflake_batch(con, sql, type = "se")

# Overlay plot
plot_multi(
  curves,
  nullvalue = 0,
  title = "Promotion Effect by Region"
)

# Summary table
curve_summary(
  lapply(curves, `[[`, 1),  # Extract first element (dataframe) from each
  nullvalue = 0
)
```

### Time Series of Effects

``` r

sql <- "
  SELECT
    DATE_TRUNC('month', analysis_date)::VARCHAR AS group_name,
    elasticity_estimate AS estimate,
    elasticity_se AS std_error,
    n_transactions - 2 AS df
  FROM analytics.monthly_elasticity
  WHERE product_category = 'Apparel'
    AND analysis_date >= '2023-01-01'
  ORDER BY analysis_date
"

monthly_curves <- curve_snowflake_batch(con, sql, type = "se")

# Select specific months for comparison
selected <- monthly_curves[c("2024-01", "2024-04", "2024-07", "2024-10")]
plot_multi(selected, nullvalue = -1, title = "Elasticity Trend (Quarterly)")
```

## Complete SQL Patterns

### Pattern 1: Lift Measurement with Bootstrap SE

``` sql
-- Compute lift with standard error from bootstrapped samples
WITH daily_metrics AS (
  SELECT
  sale_date,
  SUM(CASE WHEN in_treatment THEN revenue END) AS treatment_rev,
  SUM(CASE WHEN NOT in_treatment THEN revenue END) AS control_rev,
  COUNT(DISTINCT CASE WHEN in_treatment THEN customer_id END) AS treatment_n,
  COUNT(DISTINCT CASE WHEN NOT in_treatment THEN customer_id END) AS control_n
  FROM sales
  WHERE campaign_id = 'test_123'
  GROUP BY sale_date
)
SELECT
AVG(treatment_rev / NULLIF(treatment_n, 0) - control_rev / NULLIF(control_n, 0)) AS estimate,
STDDEV(treatment_rev / NULLIF(treatment_n, 0) - control_rev / NULLIF(control_n, 0))
/ SQRT(COUNT(*)) AS std_error,
COUNT(*) - 1 AS df
FROM daily_metrics
```

### Pattern 2: Weighted Mean Difference

``` sql
-- Weighted difference between groups with pooled variance
WITH group_stats AS (
  SELECT
  treatment_group,
  AVG(outcome) AS mean_outcome,
  VARIANCE(outcome) AS var_outcome,
  COUNT(*) AS n
  FROM experiment_data
  GROUP BY treatment_group
)
SELECT
MAX(CASE WHEN treatment_group = 1 THEN mean_outcome END) -
  MAX(CASE WHEN treatment_group = 0 THEN mean_outcome END) AS estimate,

SQRT(
  SUM(var_outcome * (n - 1)) / (SUM(n) - 2) *  -- Pooled variance
  (1.0 / MAX(CASE WHEN treatment_group = 1 THEN n END) +
      1.0 / MAX(CASE WHEN treatment_group = 0 THEN n END))
) AS std_error,

SUM(n) - 2 AS df
FROM group_stats
```

### Pattern 3: Ratio of Means (Relative Lift)

``` sql
-- Relative lift with delta method SE
WITH stats AS (
  SELECT
  AVG(CASE WHEN variant = 'treatment' THEN metric END) AS mu_t,
  AVG(CASE WHEN variant = 'control' THEN metric END) AS mu_c,
  VARIANCE(CASE WHEN variant = 'treatment' THEN metric END) AS var_t,
  VARIANCE(CASE WHEN variant = 'control' THEN metric END) AS var_c,
  COUNT_IF(variant = 'treatment') AS n_t,
  COUNT_IF(variant = 'control') AS n_c
  FROM experiment_results
)
SELECT
mu_t / NULLIF(mu_c, 0) AS estimate,
-- 95% CI via delta method
(mu_t / NULLIF(mu_c, 0)) * (1 - 1.96 * SQRT(var_t / (n_t * mu_t * mu_t) +
                                              var_c / (n_c * mu_c * mu_c))) AS lower_ci,
(mu_t / NULLIF(mu_c, 0)) * (1 + 1.96 * SQRT(var_t / (n_t * mu_t * mu_t) +
                                              var_c / (n_c * mu_c * mu_c))) AS upper_ci
FROM stats
```

## Exporting to Power BI

For integration with Power BI dashboards:

``` r

# Single curve
curve <- curve_snowflake(con, sql, type = "se")
export_for_powerbi(
  curve[[1]],
  filename = "elasticity_curve.csv",
  thin_factor = 10,  # Keep every 10th row
  metadata = list(
    model = "price_elasticity",
    category = "Electronics",
    run_date = Sys.Date()
  )
)

# Multiple curves for comparison dashboard
for (region in names(curves)) {
  export_for_powerbi(
    curves[[region]][[1]],
    filename = paste0("curve_", region, ".csv"),
    metadata = list(region = region)
  )
}

# Or combine into single file
all_curves <- do.call(rbind, lapply(names(curves), function(nm) {
  export_for_powerbi(
    curves[[nm]][[1]],
    metadata = list(group = nm)
  )
}))
write.csv(all_curves, "all_regional_curves.csv", row.names = FALSE)
```

### Power BI Visualization Tips

In Power BI, create a custom visual:

1.  Import the CSV
2.  Create a Line Chart with:

- X-axis: `lower_limit` and `upper_limit` (use a union)
- Y-axis: `pvalue` (for consonance) or `svalue` (for surprisal)
- Legend: `group` (if comparing multiple)

3.  Add a constant line at `pvalue = 0.05` for reference
4.  Use conditional formatting to highlight regions

## Quantifying Curve Overlap

Compare how similar two estimates are:

``` r

# Extract dataframes from batch results
curve_east <- curves[["East"]][[1]]
curve_west <- curves[["West"]][[1]]

# Calculate overlap
overlap_stats <- curve_overlap(curve_east, curve_west)
print(overlap_stats)
# $overlap_pct: 45.2
# $interpretation: "Moderate overlap - effects may differ"
```

## Summary Table for Reporting

Generate a publication-ready comparison table:

``` r

# Extract dataframes
curve_dfs <- lapply(curves, `[[`, 1)

# Generate summary
summary_tbl <- curve_summary(curve_dfs, nullvalue = 0, ci_levels = c(0.90, 0.95))
print(summary_tbl)

# Export for reporting
knitr::kable(summary_tbl, digits = 3, caption = "Regional Effect Comparison")
```

## Best Practices

1 . **Always specify degrees of freedom** when available for proper
t-distribution intervals 2. **Use `type = "ratio"`** for multiplicative
measures (OR, RR, HR) to ensure proper log-scale computation 3. **Thin
data for export** (`thin_factor = 10-20`) to keep file sizes manageable
4. **Add metadata** when exporting to track model versions and run dates
5. **Check S-values** rather than just p-values for clearer
interpretation of evidence

## Troubleshooting

### Column Not Found

### NA Values

``` r

# Ensure your query handles NULLs
sql <- "
  SELECT
    COALESCE(estimate, 0) AS estimate,
    COALESCE(std_error, 0.001) AS std_error  -- Small default
  FROM results
  WHERE estimate IS NOT NULL
"
```

### Large Result Sets

``` r

# For batch processing many groups, consider:
# 1. Filtering to relevant groups in SQL
# 2. Using parallel processing
library(parallel)
curves <- mclapply(groups, function(g) {
  sql <- sprintf("SELECT ... WHERE group = '%s'", g)
  curve_snowflake(con, sql)
}, mc.cores = 4)
```

## Conclusion

The Snowflake integration enables a seamless workflow from data
warehouse to uncertainty visualization. By computing statistics in SQL
and constructing consonance distributions in R, you can:

- Visualize the full range of compatible effect sizes
- Compare effects across segments with proper uncertainty quantification
- Export results to BI tools for dashboards
- Make better decisions by seeing beyond single point estimates

``` r

# Clean up
dbDisconnect(con)
```
