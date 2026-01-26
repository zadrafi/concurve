# Generic Wrapper for Any CI-Producing Function

Wraps any function that produces confidence intervals at a given level
and generates a full consonance distribution by computing intervals at
all levels from 0 to 1.

## Usage

``` r
curve_wrap(ci_func, steps = 1000, cores = getOption("mc.cores", 1L),
  table = TRUE, ...)
```

## Arguments

- ci_func:

  A function that takes `conf.level` as an argument and returns a
  numeric vector of length 2: c(lower, upper). Additional arguments can
  be passed via `...`.

- steps:

  Indicates how many consonance intervals are to be calculated. By
  default, it is set to 1000. Higher values produce smoother curves but
  take longer to compute.

- cores:

  Number of cores for parallel computation. Default is
  `getOption("mc.cores", 1L)`.

- table:

  Logical. If TRUE (default), includes a summary table in the output
  list.

- ...:

  Additional arguments passed to `ci_func`.

## Value

A list with class "concurve" containing:

- Intervals Dataframe: Data frame with columns lower.limit, upper.limit,
  intrvl.width, intrvl.level, cdf, pvalue, svalue

- Intervals Density: Data frame for density plotting

- Intervals Table: Summary table (if table = TRUE)

## See also

[`curve_gen()`](reference/curve_gen.md),
[`curve_mean()`](reference/curve_mean.md),
[`curve_from_se()`](reference/curve_from_se.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Wrap a proportion test
my_prop_ci <- function(conf.level) {
  prop.test(84, 100, conf.level = conf.level)$conf.int
}
result <- curve_wrap(my_prop_ci)
ggcurve(result[[1]], nullvalue = 0.80)

# Wrap with additional arguments
my_ttest_ci <- function(conf.level, x, y) {
  t.test(x, y, conf.level = conf.level)$conf.int
}
GroupA <- rnorm(50)
GroupB <- rnorm(50)
result <- curve_wrap(my_ttest_ci, x = GroupA, y = GroupB)
} # }
```
