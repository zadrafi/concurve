# Construct Consonance Functions from Any CI-Producing Function

A generic wrapper that constructs consonance, surprisal, and likelihood
functions from any function that produces confidence intervals. This is
the most flexible approach for generating consonance curves from
arbitrary statistical procedures.

## Usage

``` r
curve_wrap(ci_func, steps = 1000, cores = getOption("mc.cores", 1L),
  table = TRUE)
```

## Arguments

- ci_func:

  A function that takes a confidence level (0-1) as its first argument
  and returns a numeric vector of length 2 (lower, upper bounds). See
  examples for common patterns.

- steps:

  Number of consonance levels to compute. Default is 1000.

- cores:

  Number of cores for parallel computation. Default uses
  `getOption("mc.cores", 1L)`.

- table:

  Logical. If TRUE (default), includes a summary table of key intervals
  in the output.

## Value

A list with class "concurve" containing:

- Intervals Dataframe:

  Data frame with columns: lower.limit, upper.limit, intrvl.width,
  intrvl.level, cdf, pvalue, svalue

- Intervals Density:

  Data frame for plotting density functions

- Intervals Table:

  Summary table of key intervals (if table = TRUE)

## Details

This function repeatedly calls `ci_func` at different confidence levels
to construct the full consonance function. The `ci_func` argument must
be a function that:

1.  Takes a confidence level (numeric between 0 and 1) as its first
    argument

2.  Returns a numeric vector of exactly length 2: c(lower_bound,
    upper_bound)

Common patterns for `ci_func`:

- Linear models:
  `function(level) confint.default(model, parm = "x", level = level)`

- GLMs (profile):
  `function(level) confint(model, parm = "x", level = level)`

- t-tests: `function(level) t.test(x, conf.level = level)$conf.int`

- Correlations:
  `function(level) cor.test(x, y, conf.level = level)$conf.int`

- Proportions:
  `function(level) prop.test(x, n, conf.level = level)$conf.int`

- Survival:
  `function(level) exp(confint(coxph_model, level = level)["var",])`

## See also

[`ggcurve()`](reference/ggcurve.md) for plotting

`curve_gen()` for model-specific consonance functions

[`curve_rev()`](reference/curve_rev.md) for constructing curves from
published intervals

[`curve_table()`](reference/curve_table.md) for interval summaries

## Examples

``` r
if (FALSE) { # \dontrun{
# Example 1: Linear model
data(mtcars)
model <- lm(mpg ~ wt + hp, data = mtcars)
ci_func <- function(level) confint.default(model, parm = "wt", level = level)
result <- curve_wrap(ci_func)
ggcurve(result[[1]], type = "c")

# Example 2: t-test
x <- rnorm(30, mean = 5)
ci_func <- function(level) t.test(x, conf.level = level)$conf.int
result <- curve_wrap(ci_func)
ggcurve(result[[1]], type = "c", nullvalue = 0)

# Example 3: Correlation
x <- rnorm(50)
y <- x + rnorm(50)
ci_func <- function(level) cor.test(x, y, conf.level = level)$conf.int
result <- curve_wrap(ci_func)

# Example 4: GLM with profile likelihood CIs
model <- glm(am ~ wt, data = mtcars, family = binomial)
ci_func <- function(level) confint(model, parm = "wt", level = level)
result <- curve_wrap(ci_func)

# Example 5: Proportion test
ci_func <- function(level) prop.test(45, 100, conf.level = level)$conf.int
result <- curve_wrap(ci_func)

# Example 6: Custom function with additional arguments
my_ci <- function(level, data, method) {
  cor.test(data$x, data$y, method = method, conf.level = level)$conf.int
}
df <- data.frame(x = rnorm(50), y = rnorm(50))
ci_func <- function(level) my_ci(level, data = df, method = "spearman")
result <- curve_wrap(ci_func)
} # }
```
