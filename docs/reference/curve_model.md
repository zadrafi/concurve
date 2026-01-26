# Construct Consonance Functions from Fitted Models

A convenience wrapper around [`curve_wrap`](reference/curve_wrap.md) for
common model objects. Automatically selects the appropriate confidence
interval method based on the model class or user specification.

## Usage

``` r
curve_model(model, param, method = "default", steps = 1000,
  cores = getOption("mc.cores", 1L), table = TRUE)
```

## Arguments

- model:

  A fitted model object (lm, glm, nls, lme, etc.)

- param:

  Character string specifying which parameter to extract.

- method:

  Method for computing confidence intervals:

  "default"

  :   Uses [`confint.default()`](https://rdrr.io/r/stats/confint.html) -
      Wald/normal approximation (fast)

  "profile"

  :   Uses [`confint()`](https://rdrr.io/r/stats/confint.html) - profile
      likelihood (slower, more accurate for GLMs)

- steps:

  Number of consonance levels to compute. Default is 1000.

- cores:

  Number of cores for parallel computation.

- table:

  Logical. If TRUE (default), includes a summary table.

## Value

A list with class "concurve" (see
[`curve_wrap`](reference/curve_wrap.md) for details).

## Details

This is a convenience function that constructs the appropriate `ci_func`
for [`curve_wrap`](reference/curve_wrap.md) based on the specified
method. For maximum flexibility, use `curve_wrap` directly.

Method selection guidelines:

- **Linear models (lm)**: "default" is appropriate and fast

- **GLMs**: "profile" is more accurate, especially for small samples

- **Mixed models**: "profile" recommended but can be slow

- **Robust models (rlm)**: "default" typically used

## See also

[`curve_wrap()`](reference/curve_wrap.md) for the generic wrapper

`curve_gen()` for the original model-based function

[`ggcurve()`](reference/ggcurve.md) for plotting

## Examples

``` r
if (FALSE) { # \dontrun{
# Linear model - Wald intervals (fast)
model <- lm(mpg ~ wt + hp, data = mtcars)
result <- curve_model(model, param = "wt", method = "default")
ggcurve(result[[1]], type = "c")

# GLM - Profile likelihood intervals (more accurate)
model <- glm(am ~ wt, data = mtcars, family = binomial)
result <- curve_model(model, param = "wt", method = "profile")

# Compare methods
wald <- curve_model(model, param = "wt", method = "default")
profile <- curve_model(model, param = "wt", method = "profile")
# Plot both to see the difference
} # }
```
