# Confidence intervals for likelihood function parameters

Confidence intervals for likelihood function parameters

## Usage

``` r
# S3 method for class 'likelihood_function'
confint(object, parm = NULL, level = 0.95,
  ...)
```

## Arguments

- object:

  A `likelihood_function` object

- parm:

  A character vector of parameter names for which to compute intervals.
  If `NULL`, intervals are computed for all parameters.

- level:

  Confidence level (default: 0.95)

- ...:

  Additional arguments (unused)

## Value

A matrix with two columns containing the lower and upper confidence
limits for each parameter. Column names indicate the confidence level.

## Details

Uses profile likelihood methodology to construct confidence intervals.
The intervals are obtained by finding where the profile log-likelihood
drops by \\\chi^2\_{1,\alpha}/2\\ from its maximum.
