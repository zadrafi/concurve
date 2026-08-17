# Summarize a likelihood function object

Summarize a likelihood function object

## Usage

``` r
# S3 method for class 'likelihood_function'
summary(object, ...)
```

## Arguments

- object:

  A `likelihood_function` object

- ...:

  Additional arguments (unused)

## Value

A `summary.likelihood_function` object containing coefficient estimates,
standard errors, z-values, p-values, dispersion, log-likelihood, AIC,
and BIC.

## Details

Computes the variance-covariance matrix from the information matrix at
the MLE and provides a summary table with estimates, standard errors,
and Wald test statistics.
