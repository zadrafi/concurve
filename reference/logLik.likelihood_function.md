# Extract log-likelihood from a likelihood function

Extract log-likelihood from a likelihood function

## Usage

``` r
# S3 method for class 'likelihood_function'
logLik(object, ...)
```

## Arguments

- object:

  A `likelihood_function` object

- ...:

  Additional arguments (unused)

## Value

An object of class `logLik` containing the log-likelihood at the MLE,
with attributes `df` (number of parameters) and `nobs` (number of
observations)

## Details

Returns the log-likelihood evaluated at the maximum likelihood
estimates. The value includes attributes that make it compatible with
information criteria calculations (AIC, BIC, etc.).
