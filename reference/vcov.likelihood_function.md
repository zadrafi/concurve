# Extract variance-covariance matrix from a likelihood function

Extract variance-covariance matrix from a likelihood function

## Usage

``` r
# S3 method for class 'likelihood_function'
vcov(object, ...)
```

## Arguments

- object:

  A `likelihood_function` object

- ...:

  Additional arguments (unused)

## Value

A symmetric matrix of covariances between parameter estimates

## Details

Computes the variance-covariance matrix as the inverse of the observed
information matrix at the MLE. For Gaussian models, uses the unbiased
estimator of variance (SSR/(n-p)) for compatibility with
[`vcov.lm`](https://rdrr.io/r/stats/vcov.html).
