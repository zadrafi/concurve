# Print a likelihood function object

Print a likelihood function object

## Usage

``` r
# S3 method for class 'likelihood_function'
print(x, ...)
```

## Arguments

- x:

  A `likelihood_function` object

- ...:

  Additional arguments (unused)

## Value

Invisibly returns `x`

## Details

Displays a summary of the likelihood function including family, link
function, number of observations and parameters, convergence status,
maximum likelihood estimates, and log-likelihood at the MLE.
