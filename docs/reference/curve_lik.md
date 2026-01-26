# Compute Profile Likelihood Functions

Compute Profile Likelihood Functions

## Usage

``` r
curve_lik(likobject, data, table = TRUE)
```

## Arguments

- likobject:

  An object from the ProfileLikelihood package

- data:

  The dataframe that was used to create the likelihood object in the
  ProfileLikelihood package.

- table:

  Indicates whether or not a table output with some relevant statistics
  should be generated. The default is TRUE and generates a table which
  is included in the list object.

## Value

A list with 2 items where the dataframe of values is in the first
object, and the table for the values in the second if table = TRUE.

## Examples

``` r
library(ProfileLikelihood)
data(dataglm)
xx <- profilelike.glm(y ~ x1 + x2, dataglm, profile.theta = "group", binomial("logit"))
#> Warning message: provide lo.theta and hi.theta 
lik <- curve_lik(xx, dataglm)
```
