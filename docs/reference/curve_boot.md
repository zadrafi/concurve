# Generate Consonance Functions via Bootstrapping

Use the BCa bootstrap method and the t-bootstrap method from the bcaboot
and boot packages to generate consonance distributions.

## Usage

``` r
curve_boot(data, func, intrvl.level, method = "bca", t0 = NULL,
  tt = NULL, bb = NULL, replicates = 2000, steps = 1000,
  cores = getOption("mc.cores", 1L), table = TRUE)
```

## Arguments

- data:

  Dataset that is being used to create a consonance function.

- func:

  Custom function that is used to create parameters of interest that
  will be bootstrapped.

- method:

  The bootstrap method that will be used to generate the functions.
  Methods include "bca" which is the default, "bcapar", which is
  parametric bootstrapping using the BCa method and "t", for the
  t-bootstrap/percentile method.

- t0:

  Only used for the "bcapar" method. Observed estimate of theta, usually
  by maximum likelihood.

- tt:

  Only used for the "bcapar" method. A vector of parametric bootstrap
  replications of theta of length B, usually large, say B = 2000.

- bb:

  Only used for the "bcapar" method. A B by p matrix of natural
  sufficient vectors, where p is the dimension of the exponential
  family.

- replicates:

  Indicates how many bootstrap replicates are to be performed. The
  default is currently 2000 but more may be desirable, especially to
  make the functions more smooth.

- steps:

  Indicates how many consonance intervals are to be calculated at
  various levels. For example, setting this to 100 will produce 100
  consonance intervals from 0 to 100. Setting this to 10000 will produce
  more consonance levels. By default, it is set to 1000. Increasing the
  number substantially is not recommended as it will take longer to
  produce all the intervals and store them into a dataframe.

- cores:

  Select the number of cores to use in order to compute the intervals.
  The default is 1 core.

- table:

  Indicates whether or not a table output with some relevant statistics
  should be generated. The default is TRUE and generates a table which
  is included in the list object.

## Value

A list with class "concurve" containing:

- For method "bca": Standard Intervals, Standard Table (if table=TRUE),
  BCA Intervals, BCA Table (if table=TRUE), Bootstrap Statistics, BCA
  Statistics

- For method "bcapar": Same as "bca" plus BCA Density

- For method "t": Intervals Dataframe, Bootstrap Distribution, Intervals
  Density, Intervals Table (if table=TRUE)

## See also

[`curve_mean()`](reference/curve_mean.md),
[`curve_gen()`](reference/curve_gen.md),
[`bcaboot::bcajack()`](https://bnaras.github.io/bcaboot/reference/bcajack.html)

## Examples

``` r
if (FALSE) { # \dontrun{
# BCa bootstrap for a mean
my_func <- function(x) mean(x)
data <- rnorm(100, mean = 5, sd = 2)
result <- curve_boot(data = data, func = my_func, method = "bca")
ggcurve(result[["BCA Intervals"]], type = "c")

# t-bootstrap method
my_stat <- function(data, indices) {
  mean(data[indices])
}
result_t <- curve_boot(data = data, func = my_stat, method = "t")
ggcurve(result_t[[1]], type = "c")
} # }
```
