# Generate Consonance Functions via Bootstrapping

Use the Bca bootstrap method and the t-boostrap method from the bcaboot
and boot packages to generate consonance distrbutions.

## Usage

``` r
curve_boot(data = data, func = func, method = "bca", t0, tt, bb,
  replicates = 2000, steps = 1000, cores = getOption("mc.cores", 1L),
  table = TRUE)
```

## Arguments

- data:

  Dataset that is being used to create a consonance function.

- func:

  Custom function that is used to create parameters of interest that
  will be bootstrapped.

- method:

  The boostrap method that will be used to generate the functions.
  Methods include "bca" which is the default, "bcapar", which is
  parametric bootstrapping using the bca method and "t", for the
  t-bootstrap/percentile method.

- t0:

  Only used for the "bcapar" method. Observed estimate of theta, usually
  by maximum likelihood.

- tt:

  Only used for the "bcapar" method. A vector of parametric bootstrap
  replications of theta of length B, usually large, say B = 2000

- bb:

  Only used for the "bcapar" method. A B by p matrix of natural
  sufficient vectors, where p is the dimension of the exponential
  family.

- replicates:

  Indicates how many bootstrap replicates are to be performed. The
  default is currently 20000 but more may be desirable, especially to
  make the functions more smooth.

- steps:

  Indicates how many consonance intervals are to be calculated at
  various levels. For example, setting this to 100 will produce 100
  consonance intervals from 0 to 100. Setting this to 10000 will produce
  more consonance levels. By default, it is set to 1000. Increasing the
  number substantially is not recommended as it will take longer to
  produce all the intervals and store them into a dataframe.

- cores:

  Select the number of cores to use in order to compute the intervals
  The default is 1 core.

- table:

  Indicates whether or not a table output with some relevant statistics
  should be generated. The default is TRUE and generates a table which
  is included in the list object.

## Value

A list with 7 items where the dataframe of standard values is in the
first list and the table for it in the second if table = TRUE. The Bca
intervals and table are found in the third and fourth list. The values
for the density function are in the fifth object, while the Bca stats
are in the sixth and seventh objects.
