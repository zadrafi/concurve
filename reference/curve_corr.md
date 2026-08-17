# Consonance Functions for Correlations

Computes consonance intervals to produce P- and S-value functions for
correlational analysesusing the cor.test function in base R and places
the interval limits for each interval levelinto a data frame along with
the corresponding p-values and s-values.

## Usage

``` r
curve_corr(x, y, alternative, method, steps = 10000,
  cores = getOption("mc.cores", 1L), table = TRUE)
```

## Arguments

- x:

  A vector that contains the data for one of the variables that will be
  analyzed for correlational analysis.

- y:

  A vector that contains the data for one of the variables that will be
  analyzed for correlational analysis.

- alternative:

  Indicates the alternative hypothesis and must be one of "two.sided",
  "greater" or "less". You can specify just the initial letter.
  "greater" corresponds to positive association, "less" to negative
  association.

- method:

  A character string indicating which correlation coefficient is to be
  used for the test. One of "pearson", "kendall", or "spearman", can be
  abbreviated.

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

A list with 3 items where the dataframe of values is in the first
object, the values needed to calculate the density function in the
second, and the table for the values in the third if table = TRUE.

## Examples

``` r
if (FALSE) { # \dontrun{
GroupA <- rnorm(50)
GroupB <- rnorm(50)
joe <- curve_corr(x = GroupA, y = GroupB, alternative = "two.sided", method = "pearson")
} # }
```
