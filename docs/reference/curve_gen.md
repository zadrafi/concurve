# Consonance Functions For Linear Models, Generalized Linear Models, and Robust Linear Models

Computes thousands of consonance (confidence) intervals for the chosen
parameter in the selected model (linear models, general linear models,
robust linear models, and generalized least squares and places the
interval limits for each interval level into a data frame along with the
corresponding p-values and s-values. Can also adjust for multiple
comparisons. It is generally recommended to wrap this function using
suppressMessages() due to the long list of profiling messages.

## Usage

``` r
curve_gen(model, var, method = "lm", log = FALSE, penalty = NULL,
  m = NULL, steps = 1000, table = TRUE)
```

## Arguments

- model:

  The statistical model of interest (ANOVA, regression, logistic
  regression) is to be indicated here.

- var:

  The variable of interest from the model (coefficients, intercept) for
  which the intervals are to be produced.

- method:

  Chooses the method to be used to calculate the consonance intervals.
  There are currently five methods: "lm", rms::ols objects can be used
  with the "lm" option, "rlm", "glm" and "aov", and "gls". The "lm"
  method uses the profile likelihood method to compute intervals and can
  be used for models created by the 'lm' function. It is typically what
  most people are familiar with when computing intervals based on the
  calculated standard error. The ols function from the rms package can
  also be used for this option. The "rlm" method is designed for usage
  with the "rlm" function from the MASS package. The "glm" method allows
  this function to be used for specific scenarios like logistic
  regression and the 'glm' function. Similarly, the Glm function from
  the rms package can also be used for this option. The gls method
  allows objects from gls() or from Gls() from the rms package.

- log:

  Determines whether the coefficients will be exponentiated or not. By
  default, it is off and set to FALSE or F, but changing this to TRUE or
  T, will exponentiate the results which may be useful if trying to view
  the results from a logistic regression on a scale that is not
  logarithmic.

- penalty:

  An input to specify whether the confidence intervals should be
  corrected for multiple comparisons. The default is NULL, so there is
  no correction. Other options include "bonferroni" and "sidak".

- m:

  Indicates how many comparisons are being done and the number that
  should be used to correct for multiple comparisons. The default is
  NULL.

- steps:

  Indicates how many consonance intervals are to be calculated at
  various levels. For example, setting this to 100 will produce 100
  consonance intervals from 0 to 100. Setting this to 10000 will produce
  more consonance levels. By default, it is set to 1000. Increasing the
  number substantially is not recommended as it will take longer to
  produce all the intervals and store them into a data frame.

- table:

  Indicates whether or not a table output with some relevant statistics
  should be generated. The default is TRUE and generates a table which
  is included in the list object.

- cores:

  Select the number of cores to use in order to compute the intervals
  The default is 1 core.

## Value

A list with 3 items where the dataframe of values is in the first
object, the values needed to calculate the density function in the
second, and the table for the values in the third if table = TRUE.

## Examples

``` r
if (FALSE) { # \dontrun{
# Simulate random data
GroupA <- rnorm(50)
GroupB <- rnorm(50)
RandomData <- data.frame(GroupA, GroupB)
rob <- lm(GroupA ~ GroupB, data = RandomData)
bob <- curve_gen(rob, "GroupB")
} # }
```
