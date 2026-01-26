# Compare Two Functions and Produces An AUC Score

Compares the p-value/s-value, and likelihood functions and computes an
AUC number.

## Usage

``` r
curve_compare(data1, data2, type = "c", plot = TRUE, ...)
```

## Arguments

- data1:

  The first dataframe produced by one of the interval functions in which
  the intervals are stored.

- data2:

  The second dataframe produced by one of the interval functions in
  which the intervals are stored.

- type:

  Choose whether to plot a "consonance" function, a "surprisal" function
  or "likelihood". The default option is set to "c". The type must be
  set in quotes, for example curve_compare (type = "s") or
  curve_compare(type = "c"). Other options include "pd" for the
  consonance distribution function, and "cd" for the consonance density
  function, "l1" for relative likelihood, "l2" for log-likelihood, "l3"
  for likelihood and "d" for deviance function.

- plot:

  by default it is set to TRUE and will use the plot_compare() function
  to plot the two functions.

- ...:

  Can be used to pass further arguments to plot_compare().

## Value

Computes an AUC score and returns a plot that graphs two functions.

## See also

[`plot_compare()`](reference/plot_compare.md)

[`ggcurve()`](reference/ggcurve.md)

[`curve_table()`](reference/curve_table.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(concurve)
GroupA <- rnorm(50)
GroupB <- rnorm(50)
RandomData <- data.frame(GroupA, GroupB)
intervalsdf <- curve_mean(GroupA, GroupB, data = RandomData)
GroupA2 <- rnorm(50)
GroupB2 <- rnorm(50)
RandomData2 <- data.frame(GroupA2, GroupB2)
model <- lm(GroupA2 ~ GroupB2, data = RandomData2)
randomframe <- curve_gen(model, "GroupB2")
curve_compare(intervalsdf[[1]], randomframe[[1]])
} # }
```
