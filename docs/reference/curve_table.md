# Produce Tables For concurve Functions

Produces publication-ready tables with relevant statistics of interest
for functions produced from the concurve package.

## Usage

``` r
curve_table(data, levels, type = "c", format = "data.frame")
```

## Arguments

- data:

  Dataframe from a concurve function to produce a table for

- levels:

  Levels of the consonance intervals or likelihood intervals that should
  be included in the table.

- type:

  Indicates whether the table is for a consonance function or likelihood
  function. The default is set to "c" for consonance and can be switched
  to "l" for likelihood.

- format:

  The format of the tables. The options include "data.frame" which is
  the default, "docx" (which creates a table for a word document),
  "pptx" (which creates a table for powerpoint), "latex", (which creates
  a table for a TeX document), and "image", which produces an image of
  the table.

## See also

[`ggcurve()`](reference/ggcurve.md)

[`curve_compare()`](reference/curve_compare.md)

[`plot_compare()`](reference/plot_compare.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(concurve)

GroupA <- rnorm(500)
GroupB <- rnorm(500)

RandomData <- data.frame(GroupA, GroupB)

intervalsdf <- curve_mean(GroupA, GroupB, data = RandomData, method = "default")

(z <- curve_table(intervalsdf[[1]], format = "data.frame"))
(z <- curve_table(intervalsdf[[1]], format = "latex"))
(z <- curve_table(intervalsdf[[1]], format = "image"))
} # }
```
