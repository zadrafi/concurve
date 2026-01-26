## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  message = TRUE,
  warning = TRUE,
  collapse = TRUE,
  comment = "#>"
)

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
library(concurve)
GroupA <- rnorm(500)
GroupB <- rnorm(500)
RandomData <- data.frame(GroupA, GroupB)

intervalsdf <- curve_mean(GroupA, GroupB,
  data = RandomData, method = "default"
)

## ----echo=TRUE, fig.height=2, fig.width=4-------------------------------------
(x <- curve_table(data = intervalsdf[[1]], format = "image"))

## ----echo=TRUE, fig.height=2, fig.width=4-------------------------------------
(z <- curve_table(intervalsdf[[1]], format = "latex"))

## ----echo=TRUE, fig.height=2, fig.width=4-------------------------------------
(df <- curve_table(intervalsdf[[1]], format = "data.frame"))

## -----------------------------------------------------------------------------
citation("concurve")
citation("flextable")
citation("officer")

