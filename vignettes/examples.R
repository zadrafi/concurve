## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  message = TRUE,
  warning = TRUE,
  collapse = TRUE,
  comment = "#>"
)

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
library(concurve)
set.seed(1031)
GroupA <- rnorm(500)
GroupB <- rnorm(500)
RandomData <- data.frame(GroupA, GroupB)

## -----------------------------------------------------------------------------
intervalsdf <- curve_mean(GroupA, GroupB,
  data = RandomData, method = "default"
)

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
head(intervalsdf[[1]], 10)

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
(function1 <- ggcurve(data = intervalsdf[[1]], type = "c", nullvalue = TRUE))

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
(function1 <- ggcurve(data = intervalsdf[[1]], type = "s"))

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
(function1s <- ggcurve(data = intervalsdf[[2]], type = "cdf", nullvalue = TRUE))

## ----echo=TRUE, fig.height=2, fig.width=4-------------------------------------
(x <- curve_table(data = intervalsdf[[1]], format = "image"))

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
GroupA2 <- rnorm(500)
GroupB2 <- rnorm(500)
RandomData2 <- data.frame(GroupA2, GroupB2)
model <- lm(GroupA2 ~ GroupB2, data = RandomData2)
randomframe <- curve_gen(model, "GroupB2")

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
(function2 <- ggcurve(type = "c", randomframe[[1]], levels = c(0.50, 0.75, 0.95), nullvalue = TRUE))

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
(curve_compare(
  data1 = intervalsdf[[1]], data2 = randomframe[[1]], type = "c",
  plot = TRUE, measure = "default", nullvalue = TRUE
))

## ----echo=TRUE----------------------------------------------------------------
cowplot::plot_grid(function1, function2)

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
(curve_compare(
  data1 = intervalsdf[[1]], data2 = randomframe[[1]], type = "s",
  plot = TRUE, measure = "default", nullvalue = FALSE
))

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
curve1 <- curve_rev(point = 1.7, LL = 1.1, UL = 2.6, type = "c", measure = "ratio", steps = 10000)
(ggcurve(data = curve1[[1]], type = "c", measure = "ratio", nullvalue = TRUE))
curve2 <- curve_rev(point = 1.61, LL = 0.997, UL = 2.59, type = "c", measure = "ratio", steps = 10000)
(ggcurve(data = curve2[[1]], type = "c", measure = "ratio", nullvalue = TRUE))

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
lik1 <- curve_rev(point = 1.7, LL = 1.1, UL = 2.6, type = "l", measure = "ratio", steps = 10000)
(ggcurve(data = lik1[[1]], type = "l1", measure = "ratio", nullvalue = TRUE))
lik2 <- curve_rev(point = 1.61, LL = 0.997, UL = 2.59, type = "l", measure = "ratio", steps = 10000)
(ggcurve(data = lik2[[1]], type = "l1", measure = "ratio", nullvalue = TRUE))

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
(plot_compare(
  data1 = lik1[[1]], data2 = lik2[[1]], type = "l1", measure = "ratio", nullvalue = TRUE, title = "Brown et al. 2017. J Clin Psychiatry. vs. \nBrown et al. 2017. JAMA.",
  subtitle = "J Clin Psychiatry: OR = 1.7, 1/6.83 LI: LL = 1.1, UL = 2.6 \nJAMA: HR = 1.61, 1/6.83 LI: LL = 0.997, UL = 2.59", xaxis = expression(Theta ~ "= Hazard Ratio / Odds Ratio")
))

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
(plot_compare(
  data1 = curve1[[1]], data2 = curve2[[1]], type = "c", measure = "ratio", nullvalue = TRUE, title = "Brown et al. 2017. J Clin Psychiatry. vs. \nBrown et al. 2017. JAMA.",
  subtitle = "J Clin Psychiatry: OR = 1.7, 1/6.83 LI: LL = 1.1, UL = 2.6 \nJAMA: HR = 1.61, 1/6.83 LI: LL = 0.997, UL = 2.59", xaxis = expression(Theta ~ "= Hazard Ratio / Odds Ratio")
))

