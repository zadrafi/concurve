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
tibble::tibble(intervalsdf[[1]])

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
(function1 <- ggcurve(data = intervalsdf[[1]], type = "c"))

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
(function1 <- ggcurve(data = intervalsdf[[1]], type = "s"))

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
(function1s <- ggcurve(data = intervalsdf[[2]], type = "cdf"))

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

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
(curve_compare(
  data1 = intervalsdf[[1]], data2 = randomframe[[1]], type = "s",
  plot = TRUE, measure = "default", nullvalue = FALSE
))

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
curve1 <- curve_rev(point = 1.7, LL = 1.1, UL = 2.6, type = "c", measure = "ratio", steps = 10000)
(ggcurve(data = curve1[[1]], type = "c", measure = "ratio", nullvalue = TRUE))
curve2 <- curve_rev(point = 1.61, LL = 0.997, UL = 2.59,type = "c", measure = "ratio", steps = 10000)
(ggcurve(data = curve2[[1]], type = "c", measure = "ratio", nullvalue = TRUE))

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
lik1 <- curve_rev(point = 1.7, LL = 1.1, UL = 2.6, type = "l", measure = "ratio", steps = 10000)
(ggcurve(data = lik1[[1]], type = "l1", measure = "ratio", nullvalue = TRUE))
lik2 <- curve_rev(point = 1.61, LL = 0.997, UL = 2.59,type = "l", measure = "ratio", steps = 10000)
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

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
iris <- datasets::iris
foo <- function(data, indices) {
  dt <- data[indices, ]
  c(
    cor(dt[, 1], dt[, 2], method = "p")
  )
}

## ----include=FALSE------------------------------------------------------------
y <- curve_boot(data = iris, func = foo, method = "bca", replicates = 2000, steps = 1000)

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
ggcurve(data = y[[1]])
ggcurve(data = y[[3]])

## ----echo=TRUE, fig.height=2, fig.width=4-------------------------------------
(gg <- curve_table(data = y[[1]], format = "image"))

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
plot_compare(y[[1]], y[[3]])

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
library(Lock5Data)
dataz <- data(CommuteAtlanta)
func = function(data, index) {
  x <- as.numeric(unlist(data[1]))
  y <- as.numeric(unlist(data[2]))
  return(mean(x[index]) - mean(y[index]))
}

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
z <- curve_boot(data = CommuteAtlanta, func = func, method = "t", replicates = 2000, steps = 1000)
ggcurve(data = z[[1]])
ggcurve(data = z[[2]], type = "cd")

## ----echo=TRUE, fig.height=2, fig.width=4-------------------------------------
(zz <- curve_table(data = z[[1]], format = "image"))

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
library(ProfileLikelihood)

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
data(dataglm)
xx <- profilelike.glm(y ~ x1 + x2, data=dataglm, profile.theta="group",
family=binomial(link="logit"), length=500, round=2)

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
lik <- curve_lik(xx, dataglm)
tibble::tibble(lik[[1]])

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
ggcurve(lik[[1]], type = "l1")
ggcurve(lik[[1]], type = "l2")
ggcurve(lik[[1]], type = "l3")
ggcurve(lik[[1]], type = "d")

