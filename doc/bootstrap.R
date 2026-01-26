## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  message = TRUE,
  warning = TRUE,
  collapse = TRUE,
  comment = "#>"
)
library(concurve)

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
ggcurve(data = y[[1]], nullvalue = TRUE)
ggcurve(data = y[[3]], nullvalue = TRUE)

## ----echo=TRUE, fig.height=2, fig.width=4-------------------------------------
(gg <- curve_table(data = y[[1]], format = "image"))

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
plot_compare(y[[1]], y[[3]])

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
knitr::kable(y[[5]])

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
library(Lock5Data)
dataz <- data(CommuteAtlanta)
func <- function(data, index) {
  x <- as.numeric(unlist(data[1]))
  y <- as.numeric(unlist(data[2]))
  return(mean(x[index]) - mean(y[index]))
}

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
z <- curve_boot(data = CommuteAtlanta, func = func, method = "t", replicates = 2000, steps = 1000)
ggcurve(data = z[[1]], nullvalue = FALSE)
ggcurve(data = z[[2]], type = "cd", nullvalue = FALSE)

## ----echo=TRUE, fig.height=2, fig.width=4-------------------------------------
(zz <- curve_table(data = z[[1]], format = "image"))

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
data(diabetes, package = "bcaboot")
X <- diabetes$x
y <- scale(diabetes$y, center = TRUE, scale = FALSE)
lm.model <- lm(y ~ X - 1)
mu.hat <- lm.model$fitted.values
sigma.hat <- stats::sd(lm.model$residuals)
t0 <- summary(lm.model)$adj.r.squared
y.star <- sapply(mu.hat, rnorm, n = 1000, sd = sigma.hat)
tt <- apply(y.star, 1, function(y) summary(lm(y ~ X - 1))$adj.r.squared)
b.star <- y.star %*% X

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
df <- curve_boot(method = "bcapar", t0 = t0, tt = tt, bb = b.star)

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
ggcurve(df[[1]], nullvalue = FALSE)
ggcurve(df[[3]], nullvalue = FALSE)

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
plot_compare(df[[1]], df[[3]])

## -----------------------------------------------------------------------------
citation("concurve")
citation("boot")
citation("bcaboot")

