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

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
(curve_compare(
  data1 = intervalsdf[[1]], data2 = randomframe[[1]], type = "s",
  plot = TRUE, measure = "default", nullvalue = FALSE
))

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
library(carData)
Rossi[1:5, 1:10]

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
library(survival)
mod.allison <- coxph(Surv(week, arrest) ~
fin + age + race + wexp + mar + paro + prio,
data = Rossi
)
mod.allison

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
z <- curve_surv(mod.allison, "prio")

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
ggcurve(z[[1]], measure = "ratio", nullvalue = TRUE)
ggcurve(z[[2]], type = "cd", measure = "ratio", nullvalue = TRUE)
curve_table(z[[1]], format = "image")

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
x <- curve_surv(mod.allison, "age")
ggcurve(x[[1]], measure = "ratio")
ggcurve(x[[2]], type = "cd", measure = "ratio")
curve_table(x[[1]], format = "image")

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
library(metafor)
dat.hine1989

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
dat <- escalc(measure = "RD", n1i = n1i, n2i = n2i, ai = ai, ci = ci, data = dat.hine1989)
dat

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
dat$yi <- dat$yi * 100
dat$vi <- dat$vi * 100^2

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
fe <- rma(yi, vi, data = dat, method = "FE")

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
fecurve <- curve_meta(fe)

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
ggcurve(fecurve[[1]], nullvalue = TRUE)

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
re <- rma(yi, vi, data = dat, method = "REML")

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
recurve <- curve_meta(re)

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
ggcurve(recurve[[1]], nullvalue = TRUE)

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
curve_compare(fecurve[[1]], recurve[[1]], plot = TRUE)

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

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
ggcurve(df[[5]], type = "cd", nullvalue = FALSE)

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
library(ProfileLikelihood)

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
data(dataglm)
xx <- profilelike.glm(y ~ x1 + x2,
  data = dataglm, profile.theta = "group",
  family = binomial(link = "logit"), length = 500, round = 2
)

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
lik <- curve_lik(xx, dataglm)
tibble::tibble(lik[[1]])

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
ggcurve(lik[[1]], type = "l1", nullvalue = TRUE)
ggcurve(lik[[1]], type = "l2")
ggcurve(lik[[1]], type = "l3")
ggcurve(lik[[1]], type = "d")

