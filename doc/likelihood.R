## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  message = TRUE,
  warning = TRUE,
  collapse = TRUE,
  comment = "#>"
)
library(concurve)

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
library(ProfileLikelihood)

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
data(dataglm)
xx <- profilelike.glm(y ~ x1 + x2,
  data = dataglm, profile.theta = "group",
  family = binomial(link = "logit"), length = 500, round = 2
)

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
lik <- curve_lik(xx, data = dataglm)

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
ggcurve(lik[[1]], type = "l1", nullvalue = TRUE)
ggcurve(lik[[1]], type = "l2")
ggcurve(lik[[1]], type = "l3")
ggcurve(lik[[1]], type = "d")

## -----------------------------------------------------------------------------
citation("concurve")
citation("ProfileLikelihood")

