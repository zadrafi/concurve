## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  message = TRUE,
  warning = TRUE,
  collapse = TRUE,
  comment = "#>"
)
library(concurve)
library(lme4)
library(daewr)

## -----------------------------------------------------------------------------
(confint.merMod(fm1 <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy, REML = TRUE)))

## ----eval=TRUE, include=TRUE--------------------------------------------------
library(concurve)
object1 <- suppressMessages(curve_lmer(object = fm1, parm = "Days", method = "profile", steps = 100))

sample1 <- suppressMessages(curve_lmer(object = fm1, parm = ".sig01", method = "profile", steps = 100))

ggcurve(data = sample1[[1]], type = "c", measure = "default")

## -----------------------------------------------------------------------------
fm1M <- lmer( yield ~ 1 + (1| sample), data = Naph, REML = TRUE)

summary(fm1M)

## ----eval=TRUE, include=TRUE--------------------------------------------------
sample2 <- suppressMessages(curve_lmer(object = fm1M, parm = ".sig01", method = "profile", steps = 100))

ggcurve(data = sample2[[1]], type = "c", measure = "default")

## ----eval=TRUE, include=TRUE--------------------------------------------------
c1 <- c( .5, -.5)

mod4 <- lmer( pl ~ 1 + Group + (1|Subject:Group) + Period + Treat, contrasts = list(Group = c1, Period = c1, Treat = c1), data = antifungal)

summary(mod4)

## ----eval=TRUE, include=TRUE--------------------------------------------------
crossed <- suppressMessages(curve_lmer(object = mod4, parm = "Period1", method = "profile"))

ggcurve(data = crossed[[1]], type = "c")

## ----session_info, include=TRUE, echo=FALSE-----------------------------------
sessionInfo()

