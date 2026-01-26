## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  message = TRUE,
  warning = TRUE,
  collapse = TRUE,
  comment = "#>"
)

## ----echo=TRUE, fig.height=4.5, fig.width=6-----------------------------------
library(concurve)
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

## -----------------------------------------------------------------------------
citation("concurve")
citation("survival")
citation("carData")
citation("survminer")

