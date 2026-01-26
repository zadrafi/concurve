## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  message = TRUE,
  warning = TRUE,
  collapse = TRUE,
  comment = "#>"
)
library(concurve)

## ----eval=FALSE---------------------------------------------------------------
# install.packages("concurve", dep = TRUE)

## ----eval=FALSE---------------------------------------------------------------
# library(devtools)
# install_github("zadrafi/concurve")

## -----------------------------------------------------------------------------
library(concurve)
set.seed(1031)
GroupA <- rnorm(500)
GroupB <- rnorm(500)
RandomData <- data.frame(GroupA, GroupB)
object <- curve_mean(GroupA, GroupB,
  data = RandomData, method = "default"
)

## -----------------------------------------------------------------------------
head(object[[1]], 5)

## -----------------------------------------------------------------------------
head(object[[2]], 5)

## -----------------------------------------------------------------------------
head(object[[3]], 5)

## ----eval=FALSE---------------------------------------------------------------
# library(parallel)
# options(mc.cores = 1)

## ----eval=TRUE, include=TRUE, results = FALSE---------------------------------
library(bench)
library(parallel)
options(mc.cores = 1)
getOption("mc.cores", 1L)
set.seed(1031)
func1 <- mark(df1 <- curve_rev(
  point = 1.61, LL = 0.997, UL = 2.59,
  measure = "ratio", steps = 100
), memory = FALSE)

func2 <- mark(df1 <- curve_rev(
  point = 1.61, LL = 0.997, UL = 2.59,
  measure = "ratio", steps = 500000
), memory = FALSE)

## ----eval=TRUE, include=TRUE, results = FALSE---------------------------------

getOption("mc.cores", 1L)
set.seed(1031)
func3 <- mark(df1 <- curve_rev(
  point = 1.61, LL = 0.997, UL = 2.59,
  measure = "ratio", steps = 100
), memory = FALSE)

func4 <- mark(df1 <- curve_rev(
  point = 1.61, LL = 0.997, UL = 2.59,
  measure = "ratio", steps = 500000
), memory = FALSE)

## -----------------------------------------------------------------------------
func1$median
func2$median
func3$median
func4$median

## ----session_info, include=TRUE, echo=FALSE-----------------------------------
sessionInfo()

