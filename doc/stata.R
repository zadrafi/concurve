## ----setup, include=FALSE-----------------------------------------------------
require(Statamarkdown)
statapath = ("/Applications/Stata/StataIC.app/Contents/MacOS/StataIC")
knitr::opts_chunk$set(engine.path=list(stata=statapath))

## -----------------------------------------------------------------------------
citation("Statamarkdown")

## ----session_info, include=TRUE, echo=FALSE-----------------------------------
sessionInfo()

