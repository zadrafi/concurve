## ----setup, include=FALSE-----------------------------------------------------
statapath <- "/Applications/Stata/StataIC.app/Contents/MacOS/StataIC"
stata_available <- file.exists(statapath) &&
  requireNamespace("Statamarkdown", quietly = TRUE)

if (stata_available) {
  library(Statamarkdown)
  knitr::opts_chunk$set(engine.path = list(stata = statapath))
}

# Disable stata chunks when Stata is not available on this machine
knitr::opts_hooks$set(engine = function(options) {
  if (identical(options$engine, "stata") && !stata_available) {
    options$eval <- FALSE
  }
  options
})

## -----------------------------------------------------------------------------
citation("Statamarkdown")

## ----session_info, include=TRUE, echo=FALSE-----------------------------------
sessionInfo()

