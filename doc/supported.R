## ----setup, include=FALSE-----------------------------------------------------
library(knitr)
knitr::opts_chunk$set(
  message = TRUE,
  warning = TRUE,
  collapse = TRUE,
  comment = "#>")
library(magrittr)
library(kableExtra)
library(magick)

## ----eval=FALSE---------------------------------------------------------------
# install_github("zadrafi/concurve@master", dependencies = TRUE)

## ----echo=FALSE---------------------------------------------------------------
versions <-  c(2.77, 2.76, 2.75, 2.70, 2.60)
support <- c(rep("-", 5))
information <- c(
  "[Updates and Information about version 2.7.7](https://data.lesslikely.com/concurve/news/index.html#concurve-2-7-7-unreleased)",
  "[Updates and Information about version 2.7.6](https://data.lesslikely.com/concurve/news/index.html#concurve-2-7-6-unreleased)",
  "[Updates and Information about version 2.7.5](https://data.lesslikely.com/concurve/news/index.html#concurve-2-7-5-unreleased)",
  "[Updates and Information about version 2.7.0](https://data.lesslikely.com/concurve/news/index.html#concurve-2-7-0-unreleased)",
  "[Updates and Information about version 2.6.0](https://data.lesslikely.com/concurve/news/index.html#concurve-2-6-0-unreleased)"
)

check <- ("https://res.cloudinary.com/less-likely/image/upload/v1602195577/Site/checkmark.png")
cross <- ("https://res.cloudinary.com/less-likely/image/upload/v1602195577/Site/crossmark.png")

df <- data.frame(versions, support, information)
colnames(df) <- c("Package Version", "Currently Supported", "Information About Version")

kable(df, booktabs = F, format = "html", col.names = colnames(df), table.attr = "class=\"striped\"") %>%
  column_spec(2, width = "20em", bold = TRUE, image = spec_image(c(rep(check, 4), cross), width = 150, height = 150, res = 300)) %>%
  column_spec(1, width = "15em", bold = TRUE) %>%
  column_spec(3, width = "40em", bold = TRUE) %>%
  kable_styling(full_width = TRUE, "striped", font_size = 20, position = "right", html_font = "helvetica")

## ----echo=FALSE---------------------------------------------------------------
packinfo <- installed.packages(fields = c("Package", "Version"))


# Package imports
Imports <- c(
  "MASS", "methods", "bcaboot", "boot", "lme4", "dplyr", "flextable", "ggplot2",
  "knitr", "metafor", "officer", "parallel", "pbmcapply", "ProfileLikelihood",
  "scales", "colorspace", "survival", "survminer", "tibble", "tidyr"
)

Imports <- packinfo[Imports, c("Package", "Version")]

colnames(Imports) <- c("Package", "Package Version")

kable(Imports, booktabs = T, format = "html", col.names = colnames(Imports), table.attr = "class=\"striped\"", 
caption = "Package Imports") %>%
  kable_styling(full_width = TRUE, "striped", font_size = 10, position = "center", html_font = "helvetica")


## ----echo=FALSE---------------------------------------------------------------
# Package suggests
suggests <- c(
  "covr", "roxygen2", "spelling", "testthat", "rmarkdown", "Lock5Data",
  "carData", "bench", "rms", "brms", "rstanarm", "bayesplot",
  "vdiffr", "ggtext", "daewr", "svglite", "data.table", "nlme",
  "simstudy"
)

suggests_df <- packinfo[suggests, c("Package", "Version")]

colnames(suggests_df) <- c("Package", "Package Version")

kable(suggests_df, booktabs = T, format = "html", col.names = colnames(suggests_df), table.attr = "class=\"striped\"", 
caption = "Package Suggestions") %>%
  kable_styling(full_width = TRUE, "striped", font_size = 10, position = "center", html_font = "helvetica")


## ----session_info, include=TRUE, echo=FALSE-----------------------------------
sessionInfo()

