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
intervalsdf <- curve_mean(GroupA, GroupB,
  data = RandomData, method = "default"
)
(function1 <- ggcurve(data = intervalsdf[[1]], type = "c", nullvalue = TRUE))

## -----------------------------------------------------------------------------
library(ggplot2)
function1 +
  labs(
    title = "Random Title",
    subtitle = "Random Subtitle",
    x = "x-axis",
    y = "y-axis",
    caption = "Custom Caption"
  )

## -----------------------------------------------------------------------------
library(cowplot)
logo_file <- "https://res.cloudinary.com/less-likely/image/upload/v1575441662/Site/Logo2.jpg"

function1 <- function1 +
  theme_cowplot()

function2 <- ggdraw(function1) +
  draw_image(logo_file, x = 1, y = 1, hjust = 2, vjust = 1.75, width = 0.13, height = 0.2)

function2

## -----------------------------------------------------------------------------

(function1 <- ggcurve(data = intervalsdf[[1]], type = "c", nullvalue = TRUE))

## -----------------------------------------------------------------------------
(function1 <- ggcurve(data = intervalsdf[[1]], type = "c", nullvalue = TRUE, title = "Something Super Important", xaxis = "Theta"))

## -----------------------------------------------------------------------------

library(ggtext)

function1 <- ggcurve(data = intervalsdf[[1]], type = "c", nullvalue = TRUE, title = "Something Super Important", xaxis = "<span style = 'color:#3f8f9b;'>Theta</span> ")

function1 +
  labs(
    title = "*P*-value Function / Consonance Curve<br><span style = 'font-size:9pt;'>
The function below contains <span style = 'color:#3f8f9b;'>nested</span> confidence/compatibility intervals at <span style = 'color:#3f8f9b;'>every possible level</span> (95%, 90%, 75%, 50%)
allowing one to see a range of estimates that are <span style = 'color:#3f8f9b;'>consistent with the model and its assumptions</span> that were used to compute
the test statistics, *P*-values, and interval estimates. </span>",
    subtitle = NULL
  ) +
  theme(
    plot.title = element_textbox_simple(
      size = 11, lineheight = 1.1,
      linetype = 1, # turn on border
      box.color = "#748696", # border color
      fill = "white", # background fill color
      r = grid::unit(3, "pt"),
      padding = margin(8, 8, 8, 8), # padding around text inside the box
      maxwidth = unit(8, "in"), # margin outside the box
    ),
    axis.text = element_text(size = 10),
    axis.title.y = element_text(size = 11),
    axis.title.x = element_textbox_simple(
      size = 10,
      width = NULL,
      padding = margin(4, 4, 4, 4),
      margin = margin(4, 0, 0, 0),
      linetype = 1,
      r = grid::unit(3, "pt"),
      box.color = "#748696", # border color
      fill = "white", # background fill color
    )
  )

## -----------------------------------------------------------------------------
save_plot("function2.pdf", function2)

## -----------------------------------------------------------------------------
library(svglite)
library(ggplot2)

res <- 144
svglite("pvalfunc.svg", width = 720/res, height = 500/res)
(function1 <- ggcurve(data = intervalsdf[[1]], type = "c", nullvalue = TRUE))
dev.off()

## -----------------------------------------------------------------------------
citation("concurve")
citation("ggplot2")
citation("ggtext")
citation("cowplot")

