#' Produce Tables For concurve Functions
#'
#' Produces publication-ready tables with relevant statistics of interest
#' for functions produced from the concurve package.
#'
#' @param data Dataframe from a concurve function to produce a table for
#' @param levels Levels of the consonance intervals or likelihood intervals that should be
#' included in the table.
#' @param type Indicates whether the table is for a consonance function or likelihood function.
#' The default is set to "c" for consonance and can be switched to "l" for likelihood.
#' @param format The format of the tables. The options include "data.frame" which is the
#' default, "docx" (which creates a table for a word document), "pptx" (which
#' creates a table for powerpoint), "latex", (which creates a table for a TeX document), and
#' "image", which produces an image of the table.
#'
#' @examples
#' curves <- curve_gen(lm(mpg ~ wt, data = mtcars), "wt")
#'
#' # Interval limits at conventional levels, with their P- and S-values
#' curve_table(curves[[1]], format = "data.frame")
#'
#' # The same table as LaTeX, for a manuscript
#' curve_table(curves[[1]], format = "latex")
#'
#' # "docx", "pptx", and "image" return flextable objects for Word,
#' # PowerPoint, and figure output respectively.
#' @seealso [ggcurve()]
#' @seealso [curve_compare()]
#' @seealso [plot_compare()]
#'
#' @export
curve_table <- function(data, levels, type = "c", format = "data.frame") {
  if (type == "c") {
    levels <- c(0.25, 0.50, 0.75, 0.80, 0.85, 0.90, 0.95, 0.975, 0.99)

    subdf <- parallel::mclapply(levels, FUN = function(i) (data[data$intrvl.level == i, ]), mc.cores = getOption("mc.cores", 1L))
    subdf <- data.frame(do.call(rbind, subdf))
    class(subdf) <- c("data.frame", "concurve")
    subdf$intrvl.level <- (subdf$intrvl.level * 100)
    subcolnames <- c("Lower Limit", "Upper Limit", "Interval Width", "Interval Level (%)", "CDF", "P-value", "S-value (bits)")
    colnames(subdf) <- subcolnames
    subdf <- round(subdf, digits = 3)
  } else if (type == "l") {
    levels <- c(0.03, 0.05, 0.12, 0.14)

    subdf <- parallel::mclapply(levels, FUN = function(i) (data[round(data$support, 2) == i, ]), mc.cores = getOption("mc.cores", 1L))
    subdf <- data.frame(do.call(rbind, subdf))
    class(subdf) <- c("data.frame", "concurve")
    subcolnames <- c("Theta", "Likelihood", "Log Likelihood", "Relative Likelihood", "Deviance Statistic")
    colnames(subdf) <- subcolnames
    subdf <- round(subdf, digits = 3)
  }

  if (format == "data.frame") {
    return(subdf)
  } else if (format == "docx") {
    subdf <- flextable::flextable(subdf)
    subdf <- flextable::autofit(subdf)
    subdf
    return(print(subdf, preview = "docx"))
  } else if (format == "pptx") {
    subdf <- flextable::flextable(subdf)
    subdf <- flextable::autofit(subdf)
    subdf
    return(print(subdf, preview = "pptx"))
  } else if (format == "latex") {
    subdf <- knitr::kable(
      subdf,
      booktabs = TRUE,
      label = "A table of some interval estimates at various levels and corresponding statistics."
    )
    return(subdf)
  } else if (format == "image") {
    subdf <- flextable::flextable(subdf)
    subdf <- flextable::autofit(subdf)
    subdf
    return(subdf)
  }
}

utils::globalVariables(c("subdf", "Lower Limit", "Upper Limit", "Interval Width", "Interval Level", "CDF", "P-value", "S-value"))
