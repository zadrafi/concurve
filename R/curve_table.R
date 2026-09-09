#' Produce Tables For concurve Functions
#'
#' Produces publication-ready tables with relevant statistics of interest
#' for functions produced from the concurve package.
#'
#' @param data Dataframe from a concurve function to produce a table for
#' @param levels Levels of the consonance intervals or likelihood intervals
#' that should be included in the table. If `NULL`, the default, a
#' conventional set is used: 0.25, 0.50, 0.75, 0.80, 0.85, 0.90, 0.95, 0.975
#' and 0.99 for `type = "c"`, and 0.03, 0.05, 0.12 and 0.14 for
#' `type = "l"`. Values must match levels actually present in `data`, which
#' depends on the `steps` used to build it; any supplied level that matches
#' no row is omitted, with a warning naming it.
#' @param type Indicates whether the table is for a consonance function or likelihood function.
#' The default is set to "c" for consonance and can be switched to "l" for likelihood.
#' @param format The format of the tables. The options include "data.frame" which is the
#' default, "docx" (which creates a table for a word document), "pptx" (which
#' creates a table for powerpoint), "latex", (which creates a table for a TeX document), and
#' "image", which produces an image of the table.
#'
#' @return The returned object depends on `format`:
#' * `"data.frame"` (the default): a data frame of class
#'   `c("data.frame", "concurve")` giving the interval limits and their
#'   statistics at conventional levels, rounded to three digits.
#' * `"latex"`: a `knitr_kable` object for inclusion in a TeX document.
#' * `"image"`: a `flextable` object.
#' * `"docx"` or `"pptx"`: the result of printing the `flextable` to a
#'   temporary Word or PowerPoint document.
#'
#' @examples
#' curves <- curve_gen(lm(mpg ~ wt, data = mtcars), "wt")
#'
#' # Interval limits at conventional levels, with their P- and S-values
#' curve_table(curves[[1]], format = "data.frame")
#'
#' # Only the levels you ask for
#' curve_table(curves[[1]], levels = c(0.50, 0.95), format = "data.frame")
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
curve_table <- function(data, levels = NULL, type = "c", format = "data.frame") {
  # Only report unmatched levels the caller actually asked for. The default
  # set is a superset of what a coarse `steps` can produce, and the internal
  # callers have always taken whatever subset existed.
  supplied <- !is.null(levels)

  if (supplied) {
    if (!is.numeric(levels) || length(levels) == 0L || anyNA(levels)) {
      stop("Error: 'levels' must be a numeric vector with no missing values")
    }
  }

  if (type == "c") {
    if (is.null(levels)) {
      levels <- c(0.25, 0.50, 0.75, 0.80, 0.85, 0.90, 0.95, 0.975, 0.99)
    } else if (any(levels <= 0 | levels >= 1)) {
      stop("Error: 'levels' must be between 0 and 1 when 'type' is \"c\"")
    }

    pieces <- parallel::mclapply(levels, FUN = function(i) (data[data$intrvl.level == i, ]), mc.cores = getOption("mc.cores", 1L))
    if (supplied) .warn_unmatched_levels(pieces, levels)
    subdf <- data.frame(do.call(rbind, pieces))
    class(subdf) <- c("data.frame", "concurve")
    subdf$intrvl.level <- (subdf$intrvl.level * 100)
    subcolnames <- c("Lower Limit", "Upper Limit", "Interval Width", "Interval Level (%)", "CDF", "P-value", "S-value (bits)")
    colnames(subdf) <- subcolnames
    subdf <- round(subdf, digits = 3)
  } else if (type == "l") {
    if (is.null(levels)) {
      levels <- c(0.03, 0.05, 0.12, 0.14)
    }

    pieces <- parallel::mclapply(levels, FUN = function(i) (data[round(data$support, 2) == i, ]), mc.cores = getOption("mc.cores", 1L))
    if (supplied) .warn_unmatched_levels(pieces, levels)
    subdf <- data.frame(do.call(rbind, pieces))
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

#' Warn about requested levels that matched no rows
#'
#' `curve_table()` selects rows by exact match on the interval level, so a
#' level that was never computed yields an empty piece rather than an error.
#' Reporting it is the difference between an informative table and a
#' silently short one.
#'
#' @param pieces List of data frames, one per requested level.
#' @param levels The requested levels, in the same order as `pieces`.
#' @return `TRUE`/`FALSE` for each level, invisibly.
#' @noRd
.warn_unmatched_levels <- function(pieces, levels) {
  found <- vapply(pieces, NROW, integer(1)) > 0L
  if (!all(found)) {
    warning(
      "No rows in 'data' at level(s): ",
      paste(levels[!found], collapse = ", "),
      call. = FALSE
    )
  }
  invisible(found)
}

utils::globalVariables(c("subdf", "Lower Limit", "Upper Limit", "Interval Width", "Interval Level", "CDF", "P-value", "S-value"))
