#' Consonance Functions for Correlations
#'
#' Computes consonance intervals to produce P- and S-value functions for
#' correlational analysesusing the cor.test function in base R and places the
#' interval limits for each interval levelinto a data frame along with the
#' corresponding p-values and s-values.
#'
#' @param x A vector that contains the data for one of the variables that will
#' be analyzed for correlational analysis.
#' @param y A vector that contains the data for one of the variables that will
#' be analyzed for correlational analysis.
#' @param alternative Indicates the alternative hypothesis and must be one of "two.sided",
#' "greater" or "less". You can specify just the initial letter. "greater" corresponds to
#' positive association, "less" to negative association.
#' @param method A character string indicating which correlation coefficient is
#' to be used for the test. One of "pearson", "kendall", or "spearman",
#' can be abbreviated.
#' @param steps Indicates how many consonance intervals are to be calculated at
#' various levels. For example, setting this to 100 will produce 100 consonance
#' intervals from 0 to 100. Setting this to 10000 will produce more consonance
#' levels. By default, it is set to 1000. Increasing the number substantially
#' is not recommended as it will take longer to produce all the intervals and
#' store them into a dataframe.
#' @param table Indicates whether or not a table output with some relevant
#' statistics should be generated. The default is TRUE and generates a table
#' which is included in the list object.
#'
#' @return A list with 3 items where the dataframe of values is in the first
#' object, the values needed to calculate the density function in the second,
#' and the table for the values in the third if table = TRUE.
#'
#' @examples
#'
#' GroupA <- rnorm(50)
#' GroupB <- rnorm(50)
#' joe <- curve_corr(x = GroupA, y = GroupB, alternative = "two.sided", method = "pearson")
curve_corr <- function(x, y, alternative, method, steps = 10000, table = TRUE) {
  if (is.numeric(x) != TRUE) {
    stop("Error: 'x' must be a numeric vector")
  }
  if (is.numeric(y) != TRUE) {
    stop("Error: 'y' must be a numeric vector")
  }
  if (is.numeric(steps) != TRUE) {
    stop("Error: 'steps' must be a numeric vector")
  }


  intrvls <- (0:steps) / steps
  results <- pbmclapply(intrvls, FUN = function(i) {
    cor.test(x, y,
      alternative = alternative, method = method,
      exact = NULL, conf.level = i, continuity = FALSE
    )$conf.int[]
  }, mc.cores = getOption("mc.cores", 1L))
  df <- data.frame(do.call(rbind, results))
  intrvl.limit <- c("lower.limit", "upper.limit")
  colnames(df) <- intrvl.limit
  df$intrvl.width <- (abs((df$upper.limit) - (df$lower.limit)))
  df$intrvl.level <- intrvls
  df$cdf <- (abs(df$intrvl.level / 2)) + 0.5
  df$pvalue <- 1 - intrvls
  df$svalue <- -log2(df$pvalue)
  df <- head(df, -1)
  class(df) <- c("data.frame", "concurve")
  densdf <- data.frame(c(df$lower.limit, df$upper.limit))
  colnames(densdf) <- "x"
  densdf <- head(densdf, -1)
  class(densdf) <- c("data.frame", "concurve")


  if (table == TRUE) {
    levels <- c(0.25, 0.50, 0.75, 0.80, 0.85, 0.90, 0.95, 0.975, 0.99)
    (df_subintervals <- (curve_table(df, levels, type = "c", format = "data.frame")))
    class(df_subintervals) <- c("data.frame", "concurve")
    dataframes <- list(df, densdf, df_subintervals)
    names(dataframes) <- c("Intervals Dataframe", "Intervals Density", "Intervals Table")
    class(dataframes) <- "concurve"
    return(dataframes)
  } else if (table == FALSE) {
    return(list(df, densdf))
  }
}

# RMD Check
utils::globalVariables(c("df", "lower.limit", "upper.limit", "intrvl.width", "intrvl.level", "cdf", "pvalue", "svalue"))
