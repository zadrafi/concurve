#' Consonance Functions For Survival Data
#'
#' Computes thousands of consonance (confidence) intervals for the chosen
#' parameter in the Cox model computed by the 'survival' package and places
#' the interval limits for each interval level into a data frame along
#' with the corresponding p-value and s-value.
#'
#' @param data Object where the Cox model is stored, typically a list produced by the
#' 'survival' package.
#' @param x Predictor of interest within the survival model for which the
#' consonance intervals should be computed.
#' @param steps Indicates how many consonance intervals are to be calculated at
#' various levels. For example, setting this to 100 will produce 100 consonance
#' intervals from 0 to 100. Setting this to 10000 will produce more consonance
#' levels. By default, it is set to 1000. Increasing the number substantially
#' is not recommended as it will take longer to produce all the intervals and
#' store them into a dataframe.
#' @param cores Select the number of cores to use in  order to compute the intervals
#'  The default is 1 core.
#' @param table Indicates whether or not a table output with some relevant
#' statistics should be generated. The default is TRUE and generates a table
#' which is included in the list object.
#'
#' @return A list with 3 items where the dataframe of values is in the first
#' object, the values needed to calculate the density function in the second,
#' and the table for the values in the third if table = TRUE.
#'
#' @examples
#' \dontrun{
#' library(carData)
#' Rossi[1:5, 1:10]
#' library(survival)
#'
#' mod.allison <- coxph(Surv(week, arrest) ~ fin + age + race + wexp + mar + paro + prio,
#'   data = Rossi
#' )
#' mod.allison
#'
#' z <- curve_surv(mod.allison, "prio")
#' }
curve_surv <- function(data, x, steps = 10000, cores = getOption("mc.cores", 1L), table = TRUE) {
  if (is.list(data) != TRUE) {
    stop("Error: 'data' must be an object with a Cox Proportional Hazards model")
  }
  if (is.numeric(steps) != TRUE) {
    stop("Error: 'steps' must be a numeric vector")
  }

  intrvls <- (1:steps) / steps
  results <- pbmclapply(intrvls, FUN = function(i) summary(data, conf.int = i)$conf.int[x, ], mc.cores = cores)

  df <- data.frame(do.call(rbind, results))[, 3:4]
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
