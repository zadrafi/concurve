#' General Consonance Functions Using Profile Likelihood, Wald,
#' or the bootstrap method for linear models.
#'
#' Computes thousands of consonance (confidence) intervals for
#' the chosen parameter in the selected model
#' (ANOVA, ANCOVA, regression, logistic regression) and places
#' the interval limits for each interval level into a data frame along
#' with the corresponding p-values and s-values.
#'
#' @param model The statistical model of interest
#' (ANOVA, regression, logistic regression) is to be indicated here.
#' @param var The variable of interest from the model (coefficients, intercept)
#' for which the intervals are to be produced.
#' @param method Chooses the method to be used to calculate the
#' consonance intervals. There are currently four methods:
#' "default", "wald", "lm", and "boot". The "default" method uses the profile
#' likelihood method to compute intervals and can be used for models created by
#' the 'lm' function. The "wald" method is typicallywhat most people are
#' familiar with when computing intervals based on the calculated standard error.
#' The "lm" method allows this function to be used for specific scenarios like
#' logistic regression and the 'glm' function. The "boot" method allows for
#' bootstrapping at certain levels.
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
#' @examples
#'
#' \donttest{
#' # Simulate random data
#' GroupA <- rnorm(50)
#' GroupB <- rnorm(50)
#' RandomData <- data.frame(GroupA, GroupB)
#' rob <- lm(GroupA ~ GroupB, data = RandomData)
#' bob <- curve_gen(rob, "GroupB")
#' tibble::tibble(bob[[1]])
#' }
#'
curve_gen <- function(model, var, method = "wald", steps = 1000, table = TRUE) {
  if (is.character(method) != TRUE) {
    stop("Error: 'method' must be a character vector")
  }
  if (is.numeric(steps) != TRUE) {
    stop("Error: 'steps' must be a numeric vector")
  }

  intrvls <- (1:(steps - 1)) / steps

  if (method == "wald") {
    results <- pbmclapply(intrvls, FUN = function(i) confint.default(object = model, level = i)[var, ], mc.cores = getOption("mc.cores", 1L))
  } else if (method == "glm") {
    results <- pbmclapply(intrvls, FUN = function(i) confint(object = model, level = i, trace = FALSE)[var, ], mc.cores = getOption("mc.cores", 1L))
  }

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
