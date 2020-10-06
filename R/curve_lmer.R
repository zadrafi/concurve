#' Consonance Functions For Linear & Non-Linear Mixed-Effects Models.
#'
#' Computes thousands of consonance (confidence) intervals for
#' the chosen parameter in the selected lme4 model and places
#' the interval limits for each interval level into a data frame along
#' with the corresponding p-values and s-values.. It is generally
#' recommended to wrap this function using suppressMessages() due to the long list of profiling messages
#'
#' @param object The statistical model of interest from lme4 is to be indicated here.
#' @param parm The variable of interest from the model (coefficients, intercept)
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
#' @param zeta (for method = "profile" only:)
#' likelihood cutoff (if not specified, as by default, computed from level).
#' @param nsim number of simulations for parametric bootstrap intervals.
#' @param FUN function; if NULL, an internal function that returns the
#' fixed-effect parameters as well as the random-effect parameters on
#' the standard deviation/correlationscale will be used.
#' @param boot.type bootstrap confidence interval type, as described in boot.c i.
#' Methods stud and bca are unavailable because they
#' require additional components to be calculated.
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



curve_lmer <- function(object, parm, method = "profile", zeta = NULL,
                       nsim = NULL, FUN = NULL, boot.type = NULL, steps = 1000, cores = getOption("mc.cores", 1L), table = FALSE) {
  if (is.numeric(steps) != TRUE) {
    stop("Error: 'steps' must be a numeric vector")
  }

  intrvls <- (1:steps) / steps
  results <- pbmclapply(intrvls, FUN = function(i) {
    confint.merMod(
      object = object, parm = parm, level = i,
      method = method,
      nsim = nsim, boot.type = boot.type,
      FUN = FUN, quiet = FALSE
    )[1:2]
  }, mc.cores = cores)


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
utils::globalVariables(c("df", "lower.limit", "confint.merMod", "upper.limit", "intrvl.width", "intrvl.level", "cdf", "pvalue", "svalue"))
