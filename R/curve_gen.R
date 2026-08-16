#' Consonance Functions For Linear Models, Generalized Linear Models, and Robust Linear Models
#'
#' Computes thousands of consonance (confidence) intervals for
#' the chosen parameter in the selected model
#' (linear models, general linear models, robust linear models, and generalized least squares and places
#' the interval limits for each interval level into a data frame along
#' with the corresponding p-values and s-values. Can also adjust for multiple comparisons. It is generally
#' recommended to wrap this function using suppressMessages() due to the long list of profiling messages.
#'
#' @param model The statistical model of interest
#' (ANOVA, regression, logistic regression) is to be indicated here.
#' @param var The variable of interest from the model (coefficients, intercept)
#' for which the intervals are to be produced.
#' @param method Chooses the method to be used to calculate the
#' consonance intervals. There are currently five methods:
#' "lm", rms::ols objects can be used with the "lm" option, "rlm", "glm" and "aov", and "gls".
#' The "lm" method uses the profile likelihood method to compute intervals and can be used for models created by
#' the 'lm' function. It is typically what most people are
#' familiar with when computing intervals based on the calculated standard error.
#' The ols function from the rms package can also be used for this option.
#' The "rlm" method is designed for usage with the "rlm" function from the MASS
#' package.
#' The "glm" method allows this function to be used for specific scenarios like
#' logistic regression and the 'glm' function. Similarly, the Glm function from the
#' rms package can also be used for this option. The gls method allows objects from gls()
#' or from Gls() from the rms package.
#' @param log Determines whether the coefficients will be exponentiated or not. By default,
#' it is off and set to FALSE or F, but changing this to TRUE or T, will exponentiate the results
#' which may be useful if trying to view the results from a logistic regression on a scale that is not
#' logarithmic.
#' @param penalty An input to specify whether the confidence intervals should be corrected
#' for multiple comparisons. The default is NULL, so there is no correction. Other options include
#' "bonferroni" and "sidak".
#' @param m Indicates how many comparisons are being done and the number that should be used to
#' correct for multiple comparisons. The default is NULL.
#' @param steps Indicates how many consonance intervals are to be calculated at
#' various levels. For example, setting this to 100 will produce 100 consonance
#' intervals from 0 to 100. Setting this to 10000 will produce more consonance
#' levels. By default, it is set to 1000. Increasing the number substantially
#' is not recommended as it will take longer to produce all the intervals and
#' store them into a data frame.
#' @param cores Select the number of cores to use in  order to compute the intervals
#'  The default is 1 core. Parallel computation uses forking via
#'  \code{parallel::mclapply()}, which is unavailable on Windows; on that
#'  platform the intervals are computed sequentially regardless of this value.
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
#' # Simulate random data
#' GroupA <- rnorm(50)
#' GroupB <- rnorm(50)
#' RandomData <- data.frame(GroupA, GroupB)
#' rob <- lm(GroupA ~ GroupB, data = RandomData)
#' bob <- curve_gen(rob, "GroupB")
#' }
#'
#' @export
curve_gen <- function(model, var, method = "lm", log = FALSE, penalty = NULL, m = NULL,
                      steps = 1000, cores = getOption("mc.cores", 1L), table = TRUE) {
  if (is.character(method) != TRUE) {
    stop("Error: 'method' must be a character vector")
  }
  if (is.numeric(steps) != TRUE) {
    stop("Error: 'steps' must be a numeric vector")
  }

  intrvls <- (1:(steps - 1)) / steps

  # Adjust the requested levels for multiple comparisons, if asked ------------

  adj_levels <- if (is.null(penalty) && is.null(m)) {
    intrvls
  } else if (identical(penalty, "bonferroni") && !is.null(m) && m > 1) {
    1 - ((1 - intrvls) / m)
  } else if (identical(penalty, "sidak") && !is.null(m) && m > 1) {
    intrvls^(1 / m)
  } else {
    stop(
      "Error: 'penalty' must be NULL, \"bonferroni\", or \"sidak\", ",
      "and 'm' must be greater than 1"
    )
  }

  # Interval function for the requested model type ---------------------------

  ci_fun <- switch(method,
    lm = function(i) confint.default(object = model, level = i)[var, ],
    gls = function(i) confint.default(object = model, level = i)[var, ],
    rlm = function(i) confint(object = model, level = i)[var, ],
    aov = function(i) confint(object = model, level = i)[var, ],
    glm = function(i) confint(object = model, level = i, trace = FALSE)[var, ],
    stop("Error: 'method' must be one of \"lm\", \"rlm\", \"glm\", \"aov\", or \"gls\"")
  )

  results <- .curve_apply(adj_levels, ci_fun, cores)

  df <- data.frame(do.call(rbind, results))

  if (log == FALSE) {
    df <- (df)
  } else if (log == TRUE) {
    df <- exp(df)
  }

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


# Internal: apply FUN over X, in parallel where the platform allows it.
#
# parallel::mclapply() parallelises by forking, which is unavailable on
# Windows, so fall back to lapply() there (and whenever a single core is
# requested). This is resolved at *call* time; deciding it at install time
# via Sys.info() bakes the host's OS into the installed package and leaves
# the function undefined entirely on unhandled platforms such as Linux.
.curve_apply <- function(X, FUN, cores = 1L) {
  if (.Platform$OS.type == "windows" || !is.numeric(cores) || cores <= 1L) {
    lapply(X, FUN)
  } else {
    parallel::mclapply(X, FUN, mc.cores = cores)
  }
}

# RMD Check
utils::globalVariables(c("df", "lower.limit", "upper.limit", "intrvl.width", "intrvl.level", "cdf", "pvalue", "svalue"))
