#' Construct Consonance Functions from Any CI-Producing Function
#'
#' A generic wrapper that constructs consonance, surprisal, and likelihood
#' functions from any function that produces confidence intervals. This is the
#' most flexible approach for generating consonance curves from arbitrary
#' statistical procedures.
#'
#' @param ci_func A function that takes a confidence level (0-1) as its first
#'   argument and returns a numeric vector of length 2 (lower, upper bounds).
#'   See examples for common patterns.
#' @param steps Number of consonance levels to compute. Default is 1000.
#' @param cores Number of cores for parallel computation. Default uses
#'   \code{getOption("mc.cores", 1L)}.
#' @param table Logical. If TRUE (default), includes a summary table of key
#'   intervals in the output.
#'
#' @return A list with class "concurve" containing:
#' \describe{
#'   \item{Intervals Dataframe}{Data frame with columns: lower.limit, upper.limit,
#'     intrvl.width, intrvl.level, cdf, pvalue, svalue}
#'   \item{Intervals Density}{Data frame for plotting density functions}
#'   \item{Intervals Table}{Summary table of key intervals (if table = TRUE)}
#' }
#'
#' @details
#' This function repeatedly calls \code{ci_func} at different confidence levels
#' to construct the full consonance function. The \code{ci_func} argument must
#' be a function that:
#' \enumerate{
#'   \item Takes a confidence level (numeric between 0 and 1) as its first argument
#'   \item Returns a numeric vector of exactly length 2: c(lower_bound, upper_bound)
#' }
#'
#' Common patterns for \code{ci_func}:
#' \itemize{
#'   \item Linear models: \code{function(level) confint.default(model, parm = "x", level = level)}
#'   \item GLMs (profile): \code{function(level) confint(model, parm = "x", level = level)}
#'   \item t-tests: \code{function(level) t.test(x, conf.level = level)$conf.int}
#'   \item Correlations: \code{function(level) cor.test(x, y, conf.level = level)$conf.int}
#'   \item Proportions: \code{function(level) prop.test(x, n, conf.level = level)$conf.int}
#'   \item Survival: \code{function(level) exp(confint(coxph_model, level = level)["var",])}
#' }
#'
#' @examples
#' \dontrun{
#' # Example 1: Linear model
#' data(mtcars)
#' model <- lm(mpg ~ wt + hp, data = mtcars)
#' ci_func <- function(level) confint.default(model, parm = "wt", level = level)
#' result <- curve_wrap(ci_func)
#' ggcurve(result[[1]], type = "c")
#'
#' # Example 2: t-test
#' x <- rnorm(30, mean = 5)
#' ci_func <- function(level) t.test(x, conf.level = level)$conf.int
#' result <- curve_wrap(ci_func)
#' ggcurve(result[[1]], type = "c", nullvalue = 0)
#'
#' # Example 3: Correlation
#' x <- rnorm(50)
#' y <- x + rnorm(50)
#' ci_func <- function(level) cor.test(x, y, conf.level = level)$conf.int
#' result <- curve_wrap(ci_func)
#'
#' # Example 4: GLM with profile likelihood CIs
#' model <- glm(am ~ wt, data = mtcars, family = binomial)
#' ci_func <- function(level) confint(model, parm = "wt", level = level)
#' result <- curve_wrap(ci_func)
#'
#' # Example 5: Proportion test
#' ci_func <- function(level) prop.test(45, 100, conf.level = level)$conf.int
#' result <- curve_wrap(ci_func)
#'
#' # Example 6: Custom function with additional arguments
#' my_ci <- function(level, data, method) {
#'   cor.test(data$x, data$y, method = method, conf.level = level)$conf.int
#' }
#' df <- data.frame(x = rnorm(50), y = rnorm(50))
#' ci_func <- function(level) my_ci(level, data = df, method = "spearman")
#' result <- curve_wrap(ci_func)
#' }
#'
#' @seealso [ggcurve()] for plotting
#' @seealso [curve_gen()] for model-specific consonance functions
#' @seealso [curve_rev()] for constructing curves from published intervals
#' @seealso [curve_table()] for interval summaries
#'
#' @export
curve_wrap <- function(ci_func, steps = 1000, cores = getOption("mc.cores", 1L), table = TRUE) {

  # Input validation
  if (!is.function(ci_func)) {
    stop("Error: 'ci_func' must be a function that takes a confidence level and returns c(lower, upper)")
  }
  if (!is.numeric(steps) || steps < 10) {
    stop("Error: 'steps' must be a numeric value >= 10")
  }
  if (!is.numeric(cores) || cores < 1) {
    stop("Error: 'cores' must be a positive integer")
  }

  # Test the ci_func with a sample level
  test_result <- tryCatch(
    ci_func(0.95),
    error = function(e) {
      stop("Error: 'ci_func' failed at level 0.95. Ensure it accepts a confidence level and returns c(lower, upper).\nOriginal error: ", e$message)
    }
  )

  if (!is.numeric(test_result) || length(test_result) != 2) {
    stop("Error: 'ci_func' must return a numeric vector of length 2 (lower, upper bounds)")
  }

  # Generate interval levels (exclude 0 and 1 to avoid edge cases)
  intrvls <- (1:(steps - 1)) / steps

  # Compute intervals in parallel
  results <- parallel::mclapply(intrvls, function(level) {
    ci <- ci_func(level)
    c(ci[1], ci[2])
  }, mc.cores = cores)

  # Build dataframe
  df <- data.frame(do.call(rbind, results))
  colnames(df) <- c("lower.limit", "upper.limit")
  df$intrvl.width <- abs(df$upper.limit - df$lower.limit)
  df$intrvl.level <- intrvls
  df$cdf <- (df$intrvl.level / 2) + 0.5
  df$pvalue <- 1 - intrvls
  df$svalue <- -log2(df$pvalue)

  class(df) <- c("data.frame", "concurve")

  # Build density dataframe for plotting
  densdf <- data.frame(x = c(df$lower.limit, df$upper.limit))
  class(densdf) <- c("data.frame", "concurve")

  # Build output list
  if (isTRUE(table)) {
    levels <- c(0.25, 0.50, 0.75, 0.80, 0.85, 0.90, 0.95, 0.975, 0.99)
    df_subintervals <- curve_table(df, levels, type = "c", format = "data.frame")
    class(df_subintervals) <- c("data.frame", "concurve")

    dataframes <- list(df, densdf, df_subintervals)
    names(dataframes) <- c("Intervals Dataframe", "Intervals Density", "Intervals Table")
  } else {
    dataframes <- list(df, densdf)
    names(dataframes) <- c("Intervals Dataframe", "Intervals Density")
  }

  class(dataframes) <- "concurve"
  return(dataframes)
}


#' Construct Consonance Functions from Fitted Models
#'
#' A convenience wrapper around \code{\link{curve_wrap}} for common model objects.
#' Automatically selects the appropriate confidence interval method based on the
#' model class or user specification.
#'
#' @param model A fitted model object (lm, glm, nls, lme, etc.)
#' @param param Character string specifying which parameter to extract.
#' @param method Method for computing confidence intervals:
#'   \describe{
#'     \item{"default"}{Uses \code{confint.default()} - Wald/normal approximation (fast)}
#'     \item{"profile"}{Uses \code{confint()} - profile likelihood (slower, more accurate for GLMs)}
#'   }
#' @param steps Number of consonance levels to compute. Default is 1000.
#' @param cores Number of cores for parallel computation.
#' @param table Logical. If TRUE (default), includes a summary table.
#'
#' @return A list with class "concurve" (see \code{\link{curve_wrap}} for details).
#'
#' @details
#' This is a convenience function that constructs the appropriate \code{ci_func}
#' for \code{\link{curve_wrap}} based on the specified method. For maximum
#' flexibility, use \code{curve_wrap} directly.
#'
#' Method selection guidelines:
#' \itemize{
#'   \item \strong{Linear models (lm)}: "default" is appropriate and fast
#'   \item \strong{GLMs}: "profile" is more accurate, especially for small samples
#'   \item \strong{Mixed models}: "profile" recommended but can be slow
#'   \item \strong{Robust models (rlm)}: "default" typically used
#' }
#'
#' @examples
#' \dontrun{
#' # Linear model - Wald intervals (fast)
#' model <- lm(mpg ~ wt + hp, data = mtcars)
#' result <- curve_model(model, param = "wt", method = "default")
#' ggcurve(result[[1]], type = "c")
#'
#' # GLM - Profile likelihood intervals (more accurate)
#' model <- glm(am ~ wt, data = mtcars, family = binomial)
#' result <- curve_model(model, param = "wt", method = "profile")
#'
#' # Compare methods
#' wald <- curve_model(model, param = "wt", method = "default")
#' profile <- curve_model(model, param = "wt", method = "profile")
#' # Plot both to see the difference
#' }
#'
#' @seealso [curve_wrap()] for the generic wrapper
#' @seealso [curve_gen()] for the original model-based function
#' @seealso [ggcurve()] for plotting
#'
#' @export
curve_model <- function(model, param, method = "default", steps = 1000,
                        cores = getOption("mc.cores", 1L), table = TRUE) {

  # Input validation
  if (missing(param) || !is.character(param)) {
    stop("Error: 'param' must be a character string specifying the parameter name")
  }

  valid_methods <- c("default", "profile")
  if (!method %in% valid_methods) {
    stop("Error: 'method' must be one of: ", paste(valid_methods, collapse = ", "))
  }

  # Verify param exists in model
  model_coefs <- tryCatch(
    names(coef(model)),
    error = function(e) NULL
  )

  if (!is.null(model_coefs) && !param %in% model_coefs) {
    stop("Error: '", param, "' not found in model coefficients. Available: ",
         paste(model_coefs, collapse = ", "))
  }

  # Construct ci_func based on method
  ci_func <- switch(method,
    "default" = function(level) {
      ci <- confint.default(model, parm = param, level = level)
      as.numeric(ci)
    },
    "profile" = function(level) {
      # Suppress iteration output for glm profile
      ci <- suppressMessages(confint(model, parm = param, level = level))
      as.numeric(ci)
    }
  )

  # Delegate to curve_wrap
  curve_wrap(ci_func, steps = steps, cores = cores, table = table)
}


# R CMD check
utils::globalVariables(c("lower.limit", "upper.limit", "intrvl.width",
                         "intrvl.level", "cdf", "pvalue", "svalue"))
