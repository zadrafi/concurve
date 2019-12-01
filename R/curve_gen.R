# General Consonance Functions Using Profile Likelihood, Wald, or the bootstrap method for linear models.

curve_gen <- function(model, var, method = "wald", replicates = 1000, steps = 10000, table = TRUE, ...) {
  if (is.character(method) != TRUE) {
    stop("Error: 'method' must be a character vector")
  }
  if (is.numeric(replicates) != TRUE) {
    stop("Error: 'replicates' must be a numeric vector")
  }
  if (is.numeric(steps) != TRUE) {
    stop("Error: 'steps' must be a numeric vector")
  }

  intrvls <- (0:steps) / steps
  if (method == "wald") {
    results <- pbmclapply(intrvls, FUN = function(i) confint.default(object = model, level = i)[var, ], mc.cores = detectCores() - 1)
  } else if (method == "glm") {
    require(MASS)
    results <- pbmclapply(intrvls, FUN = function(i) confint(object = model, level = i)[var, ], mc.cores = detectCores() - 1)
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
    (df_subintervals <- (curve_table(df, levels, type = "data.frame")))
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
