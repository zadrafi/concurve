# Meta-analytic Consonance Function

curve_meta <- function(x, measure = "default", steps = 10000) {
  if (is.list(x) != TRUE) {
    stop("Error: 'x' must be a list from 'metafor'")
  }
  if (is.character(measure) != TRUE) {
    stop("Error: 'measure' must be a string such as 'default' or 'ratio'")
  }
  if (is.numeric(steps) != TRUE) {
    stop("Error: 'steps' must be a numeric vector")
  }
  intrvls <- (0:steps) / steps
  results <- mclapply(intrvls, FUN = function(i) confint.default(object = x, fixed = TRUE, random = FALSE, level = i)[])
  df <- data.frame(do.call(rbind, results))
  intrvl.limit <- c("lower.limit", "upper.limit")
  colnames(df) <- intrvl.limit
  df$intrvl.level <- intrvls
  df$pvalue <- 1 - intrvls
  df$svalue <- -log2(df$pvalue)
  if (measure == "default") {
    df$lower.limit <- df$lower.limit
    df$upper.limit <- df$upper.limit
  } else if (measure == "ratio") {
    df$lower.limit <- exp(df$lower.limit)
    df$upper.limit <- exp(df$upper.limit)
  }
  df <- head(df, -1)
  return(df)
}

# RMD Check
utils::globalVariables(c("df", "lower.limit", "upper.limit", "intrvl.level", "pvalue", "svalue"))
