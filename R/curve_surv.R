# Survival Data Consonance Function

curve_surv <- function(data, x, steps = 10000) {
  if (is.list(data) != TRUE) {
    stop("Error: 'data' must be an object with a Cox Proportional Hazards model")
  }
  if (is.numeric(steps) != TRUE) {
    stop("Error: 'steps' must be a numeric vector")
  }
  pboptions(type = "timer", style = 1, char = "+")
  intrvls <- (1:steps) / steps
  results <- pblapply(intrvls, FUN = function(i) summary(data, conf.int = i)$conf.int[x, ], cl = detectCores() - 1)

  df <- data.frame(do.call(rbind, results))[, 3:4]
  intrvl.limit <- c("lower.limit", "upper.limit")
  colnames(df) <- intrvl.limit
  df$intrvl.width <- (abs((df$upper.limit) - (df$lower.limit)))
  df$intrvl.level <- intrvls
  df$cdf <- (abs(df$intrvl.level / 2)) + 0.5
  df$pvalue <- 1 - intrvls
  df$svalue <- -log2(df$pvalue)
  df <- head(df, -1)
  return(df)
}

# RMD Check
utils::globalVariables(c("df", "lower.limit", "upper.limit", "intrvl.width", "intrvl.level", "cdf", "pvalue", "svalue"))
