# Reverse Engineer Consonance Functions Using the Point Estimate and Confidence Limits

curve_rev <- function(point, LL, UL, measure = "default") {
  if (is.numeric(point) != TRUE) {
    stop("Error: 'x' must be a numeric vector")
  }
  if (is.numeric(LL) != TRUE) {
    stop("Error: 'y' must be a numeric vector")
  }
  if (is.numeric(UL) != TRUE) {
    stop("Error: 'y' must be a numeric vector")
  }
  if (is.character(measure) != TRUE) {
    stop("Error: 'measure' must be a string such as 'default' or 'ratio'")
  }

  intrvls <- (1:10000) / 10000
  z <- qnorm(1 - intrvls / 2)

  if (measure == "default") {
    se <- (UL / LL) / 3.92
    LL <- mclapply(z, FUN = function(i) point + (i * se))
    UL <- mclapply(z, FUN = function(i) point - (i * se))
    df <- data.frame(do.call(rbind, UL), do.call(rbind, LL))
    intrvl.limit <- c("lower.limit", "upper.limit")
    colnames(df) <- intrvl.limit
  }

  else if (measure == "ratio") {
    se <- log(UL / LL) / 3.92
    logpoint <- log(point)
    logLL <- mclapply(z, FUN = function(i) logpoint + (i * se))
    logUL <- mclapply(z, FUN = function(i) logpoint - (i * se))
    df <- data.frame(do.call(rbind, logUL), do.call(rbind, logLL))
    intrvl.limit <- c("lower.limit", "upper.limit")
    colnames(df) <- intrvl.limit
    df$lower.limit <- exp(df$lower.limit)
    df$upper.limit <- exp(df$upper.limit)
  }

  df$intrvl.level <- 1 - intrvls
  df$pvalue <- 1 - (1 - intrvls)
  df$svalue <- -log2(df$pvalue)
  df <- head(df, -1)
  return(df)
}

# RMD Check
utils::globalVariables(c("df", "lower.limit", "upper.limit", "intrvl.level", "pvalue", "svalue"))
