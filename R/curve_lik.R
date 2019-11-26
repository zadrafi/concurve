curve_lik <- function(point, LL, UL, measure = "ratio", steps = 10000) {
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

  intrvls <- (1:steps) / steps
  z <- qnorm(1 - intrvls / 2)

  if (measure == "default") {
    se <- (UL / LL) / 3.92
    LL <- mclapply(z, FUN = function(i) point + (i * se), mc.cores = detectCores(logical = FALSE) - 1)
    UL <- mclapply(z, FUN = function(i) point - (i * se), mc.cores = detectCores(logical = FALSE) - 1)
    df <- data.frame(do.call(rbind, UL), do.call(rbind, LL))
    intrvl.limit <- c("lower.limit", "upper.limit")
    colnames(df) <- intrvl.limit
  }

  else if (measure == "ratio") {
    se <- log(UL / LL) / 3.92
    logpoint <- log(point)
    logLL <- mclapply(z, FUN = function(i) logpoint + (i * se), mc.cores = detectCores(logical = FALSE) - 1)
    logUL <- mclapply(z, FUN = function(i) logpoint - (i * se), mc.cores = detectCores(logical = FALSE) - 1)
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


  se <- log(UL / LL) / 3.92
  values <- seq(from = df[1, 1], to = df[1, 2], by = 0.01)
  zscore <- sapply(
    values,
    function(j) (log(j / point) / se)
  )

  support <- exp((-zscore^2) / 2)
  deviancestat <- (zscore^2)
  likelihood <- support * (log(point))
  loglikelihood <- log(likelihood)
  likfunction <- data.frame(values, zscore, likelihood, loglikelihood, support, deviancestat)
  return(likfunction)
}

utils::globalVariables(c("likfunction", "values", "zscore", "likelihood", "loglikelihood", "support", "deviancestat"))
