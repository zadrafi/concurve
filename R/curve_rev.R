# Reverse Engineer Consonance / Likelihood Functions Using the Point Estimate and Confidence Limits

curve_rev <- function(point, LL, UL, type = "c", measure = "default", steps = 10000, table = TRUE) {


  # Produce Consonance / Surprisal Functions --------------------------------

  if (type == "c") {
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
      LL <- pbmclapply(z, FUN = function(i) point + (i * se), mc.cores = detectCores() - 1)
      UL <- pbmclapply(z, FUN = function(i) point - (i * se), mc.cores = detectCores() - 1)
      df <- data.frame(do.call(rbind, UL), do.call(rbind, LL))
      intrvl.limit <- c("lower.limit", "upper.limit")
      colnames(df) <- intrvl.limit
    }

    else if (measure == "ratio") {
      se <- log(UL / LL) / 3.92
      logpoint <- log(point)
      logLL <- pbmclapply(z, FUN = function(i) logpoint + (i * se), mc.cores = detectCores() - 1)
      logUL <- pbmclapply(z, FUN = function(i) logpoint - (i * se), mc.cores = detectCores() - 1)
      df <- data.frame(do.call(rbind, logUL), do.call(rbind, logLL))
      intrvl.limit <- c("lower.limit", "upper.limit")
      colnames(df) <- intrvl.limit
      df$lower.limit <- exp(df$lower.limit)
      df$upper.limit <- exp(df$upper.limit)
    }
    df$intrvl.width <- (abs((df$upper.limit) - (df$lower.limit)))
    df$intrvl.level <- 1 - intrvls
    df$cdf <- (abs(df$intrvl.level / 2)) + 0.5
    df$pvalue <- 1 - (1 - intrvls)
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


    # Produce Likelihood / Deviance Functions ---------------------------------
  } else if (type == "l") {
    intrvls <- (1:steps) / steps
    z <- qnorm(1 - intrvls / 2)

    if (measure == "default") {
      se <- (UL / LL) / 3.92
      LL <- pbmclapply(z, FUN = function(i) point + (i * se), mc.cores = detectCores() - 1)
      UL <- pbmclapply(z, FUN = function(i) point - (i * se), mc.cores = detectCores() - 1)
      df <- data.frame(do.call(rbind, UL), do.call(rbind, LL))
      intrvl.limit <- c("lower.limit", "upper.limit")
      colnames(df) <- intrvl.limit
    }

    else if (measure == "ratio") {
      se <- log(UL / LL) / 3.92
      logpoint <- log(point)
      logLL <- pbmclapply(z, FUN = function(i) logpoint + (i * se), mc.cores = detectCores() - 1)
      logUL <- pbmclapply(z, FUN = function(i) logpoint - (i * se), mc.cores = detectCores() - 1)
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
    likfunction <- data.frame(values, likelihood, loglikelihood, support, deviancestat)


    if (table == TRUE) {
      levels <- c(0.03, 0.05, 0.12, 0.14)
      (df_subintervals <- (curve_table(likfunction, levels, type = "l", format = "data.frame")))
      class(df_subintervals) <- c("data.frame", "concurve")
      dataframes <- list(likfunction, df_subintervals)
      names(dataframes) <- c("Intervals Dataframe", "Intervals Table")
      class(dataframes) <- "concurve"
      return(dataframes)
    } else if (table == FALSE) {
      return(list(likfunction))
    }
  }
}

# RMD Check
utils::globalVariables(c("df", "lower.limit", "upper.limit", "intrvl.width", "intrvl.level", "cdf", "pvalue", "svalue"))
utils::globalVariables(c("likfunction", "values", "likelihood", "loglikelihood", "support", "deviancestat"))
