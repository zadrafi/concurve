#' Reverse Engineer Consonance / Likelihood Functions Using the Point
#' Estimate and Confidence Limits
#'
#' Using the confidence limits and point estimates from a dataset, one can use
#' these estimates to compute thousands of consonance intervals and graph the
#' intervals to form a consonance, surprisal, and likelihood functions. The intervals are
#' calculated from the approximated normal distribution, however, users should be
#' cautious as this this function is currently designed for similar situations
#' (involving ratios and normal approximations), nevertheless the function also works for means
#' but should be used skeptically, as it can break down in many situations and give implausible numbers.
#' Computations of likelihood functions for means is currently not supported.
#'
#' @param point The point estimate from an analysis. Ex: 1.20
#' @param LL The lower confidence limit from an analysis Ex: 1.0
#' @param UL The upper confidence limit from an analysis Ex: 1.4
#' @param se The standard error of the point estimate. Ex: 0.05
#' @param conf.level Confidence level of the interval estimate.
#' @param type Indicates whether the produced result should be a consonance
#' function or a likelihood function. The default is "c" for consonance and
#' likelihood can be set via "l".
#' @param measure The type of data being used. If they involve mean differences,
#' then the "mean" option should be used. If the data are ratios, then the "ratio"
#' option should be used. "ratio" is currently the default option.
#' Currently, this function is designed to be used with ratios
#' and normal approximations rather than means.
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
#' @examples
#' \dontrun{
#' # From a real published study. Point estimate of the result was hazard ratio of 1.61 and
#' # lower bound of the interval is 0.997 while upper bound of the interval is 2.59.
#' #
#' df <- curve_rev(point = 1.61, LL = 0.997, UL = 2.59, measure = "ratio")
#' }
#' @seealso [ggcurve()]
#' @seealso [curve_compare()]
#' @seealso [plot_compare()]
#'
curve_rev <- function(point,
                      LL = NULL, UL = NULL,
                      se = NULL,
                      conf.level = .95,
                      type = "c", measure = "ratio",
                      steps = 10000, cores = getOption("mc.cores", 1L),
                      table = TRUE) {


  # Produce Consonance / Surprisal Functions --------------------------------

  # Moved outside the "c" conditional; otherwise skipped in "l"
  if (is.numeric(point) != TRUE) {
    stop("Error: 'point' must be a numeric value")
  }

  if (is.character(measure) != TRUE) {
    stop("Error: 'measure' must be a string such as 'mean' or 'ratio'")
  }

  if (is.null(se) && (is.null(LL) & is.null(UL))) {
    stop("se or UL & LL must be entered")
  }

  if (is.null(se)) {
    if (is.numeric(LL) != TRUE) {
      stop("Error: 'LL' must be a numeric value")
    }
    if (is.numeric(UL) != TRUE) {
      stop("Error: 'UL' must be a numeric value")
    }
  }

  intrvls <- (1:steps) / steps
  z <- qnorm(1 - intrvls / 2)
  conf.level_two <- conf.level + (1 - conf.level) / 2
  conf_range <- qnorm(conf.level_two)


  if (!is.null(se)) {
    LL <- point - conf_range * se
    UL <- point + conf_range * se
  }

  if (is.null(se) && measure == "mean") {
    se <- (UL - LL) / (2 * conf_range)
  }

  if (is.null(se) && measure == "ratio") {
    se <- log(UL / LL) / (2 * conf_range)
    print(se)
  }



  if (type == "c") {

    # intrvls <- (1:steps) / steps
    # z <- qnorm(1 - intrvls / 2)
    # conf.level_two = (1-conf.level)/2
    # conf_range = qnorm(conf.level_two)

    if (measure == "mean") {
      # se <- (UL - LL) / (2*conf_range)
      LL <- pbmclapply(z, FUN = function(i) point + (i * se), mc.cores = cores)
      UL <- pbmclapply(z, FUN = function(i) point - (i * se), mc.cores = cores)
      df <- data.frame(do.call(rbind, UL), do.call(rbind, LL))
      intrvl.limit <- c("lower.limit", "upper.limit")
      colnames(df) <- intrvl.limit
    }

    else if (measure == "ratio") {
      # se <- log(UL / LL) / (2*conf_range)
      logpoint <- log(point)
      logLL <- pbmclapply(z, FUN = function(i) logpoint + (i * se), mc.cores = cores)
      logUL <- pbmclapply(z, FUN = function(i) logpoint - (i * se), mc.cores = cores)
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
    # intrvls <- (1:steps) / steps
    # z <- qnorm(1 - intrvls / 2)

    if (measure == "ratio") {
      # se <- log(UL / LL) / (2*conf_range)
      logpoint <- log(point)
      logLL <- pbmclapply(z, FUN = function(i) logpoint + (i * se), mc.cores = cores)
      logUL <- pbmclapply(z, FUN = function(i) logpoint - (i * se), mc.cores = cores)
      df <- data.frame(do.call(rbind, logUL), do.call(rbind, logLL))
      intrvl.limit <- c("lower.limit", "upper.limit")
      colnames(df) <- intrvl.limit
      df$lower.limit <- exp(df$lower.limit)
      df$upper.limit <- exp(df$upper.limit)
      class(df) <- c("data.frame", "concurve")
    }


    else if (measure == "mean") {
      stop("likelihood functions for continuous measures currently not supported")
      #      # se <- (UL - LL) / (2*conf_range)
      #      LL <- pbmclapply(z, FUN = function(i) point + (i * se), mc.cores = cores)
      #      UL <- pbmclapply(z, FUN = function(i) point - (i * se), mc.cores = cores)
      #      df <- data.frame(do.call(rbind, UL), do.call(rbind, LL))
      #      intrvl.limit <- c("lower.limit", "upper.limit")
      #      colnames(df) <- intrvl.limit
      #      class(df) <- c("data.frame", "concurve")
    }

    df$intrvl.level <- 1 - intrvls
    df$pvalue <- 1 - (1 - intrvls)
    df$svalue <- -log2(df$pvalue)
    df <- head(df, -1)
    class(df) <- c("data.frame", "concurve")


    # se <- log(UL / LL) / (2*conf_range)
    if (measure == "ratio") {
      values <- seq(from = df[1, 1], to = df[1, 2], by = 0.01)
      zscore <- sapply(
        values,
        function(j) (log(j / point) / se)
      )
    }

    if (measure == "mean") {
      stop("likelihood functions for continuous measures currently not supported")
      #      values <- seq(from = df[1, 1], to = df[1, 2], by = 0.01)
      #      zscore <- sapply(
      #        values,
      #        function(j) ((j - point) / se)
      #      )
    }

    support <- exp((-zscore^2) / 2)
    deviancestat <- (zscore^2)
    if (measure == "ratio") {
      likelihood <- support * (log(point))
      #    } else {
      #      likelihood <- support * point
    }
    loglikelihood <- log(likelihood)
    likfunction <- data.frame(values, likelihood, loglikelihood, support, deviancestat)
    class(likfunction) <- c("data.frame", "concurve")


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
