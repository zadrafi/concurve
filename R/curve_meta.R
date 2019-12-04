#' Meta-analytic Consonance Function
#'
#' Computes thousands of consonance (confidence) intervals for the chosen
#' parameter in the meta-analysis done by the metafor package and places the
#' interval limits for each interval level into a data frame along with the
#' corresponding p-values and s-values.
#'
#' @param x Object where the meta-analysis parameters are stored, typically a
#' list produced by 'metafor'
#' @param measure Indicates whether the object has a log transformation or is normal/default.
#' The default setting is "default. If the measure is set to "ratio", it will take
#' logarithmically transformed values and convert them back to normal values in the dataframe.
#' This is typically a setting used for binary outcomes such as risk ratios,
#' hazard ratios, and odds ratios.
#' @param steps Indicates how many consonance intervals are to be calculated at
#' various levels. For example, setting this to 100 will produce 100 consonance
#' intervals from 0 to 100. Setting this to 10000 will produce more consonance
#' levels. By default, it is set to 1000. Increasing the number substantially
#' is not recommended as it will take longer to produce all the intervals and
#' store them into a dataframe.
#' @param table Indicates whether or not a table output with some relevant
#' statistics should be generated. The default is TRUE and generates a table
#' which is included in the list object.
#'
#' @examples
#'
#' # Simulate random data for two groups in two studies
#' GroupAData <- runif(20, min = 0, max = 100)
#' GroupAMean <- round(mean(GroupAData), digits = 2)
#' GroupASD <- round(sd(GroupAData), digits = 2)
#'
#' GroupBData <- runif(20, min = 0, max = 100)
#' GroupBMean <- round(mean(GroupBData), digits = 2)
#' GroupBSD <- round(sd(GroupBData), digits = 2)
#'
#' GroupCData <- runif(20, min = 0, max = 100)
#' GroupCMean <- round(mean(GroupCData), digits = 2)
#' GroupCSD <- round(sd(GroupCData), digits = 2)
#'
#' GroupDData <- runif(20, min = 0, max = 100)
#' GroupDMean <- round(mean(GroupDData), digits = 2)
#' GroupDSD <- round(sd(GroupDData), digits = 2)
#'
#' # Combine the data
#'
#' StudyName <- c("Study1", "Study2")
#' MeanTreatment <- c(GroupAMean, GroupCMean)
#' MeanControl <- c(GroupBMean, GroupDMean)
#' SDTreatment <- c(GroupASD, GroupCSD)
#' SDControl <- c(GroupBSD, GroupDSD)
#' NTreatment <- c(20, 20)
#' NControl <- c(20, 20)
#'
#' metadf <- data.frame(
#'   StudyName, MeanTreatment, MeanControl,
#'   SDTreatment, SDControl, NTreatment, NControl
#' )
#'
#' # Use metafor to calculate the standardized mean difference
#'
#' library(metafor)
#'
#' dat <- escalc(
#'   measure = "SMD", m1i = MeanTreatment, sd1i = SDTreatment,
#'   n1i = NTreatment, m2i = MeanControl, sd2i = SDControl,
#'   n2i = NControl, data = metadf
#' )
#'
#' # Pool the data using a particular method. Here "FE" is the fixed-effects model
#'
#' res <- rma(yi, vi,
#'   data = dat, slab = paste(StudyName, sep = ", "),
#'   method = "FE", digits = 2
#' )
#'
#' # Calculate the intervals using the metainterval function
#'
#' metaf <- curve_meta(res)
#'
#' tibble::tibble(metaf[[1]])
curve_meta <- function(x, measure = "default", steps = 10000, table = TRUE) {
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
  results <- pbmclapply(intrvls, FUN = function(i) confint.default(object = x, fixed = TRUE, random = FALSE, level = i)[], mc.cores = getOption("mc.cores", 1L))
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
  df$intrvl.width <- (abs((df$upper.limit) - (df$lower.limit)))
  df$cdf <- (abs(df$intrvl.level / 2)) + 0.5
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
utils::globalVariables(c("df", "lower.limit", "upper.limit", "intrvl.width", "intrvl.level", "cdf", "pvalue", "svalue"))
