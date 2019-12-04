#' Mean Interval Consonance Function
#'
#' Computes thousands of consonance (confidence) intervals for the chosen
#' parameter in a statistical test that compares means and places the interval
#' limits for each interval level into a data frame along with the corresponding
#' p-values and s-values.
#'
#' @param x Variable that contains the data for the first group being compared.
#' @param y Variable that contains the data for the second group being compared.
#' @param data Data frame from which the variables are being extracted from.
#' @param paired Indicates whether the statistical test is a paired difference test.
#' By default, it is set to "F",which means the function will be an unpaired
#' statistical test comparing two independent groups.Inserting "paired" will
#' change the test to a paired difference test.
#' @param method By default this is turned off (set to "default"), but
#' allows for bootstrapping if "boot" is insertedinto the function call.
#' @param replicates Indicates how many bootstrap replicates are to be performed.
#' The defaultis currently 20000 but more may be desirable, especially to make
#' the functions more smooth.
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
#' # Simulate random data
#' GroupA <- runif(100, min = 0, max = 100)
#' GroupB <- runif(100, min = 0, max = 100)
#' RandomData <- data.frame(GroupA, GroupB)
#' bob <- curve_mean(GroupA, GroupB, RandomData)
#' tibble::tibble(bob[[1]])
curve_mean <- function(x, y, data, paired = F, method = "default", replicates = 1000, steps = 10000, table = TRUE) {
  if (is.numeric(x) != TRUE) {
    stop("Error: 'x' must be a numeric vector")
  }
  if (is.numeric(y) != TRUE) {
    stop("Error: 'y' must be a numeric vector")
  }
  if (is.data.frame(data) != TRUE) {
    stop("Error: 'data' must be a data frame")
  }
  if (is.numeric(replicates) != TRUE) {
    stop("Error: 'replicates' must be a numeric vector")
  }
  if (is.numeric(steps) != TRUE) {
    stop("Error: 'steps' must be a numeric vector")
  }

  intrvls <- (0:steps) / steps
  if (method == "default") {
    results <- pbmclapply(intrvls, FUN = function(i) t.test(x, y, data = data, paired = paired, conf.level = i)$conf.int[], mc.cores = getOption("mc.cores", 1L))
  } else if (method == "boot") {
    diff <- mean(x) - mean(y)
    if (paired) {
      diffs <- x - y
      boot_dist <- replicate(replicates,
        expr = mean(diffs[sample(length(diffs), replace = T)])
      ) - diff
    } else {
      boot_dist <- replicate(replicates,
        expr = mean(sample(x, length(x), replace = T)) -
          mean(sample(y, length(y), replace = T))
      ) - diff
    }
    results <- pbmclapply(intrvls, FUN = function(i) diff - quantile(boot_dist, probs = (1 + c(i, -i)) / 2), mc.cores = getOption("mc.cores", 1L))
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
