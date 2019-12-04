#' Generate Consonance Functions via Bootstrapping
#'
#' Use the BCa bootstrap method and the t boostrap method from the bcaboot and boot packages
#' to generate consonance distrbutions.
#'
#' @param data Dataset that is being used to create a consonance function.
#' @param func Custom function that is used to create parameters of interest that
#' will be bootstrapped.
#' @param method The boostrap method that will be used to generate the functions.
#' Methods include "bca" which is the default and "t".
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
#' @return A list with the dataframe of values in the first list and the table
#' in the second if table = TRUE.
#'
#' @examples
#'
#' \donttest{
#' data(diabetes, package = "bcaboot")
#' Xy <- cbind(diabetes$x, diabetes$y)
#' rfun <- function(Xy) {
#'   y <- Xy[, 11]
#'   X <- Xy[, 1:10]
#'   return(summary(lm(y ~ X))$adj.r.squared)
#' }
#'
#' x <- curve_boot(data = Xy, func = rfun, method = "bca", replicates = 200, steps = 1000)
#'
#' ggcurve(data = x[[1]])
#' ggcurve(data = x[[3]])
#'
#' plot_compare(x[[1]], x[[3]])
#' }
#'
curve_boot <- function(data = data, func = func, method = "bca", replicates = 2000, steps = 1000, table = TRUE) {


  # BCA Bootstrap Method  ---------------------------------------------------

  if (method == "bca") {
    intrvls <- 0.5 / steps
    alpha <- seq(0.00, 0.50, intrvls)
    result <- bcajack(x = data, B = replicates, func = func, alpha = alpha, verbose = TRUE)


    z <- result[["lims"]]
    z <- as.data.frame(z)
    z <- as_tibble(rownames_to_column(z))
    colnames(z)[1] <- "alphaperc"
    z$alphaperc <- as.numeric(z$alphaperc)
    1:length(alpha)

    # Data Frame with BCA Intervals ------------------------------

    bca <- pbmclapply(1:length(alpha), FUN = function(i) c(nth(z$bca, i), nth(z$bca, -i)), mc.cores = getOption("mc.cores", 1L))
    bcaintervals <- data.frame(do.call(rbind, bca))
    intrvl.limit <- c("lower.limit", "upper.limit")
    colnames(bcaintervals) <- intrvl.limit
    news <- pbmclapply(1:length(alpha), FUN = function(i) nth(z$bca, -i) - nth(z$bca, i), mc.cores = getOption("mc.cores", 1L))
    width <- data.frame(do.call(rbind, news))
    colnames(width) <- "intrvl.width"
    bews <- pbmclapply(1:length(alpha), FUN = function(i) nth(z$alphaperc, -i) - nth(z$alphaperc, i), mc.cores = getOption("mc.cores", 1L))
    levels <- data.frame(do.call(rbind, bews))
    colnames(levels) <- "intrvl.level"

    df_bca <- data.frame(bcaintervals$lower.limit, bcaintervals$upper.limit, levels$intrvl.level, width$intrvl.width)
    df_names <- c("lower.limit", "upper.limit", "intrvl.level", "intrvl.width")
    colnames(df_bca) <- df_names
    df_bca$pvalue <- 1 - df_bca$intrvl.level
    df_bca$svalue <- -log2(df_bca$pvalue)
    df_bca$cdf <- (abs(df_bca$intrvl.level / 2)) + 0.5
    df_bca <- head(df_bca, -1)
    df_bca <- df_bca[-1, ]
    class(df_bca) <- c("data.frame", "concurve")

    # Data Frame with Standard Intervals ------------------------------

    std <- pbmclapply(1:length(alpha), FUN = function(i) c(nth(z$std, i), nth(z$std, -i)), mc.cores = getOption("mc.cores", 1L))
    stdintervals <- data.frame(do.call(rbind, std))
    intrvl.limit <- c("lower.limit", "upper.limit")
    colnames(stdintervals) <- intrvl.limit
    news <- pbmclapply(1:length(alpha), FUN = function(i) nth(z$std, -i) - nth(z$std, i), mc.cores = getOption("mc.cores", 1L))
    width <- data.frame(do.call(rbind, news))
    colnames(width) <- "intrvl.width"
    bews <- pbmclapply(1:length(alpha), FUN = function(i) nth(z$alphaperc, -i) - nth(z$alphaperc, i), mc.cores = getOption("mc.cores", 1L))
    levels <- data.frame(do.call(rbind, bews))
    colnames(levels) <- "intrvl.level"

    df_std <- data.frame(stdintervals$lower.limit, stdintervals$upper.limit, levels$intrvl.level, width$intrvl.width)
    df_names <- c("lower.limit", "upper.limit", "intrvl.level", "intrvl.width")
    colnames(df_std) <- df_names
    df_std$pvalue <- 1 - df_std$intrvl.level
    df_std$svalue <- -log2(df_std$pvalue)
    df_std$cdf <- (abs(df_std$intrvl.level / 2)) + 0.5
    df_std <- head(df_std, -1)
    df_std <- df_std[-1, ]
    class(df_std) <- c("data.frame", "concurve")


    # Combine Data Frames -----------------------------------------------------

    if (table == TRUE) {
      levels <- c(0.25, 0.50, 0.75, 0.80, 0.85, 0.90, 0.95, 0.975, 0.99)
      (bca_subintervals <- (curve_table(df_bca, levels, type = "c", format = "data.frame")))
      class(bca_subintervals) <- c("data.frame", "concurve")
      (std_subintervals <- (curve_table(df_std, levels, type = "c", format = "data.frame")))
      class(std_subintervals) <- c("data.frame", "concurve")
      dataframes <- list(df_std, std_subintervals, df_bca, bca_subintervals)
      names(dataframes) <- c("Standard Intervals", "Standard Table", "BCA Intervals", "BCA Table")
      class(dataframes) <- "concurve"
      return(dataframes)
    } else if (table == FALSE) {
      dataframes <- list(df_std, df_bca)
      names(dataframes) <- c("Standard", "BCA")
      class(dataframes) <- "concurve"
      return(dataframes)
    }


    # Boot Percentile Method For Density --------------------------------------
  } else if (method == "t") {
    t.boot <- boot(data = data, statistic = func, R = replicates, parallel = "multicore", ncpus = getOption("mc.cores", 1L))

    intrvls <- 1:steps / steps

    t <- pbmclapply(intrvls,
      FUN = function(i) boot.ci(t.boot, conf = i, type = "perc")$perc[4:5],
      mc.cores = getOption("mc.cores", 1L)
    )

    df <- data.frame(do.call(rbind, t))
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
}

# RMD Check -----------------------------------------------------
utils::globalVariables(c("df", "lower.limit", "upper.limit", "intrvl.width", "intrvl.level", "cdf", "pvalue", "svalue"))
