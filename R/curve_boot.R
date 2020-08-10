#' Generate Consonance Functions via Bootstrapping
#'
#' Use the Bca bootstrap method and the t-boostrap method from the bcaboot and boot packages
#' to generate consonance distrbutions.
#'
#' @param data Dataset that is being used to create a consonance function.
#' @param func Custom function that is used to create parameters of interest that
#' will be bootstrapped.
#' @param method The boostrap method that will be used to generate the functions.
#' Methods include "bca" which is the default, "bcapar", which is parametric
#' bootstrapping using the bca method and "t", for the t-bootstrap/percentile method.
#' @param t0 	Only used for the "bcapar" method.
#' Observed estimate of theta, usually by maximum likelihood.
#' @param tt Only used for the "bcapar" method.
#' A vector of parametric bootstrap replications of theta of length B,
#' usually large, say B = 2000
#' @param bb Only used for the "bcapar" method.
#' A B by p matrix of natural sufficient vectors,
#' where p is the dimension of the exponential family.
#' @param replicates Indicates how many bootstrap replicates are to be performed.
#' The default is currently 20000 but more may be desirable, especially to make
#' the functions more smooth.
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
#' @return A list with 7 items where the dataframe of standard values is in the first
#' list and the table for it in the second if table = TRUE. The Bca intervals and table
#' are found in the third and fourth list. The values for the density function are in
#' the fifth object, while the Bca stats are in the sixth and seventh objects.
#'
#'

curve_boot <- function(data = data, func = func, method = "bca", t0, tt, bb,
                       replicates = 2000, steps = 1000, cores = getOption("mc.cores", 1L), table = TRUE) {


  # BCA Non-Parametric Bootstrap Method  ---------------------------------------------------

  if (method == "bca") {
    intrvls <- 0.5 / steps
    alpha <- seq(0.00, 0.50, intrvls)
    result <- bcajack(x = data, B = replicates, func = func, alpha = alpha, verbose = TRUE)


    z <- result[["lims"]]
    z <- as.data.frame(z)
    z <- tibble::as_tibble(rownames_to_column(z))
    colnames(z)[1] <- "alphaperc"
    z$alphaperc <- as.numeric(z$alphaperc)
    1:length(alpha)


    # Bootstrap Statistics ----------------------------------------------------

    bootstats <- result[["stats"]]
    bootstats <- as.data.frame(bootstats)
    class(bootstats) <- c("data.frame", "concurve")

    bcastats <- result[["ustats"]]
    bcastats <- as.data.frame(bcastats)
    class(bcastats) <- c("data.frame", "concurve")

    # Data Frame with BCA Intervals ------------------------------

    bca <- pbmclapply(1:length(alpha), FUN = function(i) c(nth(z$bca, i), nth(z$bca, -i)), mc.cores = cores)
    bcaintervals <- data.frame(do.call(rbind, bca))
    intrvl.limit <- c("lower.limit", "upper.limit")
    colnames(bcaintervals) <- intrvl.limit
    news <- pbmclapply(1:length(alpha), FUN = function(i) nth(z$bca, -i) - nth(z$bca, i), mc.cores = cores)
    width <- data.frame(do.call(rbind, news))
    colnames(width) <- "intrvl.width"
    bews <- pbmclapply(1:length(alpha), FUN = function(i) nth(z$alphaperc, -i) - nth(z$alphaperc, i), mc.cores = cores)
    levels <- data.frame(do.call(rbind, bews))
    colnames(levels) <- "intrvl.level"

    df_bca <- data.frame(bcaintervals$lower.limit, bcaintervals$upper.limit, width$intrvl.width, levels$intrvl.level)
    df_names <- c("lower.limit", "upper.limit", "intrvl.width", "intrvl.level")
    colnames(df_bca) <- df_names
    df_bca$cdf <- (abs(df_bca$intrvl.level / 2)) + 0.5
    df_bca$pvalue <- 1 - df_bca$intrvl.level
    df_bca$svalue <- -log2(df_bca$pvalue)
    df_bca <- head(df_bca, -1)
    df_bca <- df_bca[-1, ]
    class(df_bca) <- c("data.frame", "concurve")

    # Data Frame with Standard Intervals ------------------------------

    std <- pbmclapply(1:length(alpha), FUN = function(i) c(nth(z$std, i), nth(z$std, -i)), mc.cores = cores)
    stdintervals <- data.frame(do.call(rbind, std))
    intrvl.limit <- c("lower.limit", "upper.limit")
    colnames(stdintervals) <- intrvl.limit
    news <- pbmclapply(1:length(alpha), FUN = function(i) nth(z$std, -i) - nth(z$std, i), mc.cores = cores)
    width <- data.frame(do.call(rbind, news))
    colnames(width) <- "intrvl.width"
    bews <- pbmclapply(1:length(alpha), FUN = function(i) nth(z$alphaperc, -i) - nth(z$alphaperc, i), mc.cores = cores)
    levels <- data.frame(do.call(rbind, bews))
    colnames(levels) <- "intrvl.level"

    df_std <- data.frame(stdintervals$lower.limit, stdintervals$upper.limit, width$intrvl.width, levels$intrvl.level)
    df_names <- c("lower.limit", "upper.limit", "intrvl.width", "intrvl.level")
    colnames(df_std) <- df_names
    df_std$cdf <- (abs(df_std$intrvl.level / 2)) + 0.5
    df_std$pvalue <- 1 - df_std$intrvl.level
    df_std$svalue <- -log2(df_std$pvalue)
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
      dataframes <- list(df_std, std_subintervals, df_bca, bca_subintervals, bootstats, bcastats)
      names(dataframes) <- c("Standard Intervals", "Standard Table", "BCA Intervals", "BCA Table", "Bootstrap Statistics", "BCA Statistics")
      class(dataframes) <- "concurve"
      return(dataframes)
    } else if (table == FALSE) {
      dataframes <- list(df_std, df_bca, bootstats, bcastats)
      names(dataframes) <- c("Standard", "BCA", "Bootstrap Statistics", "BCA Statistics")
      class(dataframes) <- "concurve"
      return(dataframes)
    }


    # Parametric BCA Bootstrap Method -----------------------------------------
  } else if (method == "bcapar") {
    intrvls <- 0.5 / steps
    alpha <- seq(0.00, 0.50, intrvls)

    result <- bcapar(t0 = t0, tt = tt, bb = bb, alpha = alpha, cd = 1)

    # Parametric Bootstrap Statistics -----------------------------------------

    bootstats <- result[["stats"]]
    bootstats <- as.data.frame(bootstats)
    class(bootstats) <- c("data.frame", "concurve")

    bcastats <- result[["ustats"]]
    bcastats <- as.data.frame(bcastats)
    class(bcastats) <- c("data.frame", "concurve")


    # Parametric BCA Bootstrap Density ----------------------------------------

    densdf <- result[["w"]]
    densdf <- as.data.frame(densdf)
    class(densdf) <- c("data.frame", "concurve")
    colnames(densdf) <- "x"

    z <- result[["lims"]]
    z <- as.data.frame(z)
    z <- tibble::as_tibble(rownames_to_column(z))
    colnames(z)[1] <- "alphaperc"
    z$alphaperc <- as.numeric(z$alphaperc)
    1:length(alpha)

    bca <- pbmclapply(1:length(alpha), FUN = function(i) c(nth(z$bca, i), nth(z$bca, -i)), mc.cores = cores)
    bcaintervals <- data.frame(do.call(rbind, bca))
    intrvl.limit <- c("lower.limit", "upper.limit")
    colnames(bcaintervals) <- intrvl.limit
    news <- pbmclapply(1:length(alpha), FUN = function(i) nth(z$bca, -i) - nth(z$bca, i), mc.cores = cores)
    width <- data.frame(do.call(rbind, news))
    colnames(width) <- "intrvl.width"
    bews <- pbmclapply(1:length(alpha), FUN = function(i) nth(z$alphaperc, -i) - nth(z$alphaperc, i), mc.cores = cores)
    levels <- data.frame(do.call(rbind, bews))
    colnames(levels) <- "intrvl.level"

    df_bca <- data.frame(bcaintervals$lower.limit, bcaintervals$upper.limit, width$intrvl.width, levels$intrvl.level)
    df_names <- c("lower.limit", "upper.limit", "intrvl.width", "intrvl.level")
    colnames(df_bca) <- df_names
    df_bca$cdf <- (abs(df_bca$intrvl.level / 2)) + 0.5
    df_bca$pvalue <- 1 - df_bca$intrvl.level
    df_bca$svalue <- -log2(df_bca$pvalue)
    df_bca <- head(df_bca, -1)
    df_bca <- df_bca[-1, ]
    class(df_bca) <- c("data.frame", "concurve")

    # Data Frame with Standard Intervals ------------------------------

    std <- pbmclapply(1:length(alpha), FUN = function(i) c(nth(z$std, i), nth(z$std, -i)), mc.cores = cores)
    stdintervals <- data.frame(do.call(rbind, std))
    intrvl.limit <- c("lower.limit", "upper.limit")
    colnames(stdintervals) <- intrvl.limit
    news <- pbmclapply(1:length(alpha), FUN = function(i) nth(z$std, -i) - nth(z$std, i), mc.cores = cores)
    width <- data.frame(do.call(rbind, news))
    colnames(width) <- "intrvl.width"
    bews <- pbmclapply(1:length(alpha), FUN = function(i) nth(z$alphaperc, -i) - nth(z$alphaperc, i), mc.cores = cores)
    levels <- data.frame(do.call(rbind, bews))
    colnames(levels) <- "intrvl.level"

    df_std <- data.frame(stdintervals$lower.limit, stdintervals$upper.limit, width$intrvl.width, levels$intrvl.level)
    df_names <- c("lower.limit", "upper.limit", "intrvl.width", "intrvl.level")
    colnames(df_std) <- df_names
    df_std$cdf <- (abs(df_std$intrvl.level / 2)) + 0.5
    df_std$pvalue <- 1 - df_std$intrvl.level
    df_std$svalue <- -log2(df_std$pvalue)
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
      dataframes <- list(df_std, std_subintervals, df_bca, bca_subintervals, densdf, bootstats, bcastats)
      names(dataframes) <- c("Standard Intervals", "Standard Table", "BCA Intervals", "BCA Table", "BCA Density", "Bootstrap Statistics", "BCA Statistics")
      class(dataframes) <- "concurve"
      return(dataframes)
    } else if (table == FALSE) {
      dataframes <- list(df_std, df_bca, densdf, bootstats, bcastats)
      names(dataframes) <- c("Standard", "BCA", "BCA Density", "Bootstrap Statistics", "BCA Statistics")
      class(dataframes) <- "concurve"
      return(dataframes)
    }


    # Boot t Method For Density --------------------------------------
  } else if (method == "t") {
    t.boot <- boot(data = data, statistic = func, R = replicates, parallel = "multicore", ncpus = cores)

    intrvls <- 1:steps / steps

    t <- pbmclapply(intrvls,
      FUN = function(i) boot.ci(t.boot, conf = i, type = "perc")$perc[4:5],
      mc.cores = cores
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

    # Bootstrap Distribution
    boot_dens <- t.boot[["t"]]
    colnames(boot_dens) <- "x"
    boot_dens <- as.data.frame(boot_dens)
    boot_dens <- head(boot_dens, -1)
    class(boot_dens) <- c("data.frame", "concurve")

    # Interval Density
    densdf <- data.frame(c(df$lower.limit, df$upper.limit))
    colnames(densdf) <- "x"
    densdf <- head(densdf, -1)
    class(densdf) <- c("data.frame", "concurve")


    if (table == TRUE) {
      levels <- c(0.25, 0.50, 0.75, 0.80, 0.85, 0.90, 0.95, 0.975, 0.99)
      (df_subintervals <- (curve_table(df, levels, type = "c", format = "data.frame")))
      class(df_subintervals) <- c("data.frame", "concurve")
      dataframes <- list(df, boot_dens, densdf, df_subintervals)
      names(dataframes) <- c("Intervals Dataframe", "Bootstrap Distribution", "Intervals Density", "Intervals Table")
      class(dataframes) <- "concurve"
      return(dataframes)
    } else if (table == FALSE) {
      return(list(df, densdf))
    }
  }
}

# RMD Check -----------------------------------------------------
utils::globalVariables(c("df", "lower.limit", "upper.limit", "intrvl.width", "intrvl.level", "cdf", "pvalue", "svalue"))
