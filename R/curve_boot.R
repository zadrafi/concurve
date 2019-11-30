curve_boot <- function(data = data, func = func, replicates = 20000, steps = 1000) {
  intrvls <- 0.5 / steps
  alpha <- seq(0.00, 0.50, intrvls)
  pboptions(type = "timer", style = 1, char = "+")

  result <- bcajack(x = data, B = replicates, func = func, alpha = alpha, verbose = FALSE)


  z <- result[["lims"]]
  z <- as.data.frame(z)
  z <- as_tibble(rownames_to_column(z))
  colnames(z)[1] <- "alphaperc"
  z$alphaperc <- as.numeric(z$alphaperc)
  1:length(alpha)

  # Data Frame with BCA Intervals ------------------------------
  pboptions(type = "timer", style = 1, char = "+")
  bca <- pblapply(1:length(alpha), FUN = function(i) c(nth(z$bca, i), nth(z$bca, -i)), cl = detectCores() - 1)
  bcaintervals <- data.frame(do.call(rbind, bca))
  intrvl.limit <- c("lower.limit", "upper.limit")
  colnames(bcaintervals) <- intrvl.limit
  news <- pblapply(1:length(alpha), FUN = function(i) nth(z$bca, -i) - nth(z$bca, i), cl = detectCores() - 1)
  width <- data.frame(do.call(rbind, news))
  colnames(width) <- "intrvl.width"
  bews <- pblapply(1:length(alpha), FUN = function(i) nth(z$alphaperc, -i) - nth(z$alphaperc, i), cl = detectCores() - 1)
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

  # Data Frame with Standard Intervals ------------------------------

  std <- pblapply(1:length(alpha), FUN = function(i) c(nth(z$std, i), nth(z$std, -i)), cl = detectCores() - 1)
  stdintervals <- data.frame(do.call(rbind, std))
  intrvl.limit <- c("lower.limit", "upper.limit")
  colnames(stdintervals) <- intrvl.limit
  news <- pblapply(1:length(alpha), FUN = function(i) nth(z$std, -i) - nth(z$std, i), cl = detectCores() - 1)
  width <- data.frame(do.call(rbind, news))
  colnames(width) <- "intrvl.width"
  bews <- pblapply(1:length(alpha), FUN = function(i) nth(z$alphaperc, -i) - nth(z$alphaperc, i), cl = detectCores() - 1)
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


  # Combine Data Frames -----------------------------------------------------

  dataframes <- list(df_std, df_bca)
  names(dataframes) <- c("Standard", "BCA")
  return(dataframes)
}

# RMD Check -----------------------------------------------------
utils::globalVariables(c("df", "lower.limit", "upper.limit", "intrvl.width", "intrvl.level", "cdf", "pvalue", "svalue"))
