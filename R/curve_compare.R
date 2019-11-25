curve_compare <- function(data1, data2, type = "consonance", plot = TRUE, ...) {

  # Consonance Function -----------------------------------------------------

  if (type == "consonance") {
    if (is.data.frame(data1) != TRUE) {
      stop("Error: 'x' must be a data frame from 'concurve'.")
    }
    if (ncol(data1) != 6) {
      stop("Error: 'x' must be a data frame from 'concurve'.")
    }
    if (is.data.frame(data2) != TRUE) {
      stop("Error: 'x' must be a data frame from 'concurve'.")
    }
    if (ncol(data2) != 6) {
      stop("Error: 'x' must be a data frame from 'concurve'.")
    }
    if (plot == TRUE) {
      plot_comparison <- (plot_compare(data1, data2, type = "consonance", ...))
    } else if (plot == FALSE) {

    }

    df1 <- pivot_longer(data1, lower.limit:upper.limit, names_to = "limit.bound", values_to = "Limit")

    df2 <- pivot_longer(data2, lower.limit:upper.limit, names_to = "limit.bound", values_to = "Limit")

    df1 <- data.frame(
      "x" = df1$Limit,
      "y" = df1$pvalue
    )
    df2 <- data.frame(
      "x" = df2$Limit,
      "y" = df2$pvalue
    )

    if (max(df1$x) < min(df2$x) || min(df1$x) > max(df2$x)) {
      print("Out of Range, AUC = 0")
    } else {
      f0 <- approxfun(df1$x, df1$y, ties = "mean")
      f1 <- approxfun(df2$x, df2$y, ties = "mean")
      f <- Vectorize(function(x) {
        min(f0(x), f1(x))
      })
      domain <- c(
        max(min(df1$x), min(df2$x)),
        min(max(df1$x), max(df2$x))
      )
      AUC_1 <- integrate(f0, min(df1$x), max(df1$x))$value
      AUC_2 <- integrate(f1, min(df2$x), max(df2$x))$value
      AUC_shared <- integrate(f, domain[1], domain[2])$value

      AUC_overlap <- (AUC_shared / (AUC_1 + AUC_2 - AUC_shared))
      AUC_ratio <- (AUC_shared / (AUC_1 + AUC_2 - 2 * AUC_shared))

      AUC_results <- data.frame(AUC_1, AUC_2, AUC_shared, AUC_overlap, AUC_ratio)
      AUC_results <- round(AUC_results, digits = 3)
      names <- c("AUC 1", "AUC 2", "Shared AUC", "AUC Overlap (%)", "Overlap:Non-Overlap AUC Ratio")
      colnames(AUC_results) <- names
      AUC_results <- knitr::kable(
        AUC_results,
        booktabs = TRUE
      )
      print("AUC = Area Under the Curve")
      return(list(AUC_results, plot_comparison))
    }


    # Surprisal Function ------------------------------------------------------
  } else if (type == "surprisal") {
    if (is.data.frame(data1) != TRUE) {
      stop("Error: 'x' must be a data frame from 'concurve'.")
    }
    if (ncol(data1) != 6) {
      stop("Error: 'x' must be a data frame from 'concurve'.")
    }
    if (is.data.frame(data2) != TRUE) {
      stop("Error: 'x' must be a data frame from 'concurve'.")
    }
    if (ncol(data2) != 6) {
      stop("Error: 'x' must be a data frame from 'concurve'.")
    }

    if (plot == TRUE) {
      plot_comparison <- (plot_compare(data1, data2, type = "surprisal", ...))
    } else if (plot == FALSE) {

    }

    df1 <- pivot_longer(data1, lower.limit:upper.limit, names_to = "limit.bound", values_to = "Limit")

    df2 <- pivot_longer(data2, lower.limit:upper.limit, names_to = "limit.bound", values_to = "Limit")

    df1 <- data.frame(
      "x" = df1$Limit,
      "y" = max(df1$svalue) - df1$svalue
    )
    df2 <- data.frame(
      "x" = df2$Limit,
      "y" = max(df2$svalue) - df2$svalue
    )

    if (max(df1$x) < min(df2$x) || min(df1$x) > max(df2$x)) {
      print("Out of Range, AUC = 0")
    } else {
      f0 <- approxfun(df1$x, df1$y, ties = "mean")
      f1 <- approxfun(df2$x, df2$y, ties = "mean")
      f <- Vectorize(function(x) {
        min(f0(x), f1(x))
      })
      domain <- c(
        max(min(df1$x), min(df2$x)),
        min(max(df1$x), max(df2$x))
      )
      AUC_1 <- integrate(f0, min(df1$x), max(df1$x))$value
      AUC_2 <- integrate(f1, min(df2$x), max(df2$x))$value
      AUC_shared <- integrate(f, domain[1], domain[2])$value

      AUC_overlap <- (AUC_shared / (AUC_1 + AUC_2 - AUC_shared))
      AUC_ratio <- (AUC_shared / (AUC_1 + AUC_2 - 2 * AUC_shared))

      AUC_results <- data.frame(AUC_1, AUC_2, AUC_shared, AUC_overlap, AUC_ratio)
      AUC_results <- round(AUC_results, digits = 3)
      names <- c("AUC 1", "AUC 2", "Shared AUC", "AUC Overlap (%)", "Overlap:Non-Overlap AUC Ratio")
      colnames(AUC_results) <- names
      AUC_results <- knitr::kable(
        AUC_results,
        booktabs = TRUE
      )
      print("AUC = Area Under the Curve")
      return(list(AUC_results, plot_comparison))
    }
  }
}
