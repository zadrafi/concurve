curve_table <- function(data, levels, type = "data.frame") {
  levels <- c(0.25, 0.50, 0.75, 0.80, 0.85, 0.90, 0.95, 0.975, 0.99)

  subdf <- pbmclapply(levels, FUN = function(i) (subset(data, intrvl.level == i)), mc.cores = detectCores() - 1)
  subdf <- data.frame(do.call(rbind, subdf))
  class(subdf) <- c("data.frame", "concurve")
  subdf$intrvl.level <- (subdf$intrvl.level * 100)
  subcolnames <- c("Lower Limit", "Upper Limit", "Interval Width", "Interval Level (%)", "CDF", "P-value", "S-value (bits)")
  colnames(subdf) <- subcolnames
  subdf <- round(subdf, digits = 3)

  if (type == "data.frame") {
    return(subdf)
  } else if (type == "tibble") {
    subdf <- tibble::tibble(subdf)
    return(subdf)
  } else if (type == "docx") {
    subdf <- flextable(subdf)
    subdf <- autofit(subdf)
    subdf
    return(print(subdf, preview = "docx"))
  } else if (type == "pptx") {
    subdf <- flextable(subdf)
    subdf <- autofit(subdf)
    subdf
    return(print(subdf, preview = "pptx"))
  } else if (type == "latex") {
    subdf <- knitr::kable(
      subdf,
      booktabs = TRUE,
      label = "A table of some interval estimates at various levels and corresponding statistics."
    )
    return(subdf)
  } else if (type == "image") {
    subdf <- flextable(subdf)
    subdf <- autofit(subdf)
    subdf
    return(subdf)
  }
}

utils::globalVariables(c("subdf", "Lower Limit", "Upper Limit", "Interval Width", "Interval Level", "CDF", "P-value", "S-value"))
