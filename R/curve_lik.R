curve_lik <- function(likobject, data, table = TRUE) {
  values <- likobject[[1]] # theta values
  likelihood <- likobject[[2]] # profile likelihoods
  support <- likobject[[3]] # normalized profile likelihoods
  loglikelihood <- log(support) # log of normalized profile likelihoods
  deviancestat <- -(loglikelihood) # deviance statistic

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

utils::globalVariables(c("likfunction", "values", "likelihood", "loglikelihood", "support", "deviancestat"))
