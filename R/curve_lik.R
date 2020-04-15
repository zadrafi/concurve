#' Compute Profile Likelihood Functions
#'
#' @param likobject An object from the ProfileLikelihood package
#' @param data The dataframe that was used to create the likelihood
#' object in the ProfileLikelihood package.
#' @param table Indicates whether or not a table output with some relevant
#' statistics should be generated. The default is TRUE and generates a table
#' which is included in the list object.
#'
#' @return A list with 2 items where the dataframe of values is in the first
#' object, and the table for the values in the second if table = TRUE.
#'
#' @examples
#'
#' library(ProfileLikelihood)
#' data(dataglm)
#' xx <- profilelike.glm(y ~ x1 + x2, dataglm, profile.theta = "group", binomial("logit"))
#' lik <- curve_lik(xx, dataglm)
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
