#' Compute Modified Profile Likelihood Functions
#'
#' Computes a likelihood function for a scalar parameter of interest
#' using the modified profile likelihood (MPL) of Barndorff-Nielsen, as
#' implemented by [likelihoodAsy::logMPL()]. The ordinary profile
#' likelihood - which [curve_lik()] constructs via the ProfileLikelihood
#' package - treats nuisance parameter estimates as known, and can be
#' noticeably biased when nuisance parameters are numerous relative to
#' the sample size (e.g., stratified models, variance components). The
#' MPL adjusts for nuisance parameter estimation and is generally
#' preferable in those settings; for variance parameters in mixed
#' models it closely tracks REML.
#'
#' The user must supply the same ingredients required by
#' \code{likelihoodAsy}: a function evaluating the log likelihood, a
#' function that simulates a data set from the fitted model, the full
#' maximum likelihood estimate, and the index of the parameter of
#' interest within the parameter vector.
#'
#' Because the MPL, like any likelihood, is defined only up to a
#' multiplicative constant, the returned `likelihood` column is
#' normalized so that its maximum is 1 and is therefore identical to
#' the `support` column.
#'
#' @param data A list containing the data, in the format expected by
#' the user-supplied `floglik` and `datagen` functions (see the
#' likelihoodAsy documentation).
#' @param mle Numeric vector with the full maximum likelihood estimate
#' of the model parameter.
#' @param floglik A function of `(theta, data)` returning the log
#' likelihood at `theta`.
#' @param datagen A function of `(theta, data)` returning a copy of
#' `data` with the response simulated from the model at `theta`.
#' @param indpsi Integer index of the parameter of interest within
#' `theta`.
#' @param lo Lower limit of the grid of values for the parameter of
#' interest over which the modified profile likelihood is evaluated.
#' @param hi Upper limit of the grid of values for the parameter of
#' interest.
#' @param steps Number of grid points at which the modified profile
#' likelihood is evaluated. The default is 50; more points give a
#' smoother function at proportionally greater computational cost.
#' @param fscore Optional function of `(theta, data)` returning the
#' gradient of the log likelihood; supplying it speeds up computation.
#' @param R Number of Monte Carlo draws used for the Skovgaard-type
#' covariance approximations inside [likelihoodAsy::logMPL()]. The
#' default is 100, which suffices for (curved) exponential family
#' models; increase for stability in other models.
#' @param seed Optional seed for the Monte Carlo computation, for
#' reproducibility.
#' @param table Indicates whether or not a table output with some
#' relevant statistics should be generated. The default is TRUE and
#' generates a table which is included in the list object.
#' @param ... Further arguments passed to [likelihoodAsy::logMPL()].
#'
#' @return A list with 2 items where the dataframe of values is in the
#' first object, and the table for the values in the second if
#' table = TRUE.
#'
#' @references
#' Barndorff-Nielsen OE. On a formula for the distribution of the
#' maximum likelihood estimator. Biometrika. 1983;70:343-365.
#'
#' Pierce DA, Bellio R. Modern likelihood-frequentist inference.
#' International Statistical Review. 2017;85:519-541.
#'
#' @examples
#' \dontrun{
#' # Logistic regression on the crying babies data (cond package),
#' # parameter of interest = coefficient of lull (index 19)
#' lik <- curve_mpl(
#'   data = data.obj, mle = coef(mod.glm),
#'   floglik = loglik.logit, datagen = gendat.logit,
#'   indpsi = 19, lo = -0.3, hi = 3.7, seed = 2020
#' )
#' ggcurve(lik[[1]], type = "l1")
#' }
#' @seealso [curve_lik()], [curve_rstar()], [ggcurve()]
#' @export
curve_mpl <- function(data, mle, floglik, datagen, indpsi, lo, hi,
                      steps = 50, fscore = NULL, R = 100, seed = NULL,
                      table = TRUE, ...) {
  if (!requireNamespace("likelihoodAsy", quietly = TRUE)) {
    stop(
      "the 'likelihoodAsy' package is required for curve_mpl(); ",
      "please install it first"
    )
  }
  if (!is.function(floglik) || !is.function(datagen)) {
    stop("'floglik' and 'datagen' must be functions of (theta, data)")
  }
  if (!is.numeric(lo) || !is.numeric(hi) || lo >= hi) {
    stop("'lo' and 'hi' must be numeric with lo < hi")
  }

  values <- seq(from = lo, to = hi, length.out = steps)
  logmpl <- vapply(
    values,
    function(p) {
      likelihoodAsy::logMPL(
        psival = p, data = data, mle = mle, floglik = floglik,
        fscore = fscore, indpsi = indpsi, datagen = datagen,
        R = R, seed = seed, trace = FALSE, ...
      )
    },
    numeric(1)
  )

  loglikelihood <- logmpl - max(logmpl) # relative log MPL, max = 0
  support <- exp(loglikelihood) # normalized MPL, max = 1
  likelihood <- support # MPL is defined up to a constant
  # D = -2 log(L / Lmax), on the chi-squared(1) scale
  deviancestat <- -2 * loglikelihood

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
