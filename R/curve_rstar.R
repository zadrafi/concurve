#' Compute Second-Order Consonance Functions via the r* Statistic
#'
#' Converts an object produced by [likelihoodAsy::rstar.ci()] into a
#' consonance (confidence) function based on higher-order likelihood
#' asymptotics. The r* statistic (Barndorff-Nielsen; Skovgaard) yields
#' interval estimates and P-values that are second-order accurate, unlike
#' the first-order Wald and likelihood-root intervals produced by the
#' other constructors in this package. This is especially relevant for
#' small samples and models with many nuisance parameters, where
#' first-order consonance functions can be visibly miscalibrated.
#'
#' The confidence limits at each level are obtained by inverting the
#' monotone relationship between the parameter of interest and the
#' statistic with the same interpolating smoothing spline used
#' internally by \code{likelihoodAsy} (Bellio & Pierce), so limits at
#' the 90\%, 95\%, and 99\% levels reproduce \code{$CIrs} (or
#' \code{$CIr}) from the input object.
#'
#' Levels that would require extrapolating beyond the grid of
#' \code{psivals} covered by the input object are dropped with a
#' message, since spline extrapolation outside the computed range of
#' the statistic is not reliable.
#'
#' @param object An object of class `rstarci` produced by
#' [likelihoodAsy::rstar.ci()].
#' @param statistic Indicates which statistic should be inverted to form
#' the consonance function. The default, `"rstar"`, uses the modified
#' (second-order) likelihood root r*. The alternative `"r"` uses the
#' first-order likelihood root, which is useful for comparing the two
#' functions with [plot_compare()].
#' @param steps Indicates how many consonance intervals should be
#' computed. The default is 1000, which corresponds to a fine grid of
#' levels; fewer steps will lead to a less smooth function.
#' @param table Indicates whether or not a table output with some
#' relevant statistics should be generated. The default is TRUE and
#' generates a table which is included in the list object.
#'
#' @return A list with 3 items where the dataframe of standard values
#' such as the levels and the interval limits are in the first object,
#' the values needed for the consonance density in the second object,
#' and the table of relevant statistics in the third if table = TRUE.
#'
#' @references
#' Pierce DA, Bellio R. Modern likelihood-frequentist inference.
#' International Statistical Review. 2017;85:519-541.
#'
#' Bellio R, Pierce DA. likelihoodAsy: Functions for Likelihood Asymptotics.
#'
#' @examples
#' \dontrun{
#' library(likelihoodAsy)
#' # log likelihood, data generator, and interest function as in
#' # the likelihoodAsy vignette (Weibull regression on the leuk data)
#' rs.int <- rstar.ci(
#'   data = data.fz, thetainit = c(0, 0, 0), floglik = loglik.Wbl,
#'   fpsi = psifcn.Wbl, datagen = gendat.Wbl, seed = 1223
#' )
#' curve1 <- curve_rstar(rs.int)
#' ggcurve(curve1[[1]], type = "c")
#' }
#' @seealso [ggcurve()], [curve_compare()], [curve_mpl()]
#' @export
curve_rstar <- function(object, statistic = "rstar", steps = 1000, table = TRUE) {
  if (!inherits(object, "rstarci")) {
    stop("'object' must be an 'rstarci' object produced by likelihoodAsy::rstar.ci()")
  }
  if (!statistic %in% c("rstar", "r")) {
    stop("'statistic' must be either \"rstar\" or \"r\"")
  }
  statvals <- if (statistic == "rstar") object$rsvals else object$rvals
  if (is.null(statvals)) {
    stop(
      "the requested statistic is not present in 'object'; ",
      "run likelihoodAsy::rstar.ci() with ronly = FALSE for r*"
    )
  }

  # Same inversion used by likelihoodAsy to produce $CIr / $CIrs:
  # a smoothing spline of psi against the statistic evaluated at
  # standard normal quantiles.
  invfit <- stats::smooth.spline(statvals, object$psivals, all.knots = TRUE)

  intrvls <- (1:(steps - 1)) / steps
  alpha <- 1 - intrvls
  zlo <- stats::qnorm(alpha / 2)
  zhi <- stats::qnorm(1 - alpha / 2)

  # Drop levels whose quantiles fall outside the computed range of the
  # statistic; spline extrapolation there is unreliable.
  rng <- range(statvals)
  keep <- (zlo >= rng[1]) & (zhi <= rng[2])
  if (any(!keep)) {
    message(
      sum(!keep), " level(s) beyond the computed grid of '",
      statistic, "' were dropped; widen the grid in rstar.ci() ",
      "to obtain more extreme consonance levels."
    )
  }
  intrvls <- intrvls[keep]
  zlo <- zlo[keep]
  zhi <- zhi[keep]

  lims <- t(vapply(
    seq_along(intrvls),
    function(i) sort(stats::predict(invfit, c(zlo[i], zhi[i]))$y),
    numeric(2)
  ))

  df <- data.frame(lower.limit = lims[, 1], upper.limit = lims[, 2])
  df$intrvl.width <- (df$upper.limit) - (df$lower.limit)
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
