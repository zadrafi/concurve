#' Consonance Functions From Monte Carlo Confidence Distributions
#'
#' Builds a consonance object from draws of a confidence distribution
#' obtained by simulation: for example a generalized fiducial distribution
#' sampled with Stan, a bootstrap distribution, or any other Monte Carlo
#' approximation whose quantiles are to be read as confidence limits. The
#' limits of the interval at each level are the corresponding sample
#' quantiles of the draws, so the \eqn{100(1-\alpha)\%} interval is
#' \eqn{[Q(\alpha/2), Q(1-\alpha/2)]}. The output has the same structure
#' as that of [curve_gen()] and [curve_analytic()], so [ggcurve()],
#' [curve_compare()], [plot_compare()], and [curve_table()] work unchanged.
#'
#' No Stan installation is needed for this function. [curve_stan_fit()]
#' wraps the sampling step for users who have \pkg{rstan} installed.
#'
#' @param draws A numeric vector of draws from the confidence distribution
#' of the parameter of interest.
#' @param steps Indicates how many consonance intervals are to be
#' calculated at various levels. By default, it is set to 1000.
#' @param table Indicates whether or not a table output with some relevant
#' statistics should be generated. The default is TRUE and generates a
#' table which is included in the list object.
#'
#' @return A list with 3 items where the dataframe of values is in the
#' first object, the values needed to calculate the density function in
#' the second, and the table for the values in the third if
#' \code{table = TRUE}.
#'
#' @details
#' A confidence distribution is a sample-dependent distribution function on
#' the parameter space whose quantiles are confidence limits at every level
#' (Xie & Singh, 2013). When it is available only as Monte Carlo draws, the
#' empirical quantiles converge to the true limits, with Monte Carlo error
#' that is largest in the extreme tails. Use enough draws (tens of
#' thousands) if the 99\% limits matter.
#'
#' Whether a set of draws is a valid confidence distribution is a property
#' of how the draws were generated, not of this function: a Bayesian
#' posterior under an informative prior, for example, generally is not one.
#' Generalized fiducial distributions (Hannig et al., 2016) and posteriors
#' under matching priors are the usual sources.
#'
#' @references
#' Xie M, Singh K. Confidence distribution, the frequentist distribution
#' estimator of a parameter: a review. Int Stat Rev. 2013;81(1):3-39.
#'
#' Hannig J, Iyer H, Lai RCS, Lee TCM. Generalized fiducial inference: a
#' review and new results. J Am Stat Assoc. 2016;111(515):1346-1361.
#'
#' Schweder T, Hjort NL. Confidence, Likelihood, Probability. Cambridge
#' University Press; 2016.
#'
#' @examples
#' # Draws from the exact confidence distribution of a normal mean with
#' # n = 12: t_{11} scaled by the standard error.
#' set.seed(4821)
#' y <- rnorm(12, 3.2, 1.4)
#' draws <- mean(y) + sd(y) / sqrt(12) * rt(20000, df = 11)
#' cd <- curve_stan(draws)
#' ggcurve(cd[[1]], type = "c", nullvalue = 0)
#'
#' # Agrees with the analytic t-based curve
#' an <- curve_analytic(mean(y), se = sd(y) / sqrt(12), df = 11, dist = "t")
#' plot_compare(cd[[1]], an[[1]], type = "c")
#'
#' @seealso [curve_stan_fit()] to sample a bundled or user-supplied Stan
#' model with \pkg{rstan}; [curve_analytic()] for closed-form curves.
#'
#' @export
curve_stan <- function(draws, steps = 1000, table = TRUE) {
  draws <- as.numeric(draws)
  draws <- draws[is.finite(draws)]
  if (length(draws) < 100) {
    stop("'draws' must contain at least 100 finite values.", call. = FALSE)
  }
  if (!is.numeric(steps) || length(steps) != 1 || steps < 10) {
    stop("'steps' must be a single number of at least 10.", call. = FALSE)
  }

  intrvls <- (1:steps) / steps
  alpha_half <- (1 - intrvls) / 2

  lower <- stats::quantile(draws, probs = alpha_half, type = 8, names = FALSE)
  upper <- stats::quantile(draws, probs = 1 - alpha_half, type = 8, names = FALSE)

  df_out <- data.frame(lower.limit = lower, upper.limit = upper)
  df_out$intrvl.width <- abs(df_out$upper.limit - df_out$lower.limit)
  df_out$intrvl.level <- intrvls
  df_out$cdf <- (abs(df_out$intrvl.level / 2)) + 0.5
  df_out$pvalue <- 1 - intrvls
  df_out$svalue <- -log2(df_out$pvalue)
  df_out <- utils::head(df_out, -1)
  class(df_out) <- c("data.frame", "concurve")

  densdf <- data.frame(x = c(df_out$lower.limit, df_out$upper.limit))
  densdf <- utils::head(densdf, -1)
  class(densdf) <- c("data.frame", "concurve")

  if (isTRUE(table)) {
    levels <- c(0.25, 0.50, 0.75, 0.80, 0.85, 0.90, 0.95, 0.975, 0.99)
    df_subintervals <- curve_table(df_out, levels, type = "c", format = "data.frame")
    class(df_subintervals) <- c("data.frame", "concurve")
    dataframes <- list(df_out, densdf, df_subintervals)
    names(dataframes) <- c("Intervals Dataframe", "Intervals Density", "Intervals Table")
    class(dataframes) <- "concurve"
    return(dataframes)
  }

  list(df_out, densdf)
}


#' Sample a Stan Model and Build a Consonance Function
#'
#' Compiles (if necessary) and samples a Stan program with \pkg{rstan},
#' extracts the draws for one parameter, and passes them to [curve_stan()].
#' The Stan program may be one of the models bundled with \pkg{concurve}
#' (see [concurve_stan_file()]), a path to a \code{.stan} file, or an
#' already compiled \code{stanmodel} object.
#'
#' \pkg{rstan} is not a hard dependency of \pkg{concurve}; the package
#' installs and works without it, and this function stops with an
#' informative message if it is unavailable. Models are compiled with the
#' C++ toolchain at first use, which takes on the order of a minute, and
#' the compiled object is cached for the rest of the session (and on disk
#' if \code{rstan::rstan_options(auto_write = TRUE)} is set).
#'
#' @param model A \code{stanmodel} object, a path to a \code{.stan} file,
#' or the name of a bundled model accepted by [concurve_stan_file()].
#' @param data A named list of data for the Stan program.
#' @param parameter The name of the (scalar) parameter whose confidence
#' distribution is wanted.
#' @param ... Further arguments passed to \code{rstan::sampling()}, such as
#' \code{chains}, \code{iter}, \code{warmup}, \code{seed}, and
#' \code{cores}.
#' @inheritParams curve_stan
#'
#' @return As [curve_stan()]. The \code{stanfit} object is attached as the
#' attribute \code{"stanfit"} for diagnostics.
#'
#' @details
#' The draws are taken as a Monte Carlo approximation to a confidence
#' distribution. That is justified when the Stan program's target density
#' is a generalized fiducial density (Hannig et al., 2016), as in the
#' bundled \code{"normal_gfd"} model, or a posterior under a probability
#' matching prior. It is not justified for arbitrary Bayesian models; see
#' [curve_stan()].
#'
#' @examples
#' \dontrun{
#' # Requires rstan and a working C++ toolchain; compiles on first use.
#' set.seed(4821)
#' y <- rnorm(12, 3.2, 1.4)
#' gfd <- curve_stan_fit(
#'   "normal_gfd",
#'   data = list(N = length(y), y = y), parameter = "mu",
#'   chains = 4, iter = 22000, warmup = 2000, seed = 1859
#' )
#' ggcurve(gfd[[1]], type = "c")
#'
#' # The GFD marginal for mu is exactly Student-t, so this should match:
#' an <- curve_analytic(mean(y), se = sd(y) / sqrt(12), df = 11, dist = "t")
#' plot_compare(gfd[[1]], an[[1]], type = "c")
#' }
#'
#' @seealso [curve_stan()], [concurve_stan_file()]
#'
#' @export
curve_stan_fit <- function(model, data, parameter, ..., steps = 1000, table = TRUE) {
  if (!requireNamespace("rstan", quietly = TRUE)) {
    stop(
      "Package 'rstan' is required for curve_stan_fit(). ",
      "Install it with install.packages(\"rstan\"), or sample the model ",
      "yourself and pass the draws to curve_stan().",
      call. = FALSE
    )
  }
  if (!is.character(parameter) || length(parameter) != 1) {
    stop("'parameter' must be a single parameter name.", call. = FALSE)
  }

  if (is.character(model)) {
    path <- if (file.exists(model)) model else concurve_stan_file(model)
    model <- stan_model_cached(path)
  } else if (!methods::is(model, "stanmodel")) {
    stop("'model' must be a stanmodel, a path to a .stan file, or a bundled model name.",
      call. = FALSE
    )
  }

  fit <- rstan::sampling(model, data = data, pars = parameter, refresh = 0, ...)
  draws <- rstan::extract(fit, pars = parameter, permuted = TRUE)[[parameter]]
  if (!is.null(dim(draws)) && length(dim(draws)) > 1) {
    stop("'parameter' must be scalar; '", parameter, "' has dimensions ",
      paste(dim(draws)[-1], collapse = "x"), ".",
      call. = FALSE
    )
  }

  out <- curve_stan(as.numeric(draws), steps = steps, table = table)
  attr(out, "stanfit") <- fit
  out
}


#' Locate Stan Programs Bundled With concurve
#'
#' Returns the path to one of the Stan programs shipped in
#' \code{inst/stan}. They are plain text files and are compiled only on
#' demand by [curve_stan_fit()] or by the user with
#' \code{rstan::stan_model()}.
#'
#' @param model One of:
#' \describe{
#'   \item{\code{"normal_gfd"}}{Generalized fiducial distribution for the
#'   normal location-scale model, \eqn{L(\mu,\sigma)/\sigma}. Data:
#'   \code{N}, \code{y}. The marginal for \code{mu} is exactly
#'   \eqn{t_{n-1}}.}
#'   \item{\code{"normal_profile"}}{Normal likelihood with \code{mu_fixed}
#'   passed as data, for profiling over \code{sigma} with
#'   \code{rstan::optimizing()}. Data: \code{N}, \code{y}, \code{mu_fixed}.}
#'   \item{\code{"normal_mle"}}{Unrestricted normal likelihood, for the
#'   joint MLE. Data: \code{N}, \code{y}.}
#' }
#'
#' @return A file path.
#'
#' @examples
#' concurve_stan_file("normal_gfd")
#' cat(readLines(concurve_stan_file("normal_gfd")), sep = "\n")
#'
#' @seealso [curve_stan_fit()]
#'
#' @export
concurve_stan_file <- function(model = c("normal_gfd", "normal_profile", "normal_mle")) {
  model <- match.arg(model)
  path <- system.file("stan", paste0(model, ".stan"), package = "concurve", mustWork = FALSE)
  if (!nzchar(path)) {
    stop("Bundled Stan program '", model, "' not found.", call. = FALSE)
  }
  path
}


# Session cache of compiled stanmodel objects, keyed by normalized path.
.stan_model_cache <- new.env(parent = emptyenv())

stan_model_cached <- function(path) {
  key <- normalizePath(path, mustWork = TRUE)
  if (!exists(key, envir = .stan_model_cache, inherits = FALSE)) {
    assign(key, rstan::stan_model(file = key), envir = .stan_model_cache)
  }
  get(key, envir = .stan_model_cache, inherits = FALSE)
}
