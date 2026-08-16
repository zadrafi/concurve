#' Construct Likelihood Function for Statistical Models
#'
#' @description
#' Build likelihood, log-likelihood, and deviance functions from scratch
#' without external dependencies. Supports profile likelihood and
#' likelihood-based inference.
#'
#' @param model Fitted model object (lm, glm, etc.) or NULL to build from scratch
#' @param data Data frame (required if model is NULL)
#' @param formula Model formula (required if model is NULL)
#' @param family Family object for GLMs (default: gaussian)
#' @param method Method for likelihood construction: "auto", "numeric", "analytic"
#'
#' @details
#' Based on:
#' - Pawitan (2001). In All Likelihood. Oxford University Press.
#' - Venzon & Moolgavkar (1988). A method for computing profile-likelihood-based
#'   confidence intervals. Applied Statistics, 37(1), 87-94.
#' - Murphy & van der Vaart (2000). On profile likelihood. JASA, 95(450), 449-465.
#'
#' @return List containing likelihood functions and methods
#' @export
construct_likelihood <- function(model = NULL,
                                 data = NULL,
                                 formula = NULL,
                                 family = gaussian(),
                                 method = c("auto", "numeric", "analytic")) {
  method <- match.arg(method)

  # ============================================================================
  # Extract or validate inputs
  # ============================================================================

  if (!is.null(model)) {
    # Extract from existing model
    data <- model$model
    formula <- formula(model)
    family <- if (inherits(model, "glm")) model$family else gaussian()
    y <- model$y
    if (is.null(y)) y <- model.response(model.frame(model))
    X <- model.matrix(model)
  } else {
    # Build from scratch
    if (is.null(data) || is.null(formula)) {
      stop("Must provide either 'model' or both 'data' and 'formula'",
        call. = FALSE
      )
    }

    mf <- model.frame(formula, data = data)
    y <- model.response(mf)
    X <- model.matrix(formula, data = data)
  }

  n <- length(y)
  p <- ncol(X)

  # Parameter names
  param_names <- colnames(X)

  # ============================================================================
  # Construct log-likelihood function
  # ============================================================================

  loglik_func <- .make_loglik_function(y, X, family)

  # ============================================================================
  # Construct deviance function
  # ============================================================================

  deviance_func <- function(params) {
    -2 * loglik_func(params)
  }

  # ============================================================================
  # Construct score function (gradient)
  # ============================================================================

  score_func <- .make_score_function(y, X, family)

  # ============================================================================
  # Construct information matrix function
  # ============================================================================

  information_func <- .make_information_function(y, X, family)

  # ============================================================================
  # Get MLE (if model provided, use it; otherwise optimize)
  # ============================================================================

  if (!is.null(model)) {
    mle <- coef(model)
    if (inherits(model, "glm")) {
      dispersion <- summary(model)$dispersion
    } else {
      dispersion <- summary(model)$sigma^2
    }
  } else {
    # Optimize to find MLE
    start_params <- rep(0, p)

    opt_result <- optim(
      par = start_params,
      fn = function(params) -loglik_func(params),
      gr = function(params) -score_func(params),
      method = "BFGS",
      hessian = TRUE
    )

    if (opt_result$convergence != 0) {
      warning("Optimization did not converge. Results may be unreliable.",
        call. = FALSE
      )
    }

    mle <- opt_result$par
    names(mle) <- param_names

    # Estimate dispersion
    fitted_vals <- as.vector(X %*% mle)
    residuals <- y - family$linkinv(fitted_vals)
    dispersion <- sum(residuals^2) / (n - p)
  }

  # ============================================================================
  # Profile likelihood function
  # ============================================================================

  profile_lik <- .make_profile_likelihood(
    loglik_func = loglik_func,
    mle = mle,
    param_names = param_names
  )

  # ============================================================================
  # Likelihood-based confidence intervals
  # ============================================================================

  lik_confint <- .make_lik_confint(
    loglik_func = loglik_func,
    profile_lik = profile_lik,
    mle = mle,
    param_names = param_names
  )

  # ============================================================================
  # Return structure
  # ============================================================================

  structure(
    list(
      # Core functions
      loglik = loglik_func,
      deviance = deviance_func,
      score = score_func,
      information = information_func,

      # Profile likelihood
      profile = profile_lik,
      confint = lik_confint,

      # Estimates
      mle = mle,
      dispersion = dispersion,

      # Data
      y = y,
      X = X,
      n = n,
      p = p,
      param_names = param_names,
      family = family,

      # Metadata
      method = method,
      converged = if (!is.null(model)) TRUE else opt_result$convergence == 0
    ),
    class = "likelihood_function"
  )
}


# ============================================================================
# Internal: Create log-likelihood function
# ============================================================================

.make_loglik_function <- function(y, X, family) {
  n <- length(y)

  # Dispatch based on family
  if (family$family == "gaussian") {
    # Profile log-likelihood for beta (sigma2 concentrated out analytically).
    # At any beta, MLE of sigma2 is ssr/n, giving:
    #   l(beta) = -n/2 * (log(2*pi) + 1 + log(ssr/n))
    # This parameterization keeps params = beta only, matching coef(lm()).
    function(params) {
      eta <- as.vector(X %*% params)
      ssr <- sum((y - eta)^2)
      if (ssr <= 0) {
        return(0)
      }
      -n / 2 * (log(2 * pi) + 1 + log(ssr / n))
    }
  } else if (family$family == "binomial") {
    # Binomial log-likelihood
    function(params) {
      eta <- as.vector(X %*% params)
      mu <- family$linkinv(eta)

      # Add small constant to avoid log(0)
      mu <- pmax(pmin(mu, 1 - 1e-10), 1e-10)

      sum(y * log(mu) + (1 - y) * log(1 - mu))
    }
  } else if (family$family == "poisson") {
    # Poisson log-likelihood
    function(params) {
      eta <- as.vector(X %*% params)
      mu <- family$linkinv(eta)

      sum(y * log(mu) - mu - lfactorial(y))
    }
  } else if (family$family == "Gamma") {
    # Gamma log-likelihood
    function(params) {
      shape <- params[length(params)]
      beta <- params[-length(params)]

      eta <- as.vector(X %*% beta)
      mu <- family$linkinv(eta)

      sum(dgamma(y, shape = shape, scale = mu / shape, log = TRUE))
    }
  } else {
    # Generic using family$aic (requires dispersion)
    function(params) {
      eta <- as.vector(X %*% params)
      mu <- family$linkinv(eta)

      # Use deviance residuals
      dev_resids <- family$dev.resids(y, mu, wt = rep(1, length(y)))
      -sum(dev_resids) / 2
    }
  }
}


# ============================================================================
# Internal: Create score function (gradient)
# ============================================================================

.make_score_function <- function(y, X, family) {
  n <- length(y)

  if (family$family == "gaussian") {
    # Gradient of the profile log-likelihood w.r.t. beta:
    #   dl/dbeta = (n/ssr) * t(X) %*% residuals
    function(params) {
      residuals <- y - as.vector(X %*% params)
      ssr <- sum(residuals^2)
      if (ssr <= 0) {
        return(rep(0, length(params)))
      }
      n / ssr * as.vector(t(X) %*% residuals)
    }
  } else {
    # Numerical gradient for other families
    function(params) {
      numDeriv::grad(
        func = function(p) {
          eta <- as.vector(X %*% p)
          mu <- family$linkinv(eta)
          sum(family$dev.resids(y, mu, wt = rep(1, length(y))))
        },
        x = params
      )
    }
  }
}


# ============================================================================
# Internal: Create information matrix function
# ============================================================================

.make_information_function <- function(y, X, family) {
  n <- length(y)
  p <- ncol(X)

  if (family$family == "gaussian") {
    # For the profile log-likelihood, the observed information at beta_hat is
    # t(X) %*% X / sigma2. Use the unbiased estimator sigma2 = ssr/(n-p) so
    # that vcov() matches vcov(lm()) exactly.
    function(params) {
      eta <- as.vector(X %*% params)
      ssr <- sum((y - eta)^2)
      sigma2 <- ssr / (n - p)
      t(X) %*% X / sigma2
    }
  } else {
    function(params) {
      eta <- as.vector(X %*% params)
      mu <- family$linkinv(eta)
      mu_eta <- family$mu.eta(eta)

      # Variance function
      V <- family$variance(mu)

      # Weight matrix
      W <- diag(as.vector(mu_eta^2 / V))

      # Fisher information
      t(X) %*% W %*% X
    }
  }
}


# ============================================================================
# Internal: Create profile likelihood function
# ============================================================================

.make_profile_likelihood <- function(loglik_func, mle, param_names) {
  function(parameter, values) {
    # Validate parameter
    if (!parameter %in% param_names) {
      stop("Parameter '", parameter, "' not found in model", call. = FALSE)
    }

    param_idx <- which(param_names == parameter)
    n_params <- length(mle)
    other_params <- setdiff(seq_len(n_params), param_idx)

    # Profile over each value
    profile_results <- vapply(values, function(val) {
      # Optimize over other parameters
      opt <- optim(
        par = mle[other_params],
        fn = function(other) {
          full_params <- numeric(n_params)
          full_params[param_idx] <- val
          full_params[other_params] <- other
          -loglik_func(full_params)
        },
        method = "BFGS"
      )

      # Return profile log-likelihood
      -opt$value
    }, numeric(1))

    data.frame(
      parameter = parameter,
      value = values,
      loglik = profile_results,
      deviance = -2 * (profile_results - loglik_func(mle))
    )
  }
}


# ============================================================================
# Internal: Likelihood-based confidence intervals
# ============================================================================

.make_lik_confint <- function(loglik_func, profile_lik, mle, param_names) {
  function(parameter, level = 0.95) {
    if (!parameter %in% param_names) {
      stop("Parameter '", parameter, "' not found", call. = FALSE)
    }

    param_idx <- which(param_names == parameter)
    mle_val <- mle[param_idx]

    # Critical value from chi-square distribution (1 df for profile)
    chi2_crit <- qchisq(level, df = 1)

    # Function to find where deviance crosses critical value
    deviance_target <- function(val) {
      full_params <- mle
      full_params[param_idx] <- val

      # Optimize over other parameters
      other_params <- setdiff(seq_along(mle), param_idx)

      if (length(other_params) > 0) {
        opt <- optim(
          par = mle[other_params],
          fn = function(other) {
            full_params[other_params] <- other
            -loglik_func(full_params)
          },
          method = "BFGS"
        )
        profile_loglik <- -opt$value
      } else {
        profile_loglik <- loglik_func(full_params)
      }

      # Deviance difference
      dev <- -2 * (profile_loglik - loglik_func(mle))
      dev - chi2_crit
    }

    # Find lower limit
    lower <- tryCatch(
      {
        uniroot(
          f = deviance_target,
          interval = c(mle_val - 10 * abs(mle_val), mle_val),
          extendInt = "downX"
        )$root
      },
      error = function(e) {
        -Inf
      }
    )

    # Find upper limit
    upper <- tryCatch(
      {
        uniroot(
          f = deviance_target,
          interval = c(mle_val, mle_val + 10 * abs(mle_val)),
          extendInt = "upX"
        )$root
      },
      error = function(e) {
        Inf
      }
    )

    c(lower = lower, upper = upper)
  }
}


# ============================================================================
# S3 Methods
# ============================================================================

#' Print a likelihood function object
#'
#' @param x A \code{likelihood_function} object
#' @param ... Additional arguments (unused)
#'
#' @return Invisibly returns \code{x}
#'
#' @details Displays a summary of the likelihood function including family,
#'   link function, number of observations and parameters, convergence status,
#'   maximum likelihood estimates, and log-likelihood at the MLE.
#'
#' @export
print.likelihood_function <- function(x, ...) {
  cat("Likelihood Function Object\n")
  cat("==========================\n\n")
  cat(sprintf("Family: %s\n", x$family$family))
  cat(sprintf("Link: %s\n", x$family$link))
  cat(sprintf("Observations: %d\n", x$n))
  cat(sprintf("Parameters: %d\n", x$p))
  cat(sprintf("Converged: %s\n\n", x$converged))

  cat("Maximum Likelihood Estimates:\n")
  print(x$mle)
  cat("\n")

  cat(sprintf("Dispersion: %.4f\n", x$dispersion))
  cat(sprintf("Log-likelihood at MLE: %.4f\n", x$loglik(x$mle)))

  invisible(x)
}


#' Summarize a likelihood function object
#'
#' @param object A \code{likelihood_function} object
#' @param ... Additional arguments (unused)
#'
#' @return A \code{summary.likelihood_function} object containing coefficient
#'   estimates, standard errors, z-values, p-values, dispersion, log-likelihood,
#'   AIC, and BIC.
#'
#' @details Computes the variance-covariance matrix from the information matrix
#'   at the MLE and provides a summary table with estimates, standard errors,
#'   and Wald test statistics.
#'
#' @export
summary.likelihood_function <- function(object, ...) {
  # Compute standard errors from information matrix
  info_matrix <- object$information(object$mle)
  vcov_matrix <- tryCatch(
    solve(info_matrix),
    error = function(e) {
      warning("Could not invert information matrix", call. = FALSE)
      matrix(NA, nrow = object$p, ncol = object$p)
    }
  )

  se <- sqrt(diag(vcov_matrix))

  # Create summary table
  summary_table <- data.frame(
    Estimate = object$mle,
    Std.Error = se,
    z.value = object$mle / se,
    p.value = 2 * pnorm(-abs(object$mle / se))
  )

  rownames(summary_table) <- object$param_names

  structure(
    list(
      call = NULL,
      coefficients = summary_table,
      dispersion = object$dispersion,
      loglik = object$loglik(object$mle),
      aic = -2 * object$loglik(object$mle) + 2 * object$p,
      bic = -2 * object$loglik(object$mle) + log(object$n) * object$p,
      n = object$n,
      p = object$p,
      family = object$family
    ),
    class = "summary.likelihood_function"
  )
}


#' Print a likelihood function summary
#'
#' @param x A \code{summary.likelihood_function} object
#' @param digits Number of digits for printing numeric values
#' @param ... Additional arguments (unused)
#'
#' @return Invisibly returns \code{x}
#'
#' @details Prints the coefficient table, dispersion, log-likelihood, AIC, and BIC.
#'
#' @export
print.summary.likelihood_function <- function(x, digits = 4, ...) {
  cat("\nLikelihood Function Summary\n")
  cat("===========================\n\n")

  cat("Coefficients:\n")
  printCoefmat(x$coefficients, digits = digits)

  cat("\n")
  cat(sprintf("Dispersion: %.4f\n", x$dispersion))
  cat(sprintf("Log-likelihood: %.4f\n", x$loglik))
  cat(sprintf("AIC: %.2f\n", x$aic))
  cat(sprintf("BIC: %.2f\n", x$bic))

  invisible(x)
}


#' Extract coefficients from a likelihood function
#'
#' @param object A \code{likelihood_function} object
#' @param ... Additional arguments (unused)
#'
#' @return A named numeric vector of maximum likelihood estimates
#'
#' @details Returns the parameter estimates (MLE) from the likelihood function.
#'
#' @export
coef.likelihood_function <- function(object, ...) {
  object$mle
}


#' Extract variance-covariance matrix from a likelihood function
#'
#' @param object A \code{likelihood_function} object
#' @param ... Additional arguments (unused)
#'
#' @return A symmetric matrix of covariances between parameter estimates
#'
#' @details Computes the variance-covariance matrix as the inverse of the
#'   observed information matrix at the MLE. For Gaussian models, uses the
#'   unbiased estimator of variance (SSR/(n-p)) for compatibility with \code{\link{vcov.lm}}.
#'
#' @export
vcov.likelihood_function <- function(object, ...) {
  info_matrix <- object$information(object$mle)
  vcov_matrix <- solve(info_matrix)
  dimnames(vcov_matrix) <- list(object$param_names, object$param_names)
  vcov_matrix
}


#' Extract log-likelihood from a likelihood function
#'
#' @param object A \code{likelihood_function} object
#' @param ... Additional arguments (unused)
#'
#' @return An object of class \code{logLik} containing the log-likelihood at the MLE,
#'   with attributes \code{df} (number of parameters) and \code{nobs} (number of observations)
#'
#' @details Returns the log-likelihood evaluated at the maximum likelihood estimates.
#'   The value includes attributes that make it compatible with information criteria
#'   calculations (AIC, BIC, etc.).
#'
#' @export
logLik.likelihood_function <- function(object, ...) {
  val <- object$loglik(object$mle)
  attr(val, "df") <- object$p
  attr(val, "nobs") <- object$n
  class(val) <- "logLik"
  val
}


#' Confidence intervals for likelihood function parameters
#'
#' @param object A \code{likelihood_function} object
#' @param parm A character vector of parameter names for which to compute intervals.
#'   If \code{NULL}, intervals are computed for all parameters.
#' @param level Confidence level (default: 0.95)
#' @param ... Additional arguments (unused)
#'
#' @return A matrix with two columns containing the lower and upper confidence limits
#'   for each parameter. Column names indicate the confidence level.
#'
#' @details Uses profile likelihood methodology to construct confidence intervals.
#'   The intervals are obtained by finding where the profile log-likelihood drops
#'   by \eqn{\chi^2_{1,\alpha}/2} from its maximum.
#'
#' @export
confint.likelihood_function <- function(object, parm = NULL, level = 0.95, ...) {
  if (is.null(parm)) {
    parm <- object$param_names
  }

  ci_matrix <- t(vapply(parm, function(p) {
    object$confint(p, level = level)
  }, numeric(2)))

  colnames(ci_matrix) <- c(
    sprintf("%.1f %%", (1 - level) / 2 * 100),
    sprintf("%.1f %%", (1 + level) / 2 * 100)
  )

  ci_matrix
}


# ============================================================================
# Plotting functions
# ============================================================================

#' Plot likelihood function
#' @export
plot.likelihood_function <- function(x,
                                     parameter = NULL,
                                     type = c("likelihood", "deviance"),
                                     n_points = 100,
                                     interval = NULL,
                                     add_ci = TRUE,
                                     ci_level = 0.95,
                                     ...) {
  type <- match.arg(type)

  if (is.null(parameter)) {
    parameter <- x$param_names[1]
    message("Plotting first parameter: ", parameter)
  }

  param_idx <- which(x$param_names == parameter)
  mle_val <- x$mle[param_idx]

  # Determine plotting interval
  if (is.null(interval)) {
    se <- sqrt(diag(vcov(x)))[param_idx]
    interval <- c(mle_val - 4 * se, mle_val + 4 * se)
  }

  # Generate values
  param_values <- seq(interval[1], interval[2], length.out = n_points)

  # Compute profile likelihood
  profile_data <- x$profile(parameter, param_values)

  # Plot
  if (type == "likelihood") {
    # Relative likelihood
    rel_lik <- exp(profile_data$loglik - max(profile_data$loglik))

    plot(param_values, rel_lik,
      type = "l", lwd = 2,
      xlab = parameter,
      ylab = "Relative Likelihood",
      main = sprintf("Likelihood Function: %s", parameter),
      ...
    )

    abline(v = mle_val, col = "red", lty = 2)

    if (add_ci) {
      ci <- x$confint(parameter, level = ci_level)
      abline(v = ci, col = "blue", lty = 2)
      abline(h = exp(-qchisq(ci_level, 1) / 2), col = "gray", lty = 3)
    }
  } else {
    # Deviance
    plot(param_values, profile_data$deviance,
      type = "l", lwd = 2,
      xlab = parameter,
      ylab = "Deviance",
      main = sprintf("Profile Deviance: %s", parameter),
      ...
    )

    abline(v = mle_val, col = "red", lty = 2)
    abline(h = 0, col = "gray", lty = 1)

    if (add_ci) {
      ci <- x$confint(parameter, level = ci_level)
      abline(v = ci, col = "blue", lty = 2)
      abline(h = qchisq(ci_level, 1), col = "gray", lty = 3)
    }
  }

  invisible(profile_data)
}
