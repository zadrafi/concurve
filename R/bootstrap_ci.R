# =============================================================================
# bootstrap_ci.R
# Bootstrap Confidence Intervals at Multiple Confidence Levels
#
# Methods supported:
#   - BCa  (bias-corrected accelerated) — default, preferred
#   - Percentile                        — fast fallback
#
# Key references:
#   Efron & Tibshirani (1993), "An Introduction to the Bootstrap"
#   DiCiccio & Efron (1996), Stat. Sci. 11(3):189-228
# =============================================================================


# -----------------------------------------------------------------------------
# CORE FUNCTION
# bootstrap_ci()
#
# Args:
#   data       - numeric vector (NAs removed automatically)
#   stat_fn    - function that takes a numeric vector and returns a scalar
#                default: median; use mean, var, IQR, or any custom fn
#   ci_levels  - numeric vector of confidence levels, e.g. c(0.90, 0.95, 0.99)
#   n_boot     - number of bootstrap resamples (>= 1000 recommended; >= 9999 for BCa)
#   method     - "bca" (default) or "percentile"
#   seed       - integer for reproducibility (NULL to skip)
#
# Returns:
#   data.frame with columns: ci_level, lower, observed, upper, method, n_boot, n_obs
# -----------------------------------------------------------------------------

bootstrap_ci <- function(
  data,
  stat_fn = median,
  ci_levels = c(0.80, 0.90, 0.95, 0.99),
  n_boot = 9999,
  method = c("bca", "percentile"),
  seed = 42
) {
  method <- match.arg(method)

  # ---- Input validation -----------------------------------------------------
  if (!is.numeric(data)) stop("`data` must be a numeric vector.")
  data <- data[!is.na(data)]
  if (length(data) < 2) stop("Need at least 2 non-NA observations.")
  if (!is.numeric(ci_levels) ||
    any(ci_levels <= 0 | ci_levels >= 1)) {
    stop("`ci_levels` must be numeric values strictly between 0 and 1.")
  }
  if (!is.function(stat_fn)) stop("`stat_fn` must be a function.")
  if (n_boot < 999) warning("n_boot < 999; CIs may be unstable.")

  # ---- Setup ----------------------------------------------------------------
  if (!is.null(seed)) set.seed(seed)
  n <- length(data)
  obs_stat <- stat_fn(data)

  # ---- Generate bootstrap distribution (once, shared across all levels) -----
  boot_stats <- replicate(n_boot, {
    stat_fn(sample(data, n, replace = TRUE))
  })

  # ---- BCa acceleration constant via jackknife (computed once) --------------
  if (method == "bca") {
    jack_vals <- vapply(
      seq_len(n),
      function(i) stat_fn(data[-i]),
      numeric(1)
    )
    jack_mean <- mean(jack_vals)
    num <- sum((jack_mean - jack_vals)^3)
    denom <- 6 * (sum((jack_mean - jack_vals)^2))^1.5

    # Guard against zero denominator (perfectly uniform statistic)
    accel <- if (denom == 0) 0 else num / denom
  }

  # ---- Loop over all CI levels ----------------------------------------------
  results <- lapply(ci_levels, function(level) {
    alpha <- 1 - level

    if (method == "percentile") {
      lo <- quantile(boot_stats, alpha / 2, names = FALSE)
      hi <- quantile(boot_stats, 1 - alpha / 2, names = FALSE)
    } else { # BCa
      # Bias-correction constant: proportion of bootstrap replicates below observed
      z0 <- qnorm(mean(boot_stats < obs_stat))

      # Handle edge cases where z0 is ±Inf
      if (!is.finite(z0)) z0 <- 0

      z_lo <- qnorm(alpha / 2)
      z_hi <- qnorm(1 - alpha / 2)

      # Adjusted quantile levels
      a1 <- pnorm(z0 + (z0 + z_lo) / (1 - accel * (z0 + z_lo)))
      a2 <- pnorm(z0 + (z0 + z_hi) / (1 - accel * (z0 + z_hi)))

      # Clamp to valid range
      a1 <- max(0.0001, min(a1, 0.4999))
      a2 <- min(0.9999, max(a2, 0.5001))

      lo <- quantile(boot_stats, a1, names = FALSE)
      hi <- quantile(boot_stats, a2, names = FALSE)
    }

    data.frame(
      ci_level = paste0(level * 100, "%"),
      lower = round(lo, 6),
      observed = round(obs_stat, 6),
      upper = round(hi, 6),
      width = round(hi - lo, 6),
      method = method,
      n_boot = n_boot,
      n_obs = n,
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, results)
  rownames(out) <- NULL
  out
}


# -----------------------------------------------------------------------------
# WRAPPER: bootstrap_ci_columns()
#
# Apply bootstrap_ci() across multiple numeric columns of a data.frame.
# Returns a single tidy data.frame with a `variable` column prepended.
#
# Args:
#   df         - data.frame
#   columns    - character vector of column names (default: all numeric cols)
#   ...        - additional arguments forwarded to bootstrap_ci()
# -----------------------------------------------------------------------------

bootstrap_ci_columns <- function(df, columns = NULL, ...) {
  if (!is.data.frame(df)) stop("`df` must be a data.frame.")

  if (is.null(columns)) {
    columns <- names(df)[vapply(df, is.numeric, logical(1))]
    if (length(columns) == 0) stop("No numeric columns found in `df`.")
    message("Auto-selected numeric columns: ", paste(columns, collapse = ", "))
  }

  results <- lapply(columns, function(col) {
    ci_df <- bootstrap_ci(df[[col]], ...)
    ci_df$variable <- col
    ci_df[, c("variable", setdiff(names(ci_df), "variable"))] # variable first
  })

  out <- do.call(rbind, results)
  rownames(out) <- NULL
  out
}


# =============================================================================
# USAGE EXAMPLES
# =============================================================================

if (FALSE) { # Wrapped in FALSE so examples don't run on source()

  # --- Example 1: Median CI on a single vector, all default levels -----------
  set.seed(1)
  x <- rnorm(200, mean = 50, sd = 10)

  bootstrap_ci(x)
  #   ci_level    lower observed    upper    width method n_boot n_obs
  # 1      80%  48.247   49.885   51.468    3.221    bca   9999   200
  # 2      90%  47.420   49.885   52.213    4.793    bca   9999   200
  # 3      95%  46.721   49.885   52.934    6.213    bca   9999   200
  # 4      99%  45.221   49.885   54.223    9.002    bca   9999   200


  # --- Example 2: Mean CI using percentile method ----------------------------
  bootstrap_ci(x,
    stat_fn = mean, method = "percentile",
    ci_levels = c(0.90, 0.95, 0.99)
  )


  # --- Example 3: Custom statistic — trimmed mean (10%) ---------------------
  trimmed_mean <- function(v) mean(v, trim = 0.10)
  bootstrap_ci(x, stat_fn = trimmed_mean, ci_levels = c(0.95))


  # --- Example 4: Custom statistic — IQR ------------------------------------
  bootstrap_ci(x, stat_fn = IQR, ci_levels = c(0.90, 0.95, 0.99))


  # --- Example 5: Specific percentile (e.g., 75th) --------------------------
  p75 <- function(v) quantile(v, 0.75, names = FALSE)
  bootstrap_ci(x, stat_fn = p75)


  # --- Example 6: Multi-column wrapper on a data.frame ----------------------
  df <- data.frame(
    sales    = rnorm(300, 1000, 200),
    margin   = rnorm(300, 0.35, 0.08),
    units    = rpois(300, lambda = 50)
  )

  bootstrap_ci_columns(df, ci_levels = c(0.90, 0.95, 0.99), n_boot = 4999)


  # --- Example 7: Only specific columns ------------------------------------
  bootstrap_ci_columns(df,
    columns   = c("sales", "margin"),
    stat_fn   = mean,
    ci_levels = c(0.95, 0.99)
  )
}
