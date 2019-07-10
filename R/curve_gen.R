# General Consonance Functions Using Profile Likelihood, Wald, or the bootstrap method for linear models.

curve_gen <- function(model, var, method = "default", replicates = 1000, steps = 10000) {
  if (is.list(model) != TRUE) {
    stop("Error: 'model' must be an object with a statistical model")
  }
  if (is.character(method) != TRUE) {
    stop("Error: 'method' must be a character vector")
  }
  if (is.numeric(replicates) != TRUE) {
    stop("Error: 'replicates' must be a numeric vector")
  }
  if (is.numeric(steps) != TRUE) {
    stop("Error: 'steps' must be a numeric vector")
  }
  intrvls <- (0:steps) / steps
  if (method == "default") {
    results <- mclapply(intrvls, FUN = function(i) confint(object = model, level = i)[var, ])
  } else if (method == "Wald") {
    results <- mclapply(intrvls, FUN = function(i) confint.default(object = model, level = i)[var, ])
  } else if (method == "lm") {
    results <- mclapply(intrvls, FUN = function(i) confint.lm(object = model, level = i)[var, ])
  } else if (method == "boot") {
    effect <- coef(model)[[var]]
    boot_dist <- replicate(replicates,
      expr = coef(lm(model$call$formula,
        data = model$model[sample(nrow(model$model), replace = T), ]
      ))[[var]]
    ) - effect
    results <- mclapply(intrvls, FUN = function(i) effect - quantile(boot_dist, probs = (1 + c(i, -i)) / 2))
  }

  df <- data.frame(do.call(rbind, results))
  intrvl.limit <- c("lower.limit", "upper.limit")
  colnames(df) <- intrvl.limit
  df$intrvl.level <- intrvls
  df$pvalue <- 1 - intrvls
  df$svalue <- -log2(df$pvalue)
  df <- head(df, -1)
  return(df)
}

# RMD Check
utils::globalVariables(c("df", "lower.limit", "upper.limit", "intrvl.level", "pvalue", "svalue"))
