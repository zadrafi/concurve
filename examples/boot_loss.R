
boot_loss <- function(pred, obs, type = "rmse", reps, m, cores, cl = NULL) {

  pred <- as.matrix(pred)
  obs <- as.matrix(obs)

  matrix_loss <- cbind(pred, obs)

  n <- ((length(matrix_loss)) / (2))

  if (type == "rmse") {
    for (i in 1:n) {
      loss_looped <- sqrt(mean(((matrix_loss[i, 1] - matrix_loss[i, 2])^2)))
      (looped_loss <- round(loss_looped, 2))
    }

    theta <- function(matrix_loss, i) {
      sqrt(mean(((matrix_loss[i, 1] - matrix_loss[i, 2])^2)))
    }
  } else if (type == "mae") {
    for (i in 1:n) {
      loss_looped <- (mean((abs(matrix_loss[i, 1] - matrix_loss[i, 2]))))
      (looped_loss <- round(loss_looped, 2))
    }


    theta <- function(matrix_loss, i) {
      (mean((abs(matrix_loss[i, 1] - matrix_loss[i, 2]))))
    }
  }

  loss_bootstrap <- boot(
    data = matrix_loss, statistic = theta, R = reps, stype = "i",
    sim = "ordinary", parallel = "multicore", ncpus = cores, cl = NULL
  )

  loss_se <- sd(loss_bootstrap[["t"]])

  loss_ci <- boot.ci(loss_bootstrap,
    conf = c(0.90, 0.95),
    type = c("norm", "basic", "perc", "bca")
  )

  boot_loss_output <- list(looped_loss, loss_bootstrap, loss_se, loss_ci)
  names(boot_loss_output) <- c(
    "Looped Iteration Loss", "Bootstrapped Loss",
    "Loss SE", "Loss CI"
  )
  class(boot_loss_output) <- "concurve"
  return(boot_loss_output)
}
