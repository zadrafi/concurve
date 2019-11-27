curve_sampling <- function(x, B, func, ..., m = nrow(x), mr = 5, K = 2, J = 10,
                           alpha, verbose = TRUE) {
  call <- match.call()
  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    stats::runif(1)
  }
  seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (is.vector(x)) {
    x <- as.matrix(x)
  }
  n <- nrow(x)
  nc <- ncol(x)
  ttind <- ifelse(length(B) > 1, 1, 0)
  if (ttind == 1) {
    tt <- B
    B <- length(tt)
  }
  else {
    tt <- rep(0, B)
  }
  t0 <- func(x, ...)
  u <- numeric(length = m)
  m1 <- sqrt(m * (m - 1))
  if (m == n) {
    for (i in seq_len(n)) {
      u[i] <- func(x[-i, ], ...)
    }
    t. <- (mean(u) - u) * (m - 1)
    a <- (1 / 6) * sum(t.^3) / (sum(t.^2))^1.5
    sdjack <- sqrt(sum(t.^2)) / m1
  }
  if (m < n) {
    aa <- ssj <- numeric(mr)
    r <- n %% m
    seq_len_m <- seq_len(m)
    for (k in seq_len(mr)) {
      Imat <- sapply(seq_len_m, sample.int, n = n, size = n -
        r)
      Iout <- setdiff(seq_len(n), Imat)
      for (j in seq_len_m) {
        Ij <- setdiff(seq_len_m, j)
        ij <- c(c(Imat[Ij, ], Iout))
        u[j] <- func(x[ij, ])
      }
      t. <- (mean(u) - u) * (m - 1)
      aa[k] <- (1 / 6) * sum(t.^3) / (sum(t.^2))^1.5
      ssj[k] <- sqrt(sum(t.^2)) / m1
    }
    a <- mean(aa)
    sdjack <- mean(ssj)
  }
  if (ttind == 0) {
    tY. <- Y. <- rep(0, n)
    if (verbose) {
      pb <- utils::txtProgressBar(min = 0, max = B, style = 3)
    }
    for (j in seq_len(B)) {
      ij <- sample(x = n, size = n, replace = TRUE)
      Yj <- table(c(ij, 1:n)) - 1
      tt[j] <- func(x[ij, ], ...)
      tY. <- tY. + tt[j] * Yj
      Y. <- Y. + Yj
      if (verbose) {
        utils::setTxtProgressBar(pb, j)
      }
    }
    if (verbose) {
      close(pb)
    }
    tt. <- mean(tt)
    tY. <- tY. / B
    Y. <- Y. / B
    s. <- n * (tY. - tt. * Y.)
    u. <- 2 * t. - s.
    sdu <- sqrt(sum(u.^2)) / n
    ustat <- 2 * t0 - tt.
    ustats <- c(ustat, sdu)
    names(ustats) <- c("ustat", "sdu")
  }
  B.mean <- c(B, mean(tt))
  alpha <- alpha[alpha < 0.5]
  alpha <- c(alpha, 0.5, rev(1 - alpha))
  zalpha <- stats::qnorm(alpha)
  nal <- length(alpha)
  sdboot0 <- stats::sd(tt)
  z00 <- stats::qnorm(sum(tt < t0) / B)
  iles <- stats::pnorm(z00 + (z00 + zalpha) / (1 - a * (z00 +
    zalpha)))
  ooo <- trunc(iles * B)
  ooo <- pmin(pmax(ooo, 1), B)
  lims0 <- sort(tt)[ooo]
  standard <- t0 + sdboot0 * stats::qnorm(alpha)
  lims0 <- cbind(lims0, standard)
  dimnames(lims0) <- list(alpha, c("bca", "std"))
  stats0 <- c(t0, sdboot0, z00, a, sdjack)
  names(stats0) <- c("theta", "sdboot", "z0", "a", "sdjack")
  vl0 <- list(
    lims = lims0, stats = stats0, B.mean = B.mean,
    call = call, seed = seed
  )
  if (K == 0) {
    bcaboot.return(vl0)
  }
  pct <- rep(0, nal)
  for (i in 1:nal) pct[i] <- sum(tt <= lims0[i, 1]) / B
  Stand <- vl0$stats[1] + vl0$stats[2] * stats::qnorm(alpha)
  Limsd <- matrix(0, length(alpha), K)
  Statsd <- matrix(0, 5, K)
  for (k in 1:K) {
    II <- sample(x = B, size = B)
    II <- matrix(II, ncol = J)
    lims <- matrix(0, length(alpha), J)
    stats <- matrix(0, 5, J)
    for (j in 1:J) {
      iij <- c(II[, -j])
      ttj <- tt[iij]
      Bj <- length(ttj)
      sdboot <- stats::sd(ttj)
      z0 <- stats::qnorm(sum(ttj < t0) / Bj)
      iles <- stats::pnorm(z0 + (z0 + zalpha) / (1 - a *
        (z0 + zalpha)))
      oo <- trunc(iles * Bj)
      oo <- pmin(pmax(oo, 1), Bj)
      li <- sort(ttj)[oo]
      standard <- t0 + sdboot * stats::qnorm(alpha)
      sta <- c(t0, sdboot, z0, a, sdjack)
      names(sta) <- c("theta", "sdboot", "z0", "a", "sdjack")
      lims[, j] <- li
      stats[, j] <- sta
    }
    Limsd[, k] <- apply(lims, 1, sd) * (J - 1) / sqrt(J)
    Statsd[, k] <- apply(stats, 1, sd) * (J - 1) / sqrt(J)
  }
  limsd <- rowMeans(Limsd, 1)
  statsd <- rowMeans(Statsd, 1)
  limits <- cbind(vl0$lims[, 1], limsd, vl0$lims[, 2], pct)
  dimnames(limits) <- list(alpha, c(
    "bca", "jacksd", "std",
    "pct"
  ))
  stats <- rbind(stats0, statsd)
  dimnames(stats) <- list(c("est", "jsd"), c(
    "theta", "sdboot",
    "z0", "a", "sdjack"
  ))
  vl <- list(
    call = call, lims = limits, stats = stats, B.mean = B.mean,
    seed = seed
  )
  if (ttind == 0) {
    vl$ustats <- ustats
  }
  bcaboot.return(vl)
}

bcaboot.return <- function(x) {
  class(x) <- "bcaboot"
  x
}

#' @export
print.bcaboot <- function(x, digits = getOption("digits"), ...) {
  result <- x
  result$seed <- NULL
  print.default(result)
  invisible(x)
}
