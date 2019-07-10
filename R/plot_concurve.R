plot_concurve <- function(type = "consonance",
                          data,
                          title = "Consonance Function",
                          xlab = "Theta",
                          ylab1 = "P-value",
                          ylab2 = "Confidence Level (%)") {
  par(
    mar = c(4.9, 5, 3.5, 5),
    font.main = 1,
    cex.main = 1.3,
    cex.lab = 1.15
  )

  if (type == "consonance") {
    with(
      data = data,
      plot(lower.limit, pvalue,
        xlim = c(min(lower.limit), max(upper.limit)),
        panel.first = grid(ny = 0),
        type = "l",
        xlab = xlab,
        ylab = ylab1,
        main = title,
        yaxt = "n",
        las = 1
      )
    )
    with(data = data, lines(upper.limit, pvalue))

    # Shade in area under the curve

    polygon(c(max(data$lower.limit), data$lower.limit),
      c(min(data$pvalue), data$pvalue),
      col = "#239a9880", border = NA
    )
    polygon(c(min(data$upper.limit), data$upper.limit),
      c(min(data$pvalue), data$pvalue),
      col = "#239a9880", border = NA
    )
    axis(
      side = 2, at = c(seq(from = 0, to = 1, by = .10)),
      lty = 2, col = "grey", las = 1
    )
    par(new = T)
    axis(
      side = 2, at = c(seq(from = 0, to = 1, by = .10)),
      tck = -0.030, lty = 2, col = "grey", labels = NA
    )
    par(new = T)

    # Create second y-axis for confidence/consonance levels

    with(
      data,
      plot(1,
        type = "n",
        ylim = rev(range(intrvl.level * 100)),
        axes = F,
        xlab = NA,
        ylab = NA
      )
    )
    axis(
      side = 4, at = c(seq(from = 0, to = 100, by = 10)),
      tck = 1, lty = 2, col = "grey", las = 1
    )
    text(par("usr") + 0.95, 28,
      srt = -90, adj = 0, labels = "Consonance Level (%)", cex = 1.05,
      xpd = TRUE
    )
    par(new = T)
    axis(
      side = 4, at = c(seq(from = 0, to = 100, by = 10)),
      tck = -0.030, lty = 2, col = "grey", labels = NA
    )

    # Labels for interval estimates and maximum likelihood

    text(
      x = 1.27, y = 3,
      paste(
        "Point Estimate:",
        round(((max(data$lower.limit) + min(data$upper.limit)) / 2), 3)
      ),
      cex = 0.93, col = "black"
    )
    text(
      x = 1.27, y = 10,
      paste("50% CI:",
        round(unname((quantile(data$lower.limit, prob = 0.50))), 3), "-",
        round(unname((quantile(data$upper.limit, prob = 0.50))), 3),
        sep = " ", collapse = ", "
      ),
      cex = 0.93, col = "black"
    )
    text(
      x = 1.27, y = 17,
      paste("75% CI:",
        round(unname((quantile(data$lower.limit, prob = 0.25))), 3), "-",
        round(unname((quantile(data$upper.limit, prob = 0.75))), 3),
        sep = " ", collapse = ", "
      ),
      cex = 0.93, col = "black"
    )
    text(
      x = 1.27, y = 24,
      paste("95% CI:",
        round(unname((quantile(data$lower.limit, prob = 0.05))), 3), "-",
        round(unname((quantile(data$upper.limit, prob = 0.95))), 3),
        sep = " ", collapse = ", "
      ),
      cex = 0.93, col = "black"
    )
    text(
      x = 1.27, y = 31,
      paste("99% CI:",
        round(unname((quantile(data$lower.limit, prob = 0.01))), 3), "-",
        round(unname((quantile(data$upper.limit, prob = 0.99))), 3),
        sep = " ", collapse = ", "
      ),
      cex = 0.93, col = "black"
    )
  } else if (type == "surprisal") {
    with(
      data,
      plot(lower.limit, svalue,
        xlim = c(min(lower.limit), max(upper.limit)),
        panel.first = grid(),
        type = "l",
        xlab = "Theta",
        ylab = "S-value",
        main = "Surpisal Function",
        yaxt = "n",
        las = 1
      )
    )
    with(data, lines(upper.limit, svalue))
    polygon(c(max(data$lower.limit), data$lower.limit),
      c(max(data$svalue), data$svalue),
      col = "#239a9880", border = NA
    )
    polygon(c(min(data$upper.limit), data$upper.limit),
      c(max(data$svalue), data$svalue),
      col = "#239a9880", border = NA
    )
    axis(side = 2, lty = 2, col = "grey", las = 1)
    par(new = T)
    axis(side = 2, tck = -0.025, lty = 2, col = "grey", labels = NA)
    par(new = T)
  }
}


# RMD Check
utils::globalVariables(c("df", "lower.limit", "upper.limit", "intrvl.level", "pvalue", "svalue"))
