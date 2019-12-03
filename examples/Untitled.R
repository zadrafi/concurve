
library(concurve)
library(rstan)
library(rstanarm)

GroupA2<-rnorm(1000)
GroupB2<-rnorm(1000)

RandomData2<-data.frame(GroupA2, GroupB2)

model<-lm(GroupA2 ~ GroupB2, data=RandomData2)
model2 <- stan_glm(GroupA2 ~ GroupB2, data = RandomData2)

z<- bayes_R2(model2)
install.packages("tidybayes")

median(z)

summary(model)
summary(model2)
model2
library(tidybayes)
install.packages("broom")
library(broom)
tidymodel<- tidy(model2)

model_intercept <- tidymodel$estimate[1]
model_slope <- tidymodel$estimate[2]
draws <- spread_draws(model2, `(Intercept)`, GroupB2)

library(ggplot2)
ggplot() +
        geom_point(data = RandomData2, mapping = aes(x = GroupB2, y = GroupA2)) +
        geom_abline(data = draws, aes(intercept = `(Intercept)`, slope = GroupB2), color = "skyblue", alpha = 0.2, size = 0.2) +
        geom_abline(slope = model_slope, intercept = model_intercept)


ggplot(data = RandomData2, mapping = aes(x = GroupB2, y = GroupA2)) +
        geom_point() +
        geom_smooth(se = TRUE)

posterior_interval(model2)

system.time(randomframe<-genintervals(model, "GroupB2"))



par(mar = c(5, 5, 4, 5), font.main = 1, cex.main = 1.3, cex.lab = 1.15, las = 1)
with(randomframe,
     plot(lower.limit, pvalue,
          xlim = c(min(lower.limit), max(upper.limit)),
          panel.first = grid(nx = 10, ny = 0),
          type = "l",
          xlab = "Theta",
          ylab = "P-value",
          main = "P-value Function",
          yaxt = "n",
          las = 1))
with(randomframe, lines(upper.limit, pvalue))
polygon(c(max(randomframe$lower.limit), randomframe$lower.limit),
        c(min(randomframe$pvalue), randomframe$pvalue), col = "#239a9880", border = NA)
polygon(c(min(randomframe$upper.limit), randomframe$upper.limit),
        c(min(randomframe$pvalue), randomframe$pvalue), col = "#239a9880", border = NA)
axis(side = 2, at = c(seq(from = 0, to = 1, by = .10)), lty = 2, col = "grey", las = 1)
abline(v = 0, lty = 2, lwd = 1.2, col = "black")
par(new = T)
axis(side = 2, at = c(seq(from = 0, to = 1, by = .10)),
     tck = -0.025, lty = 2, col = "grey", labels = NA)
par(new = T)
with(randomframe,
     plot(1, type = "n",
          ylim = rev(range(intrvl.level * 100)),
          axes = F,
          xlab = NA,
          ylab = NA))
axis(side = 4, at = c(seq(from = 0, to = 100, by = 10)),
     tck = 1, lty = 2, col = "grey", las = 1)
text(par("usr") + 0.95, 28, srt=-90, adj = 0, labels = "Consonance Level (%)", cex = 1.05,
     xpd = TRUE)
par(new = T)
axis(side = 4, at = c(seq(from = 0, to = 100, by = 10)),
     tck = -0.025, lty = 2, col = "grey", labels = NA)
text(x = 1.25, y = 10, paste("MLE:",
                            round(((max(randomframe$lower.limit) + min(randomframe$upper.limit)) / 2), 3)),
     cex = 0.8, col = "black")
text(x = 1.25, y = 15, paste("50% CI:",
                             round(unname((quantile(randomframe$lower.limit, prob = 0.50))), 3), "-",
                             round(unname((quantile(randomframe$upper.limit, prob = 0.50))), 3),  sep=" ", collapse=", "),
     cex = 0.8, col = "black")
text(x = 1.25, y = 20, paste("75% CI:",
                             round(unname((quantile(randomframe$lower.limit, prob = 0.25))), 3), "-",
                             round(unname((quantile(randomframe$upper.limit, prob = 0.75))), 3),  sep=" ", collapse=", "),
     cex = 0.8, col = "black")
text(x = 1.25, y = 25, paste("95% CI:",
                             round(unname((quantile(randomframe$lower.limit, prob = 0.05))), 3), "-",
                             round(unname((quantile(randomframe$upper.limit, prob = 0.95))), 3),  sep=" ", collapse=", "),
     cex = 0.8, col = "black")
text(x = 1.25, y = 30, paste("99% CI:",
                             round(unname((quantile(randomframe$lower.limit, prob = 0.01))), 3), "-",
                             round(unname((quantile(randomframe$upper.limit, prob = 0.99))), 3),  sep=" ", collapse=", "),
     cex = 0.8, col = "black")





par(mar = c(5, 5, 4, 5), font.main = 1, cex.main = 1.3, cex.lab = 1.15, las = 1)
with(randomframe,
     plot(lower.limit, svalue,
          xlim = c(min(lower.limit), max(upper.limit)),
          panel.first = grid(),
          type = "l",
          xlab = "Theta",
          ylab = "S-value",
          main = "Surpisal Function",
          yaxt = "n",
          las = 1))
with(randomframe, lines(upper.limit, svalue))
polygon(c(max(randomframe$lower.limit), randomframe$lower.limit),
        c(max(randomframe$svalue), randomframe$svalue), col = "#239a9880", border = NA)
polygon(c(min(randomframe$upper.limit), randomframe$upper.limit),
        c(max(randomframe$svalue), randomframe$svalue), col = "#239a9880", border = NA)
axis(side = 2, lty = 2, col = "grey", las = 1)
par(new = T)
axis(side = 2, tck = -0.025, lty = 2, col = "grey", labels = NA)
par(new = T)
with(randomframe,
     plot(1, type = "n",
          ylim = rev(range(intrvl.level * 100)),
          axes = F,
          xlab = NA,
          ylab = NA))
axis(side = 4, at = c(seq(from = 0, to = 100, by = 10)),
     tck = 1, lty = 2, col = "grey", las = 1)
text(par("usr") + 0.95, 28, srt=-90, adj = 0, labels = "Consonance Level (%)", cex = 1.05,
     xpd = TRUE)
par(new = T)
axis(side = 4, at = c(seq(from = 0, to = 100, by = 10)),
     tck = -0.025, lty = 2, col = "grey", labels = NA)
text(x = 1.25, y = 10, paste("MLE:",
                             round(((max(randomframe$lower.limit) + min(randomframe$upper.limit)) / 2), 3)),
     cex = 0.8, col = "black")
text(x = 1.25, y = 15, paste("50% CI:",
                             round(unname((quantile(randomframe$lower.limit, prob = 0.50))), 3), "-",
                             round(unname((quantile(randomframe$upper.limit, prob = 0.50))), 3),  sep=" ", collapse=", "),
     cex = 0.8, col = "black")
text(x = 1.25, y = 20, paste("75% CI:",
                             round(unname((quantile(randomframe$lower.limit, prob = 0.25))), 3), "-",
                             round(unname((quantile(randomframe$upper.limit, prob = 0.75))), 3),  sep=" ", collapse=", "),
     cex = 0.8, col = "black")
text(x = 1.25, y = 25, paste("95% CI:",
                             round(unname((quantile(randomframe$lower.limit, prob = 0.05))), 3), "-",
                             round(unname((quantile(randomframe$upper.limit, prob = 0.95))), 3),  sep=" ", collapse=", "),
     cex = 0.8, col = "black")
text(x = 1.25, y = 30, paste("99% CI:",
                             round(unname((quantile(randomframe$lower.limit, prob = 0.01))), 3), "-",
                             round(unname((quantile(randomframe$upper.limit, prob = 0.99))), 3),  sep=" ", collapse=", "),
     cex = 0.8, col = "black")





ggconcurve("consonance", randomframe, position = "pyramid", nullvalue = "present")
plot.concurve(type = "consonance", data = randomframe)

library(microbenchmark)
library(ggplot2)
library(concurve)

microbenchmark(
        base_R = plot_concurve(randomframe),
        ggplot_R = gg_consonance(randomframe)
)

library(benchplot)

benchplot(gg_consonance(randomframe))


install.packages("benchplot")

gg_surprisal(randomframe)






library(ProfileLikelihood)

profilelike.lm

dffer<- curve_lik(formula = GroupA2 ~ 1, data = RandomData2,
             profile.theta= "GroupB2", lo.theta = -.3, hi.theta = .3, length = 500)

(plotlikely <- gglikely(dffer, x = dffer$theta, y = dffer$log.norm.lik))

library(benchmarkme)
ram <- get_ram()
ram
cpu <- get_cpu()
cpu

# Run the io benchmark
res <- benchmark_io(runs = 1, size = 5)

# Plot the results
plot(res)
