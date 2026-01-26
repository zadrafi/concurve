## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  message = TRUE,
  warning = TRUE,
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
sim <- function() {
  fake <- data.frame((x <- rnorm(100, 100, 20)), (y <- rnorm(100, 80, 20)))
  intervals <- t.test(x = x, y = y, data = fake, conf.level = .95)$conf.int[]
}

set.seed(1031)

z <- replicate(100, sim(), simplify = FALSE)

df <- data.frame(do.call(rbind, z))
df$studynumber <- (1:length(z))
intrvl.limit <- c("lower.limit", "upper.limit", "studynumber")
colnames(df) <- intrvl.limit
df$point <- ((df$lower.limit + df$upper.limit) / 2)
df$covered <- (df$lower.limit <= 20 & 20 <= df$upper.limit)
df$coverageprob <- ((as.numeric(table(df$covered)[2]) / nrow(df) * 100))

library(ggplot2)


ggplot(data = df, aes(x = studynumber, y = point, ymin = lower.limit, ymax = upper.limit)) +
  geom_pointrange(mapping = aes(color = covered), size = .40) +
  geom_hline(yintercept = 20, lty = 1, color = "red", alpha = 0.5) +
  coord_flip() +
  labs(
    title = "Simulated 95% Intervals",
    x = "Study Number",
    y = "Estimate",
    subtitle = "Population Parameter is 20"
  ) +
  theme_bw() + # use a white background
  theme(legend.position = "none") +
  annotate(
    geom = "text", x = 102, y = 30,
    label = "Coverage (%) =", size = 2.5, color = "black"
  ) +
  annotate(
    geom = "text", x = 102, y = 35,
    label = df$coverageprob, size = 2.5, color = "black"
  )

## ----echo=TRUE----------------------------------------------------------------
library(concurve)
library(rstan)
library(rstanarm)
library(ggplot2)
library(cowplot)
library(bayesplot)
library(scales)

## ----echo=TRUE----------------------------------------------------------------
GroupA <- rnorm(50)
GroupB <- rnorm(50)
RandomData <- data.frame(GroupA, GroupB)
model_freq <- lm(GroupA ~ GroupB, data = RandomData)

## ----results = 'hide', message = FALSE----------------------------------------
rstan_options(auto_write = TRUE)

# Using flat prior
model_bayes <- stan_lm(GroupA ~ GroupB,
  data = RandomData, prior = NULL,
  iter = 5000, warmup = 1000, chains = 4
)

## ----echo=TRUE----------------------------------------------------------------
randomframe <- curve_gen(model_freq, "GroupB", steps = 10000)

(function1 <- ggcurve(type = "c", randomframe[[1]], nullvalue = TRUE))

color_scheme_set("teal")

function2 <- mcmc_dens(model_bayes, pars = "GroupB") +
  ggtitle("Posterior Distribution") +
  labs(subtitle = "Function Displays the Full Posterior Distribution", x = "Range of Values", y = "Posterior Probability") +
  scale_y_continuous(breaks = c(0, 0.30, 0.60, 0.90, 1.20, 1.50, 1.80, 2.10, 2.40, 2.70, 3.0))


(breaks1 <- c(0, 0.30, 0.60, 0.90, 1.20, 1.50, 1.80, 2.10, 2.40, 2.70, 3.0))

(adjustment <- function(x) {
  x / 3
})

(labels <- adjustment(breaks1))

breaks <- labels
labels1 <- labels

(function3 <- mcmc_dens(model_bayes, pars = "GroupB") +
  ggtitle("Posterior Distribution") +
  labs(subtitle = "Function Displays the Full Posterior Distribution", x = "Range of Values", y = "Posterior Probability") +
  scale_x_continuous(expand = c(0, 0), breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(expand = c(0, 0), breaks = waiver(), labels = waiver(), n.breaks = 10, limits = c(0, 3.25)) +
  yaxis_text(on = TRUE) +
  yaxis_ticks(on = TRUE) +
  annotate("segment",
    x = 0, xend = 0, y = 0, yend = 3,
    color = "#990000", alpha = 0.4, size = .75, linetype = 3
  ))

## ----echo=TRUE, fig.height=9, fig.width=7-------------------------------------
plot_grid(function1, function3, ncol = 1, align = "v")

## ----echo=TRUE----------------------------------------------------------------

GroupA <- rnorm(500, mean = 2)
GroupB <- rnorm(500, mean = 1)
RandomData <- data.frame(GroupA, GroupB)
model_freq <- lm(GroupA ~ GroupB, data = RandomData)

## ----results = 'hide', message = FALSE----------------------------------------

# Using flat prior
model_bayes <- stan_lm(GroupA ~ GroupB,
  data = RandomData, prior = NULL,
  iter = 5000, warmup = 1000, chains = 4
)

## ----echo=TRUE----------------------------------------------------------------

randomframe <- curve_gen(model_freq, "GroupB", steps = 10000)

(function1 <- ggcurve(type = "c", randomframe[[1]], nullvalue = TRUE))

color_scheme_set("teal")

function2 <- mcmc_dens(model_bayes, pars = "GroupB") +
  ggtitle("Posterior Distribution") +
  labs(subtitle = "Function Displays the Full Posterior Distribution", x = "Range of Values", y = "Posterior Probability") +
  scale_y_continuous(breaks = c(0, 0.30, 0.60, 0.90, 1.20, 1.50, 1.80, 2.10, 2.40, 2.70, 3.0))


(breaks1 <- c(0, 0.30, 0.60, 0.90, 1.20, 1.50, 1.80, 2.10, 2.40, 2.70, 3.0))

(adjustment <- function(x) {
  x / 3
})

(labels <- adjustment(breaks1))

breaks <- labels
labels1 <- labels

(function3 <- mcmc_dens(model_bayes, pars = "GroupB") +
  ggtitle("Posterior Distribution") +
  labs(subtitle = "Function Displays the Full Posterior Distribution", x = "Range of Values", y = "Posterior Probability") +
  scale_x_continuous(expand = c(0, 0), breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(expand = c(0, 0), breaks = waiver(), labels = waiver(), n.breaks = 10, limits = c(0, 9)) +
  yaxis_text(on = TRUE) +
  yaxis_ticks(on = TRUE) +
  annotate("segment",
    x = 0, xend = 0, y = 0, yend = 9,
    color = "#990000", alpha = 0.4, size = .75, linetype = 3
  ))

## ----echo=TRUE, fig.height=9, fig.width=7-------------------------------------
plot_grid(function1, function3, ncol = 1, align = "v")

## ----results = 'hide', message = FALSE----------------------------------------

data(kidiq)

# flat prior

post1 <- stan_lm(kid_score ~ mom_hs,
  data = kidiq, prior = NULL,
  seed = 12345
)

## -----------------------------------------------------------------------------
post2 <- lm(kid_score ~ mom_hs, data = kidiq)

df3 <- curve_gen(post2, "mom_hs")

(function99 <- ggcurve(df3[[1]]))

summary(post1)

color_scheme_set("teal")

(function101 <- mcmc_areas(post1, pars = "mom_hs", point_est = "none", prob = 1, prob_outer = 1, area_method = "equal height") +
  ggtitle("Posterior Distribution") +
  labs(subtitle = "Function Displays the Full Posterior Distribution", x = "Range of Values", y = "Posterior Probability") +
  yaxis_text(on = TRUE) +
  yaxis_ticks(on = TRUE))

## ----echo=TRUE, fig.height=9, fig.width=7-------------------------------------
cowplot::plot_grid(function99, function101, ncol = 1, align = "v")

## -----------------------------------------------------------------------------
citation("concurve")
citation("ggplot2")
citation("rstan")
citation("rstanarm")
citation("cowplot")
citation("scales")
citation("bayesplot")

