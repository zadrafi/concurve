library(concurve)

GroupA <- rnorm(500)
GroupB <- rnorm(500)

RandomData <- data.frame(GroupA, GroupB)

intervalsdf <- curve_mean(GroupA, GroupB,
  data = RandomData, method = "default")

tibble::tibble(intervalsdf[[1]])


(z <- curve_table(intervalsdf[[1]], format = "data.frame"))
(z <- curve_table(intervalsdf[[1]], format = "tibble"))
# (z <- curve_table(intervalsdf, format = "docx"))
# (z <- curve_table(intervalsdf, format = "pptx"))
(z <- curve_table(intervalsdf[[1]], format = "latex"))
(z <- curve_table(intervalsdf[[1]], format = "image"))

(function1 <- ggcurve(data = intervalsdf[[1]], type = "c", nullvalue = TRUE))

(function1 <- ggcurve(data = intervalsdf[[1]], type = "s"))
(function1s <- ggcurve(data = intervalsdf[[2]], type = "cdf", nullvalue = TRUE))


options(mc.cores = 8L)
getOption("mc.cores", 2L)

GroupA2 <- rnorm(500)
GroupB2 <- rnorm(500)

RandomData2 <- data.frame(GroupA2, GroupB2)

model <- lm(GroupA2 ~ GroupB2, data = RandomData2)

system.time(randomframe <- curve_gen(model, "GroupB2"))

options(mc.cores = 2L)

GroupA2 <- rnorm(500)
GroupB2 <- rnorm(500)

RandomData2 <- data.frame(GroupA2, GroupB2)

model <- lm(GroupA2 ~ GroupB2, data = RandomData2)

system.time(randomframe <- curve_gen(model, "GroupB2"))


(function2 <- ggcurve(type = "c", randomframe[[1]], levels = c(0.50, 0.75, 0.95), nullvalue = TRUE))

(curve_compare(
  data1 = intervalsdf[[1]], data2 = randomframe[[1]], type = "c",
  plot = TRUE, measure = "default", nullvalue = TRUE
))

(curve_compare(
  data1 = intervalsdf[[1]], data2 = randomframe[[1]], type = "s",
  plot = TRUE, measure = "default", nullvalue = FALSE
))

(plot_compare(data1 = intervalsdf[[1]], data2 = randomframe[[1]], type = "c", measure = "default", nullvalue = TRUE))


(df1 <- curve_rev(point = 1.61, LL = 0.997, UL = 2.59, measure = "ratio", steps = 10000)) ## may take some time
(df2 <- curve_rev(point = 1.7, LL = 1.1, UL = 2.6, measure = "ratio", steps = 10000)) ## may take some time


(r <- (curve_compare(
  data1 = df1[[1]], data2 = df2[[1]], type = "c",
  plot = TRUE, measure = "ratio", nullvalue = TRUE
)))

(curve_compare(
  data1 = df1[[1]], data2 = df2[[1]], type = "s",
  plot = TRUE, measure = "ratio", nullvalue = FALSE
))

(plot_compare(
  data1 = df1[[1]], data2 = df2[[1]], type = "c", measure = "ratio", nullvalue = TRUE, title = "Brown et al. 2017. J Clin Psychiatry. vs. Brown et al. 2017. JAMA.",
  subtitle = "J Clin Psychiatry: OR = 1.7, 95% CL: LL = 1.1, UL = 2.6 \nJAMA: HR = 1.61, 95% CL: LL = 0.997, UL = 2.59", xaxis = expression(Theta ~ "= Hazard Ratio / Odds Ratio")
))
(plot_compare(
  data1 = df1[[1]], data2 = df2[[1]], type = "s", measure = "ratio", nullvalue = FALSE, title = "Brown et al. 2017. J Clin Psychiatry. vs. Brown et al. 2017. JAMA.",
  subtitle = "J Clin Psychiatry: OR = 1.7, 95% CL: LL = 1.1, UL = 2.6 \nJAMA: HR = 1.61, 95% CL: LL = 0.997, UL = 2.59", xaxis = expression(Theta ~ "= Hazard Ratio / Odds Ratio")
))


lik1 <- curve_rev(point = 1.7, LL = 1.1, UL = 2.6, type = "l", measure = "ratio", steps = 10000)
(ggcurve(data = lik1[[1]], type = "l1", measure = "ratio", nullvalue = TRUE))


lik2 <- curve_rev(point = 1.61, LL = 0.997, UL = 2.59,type = "l", measure = "ratio", steps = 10000)
(ggcurve(data = lik2[[1]], type = "l1", measure = "ratio", nullvalue = TRUE))


(plot_compare(
  data1 = lik1[[1]], data2 = lik2[[1]], type = "l1", measure = "ratio", nullvalue = TRUE, title = "Brown et al. 2017. J Clin Psychiatry. vs. \nBrown et al. 2017. JAMA.",
  subtitle = "J Clin Psychiatry: OR = 1.7, 1/6.83 LI: LL = 1.1, UL = 2.6 \nJAMA: HR = 1.61, 1/6.83 LI: LL = 0.997, UL = 2.59", xaxis = expression(Theta ~ "= Hazard Ratio / Odds Ratio")
))
(plot_compare(
  data1 = lik1[[1]], data2 = lik2[[1]], type = "d", measure = "ratio", nullvalue = TRUE, title = "Brown et al. 2017. J Clin Psychiatry. vs. \nBrown et al. 2017. JAMA.",
  subtitle = "J Clin Psychiatry: OR = 1.7, 1/6.83 LI: LL = 1.1, UL = 2.6 \nJAMA: HR = 1.61, 1/6.83 LI: LL = 0.997, UL = 2.59", xaxis = expression(Theta ~ "= Hazard Ratio / Odds Ratio")
))



(plot_compare(data1 = intervalsdf[[1]], data2 = randomframe[[1]], type = "c", measure = "default", nullvalue = TRUE))

plot_compare(data1 = lik1[[1]], data2 = lik2[[1]], type = "d", measure = "ratio", nullvalue = FALSE)

(function4 <- ggcurve(randomframe[[1]], type = "s", title = "Surprisal Function", subtitle = "Interval estimates are plotted to their corresponding S-values")
)


# Excel Trial

excel <- curve_rev(point = 2.8, LL = -0.9, UL = 6.5)

ggcurve(excel[[1]], nullvalue = TRUE, levels = c(0.50, 0.75, 0.95),
        subtitle = "EXCEL TRIAL: Point = 2.8, 95% CL: LL = -0.9, UL = 6.5 p = 0.13",
        xaxis = expression(Theta ~ "= % Difference For Primary Composite Outcome"))



# Bootstrapping -----------------------------------------------------------

# Nonparametric BCA Bootstrapping ---------------------------------------------

options(mc.cores = 8L)
getOption("mc.cores", 2L)

data(diabetes, package = "bcaboot")
Xy <- cbind(diabetes$x, diabetes$y)
rfun <- function(Xy) {
  y <- Xy[, 11]
  X <- Xy[, 1:10]
  return(summary(lm(y ~ X))$adj.r.squared)
}

system.time(x <- curve_boot(data = Xy, func = rfun, method = "bca", replicates = 20000, steps = 1000))


x[[5]]
x[[6]]


options(mc.cores = 2L)
getOption("mc.cores", 2L)

data(diabetes, package = "bcaboot")
Xy <- cbind(diabetes$x, diabetes$y)
rfun <- function(Xy) {
  y <- Xy[, 11]
  X <- Xy[, 1:10]
  return(summary(lm(y ~ X))$adj.r.squared)
}

system.time(x <- curve_boot(data = Xy, func = rfun, method = "bca", replicates = 20000, steps = 1000))


ggcurve(data = x[[1]])
ggcurve(data = x[[3]])

plot_compare(x[[1]], x[[3]])


iris <- datasets::iris
foo <- function(data, indices) {
  dt <- data[indices, ]
  c(
    cor(dt[, 1], dt[, 2], method = "p")
  )
}

y <- curve_boot(data = iris, func = foo, method = "bca", replicates = 200, steps = 1000)


ggcurve(data = y[[1]])
ggcurve(data = y[[3]])

plot_compare(y[[1]], y[[3]])

# Parametric BCA Bootstrapping --------------------------------------------

data(diabetes, package = "bcaboot")
X <- diabetes$x
y <- scale(diabetes$y, center = TRUE, scale = FALSE)
lm.model <- lm(y ~ X - 1)
mu.hat <- lm.model$fitted.values
sigma.hat <- stats::sd(lm.model$residuals)
t0 <- summary(lm.model)$adj.r.squared
y.star <- sapply(mu.hat, rnorm, n = 1000, sd = sigma.hat)
tt <- apply(y.star, 1, function(y) summary(lm(y ~ X - 1))$adj.r.squared)
b.star <- y.star %*% X
set.seed(1234)

replicates = 2000
steps = 1000
intrvls <- 0.5 / steps
alpha <- seq(0.00, 0.50, intrvls)

df <- curve_boot(method = "bcapar", t0 = t0, tt = tt, bb = b.star)

ggcurve(df[[1]])
ggcurve(df[[3]])
ggcurve(df[[5]], type = "cd")

# Percentile Bootstrapping ------------------------------------------------


library(Lock5Data)
dataz <- data(CommuteAtlanta)

func = function(data, index) {
  x <- as.numeric(unlist(data[1]))
  y <- as.numeric(unlist(data[2]))
  return(mean(x[index]) - mean(y[index]))
}

z <- curve_boot(data = CommuteAtlanta, func = func, method = "t", replicates = 200, steps = 1000)
ggcurve(data = z[[1]])
ggcurve(data = z[[2]], type = "cd")

# Meta-Analysis -----------------------------------------------------------

GroupAData <- runif(20, min = 0, max = 100)
GroupAMean <- round(mean(GroupAData), digits = 2)
GroupASD <- round(sd(GroupAData), digits = 2)

GroupBData <- runif(20, min = 0, max = 100)
GroupBMean <- round(mean(GroupBData), digits = 2)
GroupBSD <- round(sd(GroupBData), digits = 2)

GroupCData <- runif(20, min = 0, max = 100)
GroupCMean <- round(mean(GroupCData), digits = 2)
GroupCSD <- round(sd(GroupCData), digits = 2)

GroupDData <- runif(20, min = 0, max = 100)
GroupDMean <- round(mean(GroupDData), digits = 2)
GroupDSD <- round(sd(GroupDData), digits = 2)


StudyName <- c("Study1", "Study2")
MeanTreatment <- c(GroupAMean, GroupCMean)
MeanControl <- c(GroupBMean, GroupDMean)
SDTreatment <- c(GroupASD, GroupCSD)
SDControl <- c(GroupBSD, GroupDSD)
NTreatment <- c(20, 20)
NControl <- c(20, 20)

metadf <- data.frame(
  StudyName, MeanTreatment, MeanControl,
  SDTreatment, SDControl,
  NTreatment, NControl
)

library(metafor)

dat <- escalc(
  measure = "SMD",
  m1i = MeanTreatment, sd1i = SDTreatment, n1i = NTreatment,
  m2i = MeanControl, sd2i = SDControl, n2i = NControl,
  data = metadf
)


res <- rma(yi, vi,
  data = dat, slab = paste(StudyName, sep = ", "),
  method = "FE", digits = 2
)

res


metaf <- curve_meta(res)
tibble::tibble(metaf[[1]])

res2 <- rma(yi, vi,
  data = dat, slab = paste(StudyName, sep = ", "),
  method = "REML", digits = 2
)

res2


metaf2 <- curve_meta(res2)
tibble::tibble(metaf2[[1]])


(function5 <- ggcurve(metaf[[1]], type = "c"))


(function6 <- ggcurve(metaf[[1]], type = "s", title = "Surprisal Function", subtitle = "Interval estimates are plotted to their corresponding S-values")
)

(function7 <- ggcurve(metaf2[[1]], type = "c"))


(function8 <- ggcurve(metaf2[[1]], type = "s", title = "Surprisal Function", subtitle = "Interval estimates are plotted to their corresponding S-values")
)

(curve_compare(data1 = metaf[[1]], data2 = metaf2[[1]], type = "c", plot = TRUE, measure = "default")
)
(curve_compare(data1 = metaf[[1]], data2 = metaf2[[1]], type = "s", plot = TRUE, measure = "default")
)

??optimal
