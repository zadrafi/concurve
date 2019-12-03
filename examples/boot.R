library(boot)
library(boot)
library(Lock5Data)
dataz <- data(CommuteAtlanta)

func = function(data, index) {
  x <- as.numeric(unlist(data[1]))
  y <- as.numeric(unlist(data[2]))
  return(mean(x[index]) - mean(y[index]))
}

library(parallel)
detectCores()

intervals <- 1:1000/1000

set.seed(1031)

GroupA <- runif(100, min = 0, max = 100)
GroupB <- runif(100, min = 0, max = 100)

RandomData <- data.frame(GroupA, GroupB)

replicates <- 20000


t.boot = boot(data = CommuteAtlanta, func, replicates, parallel = "multicore", ncpus = (detectCores()))

intrvls <- 1:1000/1000

library(parallel)
library(pbapply)
library(microbenchmark)
library(boot)
library(concurve)

microbenchmark(
t = mclapply(intrvls, FUN = function(i) boot.ci(t.boot, conf = i, type = "perc")$perc[4:5],
              mc.cores = detectCores()),
a = pblapply(intrvls, FUN = function(i) boot.ci(t.boot, conf = i, type = "perc")$perc[4:5],
            cl = detectCores()))

df <- data.frame(do.call(rbind, t))
intrvl.limit <- c("lower.limit", "upper.limit")
colnames(df) <- intrvl.limit
df$intrvl.width <- (abs((df$upper.limit) - (df$lower.limit)))
df$intrvl.level <- intrvls
df$cdf <- (abs(df$intrvl.level / 2)) + 0.5
df$pvalue <- 1 - intrvls
df$svalue <- -log2(df$pvalue)
df <- head(df, -1)
class(df) <- c("data.frame","concurve")


densdf <- data.frame(c(df$lower.limit, df$upper.limit))
colnames(densdf) <- "x"
densdf <- head(densdf, -1)
class(densdf) <- c("data.frame","concurve")
plot(density(densdf$x))

if (table == TRUE) {
  levels <- c(0.25, 0.50, 0.75, 0.80, 0.85, 0.90, 0.95, 0.975, 0.99)
  (df_subintervals <- (curve_table(df, levels, type = "data.frame")))
  class(df_subintervals) <- c("data.frame","concurve")
  dataframes <- list(df, densdf, df_subintervals)
  names(dataframes) <- c("Intervals Dataframe", "Intervals Density", "Intervals Table")
  class(dataframes) <- "concurve"
  return(dataframes)

} else if (table == FALSE) {
  return(list(df, densdf))
}

