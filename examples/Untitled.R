

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
