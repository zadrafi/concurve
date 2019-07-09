defunct <- function(msg = "This function is depreciated") function(...) return(stop(msg))

# Graphical functions
plotpint <- defunct("plotpint() is now depreciated. Please use ggconcurve() or plot.concurve instead.")
plotsint <- defunct("plotsint() is now depreciated. Please use ggconcurve() or plot.concurve instead.")

# Computational functions
meanintervals <- defunct("meanintervals() is now depreciated. Please use curve_mean() instead.")
metaintervals <- defunct("metaintervals() is now depreciated. Please use curve_meta() instead.")
genintervals <- defunct("genintervals() is now depreciated. Please use curve_gen() instead.")
corrintervals <- defunct("corrintervals() is now depreciated. Please use curve_corr() instead.")
survintervals <- defunct("survintervals() is now depreciated. Please use curve_surv() instead.")
likintervals <- defunct("likintervals() is now depreciated. Please use curve_lik() instead.")
rev_eng <- defunct("rev_eng() is now depreciated. Please use curve_rev instead.")

# RMD Check
utils::globalVariables(c("df", "lower.limit", "upper.limit", "intrvl.level", "pvalue", "svalue"))
