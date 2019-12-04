defunct <- function(msg = "This function is deprecated") {
  function(...) {
    return(stop(msg))
  }
}

# Graphical functions
plotpint <- defunct("plotpint() is now deprecated. Please use ggcurve() instead.")
plotsint <- defunct("plotsint() is now deprecated. Please use ggcurve() instead.")
ggconcurve <- defunct("ggconcurve() is now deprecated. Please use ggcurve() instead.")
plot_concurve <- defunct("plot_concurve() is now deprecated. Please use ggcurve() instead.")


# Computational functions
meanintervals <- defunct("meanintervals() is now deprecated. Please use curve_mean() instead.")
metaintervals <- defunct("metaintervals() is now deprecated. Please use curve_meta() instead.")
genintervals <- defunct("genintervals() is now deprecated. Please use curve_gen() instead.")
corrintervals <- defunct("corrintervals() is now deprecated. Please use curve_corr() instead.")
survintervals <- defunct("survintervals() is now deprecated. Please use curve_surv() instead.")
rev_eng <- defunct("rev_eng() is now deprecated. Please use curve_rev() instead.")
