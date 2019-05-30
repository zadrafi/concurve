defunct = function(msg = "This function is depreciated") function(...) return(stop(msg))

# Graphical functions
plotpint = defunct("plotpint() is now depreciated. Please use geom_consonance() instead.")
plotsint = defunct("plotsint() is now depreciated. Please use geom_surprisal() instead.")

# Computational functions
meanintervals = defunct("meanintervals() is now depreciated. Please use geom_consonance() instead.")
metaintervals = defunct("metaintervals() is now depreciated. Please use geom_surprisal() instead.")
genintervals = defunct("genintervals() is now depreciated. Please use geom_consonance() instead.")
corrintervals = defunct("corrintervals() is now depreciated. Please use geom_surprisal() instead.")
survintervals = defunct("survintervals() is now depreciated. Please use geom_consonance() instead.")

# Current names

meanintervals
metaintervals
genintervals
corrintervals
survintervals

# Possible standardized names

curve_mean
curve_corr
curve_gen
curve_surv
curve_meta

intervals_mean
intervals_corr
intervas_gen
intervals_surv
intervals_meta

function_mean
function_corr
function_gen
function_meta
function_surv
