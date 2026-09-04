#' Defunct Functions in concurve
#'
#' @description
#' The functions listed here are **defunct**: they have been removed and
#' calling one is an error. They are retained as stubs so that old scripts
#' fail with a message naming the current replacement, rather than an
#' opaque "could not find function".
#'
#' They date from the reorganisation in which the interval constructors
#' were renamed to a common `curve_*` prefix and the several plotting
#' functions were consolidated into a single [ggcurve()].
#'
#' @details
#' \tabular{ll}{
#'   \strong{Defunct} \tab \strong{Use instead} \cr
#'   `plotpint()`      \tab [ggcurve()]     \cr
#'   `plotsint()`      \tab [ggcurve()]     \cr
#'   `ggconcurve()`    \tab [ggcurve()]     \cr
#'   `plot_concurve()` \tab [ggcurve()]     \cr
#'   `meanintervals()` \tab [curve_mean()]  \cr
#'   `metaintervals()` \tab [curve_meta()]  \cr
#'   `genintervals()`  \tab [curve_gen()]   \cr
#'   `corrintervals()` \tab [curve_corr()]  \cr
#'   `survintervals()` \tab [curve_surv()]  \cr
#'   `likintervals()`  \tab [curve_lik()]   \cr
#'   `rev_eng()`       \tab [curve_rev()]   \cr
#' }
#'
#' The replacements are not drop-in: the `curve_*` functions return a
#' classed `concurve` list of three elements (the interval function, the
#' density, and a summary table) rather than a bare data frame, and
#' [ggcurve()] takes the first element of that list together with a `type`
#' argument. See `vignette("examples", package = "concurve")`.
#'
#' @param ... Ignored. Accepted so that an old call reaches the error
#'   message instead of failing on argument matching first.
#'
#' @return These functions never return. Each signals an error of class
#'   `defunctError`.
#'
#' @name defunct
#' @keywords internal
NULL


# Signal a standard defunctError naming the replacement. Using .Defunct()
# rather than stop() gives the condition the "defunctError" class, so it
# can be caught and tested for specifically.
defunct_stop <- function(old, new) {
  .Defunct(
    new = paste0(new, "()"),
    package = "concurve",
    msg = sprintf(
      "%s() is defunct.\nUse %s() instead.",
      old, new
    )
  )
}


# Graphical functions ---------------------------------------------------

#' @rdname defunct
#' @export
plotpint <- function(...) defunct_stop("plotpint", "ggcurve")

#' @rdname defunct
#' @export
plotsint <- function(...) defunct_stop("plotsint", "ggcurve")

#' @rdname defunct
#' @export
ggconcurve <- function(...) defunct_stop("ggconcurve", "ggcurve")

#' @rdname defunct
#' @export
plot_concurve <- function(...) defunct_stop("plot_concurve", "ggcurve")


# Computational functions -----------------------------------------------

#' @rdname defunct
#' @export
meanintervals <- function(...) defunct_stop("meanintervals", "curve_mean")

#' @rdname defunct
#' @export
metaintervals <- function(...) defunct_stop("metaintervals", "curve_meta")

#' @rdname defunct
#' @export
genintervals <- function(...) defunct_stop("genintervals", "curve_gen")

#' @rdname defunct
#' @export
corrintervals <- function(...) defunct_stop("corrintervals", "curve_corr")

#' @rdname defunct
#' @export
survintervals <- function(...) defunct_stop("survintervals", "curve_surv")

#' @rdname defunct
#' @export
likintervals <- function(...) defunct_stop("likintervals", "curve_lik")

#' @rdname defunct
#' @export
rev_eng <- function(...) defunct_stop("rev_eng", "curve_rev")
