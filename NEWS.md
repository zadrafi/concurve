# concurve 1.1.1

## Major changes 

* The plotpint() function which plotted consonance functions has been renamed to gg_consonance().
* The plotsint() function which plotted surprisal functions has been renamed to gg_surprisal().
* Consonance functions can be plotted as a pyramid (right side up) or inverted (upside down) via the "position" item in gg_consonance().
* Null values (for means & ratios) can be plotted via all the gg_ functions to show how much of the interval surrounds it.
* Log transformations included in all the plotting functions for ratio measures. 
* New function to compute log-likelihood functions (also known as support intervals) via the likintervals() function.
* Support intervals can now be graphed via the gg_likely() function.

# concurve 1.08

## Major changes

* Can produce consonance and surprisal functions for correlations via the `corrintervals` function.
* Now able to construct consonance and surprisal functions from the point estiate, and confidence limits via the `reveng` function.
* Graphs produced via the `plotpint` or `plotsint` function now able to take custom titles, subtitles, x-axis titles, and captions.

# concurve 1.07

## Major changes

* Can now produce consonance and surprisal functions for survival data produced with the `survival` package.

# concurve 1.06

## Major changes

* Now contains [documentation](https://data.lesslikely.com/concurve/articles/stata.html) for producing interval functons in `Stata`.

## Minor changes

* Default plots now contain grids, title, subtitle, and a caption. 
* Updated figures in README and the 'Examples in R' vignette/article.
