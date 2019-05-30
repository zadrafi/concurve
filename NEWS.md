# concurve 1.1.1

## Major changes 

* New function to compute log-likelihood functions (also known as support intervals) via the supportintervals() function.
* Support intervals can now be graphed via the geom_likely() function.
* The plotpint() function which plotted consonance functions has been renamed to geom_consonance().
* The plotsint() function which plotted surprisal functions has been renamed to geom_surprisal().
* Consonance functions can be plotted as a pyramid (right side up) or inverted (upside down) via the "position" item in geom_consonance().
* Null values (for means & ratios) can be plotted via all the geoms to show how much of the interval surrounds it.
* Log transformations included in all the geoms for ratio measures. 
* 


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
