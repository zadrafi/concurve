# Logistic Regression in R

Suppose we wanted to produce confidence distributions for data with
binary outcomes and where we employ a logistic regression, we would do
the following. Here, I use the mtcars dataset for the example and also
simulate some very simple binary data. We use
[`suppressMessages()`](https://rdrr.io/r/base/message.html) to avoid
seeing the long list of profiling messages.

## Cite R Packages

Please remember to cite the R packages that you use in your work.

``` r

citation("concurve")
```

    ## To cite package 'concurve' in publications use:
    ## 
    ##   Rafi Z, Vigotsky A (2026). _concurve: Computes and Plots
    ##   Compatibility (Confidence) Intervals, P-Values, S-Values, &
    ##   Likelihood Intervals to Form Consonance, Surprisal, & Likelihood
    ##   Functions_. R package version 3.0.0,
    ##   <https://CRAN.R-project.org/package=concurve>.
    ## 
    ##   Rafi Z, Greenland S (2020). "Semantic and Cognitive Tools to Aid
    ##   Statistical Science: Replace Confidence and Significance by
    ##   Compatibility and Surprise." _BMC Medical Research Methodology_,
    ##   *20*, 244. ISSN 1471-2288, doi:10.1186/s12874-020-01105-9
    ##   <https://doi.org/10.1186/s12874-020-01105-9>,
    ##   <https://doi.org/10.1186/s12874-020-01105-9>.
    ## 
    ## To see these entries in BibTeX format, use 'print(<citation>,
    ## bibtex=TRUE)', 'toBibtex(.)', or set
    ## 'options(citation.bibtex.max=999)'.

``` r

citation("cowplot")
```

    ## To cite package 'cowplot' in publications use:
    ## 
    ##   Wilke C (2025). _cowplot: Streamlined Plot Theme and Plot Annotations
    ##   for 'ggplot2'_. doi:10.32614/CRAN.package.cowplot
    ##   <https://doi.org/10.32614/CRAN.package.cowplot>, R package version
    ##   1.2.0, <https://CRAN.R-project.org/package=cowplot>.
    ## 
    ## A BibTeX entry for LaTeX users is
    ## 
    ##   @Manual{,
    ##     title = {cowplot: Streamlined Plot Theme and Plot Annotations for 'ggplot2'},
    ##     author = {Claus O. Wilke},
    ##     year = {2025},
    ##     note = {R package version 1.2.0},
    ##     url = {https://CRAN.R-project.org/package=cowplot},
    ##     doi = {10.32614/CRAN.package.cowplot},
    ##   }

------------------------------------------------------------------------

## References

------------------------------------------------------------------------
