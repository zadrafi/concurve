# Wish List

Here are some to-dos for `concurve` in the future.

- More unit tests for various purposes as [Alex Hayes describes in his
  wonderful
  article](https://www.alexpghayes.com/blog/testing-statistical-software/).
  He classifies them into three types, which I copy and paste below:
  - “Correctness tests check whether the code calculates the quantity it
    is supposed to calculated.”
  - “Parameter recovery tests check whether the implementation can
    recover correct parameters in well understood scenarios.”
  - “Convergence tests check whether iterative fitting procedures have
    actually reached a solution.”
  - “Identification tests check whether the solution is unique, and
    stable under small perturbations to the data.” \* \* \*
- Make [`curve_rev()`](reference/curve_rev.md) more usable for a wide
  range of scenarios (beyond proportions, and normal approximations).
  The confidence functions they produce especially break down with
  continuous variables, so I have completely disabled the option for
  now, and will implement an approach by working backwards from the
  deviance and log-likelihood functions. \* \* \*
- Produce consonance/confidence functions for quantile regressions from
  the `quantreg` package
- Implementation with the `nlme` package, although that has already
  started with some experimental work I and the other developers have
  done with the generalized least squares method from `nlme`. \* \* \*
- Possibly incorporate `rstan` via `rstantools` to produce likelihoods
  and posteriors

If you’d like to help contribute, please see the [contributing
guide](https://data.lesslikely.com/concurve/CONTRIBUTING.html).

### R Package Citations

``` r

citation("quantreg")
```

    ## To cite package 'quantreg' in publications use:
    ## 
    ##   Koenker R (2025). _quantreg: Quantile Regression_.
    ##   doi:10.32614/CRAN.package.quantreg
    ##   <https://doi.org/10.32614/CRAN.package.quantreg>, R package version
    ##   6.1, <https://CRAN.R-project.org/package=quantreg>.
    ## 
    ## A BibTeX entry for LaTeX users is
    ## 
    ##   @Manual{,
    ##     title = {quantreg: Quantile Regression},
    ##     author = {Roger Koenker},
    ##     year = {2025},
    ##     note = {R package version 6.1},
    ##     url = {https://CRAN.R-project.org/package=quantreg},
    ##     doi = {10.32614/CRAN.package.quantreg},
    ##   }

``` r

citation("nlme")
```

    ## To cite package 'nlme' in publications use:
    ## 
    ##   Pinheiro J, Bates D, R Core Team (2025). _nlme: Linear and Nonlinear
    ##   Mixed Effects Models_. doi:10.32614/CRAN.package.nlme
    ##   <https://doi.org/10.32614/CRAN.package.nlme>, R package version
    ##   3.1-168, <https://CRAN.R-project.org/package=nlme>.
    ## 
    ##   Pinheiro JC, Bates DM (2000). _Mixed-Effects Models in S and S-PLUS_.
    ##   Springer, New York. doi:10.1007/b98882
    ##   <https://doi.org/10.1007/b98882>.
    ## 
    ## To see these entries in BibTeX format, use 'print(<citation>,
    ## bibtex=TRUE)', 'toBibtex(.)', or set
    ## 'options(citation.bibtex.max=999)'.

``` r

citation("rstan")
```

    ## To cite RStan in publications use:
    ## 
    ##   Stan Development Team (2025). RStan: the R interface to Stan. R
    ##   package version 2.32.7. https://mc-stan.org/.
    ## 
    ## A BibTeX entry for LaTeX users is
    ## 
    ##   @Misc{,
    ##     title = {{RStan}: the {R} interface to {Stan}},
    ##     author = {{Stan Development Team}},
    ##     note = {R package version 2.32.7},
    ##     year = {2025},
    ##     url = {https://mc-stan.org/},
    ##   }

``` r

citation("rstantools")
```

    ## To cite package 'rstantools' in publications use:
    ## 
    ##   Gabry J, Goodrich B, Lysy M, Johnson A (2026). _rstantools: Tools for
    ##   Developing R Packages Interfacing with 'Stan'_.
    ##   doi:10.32614/CRAN.package.rstantools
    ##   <https://doi.org/10.32614/CRAN.package.rstantools>, R package version
    ##   2.6.0, <https://CRAN.R-project.org/package=rstantools>.
    ## 
    ## A BibTeX entry for LaTeX users is
    ## 
    ##   @Manual{,
    ##     title = {rstantools: Tools for Developing R Packages Interfacing with 'Stan'},
    ##     author = {Jonah Gabry and Ben Goodrich and Martin Lysy and Andrew Johnson},
    ##     year = {2026},
    ##     note = {R package version 2.6.0},
    ##     url = {https://CRAN.R-project.org/package=rstantools},
    ##     doi = {10.32614/CRAN.package.rstantools},
    ##   }
