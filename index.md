### `concurve` \| Graph Frequentist Distributions of Parameters ![](reference/figures/logo.svg)

------------------------------------------------------------------------

#### [Compute & Graph](https://data.lesslikely.com/concurve/reference/ggcurve.html) Confidence Distributions and Likelihood Functions

![](articles/confdistributions.png)

Sample image taken from Schweder T, Hjort NL. (2016)

------------------------------------------------------------------------

#### For A Better Understanding of What These Are, See the [Background Literature](https://data.lesslikely.com/concurve/articles/literature.html)

------------------------------------------------------------------------

#### [Compare](https://data.lesslikely.com/concurve/reference/plot_compare.html) The Functions From Different Studies

![](https://res.cloudinary.com/less-likely/image/upload/v1591475692/Site/functions.png)![](https://res.cloudinary.com/less-likely/image/upload/v1591475692/Site/lfunctions.png)

------------------------------------------------------------------------

#### [Export Tables](https://data.lesslikely.com/concurve/reference/curve_table.html) Easily For Word, Powerpoint, & TeX documents

![](https://res.cloudinary.com/less-likely/image/upload/v1574628079/Site/tables.png)

------------------------------------------------------------------------

##### Install the package from [CRAN](https://cran.r-project.org/package=concurve) and check out the [articles](https://data.lesslikely.com/concurve/articles/index.html), which can also be found below.

(`It is highly recommended that you look at the articles.`)

``` r

install.packages("concurve")
```

Try the following script if you run into any installation issues:

``` r

install.packages("concurve", repos = "https://cloud.r-project.org/", dep = TRUE)
```

Install the developer version from
[GitHub](https://github.com/zadrafi/concurve/) using the following:

``` r

remotes::install_github("zadrafi/concurve@master", dependencies = TRUE)
```

See which versions of the package are currently [supported
here](https://stat.lesslikely.com/concurve/articles/supported.md).

------------------------------------------------------------------------

#### Articles

##### Check out the [article on using `Stata`](https://data.lesslikely.com/concurve/articles/stata.html) to obtain similar functions.

------------------------------------------------------------------------

##### See the following articles:

- [**Comparison to Bayesian Posterior
  Distributions**](https://data.lesslikely.com/concurve/articles/bayes.html)
- [**The Bootstrap and Consonance
  Functions**](https://data.lesslikely.com/concurve/articles/bootstrap.html)
- [**Background
  Literature**](https://data.lesslikely.com/concurve/articles/literature.html)
- [**Customizing
  Plots**](https://data.lesslikely.com/concurve/articles/customizing.html)
- [**Examples in
  R**](https://data.lesslikely.com/concurve/articles/examples.html)
- [**Logistic Regression in
  R**](https://data.lesslikely.com/concurve/articles/logistic.html)
- [**Profile
  Likelihoods**](https://data.lesslikely.com/concurve/articles/likelihood.html)
- [**Meta-Analysis
  Examples**](https://data.lesslikely.com/concurve/articles/meta-analysis.html)
- [**Using
  Stata**](https://data.lesslikely.com/concurve/articles/stata.html)
- [**Survival
  Modeling**](https://data.lesslikely.com/concurve/articles/survival.html)
- [**S-values**](https://data.lesslikely.com/concurve/articles/svalues.html)
- [**Generating
  Tables**](https://data.lesslikely.com/concurve/articles/tables.html)
- [**Troubleshooting**](https://data.lesslikely.com/concurve/articles/troubleshooting.html)
- [**Consonance Functions for Linear Mixed-Effects
  Models**](https://data.lesslikely.com/concurve/articles/variancecomponents.html)
- [**Wish
  List**](https://data.lesslikely.com/concurve/articles/wishlist.html)

------------------------------------------------------------------------

### Citation

To properly cite the package, please see the [following
page](file:///Users/zad/Desktop/GitHub/concurve/docs/authors.md) or run
the R script below.

``` r

citation("concurve")
```

------------------------------------------------------------------------

### Code of Conduct

Please note that the concurve project is released with a [Contributor
Code of
Conduct](https://data.lesslikely.com/concurve//CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.

##### Environment

The package was currently run on:

``` R
## R version 4.6.0 (2026-04-24)
## Platform: aarch64-apple-darwin25.4.0
## Running under: macOS Tahoe 26.5.1
## 
## Matrix products: default
## BLAS:   /opt/homebrew/Cellar/openblas/0.3.33/lib/libopenblasp-r0.3.33.dylib 
## LAPACK: /opt/homebrew/Cellar/r/4.6.0/lib/R/lib/libRlapack.dylib;  LAPACK version 3.12.1
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## time zone: America/New_York
## tzcode source: internal
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## loaded via a namespace (and not attached):
##  [1] compiler_4.6.0    here_1.0.2        fastmap_1.2.0     cli_3.6.6         rprojroot_2.1.1   htmltools_0.5.9   tools_4.6.0      
##  [8] parallel_4.6.0    otel_0.2.0        rstudioapi_0.19.0 yaml_2.3.12       rmarkdown_2.31    knitr_1.51        xfun_0.59        
## [15] digest_0.6.39     rlang_1.2.0       evaluate_1.0.5
```

------------------------------------------------------------------------
