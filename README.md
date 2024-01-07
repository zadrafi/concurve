
### <span style="color:#000; font-weight: 400;">`concurve`</span> \| Graph Frequentist Distributions of Parameters </strong> <img src="https://raw.githubusercontent.com/zadrafi/concurve/master/man/figures/logo.svg" align="right" width="70"/>

------------------------------------------------------------------------

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/concurve)](https://CRAN.R-project.org/package=concurve)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Monthly
Downloads](https://cranlogs.r-pkg.org/badges/concurve)](https://cran.r-project.org/package=concurve)
[![Total
Downloads](https://cranlogs.r-pkg.org/badges/grand-total/concurve)](https://cran.r-project.org/package=concurve)
[![Rdoc](http://www.rdocumentation.org/badges/version/concurve)](http://www.rdocumentation.org/packages/concurve)
[![License: GPL
v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Travis build
status](https://travis-ci.com/zadrafi/concurve.svg?branch=master)](https://travis-ci.com/zadrafi/concurve)
[![Codecov test
coverage](https://codecov.io/gh/zadrafi/concurve/branch/master/graph/badge.svg)](https://codecov.io/gh/zadrafi/concurve?branch=master)
<!-- badges: end -->

#### [Compute & Graph](https://data.lesslikely.com/concurve/reference/ggcurve.html) Confidence Distributions and Likelihood Functions

<center>
<figure>
<img src="vignettes/confdistributions.png" align="center" width="550"/>  
<figcaption>
<p style="font-size: 12px">
Sample image taken from Schweder T, Hjort NL. (2016)
</p>
</figcaption>
</figure>
</center>

------------------------------------------------------------------------

#### For A Better Understanding of What These Are, See the [Background Literature](https://data.lesslikely.com/concurve/articles/literature.html)

------------------------------------------------------------------------

#### [Compare](https://data.lesslikely.com/concurve/reference/plot_compare.html) The Functions From Different Studies

<img src = "https://res.cloudinary.com/less-likely/image/upload/v1591475692/Site/functions.png" align="center" width ="350">
<img src = "https://res.cloudinary.com/less-likely/image/upload/v1591475692/Site/lfunctions.png" align="center" width ="350">

------------------------------------------------------------------------

#### [Export Tables](https://data.lesslikely.com/concurve/reference/curve_table.html) Easily For Word, Powerpoint, & TeX documents

<center>

<img src = "https://res.cloudinary.com/less-likely/image/upload/v1574628079/Site/tables.png" align="center" width="500">

------------------------------------------------------------------------

##### Install the package from [CRAN](https://cran.r-project.org/package=concurve) and check out the [articles](https://data.lesslikely.com/concurve/articles/index.html), which can also be found below.

(`It is highly recommended that you look at the articles.`)

    install.packages("concurve")

Try the following script if you run into any installation issues:

    install.packages("concurve", repos = "https://cloud.r-project.org/", dep = TRUE)

Install the developer version from
[GitHub](https://github.com/zadrafi/concurve/) using the following:

    remotes::install_github("zadrafi/concurve@master", dependencies = TRUE)

See which versions of the package are currently [supported
here](articles/supported.html).

------------------------------------------------------------------------

#### Articles

##### Check out the [article on using `Stata`](https://data.lesslikely.com/concurve/articles/stata.html) to obtain similar functions.

------------------------------------------------------------------------

##### See the following articles:

-   **[Comparison to Bayesian Posterior
    Distributions](https://data.lesslikely.com/concurve/articles/bayes.html)**
-   **[The Bootstrap and Consonance
    Functions](https://data.lesslikely.com/concurve/articles/bootstrap.html)**
-   **[Background
    Literature](https://data.lesslikely.com/concurve/articles/literature.html)**
-   **[Customizing
    Plots](https://data.lesslikely.com/concurve/articles/customizing.html)**
-   **[Examples in
    R](https://data.lesslikely.com/concurve/articles/examples.html)**
-   **[Logistic Regression in
    R](https://data.lesslikely.com/concurve/articles/logistic.html)**
-   **[Profile
    Likelihoods](https://data.lesslikely.com/concurve/articles/likelihood.html)**
-   **[Meta-Analysis
    Examples](https://data.lesslikely.com/concurve/articles/meta-analysis.html)**
-   **[Using
    Stata](https://data.lesslikely.com/concurve/articles/stata.html)**
-   **[Survival
    Modeling](https://data.lesslikely.com/concurve/articles/survival.html)**
-   **[S-values](https://data.lesslikely.com/concurve/articles/svalues.html)**
-   **[Generating
    Tables](https://data.lesslikely.com/concurve/articles/tables.html)**
-   **[Troubleshooting](https://data.lesslikely.com/concurve/articles/troubleshooting.html)**
-   **[Consonance Functions for Linear Mixed-Effects
    Models](https://data.lesslikely.com/concurve/articles/variancecomponents.html)**
-   **[Wish
    List](https://data.lesslikely.com/concurve/articles/wishlist.html)**

------------------------------------------------------------------------

### Citation

To properly cite the package, please see the [following
page](file:///Users/zad/Desktop/GitHub/concurve/docs/authors.html) or
run the R script below.

    citation("concurve")

------------------------------------------------------------------------

### Code of Conduct

Please note that the concurve project is released with a [Contributor
Code of
Conduct](https://data.lesslikely.com/concurve//CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.

##### Environment

The package was currently run on:

    ## R version 4.0.2 (2020-06-22)
    ## Platform: x86_64-apple-darwin17.0 (64-bit)
    ## Running under: macOS Catalina 10.15.6
    ## 
    ## Matrix products: default
    ## BLAS:   /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRblas.dylib
    ## LAPACK: /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRlapack.dylib
    ## 
    ## locale:
    ## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] compiler_4.0.2    magrittr_1.5      credentials_1.3.0 htmltools_0.5.0   tools_4.0.2       yaml_2.2.1        stringi_1.5.3    
    ##  [8] rmarkdown_2.4     knitr_1.30        stringr_1.4.0     xfun_0.18         digest_0.6.25     rlang_0.4.7       openssl_1.4.3    
    ## [15] sys_3.4           evaluate_0.14     askpass_1.1

------------------------------------------------------------------------
