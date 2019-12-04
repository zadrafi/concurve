
<center>

<strong> concurve | Graph Interval Functions </strong>
<img src="man/figures/logo.svg" align="right" width="70"/>

</center>

-----

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/concurve)](https://CRAN.R-project.org/package=concurve)
[![Travis build
status](https://travis-ci.org/Zadchow/concurve.svg?branch=master)](https://travis-ci.org/Zadchow/concurve)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![](https://cranlogs.r-pkg.org/badges/grand-total/concurve)](https://cran.r-project.org/package=concurve)
[![Rdoc](http://www.rdocumentation.org/badges/version/concurve)](http://www.rdocumentation.org/packages/concurve)
[![License: GPL
v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
<!-- badges: end -->

### [Compare](https://data.lesslikely.com/concurve/reference/curve_compare.html) Functions From Different Datasets/Studies

<img src = "https://res.cloudinary.com/less-likely/image/upload/v1574598684/Site/cfunctions.png" align="center" width ="200">
<img src = "https://res.cloudinary.com/less-likely/image/upload/v1574598684/Site/sfunctions.png" align="center" width="200">
<img src = "https://res.cloudinary.com/less-likely/image/upload/v1574598684/Site/lfunctions.png" align="center" width ="200">
<img src = "https://res.cloudinary.com/less-likely/image/upload/v1574598684/Site/dfunctions.png" align="center" width="200">

-----

### [Export Tables](https://data.lesslikely.com/concurve/reference/curve_table.html) Easily For Word, Powerpoint, & TeX documents

<center>

<img src = "https://res.cloudinary.com/less-likely/image/upload/v1574628079/Site/tables.png" align="center" width="375">

<img src = "https://res.cloudinary.com/less-likely/image/upload/v1575441662/Site/Logo2.jpg" align="center" width="150">

</center>

-----

### Install the Package From [CRAN](https://cran.r-project.org/package=concurve) Below To Follow The [Examples](https://data.lesslikely.com/concurve/articles/examples.html).

(`Serious
Recommendation`)

    install.packages("concurve")

-----

## Check out the [Article on Using Stata](https://data.lesslikely.com/concurve/articles/stata.html) for concurve.

-----

## Dependencies

  - ggplot2
  - metafor
  - parallel
  - MASS
  - boot
  - bcaboot
  - compiler
  - ProfileLikelihood
  - pbmcapply
  - rlang
  - magrittr
  - dplyr
  - tidyr
  - knitr
  - flextable
  - officer
  - tibble
  - survival
  - survminer
  - scales

-----

# Purpose

> In particular, the usual 95% default forces the user’s focus onto
> parameter values that yield p \> 0.05, without regard to the trivial
> difference between (say) p = 0.06 and p = 0.04 (a difference not even
> worth a coin toss). To address this problem, we first note that a 95%
> interval estimate is only one of a number of arbitrary dichotomization
> of possibilities of parameter values (into either inside or outside of
> an interval). A more accurate picture of uncertainty is then obtained
> by examining intervals using other percentiles, e.g.,
> proportionally-spaced compatibility levels such as p 0.25, 0.05, 0.01,
> which correspond to 75%, 95%, 99% CIs and equally-spaced S-values of s
> \< 2, 4.32, 6.64 bits. When a detailed picture is desired, a table or
> graph of all the P-values and S-values across a broad range of
> parameter values seems the clearest way to see how compatibility
> varies smoothly across the values.
> 
> Graphs of P-values or their equivalent have been promoted for decades
> \[34, 56–59\], yet their adoption has been slight. Nonetheless,
> P-value and S-value graphing software is now available freely through
> several statistical packages \[60–62\]. A graph of the P-values p
> against possible parameter values allows one to see at a glance which
> parameter values are most compatible with the data under the
> background assumptions. This graph is known as the P-value function,
> or compatibility, consonance, or confidence curve \[34, 57, 58, 63,
> 64\]. Transforming the corresponding P-values in the graph to S-values
> produces an S-value (surprisal) function.
> 
> Following the common (and important) warning that P-values are not
> hypothesis probabilities, we caution that the P-value graph is not a
> probability distribution: It shows compatibility of parameter values
> with the data, rather than plausibility or probability of those values
> given the data. This is not a subtle difference: compatibility is a
> much weaker condition than plausibility. Consider for example that
> complete fabrication of the data is always an explanation compatible
> with the data (and indeed has happened in some influential medical
> studies \[65\]), but in studies with many participants and authors
> involved in all aspects of data collection it becomes so implausible
> or improbable as to not even merit mention. We emphasize then that all
> the P-value ever addresses in a direct logical sense is compatibility;
> for hypothesis probabilities one must turn to Bayesian methods \[25\].
> - [Chow & Greenland, 2019](https://arxiv.org/abs/1909.08579);
> [Greenland & Chow, 2019](https://arxiv.org/abs/1909.08583)

-----

> In addition to the overt statistical position, the p-value function
> also provides easily and accurately many of the familiar types of
> summary information: a **median estimate** of the parameter; a
> **one-sided test statistic** for a scalar parameter value at any
> chosen level; the related **power function**; a **lower confidence
> bound** at any level; an **upper confidence bound** at any level; and
> **confidence intervals** with chosen upper and lower confidence
> limits. The p value reports all the **common inference material**, but
> with **high accuracy, basic uniqueness, and wide generality**.
> 
> From a scientific perspective, the **likelihood function** and
> **p-value function** provide the basis for scientific judgments by an
> investigator, and by other investigators who might have interest. **It
> thus replaces a blunt yes or no decision by an opportunity for
> appropriate informed judgment.**” -
> [Fraser, 2019](https://doi.org/10.1080/00031305.2018.1556735)

-----

> *Statistical software does not help you know what to compute, nor how
> to interpret the result. It does not offer to explain the assumptions
> behind methods, nor does it flag delicate or dubious assumptions. It
> does not warn you about multiplicity or p-hacking. It does not check
> whether you picked the hypothesis or analysis after looking at the
> data, nor track the number of analyses you tried before arriving at
> the one you sought to publish – another form of multiplicity. The more
> “powerful” and “user-friendly” the software is, the more it invites
> cargo-cult statistics*." - Stark & Saltelli, 2018

# References

1.  Chow ZR, Greenland S. Semantic and Cognitive Tools to Aid
    Statistical Inference: Replace Confidence and Significance by
    Compatibility and Surprise. [*arXiv:1909.08579
    \[stat.ME\]*.](https://arxiv.org/abs/1909.08579) 2019
2.  Greenland S, Chow ZR. To Aid Statistical Inference, Emphasize
    Unconditional Descriptions of Statistics. [*arXiv:1909.08583
    \[stat.ME\]*.](https://arxiv.org/abs/1909.08583) 2019
3.  Poole C. Beyond the confidence interval. *Am J Public Health.*
    1987;77(2):195-199.
4.  Sullivan KM, Foster DA. Use of the confidence interval
    function.\_Epidemiology.\_ 1990;1(1):39-42.
5.  Rothman KJ, Greenland S, Lash TL. Modern epidemiology. 2012.
6.  Singh K, Xie M, Strawderman WE. Confidence distribution (CD) –
    distribution estimator of a parameter. *arXiv \[mathST\]*. 2007.
7.  Schweder T, Hjort NL. Confidence and Likelihood\*. *Scand J Stat.*
    2002;29(2):309-332.
8.  Amrhein V, Trafimow D, Greenland S. Inferential Statistics as
    Descriptive Statistics: There is No Replication Crisis if We Don’t
    Expect Replication. *Am Stat*. 2019
9.  Greenland S. Valid P-values Behave Exactly As They Should. Some
    misleading criticisms of P-values and their resolution with
    S-values. *Am Stat*. 2019;18(136).
10. Fraser DAS. The p-value Function and Statistical Inference. *Am
    Stat*. 2019
11. Stark PB, Saltelli A. Cargo-cult statistics and scientific crisis.
    *Significance.* 2018;15(4):40-43.

# Session info

    ## R version 3.6.1 (2019-07-05)
    ## Platform: x86_64-apple-darwin15.6.0 (64-bit)
    ## Running under: macOS Catalina 10.15.1
    ## 
    ## Matrix products: default
    ## BLAS:   /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRblas.0.dylib
    ## LAPACK: /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRlapack.dylib
    ## 
    ## locale:
    ## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     methods   base     
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] compiler_3.6.1  magrittr_1.5    tools_3.6.1     htmltools_0.4.0 yaml_2.2.0      Rcpp_1.0.3      stringi_1.4.3  
    ##  [8] rmarkdown_1.18  knitr_1.26      stringr_1.4.0   xfun_0.11       digest_0.6.23   rlang_0.4.2     evaluate_0.14
