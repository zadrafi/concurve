concurve
================

# concurve | Graph Interval Functions <img src="man/figures/logo.svg" align="right" width="120" />

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/concurve)](https://cran.r-project.org/package=concurve)
[![Build
Status](https://travis-ci.org/Zadchow/concurve.svg?branch=master)](https://travis-ci.org/Zadchow/concurve)
[![Build
status](https://ci.appveyor.com/api/projects/status/v8sp9x96dap2om9s?svg=true)](https://ci.appveyor.com/project/Zadchow/concurve)
[![DOI](https://zenodo.org/badge/165464881.svg)](https://zenodo.org/badge/latestdoi/165464881)
[![](https://cranlogs.r-pkg.org/badges/grand-total/concurve)](https://cran.r-project.org/package=concurve)
[![Rdoc](http://www.rdocumentation.org/badges/version/concurve)](http://www.rdocumentation.org/packages/concurve)

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
> From a scientific perspective, the likelihood function and p-value
> function provide the basis for scientific judgments by an
> investigator, and by other investigators who might have interest. **It
> thus replaces a blunt yes or no decision by an opportunity for
> appropriate informed judgment.**” - [D. A. S.
> Fraser, 2019](https://doi.org/10.1080/00031305.2018.1556735)

# Examples

<img src = "https://res.cloudinary.com/less-likely/image/upload/v1562705215/Site/Figure1.png" align="center" width ="400">
<img src = "https://res.cloudinary.com/less-likely/image/upload/v1562705215/Site/Figure2.png" align="center" width="400">

-----

# Installation

## For R:

### Install the Package From CRAN

``` r
install.packages("concurve")
```

### Install the Developer Version

``` r
library(devtools)
install_github("zadchow/concurve")
```

### Check out the [Examples](https://data.lesslikely.com/concurve/articles/examples.html).

-----

## For Stata:

### Check out the [Article on Using Stata](https://data.lesslikely.com/concurve/articles/stata.html) for concurve.

-----

# Dependencies

  - ggplot2
  - metafor
  - parallel
  - dplyr
  - tibble
  - survival
  - survminer
  - scales

-----

> "*Statistical software enables and promotes cargo-cult statistics.
> Marketing and adoption of statistical software are driven by ease of
> use and the range of statistical routines the software implements.
> Offering complex and “modern” methods provides a competitive
> advantage. And some disciplines have in effect standardised on
> particular statistical software, often proprietary software*.
> 
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

1.  Stark PB, Saltelli A. Cargo-cult statistics and scientific crisis.
    *Significance.* 2018;15(4):40-43.
2.  Poole C. Beyond the confidence interval. *Am J Public Health.*
    1987;77(2):195-199.
3.  Sullivan KM, Foster DA. Use of the confidence interval function.
    *Epidemiology.* 1990;1(1):39-42.
4.  Rothman KJ, Greenland S, Lash TL. Modern epidemiology. 2012.
5.  Singh K, Xie M, Strawderman WE. Confidence distribution (CD) –
    distribution estimator of a parameter. *arXiv \[mathST\]*. 2007.
6.  Schweder T, Hjort NL. Confidence and Likelihood\*. *Scand J Stat.*
    2002;29(2):309-332.
7.  Amrhein V, Trafimow D, Greenland S. Inferential Statistics as
    Descriptive Statistics: There is No Replication Crisis if We Don’t
    Expect Replication. *Am Stat*. 2019
8.  Greenland S. Valid P-values Behave Exactly As They Should. Some
    misleading criticisms of P-values and their resolution with
    S-values. *Am Stat*. 2019;18(136).
9.  Fraser DAS. The p-value Function and Statistical Inference. *Am
    Stat*. 2019
