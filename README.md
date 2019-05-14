concurve
================

# Plot Interval Functions With the concurve Package <img src="man/figures/logo.svg" align="right" width="100" />

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/concurve)](https://cran.r-project.org/package=concurve)
[![Build
Status](https://travis-ci.org/Zadchow/concurve.svg?branch=master)](https://travis-ci.org/Zadchow/concurve)
[![Build
status](https://ci.appveyor.com/api/projects/status/v8sp9x96dap2om9s?svg=true)](https://ci.appveyor.com/project/Zadchow/concurve)
[![DOI](https://zenodo.org/badge/165464881.svg)](https://zenodo.org/badge/latestdoi/165464881)
[![](https://cranlogs.r-pkg.org/badges/grand-total/concurve)](https://cran.r-project.org/package=concurve)
[![Rdoc](http://www.rdocumentation.org/badges/version/concurve)](http://www.rdocumentation.org/packages/concurve)

## A Single Interval Isn’t Enough

Interval estimates such as ~~confidence~~ **consonance** intervals are
now widely reported in many journals alongside the exact P-value of a
statistical test and point estimate. While this is a large improvement
over what constituted statistical reporting in the past two decades, it
is still largely inadequate.

Take for example, the 95% consonance interval. As many have stated
before, there is nothing special about **95%**, yet we rarely see
intervals of any other level. Choosing to compute a 95% interval is as
mindless as choosing a 5% alpha level for hypothesis testing.

A single consonance interval is only a *slice* of a wide range of
consonance intervals at different levels. Mindlessly reporting 95%
intervals only promotes [cargo-cult
statistics](https://rss.onlinelibrary.wiley.com/doi/10.1111/j.1740-9713.2018.01174.x)
since there is not much thought behind the choice. (1)

> “***Cargo-cult statistics*** – *the ritualistic miming of statistics
> rather than conscientious practice*.” - Stark & Saltelli, 2018

Thus, we propose that instead of only calculating ***one*** interval
estimate, ***every*** interval associated with a consonance level be
calculated, along with its corresponding P-value and S-value, and
plotted to form a function.
(2-8)

<img src="https://res.cloudinary.com/less-likely/image/upload/v1557366230/Site/FrontConsonance.svg" align="center" width="400" />
<img src="https://res.cloudinary.com/less-likely/image/upload/v1557366230/Site/FrontSurprisal.svg" align="center" width="400" />

This can be accomplished using the
[**concurve**](https://github.com/Zadchow/concurve)</span> package in R
and on Stata.

-----

# For R:

## Install the Package From CRAN

``` r
install.packages("concurve")
```

## Install the Developer Version

``` r
library(devtools)
install_github("zadchow/concurve")
```

## Check out the [Examples](https://data.lesslikely.com/concurve/articles/examples.html).

-----

# For Stata:

## Check out the [Article on Using Stata](https://data.lesslikely.com/concurve/articles/stata.html) for concurve.

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
