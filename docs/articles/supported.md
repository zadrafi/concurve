# Supported Versions

![](https://res.cloudinary.com/less-likely/image/upload/v1575441662/Site/Logo2.jpg)

## Staying Up to Date

------------------------------------------------------------------------

This page is a summary of which versions of
[`concurve`](https://cran.r-project.org/package=concurve) are currently
supported and which we will provide support with depending on the
context. We will mostly be supporting the most recent versions of the
package, as these versions will also be using the latest version of `R`
and other dependencies that are necessary. As such, those who continue
to use older versions of the package may be out of the loop on new
changes in arguments to functions, along with new functions. We urge
users to consult and stay up to date with the documentation, and update
their `R` packages as frequently as possible. An easy way to remain
updated is to visit the [package
website](https://data.lesslikely.com/concurve/), as the newest version
of the package number will always be on the top left.

## Previous Versions

However, we’d still like users to be able to access old versions of
concurve for whatever reason, and in addition to using GitHub, those can
also be found on the [CRAN
archive](https://cloud.r-project.org/src/contrib/Archive/concurve/).

Another useful link will be the
[NEWS](https://data.lesslikely.com/concurve/news/index.html) section,
which will inform users of bugs that have been patched and new changes
to old functions. However, checking `CRAN` is less than desirable, due
to the fact that new changes (that may even be very important), will
often quickly be pushed to `GitHub`, rather than `CRAN`. Thus,
installing or upgrading to the newest version of `concurve` via the
script,

``` r

remotes::install_github("zadrafi/concurve@master", dependencies = TRUE)
```

may be useful.

You can also directly download a tarball or zip of the most recent
package version using the [following
link](https://github.com/zadrafi/concurve/releases).

------------------------------------------------------------------------

## Supported Versions

------------------------------------------------------------------------

[TABLE]

------------------------------------------------------------------------

## What’s New in Version 3.0

------------------------------------------------------------------------

Version 3.0 introduces several significant updates:

### New Functions

- **[`curve_wrap()`](reference/curve_wrap.md)** - Generic wrapper for
  any CI-producing function
- **[`curve_from_ratio()`](reference/curve_from_ratio.md)** - Construct
  curves from ratio estimates
- **[`curve_from_se()`](reference/curve_from_se.md)** - Construct curves
  from standard error estimates
- **[`curve_overlap()`](reference/curve_overlap.md)** - Calculate
  overlap between consonance functions
- **[`curve_summary()`](reference/curve_summary.md)** - Generate summary
  statistics for consonance objects
- **[`plot_multi()`](reference/plot_multi.md)** - Plot multiple
  consonance functions simultaneously

### Dependency Changes

- **Removed `pbmcapply`** - Replaced with base R
  [`parallel::mclapply()`](https://rdrr.io/r/parallel/mclapply.html) for
  parallel processing, reducing external dependencies
- **Optional Snowflake support** - `DBI` and `odbc` packages moved to
  Suggests for optional database connectivity

### Bug Fixes

- Fixed scoping issues with
  [`subset()`](https://rdrr.io/r/base/subset.html) inside parallel
  operations
- Improved compatibility with newer versions of R and tidyverse packages

------------------------------------------------------------------------

## Dependencies

------------------------------------------------------------------------

The package constructs interval estimates using a wide variety of
functions that already exist within the R ecosystem, whether that is
from base R or from community-contributed packages that are on CRAN.
Here is a list of those packages. Those that are necessary to produce
distributions of parameters are specified as `Imports`, while those that
are optional and not necessary are specified as `Suggests`.

Those that are listed in the Suggests section are typically used for
additional functionality such as database connectivity (Snowflake) or
testing.

------------------------------------------------------------------------

### Package Imports

------------------------------------------------------------------------

|                   | Package           | Package Version |
|:------------------|:------------------|:----------------|
| methods           | methods           | 4.5.2           |
| bcaboot           | bcaboot           | 0.2-3           |
| boot              | boot              | 1.3-32          |
| lme4              | lme4              | 1.1-38          |
| dplyr             | dplyr             | 1.1.4           |
| flextable         | flextable         | 0.9.10          |
| ggplot2           | ggplot2           | 4.0.1           |
| knitr             | knitr             | 1.51            |
| metafor           | metafor           | 4.8-0           |
| officer           | officer           | 0.7.3           |
| parallel          | parallel          | 4.5.2           |
| ProfileLikelihood | ProfileLikelihood | 1.3             |
| scales            | scales            | 1.4.0           |
| colorspace        | colorspace        | 2.1-2           |
| survival          | survival          | 3.8-6           |
| survminer         | survminer         | 0.5.1           |
| tibble            | tibble            | 3.3.1           |
| tidyr             | tidyr             | 1.3.2           |
| rlang             | rlang             | 1.1.7           |

Package Imports {.table .striped .table .table-striped
style="font-size: 10px; font-family: helvetica; margin-left: auto; margin-right: auto;"}

------------------------------------------------------------------------

### Package Suggestions

------------------------------------------------------------------------

|            | Package    | Package Version |
|:-----------|:-----------|:----------------|
| DBI        | DBI        | 1.2.3           |
| odbc       | odbc       | 1.6.4.1         |
| covr       | covr       | 3.6.5           |
| roxygen2   | roxygen2   | 7.3.3           |
| spelling   | spelling   | 2.3.2           |
| testthat   | testthat   | 3.3.2           |
| rmarkdown  | rmarkdown  | 2.30            |
| Lock5Data  | Lock5Data  | 4.0.1           |
| carData    | carData    | 3.0-5           |
| bench      | bench      | 1.1.4           |
| rms        | rms        | 8.1-0           |
| brms       | brms       | 2.23.0          |
| rstanarm   | rstanarm   | 2.32.2          |
| bayesplot  | bayesplot  | 1.15.0          |
| vdiffr     | vdiffr     | 1.0.8           |
| ggtext     | ggtext     | 0.1.2           |
| daewr      | daewr      | 1.2-11          |
| svglite    | svglite    | 2.2.2           |
| data.table | data.table | 1.18.0          |
| nlme       | nlme       | 3.1-168         |
| simstudy   | simstudy   | 0.9.1           |
| MASS       | MASS       | 7.3-65          |

Package Suggestions {.table .striped .table .table-striped
style="font-size: 10px; font-family: helvetica; margin-left: auto; margin-right: auto;"}

------------------------------------------------------------------------

## Reporting Bugs, Vulnerabilities, and Issues

------------------------------------------------------------------------

Please [see
here](https://data.lesslikely.com/concurve/CONTRIBUTING.html) to report
any vulnerabilities within the package, and to follow best practices for
reporting any bugs, issues, or concerns. When submitting an issue,
please try to submit a REPREX, so that the developers may quickly see
the issue.

------------------------------------------------------------------------

## Session info

    #> R version 4.5.2 (2025-10-31)
    #> Platform: aarch64-apple-darwin20
    #> Running under: macOS Tahoe 26.3
    #> 
    #> Matrix products: default
    #> BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
    #> LAPACK: /Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.1
    #> 
    #> locale:
    #> [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    #> 
    #> time zone: America/New_York
    #> tzcode source: internal
    #> 
    #> attached base packages:
    #> [1] stats     graphics  grDevices utils     datasets  methods   base     
    #> 
    #> other attached packages:
    #> [1] magick_2.9.0     kableExtra_1.4.0 magrittr_2.0.4   knitr_1.51      
    #> 
    #> loaded via a namespace (and not attached):
    #>  [1] vctrs_0.7.1        svglite_2.2.2      cli_3.6.5          rlang_1.1.7       
    #>  [5] xfun_0.56          stringi_1.8.7      otel_0.2.0         textshaping_1.0.4 
    #>  [9] jsonlite_2.0.0     glue_1.8.0         htmltools_0.5.9    ragg_1.5.0        
    #> [13] sass_0.4.10        scales_1.4.0       rmarkdown_2.30     evaluate_1.0.5    
    #> [17] jquerylib_0.1.4    fastmap_1.2.0      yaml_2.3.12        lifecycle_1.0.5   
    #> [21] stringr_1.6.0      compiler_4.5.2     RColorBrewer_1.1-3 fs_1.6.6          
    #> [25] Rcpp_1.1.1         htmlwidgets_1.6.4  rstudioapi_0.18.0  farver_2.1.2      
    #> [29] systemfonts_1.3.1  digest_0.6.39      viridisLite_0.4.2  R6_2.6.1          
    #> [33] dichromat_2.0-0.1  bslib_0.9.0        tools_4.5.2        xml2_1.5.2        
    #> [37] pkgdown_2.2.0      cachem_1.1.0       desc_1.4.3
