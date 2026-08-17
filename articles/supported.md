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
concurve for whatever, reason, and in addition to using GitHub, those
can also be found on the [CRAN
archive](https://cloud.r-project.org/web/checks/check_results_concurve.html).
A page with the test results for the most recent package on CRAN can
also be [found
here](https://cloud.r-project.org/web/checks/check_results_concurve.html).

Another useful link will be the
[NEWS](https://data.lesslikely.com/concurve/news/index.html) section,
which will inform users of bugs that have been patched and new changes
to old functions. However, checking `CRAN` is less than desirable, due
to the fact that new changes (that may even be very important), will
often quickly be pushed to `GitHub`, rather than `CRAN`. Thus,
installing or upgrading to the newest version of `concurve` via the
script,

``` r

install_github("zadrafi/concurve@master", dependencies = TRUE)
```

may be useful.

You can also directly download a tarball or zip of the most recent
package version using the [following
link](https://github.com/zadrafi/concurve/releases).

------------------------------------------------------------------------

## Supported Versions

------------------------------------------------------------------------

------------------------------------------------------------------------

## What’s New in Version 3.0

------------------------------------------------------------------------

Version 3.0 is a major update that includes new functionality, reduced
dependencies, and improved compatibility.

### New Functions

- **[`curve_wrap()`](https://stat.lesslikely.com/concurve/reference/curve_wrap.md)** -
  Generic wrapper for any CI-producing function
- **[`curve_model()`](https://stat.lesslikely.com/concurve/reference/curve_model.md)** -
  Convenience wrapper for model objects
- **[`curve_snowflake()`](https://stat.lesslikely.com/concurve/reference/curve_snowflake.md)** -
  Construct consonance distributions directly from Snowflake database
  query results
- **[`curve_snowflake_batch()`](https://stat.lesslikely.com/concurve/reference/curve_snowflake_batch.md)** -
  Batch processing for multiple Snowflake analyses
- **[`export_for_powerbi()`](https://stat.lesslikely.com/concurve/reference/export_for_powerbi.md)** -
  Export consonance data in Power BI-ready format
- **[`curve_from_ratio()`](https://stat.lesslikely.com/concurve/reference/curve_from_ratio.md)** -
  Construct curves from ratio estimates
- **[`curve_from_se()`](https://stat.lesslikely.com/concurve/reference/curve_from_se.md)** -
  Construct curves from standard error estimates
- **[`curve_overlap()`](https://stat.lesslikely.com/concurve/reference/curve_overlap.md)** -
  Calculate overlap between consonance functions
- **[`curve_summary()`](https://stat.lesslikely.com/concurve/reference/curve_summary.md)** -
  Generate summary statistics for consonance objects
- **[`plot_multi()`](https://stat.lesslikely.com/concurve/reference/plot_multi.md)** -
  Plot multiple consonance functions simultaneously

### Dependency Changes

- **Removed `pbmcapply` dependency** - Replaced with base R
  [`parallel::mclapply()`](https://rdrr.io/r/parallel/mclapply.html) for
  parallel processing, reducing the package’s dependency footprint
- **Optional database dependencies** - `DBI` and `odbc` are now in
  Suggests for Snowflake integration (install only if needed)

### Bug Fixes and Improvements

- Fixed scoping issues with
  [`subset()`](https://rdrr.io/r/base/subset.html) inside parallel
  processing calls
- Improved roxygen2 documentation across all functions
- Enhanced error messages and input validation
- Updated vignettes with corrected examples

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
additional functionality such as database connectivity or testing.

------------------------------------------------------------------------

### Package Imports

------------------------------------------------------------------------

------------------------------------------------------------------------

### Package Suggestions

------------------------------------------------------------------------

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

    #> R version 4.6.1 (2026-06-24)
    #> Platform: x86_64-pc-linux-gnu
    #> Running under: Ubuntu 24.04.4 LTS
    #> 
    #> Matrix products: default
    #> BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
    #> LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
    #> 
    #> locale:
    #>  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
    #>  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
    #>  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
    #> [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
    #> 
    #> time zone: UTC
    #> tzcode source: system (glibc)
    #> 
    #> attached base packages:
    #> [1] stats     graphics  grDevices utils     datasets  methods   base     
    #> 
    #> other attached packages:
    #> [1] knitr_1.51
    #> 
    #> loaded via a namespace (and not attached):
    #>  [1] digest_0.6.39     desc_1.4.3        R6_2.6.1          fastmap_1.2.0    
    #>  [5] xfun_0.60         cachem_1.1.0      htmltools_0.5.9   rmarkdown_2.31   
    #>  [9] lifecycle_1.0.5   cli_3.6.6         sass_0.4.10       pkgdown_2.2.1    
    #> [13] textshaping_1.0.5 jquerylib_0.1.4   systemfonts_1.3.2 compiler_4.6.1   
    #> [17] tools_4.6.1       ragg_1.5.2        bslib_0.12.0      evaluate_1.0.5   
    #> [21] yaml_2.3.12       otel_0.2.0        jsonlite_2.0.0    rlang_1.3.0      
    #> [25] fs_2.1.0          htmlwidgets_1.6.4
