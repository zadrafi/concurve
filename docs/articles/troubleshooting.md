# Troubleshooting

![](https://res.cloudinary.com/less-likely/image/upload/v1575441662/Site/Logo2.jpg)

## Installation

If you encounter any issues with installation, try the following script
to see if it resolves your issue, as this will also install the other
packages that are necessary for `concurve` to function.

``` r

install.packages("concurve", dep = TRUE)
```

If that doesn’t work, please try installing, and resinstalling `R`, and
then installing the package again.

You can also try installing the developer version with

``` r

library(devtools)
install_github("zadrafi/concurve")
```

## Unable to Plot Function

If you encounter an error such as “Error: ‘data’ must be a data frame
from ‘concurve’.”, it is very likely that you are not providing
[`ggcurve()`](reference/ggcurve.md) the correct argument. If you used a
function like `curve_gen()` to generate intervals and saved it to an
object called ‘object’, you need to provide
[`ggcurve()`](reference/ggcurve.md) a data argument such as
`object[[1]]` rather than `object` or `object[1]`. This is because
although you saved your results to something called `object`, you ended
up with a list with multiple components used for different purposes,
usually with the first part of the list being the most commonly used
part.

We can actually see these components one by one. Let’s generate some
example data.

``` r

library(concurve)
set.seed(1031)
GroupA <- rnorm(500)
GroupB <- rnorm(500)
RandomData <- data.frame(GroupA, GroupB)
object <- curve_mean(GroupA, GroupB,
  data = RandomData, method = "default"
)
```

As stated, the first part of the list `object[[1]]` contains what we
usually want (I’m restricting to the first 5 results using the
[`head()`](https://rdrr.io/r/utils/head.html) function so that we don’t
print a giant list with 1000 rows.)

``` r

head(object[[1]], 5)
#>   lower.limit upper.limit intrvl.width intrvl.level     cdf pvalue       svalue
#> 1  -0.1125581  -0.1125581 0.000000e+00        0e+00 0.50000 1.0000 0.0000000000
#> 2  -0.1125658  -0.1125504 1.543412e-05        1e-04 0.50005 0.9999 0.0001442767
#> 3  -0.1125736  -0.1125427 3.086824e-05        2e-04 0.50010 0.9998 0.0002885679
#> 4  -0.1125813  -0.1125350 4.630236e-05        3e-04 0.50015 0.9997 0.0004328734
#> 5  -0.1125890  -0.1125273 6.173649e-05        4e-04 0.50020 0.9996 0.0005771935
```

while the second and third parts of the list contain dataframes and
lists for other functions such as generating density functions or for
other functions such as [`curve_table()`](reference/curve_table.md).

``` r

head(object[[2]], 5)
#>            x
#> 1 -0.1125581
#> 2 -0.1125658
#> 3 -0.1125736
#> 4 -0.1125813
#> 5 -0.1125890
```

``` r

head(object[[3]], 5)
#>      Lower Limit Upper Limit Interval Width Interval Level (%)   CDF P-value
#> 2501      -0.132      -0.093          0.039                 25 0.625    0.75
#> 5001      -0.154      -0.071          0.083                 50 0.750    0.50
#> 7501      -0.183      -0.042          0.142                 75 0.875    0.25
#> 8001      -0.192      -0.034          0.158                 80 0.900    0.20
#> 8501      -0.201      -0.024          0.177                 85 0.925    0.15
#>      S-value (bits)
#> 2501          0.415
#> 5001          1.000
#> 7501          2.000
#> 8001          2.322
#> 8501          2.737
```

## Distorted Plots

If you encounter issues when plotting the functions, it is because there
are a large number of points being plotted, which could lead to the
graph being slightly distorted or not loading at all. The simplest
solution to this is to refresh the plot and try the function again.

This applies to the [`ggcurve()`](reference/ggcurve.md),
[`curve_compare()`](reference/curve_compare.md), and
[`plot_compare()`](reference/plot_compare.md) functions.

I would also recommend saving plots using the
[`cowplot::save_plot()`](https://wilkelab.org/cowplot/reference/save_plot.html)
function with the actual [`ggcurve()`](reference/ggcurve.md) object. It
has better default settings than the
[`ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)
function.

## Slow Performance

Because this package is computing thousands of interval estimates via
iterations and bootstrapping, it requires a lot of computational power.
Luckily, `concurve` supports parallelization, although it is disabled by
default because some users, such as those who use Windows, are unable to
use it.

However, if you are able to use parallelization, you can enable it with
the following script

``` r

library(parallel)
options(mc.cores = 1)
```

The script will detect the number of cores on your machine via the
`parallel` package and use them to speed up the computations, especially
for bootstrapping.

However, if you would like to speed up the computations and are unable
to use parallelization, then you can reduce the number of `steps` in the
each of the `concurve` functions, which will drastically reduce the time
it takes to complete the operation. By default, most of the `steps`
arguments are set to 10000.

For example, here I changed the number of steps to 100, which is the
minimum needed to plot a function, and the process is now much quicker.
We can evaluate this using a microbenchmark. Here I use the `bench`
package and the [`mark()`](https://bench.r-lib.org/reference/mark.html)
function. Because we are using parallelization, we must also set the
`memory` argument to `FALSE`.

``` r

library(bench)
library(parallel)
options(mc.cores = 1)
getOption("mc.cores", 1L)
set.seed(1031)
func1 <- mark(df1 <- curve_rev(
  point = 1.61, LL = 0.997, UL = 2.59,
  measure = "ratio", steps = 100
), memory = FALSE)

func2 <- mark(df1 <- curve_rev(
  point = 1.61, LL = 0.997, UL = 2.59,
  measure = "ratio", steps = 500000
), memory = FALSE)
#> Warning: Some expressions had a GC in every iteration; so filtering is
#> disabled.
```

I’ll now enable parallelization by setting the `mc.cores` option to
detect the max number of cores available using
[`detectCores()`](https://rdrr.io/r/parallel/detectCores.html). The
`mc.cores` option is the argument that almost all the curve functions in
the `concurve` package use for parallel computing.

``` r


getOption("mc.cores", 1L)
set.seed(1031)
func3 <- mark(df1 <- curve_rev(
  point = 1.61, LL = 0.997, UL = 2.59,
  measure = "ratio", steps = 100
), memory = FALSE)

func4 <- mark(df1 <- curve_rev(
  point = 1.61, LL = 0.997, UL = 2.59,
  measure = "ratio", steps = 500000
), memory = FALSE)
#> Warning: Some expressions had a GC in every iteration; so filtering is
#> disabled.
```

``` r

func1$median
#> [1] 748µs
func2$median
#> [1] 1.35s
func3$median
#> [1] 788µs
func4$median
#> [1] 980ms
```

When setting the number of iterations to 100, utilizing parallelization
doesn’t seem to help much, but when setting the number of iterations to
500000 and using multiple cores, there seems to be a computational
advantage.

## Bugs

If you encounter any other bugs, please report them at
<https://github.com/zadrafi/concurve/issues>

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
    #> [1] parallel  stats     graphics  grDevices utils     datasets  methods  
    #> [8] base     
    #> 
    #> other attached packages:
    #> [1] bench_1.1.4    concurve_3.0.0
    #> 
    #> loaded via a namespace (and not attached):
    #>  [1] tidyselect_1.2.1        dplyr_1.1.4             farver_2.1.2           
    #>  [4] S7_0.2.1                fastmap_1.2.0           fontquiver_0.2.1       
    #>  [7] mathjaxr_2.0-0          digest_0.6.39           lifecycle_1.0.5        
    #> [10] survival_3.8-6          magrittr_2.0.4          compiler_4.5.2         
    #> [13] rlang_1.1.7             sass_0.4.10             tools_4.5.2            
    #> [16] yaml_2.3.12             data.table_1.18.0       knitr_1.51             
    #> [19] ggsignif_0.6.4          askpass_1.2.1           htmlwidgets_1.6.4      
    #> [22] xml2_1.5.2              RColorBrewer_1.1-3      abind_1.4-8            
    #> [25] purrr_1.2.1             numDeriv_2016.8-1.1     desc_1.4.3             
    #> [28] bcaboot_0.2-3           grid_4.5.2              ggpubr_0.6.2           
    #> [31] gdtools_0.4.4           xtable_1.8-4            colorspace_2.1-2       
    #> [34] ggplot2_4.0.1           scales_1.4.0            MASS_7.3-65            
    #> [37] dichromat_2.0-0.1       cli_3.6.5               rmarkdown_2.30         
    #> [40] metafor_4.8-0           ragg_1.5.0              generics_0.1.4         
    #> [43] otel_0.2.0              rstudioapi_0.18.0       km.ci_0.5-6            
    #> [46] survminer_0.5.1         cachem_1.1.0            splines_4.5.2          
    #> [49] metadat_1.4-0           survMisc_0.5.6          vctrs_0.7.1            
    #> [52] boot_1.3-32             Matrix_1.7-4            jsonlite_2.0.0         
    #> [55] fontBitstreamVera_0.1.1 carData_3.0-5           car_3.1-3              
    #> [58] rstatix_0.7.3           Formula_1.2-5           systemfonts_1.3.1      
    #> [61] tidyr_1.3.2             jquerylib_0.1.4         glue_1.8.0             
    #> [64] pkgdown_2.2.0           flextable_0.9.10        gtable_0.3.6           
    #> [67] tibble_3.3.1            pillar_1.11.1           htmltools_0.5.9        
    #> [70] openssl_2.3.4           ProfileLikelihood_1.3   R6_2.6.1               
    #> [73] KMsurv_0.1-6            textshaping_1.0.4       evaluate_1.0.5         
    #> [76] lattice_0.22-7          backports_1.5.0         broom_1.0.11           
    #> [79] fontLiberation_0.1.0    bslib_0.9.0             Rcpp_1.1.1             
    #> [82] zip_2.3.3               uuid_1.2-2              gridExtra_2.3          
    #> [85] nlme_3.1-168            officer_0.7.3           xfun_0.56              
    #> [88] fs_1.6.6                zoo_1.8-15              pkgconfig_2.0.3
