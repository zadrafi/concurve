# Generating Tables

It is quite easy to generate tables of outputs for the various
`concurve` functions and in different formats. Here we show how to do
this with a simple example. First, we’ll simulate some fake data as
usual, compare the means, and then produce a confidence function of the
outputs.

``` r

library(concurve)
#> Please see the documentation on https://data.lesslikely.com/concurve/ or by typing `help(concurve)`
GroupA <- rnorm(500)
GroupB <- rnorm(500)
RandomData <- data.frame(GroupA, GroupB)

intervalsdf <- curve_mean(GroupA, GroupB,
  data = RandomData, method = "default"
)
```

The results are now stored in `intervalsdf`. Using the
[`curve_table()`](reference/curve_table.md) function, we can now produce
a high quality table with several values of interest.

``` r

(x <- curve_table(data = intervalsdf[[1]], format = "image"))
```

| Lower Limit | Upper Limit | Interval Width | Interval Level (%) | CDF | P-value | S-value (bits) |
|----|----|----|----|----|----|----|
| 0.038 | 0.079 | 0.040 | 25.0 | 0.625 | 0.750 | 0.415 |
| 0.016 | 0.101 | 0.085 | 50.0 | 0.750 | 0.500 | 1.000 |
| -0.014 | 0.131 | 0.146 | 75.0 | 0.875 | 0.250 | 2.000 |
| -0.023 | 0.139 | 0.162 | 80.0 | 0.900 | 0.200 | 2.322 |
| -0.033 | 0.149 | 0.182 | 85.0 | 0.925 | 0.150 | 2.737 |
| -0.046 | 0.163 | 0.208 | 90.0 | 0.950 | 0.100 | 3.322 |
| -0.066 | 0.182 | 0.248 | 95.0 | 0.975 | 0.050 | 4.322 |
| -0.084 | 0.200 | 0.284 | 97.5 | 0.988 | 0.025 | 5.322 |
| -0.105 | 0.222 | 0.326 | 99.0 | 0.995 | 0.010 | 6.644 |

Here we specified the format as “image”, which will give us just that.
We can also specify other options such as

``` r

(z <- curve_table(intervalsdf[[1]], format = "latex"))
```

|  | Lower Limit | Upper Limit | Interval Width | Interval Level (%) | CDF | P-value | S-value (bits) |
|:---|---:|---:|---:|---:|---:|---:|---:|
| 2501 | 0.038 | 0.079 | 0.040 | 25.0 | 0.625 | 0.750 | 0.415 |
| 5001 | 0.016 | 0.101 | 0.085 | 50.0 | 0.750 | 0.500 | 1.000 |
| 7501 | -0.014 | 0.131 | 0.146 | 75.0 | 0.875 | 0.250 | 2.000 |
| 8001 | -0.023 | 0.139 | 0.162 | 80.0 | 0.900 | 0.200 | 2.322 |
| 8501 | -0.033 | 0.149 | 0.182 | 85.0 | 0.925 | 0.150 | 2.737 |
| 9001 | -0.046 | 0.163 | 0.208 | 90.0 | 0.950 | 0.100 | 3.322 |
| 9501 | -0.066 | 0.182 | 0.248 | 95.0 | 0.975 | 0.050 | 4.322 |
| 9751 | -0.084 | 0.200 | 0.284 | 97.5 | 0.988 | 0.025 | 5.322 |
| 9901 | -0.105 | 0.222 | 0.326 | 99.0 | 0.995 | 0.010 | 6.644 |

which is useful for inserting the output into a TeX document, and we can
also specify options such as

``` r

(df <- curve_table(intervalsdf[[1]], format = "data.frame"))
#>      Lower Limit Upper Limit Interval Width Interval Level (%)   CDF P-value
#> 2501       0.038       0.079          0.040               25.0 0.625   0.750
#> 5001       0.016       0.101          0.085               50.0 0.750   0.500
#> 7501      -0.014       0.131          0.146               75.0 0.875   0.250
#> 8001      -0.023       0.139          0.162               80.0 0.900   0.200
#> 8501      -0.033       0.149          0.182               85.0 0.925   0.150
#> 9001      -0.046       0.163          0.208               90.0 0.950   0.100
#> 9501      -0.066       0.182          0.248               95.0 0.975   0.050
#> 9751      -0.084       0.200          0.284               97.5 0.988   0.025
#> 9901      -0.105       0.222          0.326               99.0 0.995   0.010
#>      S-value (bits)
#> 2501          0.415
#> 5001          1.000
#> 7501          2.000
#> 8001          2.322
#> 8501          2.737
#> 9001          3.322
#> 9501          4.322
#> 9751          5.322
#> 9901          6.644
```

The options “pptx” and “docx” can also be specified as format options,
but specifying these will open those programs if they are installed,
which may not be ideal for all because no everyone has access.

## Cite R Packages

Please remember to cite the packages that you use.

``` r

citation("concurve")
#> To cite package 'concurve' in publications use:
#> 
#>   Rafi Z, Vigotsky A (????). _concurve: Computes and Plots
#>   Compatibility (Confidence) Intervals, P-Values, S-Values, &
#>   Likelihood Intervals to Form Consonance, Surprisal, & Likelihood
#>   Functions_. R package version 3.0,
#>   <https://CRAN.R-project.org/package=concurve>.
#> 
#>   Rafi Z, Greenland S (2020). "Semantic and Cognitive Tools to Aid
#>   Statistical Science: Replace Confidence and Significance by
#>   Compatibility and Surprise." _BMC Medical Research Methodology_,
#>   *20*, 244. ISSN 1471-2288, doi:10.1186/s12874-020-01105-9
#>   <https://doi.org/10.1186/s12874-020-01105-9>,
#>   <https://doi.org/10.1186/s12874-020-01105-9>.
#> 
#> To see these entries in BibTeX format, use 'print(<citation>,
#> bibtex=TRUE)', 'toBibtex(.)', or set
#> 'options(citation.bibtex.max=999)'.
citation("flextable")
#> To cite package 'flextable' in publications use:
#> 
#>   Gohel D, Skintzos P (2025). _flextable: Functions for Tabular
#>   Reporting_. doi:10.32614/CRAN.package.flextable
#>   <https://doi.org/10.32614/CRAN.package.flextable>, R package version
#>   0.9.10, <https://CRAN.R-project.org/package=flextable>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {flextable: Functions for Tabular Reporting},
#>     author = {David Gohel and Panagiotis Skintzos},
#>     year = {2025},
#>     note = {R package version 0.9.10},
#>     url = {https://CRAN.R-project.org/package=flextable},
#>     doi = {10.32614/CRAN.package.flextable},
#>   }
citation("officer")
#> To cite package 'officer' in publications use:
#> 
#>   Gohel D, Moog S, Heckmann M (2026). _officer: Manipulation of
#>   Microsoft Word and PowerPoint Documents_.
#>   doi:10.32614/CRAN.package.officer
#>   <https://doi.org/10.32614/CRAN.package.officer>, R package version
#>   0.7.3, <https://CRAN.R-project.org/package=officer>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {officer: Manipulation of Microsoft Word and PowerPoint Documents},
#>     author = {David Gohel and Stefan Moog and Mark Heckmann},
#>     year = {2026},
#>     note = {R package version 0.7.3},
#>     url = {https://CRAN.R-project.org/package=officer},
#>     doi = {10.32614/CRAN.package.officer},
#>   }
```

------------------------------------------------------------------------

## References

------------------------------------------------------------------------
