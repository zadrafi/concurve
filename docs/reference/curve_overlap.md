# Compute Overlap Statistics Between Two Consonance Functions

Calculates the area under the curve (AUC) overlap between two consonance
or surprisal functions, providing a quantitative measure of how similar
two estimates are.

## Usage

``` r
curve_overlap(curve1, curve2, type = "c")
```

## Arguments

- curve1:

  A concurve dataframe (first curve).

- curve2:

  A concurve dataframe (second curve).

- type:

  Character. Type of function: "c" for consonance (default), "s" for
  surprisal.

## Value

A list containing:

- auc1: Area under curve 1

- auc2: Area under curve 2

- auc_shared: Shared (overlapping) area

- overlap_pct: Overlap as percentage (Jaccard-style)

- overlap_ratio: Ratio of shared to non-shared area

- interpretation: Plain-language interpretation

## See also

[`curve_compare()`](reference/curve_compare.md),
[`plot_multi()`](reference/plot_multi.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Compare two study results
study1 <- curve_from_se(point = 0.5, se = 0.15, df = 100)
study2 <- curve_from_se(point = 0.7, se = 0.18, df = 80)

overlap <- curve_overlap(study1[[1]], study2[[1]])
print(overlap)
# $overlap_pct: 68.2
# $interpretation: "Moderate overlap - effects may differ"
} # }
```
