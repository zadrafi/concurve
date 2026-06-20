# Calculate Overlap Between Consonance Functions

Quantifies the area of overlap between two consonance functions,
providing a measure of compatibility between two estimates.

## Usage

``` r
curve_overlap(data1, data2, type = "c", plot = TRUE,
  title = "Consonance Function Overlap")
```

## Arguments

- data1:

  First concurve object or intervals data frame.

- data2:

  Second concurve object or intervals data frame.

- type:

  Function type for comparison: "c" for consonance (default), "s" for
  surprisal.

- plot:

  Logical. If TRUE (default), displays overlap visualization.

- title:

  Plot title.

## Value

A list containing:

- overlap_area:

  Estimated area of overlap

- total_area:

  Total combined area

- overlap_ratio:

  Ratio of overlap to total area

- max_shared_level:

  Maximum confidence level where intervals overlap

## Details

This function computes the overlap between two consonance functions,
which provides a measure of how compatible two estimates are with each
other. Higher overlap indicates greater compatibility.

The overlap is calculated by finding the intersection of intervals at
each confidence level and integrating over the shared region.

## See also

[`curve_compare()`](reference/curve_compare.md) for graphical comparison

[`plot_compare()`](reference/plot_compare.md) for visualization

## Examples

``` r
if (FALSE) { # \dontrun{
# Compare two study results
study1 <- curve_from_ratio(1.5, 1.1, 2.0)
study2 <- curve_from_ratio(1.3, 0.9, 1.8)

overlap <- curve_overlap(study1[[1]], study2[[1]])
print(overlap)
} # }
```
