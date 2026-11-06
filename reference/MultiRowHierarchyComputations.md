# MultiRowHierarchyComputations

Showing computations behind the scenes in HierarchyCompute

## Usage

``` r
MultiRowHierarchyComputations(
  a,
  drop0 = TRUE,
  valueName = "values",
  indName = "ind",
  ind = NULL
)
```

## Arguments

- a:

  a

- drop0:

  see code

- valueName:

  Parameter to Stack

- indName:

  Parameter to Stack

- ind:

  see code

- doStack:

  Stacked output when TRUE

## Value

Data frame where the "weights in weighted sum" is named as sign. Firs
row (before stacking) contains the "results".

## Examples

``` r
# Data and hierarchies used in the examples
x <- SSBtoolsData("sprt_emp")  # Employment in sport in thousand persons from Eurostat database
geoHier <- SSBtoolsData("sprt_emp_geoHier")
ageHier <- SSBtoolsData("sprt_emp_ageHier")

# Create rowSelect
rowSelect = data.frame(geo = "Europe", age = c("Y15-64", "Y15-29"), year="2015")

# Create input
a <- HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "rowFactor"), "ths_per", rowSelect = rowSelect, output = "matrixComponents")

MultiRowHierarchyComputations(a)
#>       age      geo year sign values                ind
#> 1  Y15-64   Europe 2015    0  225.0 Europe.Y15-64.2015
#> 2  Y15-29    Spain 2015    1   63.4 Europe.Y15-64.2015
#> 3  Y15-29  Iceland 2015    1    1.9 Europe.Y15-64.2015
#> 4  Y15-29 Portugal 2015    1   14.2 Europe.Y15-64.2015
#> 5  Y30-64    Spain 2015    1  119.6 Europe.Y15-64.2015
#> 6  Y30-64  Iceland 2015    1    1.6 Europe.Y15-64.2015
#> 7  Y30-64 Portugal 2015    1   24.3 Europe.Y15-64.2015
#> 8  Y15-29   Europe 2015    0   79.5 Europe.Y15-29.2015
#> 9  Y15-29    Spain 2015    1   63.4 Europe.Y15-29.2015
#> 10 Y15-29  Iceland 2015    1    1.9 Europe.Y15-29.2015
#> 11 Y15-29 Portugal 2015    1   14.2 Europe.Y15-29.2015
```
