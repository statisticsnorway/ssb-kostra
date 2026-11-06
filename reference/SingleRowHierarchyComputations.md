# SingleRowHierarchyComputations

Showing computations behind the scenes in HierarchyCompute

## Usage

``` r
SingleRowHierarchyComputations(
  a,
  doStack = FALSE,
  valueName = "values",
  indName = "ind"
)
```

## Arguments

- a:

  a

- valueName:

  Parameter to Stack

- indName:

  Parameter to Stack

## Value

Data frame where the "weights in weighted sum" is named as sign. Firs
row (before stacking) contains the "results".

## Examples

``` r
# Data and hierarchies used in the examples
x <- SSBtoolsData("sprt_emp")  # Employment in sport in thousand persons from Eurostat database
geoHier <- SSBtoolsData("sprt_emp_geoHier")
ageHier <- SSBtoolsData("sprt_emp_ageHier")

# Create rowSelect since a single row only
rowSelect <- data.frame(age = "Y15-64", geo = "Europe", stringsAsFactors = FALSE)

# Create input
a <- HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "colFactor"), "ths_per", rowSelect = rowSelect, output = "matrixComponents")


SingleRowHierarchyComputations(a)
#>      age      geo sign  2014  2015  2016
#> 1 Y15-64   Europe    0 222.3 225.0 233.5
#> 2 Y15-29  Iceland    1   1.8   1.9   1.9
#> 3 Y15-29 Portugal    1  11.6  14.2  12.7
#> 4 Y15-29    Spain    1  66.9  63.4  69.1
#> 5 Y30-64  Iceland    1   1.5   1.6   1.9
#> 6 Y30-64 Portugal    1  20.2  24.3  25.8
#> 7 Y30-64    Spain    1 120.3 119.6 122.1

SingleRowHierarchyComputations(a, TRUE)
#>       age      geo sign  ind values
#> 1  Y15-64   Europe    0 2014  222.3
#> 2  Y15-29  Iceland    1 2014    1.8
#> 3  Y15-29 Portugal    1 2014   11.6
#> 4  Y15-29    Spain    1 2014   66.9
#> 5  Y30-64  Iceland    1 2014    1.5
#> 6  Y30-64 Portugal    1 2014   20.2
#> 7  Y30-64    Spain    1 2014  120.3
#> 8  Y15-64   Europe    0 2015  225.0
#> 9  Y15-29  Iceland    1 2015    1.9
#> 10 Y15-29 Portugal    1 2015   14.2
#> 11 Y15-29    Spain    1 2015   63.4
#> 12 Y30-64  Iceland    1 2015    1.6
#> 13 Y30-64 Portugal    1 2015   24.3
#> 14 Y30-64    Spain    1 2015  119.6
#> 15 Y15-64   Europe    0 2016  233.5
#> 16 Y15-29  Iceland    1 2016    1.9
#> 17 Y15-29 Portugal    1 2016   12.7
#> 18 Y15-29    Spain    1 2016   69.1
#> 19 Y30-64  Iceland    1 2016    1.9
#> 20 Y30-64 Portugal    1 2016   25.8
#> 21 Y30-64    Spain    1 2016  122.1
```
