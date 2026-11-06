# Aggregated comparison of two numerical variables

Calculating aggregated values for two numerical variables, useful for
comparison of the variables

## Usage

``` r
AggrSml2NumVar(data, xVar, yVar, strataVar = NULL, identiske = FALSE)
```

## Arguments

- data:

  Input data set of class data.frame.

- xVar:

  Name of the x variable to be compared.

- yVar:

  Name of the y variable to be compared.

- strataVar:

  Name of stratification variable. Optional.

- identiske:

  When TRUE, only units with value on both x and y are used in the
  calculations. Default FALSE.

## Value

Output of AggrSml2NumVar is a data set of class data.frame. The
variables in the data frame are:

- strata:

  The stratum (if strataVar is given, "1" otherwise)

- Antx:

  The number of units with x not missing used in the aggregation

- Anty:

  The number of units with y not missing used in the aggregation

- Sumx:

  The total of x in the stratum

- Sumy:

  The total of y in the stratum

- SumxProsAvTotx:

  The stratum total for x in percent of the population total for x:
  (Sumx / Totx) \* 100

- SumyProsAvToty:

  The stratum total for y in percent of the population total for y:
  (Sumy / Toty) \* 100

- Diff:

  The difference between the stratum totales of x and y: Sumy - Sumx

- AbsDiff:

  The absolute difference: \|Diff\|

- DiffProsAvSumx:

  The difference in percent of the stratum total for x: (Diff / Sumx) \*
  100

- AbsDiffProsAvSumx:

  The absolute value of DiffProsAvSumx: \|DiffProsAvSumx\|

- DiffProsAvTotx:

  The difference in percent of the population total for x: (Diff / Totx)
  \* 100

- AbsDiffProsAvTotx:

  The absolute value of DiffProsAvTotx: \|DiffProsAvTotx\|

## Author

Anna Mevik

## Examples

``` r
testdata <- KostraData("testdata")

# lager en grupperingsvariabel
testdata$gr <- c(rep(3, 30), rep(5, 40), rep(1, 61), rep(2, 91), rep(3, 68), rep(4, 61),
                 rep(5, 45), rep(4, 20))

AggrSml2NumVar(data = testdata, xVar = "areal_130_eier_2014", yVar = "areal_130_eier_2015",
               strataVar = "gr", identiske = FALSE)
#>   strata Antx Anty   Sumx   Sumy SumxProsAvTotx SumyProsAvToty   Diff AbsDiff
#> 1      1   60   58 265113 277433       15.65571       16.46214  12320   12320
#> 2      2   88   83 409114 405354       24.15940       24.05263  -3760    3760
#> 3      3   94   90 379841 359889       22.43074       21.35486 -19952   19952
#> 4      4   80   76 281728 284055       16.63687       16.85507   2327    2327
#> 5      5   82   79 357599 358548       21.11728       21.27529    949     949
#>   DiffProsAvSumx AbsDiffProsAvSumx DiffProsAvTotx AbsDiffProsAvTotx
#> 1      4.6470750         4.6470750     0.72753256        0.72753256
#> 2     -0.9190592         0.9190592    -0.22203916        0.22203916
#> 3     -5.2527242         5.2527242    -1.17822481        1.17822481
#> 4      0.8259740         0.8259740     0.13741626        0.13741626
#> 5      0.2653811         0.2653811     0.05604127        0.05604127
```
