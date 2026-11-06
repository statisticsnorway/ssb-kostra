# Difference between two numerical variables

Calculating the difference between two numerical variables Listing units
with big difference, either the k units with the biggest absolute
difference, or units with a absolute difference greater than a threshold
Only units with value on both variables are used in the calculations

## Usage

``` r
Diff2NumVar(
  data,
  idVar,
  xVar,
  yVar,
  strataVar = NULL,
  antall = 5,
  grense = NULL,
  zVar = NULL,
  kommentarVar = NULL
)
```

## Arguments

- data:

  Input data set of class data.frame.

- idVar:

  Name of an identification variable.

- xVar:

  Name of the x variable to be compared.

- yVar:

  Name of the y variable to be compared.

- strataVar:

  Name of stratification variable. Optional. If strataVar is given, the
  calculation and listing is performed within each stratum.

- antall:

  Parameter specifying how many units with the biggest difference to be
  listed. Default 5.

- grense:

  Parameter specifying a threshold for the units to be listed. This
  parameter overrules antall. Optional.

- zVar:

  Name of the original y variable, before editing. Optional.

- kommentarVar:

  Name of a variable giving information about the editing. Optional.

## Value

Output of Diff2NumVar is a data set of class data.frame. The variables
in the data frame are:

- strata:

  The stratum (if strataVar is given, "1" otherwise)

- id:

  The input identification variable

- x:

  The input x variable

- y:

  The input y variable

- Forh:

  The ratio between x and y: y / x

- Diff:

  The difference between x and y: y - x

- AbsDiff:

  The absolute difference: \|Diff\|

- DiffProsAvx:

  The difference in percent of x: (Diff / x) \* 100

- DiffProsAvSumx:

  The difference in percent of the stratum total for x: (Diff /
  stratum x) \* 100

- DiffProsAvTotx:

  The difference in percent of the total for x: (Diff / total x) \* 100

- SumDiffProsAvSumx:

  The stratum difference in percent of the stratum total for x:
  ((stratum y - stratum x) / stratum x) \* 100

- SumDiffProsAvTotx:

  The stratum difference in percent of the total for x: ((stratum y -
  stratum x) / total x) \* 100

- z:

  The input z variable

- EdEndring:

  The difference between z and y: y - z

- Kommentar:

  The input kommentar variable

## Author

Anna Mevik

## Examples

``` r
testdata <- KostraData("testdata")

# lager en grupperingsvariabel
testdata$gr <- c(rep(3, 30), rep(5, 40), rep(1, 61), rep(2, 91), rep(3, 68), rep(4, 61),
                 rep(5, 45),  rep(4, 20))

# lager en z-variabel
testdata$z <- testdata$areal_130_eier_2015
testdata$z[4*(1:104)] <- testdata$areal_130_eier_2014[2*(1:104)]
testdata$z[10*(1:40)] <- 1.2 * testdata$areal_130_eier_2015[10*(1:40)]
testdata$z[10*(1:40) - 5] <- 0.7 * testdata$areal_130_eier_2015[10*(1:40) - 5]

# lager en kommentarvariabel
testdata$kommentar <- ifelse(testdata$areal_130_eier_2015 == testdata$z, "ikke kontrollert",
                            "godkjent")
testdata$kommentar[c(88)] <- "oppgavegiver kontaktet"

# uten strata
Diff2NumVar(data = testdata, idVar = "Region", xVar = "areal_130_eier_2014", yVar = "areal_130_eier_2015",
            strataVar = NULL, antall = 5, grense = NULL, zVar = NULL, kommentarVar = NULL)
#>   strata     id     x     y      Forh  Diff AbsDiff DiffProsAvx DiffProsAvSumx
#> 1      1  60400     0 14677       Inf 14677   14677         Inf      0.8904250
#> 2      1 112000 12141 22815 1.8791698 10674   10674    87.91698      0.6475708
#> 3      1 110200 24000 33029 1.3762083  9029    9029    37.62083      0.5477718
#> 4      1 190200 10765 19292 1.7921040  8527    8527    79.21040      0.5173165
#> 5      1  40300 11234  3739 0.3328289 -7495    7495   -66.71711     -0.4547071
#>   DiffProsAvTotx SumDiffProsAvSumx SumDiffProsAvTotx  z EdEndring Kommentar
#> 1      0.8904250          1.363757          1.363757 NA        NA        NA
#> 2      0.6475708          1.363757          1.363757 NA        NA        NA
#> 3      0.5477718          1.363757          1.363757 NA        NA        NA
#> 4      0.5173165          1.363757          1.363757 NA        NA        NA
#> 5     -0.4547071          1.363757          1.363757 NA        NA        NA

# med strata
Diff2NumVar(data = testdata, idVar = "Region", xVar = "areal_130_eier_2014", yVar = "areal_130_eier_2015",
            strataVar = "gr", antall = 5, grense = NULL, zVar = NULL, kommentarVar = NULL)
#>    strata     id     x     y      Forh  Diff AbsDiff DiffProsAvx DiffProsAvSumx
#> 1       1  60400     0 14677       Inf 14677   14677         Inf      5.5973152
#> 2       1  70100  6969 13646 1.9581002  6677    6677    95.81002      2.5463837
#> 3       1  61500  2100     0 0.0000000 -2100    2100  -100.00000     -0.8008695
#> 4       1  62500  7128  5066 0.7107183 -2062    2062   -28.92817     -0.7863776
#> 5       1  54000  4263  2501 0.5866760 -1762    1762   -41.33240     -0.6719677
#> 6       2 112000 12141 22815 1.8791698 10674   10674    87.91698      2.7081954
#> 7       2 110200 24000 33029 1.3762083  9029    9029    37.62083      2.2908278
#> 8       2 112100 11195  5510 0.4921840 -5685    5685   -50.78160     -1.4423919
#> 9       2 113000  1116  5947 5.3288530  4831    4831   432.88530      1.2257159
#> 10      2 111900  5727  9645 1.6841278  3918    3918    68.41278      0.9940706
#> 11      3  13600 10562  5648 0.5347472 -4914    4914   -46.52528     -1.3165615
#> 12      3  10100     0  3956       Inf  3956    3956         Inf      1.0598936
#> 13      3 150500  7660  5040 0.6579634 -2620    2620   -34.20366     -0.7019518
#> 14      3 144300  4000  2080 0.5200000 -1920    1920   -48.00000     -0.5144074
#> 15      3 142100  5540  3940 0.7111913 -1600    1600   -28.88087     -0.4286729
#> 16      4 200400 10568 13690 1.2954201  3122    3122    29.54201      1.1427066
#> 17      4 164800  5696  4008 0.7036517 -1688    1688   -29.63483     -0.6178375
#> 18      4 200200   675  1428 2.1155556   753     753   111.55556      0.2756112
#> 19      4 174000  1026  1734 1.6900585   708     708    69.00585      0.2591404
#> 20      4 201800  1014  1498 1.4773176   484     484    47.73176      0.1771525
#> 21      5 190200 10765 19292 1.7921040  8527    8527    79.21040      2.4679745
#> 22      5  40300 11234  3739 0.3328289 -7495    7495   -66.71711     -2.1692822
#> 23      5  23500 11878 16748 1.4100017  4870    4870    41.00017      1.4095269
#> 24      5  23000 15531 13287 0.8555148 -2244    2244   -14.44852     -0.6494822
#> 25      5 193800     0  1935       Inf  1935    1935         Inf      0.5600482
#>    DiffProsAvTotx SumDiffProsAvSumx SumDiffProsAvTotx  z EdEndring Kommentar
#> 1      0.89042500          5.803634         0.9232464 NA        NA        NA
#> 2      0.40508059          5.803634         0.9232464 NA        NA        NA
#> 3     -0.12740291          5.803634         0.9232464 NA        NA        NA
#> 4     -0.12509752          5.803634         0.9232464 NA        NA        NA
#> 5     -0.10689711          5.803634         0.9232464 NA        NA        NA
#> 6      0.64757079          2.845965         0.6805135 NA        NA        NA
#> 7      0.54777184          2.845965         0.6805135 NA        NA        NA
#> 8     -0.34489788          2.845965         0.6805135 NA        NA        NA
#> 9      0.29308736          2.845965         0.6805135 NA        NA        NA
#> 10     0.23769743          2.845965         0.6805135 NA        NA        NA
#> 11    -0.29812281         -3.578347        -0.8102825 NA        NA        NA
#> 12     0.24000281         -3.578347        -0.8102825 NA        NA        NA
#> 13    -0.15895030         -3.578347        -0.8102825 NA        NA        NA
#> 14    -0.11648266         -3.578347        -0.8102825 NA        NA        NA
#> 15    -0.09706888         -3.578347        -0.8102825 NA        NA        NA
#> 16     0.18940566          1.040954         0.1725399 NA        NA        NA
#> 17    -0.10240767          1.040954         0.1725399 NA        NA        NA
#> 18     0.04568304          1.040954         0.1725399 NA        NA        NA
#> 19     0.04295298          1.040954         0.1725399 NA        NA        NA
#> 20     0.02936334          1.040954         0.1725399 NA        NA        NA
#> 21     0.51731648          1.897507         0.3977398 NA        NA        NA
#> 22    -0.45470705          1.897507         0.3977398 NA        NA        NA
#> 23     0.29545341          1.897507         0.3977398 NA        NA        NA
#> 24    -0.13613911          1.897507         0.3977398 NA        NA        NA
#> 25     0.11739268          1.897507         0.3977398 NA        NA        NA

# med z og kommentar
Diff2NumVar(data = testdata, idVar = "Region", xVar = "areal_130_eier_2014", yVar = "areal_130_eier_2015",
            strataVar = "gr", antall = 5, grense = NULL, zVar = "z", kommentarVar = "kommentar")
#>    strata     id     x     y      Forh  Diff AbsDiff DiffProsAvx DiffProsAvSumx
#> 1       1  60400     0 14677       Inf 14677   14677         Inf      5.5973152
#> 2       1  70100  6969 13646 1.9581002  6677    6677    95.81002      2.5463837
#> 3       1  61500  2100     0 0.0000000 -2100    2100  -100.00000     -0.8008695
#> 4       1  62500  7128  5066 0.7107183 -2062    2062   -28.92817     -0.7863776
#> 5       1  54000  4263  2501 0.5866760 -1762    1762   -41.33240     -0.6719677
#> 6       2 112000 12141 22815 1.8791698 10674   10674    87.91698      2.7081954
#> 7       2 110200 24000 33029 1.3762083  9029    9029    37.62083      2.2908278
#> 8       2 112100 11195  5510 0.4921840 -5685    5685   -50.78160     -1.4423919
#> 9       2 113000  1116  5947 5.3288530  4831    4831   432.88530      1.2257159
#> 10      2 111900  5727  9645 1.6841278  3918    3918    68.41278      0.9940706
#> 11      3  13600 10562  5648 0.5347472 -4914    4914   -46.52528     -1.3165615
#> 12      3  10100     0  3956       Inf  3956    3956         Inf      1.0598936
#> 13      3 150500  7660  5040 0.6579634 -2620    2620   -34.20366     -0.7019518
#> 14      3 144300  4000  2080 0.5200000 -1920    1920   -48.00000     -0.5144074
#> 15      3 142100  5540  3940 0.7111913 -1600    1600   -28.88087     -0.4286729
#> 16      4 200400 10568 13690 1.2954201  3122    3122    29.54201      1.1427066
#> 17      4 164800  5696  4008 0.7036517 -1688    1688   -29.63483     -0.6178375
#> 18      4 200200   675  1428 2.1155556   753     753   111.55556      0.2756112
#> 19      4 174000  1026  1734 1.6900585   708     708    69.00585      0.2591404
#> 20      4 201800  1014  1498 1.4773176   484     484    47.73176      0.1771525
#> 21      5 190200 10765 19292 1.7921040  8527    8527    79.21040      2.4679745
#> 22      5  40300 11234  3739 0.3328289 -7495    7495   -66.71711     -2.1692822
#> 23      5  23500 11878 16748 1.4100017  4870    4870    41.00017      1.4095269
#> 24      5  23000 15531 13287 0.8555148 -2244    2244   -14.44852     -0.6494822
#> 25      5 193800     0  1935       Inf  1935    1935         Inf      0.5600482
#>    DiffProsAvTotx SumDiffProsAvSumx SumDiffProsAvTotx       z EdEndring
#> 1      0.89042500          5.803634         0.9232464  9089.0    5588.0
#> 2      0.40508059          5.803634         0.9232464  3330.0   10316.0
#> 3     -0.12740291          5.803634         0.9232464     0.0       0.0
#> 4     -0.12509752          5.803634         0.9232464  5066.0       0.0
#> 5     -0.10689711          5.803634         0.9232464  2501.0       0.0
#> 6      0.64757079          2.845965         0.6805135 15970.5    6844.5
#> 7      0.54777184          2.845965         0.6805135 33029.0       0.0
#> 8     -0.34489788          2.845965         0.6805135     0.0    5510.0
#> 9      0.29308736          2.845965         0.6805135  5947.0       0.0
#> 10     0.23769743          2.845965         0.6805135  9645.0       0.0
#> 11    -0.29812281         -3.578347        -0.8102825   970.0    4678.0
#> 12     0.24000281         -3.578347        -0.8102825  3956.0       0.0
#> 13    -0.15895030         -3.578347        -0.8102825  8126.0   -3086.0
#> 14    -0.11648266         -3.578347        -0.8102825  2080.0       0.0
#> 15    -0.09706888         -3.578347        -0.8102825  2758.0    1182.0
#> 16     0.18940566          1.040954         0.1725399 13690.0       0.0
#> 17    -0.10240767          1.040954         0.1725399  4008.0       0.0
#> 18     0.04568304          1.040954         0.1725399  1428.0       0.0
#> 19     0.04295298          1.040954         0.1725399  1734.0       0.0
#> 20     0.02936334          1.040954         0.1725399  1498.0       0.0
#> 21     0.51731648          1.897507         0.3977398 19292.0       0.0
#> 22    -0.45470705          1.897507         0.3977398  3739.0       0.0
#> 23     0.29545341          1.897507         0.3977398 16748.0       0.0
#> 24    -0.13613911          1.897507         0.3977398 13287.0       0.0
#> 25     0.11739268          1.897507         0.3977398  3498.0   -1563.0
#>                 Kommentar
#> 1  oppgavegiver kontaktet
#> 2                godkjent
#> 3        ikke kontrollert
#> 4        ikke kontrollert
#> 5        ikke kontrollert
#> 6                godkjent
#> 7        ikke kontrollert
#> 8                godkjent
#> 9        ikke kontrollert
#> 10       ikke kontrollert
#> 11               godkjent
#> 12       ikke kontrollert
#> 13               godkjent
#> 14       ikke kontrollert
#> 15               godkjent
#> 16       ikke kontrollert
#> 17       ikke kontrollert
#> 18       ikke kontrollert
#> 19       ikke kontrollert
#> 20       ikke kontrollert
#> 21       ikke kontrollert
#> 22       ikke kontrollert
#> 23       ikke kontrollert
#> 24       ikke kontrollert
#> 25               godkjent

# med grense
Diff2NumVar(data = testdata, idVar = "Region", xVar = "areal_130_eier_2014", yVar = "areal_130_eier_2015",
            strataVar = "gr", antall = 5, grense = 5000, zVar = "z", kommentarVar = "kommentar")
#>   strata     id     x     y      Forh  Diff AbsDiff DiffProsAvx DiffProsAvSumx
#> 1      1  60400     0 14677       Inf 14677   14677         Inf       5.597315
#> 2      1  70100  6969 13646 1.9581002  6677    6677    95.81002       2.546384
#> 3      2 112000 12141 22815 1.8791698 10674   10674    87.91698       2.708195
#> 4      2 110200 24000 33029 1.3762083  9029    9029    37.62083       2.290828
#> 5      2 112100 11195  5510 0.4921840 -5685    5685   -50.78160      -1.442392
#> 6      5 190200 10765 19292 1.7921040  8527    8527    79.21040       2.467975
#> 7      5  40300 11234  3739 0.3328289 -7495    7495   -66.71711      -2.169282
#>   DiffProsAvTotx SumDiffProsAvSumx SumDiffProsAvTotx       z EdEndring
#> 1      0.8904250          5.803634         0.9232464  9089.0    5588.0
#> 2      0.4050806          5.803634         0.9232464  3330.0   10316.0
#> 3      0.6475708          2.845965         0.6805135 15970.5    6844.5
#> 4      0.5477718          2.845965         0.6805135 33029.0       0.0
#> 5     -0.3448979          2.845965         0.6805135     0.0    5510.0
#> 6      0.5173165          1.897507         0.3977398 19292.0       0.0
#> 7     -0.4547071          1.897507         0.3977398  3739.0       0.0
#>                Kommentar
#> 1 oppgavegiver kontaktet
#> 2               godkjent
#> 3               godkjent
#> 4       ikke kontrollert
#> 5               godkjent
#> 6       ikke kontrollert
#> 7       ikke kontrollert

```
