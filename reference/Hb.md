# Detection of outliers using the Hidiroglou-Berthelot (HB) method

Detects possible outliers of a variable in period t by comparing it with
revised values from period t-1

## Usage

``` r
Hb(data, id, x1, x2, pU = 0.5, pA = 0.05, pC = 20, strataName = NULL)
```

## Arguments

- data:

  Input of Hb is a data set of class data.frame.

- id:

  Name of an identification variable.

- x1:

  Name of variable in period t.

- x2:

  Name of variable in period t-1.

- pU:

  Parameter that adjusts for different level of the variables.Default
  value 0.5.

- pA:

  Parameter that adjusts for small differences between the median and
  the 1st or 3rd quartile. Default value 0.05.

- pC:

  Parameter that controls the length of the confidence interval.Default
  value 20.

- strataName:

  Name of stratification variable. Optional. If strataName is given, the
  HB method is performed within each stratum.

## Value

Output of Hb is a data set of class data.frame. All units are returned,
but the HB method is only performed on the data set where units with
both x1 and x2 not missing and greater than zero are included. In this
data set, units with x1 = x2 are included in the HB method only if they
cover less than 50 per cent of the number of units in the stratum. The
output variables are:

- id:

  The input identification variable

- x1:

  The input x1 variable

- x2:

  The input x2 variable

- maxX:

  The maximum of x1 and x2

- ratio:

  The ratio between x1 and x2

- lowerLimit:

  The lower limit of the ratio

- upperLimit:

  The upper limit of the ratio

- medRatio:

  The median ratio

- outlier:

  A binary variable indicating whether the observation is an outlier (1)
  or not (0)

- strata:

  Strata name or number

## References

Datarevisjon: Kontroll, granskning og retting av data. Anbefalt praksis,
Statistisk sentralbyrås håndbøker, 2005/84.

## Author

Magnar Lillegård

## Examples

``` r
 minedata <- KostraData("testdata")
 minedata$strata <- as.character(c(rep(1, 61), rep(2, 91), rep(3, 98), rep(4, 81), rep(5, 85)))
 Hb(data = minedata, id = "Region", x1 = "areal_381_eier_2015", x2 = "areal_381_eier_2014")
#> Warning: 71 % av enhetene har lik verdi i de to periodene. Dette er >=50 % og metoden kjører derfor kun på de
#>               enhetene som har to ulike verdier på x1 og x2.
#>         id    x1     x2   maxX     ratio lowerLimit upperLimit medRatio outlier
#> 1    10100  9020   9720   9720 0.9279835  0.5226313   3.272601 1.004104       0
#> 2    10500 24417  18364  24417 1.3296123  0.6350062   2.435386 1.004104       0
#> 3    10600 38167  35997  38167 1.0602828  0.6854390   2.148898 1.004104       0
#> 4    11100  2790   3448   3448 0.8091647  0.3942657   4.812900 1.004104       0
#> 5    12200  3169   2500   3169 1.2676000  0.3842103   4.977028 1.004104       0
#> 6    13700  1298   1212   1298 1.0709571  0.2851762   7.211852 1.004104       0
#> 7    21100 11929  11797  11929 1.0111893  0.5482154   3.051818 1.004104       0
#> 8    21300 21799  22200  22200 0.9819369  0.6238285   2.505153 1.004104       0
#> 9    21600 13678  21063  21063 0.6493852  0.6175985   2.545134 1.004104       0
#> 10   22000 38503  36303  38503 1.0606011  0.6863915   2.143892 1.004104       0
#> 11   23100 25607  24183  25607 1.0588843  0.6405423   2.401733 1.004104       0
#> 12   23500 20951  21000  21000 0.9976667  0.6172424   2.547444 1.004104       0
#> 13   23600  3425   3452   3452 0.9921784  0.3944045   4.810693 1.004104       0
#> 14   23700 10788  10533  10788 1.0242096  0.5356770   3.157386 1.004104       0
#> 15   40200  9400   9450   9450 0.9947090  0.5191004   3.304780 1.004104       0
#> 16   40300  4296  20296  20296 0.2116673  0.6131800   2.573983 1.004104       1
#> 17   44100  2852   2900   2900 0.9834483  0.3737462   5.157204 1.004104       0
#> 18   50200  9415   4765   9415 1.9758657  0.5186352   3.309052 1.004104       0
#> 19   52100  2468   1996   2468 1.2364729  0.3550260   5.506035 1.004104       0
#> 20   52900  4260   4250   4260 1.0023529  0.4198517   4.430729 1.004104       0
#> 21   60500  8718  11464  11464 0.7604676  0.5432628   3.092935 1.004104       0
#> 22   62000  3090   3245   3245 0.9522342  0.3870249   4.930228 1.004104       0
#> 23   62600  7678   6506   7678 1.1801414  0.4930445   3.556496 1.004104       0
#> 24   70100  8577   9029   9029 0.9499391  0.5133850   3.357806 1.004104       0
#> 25   70200  3099   4606   4606 0.6728181  0.4294197   4.299514 1.004104       0
#> 26   70400 29223  29917  29917 0.9768025  0.6583812   2.297145 1.004104       0
#> 27   70900 13787  11321  13787 1.2178253  0.5661627   2.908847 1.004104       0
#> 28   71300  5900   3900   5900 1.5128205  0.4600807   3.915796 1.004104       0
#> 29   80600  9200   5000   9200 1.8400000  0.5157384   3.335830 1.004104       0
#> 30   80700  5450   7875   7875 0.6920635  0.4962235   3.524368 1.004104       0
#> 31   81500  5716   6017   6017 0.9499751  0.4625291   3.887348 1.004104       0
#> 32   83100  1150   1000   1150 1.1500000  0.2729799   7.599221 1.004104       0
#> 33   90400 10391   8970  10391 1.1584169  0.5309892   3.198135 1.004104       0
#> 34   91900  9886   9477   9886 1.0431571  0.5247527   3.253475 1.004104       0
#> 35   92800  2341   2451   2451 0.9551204  0.3542332   5.521621 1.004104       0
#> 36   93500  1916   2001   2001 0.9575212  0.3313416   6.003849 1.004104       0
#> 37  100100 61868  52678  61868 1.1744561  0.7355240   1.903267 1.004104       0
#> 38  100300 12099   6599  12099 1.8334596  0.5499759   3.037381 1.004104       0
#> 39  101700  3800   3239   3800 1.1732016  0.4059627   4.632207 1.004104       0
#> 40  101800  2606   4606   4606 0.5657838  0.4294197   4.299514 1.004104       0
#> 41  102600  1355   3450   3450 0.3927536  0.3943351   4.811796 1.004104       1
#> 42  102900  2673   3257   3257 0.8206939  0.3874639   4.922989 1.004104       0
#> 43  104600  1400   1790   1790 0.7821229  0.3190917   6.290320 1.004104       0
#> 44  110200 26391  20646  26391 1.2782621  0.6440321   2.380817 1.004104       0
#> 45  110300 60935  59657  60935 1.0214225  0.7340266   1.910124 1.004104       0
#> 46  112100  4132   8159   8159 0.5064346  0.5006700   3.480117 1.004104       0
#> 47  113000  4168    712   4168 5.8539326  0.4171872   4.468341 1.004104       1
#> 48  113500  5988   5225   5988 1.1460287  0.4619266   3.894321 1.004104       0
#> 49  114900  8354   8390   8390 0.9957092  0.5041742   3.445793 1.004104       0
#> 50  115100   450    500    500 0.9000000  0.1983668  11.006094 1.004104       0
#> 51  120100 94310 101071 101071 0.9331064  0.7809839   1.707594 1.004104       0
#> 52  122200   497   1290   1290 0.3852713  0.2845455   7.231071 1.004104       0
#> 53  122400  2927   3327   3327 0.8797716  0.3899969   4.881543 1.004104       0
#> 54  123300   200    150    200 1.3333333  0.1352811  16.818639 1.004104       0
#> 55  123800  7310   7195   7310 1.0159833  0.4868835   3.619953 1.004104       0
#> 56  124300 12075  12944  12944 0.9328646  0.5583577   2.969894 1.004104       0
#> 57  125100  4911   3460   4911 1.4193642  0.4373165   4.195542 1.004104       0
#> 58  126300 11486  11468  11486 1.0015696  0.5435018   3.090933 1.004104       0
#> 59  142400  5950   6296   6296 0.9450445  0.4681876   3.822740 1.004104       0
#> 60  142600  2999   1964   2999 1.5269857  0.3776926   5.088080 1.004104       0
#> 61  143200 11500   6000  11500 1.9166667  0.5436536   3.089663 1.004104       0
#> 62  144500  6639   3308   6639 2.0069528  0.4748210   3.748963 1.004104       0
#> 63  150500  9913   9910   9913 1.0003027  0.5250944   3.250409 1.004104       0
#> 64  153900  4844   4917   4917 0.9851535  0.4374672   4.193594 1.004104       0
#> 65  157300  1890   1692   1890 1.1170213  0.3250377   6.148572 1.004104       0
#> 66  157600  3730   3066   3730 1.2165688  0.4037166   4.666093 1.004104       0
#> 67  160100 42752  42418  42752 1.0078740  0.6976482   2.085770 1.004104       0
#> 68  163400  2165   1952   2165 1.1091189  0.3401433   5.810754 1.004104       0
#> 69  163500  3842   3872   3872 0.9922521  0.4082344   4.598316 1.004104       0
#> 70  163800  4098   4870   4870 0.8414784  0.4362820   4.208948 1.004104       0
#> 71  164800  3715   3915   3915 0.9489144  0.4095728   4.578523 1.004104       0
#> 72  166300  7560   7516   7560 1.0058542  0.4911013   3.576338 1.004104       0
#> 73  171400 20710  20506  20710 1.0099483  0.6155876   2.558212 1.004104       0
#> 74  171900  2801   2500   2801 1.1204000  0.3696805   5.229961 1.004104       0
#> 75  174000  2412   1240   2412 1.9451613  0.3523967   5.557996 1.004104       0
#> 76  180400 15263  22018  22018 0.6932056  0.6228556   2.511344 1.004104       0
#> 77  181200  2200   1400   2200 1.5714286  0.3419492   5.772366 1.004104       0
#> 78  182200   900   1000   1000 0.9000000  0.2593134   8.076579 1.004104       0
#> 79  182600  1955   2074   2074 0.9426230  0.3353310   5.915071 1.004104       0
#> 80  183500  1770   1700   1770 1.0411765  0.3178700   6.320102 1.004104       0
#> 81  184800  2900   2207   2900 1.3140009  0.3737462   5.157204 1.004104       0
#> 82  185700   320    300    320 1.0666667  0.1652216  13.506592 1.004104       0
#> 83  186500 10360   7954  10360 1.3024893  0.5306155   3.201415 1.004104       0
#> 84  186800  3321   3052   3321 1.0881389  0.3897816   4.885044 1.004104       0
#> 85  190200 31859  25979  31859 1.2263367  0.6654744   2.257116 1.004104       0
#> 86  193100  5837   5833   5837 1.0006858  0.4587430   3.931467 1.004104       0
#> 87  193600  1898   2298   2298 0.8259356  0.3468799   5.669585 1.004104       0
#> 88  194100  2023   2022   2023 1.0004946  0.3325564   5.976589 1.004104       0
#> 89  200200  3910   3500   3910 1.1171429  0.4094179   4.580808 1.004104       0
#> 90  201200  2649   2698   2698 0.9818384  0.3653158   5.309870 1.004104       0
#> 91  202100  3262   3569   3569 0.9139815  0.3984028   4.747779 1.004104       0
#> 92  202200  2011   2151   2151 0.9349140  0.3394141   5.826370 1.004104       0
#> 93   10400     0     NA     NA        NA         NA         NA       NA      NA
#> 94   11800  1025   1025     NA        NA         NA         NA       NA      NA
#> 95   11900  1900   1900     NA        NA         NA         NA       NA      NA
#> 96   12100     0      0     NA        NA         NA         NA       NA      NA
#> 97   12300  2537   2537     NA        NA         NA         NA       NA      NA
#> 98   12400  7549   7549     NA        NA         NA         NA       NA      NA
#> 99   12500  4150   4150     NA        NA         NA         NA       NA      NA
#> 100  12700     0      0     NA        NA         NA         NA       NA      NA
#> 101  12800     0      0     NA        NA         NA         NA       NA      NA
#> 102  13500  3520   3520     NA        NA         NA         NA       NA      NA
#> 103  13600  7946   7946     NA        NA         NA         NA       NA      NA
#> 104  13800   500    500     NA        NA         NA         NA       NA      NA
#> 105  21400  6208   6208     NA        NA         NA         NA       NA      NA
#> 106  21500  4350   4350     NA        NA         NA         NA       NA      NA
#> 107  21700 11582  11582     NA        NA         NA         NA       NA      NA
#> 108  21900 74171  74171     NA        NA         NA         NA       NA      NA
#> 109  22100    NA     NA     NA        NA         NA         NA       NA      NA
#> 110  22700  1470   1470     NA        NA         NA         NA       NA      NA
#> 111  22800  7753   7753     NA        NA         NA         NA       NA      NA
#> 112  22900  7585   7585     NA        NA         NA         NA       NA      NA
#> 113  23000 22511  22511     NA        NA         NA         NA       NA      NA
#> 114  23400    NA     NA     NA        NA         NA         NA       NA      NA
#> 115  23800  3320   3320     NA        NA         NA         NA       NA      NA
#> 116  23900  1989   1989     NA        NA         NA         NA       NA      NA
#> 117  30100     0 141228     NA        NA         NA         NA       NA      NA
#> 118  41200  6698   6698     NA        NA         NA         NA       NA      NA
#> 119  41500  3020   3020     NA        NA         NA         NA       NA      NA
#> 120  41700  3168     NA     NA        NA         NA         NA       NA      NA
#> 121  41800  2862   2862     NA        NA         NA         NA       NA      NA
#> 122  41900  3194   3194     NA        NA         NA         NA       NA      NA
#> 123  42000  3405   3405     NA        NA         NA         NA       NA      NA
#> 124  42300  2958   2958     NA        NA         NA         NA       NA      NA
#> 125  42500  3700   3700     NA        NA         NA         NA       NA      NA
#> 126  42600  1315   1315     NA        NA         NA         NA       NA      NA
#> 127  42700  7213   7213     NA        NA         NA         NA       NA      NA
#> 128  42800    NA      0     NA        NA         NA         NA       NA      NA
#> 129  42900  1668   1668     NA        NA         NA         NA       NA      NA
#> 130  43000  2092   2092     NA        NA         NA         NA       NA      NA
#> 131  43200  1144   1144     NA        NA         NA         NA       NA      NA
#> 132  43400  1600   1600     NA        NA         NA         NA       NA      NA
#> 133  43600  1080   1080     NA        NA         NA         NA       NA      NA
#> 134  43700  2450   2450     NA        NA         NA         NA       NA      NA
#> 135  43800    NA     NA     NA        NA         NA         NA       NA      NA
#> 136  43900  1182   1182     NA        NA         NA         NA       NA      NA
#> 137  50100 19444  19444     NA        NA         NA         NA       NA      NA
#> 138  51100  1780   1780     NA        NA         NA         NA       NA      NA
#> 139  51200   748    748     NA        NA         NA         NA       NA      NA
#> 140  51300  3100   3100     NA        NA         NA         NA       NA      NA
#> 141  51400  2030   2030     NA        NA         NA         NA       NA      NA
#> 142  51500  2400   2400     NA        NA         NA         NA       NA      NA
#> 143  51600  4088   4088     NA        NA         NA         NA       NA      NA
#> 144  51700  5900   5900     NA        NA         NA         NA       NA      NA
#> 145  51900  4216   4216     NA        NA         NA         NA       NA      NA
#> 146  52000  1990   1990     NA        NA         NA         NA       NA      NA
#> 147  52200  2547   2547     NA        NA         NA         NA       NA      NA
#> 148  52800  1405   1405     NA        NA         NA         NA       NA      NA
#> 149  53200  2213   2213     NA        NA         NA         NA       NA      NA
#> 150  53300    NA      0     NA        NA         NA         NA       NA      NA
#> 151  53400  2773   2773     NA        NA         NA         NA       NA      NA
#> 152  53600  2555   2555     NA        NA         NA         NA       NA      NA
#> 153  53800  2587   2587     NA        NA         NA         NA       NA      NA
#> 154  54000     0      0     NA        NA         NA         NA       NA      NA
#> 155  54100  3100   3100     NA        NA         NA         NA       NA      NA
#> 156  54200  5988   5988     NA        NA         NA         NA       NA      NA
#> 157  54300   627    627     NA        NA         NA         NA       NA      NA
#> 158  54400     0      0     NA        NA         NA         NA       NA      NA
#> 159  54500     0      0     NA        NA         NA         NA       NA      NA
#> 160  60400  6587      0     NA        NA         NA         NA       NA      NA
#> 161  61200  1548   1548     NA        NA         NA         NA       NA      NA
#> 162  61500  2570   2570     NA        NA         NA         NA       NA      NA
#> 163  61600  2915   2915     NA        NA         NA         NA       NA      NA
#> 164  61700  5800   5800     NA        NA         NA         NA       NA      NA
#> 165  61800  2795   2795     NA        NA         NA         NA       NA      NA
#> 166  61900  2295   2295     NA        NA         NA         NA       NA      NA
#> 167  62100    NA     NA     NA        NA         NA         NA       NA      NA
#> 168  62200     0     NA     NA        NA         NA         NA       NA      NA
#> 169  62300  4507   4507     NA        NA         NA         NA       NA      NA
#> 170  62400  6000   6000     NA        NA         NA         NA       NA      NA
#> 171  62500  3718   3718     NA        NA         NA         NA       NA      NA
#> 172  62700    NA     NA     NA        NA         NA         NA       NA      NA
#> 173  62800    NA     NA     NA        NA         NA         NA       NA      NA
#> 174  63100    NA    700     NA        NA         NA         NA       NA      NA
#> 175  63200    NA      0     NA        NA         NA         NA       NA      NA
#> 176  63300  2119   2119     NA        NA         NA         NA       NA      NA
#> 177  70600  8211   8211     NA        NA         NA         NA       NA      NA
#> 178  71100  3855   3855     NA        NA         NA         NA       NA      NA
#> 179  71400  2027   2027     NA        NA         NA         NA       NA      NA
#> 180  71600  8200   8200     NA        NA         NA         NA       NA      NA
#> 181  71900  3240   3240     NA        NA         NA         NA       NA      NA
#> 182  72000  4608   4608     NA        NA         NA         NA       NA      NA
#> 183  72200  8382   8382     NA        NA         NA         NA       NA      NA
#> 184  72300  1831   1831     NA        NA         NA         NA       NA      NA
#> 185  72800   744    744     NA        NA         NA         NA       NA      NA
#> 186  80500 19328  19328     NA        NA         NA         NA       NA      NA
#> 187  81100     0      0     NA        NA         NA         NA       NA      NA
#> 188  81400  3317   3317     NA        NA         NA         NA       NA      NA
#> 189  81900  5575   5575     NA        NA         NA         NA       NA      NA
#> 190  82100     0      0     NA        NA         NA         NA       NA      NA
#> 191  82200   540    540     NA        NA         NA         NA       NA      NA
#> 192  82600  4697   4697     NA        NA         NA         NA       NA      NA
#> 193  82700    NA     NA     NA        NA         NA         NA       NA      NA
#> 194  82800  1025   1025     NA        NA         NA         NA       NA      NA
#> 195  82900  2700   2700     NA        NA         NA         NA       NA      NA
#> 196  83000     0   1000     NA        NA         NA         NA       NA      NA
#> 197  83300  2100   2100     NA        NA         NA         NA       NA      NA
#> 198  83400 12410  12410     NA        NA         NA         NA       NA      NA
#> 199  90100  5494   5494     NA        NA         NA         NA       NA      NA
#> 200  91100   546    546     NA        NA         NA         NA       NA      NA
#> 201  91200  2000   2000     NA        NA         NA         NA       NA      NA
#> 202  91400     0     NA     NA        NA         NA         NA       NA      NA
#> 203  92600  9036   9036     NA        NA         NA         NA       NA      NA
#> 204  92900  2835   2835     NA        NA         NA         NA       NA      NA
#> 205  93700   275    275     NA        NA         NA         NA       NA      NA
#> 206  93800    NA      0     NA        NA         NA         NA       NA      NA
#> 207  94000  3000   3000     NA        NA         NA         NA       NA      NA
#> 208  94100     0   3094     NA        NA         NA         NA       NA      NA
#> 209 100200  9039   9039     NA        NA         NA         NA       NA      NA
#> 210 100400  5010   5010     NA        NA         NA         NA       NA      NA
#> 211 101400  7426   7426     NA        NA         NA         NA       NA      NA
#> 212 102100  2003   2003     NA        NA         NA         NA       NA      NA
#> 213 102700   440    440     NA        NA         NA         NA       NA      NA
#> 214 103200  1200   1200     NA        NA         NA         NA       NA      NA
#> 215 103400    NA     NA     NA        NA         NA         NA       NA      NA
#> 216 103700  3895   3895     NA        NA         NA         NA       NA      NA
#> 217 110100  4631   4631     NA        NA         NA         NA       NA      NA
#> 218 110600 13610  13610     NA        NA         NA         NA       NA      NA
#> 219 111100   612    612     NA        NA         NA         NA       NA      NA
#> 220 111400    NA     NA     NA        NA         NA         NA       NA      NA
#> 221 111900     0     NA     NA        NA         NA         NA       NA      NA
#> 222 112000  7170   7170     NA        NA         NA         NA       NA      NA
#> 223 112200  4195   4195     NA        NA         NA         NA       NA      NA
#> 224 112400 22700  22700     NA        NA         NA         NA       NA      NA
#> 225 112700  6432   6432     NA        NA         NA         NA       NA      NA
#> 226 112900    89     89     NA        NA         NA         NA       NA      NA
#> 227 113300  1790   1790     NA        NA         NA         NA       NA      NA
#> 228 113400  2803   2803     NA        NA         NA         NA       NA      NA
#> 229 114100    NA      0     NA        NA         NA         NA       NA      NA
#> 230 114200  4050   4050     NA        NA         NA         NA       NA      NA
#> 231 114400    NA   1500     NA        NA         NA         NA       NA      NA
#> 232 114500    NA     NA     NA        NA         NA         NA       NA      NA
#> 233 114600  8550   8550     NA        NA         NA         NA       NA      NA
#> 234 116000 10516  10516     NA        NA         NA         NA       NA      NA
#> 235 121100   651    651     NA        NA         NA         NA       NA      NA
#> 236 121600  1900   1900     NA        NA         NA         NA       NA      NA
#> 237 121900  2975   2975     NA        NA         NA         NA       NA      NA
#> 238 122100     0      0     NA        NA         NA         NA       NA      NA
#> 239 122300  2200   2200     NA        NA         NA         NA       NA      NA
#> 240 122700     0      0     NA        NA         NA         NA       NA      NA
#> 241 122800  6398   6398     NA        NA         NA         NA       NA      NA
#> 242 123100   600    600     NA        NA         NA         NA       NA      NA
#> 243 123200  2207   2207     NA        NA         NA         NA       NA      NA
#> 244 123400    NA      0     NA        NA         NA         NA       NA      NA
#> 245 123500  6805   6805     NA        NA         NA         NA       NA      NA
#> 246 124100    NA    600     NA        NA         NA         NA       NA      NA
#> 247 124200   440    440     NA        NA         NA         NA       NA      NA
#> 248 124400  3200   3200     NA        NA         NA         NA       NA      NA
#> 249 124500  3658   3658     NA        NA         NA         NA       NA      NA
#> 250 124600    NA  13973     NA        NA         NA         NA       NA      NA
#> 251 124700 10257  10257     NA        NA         NA         NA       NA      NA
#> 252 125200  2520   2520     NA        NA         NA         NA       NA      NA
#> 253 125300  1874   1874     NA        NA         NA         NA       NA      NA
#> 254 125600    NA     NA     NA        NA         NA         NA       NA      NA
#> 255 125900  1121   1121     NA        NA         NA         NA       NA      NA
#> 256 126000  3956   3956     NA        NA         NA         NA       NA      NA
#> 257 126400  2248   2248     NA        NA         NA         NA       NA      NA
#> 258 126500  1471   1471     NA        NA         NA         NA       NA      NA
#> 259 126600  2700   2700     NA        NA         NA         NA       NA      NA
#> 260 140100  4017   4017     NA        NA         NA         NA       NA      NA
#> 261 141100     0      0     NA        NA         NA         NA       NA      NA
#> 262 141200  1019   1019     NA        NA         NA         NA       NA      NA
#> 263 141300  1568   1568     NA        NA         NA         NA       NA      NA
#> 264 141600  4584   4584     NA        NA         NA         NA       NA      NA
#> 265 141700  2019   2019     NA        NA         NA         NA       NA      NA
#> 266 141800  2645   2645     NA        NA         NA         NA       NA      NA
#> 267 141900    NA   1200     NA        NA         NA         NA       NA      NA
#> 268 142000    NA     NA     NA        NA         NA         NA       NA      NA
#> 269 142100  2570   2570     NA        NA         NA         NA       NA      NA
#> 270 142200  3726   3726     NA        NA         NA         NA       NA      NA
#> 271 142800    NA     NA     NA        NA         NA         NA       NA      NA
#> 272 142900  2277   2277     NA        NA         NA         NA       NA      NA
#> 273 143100     0      0     NA        NA         NA         NA       NA      NA
#> 274 143300  1200   1200     NA        NA         NA         NA       NA      NA
#> 275 143800  3506   3506     NA        NA         NA         NA       NA      NA
#> 276 143900    NA     NA     NA        NA         NA         NA       NA      NA
#> 277 144100    NA      0     NA        NA         NA         NA       NA      NA
#> 278 144300  2800   2800     NA        NA         NA         NA       NA      NA
#> 279 144400  2500   2500     NA        NA         NA         NA       NA      NA
#> 280 144900  1307   1307     NA        NA         NA         NA       NA      NA
#> 281 150400    NA     NA     NA        NA         NA         NA       NA      NA
#> 282 151100  1856   1856     NA        NA         NA         NA       NA      NA
#> 283 151400    NA     NA     NA        NA         NA         NA       NA      NA
#> 284 151500    NA     NA     NA        NA         NA         NA       NA      NA
#> 285 151700    NA   3008     NA        NA         NA         NA       NA      NA
#> 286 151900  2000   2000     NA        NA         NA         NA       NA      NA
#> 287 152000  2350   2350     NA        NA         NA         NA       NA      NA
#> 288 152300  2262   2262     NA        NA         NA         NA       NA      NA
#> 289 152500    NA   3024     NA        NA         NA         NA       NA      NA
#> 290 152600  2450   2450     NA        NA         NA         NA       NA      NA
#> 291 152800  3045   3045     NA        NA         NA         NA       NA      NA
#> 292 152900  2470   2470     NA        NA         NA         NA       NA      NA
#> 293 153100  3000   3000     NA        NA         NA         NA       NA      NA
#> 294 153200  6968   6968     NA        NA         NA         NA       NA      NA
#> 295 153400     0   2274     NA        NA         NA         NA       NA      NA
#> 296 153500  6015   6015     NA        NA         NA         NA       NA      NA
#> 297 154300    NA   1264     NA        NA         NA         NA       NA      NA
#> 298 154500     0      0     NA        NA         NA         NA       NA      NA
#> 299 154600  3118   3118     NA        NA         NA         NA       NA      NA
#> 300 154700     0      0     NA        NA         NA         NA       NA      NA
#> 301 154800  3000   3000     NA        NA         NA         NA       NA      NA
#> 302 155100  6287   6287     NA        NA         NA         NA       NA      NA
#> 303 155400  1825   1825     NA        NA         NA         NA       NA      NA
#> 304 155700  3745   3745     NA        NA         NA         NA       NA      NA
#> 305 156000  1500   1500     NA        NA         NA         NA       NA      NA
#> 306 156300  5193   5193     NA        NA         NA         NA       NA      NA
#> 307 156600  2100   2100     NA        NA         NA         NA       NA      NA
#> 308 156700     0      0     NA        NA         NA         NA       NA      NA
#> 309 157100  1715   1715     NA        NA         NA         NA       NA      NA
#> 310 161200  5078   5078     NA        NA         NA         NA       NA      NA
#> 311 161300   342    342     NA        NA         NA         NA       NA      NA
#> 312 161700    NA     NA     NA        NA         NA         NA       NA      NA
#> 313 162000  2044   2044     NA        NA         NA         NA       NA      NA
#> 314 162100  3545   3545     NA        NA         NA         NA       NA      NA
#> 315 162200  3300   3300     NA        NA         NA         NA       NA      NA
#> 316 162400   920     NA     NA        NA         NA         NA       NA      NA
#> 317 162700  3179   3179     NA        NA         NA         NA       NA      NA
#> 318 163000    NA      0     NA        NA         NA         NA       NA      NA
#> 319 163200    NA      0     NA        NA         NA         NA       NA      NA
#> 320 163300  1082   1082     NA        NA         NA         NA       NA      NA
#> 321 163600     0     NA     NA        NA         NA         NA       NA      NA
#> 322 164000   382     NA     NA        NA         NA         NA       NA      NA
#> 323 164400     0     NA     NA        NA         NA         NA       NA      NA
#> 324 165300  9611   9611     NA        NA         NA         NA       NA      NA
#> 325 165700  2500   2500     NA        NA         NA         NA       NA      NA
#> 326 166200  2549   2549     NA        NA         NA         NA       NA      NA
#> 327 166400  3235   3235     NA        NA         NA         NA       NA      NA
#> 328 166500  3000   3000     NA        NA         NA         NA       NA      NA
#> 329 170200     0      0     NA        NA         NA         NA       NA      NA
#> 330 170300 11596  11596     NA        NA         NA         NA       NA      NA
#> 331 171100  7100   7100     NA        NA         NA         NA       NA      NA
#> 332 171700   200      0     NA        NA         NA         NA       NA      NA
#> 333 171800  2225   2225     NA        NA         NA         NA       NA      NA
#> 334 172100   500    500     NA        NA         NA         NA       NA      NA
#> 335 172400     0      0     NA        NA         NA         NA       NA      NA
#> 336 172500    NA     NA     NA        NA         NA         NA       NA      NA
#> 337 173600     0      0     NA        NA         NA         NA       NA      NA
#> 338 173800  4410   4410     NA        NA         NA         NA       NA      NA
#> 339 173900  1586   1586     NA        NA         NA         NA       NA      NA
#> 340 174200  2841   2841     NA        NA         NA         NA       NA      NA
#> 341 174300    NA      0     NA        NA         NA         NA       NA      NA
#> 342 174400  1916   1916     NA        NA         NA         NA       NA      NA
#> 343 174800   288    288     NA        NA         NA         NA       NA      NA
#> 344 174900  1768   1768     NA        NA         NA         NA       NA      NA
#> 345 175000  4056   4056     NA        NA         NA         NA       NA      NA
#> 346 175100  3000     NA     NA        NA         NA         NA       NA      NA
#> 347 175500    NA      0     NA        NA         NA         NA       NA      NA
#> 348 175600   800    800     NA        NA         NA         NA       NA      NA
#> 349 180500 10530  10530     NA        NA         NA         NA       NA      NA
#> 350 181100     0      0     NA        NA         NA         NA       NA      NA
#> 351 181300  2920   2920     NA        NA         NA         NA       NA      NA
#> 352 181500     0      0     NA        NA         NA         NA       NA      NA
#> 353 181600   112    112     NA        NA         NA         NA       NA      NA
#> 354 181800  1909   1909     NA        NA         NA         NA       NA      NA
#> 355 182000  5845   5845     NA        NA         NA         NA       NA      NA
#> 356 182400 11200  11200     NA        NA         NA         NA       NA      NA
#> 357 182500   313    313     NA        NA         NA         NA       NA      NA
#> 358 182700  2500   2500     NA        NA         NA         NA       NA      NA
#> 359 182800     0      0     NA        NA         NA         NA       NA      NA
#> 360 183200  3716   3716     NA        NA         NA         NA       NA      NA
#> 361 183300 10751  10751     NA        NA         NA         NA       NA      NA
#> 362 183400  1260   1260     NA        NA         NA         NA       NA      NA
#> 363 183600     0      0     NA        NA         NA         NA       NA      NA
#> 364 183700    NA   2315     NA        NA         NA         NA       NA      NA
#> 365 183800     0   1600     NA        NA         NA         NA       NA      NA
#> 366 183900     0      0     NA        NA         NA         NA       NA      NA
#> 367 184000  3998   3998     NA        NA         NA         NA       NA      NA
#> 368 184100  6132   6132     NA        NA         NA         NA       NA      NA
#> 369 184500  2908     NA     NA        NA         NA         NA       NA      NA
#> 370 185000    NA    580     NA        NA         NA         NA       NA      NA
#> 371 185100    NA   3051     NA        NA         NA         NA       NA      NA
#> 372 185200  1150   1150     NA        NA         NA         NA       NA      NA
#> 373 185300     0      0     NA        NA         NA         NA       NA      NA
#> 374 185400   950    950     NA        NA         NA         NA       NA      NA
#> 375 185600    NA   1550     NA        NA         NA         NA       NA      NA
#> 376 185900   500    500     NA        NA         NA         NA       NA      NA
#> 377 186000 11380  11380     NA        NA         NA         NA       NA      NA
#> 378 186600  5210   5210     NA        NA         NA         NA       NA      NA
#> 379 187000  5932   5932     NA        NA         NA         NA       NA      NA
#> 380 187100    NA   3833     NA        NA         NA         NA       NA      NA
#> 381 187400    NA     NA     NA        NA         NA         NA       NA      NA
#> 382 190300 14981  14981     NA        NA         NA         NA       NA      NA
#> 383 191100  3750   3750     NA        NA         NA         NA       NA      NA
#> 384 191300  4000   4000     NA        NA         NA         NA       NA      NA
#> 385 191700  1500   1500     NA        NA         NA         NA       NA      NA
#> 386 191900   593    593     NA        NA         NA         NA       NA      NA
#> 387 192000  1963   1963     NA        NA         NA         NA       NA      NA
#> 388 192200  5030   5030     NA        NA         NA         NA       NA      NA
#> 389 192300  2164   2164     NA        NA         NA         NA       NA      NA
#> 390 192400  5475     NA     NA        NA         NA         NA       NA      NA
#> 391 192500  3650   3650     NA        NA         NA         NA       NA      NA
#> 392 192600     0   1300     NA        NA         NA         NA       NA      NA
#> 393 192700     0      0     NA        NA         NA         NA       NA      NA
#> 394 192800     0      0     NA        NA         NA         NA       NA      NA
#> 395 192900  2532   2532     NA        NA         NA         NA       NA      NA
#> 396 193300  1800     NA     NA        NA         NA         NA       NA      NA
#> 397 193800  2500   2500     NA        NA         NA         NA       NA      NA
#> 398 193900  1200   1200     NA        NA         NA         NA       NA      NA
#> 399 194000     0   2189     NA        NA         NA         NA       NA      NA
#> 400 194200  2560   2560     NA        NA         NA         NA       NA      NA
#> 401 194300     0      0     NA        NA         NA         NA       NA      NA
#> 402 200400 19291  19291     NA        NA         NA         NA       NA      NA
#> 403 201100    NA   2576     NA        NA         NA         NA       NA      NA
#> 404 201400     0      0     NA        NA         NA         NA       NA      NA
#> 405 201500     0    700     NA        NA         NA         NA       NA      NA
#> 406 201700  1600   1600     NA        NA         NA         NA       NA      NA
#> 407 201800  1572   1572     NA        NA         NA         NA       NA      NA
#> 408 201900    NA   4513     NA        NA         NA         NA       NA      NA
#> 409 202000  3790   3790     NA        NA         NA         NA       NA      NA
#> 410 202300  2904   2904     NA        NA         NA         NA       NA      NA
#> 411 202400  1820   1820     NA        NA         NA         NA       NA      NA
#> 412 202500  2450   2450     NA        NA         NA         NA       NA      NA
#> 413 202700   400    400     NA        NA         NA         NA       NA      NA
#> 414 202800    NA   2900     NA        NA         NA         NA       NA      NA
#> 415 203000    NA      0     NA        NA         NA         NA       NA      NA
#> 416 211100  3400      0     NA        NA         NA         NA       NA      NA
#>     strata
#> 1        1
#> 2        1
#> 3        1
#> 4        1
#> 5        1
#> 6        1
#> 7        1
#> 8        1
#> 9        1
#> 10       1
#> 11       1
#> 12       1
#> 13       1
#> 14       1
#> 15       1
#> 16       1
#> 17       1
#> 18       1
#> 19       1
#> 20       1
#> 21       1
#> 22       1
#> 23       1
#> 24       1
#> 25       1
#> 26       1
#> 27       1
#> 28       1
#> 29       1
#> 30       1
#> 31       1
#> 32       1
#> 33       1
#> 34       1
#> 35       1
#> 36       1
#> 37       1
#> 38       1
#> 39       1
#> 40       1
#> 41       1
#> 42       1
#> 43       1
#> 44       1
#> 45       1
#> 46       1
#> 47       1
#> 48       1
#> 49       1
#> 50       1
#> 51       1
#> 52       1
#> 53       1
#> 54       1
#> 55       1
#> 56       1
#> 57       1
#> 58       1
#> 59       1
#> 60       1
#> 61       1
#> 62       1
#> 63       1
#> 64       1
#> 65       1
#> 66       1
#> 67       1
#> 68       1
#> 69       1
#> 70       1
#> 71       1
#> 72       1
#> 73       1
#> 74       1
#> 75       1
#> 76       1
#> 77       1
#> 78       1
#> 79       1
#> 80       1
#> 81       1
#> 82       1
#> 83       1
#> 84       1
#> 85       1
#> 86       1
#> 87       1
#> 88       1
#> 89       1
#> 90       1
#> 91       1
#> 92       1
#> 93       1
#> 94       1
#> 95       1
#> 96       1
#> 97       1
#> 98       1
#> 99       1
#> 100      1
#> 101      1
#> 102      1
#> 103      1
#> 104      1
#> 105      1
#> 106      1
#> 107      1
#> 108      1
#> 109      1
#> 110      1
#> 111      1
#> 112      1
#> 113      1
#> 114      1
#> 115      1
#> 116      1
#> 117      1
#> 118      1
#> 119      1
#> 120      1
#> 121      1
#> 122      1
#> 123      1
#> 124      1
#> 125      1
#> 126      1
#> 127      1
#> 128      1
#> 129      1
#> 130      1
#> 131      1
#> 132      1
#> 133      1
#> 134      1
#> 135      1
#> 136      1
#> 137      1
#> 138      1
#> 139      1
#> 140      1
#> 141      1
#> 142      1
#> 143      1
#> 144      1
#> 145      1
#> 146      1
#> 147      1
#> 148      1
#> 149      1
#> 150      1
#> 151      1
#> 152      1
#> 153      1
#> 154      1
#> 155      1
#> 156      1
#> 157      1
#> 158      1
#> 159      1
#> 160      1
#> 161      1
#> 162      1
#> 163      1
#> 164      1
#> 165      1
#> 166      1
#> 167      1
#> 168      1
#> 169      1
#> 170      1
#> 171      1
#> 172      1
#> 173      1
#> 174      1
#> 175      1
#> 176      1
#> 177      1
#> 178      1
#> 179      1
#> 180      1
#> 181      1
#> 182      1
#> 183      1
#> 184      1
#> 185      1
#> 186      1
#> 187      1
#> 188      1
#> 189      1
#> 190      1
#> 191      1
#> 192      1
#> 193      1
#> 194      1
#> 195      1
#> 196      1
#> 197      1
#> 198      1
#> 199      1
#> 200      1
#> 201      1
#> 202      1
#> 203      1
#> 204      1
#> 205      1
#> 206      1
#> 207      1
#> 208      1
#> 209      1
#> 210      1
#> 211      1
#> 212      1
#> 213      1
#> 214      1
#> 215      1
#> 216      1
#> 217      1
#> 218      1
#> 219      1
#> 220      1
#> 221      1
#> 222      1
#> 223      1
#> 224      1
#> 225      1
#> 226      1
#> 227      1
#> 228      1
#> 229      1
#> 230      1
#> 231      1
#> 232      1
#> 233      1
#> 234      1
#> 235      1
#> 236      1
#> 237      1
#> 238      1
#> 239      1
#> 240      1
#> 241      1
#> 242      1
#> 243      1
#> 244      1
#> 245      1
#> 246      1
#> 247      1
#> 248      1
#> 249      1
#> 250      1
#> 251      1
#> 252      1
#> 253      1
#> 254      1
#> 255      1
#> 256      1
#> 257      1
#> 258      1
#> 259      1
#> 260      1
#> 261      1
#> 262      1
#> 263      1
#> 264      1
#> 265      1
#> 266      1
#> 267      1
#> 268      1
#> 269      1
#> 270      1
#> 271      1
#> 272      1
#> 273      1
#> 274      1
#> 275      1
#> 276      1
#> 277      1
#> 278      1
#> 279      1
#> 280      1
#> 281      1
#> 282      1
#> 283      1
#> 284      1
#> 285      1
#> 286      1
#> 287      1
#> 288      1
#> 289      1
#> 290      1
#> 291      1
#> 292      1
#> 293      1
#> 294      1
#> 295      1
#> 296      1
#> 297      1
#> 298      1
#> 299      1
#> 300      1
#> 301      1
#> 302      1
#> 303      1
#> 304      1
#> 305      1
#> 306      1
#> 307      1
#> 308      1
#> 309      1
#> 310      1
#> 311      1
#> 312      1
#> 313      1
#> 314      1
#> 315      1
#> 316      1
#> 317      1
#> 318      1
#> 319      1
#> 320      1
#> 321      1
#> 322      1
#> 323      1
#> 324      1
#> 325      1
#> 326      1
#> 327      1
#> 328      1
#> 329      1
#> 330      1
#> 331      1
#> 332      1
#> 333      1
#> 334      1
#> 335      1
#> 336      1
#> 337      1
#> 338      1
#> 339      1
#> 340      1
#> 341      1
#> 342      1
#> 343      1
#> 344      1
#> 345      1
#> 346      1
#> 347      1
#> 348      1
#> 349      1
#> 350      1
#> 351      1
#> 352      1
#> 353      1
#> 354      1
#> 355      1
#> 356      1
#> 357      1
#> 358      1
#> 359      1
#> 360      1
#> 361      1
#> 362      1
#> 363      1
#> 364      1
#> 365      1
#> 366      1
#> 367      1
#> 368      1
#> 369      1
#> 370      1
#> 371      1
#> 372      1
#> 373      1
#> 374      1
#> 375      1
#> 376      1
#> 377      1
#> 378      1
#> 379      1
#> 380      1
#> 381      1
#> 382      1
#> 383      1
#> 384      1
#> 385      1
#> 386      1
#> 387      1
#> 388      1
#> 389      1
#> 390      1
#> 391      1
#> 392      1
#> 393      1
#> 394      1
#> 395      1
#> 396      1
#> 397      1
#> 398      1
#> 399      1
#> 400      1
#> 401      1
#> 402      1
#> 403      1
#> 404      1
#> 405      1
#> 406      1
#> 407      1
#> 408      1
#> 409      1
#> 410      1
#> 411      1
#> 412      1
#> 413      1
#> 414      1
#> 415      1
#> 416      1
 Hb(data = minedata, id = "Region", x1 = "areal_381_eier_2015", x2 = "areal_381_eier_2014",
    strataName = "strata")
#> Warning: 67 % av enhetene har lik verdi i de to periodene. Dette er >=50 % og metoden kjører derfor kun på de
#>               enhetene som har to ulike verdier på x1 og x2.
#> Warning: 74 % av enhetene har lik verdi i de to periodene. Dette er >=50 % og metoden kjører derfor kun på de
#>               enhetene som har to ulike verdier på x1 og x2.
#> Warning: 67 % av enhetene har lik verdi i de to periodene. Dette er >=50 % og metoden kjører derfor kun på de
#>               enhetene som har to ulike verdier på x1 og x2.
#> Warning: 77 % av enhetene har lik verdi i de to periodene. Dette er >=50 % og metoden kjører derfor kun på de
#>               enhetene som har to ulike verdier på x1 og x2.
#> Warning: 70 % av enhetene har lik verdi i de to periodene. Dette er >=50 % og metoden kjører derfor kun på de
#>               enhetene som har to ulike verdier på x1 og x2.
#>         id    x1     x2   maxX     ratio lowerLimit upperLimit  medRatio
#> 1    10100  9020   9720   9720 0.9279835 0.67219848   2.984919 0.9976667
#> 2    10500 24417  18364  24417 1.3296123 0.76420818   2.251501 0.9976667
#> 3    10600 38167  35997  38167 1.0602828 0.80176170   2.000531 0.9976667
#> 4    11100  2790   3448   3448 0.8091647 0.55030204   4.334254 0.9976667
#> 5    12200  3169   2500   3169 1.2676000 0.53987023   4.478034 0.9976667
#> 6    13700  1298   1212   1298 1.0709571 0.42910905   6.435787 0.9976667
#> 7    21100 11929  11797  11929 1.0111893 0.69424097   2.791509 0.9976667
#> 8    21300 21799  22200  22200 0.9819369 0.75558961   2.312618 0.9976667
#> 9    21600 13678  21063  21063 0.6493852 0.75073761   2.347643 0.9976667
#> 10   22000 38503  36303  38503 1.0606011 0.80245074   1.996146 0.9976667
#> 11   23100 25607  24183  25607 1.0588843 0.76843609   2.222020 0.9976667
#> 12   23500 20951  21000  21000 0.9976667 0.75045920   2.349666 0.9976667
#> 13   23600  3425   3452   3452 0.9921784 0.55044509   4.332321 0.9976667
#> 14   23700 10788  10533  10788 1.0242096 0.68352374   2.883988 0.9976667
#> 15   40200  9400   9450   9450 0.9947090 0.66910215   3.013108 0.9976667
#> 16   40300  4296  20296  20296 0.2116673 0.74727522   2.372914 0.9976667
#> 17   44100  2852   2900   2900 0.9834483 0.52886453   4.635872 0.9976667
#> 18   10400     0     NA     NA        NA         NA         NA        NA
#> 19   11800  1025   1025     NA        NA         NA         NA        NA
#> 20   11900  1900   1900     NA        NA         NA         NA        NA
#> 21   12100     0      0     NA        NA         NA         NA        NA
#> 22   12300  2537   2537     NA        NA         NA         NA        NA
#> 23   12400  7549   7549     NA        NA         NA         NA        NA
#> 24   12500  4150   4150     NA        NA         NA         NA        NA
#> 25   12700     0      0     NA        NA         NA         NA        NA
#> 26   12800     0      0     NA        NA         NA         NA        NA
#> 27   13500  3520   3520     NA        NA         NA         NA        NA
#> 28   13600  7946   7946     NA        NA         NA         NA        NA
#> 29   13800   500    500     NA        NA         NA         NA        NA
#> 30   21400  6208   6208     NA        NA         NA         NA        NA
#> 31   21500  4350   4350     NA        NA         NA         NA        NA
#> 32   21700 11582  11582     NA        NA         NA         NA        NA
#> 33   21900 74171  74171     NA        NA         NA         NA        NA
#> 34   22100    NA     NA     NA        NA         NA         NA        NA
#> 35   22700  1470   1470     NA        NA         NA         NA        NA
#> 36   22800  7753   7753     NA        NA         NA         NA        NA
#> 37   22900  7585   7585     NA        NA         NA         NA        NA
#> 38   23000 22511  22511     NA        NA         NA         NA        NA
#> 39   23400    NA     NA     NA        NA         NA         NA        NA
#> 40   23800  3320   3320     NA        NA         NA         NA        NA
#> 41   23900  1989   1989     NA        NA         NA         NA        NA
#> 42   30100     0 141228     NA        NA         NA         NA        NA
#> 43   41200  6698   6698     NA        NA         NA         NA        NA
#> 44   41500  3020   3020     NA        NA         NA         NA        NA
#> 45   41700  3168     NA     NA        NA         NA         NA        NA
#> 46   41800  2862   2862     NA        NA         NA         NA        NA
#> 47   41900  3194   3194     NA        NA         NA         NA        NA
#> 48   42000  3405   3405     NA        NA         NA         NA        NA
#> 49   42300  2958   2958     NA        NA         NA         NA        NA
#> 50   42500  3700   3700     NA        NA         NA         NA        NA
#> 51   42600  1315   1315     NA        NA         NA         NA        NA
#> 52   42700  7213   7213     NA        NA         NA         NA        NA
#> 53   42800    NA      0     NA        NA         NA         NA        NA
#> 54   42900  1668   1668     NA        NA         NA         NA        NA
#> 55   43000  2092   2092     NA        NA         NA         NA        NA
#> 56   43200  1144   1144     NA        NA         NA         NA        NA
#> 57   43400  1600   1600     NA        NA         NA         NA        NA
#> 58   43600  1080   1080     NA        NA         NA         NA        NA
#> 59   43700  2450   2450     NA        NA         NA         NA        NA
#> 60   43800    NA     NA     NA        NA         NA         NA        NA
#> 61   43900  1182   1182     NA        NA         NA         NA        NA
#> 62   50200  9415   4765   9415 1.9758657 0.52560474   4.247417 1.0023529
#> 63   52100  2468   1996   2468 1.2364729 0.36165047   7.340478 1.0023529
#> 64   52900  4260   4250   4260 1.0023529 0.42681451   5.826589 1.0023529
#> 65   60500  8718  11464  11464 0.7604676 0.55013880   3.943152 1.0023529
#> 66   62000  3090   3245   3245 0.9522342 0.39384958   6.529818 1.0023529
#> 67   62600  7678   6506   7678 1.1801414 0.50007100   4.595784 1.0023529
#> 68   70100  8577   9029   9029 0.9499391 0.52036959   4.316056 1.0023529
#> 69   70200  3099   4606   4606 0.6728181 0.43640998   5.641855 1.0023529
#> 70   70400 29223  29917  29917 0.9768025 0.66432040   2.822785 1.0023529
#> 71   70900 13787  11321  13787 1.2178253 0.57291792   3.683981 1.0023529
#> 72   71300  5900   3900   5900 1.5128205 0.46712025   5.101631 1.0023529
#> 73   80600  9200   5000   9200 1.8400000 0.52271646   4.285115 1.0023529
#> 74   80700  5450   7875   7875 0.6920635 0.50324520   4.550553 1.0023529
#> 75   81500  5716   6017   6017 0.9499751 0.46957001   5.061580 1.0023529
#> 76   83100  1150   1000   1150 1.1500000 0.27879375  10.287407 1.0023529
#> 77   90400 10391   8970  10391 1.1584169 0.53791658   4.091259 1.0023529
#> 78   91900  9886   9477   9886 1.0431571 0.53170257   4.169171 1.0023529
#> 79   92800  2341   2451   2451 0.9551204 0.36085194   7.362420 1.0023529
#> 80   93500  1916   2001   2001 0.9575212 0.33777604   8.041335 1.0023529
#> 81   50100 19444  19444     NA        NA         NA         NA        NA
#> 82   51100  1780   1780     NA        NA         NA         NA        NA
#> 83   51200   748    748     NA        NA         NA         NA        NA
#> 84   51300  3100   3100     NA        NA         NA         NA        NA
#> 85   51400  2030   2030     NA        NA         NA         NA        NA
#> 86   51500  2400   2400     NA        NA         NA         NA        NA
#> 87   51600  4088   4088     NA        NA         NA         NA        NA
#> 88   51700  5900   5900     NA        NA         NA         NA        NA
#> 89   51900  4216   4216     NA        NA         NA         NA        NA
#> 90   52000  1990   1990     NA        NA         NA         NA        NA
#> 91   52200  2547   2547     NA        NA         NA         NA        NA
#> 92   52800  1405   1405     NA        NA         NA         NA        NA
#> 93   53200  2213   2213     NA        NA         NA         NA        NA
#> 94   53300    NA      0     NA        NA         NA         NA        NA
#> 95   53400  2773   2773     NA        NA         NA         NA        NA
#> 96   53600  2555   2555     NA        NA         NA         NA        NA
#> 97   53800  2587   2587     NA        NA         NA         NA        NA
#> 98   54000     0      0     NA        NA         NA         NA        NA
#> 99   54100  3100   3100     NA        NA         NA         NA        NA
#> 100  54200  5988   5988     NA        NA         NA         NA        NA
#> 101  54300   627    627     NA        NA         NA         NA        NA
#> 102  54400     0      0     NA        NA         NA         NA        NA
#> 103  54500     0      0     NA        NA         NA         NA        NA
#> 104  60400  6587      0     NA        NA         NA         NA        NA
#> 105  61200  1548   1548     NA        NA         NA         NA        NA
#> 106  61500  2570   2570     NA        NA         NA         NA        NA
#> 107  61600  2915   2915     NA        NA         NA         NA        NA
#> 108  61700  5800   5800     NA        NA         NA         NA        NA
#> 109  61800  2795   2795     NA        NA         NA         NA        NA
#> 110  61900  2295   2295     NA        NA         NA         NA        NA
#> 111  62100    NA     NA     NA        NA         NA         NA        NA
#> 112  62200     0     NA     NA        NA         NA         NA        NA
#> 113  62300  4507   4507     NA        NA         NA         NA        NA
#> 114  62400  6000   6000     NA        NA         NA         NA        NA
#> 115  62500  3718   3718     NA        NA         NA         NA        NA
#> 116  62700    NA     NA     NA        NA         NA         NA        NA
#> 117  62800    NA     NA     NA        NA         NA         NA        NA
#> 118  63100    NA    700     NA        NA         NA         NA        NA
#> 119  63200    NA      0     NA        NA         NA         NA        NA
#> 120  63300  2119   2119     NA        NA         NA         NA        NA
#> 121  70600  8211   8211     NA        NA         NA         NA        NA
#> 122  71100  3855   3855     NA        NA         NA         NA        NA
#> 123  71400  2027   2027     NA        NA         NA         NA        NA
#> 124  71600  8200   8200     NA        NA         NA         NA        NA
#> 125  71900  3240   3240     NA        NA         NA         NA        NA
#> 126  72000  4608   4608     NA        NA         NA         NA        NA
#> 127  72200  8382   8382     NA        NA         NA         NA        NA
#> 128  72300  1831   1831     NA        NA         NA         NA        NA
#> 129  72800   744    744     NA        NA         NA         NA        NA
#> 130  80500 19328  19328     NA        NA         NA         NA        NA
#> 131  81100     0      0     NA        NA         NA         NA        NA
#> 132  81400  3317   3317     NA        NA         NA         NA        NA
#> 133  81900  5575   5575     NA        NA         NA         NA        NA
#> 134  82100     0      0     NA        NA         NA         NA        NA
#> 135  82200   540    540     NA        NA         NA         NA        NA
#> 136  82600  4697   4697     NA        NA         NA         NA        NA
#> 137  82700    NA     NA     NA        NA         NA         NA        NA
#> 138  82800  1025   1025     NA        NA         NA         NA        NA
#> 139  82900  2700   2700     NA        NA         NA         NA        NA
#> 140  83000     0   1000     NA        NA         NA         NA        NA
#> 141  83300  2100   2100     NA        NA         NA         NA        NA
#> 142  83400 12410  12410     NA        NA         NA         NA        NA
#> 143  90100  5494   5494     NA        NA         NA         NA        NA
#> 144  91100   546    546     NA        NA         NA         NA        NA
#> 145  91200  2000   2000     NA        NA         NA         NA        NA
#> 146  91400     0     NA     NA        NA         NA         NA        NA
#> 147  92600  9036   9036     NA        NA         NA         NA        NA
#> 148  92900  2835   2835     NA        NA         NA         NA        NA
#> 149  93700   275    275     NA        NA         NA         NA        NA
#> 150  93800    NA      0     NA        NA         NA         NA        NA
#> 151  94000  3000   3000     NA        NA         NA         NA        NA
#> 152  94100     0   3094     NA        NA         NA         NA        NA
#> 153 100100 61868  52678  61868 1.1744561 0.52557688   3.320620 1.0087765
#> 154 100300 12099   6599  12099 1.8334596 0.32763420   6.236549 1.0087765
#> 155 101700  3800   3239   3800 1.1732016 0.21419405  10.337015 1.0087765
#> 156 101800  2606   4606   4606 0.5657838 0.23086939   9.481626 1.0087765
#> 157 102600  1355   3450   3450 0.3927536 0.20615617  10.798759 1.0087765
#> 158 102900  2673   3257   3257 0.8206939 0.20147504  11.084647 1.0087765
#> 159 104600  1400   1790   1790 0.7821229 0.15749793  14.600200 1.0087765
#> 160 110200 26391  20646  26391 1.2782621 0.41898759   4.548453 1.0087765
#> 161 110300 60935  59657  60935 1.0214225 0.52366387   3.338251 1.0087765
#> 162 112100  4132   8159   8159 0.5064346 0.28563829   7.374870 1.0087765
#> 163 113000  4168    712   4168 5.8539326 0.22209526   9.915697 1.0087765
#> 164 113500  5988   5225   5988 1.1460287 0.25505263   8.439833 1.0087765
#> 165 114900  8354   8390   8390 0.9957092 0.28850524   7.286620 1.0087765
#> 166 115100   450    500    500 0.9000000 0.08985459  26.724956 1.0087765
#> 167 120100 94310 101071 101071 0.9331064 0.58673673   2.817525 1.0087765
#> 168 122200   497   1290   1290 0.3852713 0.13693352  17.018971 1.0087765
#> 169 122400  2927   3327   3327 0.8797716 0.20319482  10.978085 1.0087765
#> 170 123300   200    150    200 1.3333333 0.05875248  41.669627 1.0087765
#> 171 123800  7310   7195   7310 1.0159833 0.27452420   7.734403 1.0087765
#> 172 124300 12075  12944  12944 0.9328646 0.33514527   6.063032 1.0087765
#> 173 125100  4911   3460   4911 1.4193642 0.23662646   9.214304 1.0087765
#> 174 126300 11486  11468  11486 1.0015696 0.32190945   6.374238 1.0087765
#> 175 142400  5950   6296   6296 0.9450445 0.25986132   8.255790 1.0087765
#> 176 142600  2999   1964   2999 1.5269857 0.19490418  11.509113 1.0087765
#> 177 143200 11500   6000  11500 1.9166667 0.32204296   6.370971 1.0087765
#> 178 144500  6639   3308   6639 2.0069528 0.26501105   8.066102 1.0087765
#> 179 100200  9039   9039     NA        NA         NA         NA        NA
#> 180 100400  5010   5010     NA        NA         NA         NA        NA
#> 181 101400  7426   7426     NA        NA         NA         NA        NA
#> 182 102100  2003   2003     NA        NA         NA         NA        NA
#> 183 102700   440    440     NA        NA         NA         NA        NA
#> 184 103200  1200   1200     NA        NA         NA         NA        NA
#> 185 103400    NA     NA     NA        NA         NA         NA        NA
#> 186 103700  3895   3895     NA        NA         NA         NA        NA
#> 187 110100  4631   4631     NA        NA         NA         NA        NA
#> 188 110600 13610  13610     NA        NA         NA         NA        NA
#> 189 111100   612    612     NA        NA         NA         NA        NA
#> 190 111400    NA     NA     NA        NA         NA         NA        NA
#> 191 111900     0     NA     NA        NA         NA         NA        NA
#> 192 112000  7170   7170     NA        NA         NA         NA        NA
#> 193 112200  4195   4195     NA        NA         NA         NA        NA
#> 194 112400 22700  22700     NA        NA         NA         NA        NA
#> 195 112700  6432   6432     NA        NA         NA         NA        NA
#> 196 112900    89     89     NA        NA         NA         NA        NA
#> 197 113300  1790   1790     NA        NA         NA         NA        NA
#> 198 113400  2803   2803     NA        NA         NA         NA        NA
#> 199 114100    NA      0     NA        NA         NA         NA        NA
#> 200 114200  4050   4050     NA        NA         NA         NA        NA
#> 201 114400    NA   1500     NA        NA         NA         NA        NA
#> 202 114500    NA     NA     NA        NA         NA         NA        NA
#> 203 114600  8550   8550     NA        NA         NA         NA        NA
#> 204 116000 10516  10516     NA        NA         NA         NA        NA
#> 205 121100   651    651     NA        NA         NA         NA        NA
#> 206 121600  1900   1900     NA        NA         NA         NA        NA
#> 207 121900  2975   2975     NA        NA         NA         NA        NA
#> 208 122100     0      0     NA        NA         NA         NA        NA
#> 209 122300  2200   2200     NA        NA         NA         NA        NA
#> 210 122700     0      0     NA        NA         NA         NA        NA
#> 211 122800  6398   6398     NA        NA         NA         NA        NA
#> 212 123100   600    600     NA        NA         NA         NA        NA
#> 213 123200  2207   2207     NA        NA         NA         NA        NA
#> 214 123400    NA      0     NA        NA         NA         NA        NA
#> 215 123500  6805   6805     NA        NA         NA         NA        NA
#> 216 124100    NA    600     NA        NA         NA         NA        NA
#> 217 124200   440    440     NA        NA         NA         NA        NA
#> 218 124400  3200   3200     NA        NA         NA         NA        NA
#> 219 124500  3658   3658     NA        NA         NA         NA        NA
#> 220 124600    NA  13973     NA        NA         NA         NA        NA
#> 221 124700 10257  10257     NA        NA         NA         NA        NA
#> 222 125200  2520   2520     NA        NA         NA         NA        NA
#> 223 125300  1874   1874     NA        NA         NA         NA        NA
#> 224 125600    NA     NA     NA        NA         NA         NA        NA
#> 225 125900  1121   1121     NA        NA         NA         NA        NA
#> 226 126000  3956   3956     NA        NA         NA         NA        NA
#> 227 126400  2248   2248     NA        NA         NA         NA        NA
#> 228 126500  1471   1471     NA        NA         NA         NA        NA
#> 229 126600  2700   2700     NA        NA         NA         NA        NA
#> 230 140100  4017   4017     NA        NA         NA         NA        NA
#> 231 141100     0      0     NA        NA         NA         NA        NA
#> 232 141200  1019   1019     NA        NA         NA         NA        NA
#> 233 141300  1568   1568     NA        NA         NA         NA        NA
#> 234 141600  4584   4584     NA        NA         NA         NA        NA
#> 235 141700  2019   2019     NA        NA         NA         NA        NA
#> 236 141800  2645   2645     NA        NA         NA         NA        NA
#> 237 141900    NA   1200     NA        NA         NA         NA        NA
#> 238 142000    NA     NA     NA        NA         NA         NA        NA
#> 239 142100  2570   2570     NA        NA         NA         NA        NA
#> 240 142200  3726   3726     NA        NA         NA         NA        NA
#> 241 142800    NA     NA     NA        NA         NA         NA        NA
#> 242 142900  2277   2277     NA        NA         NA         NA        NA
#> 243 143100     0      0     NA        NA         NA         NA        NA
#> 244 143300  1200   1200     NA        NA         NA         NA        NA
#> 245 143800  3506   3506     NA        NA         NA         NA        NA
#> 246 143900    NA     NA     NA        NA         NA         NA        NA
#> 247 144100    NA      0     NA        NA         NA         NA        NA
#> 248 144300  2800   2800     NA        NA         NA         NA        NA
#> 249 144400  2500   2500     NA        NA         NA         NA        NA
#> 250 144900  1307   1307     NA        NA         NA         NA        NA
#> 251 150500  9913   9910   9913 1.0003027 0.84214606   1.961046 1.0078740
#> 252 153900  4844   4917   4917 0.9851535 0.78775722   2.361266 1.0078740
#> 253 157300  1890   1692   1890 1.1170213 0.69475389   3.190819 1.0078740
#> 254 157600  3730   3066   3730 1.2165688 0.76306901   2.561761 1.0078740
#> 255 160100 42752  42418  42752 1.0078740 0.92063328   1.466856 1.0078740
#> 256 163400  2165   1952   2165 1.1091189 0.70922263   3.047473 1.0078740
#> 257 163500  3842   3872   3872 0.9922521 0.76651485   2.533001 1.0078740
#> 258 163800  4098   4870   4870 0.8414784 0.78692989   2.367781 1.0078740
#> 259 164800  3715   3915   3915 0.9489144 0.76752703   2.524603 1.0078740
#> 260 166300  7560   7516   7560 1.0058542 0.82252193   2.099347 1.0078740
#> 261 171400 20710  20506  20710 1.0099483 0.88709513   1.667327 1.0078740
#> 262 171900  2801   2500   2801 1.1204000 0.73555888   2.801026 1.0078740
#> 263 174000  2412   1240   2412 1.9451613 0.72044857   2.940221 1.0078740
#> 264 150400    NA     NA     NA        NA         NA         NA        NA
#> 265 151100  1856   1856     NA        NA         NA         NA        NA
#> 266 151400    NA     NA     NA        NA         NA         NA        NA
#> 267 151500    NA     NA     NA        NA         NA         NA        NA
#> 268 151700    NA   3008     NA        NA         NA         NA        NA
#> 269 151900  2000   2000     NA        NA         NA         NA        NA
#> 270 152000  2350   2350     NA        NA         NA         NA        NA
#> 271 152300  2262   2262     NA        NA         NA         NA        NA
#> 272 152500    NA   3024     NA        NA         NA         NA        NA
#> 273 152600  2450   2450     NA        NA         NA         NA        NA
#> 274 152800  3045   3045     NA        NA         NA         NA        NA
#> 275 152900  2470   2470     NA        NA         NA         NA        NA
#> 276 153100  3000   3000     NA        NA         NA         NA        NA
#> 277 153200  6968   6968     NA        NA         NA         NA        NA
#> 278 153400     0   2274     NA        NA         NA         NA        NA
#> 279 153500  6015   6015     NA        NA         NA         NA        NA
#> 280 154300    NA   1264     NA        NA         NA         NA        NA
#> 281 154500     0      0     NA        NA         NA         NA        NA
#> 282 154600  3118   3118     NA        NA         NA         NA        NA
#> 283 154700     0      0     NA        NA         NA         NA        NA
#> 284 154800  3000   3000     NA        NA         NA         NA        NA
#> 285 155100  6287   6287     NA        NA         NA         NA        NA
#> 286 155400  1825   1825     NA        NA         NA         NA        NA
#> 287 155700  3745   3745     NA        NA         NA         NA        NA
#> 288 156000  1500   1500     NA        NA         NA         NA        NA
#> 289 156300  5193   5193     NA        NA         NA         NA        NA
#> 290 156600  2100   2100     NA        NA         NA         NA        NA
#> 291 156700     0      0     NA        NA         NA         NA        NA
#> 292 157100  1715   1715     NA        NA         NA         NA        NA
#> 293 161200  5078   5078     NA        NA         NA         NA        NA
#> 294 161300   342    342     NA        NA         NA         NA        NA
#> 295 161700    NA     NA     NA        NA         NA         NA        NA
#> 296 162000  2044   2044     NA        NA         NA         NA        NA
#> 297 162100  3545   3545     NA        NA         NA         NA        NA
#> 298 162200  3300   3300     NA        NA         NA         NA        NA
#> 299 162400   920     NA     NA        NA         NA         NA        NA
#> 300 162700  3179   3179     NA        NA         NA         NA        NA
#> 301 163000    NA      0     NA        NA         NA         NA        NA
#> 302 163200    NA      0     NA        NA         NA         NA        NA
#> 303 163300  1082   1082     NA        NA         NA         NA        NA
#> 304 163600     0     NA     NA        NA         NA         NA        NA
#> 305 164000   382     NA     NA        NA         NA         NA        NA
#> 306 164400     0     NA     NA        NA         NA         NA        NA
#> 307 165300  9611   9611     NA        NA         NA         NA        NA
#> 308 165700  2500   2500     NA        NA         NA         NA        NA
#> 309 166200  2549   2549     NA        NA         NA         NA        NA
#> 310 166400  3235   3235     NA        NA         NA         NA        NA
#> 311 166500  3000   3000     NA        NA         NA         NA        NA
#> 312 170200     0      0     NA        NA         NA         NA        NA
#> 313 170300 11596  11596     NA        NA         NA         NA        NA
#> 314 171100  7100   7100     NA        NA         NA         NA        NA
#> 315 171700   200      0     NA        NA         NA         NA        NA
#> 316 171800  2225   2225     NA        NA         NA         NA        NA
#> 317 172100   500    500     NA        NA         NA         NA        NA
#> 318 172400     0      0     NA        NA         NA         NA        NA
#> 319 172500    NA     NA     NA        NA         NA         NA        NA
#> 320 173600     0      0     NA        NA         NA         NA        NA
#> 321 173800  4410   4410     NA        NA         NA         NA        NA
#> 322 173900  1586   1586     NA        NA         NA         NA        NA
#> 323 174200  2841   2841     NA        NA         NA         NA        NA
#> 324 174300    NA      0     NA        NA         NA         NA        NA
#> 325 174400  1916   1916     NA        NA         NA         NA        NA
#> 326 174800   288    288     NA        NA         NA         NA        NA
#> 327 174900  1768   1768     NA        NA         NA         NA        NA
#> 328 175000  4056   4056     NA        NA         NA         NA        NA
#> 329 175100  3000     NA     NA        NA         NA         NA        NA
#> 330 175500    NA      0     NA        NA         NA         NA        NA
#> 331 175600   800    800     NA        NA         NA         NA        NA
#> 332 180400 15263  22018  22018 0.6932056 0.69502998   1.982197 1.0006858
#> 333 181200  2200   1400   2200 1.5714286 0.41847727   4.105767 1.0006858
#> 334 182200   900   1000   1000 0.9000000 0.32664102   5.606266 1.0006858
#> 335 182600  1955   2074   2074 0.9426230 0.41131523   4.198697 1.0006858
#> 336 183500  1770   1700   1770 1.0411765 0.39226120   4.462452 1.0006858
#> 337 184800  2900   2207   2900 1.3140009 0.45243730   3.705176 1.0006858
#> 338 185700   320    300    320 1.0666667 0.21529843   9.142278 1.0006858
#> 339 186500 10360   7954  10360 1.3024893 0.60975848   2.431570 1.0006858
#> 340 186800  3321   3052   3321 1.0881389 0.46928611   3.527946 1.0006858
#> 341 190200 31859  25979  31859 1.2263367 0.73278294   1.816645 1.0006858
#> 342 193100  5837   5833   5837 1.0006858 0.53970683   2.906978 1.0006858
#> 343 193600  1898   2298   2298 0.8259356 0.42379205   4.038837 1.0006858
#> 344 194100  2023   2022   2023 1.0004946 0.40830287   4.238757 1.0006858
#> 345 200200  3910   3500   3910 1.1171429 0.48967067   3.329828 1.0006858
#> 346 201200  2649   2698   2698 0.9818384 0.44350532   3.804591 1.0006858
#> 347 202100  3262   3569   3569 0.9139815 0.47826908   3.438559 1.0006858
#> 348 202200  2011   2151   2151 0.9349140 0.41573775   4.140935 1.0006858
#> 349 180500 10530  10530     NA        NA         NA         NA        NA
#> 350 181100     0      0     NA        NA         NA         NA        NA
#> 351 181300  2920   2920     NA        NA         NA         NA        NA
#> 352 181500     0      0     NA        NA         NA         NA        NA
#> 353 181600   112    112     NA        NA         NA         NA        NA
#> 354 181800  1909   1909     NA        NA         NA         NA        NA
#> 355 182000  5845   5845     NA        NA         NA         NA        NA
#> 356 182400 11200  11200     NA        NA         NA         NA        NA
#> 357 182500   313    313     NA        NA         NA         NA        NA
#> 358 182700  2500   2500     NA        NA         NA         NA        NA
#> 359 182800     0      0     NA        NA         NA         NA        NA
#> 360 183200  3716   3716     NA        NA         NA         NA        NA
#> 361 183300 10751  10751     NA        NA         NA         NA        NA
#> 362 183400  1260   1260     NA        NA         NA         NA        NA
#> 363 183600     0      0     NA        NA         NA         NA        NA
#> 364 183700    NA   2315     NA        NA         NA         NA        NA
#> 365 183800     0   1600     NA        NA         NA         NA        NA
#> 366 183900     0      0     NA        NA         NA         NA        NA
#> 367 184000  3998   3998     NA        NA         NA         NA        NA
#> 368 184100  6132   6132     NA        NA         NA         NA        NA
#> 369 184500  2908     NA     NA        NA         NA         NA        NA
#> 370 185000    NA    580     NA        NA         NA         NA        NA
#> 371 185100    NA   3051     NA        NA         NA         NA        NA
#> 372 185200  1150   1150     NA        NA         NA         NA        NA
#> 373 185300     0      0     NA        NA         NA         NA        NA
#> 374 185400   950    950     NA        NA         NA         NA        NA
#> 375 185600    NA   1550     NA        NA         NA         NA        NA
#> 376 185900   500    500     NA        NA         NA         NA        NA
#> 377 186000 11380  11380     NA        NA         NA         NA        NA
#> 378 186600  5210   5210     NA        NA         NA         NA        NA
#> 379 187000  5932   5932     NA        NA         NA         NA        NA
#> 380 187100    NA   3833     NA        NA         NA         NA        NA
#> 381 187400    NA     NA     NA        NA         NA         NA        NA
#> 382 190300 14981  14981     NA        NA         NA         NA        NA
#> 383 191100  3750   3750     NA        NA         NA         NA        NA
#> 384 191300  4000   4000     NA        NA         NA         NA        NA
#> 385 191700  1500   1500     NA        NA         NA         NA        NA
#> 386 191900   593    593     NA        NA         NA         NA        NA
#> 387 192000  1963   1963     NA        NA         NA         NA        NA
#> 388 192200  5030   5030     NA        NA         NA         NA        NA
#> 389 192300  2164   2164     NA        NA         NA         NA        NA
#> 390 192400  5475     NA     NA        NA         NA         NA        NA
#> 391 192500  3650   3650     NA        NA         NA         NA        NA
#> 392 192600     0   1300     NA        NA         NA         NA        NA
#> 393 192700     0      0     NA        NA         NA         NA        NA
#> 394 192800     0      0     NA        NA         NA         NA        NA
#> 395 192900  2532   2532     NA        NA         NA         NA        NA
#> 396 193300  1800     NA     NA        NA         NA         NA        NA
#> 397 193800  2500   2500     NA        NA         NA         NA        NA
#> 398 193900  1200   1200     NA        NA         NA         NA        NA
#> 399 194000     0   2189     NA        NA         NA         NA        NA
#> 400 194200  2560   2560     NA        NA         NA         NA        NA
#> 401 194300     0      0     NA        NA         NA         NA        NA
#> 402 200400 19291  19291     NA        NA         NA         NA        NA
#> 403 201100    NA   2576     NA        NA         NA         NA        NA
#> 404 201400     0      0     NA        NA         NA         NA        NA
#> 405 201500     0    700     NA        NA         NA         NA        NA
#> 406 201700  1600   1600     NA        NA         NA         NA        NA
#> 407 201800  1572   1572     NA        NA         NA         NA        NA
#> 408 201900    NA   4513     NA        NA         NA         NA        NA
#> 409 202000  3790   3790     NA        NA         NA         NA        NA
#> 410 202300  2904   2904     NA        NA         NA         NA        NA
#> 411 202400  1820   1820     NA        NA         NA         NA        NA
#> 412 202500  2450   2450     NA        NA         NA         NA        NA
#> 413 202700   400    400     NA        NA         NA         NA        NA
#> 414 202800    NA   2900     NA        NA         NA         NA        NA
#> 415 203000    NA      0     NA        NA         NA         NA        NA
#> 416 211100  3400      0     NA        NA         NA         NA        NA
#>     outlier strata
#> 1         0      1
#> 2         0      1
#> 3         0      1
#> 4         0      1
#> 5         0      1
#> 6         0      1
#> 7         0      1
#> 8         0      1
#> 9         1      1
#> 10        0      1
#> 11        0      1
#> 12        0      1
#> 13        0      1
#> 14        0      1
#> 15        0      1
#> 16        1      1
#> 17        0      1
#> 18       NA      1
#> 19       NA      1
#> 20       NA      1
#> 21       NA      1
#> 22       NA      1
#> 23       NA      1
#> 24       NA      1
#> 25       NA      1
#> 26       NA      1
#> 27       NA      1
#> 28       NA      1
#> 29       NA      1
#> 30       NA      1
#> 31       NA      1
#> 32       NA      1
#> 33       NA      1
#> 34       NA      1
#> 35       NA      1
#> 36       NA      1
#> 37       NA      1
#> 38       NA      1
#> 39       NA      1
#> 40       NA      1
#> 41       NA      1
#> 42       NA      1
#> 43       NA      1
#> 44       NA      1
#> 45       NA      1
#> 46       NA      1
#> 47       NA      1
#> 48       NA      1
#> 49       NA      1
#> 50       NA      1
#> 51       NA      1
#> 52       NA      1
#> 53       NA      1
#> 54       NA      1
#> 55       NA      1
#> 56       NA      1
#> 57       NA      1
#> 58       NA      1
#> 59       NA      1
#> 60       NA      1
#> 61       NA      1
#> 62        0      2
#> 63        0      2
#> 64        0      2
#> 65        0      2
#> 66        0      2
#> 67        0      2
#> 68        0      2
#> 69        0      2
#> 70        0      2
#> 71        0      2
#> 72        0      2
#> 73        0      2
#> 74        0      2
#> 75        0      2
#> 76        0      2
#> 77        0      2
#> 78        0      2
#> 79        0      2
#> 80        0      2
#> 81       NA      2
#> 82       NA      2
#> 83       NA      2
#> 84       NA      2
#> 85       NA      2
#> 86       NA      2
#> 87       NA      2
#> 88       NA      2
#> 89       NA      2
#> 90       NA      2
#> 91       NA      2
#> 92       NA      2
#> 93       NA      2
#> 94       NA      2
#> 95       NA      2
#> 96       NA      2
#> 97       NA      2
#> 98       NA      2
#> 99       NA      2
#> 100      NA      2
#> 101      NA      2
#> 102      NA      2
#> 103      NA      2
#> 104      NA      2
#> 105      NA      2
#> 106      NA      2
#> 107      NA      2
#> 108      NA      2
#> 109      NA      2
#> 110      NA      2
#> 111      NA      2
#> 112      NA      2
#> 113      NA      2
#> 114      NA      2
#> 115      NA      2
#> 116      NA      2
#> 117      NA      2
#> 118      NA      2
#> 119      NA      2
#> 120      NA      2
#> 121      NA      2
#> 122      NA      2
#> 123      NA      2
#> 124      NA      2
#> 125      NA      2
#> 126      NA      2
#> 127      NA      2
#> 128      NA      2
#> 129      NA      2
#> 130      NA      2
#> 131      NA      2
#> 132      NA      2
#> 133      NA      2
#> 134      NA      2
#> 135      NA      2
#> 136      NA      2
#> 137      NA      2
#> 138      NA      2
#> 139      NA      2
#> 140      NA      2
#> 141      NA      2
#> 142      NA      2
#> 143      NA      2
#> 144      NA      2
#> 145      NA      2
#> 146      NA      2
#> 147      NA      2
#> 148      NA      2
#> 149      NA      2
#> 150      NA      2
#> 151      NA      2
#> 152      NA      2
#> 153       0      3
#> 154       0      3
#> 155       0      3
#> 156       0      3
#> 157       0      3
#> 158       0      3
#> 159       0      3
#> 160       0      3
#> 161       0      3
#> 162       0      3
#> 163       0      3
#> 164       0      3
#> 165       0      3
#> 166       0      3
#> 167       0      3
#> 168       0      3
#> 169       0      3
#> 170       0      3
#> 171       0      3
#> 172       0      3
#> 173       0      3
#> 174       0      3
#> 175       0      3
#> 176       0      3
#> 177       0      3
#> 178       0      3
#> 179      NA      3
#> 180      NA      3
#> 181      NA      3
#> 182      NA      3
#> 183      NA      3
#> 184      NA      3
#> 185      NA      3
#> 186      NA      3
#> 187      NA      3
#> 188      NA      3
#> 189      NA      3
#> 190      NA      3
#> 191      NA      3
#> 192      NA      3
#> 193      NA      3
#> 194      NA      3
#> 195      NA      3
#> 196      NA      3
#> 197      NA      3
#> 198      NA      3
#> 199      NA      3
#> 200      NA      3
#> 201      NA      3
#> 202      NA      3
#> 203      NA      3
#> 204      NA      3
#> 205      NA      3
#> 206      NA      3
#> 207      NA      3
#> 208      NA      3
#> 209      NA      3
#> 210      NA      3
#> 211      NA      3
#> 212      NA      3
#> 213      NA      3
#> 214      NA      3
#> 215      NA      3
#> 216      NA      3
#> 217      NA      3
#> 218      NA      3
#> 219      NA      3
#> 220      NA      3
#> 221      NA      3
#> 222      NA      3
#> 223      NA      3
#> 224      NA      3
#> 225      NA      3
#> 226      NA      3
#> 227      NA      3
#> 228      NA      3
#> 229      NA      3
#> 230      NA      3
#> 231      NA      3
#> 232      NA      3
#> 233      NA      3
#> 234      NA      3
#> 235      NA      3
#> 236      NA      3
#> 237      NA      3
#> 238      NA      3
#> 239      NA      3
#> 240      NA      3
#> 241      NA      3
#> 242      NA      3
#> 243      NA      3
#> 244      NA      3
#> 245      NA      3
#> 246      NA      3
#> 247      NA      3
#> 248      NA      3
#> 249      NA      3
#> 250      NA      3
#> 251       0      4
#> 252       0      4
#> 253       0      4
#> 254       0      4
#> 255       0      4
#> 256       0      4
#> 257       0      4
#> 258       0      4
#> 259       0      4
#> 260       0      4
#> 261       0      4
#> 262       0      4
#> 263       0      4
#> 264      NA      4
#> 265      NA      4
#> 266      NA      4
#> 267      NA      4
#> 268      NA      4
#> 269      NA      4
#> 270      NA      4
#> 271      NA      4
#> 272      NA      4
#> 273      NA      4
#> 274      NA      4
#> 275      NA      4
#> 276      NA      4
#> 277      NA      4
#> 278      NA      4
#> 279      NA      4
#> 280      NA      4
#> 281      NA      4
#> 282      NA      4
#> 283      NA      4
#> 284      NA      4
#> 285      NA      4
#> 286      NA      4
#> 287      NA      4
#> 288      NA      4
#> 289      NA      4
#> 290      NA      4
#> 291      NA      4
#> 292      NA      4
#> 293      NA      4
#> 294      NA      4
#> 295      NA      4
#> 296      NA      4
#> 297      NA      4
#> 298      NA      4
#> 299      NA      4
#> 300      NA      4
#> 301      NA      4
#> 302      NA      4
#> 303      NA      4
#> 304      NA      4
#> 305      NA      4
#> 306      NA      4
#> 307      NA      4
#> 308      NA      4
#> 309      NA      4
#> 310      NA      4
#> 311      NA      4
#> 312      NA      4
#> 313      NA      4
#> 314      NA      4
#> 315      NA      4
#> 316      NA      4
#> 317      NA      4
#> 318      NA      4
#> 319      NA      4
#> 320      NA      4
#> 321      NA      4
#> 322      NA      4
#> 323      NA      4
#> 324      NA      4
#> 325      NA      4
#> 326      NA      4
#> 327      NA      4
#> 328      NA      4
#> 329      NA      4
#> 330      NA      4
#> 331      NA      4
#> 332       1      5
#> 333       0      5
#> 334       0      5
#> 335       0      5
#> 336       0      5
#> 337       0      5
#> 338       0      5
#> 339       0      5
#> 340       0      5
#> 341       0      5
#> 342       0      5
#> 343       0      5
#> 344       0      5
#> 345       0      5
#> 346       0      5
#> 347       0      5
#> 348       0      5
#> 349      NA      5
#> 350      NA      5
#> 351      NA      5
#> 352      NA      5
#> 353      NA      5
#> 354      NA      5
#> 355      NA      5
#> 356      NA      5
#> 357      NA      5
#> 358      NA      5
#> 359      NA      5
#> 360      NA      5
#> 361      NA      5
#> 362      NA      5
#> 363      NA      5
#> 364      NA      5
#> 365      NA      5
#> 366      NA      5
#> 367      NA      5
#> 368      NA      5
#> 369      NA      5
#> 370      NA      5
#> 371      NA      5
#> 372      NA      5
#> 373      NA      5
#> 374      NA      5
#> 375      NA      5
#> 376      NA      5
#> 377      NA      5
#> 378      NA      5
#> 379      NA      5
#> 380      NA      5
#> 381      NA      5
#> 382      NA      5
#> 383      NA      5
#> 384      NA      5
#> 385      NA      5
#> 386      NA      5
#> 387      NA      5
#> 388      NA      5
#> 389      NA      5
#> 390      NA      5
#> 391      NA      5
#> 392      NA      5
#> 393      NA      5
#> 394      NA      5
#> 395      NA      5
#> 396      NA      5
#> 397      NA      5
#> 398      NA      5
#> 399      NA      5
#> 400      NA      5
#> 401      NA      5
#> 402      NA      5
#> 403      NA      5
#> 404      NA      5
#> 405      NA      5
#> 406      NA      5
#> 407      NA      5
#> 408      NA      5
#> 409      NA      5
#> 410      NA      5
#> 411      NA      5
#> 412      NA      5
#> 413      NA      5
#> 414      NA      5
#> 415      NA      5
#> 416      NA      5
 Hb(data = minedata, id = "Region", x1 = "areal_381_eier_2015", x2 = "areal_381_eier_2014",
    pA = 0.1, strataName = "strata")
#> Warning: 67 % av enhetene har lik verdi i de to periodene. Dette er >=50 % og metoden kjører derfor kun på de
#>               enhetene som har to ulike verdier på x1 og x2.
#> Warning: 74 % av enhetene har lik verdi i de to periodene. Dette er >=50 % og metoden kjører derfor kun på de
#>               enhetene som har to ulike verdier på x1 og x2.
#> Warning: 67 % av enhetene har lik verdi i de to periodene. Dette er >=50 % og metoden kjører derfor kun på de
#>               enhetene som har to ulike verdier på x1 og x2.
#> Warning: 77 % av enhetene har lik verdi i de to periodene. Dette er >=50 % og metoden kjører derfor kun på de
#>               enhetene som har to ulike verdier på x1 og x2.
#> Warning: 70 % av enhetene har lik verdi i de to periodene. Dette er >=50 % og metoden kjører derfor kun på de
#>               enhetene som har to ulike verdier på x1 og x2.
#>         id    x1     x2   maxX     ratio lowerLimit upperLimit  medRatio
#> 1    10100  9020   9720   9720 0.9279835 0.67219848   2.984919 0.9976667
#> 2    10500 24417  18364  24417 1.3296123 0.76420818   2.251501 0.9976667
#> 3    10600 38167  35997  38167 1.0602828 0.80176170   2.000531 0.9976667
#> 4    11100  2790   3448   3448 0.8091647 0.55030204   4.334254 0.9976667
#> 5    12200  3169   2500   3169 1.2676000 0.53987023   4.478034 0.9976667
#> 6    13700  1298   1212   1298 1.0709571 0.42910905   6.435787 0.9976667
#> 7    21100 11929  11797  11929 1.0111893 0.69424097   2.791509 0.9976667
#> 8    21300 21799  22200  22200 0.9819369 0.75558961   2.312618 0.9976667
#> 9    21600 13678  21063  21063 0.6493852 0.75073761   2.347643 0.9976667
#> 10   22000 38503  36303  38503 1.0606011 0.80245074   1.996146 0.9976667
#> 11   23100 25607  24183  25607 1.0588843 0.76843609   2.222020 0.9976667
#> 12   23500 20951  21000  21000 0.9976667 0.75045920   2.349666 0.9976667
#> 13   23600  3425   3452   3452 0.9921784 0.55044509   4.332321 0.9976667
#> 14   23700 10788  10533  10788 1.0242096 0.68352374   2.883988 0.9976667
#> 15   40200  9400   9450   9450 0.9947090 0.66910215   3.013108 0.9976667
#> 16   40300  4296  20296  20296 0.2116673 0.74727522   2.372914 0.9976667
#> 17   44100  2852   2900   2900 0.9834483 0.52886453   4.635872 0.9976667
#> 18   10400     0     NA     NA        NA         NA         NA        NA
#> 19   11800  1025   1025     NA        NA         NA         NA        NA
#> 20   11900  1900   1900     NA        NA         NA         NA        NA
#> 21   12100     0      0     NA        NA         NA         NA        NA
#> 22   12300  2537   2537     NA        NA         NA         NA        NA
#> 23   12400  7549   7549     NA        NA         NA         NA        NA
#> 24   12500  4150   4150     NA        NA         NA         NA        NA
#> 25   12700     0      0     NA        NA         NA         NA        NA
#> 26   12800     0      0     NA        NA         NA         NA        NA
#> 27   13500  3520   3520     NA        NA         NA         NA        NA
#> 28   13600  7946   7946     NA        NA         NA         NA        NA
#> 29   13800   500    500     NA        NA         NA         NA        NA
#> 30   21400  6208   6208     NA        NA         NA         NA        NA
#> 31   21500  4350   4350     NA        NA         NA         NA        NA
#> 32   21700 11582  11582     NA        NA         NA         NA        NA
#> 33   21900 74171  74171     NA        NA         NA         NA        NA
#> 34   22100    NA     NA     NA        NA         NA         NA        NA
#> 35   22700  1470   1470     NA        NA         NA         NA        NA
#> 36   22800  7753   7753     NA        NA         NA         NA        NA
#> 37   22900  7585   7585     NA        NA         NA         NA        NA
#> 38   23000 22511  22511     NA        NA         NA         NA        NA
#> 39   23400    NA     NA     NA        NA         NA         NA        NA
#> 40   23800  3320   3320     NA        NA         NA         NA        NA
#> 41   23900  1989   1989     NA        NA         NA         NA        NA
#> 42   30100     0 141228     NA        NA         NA         NA        NA
#> 43   41200  6698   6698     NA        NA         NA         NA        NA
#> 44   41500  3020   3020     NA        NA         NA         NA        NA
#> 45   41700  3168     NA     NA        NA         NA         NA        NA
#> 46   41800  2862   2862     NA        NA         NA         NA        NA
#> 47   41900  3194   3194     NA        NA         NA         NA        NA
#> 48   42000  3405   3405     NA        NA         NA         NA        NA
#> 49   42300  2958   2958     NA        NA         NA         NA        NA
#> 50   42500  3700   3700     NA        NA         NA         NA        NA
#> 51   42600  1315   1315     NA        NA         NA         NA        NA
#> 52   42700  7213   7213     NA        NA         NA         NA        NA
#> 53   42800    NA      0     NA        NA         NA         NA        NA
#> 54   42900  1668   1668     NA        NA         NA         NA        NA
#> 55   43000  2092   2092     NA        NA         NA         NA        NA
#> 56   43200  1144   1144     NA        NA         NA         NA        NA
#> 57   43400  1600   1600     NA        NA         NA         NA        NA
#> 58   43600  1080   1080     NA        NA         NA         NA        NA
#> 59   43700  2450   2450     NA        NA         NA         NA        NA
#> 60   43800    NA     NA     NA        NA         NA         NA        NA
#> 61   43900  1182   1182     NA        NA         NA         NA        NA
#> 62   50200  9415   4765   9415 1.9758657 0.52560474   4.247417 1.0023529
#> 63   52100  2468   1996   2468 1.2364729 0.36165047   7.340478 1.0023529
#> 64   52900  4260   4250   4260 1.0023529 0.42681451   5.826589 1.0023529
#> 65   60500  8718  11464  11464 0.7604676 0.55013880   3.943152 1.0023529
#> 66   62000  3090   3245   3245 0.9522342 0.39384958   6.529818 1.0023529
#> 67   62600  7678   6506   7678 1.1801414 0.50007100   4.595784 1.0023529
#> 68   70100  8577   9029   9029 0.9499391 0.52036959   4.316056 1.0023529
#> 69   70200  3099   4606   4606 0.6728181 0.43640998   5.641855 1.0023529
#> 70   70400 29223  29917  29917 0.9768025 0.66432040   2.822785 1.0023529
#> 71   70900 13787  11321  13787 1.2178253 0.57291792   3.683981 1.0023529
#> 72   71300  5900   3900   5900 1.5128205 0.46712025   5.101631 1.0023529
#> 73   80600  9200   5000   9200 1.8400000 0.52271646   4.285115 1.0023529
#> 74   80700  5450   7875   7875 0.6920635 0.50324520   4.550553 1.0023529
#> 75   81500  5716   6017   6017 0.9499751 0.46957001   5.061580 1.0023529
#> 76   83100  1150   1000   1150 1.1500000 0.27879375  10.287407 1.0023529
#> 77   90400 10391   8970  10391 1.1584169 0.53791658   4.091259 1.0023529
#> 78   91900  9886   9477   9886 1.0431571 0.53170257   4.169171 1.0023529
#> 79   92800  2341   2451   2451 0.9551204 0.36085194   7.362420 1.0023529
#> 80   93500  1916   2001   2001 0.9575212 0.33777604   8.041335 1.0023529
#> 81   50100 19444  19444     NA        NA         NA         NA        NA
#> 82   51100  1780   1780     NA        NA         NA         NA        NA
#> 83   51200   748    748     NA        NA         NA         NA        NA
#> 84   51300  3100   3100     NA        NA         NA         NA        NA
#> 85   51400  2030   2030     NA        NA         NA         NA        NA
#> 86   51500  2400   2400     NA        NA         NA         NA        NA
#> 87   51600  4088   4088     NA        NA         NA         NA        NA
#> 88   51700  5900   5900     NA        NA         NA         NA        NA
#> 89   51900  4216   4216     NA        NA         NA         NA        NA
#> 90   52000  1990   1990     NA        NA         NA         NA        NA
#> 91   52200  2547   2547     NA        NA         NA         NA        NA
#> 92   52800  1405   1405     NA        NA         NA         NA        NA
#> 93   53200  2213   2213     NA        NA         NA         NA        NA
#> 94   53300    NA      0     NA        NA         NA         NA        NA
#> 95   53400  2773   2773     NA        NA         NA         NA        NA
#> 96   53600  2555   2555     NA        NA         NA         NA        NA
#> 97   53800  2587   2587     NA        NA         NA         NA        NA
#> 98   54000     0      0     NA        NA         NA         NA        NA
#> 99   54100  3100   3100     NA        NA         NA         NA        NA
#> 100  54200  5988   5988     NA        NA         NA         NA        NA
#> 101  54300   627    627     NA        NA         NA         NA        NA
#> 102  54400     0      0     NA        NA         NA         NA        NA
#> 103  54500     0      0     NA        NA         NA         NA        NA
#> 104  60400  6587      0     NA        NA         NA         NA        NA
#> 105  61200  1548   1548     NA        NA         NA         NA        NA
#> 106  61500  2570   2570     NA        NA         NA         NA        NA
#> 107  61600  2915   2915     NA        NA         NA         NA        NA
#> 108  61700  5800   5800     NA        NA         NA         NA        NA
#> 109  61800  2795   2795     NA        NA         NA         NA        NA
#> 110  61900  2295   2295     NA        NA         NA         NA        NA
#> 111  62100    NA     NA     NA        NA         NA         NA        NA
#> 112  62200     0     NA     NA        NA         NA         NA        NA
#> 113  62300  4507   4507     NA        NA         NA         NA        NA
#> 114  62400  6000   6000     NA        NA         NA         NA        NA
#> 115  62500  3718   3718     NA        NA         NA         NA        NA
#> 116  62700    NA     NA     NA        NA         NA         NA        NA
#> 117  62800    NA     NA     NA        NA         NA         NA        NA
#> 118  63100    NA    700     NA        NA         NA         NA        NA
#> 119  63200    NA      0     NA        NA         NA         NA        NA
#> 120  63300  2119   2119     NA        NA         NA         NA        NA
#> 121  70600  8211   8211     NA        NA         NA         NA        NA
#> 122  71100  3855   3855     NA        NA         NA         NA        NA
#> 123  71400  2027   2027     NA        NA         NA         NA        NA
#> 124  71600  8200   8200     NA        NA         NA         NA        NA
#> 125  71900  3240   3240     NA        NA         NA         NA        NA
#> 126  72000  4608   4608     NA        NA         NA         NA        NA
#> 127  72200  8382   8382     NA        NA         NA         NA        NA
#> 128  72300  1831   1831     NA        NA         NA         NA        NA
#> 129  72800   744    744     NA        NA         NA         NA        NA
#> 130  80500 19328  19328     NA        NA         NA         NA        NA
#> 131  81100     0      0     NA        NA         NA         NA        NA
#> 132  81400  3317   3317     NA        NA         NA         NA        NA
#> 133  81900  5575   5575     NA        NA         NA         NA        NA
#> 134  82100     0      0     NA        NA         NA         NA        NA
#> 135  82200   540    540     NA        NA         NA         NA        NA
#> 136  82600  4697   4697     NA        NA         NA         NA        NA
#> 137  82700    NA     NA     NA        NA         NA         NA        NA
#> 138  82800  1025   1025     NA        NA         NA         NA        NA
#> 139  82900  2700   2700     NA        NA         NA         NA        NA
#> 140  83000     0   1000     NA        NA         NA         NA        NA
#> 141  83300  2100   2100     NA        NA         NA         NA        NA
#> 142  83400 12410  12410     NA        NA         NA         NA        NA
#> 143  90100  5494   5494     NA        NA         NA         NA        NA
#> 144  91100   546    546     NA        NA         NA         NA        NA
#> 145  91200  2000   2000     NA        NA         NA         NA        NA
#> 146  91400     0     NA     NA        NA         NA         NA        NA
#> 147  92600  9036   9036     NA        NA         NA         NA        NA
#> 148  92900  2835   2835     NA        NA         NA         NA        NA
#> 149  93700   275    275     NA        NA         NA         NA        NA
#> 150  93800    NA      0     NA        NA         NA         NA        NA
#> 151  94000  3000   3000     NA        NA         NA         NA        NA
#> 152  94100     0   3094     NA        NA         NA         NA        NA
#> 153 100100 61868  52678  61868 1.1744561 0.52557688   3.320620 1.0087765
#> 154 100300 12099   6599  12099 1.8334596 0.32763420   6.236549 1.0087765
#> 155 101700  3800   3239   3800 1.1732016 0.21419405  10.337015 1.0087765
#> 156 101800  2606   4606   4606 0.5657838 0.23086939   9.481626 1.0087765
#> 157 102600  1355   3450   3450 0.3927536 0.20615617  10.798759 1.0087765
#> 158 102900  2673   3257   3257 0.8206939 0.20147504  11.084647 1.0087765
#> 159 104600  1400   1790   1790 0.7821229 0.15749793  14.600200 1.0087765
#> 160 110200 26391  20646  26391 1.2782621 0.41898759   4.548453 1.0087765
#> 161 110300 60935  59657  60935 1.0214225 0.52366387   3.338251 1.0087765
#> 162 112100  4132   8159   8159 0.5064346 0.28563829   7.374870 1.0087765
#> 163 113000  4168    712   4168 5.8539326 0.22209526   9.915697 1.0087765
#> 164 113500  5988   5225   5988 1.1460287 0.25505263   8.439833 1.0087765
#> 165 114900  8354   8390   8390 0.9957092 0.28850524   7.286620 1.0087765
#> 166 115100   450    500    500 0.9000000 0.08985459  26.724956 1.0087765
#> 167 120100 94310 101071 101071 0.9331064 0.58673673   2.817525 1.0087765
#> 168 122200   497   1290   1290 0.3852713 0.13693352  17.018971 1.0087765
#> 169 122400  2927   3327   3327 0.8797716 0.20319482  10.978085 1.0087765
#> 170 123300   200    150    200 1.3333333 0.05875248  41.669627 1.0087765
#> 171 123800  7310   7195   7310 1.0159833 0.27452420   7.734403 1.0087765
#> 172 124300 12075  12944  12944 0.9328646 0.33514527   6.063032 1.0087765
#> 173 125100  4911   3460   4911 1.4193642 0.23662646   9.214304 1.0087765
#> 174 126300 11486  11468  11486 1.0015696 0.32190945   6.374238 1.0087765
#> 175 142400  5950   6296   6296 0.9450445 0.25986132   8.255790 1.0087765
#> 176 142600  2999   1964   2999 1.5269857 0.19490418  11.509113 1.0087765
#> 177 143200 11500   6000  11500 1.9166667 0.32204296   6.370971 1.0087765
#> 178 144500  6639   3308   6639 2.0069528 0.26501105   8.066102 1.0087765
#> 179 100200  9039   9039     NA        NA         NA         NA        NA
#> 180 100400  5010   5010     NA        NA         NA         NA        NA
#> 181 101400  7426   7426     NA        NA         NA         NA        NA
#> 182 102100  2003   2003     NA        NA         NA         NA        NA
#> 183 102700   440    440     NA        NA         NA         NA        NA
#> 184 103200  1200   1200     NA        NA         NA         NA        NA
#> 185 103400    NA     NA     NA        NA         NA         NA        NA
#> 186 103700  3895   3895     NA        NA         NA         NA        NA
#> 187 110100  4631   4631     NA        NA         NA         NA        NA
#> 188 110600 13610  13610     NA        NA         NA         NA        NA
#> 189 111100   612    612     NA        NA         NA         NA        NA
#> 190 111400    NA     NA     NA        NA         NA         NA        NA
#> 191 111900     0     NA     NA        NA         NA         NA        NA
#> 192 112000  7170   7170     NA        NA         NA         NA        NA
#> 193 112200  4195   4195     NA        NA         NA         NA        NA
#> 194 112400 22700  22700     NA        NA         NA         NA        NA
#> 195 112700  6432   6432     NA        NA         NA         NA        NA
#> 196 112900    89     89     NA        NA         NA         NA        NA
#> 197 113300  1790   1790     NA        NA         NA         NA        NA
#> 198 113400  2803   2803     NA        NA         NA         NA        NA
#> 199 114100    NA      0     NA        NA         NA         NA        NA
#> 200 114200  4050   4050     NA        NA         NA         NA        NA
#> 201 114400    NA   1500     NA        NA         NA         NA        NA
#> 202 114500    NA     NA     NA        NA         NA         NA        NA
#> 203 114600  8550   8550     NA        NA         NA         NA        NA
#> 204 116000 10516  10516     NA        NA         NA         NA        NA
#> 205 121100   651    651     NA        NA         NA         NA        NA
#> 206 121600  1900   1900     NA        NA         NA         NA        NA
#> 207 121900  2975   2975     NA        NA         NA         NA        NA
#> 208 122100     0      0     NA        NA         NA         NA        NA
#> 209 122300  2200   2200     NA        NA         NA         NA        NA
#> 210 122700     0      0     NA        NA         NA         NA        NA
#> 211 122800  6398   6398     NA        NA         NA         NA        NA
#> 212 123100   600    600     NA        NA         NA         NA        NA
#> 213 123200  2207   2207     NA        NA         NA         NA        NA
#> 214 123400    NA      0     NA        NA         NA         NA        NA
#> 215 123500  6805   6805     NA        NA         NA         NA        NA
#> 216 124100    NA    600     NA        NA         NA         NA        NA
#> 217 124200   440    440     NA        NA         NA         NA        NA
#> 218 124400  3200   3200     NA        NA         NA         NA        NA
#> 219 124500  3658   3658     NA        NA         NA         NA        NA
#> 220 124600    NA  13973     NA        NA         NA         NA        NA
#> 221 124700 10257  10257     NA        NA         NA         NA        NA
#> 222 125200  2520   2520     NA        NA         NA         NA        NA
#> 223 125300  1874   1874     NA        NA         NA         NA        NA
#> 224 125600    NA     NA     NA        NA         NA         NA        NA
#> 225 125900  1121   1121     NA        NA         NA         NA        NA
#> 226 126000  3956   3956     NA        NA         NA         NA        NA
#> 227 126400  2248   2248     NA        NA         NA         NA        NA
#> 228 126500  1471   1471     NA        NA         NA         NA        NA
#> 229 126600  2700   2700     NA        NA         NA         NA        NA
#> 230 140100  4017   4017     NA        NA         NA         NA        NA
#> 231 141100     0      0     NA        NA         NA         NA        NA
#> 232 141200  1019   1019     NA        NA         NA         NA        NA
#> 233 141300  1568   1568     NA        NA         NA         NA        NA
#> 234 141600  4584   4584     NA        NA         NA         NA        NA
#> 235 141700  2019   2019     NA        NA         NA         NA        NA
#> 236 141800  2645   2645     NA        NA         NA         NA        NA
#> 237 141900    NA   1200     NA        NA         NA         NA        NA
#> 238 142000    NA     NA     NA        NA         NA         NA        NA
#> 239 142100  2570   2570     NA        NA         NA         NA        NA
#> 240 142200  3726   3726     NA        NA         NA         NA        NA
#> 241 142800    NA     NA     NA        NA         NA         NA        NA
#> 242 142900  2277   2277     NA        NA         NA         NA        NA
#> 243 143100     0      0     NA        NA         NA         NA        NA
#> 244 143300  1200   1200     NA        NA         NA         NA        NA
#> 245 143800  3506   3506     NA        NA         NA         NA        NA
#> 246 143900    NA     NA     NA        NA         NA         NA        NA
#> 247 144100    NA      0     NA        NA         NA         NA        NA
#> 248 144300  2800   2800     NA        NA         NA         NA        NA
#> 249 144400  2500   2500     NA        NA         NA         NA        NA
#> 250 144900  1307   1307     NA        NA         NA         NA        NA
#> 251 150500  9913   9910   9913 1.0003027 0.84214606   1.961046 1.0078740
#> 252 153900  4844   4917   4917 0.9851535 0.78775722   2.361266 1.0078740
#> 253 157300  1890   1692   1890 1.1170213 0.69475389   3.190819 1.0078740
#> 254 157600  3730   3066   3730 1.2165688 0.76306901   2.561761 1.0078740
#> 255 160100 42752  42418  42752 1.0078740 0.92063328   1.466856 1.0078740
#> 256 163400  2165   1952   2165 1.1091189 0.70922263   3.047473 1.0078740
#> 257 163500  3842   3872   3872 0.9922521 0.76651485   2.533001 1.0078740
#> 258 163800  4098   4870   4870 0.8414784 0.78692989   2.367781 1.0078740
#> 259 164800  3715   3915   3915 0.9489144 0.76752703   2.524603 1.0078740
#> 260 166300  7560   7516   7560 1.0058542 0.82252193   2.099347 1.0078740
#> 261 171400 20710  20506  20710 1.0099483 0.88709513   1.667327 1.0078740
#> 262 171900  2801   2500   2801 1.1204000 0.73555888   2.801026 1.0078740
#> 263 174000  2412   1240   2412 1.9451613 0.72044857   2.940221 1.0078740
#> 264 150400    NA     NA     NA        NA         NA         NA        NA
#> 265 151100  1856   1856     NA        NA         NA         NA        NA
#> 266 151400    NA     NA     NA        NA         NA         NA        NA
#> 267 151500    NA     NA     NA        NA         NA         NA        NA
#> 268 151700    NA   3008     NA        NA         NA         NA        NA
#> 269 151900  2000   2000     NA        NA         NA         NA        NA
#> 270 152000  2350   2350     NA        NA         NA         NA        NA
#> 271 152300  2262   2262     NA        NA         NA         NA        NA
#> 272 152500    NA   3024     NA        NA         NA         NA        NA
#> 273 152600  2450   2450     NA        NA         NA         NA        NA
#> 274 152800  3045   3045     NA        NA         NA         NA        NA
#> 275 152900  2470   2470     NA        NA         NA         NA        NA
#> 276 153100  3000   3000     NA        NA         NA         NA        NA
#> 277 153200  6968   6968     NA        NA         NA         NA        NA
#> 278 153400     0   2274     NA        NA         NA         NA        NA
#> 279 153500  6015   6015     NA        NA         NA         NA        NA
#> 280 154300    NA   1264     NA        NA         NA         NA        NA
#> 281 154500     0      0     NA        NA         NA         NA        NA
#> 282 154600  3118   3118     NA        NA         NA         NA        NA
#> 283 154700     0      0     NA        NA         NA         NA        NA
#> 284 154800  3000   3000     NA        NA         NA         NA        NA
#> 285 155100  6287   6287     NA        NA         NA         NA        NA
#> 286 155400  1825   1825     NA        NA         NA         NA        NA
#> 287 155700  3745   3745     NA        NA         NA         NA        NA
#> 288 156000  1500   1500     NA        NA         NA         NA        NA
#> 289 156300  5193   5193     NA        NA         NA         NA        NA
#> 290 156600  2100   2100     NA        NA         NA         NA        NA
#> 291 156700     0      0     NA        NA         NA         NA        NA
#> 292 157100  1715   1715     NA        NA         NA         NA        NA
#> 293 161200  5078   5078     NA        NA         NA         NA        NA
#> 294 161300   342    342     NA        NA         NA         NA        NA
#> 295 161700    NA     NA     NA        NA         NA         NA        NA
#> 296 162000  2044   2044     NA        NA         NA         NA        NA
#> 297 162100  3545   3545     NA        NA         NA         NA        NA
#> 298 162200  3300   3300     NA        NA         NA         NA        NA
#> 299 162400   920     NA     NA        NA         NA         NA        NA
#> 300 162700  3179   3179     NA        NA         NA         NA        NA
#> 301 163000    NA      0     NA        NA         NA         NA        NA
#> 302 163200    NA      0     NA        NA         NA         NA        NA
#> 303 163300  1082   1082     NA        NA         NA         NA        NA
#> 304 163600     0     NA     NA        NA         NA         NA        NA
#> 305 164000   382     NA     NA        NA         NA         NA        NA
#> 306 164400     0     NA     NA        NA         NA         NA        NA
#> 307 165300  9611   9611     NA        NA         NA         NA        NA
#> 308 165700  2500   2500     NA        NA         NA         NA        NA
#> 309 166200  2549   2549     NA        NA         NA         NA        NA
#> 310 166400  3235   3235     NA        NA         NA         NA        NA
#> 311 166500  3000   3000     NA        NA         NA         NA        NA
#> 312 170200     0      0     NA        NA         NA         NA        NA
#> 313 170300 11596  11596     NA        NA         NA         NA        NA
#> 314 171100  7100   7100     NA        NA         NA         NA        NA
#> 315 171700   200      0     NA        NA         NA         NA        NA
#> 316 171800  2225   2225     NA        NA         NA         NA        NA
#> 317 172100   500    500     NA        NA         NA         NA        NA
#> 318 172400     0      0     NA        NA         NA         NA        NA
#> 319 172500    NA     NA     NA        NA         NA         NA        NA
#> 320 173600     0      0     NA        NA         NA         NA        NA
#> 321 173800  4410   4410     NA        NA         NA         NA        NA
#> 322 173900  1586   1586     NA        NA         NA         NA        NA
#> 323 174200  2841   2841     NA        NA         NA         NA        NA
#> 324 174300    NA      0     NA        NA         NA         NA        NA
#> 325 174400  1916   1916     NA        NA         NA         NA        NA
#> 326 174800   288    288     NA        NA         NA         NA        NA
#> 327 174900  1768   1768     NA        NA         NA         NA        NA
#> 328 175000  4056   4056     NA        NA         NA         NA        NA
#> 329 175100  3000     NA     NA        NA         NA         NA        NA
#> 330 175500    NA      0     NA        NA         NA         NA        NA
#> 331 175600   800    800     NA        NA         NA         NA        NA
#> 332 180400 15263  22018  22018 0.6932056 0.69502998   1.982197 1.0006858
#> 333 181200  2200   1400   2200 1.5714286 0.41847727   4.105767 1.0006858
#> 334 182200   900   1000   1000 0.9000000 0.32664102   5.606266 1.0006858
#> 335 182600  1955   2074   2074 0.9426230 0.41131523   4.198697 1.0006858
#> 336 183500  1770   1700   1770 1.0411765 0.39226120   4.462452 1.0006858
#> 337 184800  2900   2207   2900 1.3140009 0.45243730   3.705176 1.0006858
#> 338 185700   320    300    320 1.0666667 0.21529843   9.142278 1.0006858
#> 339 186500 10360   7954  10360 1.3024893 0.60975848   2.431570 1.0006858
#> 340 186800  3321   3052   3321 1.0881389 0.46928611   3.527946 1.0006858
#> 341 190200 31859  25979  31859 1.2263367 0.73278294   1.816645 1.0006858
#> 342 193100  5837   5833   5837 1.0006858 0.53970683   2.906978 1.0006858
#> 343 193600  1898   2298   2298 0.8259356 0.42379205   4.038837 1.0006858
#> 344 194100  2023   2022   2023 1.0004946 0.40830287   4.238757 1.0006858
#> 345 200200  3910   3500   3910 1.1171429 0.48967067   3.329828 1.0006858
#> 346 201200  2649   2698   2698 0.9818384 0.44350532   3.804591 1.0006858
#> 347 202100  3262   3569   3569 0.9139815 0.47826908   3.438559 1.0006858
#> 348 202200  2011   2151   2151 0.9349140 0.41573775   4.140935 1.0006858
#> 349 180500 10530  10530     NA        NA         NA         NA        NA
#> 350 181100     0      0     NA        NA         NA         NA        NA
#> 351 181300  2920   2920     NA        NA         NA         NA        NA
#> 352 181500     0      0     NA        NA         NA         NA        NA
#> 353 181600   112    112     NA        NA         NA         NA        NA
#> 354 181800  1909   1909     NA        NA         NA         NA        NA
#> 355 182000  5845   5845     NA        NA         NA         NA        NA
#> 356 182400 11200  11200     NA        NA         NA         NA        NA
#> 357 182500   313    313     NA        NA         NA         NA        NA
#> 358 182700  2500   2500     NA        NA         NA         NA        NA
#> 359 182800     0      0     NA        NA         NA         NA        NA
#> 360 183200  3716   3716     NA        NA         NA         NA        NA
#> 361 183300 10751  10751     NA        NA         NA         NA        NA
#> 362 183400  1260   1260     NA        NA         NA         NA        NA
#> 363 183600     0      0     NA        NA         NA         NA        NA
#> 364 183700    NA   2315     NA        NA         NA         NA        NA
#> 365 183800     0   1600     NA        NA         NA         NA        NA
#> 366 183900     0      0     NA        NA         NA         NA        NA
#> 367 184000  3998   3998     NA        NA         NA         NA        NA
#> 368 184100  6132   6132     NA        NA         NA         NA        NA
#> 369 184500  2908     NA     NA        NA         NA         NA        NA
#> 370 185000    NA    580     NA        NA         NA         NA        NA
#> 371 185100    NA   3051     NA        NA         NA         NA        NA
#> 372 185200  1150   1150     NA        NA         NA         NA        NA
#> 373 185300     0      0     NA        NA         NA         NA        NA
#> 374 185400   950    950     NA        NA         NA         NA        NA
#> 375 185600    NA   1550     NA        NA         NA         NA        NA
#> 376 185900   500    500     NA        NA         NA         NA        NA
#> 377 186000 11380  11380     NA        NA         NA         NA        NA
#> 378 186600  5210   5210     NA        NA         NA         NA        NA
#> 379 187000  5932   5932     NA        NA         NA         NA        NA
#> 380 187100    NA   3833     NA        NA         NA         NA        NA
#> 381 187400    NA     NA     NA        NA         NA         NA        NA
#> 382 190300 14981  14981     NA        NA         NA         NA        NA
#> 383 191100  3750   3750     NA        NA         NA         NA        NA
#> 384 191300  4000   4000     NA        NA         NA         NA        NA
#> 385 191700  1500   1500     NA        NA         NA         NA        NA
#> 386 191900   593    593     NA        NA         NA         NA        NA
#> 387 192000  1963   1963     NA        NA         NA         NA        NA
#> 388 192200  5030   5030     NA        NA         NA         NA        NA
#> 389 192300  2164   2164     NA        NA         NA         NA        NA
#> 390 192400  5475     NA     NA        NA         NA         NA        NA
#> 391 192500  3650   3650     NA        NA         NA         NA        NA
#> 392 192600     0   1300     NA        NA         NA         NA        NA
#> 393 192700     0      0     NA        NA         NA         NA        NA
#> 394 192800     0      0     NA        NA         NA         NA        NA
#> 395 192900  2532   2532     NA        NA         NA         NA        NA
#> 396 193300  1800     NA     NA        NA         NA         NA        NA
#> 397 193800  2500   2500     NA        NA         NA         NA        NA
#> 398 193900  1200   1200     NA        NA         NA         NA        NA
#> 399 194000     0   2189     NA        NA         NA         NA        NA
#> 400 194200  2560   2560     NA        NA         NA         NA        NA
#> 401 194300     0      0     NA        NA         NA         NA        NA
#> 402 200400 19291  19291     NA        NA         NA         NA        NA
#> 403 201100    NA   2576     NA        NA         NA         NA        NA
#> 404 201400     0      0     NA        NA         NA         NA        NA
#> 405 201500     0    700     NA        NA         NA         NA        NA
#> 406 201700  1600   1600     NA        NA         NA         NA        NA
#> 407 201800  1572   1572     NA        NA         NA         NA        NA
#> 408 201900    NA   4513     NA        NA         NA         NA        NA
#> 409 202000  3790   3790     NA        NA         NA         NA        NA
#> 410 202300  2904   2904     NA        NA         NA         NA        NA
#> 411 202400  1820   1820     NA        NA         NA         NA        NA
#> 412 202500  2450   2450     NA        NA         NA         NA        NA
#> 413 202700   400    400     NA        NA         NA         NA        NA
#> 414 202800    NA   2900     NA        NA         NA         NA        NA
#> 415 203000    NA      0     NA        NA         NA         NA        NA
#> 416 211100  3400      0     NA        NA         NA         NA        NA
#>     outlier strata
#> 1         0      1
#> 2         0      1
#> 3         0      1
#> 4         0      1
#> 5         0      1
#> 6         0      1
#> 7         0      1
#> 8         0      1
#> 9         1      1
#> 10        0      1
#> 11        0      1
#> 12        0      1
#> 13        0      1
#> 14        0      1
#> 15        0      1
#> 16        1      1
#> 17        0      1
#> 18       NA      1
#> 19       NA      1
#> 20       NA      1
#> 21       NA      1
#> 22       NA      1
#> 23       NA      1
#> 24       NA      1
#> 25       NA      1
#> 26       NA      1
#> 27       NA      1
#> 28       NA      1
#> 29       NA      1
#> 30       NA      1
#> 31       NA      1
#> 32       NA      1
#> 33       NA      1
#> 34       NA      1
#> 35       NA      1
#> 36       NA      1
#> 37       NA      1
#> 38       NA      1
#> 39       NA      1
#> 40       NA      1
#> 41       NA      1
#> 42       NA      1
#> 43       NA      1
#> 44       NA      1
#> 45       NA      1
#> 46       NA      1
#> 47       NA      1
#> 48       NA      1
#> 49       NA      1
#> 50       NA      1
#> 51       NA      1
#> 52       NA      1
#> 53       NA      1
#> 54       NA      1
#> 55       NA      1
#> 56       NA      1
#> 57       NA      1
#> 58       NA      1
#> 59       NA      1
#> 60       NA      1
#> 61       NA      1
#> 62        0      2
#> 63        0      2
#> 64        0      2
#> 65        0      2
#> 66        0      2
#> 67        0      2
#> 68        0      2
#> 69        0      2
#> 70        0      2
#> 71        0      2
#> 72        0      2
#> 73        0      2
#> 74        0      2
#> 75        0      2
#> 76        0      2
#> 77        0      2
#> 78        0      2
#> 79        0      2
#> 80        0      2
#> 81       NA      2
#> 82       NA      2
#> 83       NA      2
#> 84       NA      2
#> 85       NA      2
#> 86       NA      2
#> 87       NA      2
#> 88       NA      2
#> 89       NA      2
#> 90       NA      2
#> 91       NA      2
#> 92       NA      2
#> 93       NA      2
#> 94       NA      2
#> 95       NA      2
#> 96       NA      2
#> 97       NA      2
#> 98       NA      2
#> 99       NA      2
#> 100      NA      2
#> 101      NA      2
#> 102      NA      2
#> 103      NA      2
#> 104      NA      2
#> 105      NA      2
#> 106      NA      2
#> 107      NA      2
#> 108      NA      2
#> 109      NA      2
#> 110      NA      2
#> 111      NA      2
#> 112      NA      2
#> 113      NA      2
#> 114      NA      2
#> 115      NA      2
#> 116      NA      2
#> 117      NA      2
#> 118      NA      2
#> 119      NA      2
#> 120      NA      2
#> 121      NA      2
#> 122      NA      2
#> 123      NA      2
#> 124      NA      2
#> 125      NA      2
#> 126      NA      2
#> 127      NA      2
#> 128      NA      2
#> 129      NA      2
#> 130      NA      2
#> 131      NA      2
#> 132      NA      2
#> 133      NA      2
#> 134      NA      2
#> 135      NA      2
#> 136      NA      2
#> 137      NA      2
#> 138      NA      2
#> 139      NA      2
#> 140      NA      2
#> 141      NA      2
#> 142      NA      2
#> 143      NA      2
#> 144      NA      2
#> 145      NA      2
#> 146      NA      2
#> 147      NA      2
#> 148      NA      2
#> 149      NA      2
#> 150      NA      2
#> 151      NA      2
#> 152      NA      2
#> 153       0      3
#> 154       0      3
#> 155       0      3
#> 156       0      3
#> 157       0      3
#> 158       0      3
#> 159       0      3
#> 160       0      3
#> 161       0      3
#> 162       0      3
#> 163       0      3
#> 164       0      3
#> 165       0      3
#> 166       0      3
#> 167       0      3
#> 168       0      3
#> 169       0      3
#> 170       0      3
#> 171       0      3
#> 172       0      3
#> 173       0      3
#> 174       0      3
#> 175       0      3
#> 176       0      3
#> 177       0      3
#> 178       0      3
#> 179      NA      3
#> 180      NA      3
#> 181      NA      3
#> 182      NA      3
#> 183      NA      3
#> 184      NA      3
#> 185      NA      3
#> 186      NA      3
#> 187      NA      3
#> 188      NA      3
#> 189      NA      3
#> 190      NA      3
#> 191      NA      3
#> 192      NA      3
#> 193      NA      3
#> 194      NA      3
#> 195      NA      3
#> 196      NA      3
#> 197      NA      3
#> 198      NA      3
#> 199      NA      3
#> 200      NA      3
#> 201      NA      3
#> 202      NA      3
#> 203      NA      3
#> 204      NA      3
#> 205      NA      3
#> 206      NA      3
#> 207      NA      3
#> 208      NA      3
#> 209      NA      3
#> 210      NA      3
#> 211      NA      3
#> 212      NA      3
#> 213      NA      3
#> 214      NA      3
#> 215      NA      3
#> 216      NA      3
#> 217      NA      3
#> 218      NA      3
#> 219      NA      3
#> 220      NA      3
#> 221      NA      3
#> 222      NA      3
#> 223      NA      3
#> 224      NA      3
#> 225      NA      3
#> 226      NA      3
#> 227      NA      3
#> 228      NA      3
#> 229      NA      3
#> 230      NA      3
#> 231      NA      3
#> 232      NA      3
#> 233      NA      3
#> 234      NA      3
#> 235      NA      3
#> 236      NA      3
#> 237      NA      3
#> 238      NA      3
#> 239      NA      3
#> 240      NA      3
#> 241      NA      3
#> 242      NA      3
#> 243      NA      3
#> 244      NA      3
#> 245      NA      3
#> 246      NA      3
#> 247      NA      3
#> 248      NA      3
#> 249      NA      3
#> 250      NA      3
#> 251       0      4
#> 252       0      4
#> 253       0      4
#> 254       0      4
#> 255       0      4
#> 256       0      4
#> 257       0      4
#> 258       0      4
#> 259       0      4
#> 260       0      4
#> 261       0      4
#> 262       0      4
#> 263       0      4
#> 264      NA      4
#> 265      NA      4
#> 266      NA      4
#> 267      NA      4
#> 268      NA      4
#> 269      NA      4
#> 270      NA      4
#> 271      NA      4
#> 272      NA      4
#> 273      NA      4
#> 274      NA      4
#> 275      NA      4
#> 276      NA      4
#> 277      NA      4
#> 278      NA      4
#> 279      NA      4
#> 280      NA      4
#> 281      NA      4
#> 282      NA      4
#> 283      NA      4
#> 284      NA      4
#> 285      NA      4
#> 286      NA      4
#> 287      NA      4
#> 288      NA      4
#> 289      NA      4
#> 290      NA      4
#> 291      NA      4
#> 292      NA      4
#> 293      NA      4
#> 294      NA      4
#> 295      NA      4
#> 296      NA      4
#> 297      NA      4
#> 298      NA      4
#> 299      NA      4
#> 300      NA      4
#> 301      NA      4
#> 302      NA      4
#> 303      NA      4
#> 304      NA      4
#> 305      NA      4
#> 306      NA      4
#> 307      NA      4
#> 308      NA      4
#> 309      NA      4
#> 310      NA      4
#> 311      NA      4
#> 312      NA      4
#> 313      NA      4
#> 314      NA      4
#> 315      NA      4
#> 316      NA      4
#> 317      NA      4
#> 318      NA      4
#> 319      NA      4
#> 320      NA      4
#> 321      NA      4
#> 322      NA      4
#> 323      NA      4
#> 324      NA      4
#> 325      NA      4
#> 326      NA      4
#> 327      NA      4
#> 328      NA      4
#> 329      NA      4
#> 330      NA      4
#> 331      NA      4
#> 332       1      5
#> 333       0      5
#> 334       0      5
#> 335       0      5
#> 336       0      5
#> 337       0      5
#> 338       0      5
#> 339       0      5
#> 340       0      5
#> 341       0      5
#> 342       0      5
#> 343       0      5
#> 344       0      5
#> 345       0      5
#> 346       0      5
#> 347       0      5
#> 348       0      5
#> 349      NA      5
#> 350      NA      5
#> 351      NA      5
#> 352      NA      5
#> 353      NA      5
#> 354      NA      5
#> 355      NA      5
#> 356      NA      5
#> 357      NA      5
#> 358      NA      5
#> 359      NA      5
#> 360      NA      5
#> 361      NA      5
#> 362      NA      5
#> 363      NA      5
#> 364      NA      5
#> 365      NA      5
#> 366      NA      5
#> 367      NA      5
#> 368      NA      5
#> 369      NA      5
#> 370      NA      5
#> 371      NA      5
#> 372      NA      5
#> 373      NA      5
#> 374      NA      5
#> 375      NA      5
#> 376      NA      5
#> 377      NA      5
#> 378      NA      5
#> 379      NA      5
#> 380      NA      5
#> 381      NA      5
#> 382      NA      5
#> 383      NA      5
#> 384      NA      5
#> 385      NA      5
#> 386      NA      5
#> 387      NA      5
#> 388      NA      5
#> 389      NA      5
#> 390      NA      5
#> 391      NA      5
#> 392      NA      5
#> 393      NA      5
#> 394      NA      5
#> 395      NA      5
#> 396      NA      5
#> 397      NA      5
#> 398      NA      5
#> 399      NA      5
#> 400      NA      5
#> 401      NA      5
#> 402      NA      5
#> 403      NA      5
#> 404      NA      5
#> 405      NA      5
#> 406      NA      5
#> 407      NA      5
#> 408      NA      5
#> 409      NA      5
#> 410      NA      5
#> 411      NA      5
#> 412      NA      5
#> 413      NA      5
#> 414      NA      5
#> 415      NA      5
#> 416      NA      5
```
