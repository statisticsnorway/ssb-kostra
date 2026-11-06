# OUTDATED FUNCTION: Table suppression according to a frequency rule following the standards in the Kostra project.

This function is outdated, has limited functionality, and uses a
specialized interface influenced by both the former KOSTRA/KOMPIS
standards and the design and capabilities of the R package sdcTable.
Instead, functions from the R package GaussSuppression are recommended.

## Usage

``` r
ProtectKostra(
  data,
  idVar = 1,
  strataVar = NULL,
  freqVar = 2,
  freqVarGroup = NULL,
  protectZeros = TRUE,
  maxN = 3,
  method = "Gauss",
  output = "suppressed",
  total = "Total",
  split = "_",
  singleTotal = FALSE,
  ...
)
```

## Arguments

- data:

  Input data set of class data.frame

- idVar:

  Id-variable (name or number)

- strataVar:

  Strata-variable(s) (name or number)

- freqVar:

  Variable(s) holding counts (name or number)

- freqVarGroup:

  NULL (default) or integer representing groups of variables (see
  details)

- protectZeros:

  When TRUE empty cells (count=0) is considered sensitive

- maxN:

  All cells having counts \<= maxN are set as primary suppressed

- method:

  Parameter "method" in ProtectTable: Only "Gauss" possible (only-Gauss
  replacement function )

- output:

  One of "suppressed" (default), "freq", "sdcStatus" or "extraWide"
  (only when freqVarGroup is NULL)

- total:

  String used to name totals.

- split:

  Parameter to `AutoSplit` - see varNames and rowData above. When NULL
  automatic splitting without needing a split string.

- singleTotal:

  When TRUE identical rowsums in all freqVarGroups needed. When FALSE
  totals for each freqVarGroup will be in output.

- ...:

  Additional variables that will be included in output (name or number).

## Value

A data.frame with as many rows as input

## Details

**When freqVarGroup is NULL:**

This function is a wrapper to
[`ProtectTable`](https://statisticsnorway.github.io/ssb-kostra/reference/ProtectTable.md)
with dimVar=c(idVar, strataVar). The function
[`GetData`](https://statisticsnorway.github.io/ssb-kostra/reference/GetData.md)
is used.

Note that the names of output variables are strange when a single
freqVar variable is input. This can be fixed by using `freqVarGroup=1`
instead of NULL.

**When freqVarGroup is NOT NULL:**

The suppression function (as when freqVarGroup is NULL) is run several
times according to the groups with `freqVarGroup>0`. We have to types of
groups: Single variables and several variables. All groups of several
variables must have identical rowsums.

Variables with `freqVarGroup<1` will be included in output sorted as
input.

A warning is produced if generated total-output is not unique. Only the
first result is then returned. In the case of `output="suppressed"` this
means that the suppressions of the total has been is different. In the
case of `output="sdcStatus"` only coding may have been different.

## Note

Even if freq-variables with `freqVarGroup<1` is not used they will be
read by
[`GetData`](https://statisticsnorway.github.io/ssb-kostra/reference/GetData.md)
together with the other the freq-variables variables into a matrix. Use
a common numeric type for all these variables to prevent change of data
type.

All codes in `idVar` and `strataVar` must be unique. If not, automatic
re-coding will be done with a warning. Using `addName=TRUE` in input
will prevent this warning. Anyway, when `output="extraWide"` non-unique
codes produce problematic output.

Normally a value is only safe if sdcStatus="s". When using tau-argus
sdcStatus="z" is also safe when protectZeros="FALSE". But currently
tau-argus methods are not allowed in ProtectKostra. Use a simpler
(binary) coding of "sdcStatus" in future version? When the underlying
function
[`ProtectTable`](https://statisticsnorway.github.io/ssb-kostra/reference/ProtectTable.md)
results in error: sdcStatus="e".

## Author

Øyvind Langsrud

## Examples

``` r

 # ==================================
 #    Examples without freqVarGroup
 # ==================================

 # ==== Example 1 , 8 regions ====
 z1w = KostraData("z1w")
 ProtectKostra(z1w,idVar="region",freqVar=2:5)
#> .....
#> GaussSuppression_anySum: ..........................................
#>   region annet arbeid soshjelp trygd Total
#> 1 011100    11     11       55    36   113
#> 2 043200    NA     NA       29    18    55
#> 3 051300     5      8       35    25    73
#> 4 051400    NA     NA       17    13    45
#> 5 061900     9     14       63    52   138
#> 6 062000    12      9       24    22    67
#> 7 083300     6      4       22     8    40
#> 8 083400    NA     NA       38    15    65
#> 9  Total    72     52      283   189   596

 # ==== Example 2 , 11 regions ====
 z2w <- KostraData("z2w")
 ProtectKostra(z2w,idVar="region",strataVar=c("fylke","kostragr"),freqVar=4:7)
#> .....
#> GaussSuppression_anySum: ............................
#>    region annet arbeid soshjelp trygd Total
#> 1  011100    NA     NA       55    NA   113
#> 2  043200     7     NA       29    NA    55
#> 3  051300    NA     NA       35    25    73
#> 4  051400    NA     NA       17    13    45
#> 5  061900     9     14       63    52   138
#> 6  062000    12      9       24    22    67
#> 7  083300    NA     NA       22     8    40
#> 8  083400    NA     NA       38    15    65
#> 9  012100    NA     NA        9    NA    14
#> 10 102100    NA     NA       32    20    61
#> 11 102700    NA     NA       18    NA    35
#> 12   0100    14     11       64    38   127
#> 13   0400     7     NA       29    NA    55
#> 14   0500    18     10       52    38   118
#> 15   0600    21     23       87    74   205
#> 16   0800    15      7       60    23   105
#> 17   1000    13     NA       50    NA    96
#> 18     03    NA     NA      283   189   596
#> 19     04    NA     NA       59    33   110
#> 20  Total    88     54      342   222   706

 # ==== Example 3 , 36 regions ====
 z3w <- KostraData("z3w")
 ProtectKostra(z3w,idVar="region",strataVar=c("fylke","kostragr"),freqVar=4:15)
#> .....
#> GaussSuppression_anySum: .........................
#>    region annet_m01m05 arbeid_m01m05 soshjelp_m01m05 trygd_m01m05 annet_m10m12
#> 1  111100           12             4              32           25           NA
#> 2  111200           NA            NA              20           15           NA
#> 3  111400           NA             4              11           NA           NA
#> 4  113300           NA            NA              16           16           NA
#> 5  113500            8            13              33           42           NA
#> 6  114100           NA            NA               8           19           NA
#> 7  114200            5             8              39           27           NA
#> 8  114400           NA            NA              NA           NA           NA
#> 9  114500           NA            NA              NA           NA           NA
#> 10 115100           NA            NA              NA           NA           NA
#> 11 121100           NA            NA              36           33           NA
#> 12 122200            6             5              25           22           NA
#> 13 122300           NA            NA              12           15           NA
#> 14 123100            6             5              22           11           NA
#> 15 123400           NA            NA              17            8           NA
#> 16 124100           NA            NA              15           NA           NA
#> 17 124200           NA            NA              22           13           NA
#> 18 124400           NA            NA               7           NA           NA
#> 19 125100            9             6              34           28           NA
#> 20 125900           23            14              79           48           NA
#> 21 126400           NA            NA              27           19           NA
#> 22 126500           NA            NA               4           NA           NA
#> 23 141100           NA            NA              NA           NA           NA
#> 24 141200           NA            NA              NA           NA           NA
#> 25 141300           NA            NA              NA           NA           NA
#> 26 141600           10             9              40           42           NA
#> 27 141700           NA            NA              20           NA           NA
#> 28 141800           NA            NA               6            7           NA
#> 29 141900           NA            NA              18           13           NA
#> 30 142800            5             4              37           23           NA
#> 31 142900           NA            NA              18           14           NA
#> 32 143000           NA            NA              11           NA           NA
#> 33 143100           NA            NA              25           17           NA
#> 34 143300            6            10              22           12           NA
#> 35 144100           NA            NA              12           11           NA
#> 36 144400           NA            NA              NA           NA           NA
#> 37   1100           35            51             165          156           11
#> 38   1200           72            56             300          221            8
#> 39   1400           45            54             221          168            7
#> 40     01           23            17             117           92           NA
#> 41     02           45            66             248          187           13
#> 42     03           71            69             276          238            9
#> 43     04           NA            NA               6            4           NA
#> 44     05           NA            NA              39           24           NA
#> 45  Total          152           161             686          545           26
#>    arbeid_m10m12 soshjelp_m10m12 trygd_m10m12 annet_m06m09 arbeid_m06m09
#> 1             NA              NA           NA           NA            NA
#> 2             NA               7           NA            4            NA
#> 3             NA              NA           NA           NA            NA
#> 4             NA              NA           NA           NA            NA
#> 5             NA               7           NA           NA            NA
#> 6             NA              NA           NA           NA            NA
#> 7             NA               9            6           NA            NA
#> 8             NA              NA           NA           NA            NA
#> 9             NA              NA           NA           NA            NA
#> 10            NA              NA           NA           NA            NA
#> 11            NA              NA            4           NA            NA
#> 12            NA              NA           NA           NA            NA
#> 13            NA               5           NA           NA            NA
#> 14            NA              NA           NA           NA            NA
#> 15            NA              NA           NA           NA            NA
#> 16            NA              NA           NA           NA            NA
#> 17            NA              NA           NA           NA            NA
#> 18            NA               5           NA           NA            NA
#> 19            NA               5           NA           NA            NA
#> 20            NA              14            7           NA            NA
#> 21            NA              NA            5           NA            NA
#> 22            NA              NA           NA           NA            NA
#> 23            NA              NA           NA           NA            NA
#> 24            NA              NA           NA           NA            NA
#> 25            NA              NA           NA           NA            NA
#> 26            NA              11           NA           NA            NA
#> 27            NA              NA           NA           NA            NA
#> 28            NA              NA           NA           NA            NA
#> 29            NA               4           NA           NA            NA
#> 30            NA              NA           NA           NA            NA
#> 31            NA              NA           NA           NA            NA
#> 32            NA              NA           NA           NA            NA
#> 33             6              NA           NA           NA            NA
#> 34            NA               4           NA           NA            NA
#> 35            NA              NA           NA           NA            NA
#> 36            NA              NA           NA           NA            NA
#> 37            10              33           16           21            14
#> 38            13              49           30           18            23
#> 39            18              43           16           13            13
#> 40            NA              13            8           NA            NA
#> 41            17              47           31           21            19
#> 42            18              51           16           17            23
#> 43            NA              NA           NA           NA            NA
#> 44            NA              NA           NA           NA             5
#> 45            41             125           62           52            50
#>    soshjelp_m06m09 trygd_m06m09 annet arbeid soshjelp trygd m01m05 m06m09
#> 1               NA           NA    16      5       37    28     73     NA
#> 2                5           NA     8      9       32    21     43     11
#> 3               NA           NA     7      7       14     8     24      8
#> 4               NA           NA    NA     NA       22    22     45      8
#> 5               12           NA    13     21       52    51     96     29
#> 6               NA           NA     6      5       11    24     34      7
#> 7                7            7    10     12       55    40     79     20
#> 8               NA           NA    NA     NA       NA    NA     NA     NA
#> 9               NA           NA    NA     NA       NA    NA     10     NA
#> 10              NA           NA    NA     NA       NA    NA     NA     NA
#> 11              NA            4     8      4       46    41     77     14
#> 12              NA           NA     8     10       33    27     58     14
#> 13               4           NA     6      7       21    22     37     10
#> 14              NA           NA    11      7       29    16     44     11
#> 15              NA           NA     5     NA       22    NA     27      7
#> 16              NA           NA    NA     NA       21    13     26     NA
#> 17              NA           NA    NA     NA       24    14     39     NA
#> 18               4           NA     9      8       16    15     37     NA
#> 19               7           NA    12     13       46    36     77     20
#> 20               7           13    26     26      100    68    164     31
#> 21              NA            7     4     10       33    31     56     11
#> 22              NA           NA    NA     NA        5    NA      7     NA
#> 23              NA           NA    NA     NA        9     6     12     NA
#> 24              NA           NA    NA     NA       NA    NA      5     NA
#> 25              NA           NA    NA     NA        8    NA     NA     NA
#> 26              10           NA    16     11       61    47    101     16
#> 27              NA           NA     4      6       24    16     37      8
#> 28              NA           NA     4      6       10     8     20     NA
#> 29               8           NA     4      8       30    18     40     12
#> 30              NA           NA    NA     NA       46    32     69     13
#> 31              NA           NA    NA     NA       20    16     41     NA
#> 32              NA           NA    NA     NA       17     8     23     NA
#> 33              NA           NA     8     12       27    24     49     10
#> 34               5           NA     8     12       31    15     50     11
#> 35              NA           NA    NA     NA       16    12     30     NA
#> 36              NA           NA    NA     NA       NA    NA     NA     NA
#> 37              34           30    67     75      232   202    407     99
#> 38              47           46    98     92      396   297    649    134
#> 39              42           23    65     85      306   207    488     91
#> 40              15           13    34     24      145   113    249     41
#> 41              46           38    79    102      341   256    546    124
#> 42              51           42    97    110      378   296    654    133
#> 43              NA           NA    NA     NA       14     6     14      5
#> 44              NA           NA    NA     NA       56    35     81     21
#> 45             123           99   230    252      934   706   1544    324
#>    m10m12 Total
#> 1      NA    86
#> 2      16    70
#> 3       4    36
#> 4       7    60
#> 5      12   137
#> 6       5    46
#> 7      18   117
#> 8      NA    NA
#> 9      NA    18
#> 10     NA    NA
#> 11      8    99
#> 12      6    78
#> 13      9    56
#> 14      8    63
#> 15      7    41
#> 16     NA    37
#> 17     NA    46
#> 18     NA    48
#> 19     10   107
#> 20     25   220
#> 21     11    78
#> 22     NA    10
#> 23     NA    18
#> 24     NA     7
#> 25     NA    15
#> 26     18   135
#> 27      5    50
#> 28     NA    28
#> 29      8    60
#> 30     12    94
#> 31     NA    47
#> 32     NA    31
#> 33     12    71
#> 34      5    66
#> 35     NA    37
#> 36     NA     4
#> 37     70   576
#> 38    100   883
#> 39     84   663
#> 40     26   316
#> 41    108   778
#> 42     94   881
#> 43      6    25
#> 44     20   122
#> 45    254  2122

 #  ==== Example 3b , 36 regions == with three level column name coding
 z3wb <- KostraData("z3wb")
 ProtectKostra(z3wb,idVar="region",strataVar=c("fylke","kostragr"),freqVar=4:15)
#> .....
#> GaussSuppression_anySum: .........................
#>    region annet_m01m05_M01M05 arbeid_m01m05_M01M05 soshjelp_m01m05_M01M05
#> 1  111100                  12                   NA                     32
#> 2  111200                  NA                   NA                     20
#> 3  111400                  NA                   NA                     NA
#> 4  113300                  NA                   NA                     16
#> 5  113500                   8                   13                     33
#> 6  114100                  NA                   NA                     NA
#> 7  114200                   5                    8                     39
#> 8  114400                  NA                   NA                     NA
#> 9  114500                  NA                   NA                      6
#> 10 115100                  NA                   NA                     NA
#> 11 121100                  NA                   NA                     36
#> 12 122200                  NA                   NA                     25
#> 13 122300                  NA                   NA                     NA
#> 14 123100                  NA                   NA                     22
#> 15 123400                  NA                   NA                     17
#> 16 124100                  NA                   NA                     15
#> 17 124200                  NA                   NA                     NA
#> 18 124400                  NA                   NA                     NA
#> 19 125100                  NA                   NA                     34
#> 20 125900                  NA                   NA                     79
#> 21 126400                  NA                   NA                     27
#> 22 126500                  NA                   NA                     NA
#> 23 141100                  NA                   NA                      5
#> 24 141200                  NA                   NA                     NA
#> 25 141300                  NA                   NA                     NA
#> 26 141600                  NA                   NA                     40
#> 27 141700                  NA                   NA                     20
#> 28 141800                  NA                   NA                      6
#> 29 141900                  NA                   NA                     18
#> 30 142800                   5                    4                     37
#> 31 142900                  NA                    6                     NA
#> 32 143000                  NA                   NA                     11
#> 33 143100                   4                   NA                     NA
#> 34 143300                  NA                   NA                     22
#> 35 144100                  NA                    6                     12
#> 36 144400                  NA                   NA                     NA
#> 37   1100                  35                   51                    165
#> 38   1200                  72                   56                    300
#> 39   1400                  45                   54                    221
#> 40     01                  23                   17                    117
#> 41     02                  45                   66                    248
#> 42     03                  71                   69                    276
#> 43     04                  NA                   NA                      6
#> 44     05                  NA                   NA                     39
#> 45  Total                 152                  161                    686
#>    trygd_m01m05_M01M05 annet_m10m12_M06M12 arbeid_m10m12_M06M12
#> 1                   NA                  NA                   NA
#> 2                   15                  NA                   NA
#> 3                   NA                  NA                   NA
#> 4                   16                  NA                   NA
#> 5                   42                  NA                   NA
#> 6                   19                  NA                   NA
#> 7                   27                  NA                   NA
#> 8                   NA                  NA                   NA
#> 9                   NA                  NA                   NA
#> 10                  NA                  NA                   NA
#> 11                  33                  NA                   NA
#> 12                  22                  NA                   NA
#> 13                  15                  NA                   NA
#> 14                  11                  NA                   NA
#> 15                  NA                  NA                   NA
#> 16                   9                  NA                   NA
#> 17                  NA                  NA                   NA
#> 18                  NA                  NA                   NA
#> 19                  28                  NA                   NA
#> 20                  48                  NA                   NA
#> 21                  19                  NA                   NA
#> 22                  NA                  NA                   NA
#> 23                  NA                  NA                   NA
#> 24                  NA                  NA                   NA
#> 25                  NA                  NA                   NA
#> 26                  42                  NA                   NA
#> 27                  12                  NA                   NA
#> 28                  NA                  NA                   NA
#> 29                  13                  NA                   NA
#> 30                  23                  NA                   NA
#> 31                  NA                  NA                   NA
#> 32                  NA                  NA                   NA
#> 33                  17                  NA                    6
#> 34                  NA                  NA                   NA
#> 35                  NA                  NA                   NA
#> 36                  NA                  NA                   NA
#> 37                 156                  11                   10
#> 38                 221                   8                   13
#> 39                 168                   7                   18
#> 40                  92                  NA                   NA
#> 41                 187                  13                   17
#> 42                 238                   9                   18
#> 43                  NA                  NA                   NA
#> 44                  NA                  NA                   NA
#> 45                 545                  26                   41
#>    soshjelp_m10m12_M06M12 trygd_m10m12_M06M12 annet_m06m09_M06M12
#> 1                      NA                  NA                  NA
#> 2                       7                  NA                   4
#> 3                      NA                  NA                  NA
#> 4                      NA                  NA                  NA
#> 5                       7                  NA                  NA
#> 6                      NA                  NA                  NA
#> 7                       9                   6                  NA
#> 8                      NA                  NA                  NA
#> 9                      NA                  NA                  NA
#> 10                     NA                  NA                  NA
#> 11                     NA                   4                  NA
#> 12                     NA                  NA                  NA
#> 13                      5                  NA                  NA
#> 14                     NA                  NA                   4
#> 15                     NA                  NA                  NA
#> 16                     NA                  NA                  NA
#> 17                     NA                  NA                  NA
#> 18                      5                  NA                  NA
#> 19                      5                  NA                  NA
#> 20                     14                   7                  NA
#> 21                     NA                   5                  NA
#> 22                     NA                  NA                  NA
#> 23                     NA                  NA                  NA
#> 24                     NA                  NA                  NA
#> 25                     NA                  NA                  NA
#> 26                     11                  NA                  NA
#> 27                     NA                  NA                  NA
#> 28                     NA                  NA                  NA
#> 29                      4                  NA                  NA
#> 30                     NA                  NA                  NA
#> 31                     NA                  NA                  NA
#> 32                     NA                  NA                  NA
#> 33                     NA                  NA                  NA
#> 34                      4                  NA                  NA
#> 35                     NA                  NA                  NA
#> 36                     NA                  NA                  NA
#> 37                     33                  16                  21
#> 38                     49                  30                  18
#> 39                     43                  16                  13
#> 40                     13                   8                  NA
#> 41                     47                  31                  21
#> 42                     51                  16                  17
#> 43                     NA                  NA                  NA
#> 44                     NA                  NA                  NA
#> 45                    125                  62                  52
#>    arbeid_m06m09_M06M12 soshjelp_m06m09_M06M12 trygd_m06m09_M06M12 annet_M01M05
#> 1                    NA                     NA                  NA           12
#> 2                    NA                      5                  NA           NA
#> 3                    NA                     NA                  NA           NA
#> 4                    NA                     NA                  NA           NA
#> 5                    NA                     12                  NA            8
#> 6                    NA                     NA                  NA           NA
#> 7                    NA                      7                   7            5
#> 8                    NA                     NA                  NA           NA
#> 9                    NA                     NA                  NA           NA
#> 10                   NA                     NA                  NA           NA
#> 11                   NA                     NA                   4           NA
#> 12                    4                     NA                  NA           NA
#> 13                   NA                     NA                  NA           NA
#> 14                   NA                     NA                  NA           NA
#> 15                   NA                     NA                  NA           NA
#> 16                   NA                     NA                  NA           NA
#> 17                   NA                     NA                  NA           NA
#> 18                   NA                     NA                  NA           NA
#> 19                    4                      7                  NA           NA
#> 20                   NA                      7                  13           NA
#> 21                   NA                     NA                   7           NA
#> 22                   NA                     NA                  NA           NA
#> 23                   NA                     NA                  NA           NA
#> 24                   NA                     NA                  NA           NA
#> 25                   NA                     NA                  NA           NA
#> 26                   NA                     10                  NA           NA
#> 27                   NA                     NA                  NA           NA
#> 28                   NA                     NA                  NA           NA
#> 29                   NA                      8                  NA           NA
#> 30                   NA                     NA                  NA            5
#> 31                   NA                     NA                  NA           NA
#> 32                   NA                     NA                  NA           NA
#> 33                   NA                     NA                  NA            4
#> 34                   NA                      5                  NA           NA
#> 35                   NA                     NA                  NA           NA
#> 36                   NA                     NA                  NA           NA
#> 37                   14                     34                  30           35
#> 38                   23                     47                  46           72
#> 39                   13                     42                  23           45
#> 40                   NA                     15                  13           23
#> 41                   19                     46                  38           45
#> 42                   23                     51                  42           71
#> 43                   NA                     NA                  NA           NA
#> 44                    5                     NA                  NA           NA
#> 45                   50                    123                  99          152
#>    annet_M06M12 annet arbeid_M01M05 arbeid_M06M12 arbeid soshjelp_M01M05
#> 1             4    16            NA            NA      5              32
#> 2            NA     8            NA            NA      9              20
#> 3            NA     7            NA            NA      7              NA
#> 4            NA    NA            NA            NA     NA              16
#> 5             5    13            13             8     21              33
#> 6            NA     6            NA            NA      5              NA
#> 7             5    10             8             4     12              39
#> 8            NA    NA            NA            NA     NA              NA
#> 9            NA    NA            NA            NA     NA               6
#> 10           NA    NA            NA            NA     NA              NA
#> 11           NA     8            NA            NA      4              36
#> 12           NA     8            NA            NA     10              25
#> 13           NA     6            NA            NA      7              NA
#> 14           NA    11            NA            NA      7              22
#> 15           NA     5            NA            NA     NA              17
#> 16           NA    NA            NA            NA     NA              15
#> 17           NA    NA            NA            NA     NA              NA
#> 18           NA     9            NA            NA      8              NA
#> 19           NA    12            NA            NA     13              34
#> 20           NA    26            NA            NA     26              79
#> 21           NA     4            NA            NA     10              27
#> 22           NA    NA            NA            NA     NA              NA
#> 23           NA    NA            NA            NA     NA               5
#> 24           NA    NA            NA            NA     NA              NA
#> 25           NA    NA            NA            NA     NA              NA
#> 26           NA    16            NA            NA     11              40
#> 27           NA     4            NA            NA      6              20
#> 28           NA     4            NA            NA      6               6
#> 29           NA     4            NA            NA      8              18
#> 30           NA    NA             4            NA     NA              37
#> 31           NA    NA             6            NA     NA              NA
#> 32           NA    NA            NA            NA     NA              11
#> 33            4     8            NA            NA     12              NA
#> 34           NA     8            NA            NA     12              22
#> 35           NA    NA             6            NA     NA              12
#> 36           NA    NA            NA            NA     NA              NA
#> 37           32    67            51            24     75             165
#> 38           26    98            56            36     92             300
#> 39           20    65            54            31     85             221
#> 40           11    34            17             7     24             117
#> 41           34    79            66            36    102             248
#> 42           26    97            69            41    110             276
#> 43           NA    NA            NA            NA     NA               6
#> 44           NA    NA            NA            NA     NA              39
#> 45           78   230           161            91    252             686
#>    soshjelp_M06M12 soshjelp trygd_M01M05 trygd_M06M12 trygd m01m05 m06m09
#> 1                5       37           NA           NA    28     73     NA
#> 2               12       32           15            6    21     43     11
#> 3               NA       14           NA           NA     8     24      8
#> 4                6       22           16            6    22     45      8
#> 5               19       52           42            9    51     96     29
#> 6               NA       11           19            5    24     34      7
#> 7               16       55           27           13    40     79     20
#> 8               NA       NA           NA           NA    NA     NA     NA
#> 9               NA       NA           NA           NA    NA     10     NA
#> 10              NA       NA           NA           NA    NA     NA     NA
#> 11              10       46           33            8    41     77     14
#> 12               8       33           22            5    27     58     14
#> 13              NA       21           15            7    22     37     10
#> 14               7       29           11            5    16     44     11
#> 15               5       22           NA           NA    NA     27     NA
#> 16               6       21            9            4    13     NA      8
#> 17              NA       24           NA           NA    14     39     NA
#> 18              NA       16           NA           NA    15     37     NA
#> 19              12       46           28            8    36     77     20
#> 20              21      100           48           20    68    164     31
#> 21               6       33           19           12    31     56     11
#> 22              NA        5           NA           NA    NA     NA     NA
#> 23               4        9           NA           NA     6     12     NA
#> 24              NA       NA           NA           NA    NA     NA     NA
#> 25              NA        8           NA           NA    NA     NA     NA
#> 26              21       61           42            5    47    101     16
#> 27               4       24           12            4    16     37      8
#> 28               4       10           NA           NA     8     20     NA
#> 29              12       30           13            5    18     40     12
#> 30               9       46           23            9    32     69     13
#> 31              NA       20           NA           NA    16     41     NA
#> 32               6       17           NA           NA     8     NA     NA
#> 33              NA       27           17            7    24     49     10
#> 34               9       31           NA           NA    15     50     11
#> 35               4       16           NA           NA    12     30     NA
#> 36              NA       NA           NA           NA    NA     NA     NA
#> 37              67      232          156           46   202    407     99
#> 38              96      396          221           76   297    649    134
#> 39              85      306          168           39   207    488     91
#> 40              28      145           92           21   113    249     41
#> 41              93      341          187           69   256    546    124
#> 42             102      378          238           58   296    654    133
#> 43               8       14           NA           NA     6     14      5
#> 44              17       56           NA           NA    35     81     21
#> 45             248      934          545          161   706   1544    324
#>    m10m12 M01M05 M06M12 Total
#> 1      NA     73     13    86
#> 2      16     43     27    70
#> 3       4     24     12    36
#> 4       7     45     15    60
#> 5      12     96     41   137
#> 6       5     34     12    46
#> 7      18     79     38   117
#> 8      NA     NA     NA    NA
#> 9      NA     10      8    18
#> 10     NA     NA     NA    NA
#> 11      8     77     22    99
#> 12      6     58     20    78
#> 13      9     37     19    56
#> 14      8     44     19    63
#> 15     NA     27     14    41
#> 16     NA     NA     NA    37
#> 17     NA     39      7    46
#> 18     NA     37     11    48
#> 19     10     77     30   107
#> 20     25    164     56   220
#> 21     11     56     22    78
#> 22     NA     NA     NA    10
#> 23     NA     12      6    18
#> 24     NA     NA     NA     7
#> 25     NA     NA     NA    15
#> 26     18    101     34   135
#> 27      5     37     13    50
#> 28     NA     20      8    28
#> 29      8     40     20    60
#> 30     12     69     25    94
#> 31     NA     41      6    47
#> 32      5     NA     NA    31
#> 33     12     49     22    71
#> 34      5     50     16    66
#> 35     NA     30      7    37
#> 36     NA     NA     NA     4
#> 37     70    407    169   576
#> 38    100    649    234   883
#> 39     84    488    175   663
#> 40     26    249     67   316
#> 41    108    546    232   778
#> 42     94    654    227   881
#> 43      6     14     11    25
#> 44     20     81     41   122
#> 45    254   1544    578  2122

 #  ==== Example 4 , 437 regions ====
 z4w <- KostraData("z4w")
 ProtectKostra(z4w,idVar="region",strataVar="fylke",freqVar=4:15)
#> .....
#> GaussSuppression_anySum: .........................
#>     region annet_m01m05 arbeid_m01m05 soshjelp_m01m05 trygd_m01m05 annet_m10m12
#> 1   010100          103            88             364          237           24
#> 2   010400           98            63             317          229           23
#> 3   010500          132           106             428          300           45
#> 4   010600          174           135             578          444           40
#> 5   011100            8             7              28           26           NA
#> 6   011800           NA            NA              NA           NA           NA
#> 7   011900           NA            NA              26           20           NA
#> 8   012100           NA            NA              NA           NA           NA
#> 9   012200           NA            NA              34           27           NA
#> 10  012300           17            11              48           39           NA
#> 11  012400           35            49             163          125            7
#> 12  012500           30            24             135           98            9
#> 13  012700            5             6              38           24           NA
#> 14  012800           11             5              72           38           NA
#> 15  013500           19            11              60           28            5
#> 16  013600           45            29             134          107           11
#> 17  013700            7             4              23           26           NA
#> 18  013800           NA            NA              41           31            4
#> 19  021100           28            24             108           76            6
#> 20  021300           52            39             210          138           16
#> 21  021400           20            16             114           81            6
#> 22  021500           17            15              66           67           NA
#> 23  021600           28            34             121           82            6
#> 24  021700           18            28             109           75            6
#> 25  021900          171           133             543          427           54
#> 26  022000           74            70             285          178           15
#> 27  022100           33            31             143          117            8
#> 28  022600           21            18              66           76            5
#> 29  022700           15             9              43           39           NA
#> 30  022800           33            27             111           85            9
#> 31  022900           30            13              69           57           NA
#> 32  023000           64            29             214          141           17
#> 33  023100           87            68             325          258           26
#> 34  023300           40            32             112           89            9
#> 35  023400           NA            NA              26           20           NA
#> 36  023500           73            52             205          158           13
#> 37  023600           33            29             112           84            6
#> 38  023700           52            29             173          123           10
#> 39  023800           17            16              70           42           NA
#> 40  023900           NA            NA              18           13           NA
#> 41  030101          245           183             853          645           64
#> 42  030102          233           151             707          520           38
#> 43  030103          168           129             505          381           47
#> 44  030104          132            78             362          223           26
#> 45  030105          161            94             457          297           34
#> 46  030106           39            21             104           87           10
#> 47  030107           41            33             167          111           15
#> 48  030108           89            61             271          183           14
#> 49  030109           78            66             357          241           23
#> 50  030110          116            76             355          298           29
#> 51  030111           96            87             362          276           33
#> 52  030112           96            84             374          258           26
#> 53  030113          107            53             313          219           23
#> 54  030114           59            54             246          169           22
#> 55  030115          150            93             474          354           44
#> 56  040200           63            31             215          118           15
#> 57  040300           81            65             287          203           28
#> 58  041200           67            68             295          236           23
#> 59  041500           16            17              51           51           NA
#> 60  041700           50            51             164          113           16
#> 61  041800           17             9              48           44           NA
#> 62  041900           20            10              67           55            4
#> 63  042000           23            21              83           61            4
#> 64  042300           15            12              61           42           NA
#> 65  042500           22            17             104           80           NA
#> 66  042600           16            12              37           30            8
#> 67  042700           57            49             177          148           11
#> 68  042800           19            17              56           51           NA
#> 69  042900           13            12              39           35           NA
#> 70  043000           11             8              30           26           NA
#> 71  043200           NA            NA              15           14           NA
#> 72  043400           NA            NA               9            9           NA
#> 73  043600           NA            NA              28           11            4
#> 74  043700           12            13              33           31           NA
#> 75  043800           NA            NA              21           12            4
#> 76  043900           NA            NA               9            9           NA
#> 77  044100           NA            NA              23           11           NA
#> 78  050100           64            45             227          136           17
#> 79  050200           88            80             286          204           21
#> 80  051100            4             5              14           17           NA
#> 81  051200           NA            NA               9           10           NA
#> 82  051300           NA            NA              23           16           NA
#> 83  051400           NA            NA              11           10           NA
#> 84  051500           NA            NA              16           14           NA
#> 85  051600           13            14              54           35           NA
#> 86  051700           24            13              76           59            6
#> 87  051900           NA            NA              28           19           NA
#> 88  052000           NA            NA              29           23           NA
#> 89  052100           13            11              48           28           NA
#> 90  052200           NA            NA              15           20           NA
#> 91  052800           25            21              97           73           12
#> 92  052900           31            10              98           63            4
#> 93  053200           11            10              44           28           NA
#> 94  053300           24            10              53           49           NA
#> 95  053400           28            29             105           91           13
#> 96  053600           NA            NA              27           17           NA
#> 97  053800           18            12              43           35           NA
#> 98  054000            5            17              36           32           NA
#> 99  054100           NA            NA              11            8           NA
#> 100 054200           31            20              81           84           NA
#> 101 054300            6             7              29           13           NA
#> 102 054400            6             7              17           15           NA
#> 103 054500           NA            NA              11            8           NA
#> 104 060200          171           143             572          424           52
#> 105 060400           56            48             206          124           18
#> 106 060500           80            73             299          213           27
#> 107 061200           12            10              46           28           NA
#> 108 061500           NA            NA              10           13           NA
#> 109 061600           NA            NA              27           17           NA
#> 110 061700           14            15              52           36           NA
#> 111 061800           NA            NA              15           16           NA
#> 112 061900            6             9              33           40           NA
#> 113 062000            7             5              14           16           NA
#> 114 062100            6             6              30           21           NA
#> 115 062200            6             4              21           16           NA
#> 116 062300           35            20             141           86            4
#> 117 062400           51            44             152          126           12
#> 118 062500           59            44             258          149           15
#> 119 062600           40            37             198          108           20
#> 120 062700           32            23             101           74            4
#> 121 062800           26            15              80           62            9
#> 122 063100           NA            NA              15           10           NA
#> 123 063200           NA            NA              14            7           NA
#> 124 063300           NA            NA              10            7           NA
#> 125 070100           57            56             241          157           25
#> 126 070200           29            30              97           74            9
#> 127 070400          107            74             378          285           32
#> 128 070600          127           102             407          300           32
#> 129 070900           99            65             333          246           35
#> 130 071100           11            11              54           34            6
#> 131 071300           20            14              59           44           NA
#> 132 071400           NA            NA              23           11            4
#> 133 071600           21            15              84           46           NA
#> 134 071900           12             5              52           25           NA
#> 135 072000           28            18              93           71           10
#> 136 072200           68            51             200          141           17
#> 137 072300           12             8              46           35           NA
#> 138 072800           NA            NA              23            9           NA
#> 139 080500           91            86             343          233           20
#> 140 080600          176           128             521          358           29
#> 141 080700           37            29             122          101            8
#> 142 081100           NA            NA               9           10           NA
#> 143 081400           36            18             135           93           13
#> 144 081500           55            34             132          110           13
#> 145 081700            9             8              36           44           NA
#> 146 081900           20            18              99           62           NA
#> 147 082100           15            11              46           52           NA
#> 148 082200           10            11              36           26            4
#> 149 082600           14            18              39           39           NA
#> 150 082700           12            NA              NA           14           NA
#> 151 082800           10             5              27           21           NA
#> 152 082900            5             4              23           13           NA
#> 153 083000           NA            NA              15           10           NA
#> 154 083100           NA             4              NA           NA           NA
#> 155 083300            5            NA              14           NA           NA
#> 156 083400           NA            NA              26           13           NA
#> 157 090100           24            13              68           46            4
#> 158 090400           49            33             153           88            9
#> 159 090600          137            89             477          315           29
#> 160 091100            7             9              28           18           NA
#> 161 091200           NA            NA              17            8           NA
#> 162 091400           23            14              83           64           NA
#> 163 091900           22             8              54           34           NA
#> 164 092600           15            21              86           61           NA
#> 165 092800           NA            NA              14           13           NA
#> 166 092900            7             5              11            9           NA
#> 167 093500           NA            NA              NA            7           NA
#> 168 093700           10             8              42           46           NA
#> 169 093800           NA            NA              13           NA           NA
#> 170 094000           NA            NA              10           14           NA
#> 171 094100           NA            NA              NA           NA           NA
#> 172 100100          198           159             686          481           41
#> 173 100200           31            21              93           66           NA
#> 174 100300           20             9              58           40           NA
#> 175 100400           15             7              48           23           NA
#> 176 101400           46            29             114           92           15
#> 177 101700           18             8              47           25           NA
#> 178 101800           26            28              89           65           NA
#> 179 102100           NA            NA              15           11           NA
#> 180 102600           NA            NA              NA           NA           NA
#> 181 102700           NA            NA              14            9           NA
#> 182 102900           15             9              41           19           NA
#> 183 103200           24            10              84           46           NA
#> 184 103400           NA            NA              13            6           NA
#> 185 103700            8             8              40           30           NA
#> 186 104600           NA            NA              NA           NA           NA
#> 187 110100           27            18              98           71            8
#> 188 110200          146           110             520          414           38
#> 189 110300          263           205             992          700           67
#> 190 110600           96            63             374          225           13
#> 191 111100           12             4              32           25           NA
#> 192 111200           NA            NA              20           15           NA
#> 193 111400           NA             4              11           NA           NA
#> 194 111900           31            24             102           84            9
#> 195 112000           27            20             102           59            7
#> 196 112100           45            29             135          105           11
#> 197 112200           26            30              88           64            7
#> 198 112400           45            37             152          104            8
#> 199 112700           23            22              90           72           NA
#> 200 112900           NA            NA              13            9           NA
#> 201 113000           30            14              68           68            6
#> 202 113300           NA            NA              16           16           NA
#> 203 113400            5             7              42           23           NA
#> 204 113500            8            13              33           42           NA
#> 205 114100           NA            NA               8           19           NA
#> 206 114200            5             8              39           27           NA
#> 207 114400           NA            NA              NA           NA           NA
#> 208 114500           NA            NA              NA           NA           NA
#> 209 114600           39            30             137          111            5
#> 210 114900           70            38             237          194           16
#> 211 115100           NA            NA              NA           NA           NA
#> 212 116000           11             4              44           31            6
#> 213 120100          613           436            2017         1399          164
#> 214 121100           NA            NA              36           33           NA
#> 215 121600            8             8              23           19           NA
#> 216 121900           22            11              54           40           NA
#> 217 122100           36            28             119           78            5
#> 218 122200            6             5              25           22           NA
#> 219 122300           NA            NA              12           15           NA
#> 220 122400           30            12              91           62            6
#> 221 122700           NA            NA              NA           NA           NA
#> 222 122800           23            10              68           45            5
#> 223 123100            6             5              22           11           NA
#> 224 123200           NA            NA              NA           NA           NA
#> 225 123300           NA            NA              12           NA           NA
#> 226 123400           NA            NA              17            8           NA
#> 227 123500           22            17              84           79            6
#> 228 123800           15            13              70           51            5
#> 229 124100           NA            NA              15            9           NA
#> 230 124200           NA            NA              22           13           NA
#> 231 124300           35            19             123           83           10
#> 232 124400           NA            NA               7           NA           NA
#> 233 124500           14            13              40           34           NA
#> 234 124600           44            43             194          117           13
#> 235 124700           29            20             130           76            9
#> 236 125100            9             6              34           28           NA
#> 237 125200           NA            NA              NA            4           NA
#> 238 125300           11            12              46           32           NA
#> 239 125600           19            18              59           35           NA
#> 240 125900           23            14              79           48           NA
#> 241 126000           15            11              58           34           NA
#> 242 126300           23            34             115           85           NA
#> 243 126400           NA            NA              27           19           NA
#> 244 126500           NA            NA               4           NA           NA
#> 245 126600           NA            NA               6            4           NA
#> 246 140100           22            16              93           63           NA
#> 247 141100           NA            NA               5            5           NA
#> 248 141200           NA            NA              NA           NA           NA
#> 249 141300           NA            NA              NA           NA           NA
#> 250 141600           10             9              40           42           NA
#> 251 141700           NA            NA              20           12           NA
#> 252 141800           NA            NA               6            7           NA
#> 253 141900           NA            NA              18           13           NA
#> 254 142000           18             5              62           27           NA
#> 255 142100           NA            NA               8            7           NA
#> 256 142200           11             8              23           24           NA
#> 257 142400           NA            NA              20           19           NA
#> 258 142600           NA            NA              23           12           NA
#> 259 142800            5             4              37           23           NA
#> 260 142900           NA            NA              18           14           NA
#> 261 143000           NA            NA              11           NA           NA
#> 262 143100           NA            NA              25           17           NA
#> 263 143200           38            17              99           93            6
#> 264 143300            6            10              22           12           NA
#> 265 143800            6             6              31           19           NA
#> 266 143900           16             4              23           19           NA
#> 267 144100           NA            NA              12           11           NA
#> 268 144300           13             6              56           25            4
#> 269 144400           NA            NA              NA           NA           NA
#> 270 144500           NA            NA              32           17           NA
#> 271 144900           NA            NA              31           14           NA
#> 272 150200           43            41             187          112           16
#> 273 150400           88            60             297          216           25
#> 274 150500           48            38             191          119           19
#> 275 151100           NA            NA              10           13           NA
#> 276 151400           NA            NA              NA            8           NA
#> 277 151500           10             9              33           31           NA
#> 278 151600           20            22              66           52           NA
#> 279 151700           NA            NA              22           19           NA
#> 280 151900           11            11              54           40           NA
#> 281 152000           18            11              36           40           NA
#> 282 152300            4             6              16            8           NA
#> 283 152400           NA            NA               8            9           NA
#> 284 152500           NA            NA               9            9           NA
#> 285 152600           NA            NA               6           NA           NA
#> 286 152800            9             6              34           21           NA
#> 287 152900            7             5              24           24           NA
#> 288 153100            8            NA              12           NA           NA
#> 289 153200            8             5              28           23           NA
#> 290 153500            6             9              38           28           NA
#> 291 153900           13             7              51           27           NA
#> 292 154300           NA            NA              16           11           NA
#> 293 154500           NA            NA              12            8           NA
#> 294 154600           NA            NA              NA           NA           NA
#> 295 154700           NA            NA              25           15           NA
#> 296 154800           12            12              51           49           NA
#> 297 155100           NA            NA              20           15           NA
#> 298 155400            7             6              45           24           NA
#> 299 155700           NA            NA               8            5           NA
#> 300 156000           NA            NA              11           13           NA
#> 301 156300           22             8              37           28           NA
#> 302 156600           NA            NA              33           12           NA
#> 303 156700           NA            NA               5           NA           NA
#> 304 157100           NA            NA              13            9           NA
#> 305 157300           NA            NA              18           15           NA
#> 306 157600           NA            NA              17           13           NA
#> 307 160100          395           337            1438          988          106
#> 308 161200           10             5              27           19           NA
#> 309 161300           NA            NA              NA           NA           NA
#> 310 162000            5            11              18           19           NA
#> 311 162100           18             7              42           25           NA
#> 312 162200           NA            NA               9            8           NA
#> 313 162400           15            11              45           35           NA
#> 314 162700           NA            NA              26           29           NA
#> 315 163000            6            11              14           15           NA
#> 316 163200           NA            NA              NA           NA           NA
#> 317 163300           NA            NA              NA           NA           NA
#> 318 163400           14             7              27           39           NA
#> 319 163500           NA            NA              10            6           NA
#> 320 163600           11             5              17           13           NA
#> 321 163800           25            16              90           72           17
#> 322 164000           10             8              54           26           NA
#> 323 164400            4            NA              18           NA           NA
#> 324 164800           13            10              40           33           NA
#> 325 165300           41            23              91           78            8
#> 326 165700           10             4              37           29           NA
#> 327 166200           12             5              36           37           NA
#> 328 166300           32            23              94           59           11
#> 329 166400            5             5              22           15           NA
#> 330 166500           NA            NA              16            9           NA
#> 331 170200           40            26             141          119           11
#> 332 170300           29            12              89           67            5
#> 333 171100            4             5              20           13           NA
#> 334 171400           38            31             129          111           11
#> 335 171700            4             7              21           25           NA
#> 336 171800            6             7              34           22           NA
#> 337 171900           38            27             139           96           11
#> 338 172100           44            17             117           90            6
#> 339 172400            9             5              31           25           NA
#> 340 172500           NA            NA              NA           NA           NA
#> 341 173600            5             5              12           14           NA
#> 342 173800           NA            NA              NA            5           NA
#> 343 173900           NA            NA              NA           NA           NA
#> 344 174000            5            NA              NA            7           NA
#> 345 174200            8             4              36           13           NA
#> 346 174300           NA            NA              NA           NA           NA
#> 347 174400           NA            NA              27           22           NA
#> 348 174800           NA            NA              NA           NA           NA
#> 349 174900           NA            NA              11           NA           NA
#> 350 175000           11             8              31           22           NA
#> 351 175100           14             9              57           34           NA
#> 352 175500           NA            NA              NA           NA           NA
#> 353 175600           15            11              50           39           NA
#> 354 180400           90            85             370          320           26
#> 355 180500           53            41             167          118           12
#> 356 181100           NA            NA               5           NA           NA
#> 357 181200            6             6              29           18           NA
#> 358 181300           30             9              62           55            6
#> 359 181500           NA            NA              16           13           NA
#> 360 181600           NA            NA               4           NA           NA
#> 361 181800           NA            NA              10           14           NA
#> 362 182000           20            12              52           30           NA
#> 363 182200            7             7              39           21           NA
#> 364 182400           29            23             120           68           NA
#> 365 182500           NA            NA              10           NA           NA
#> 366 182600           NA             4              NA           12           NA
#> 367 182700           NA            NA              16           12           NA
#> 368 182800            6             4              23           16           NA
#> 369 183200           10             7              30           39            6
#> 370 183300           57            39             210          149           13
#> 371 183400           NA            NA              13            5           NA
#> 372 183600           NA            NA               7           NA           NA
#> 373 183700           18            13              56           52           NA
#> 374 183800           NA            NA              18           17            5
#> 375 183900           NA            NA              10            7           NA
#> 376 184000           15            13              68           42            5
#> 377 184100           20            14              86           63           NA
#> 378 184500           NA            NA              10            9           NA
#> 379 184800           11             6              38           29           NA
#> 380 184900           13             5              18           10           NA
#> 381 185000           NA            NA              19           11           NA
#> 382 185100            9             4              20           12           NA
#> 383 185200           NA            NA              10            6           NA
#> 384 185300            6            NA              NA            4           NA
#> 385 185400            8             8              31           18           NA
#> 386 185600           NA            NA              NA           NA           NA
#> 387 185700           NA            NA              12            7           NA
#> 388 186000           11             8              80           55            7
#> 389 186500           18            18              94           54            7
#> 390 186600           12            10              76           54            6
#> 391 186700            6             6              30           17           NA
#> 392 186800            7             6              28           15           NA
#> 393 187000           20            23              89           54            7
#> 394 187100            6            17              49           30           NA
#> 395 187400           NA            NA               5           NA           NA
#> 396 190200          172           105             546          395           39
#> 397 190300           42            36             210          150           11
#> 398 191100           13             5              43           25           NA
#> 399 191300            6             5              20           21           NA
#> 400 191900            9             5              13           10           NA
#> 401 192000           NA            NA              16            8           NA
#> 402 192200            5             7              13           16           NA
#> 403 192300           NA            NA              27           11           NA
#> 404 192400            6             5              24           30            4
#> 405 192500           NA            NA              19           10           NA
#> 406 192600            4            NA               6           NA           NA
#> 407 192700            4             7              17           13           NA
#> 408 192800           NA            NA               8           NA           NA
#> 409 192900            4            NA               6           NA           NA
#> 410 193100           21            18              73           43           NA
#> 411 193300            9             6              26           26           NA
#> 412 193600            6             4              12           12           NA
#> 413 193800            5             9              15           19           NA
#> 414 193900           NA            NA              15           10           NA
#> 415 194000            7            NA              20           NA           NA
#> 416 194100           10             5              34           36           NA
#> 417 194200           15             9              43           32           NA
#> 418 194300           NA            NA              11            6           NA
#> 419 200200           NA            NA              37           17           NA
#> 420 200300           25            12              86           76            8
#> 421 200400           37            15             106           70            9
#> 422 201100            9             4              30           25           NA
#> 423 201200           55            48             202          164           19
#> 424 201400           NA            NA               5           NA           NA
#> 425 201500            4             6              21           13           NA
#> 426 201700           NA            NA              13            9           NA
#> 427 201800           NA            NA               7            8           NA
#> 428 201900            4             5              24           22           NA
#> 429 202000           24             5              43           47           NA
#> 430 202100            8             7              41           24           NA
#> 431 202200            5             5              26           15           NA
#> 432 202300            5             6              32           25           NA
#> 433 202400           NA            NA              21           10           NA
#> 434 202500            7             6              39           28           NA
#> 435 202700            4            NA              11           NA           NA
#> 436 202800            8             4              38           22           NA
#> 437 203000           15            11              71           48           NA
#> 438   0100          704           554            2513         1811          186
#> 439   0200          918           719            3243         2426          230
#> 440   0300         1810          1263            5907         4262          448
#> 441   0400          533           424            1852         1390          136
#> 442   0500          440           341            1488         1107          116
#> 443   0600          617           516            2294         1593          183
#> 444   0700          601           456            2090         1478          184
#> 445   0800          511           392            1638         1208          116
#> 446   0900          307           213            1070          730           80
#> 447   1000          419           293            1349          919           84
#> 448   1100          922           705            3359         2490          216
#> 449   1200         1038           767            3615         2506          260
#> 450   1400          201           125             722          507           37
#> 451   1500          392           331            1437         1027          119
#> 452   1600          640           505            2178         1564          183
#> 453   1700          289           183             967          743           63
#> 454   1800          534           425            2044         1472          144
#> 455   1900          364           244            1217          891           90
#> 456   2000          231           148             853          636           71
#> 457  Total        11471          8604           39836        28760         2946
#>     arbeid_m10m12 soshjelp_m10m12 trygd_m10m12 annet_m06m09 arbeid_m06m09
#> 1              20             131           34           28            18
#> 2              18             123           36           25            19
#> 3              21             168           54           36            32
#> 4              36             240           59           58            50
#> 5              NA              16            4           NA            NA
#> 6              NA              NA           NA           NA            NA
#> 7              NA              11            5           NA            NA
#> 8              NA              NA           NA           NA            NA
#> 9              NA              10           NA            4            NA
#> 10             NA              25            6           NA            NA
#> 11             14              72           29           10            11
#> 12              4              53           13            8             9
#> 13             NA              10           NA           NA            NA
#> 14             NA              24            9           NA            NA
#> 15             NA              23           NA            4            NA
#> 16              8              56           15           13            10
#> 17             NA              13           NA           NA            NA
#> 18             NA              19           NA           NA             7
#> 19              4              49           13            8             8
#> 20             12              87           30           20            17
#> 21              4              33           12            8             5
#> 22             NA              22            5           NA            NA
#> 23             10              50           24           10            11
#> 24              7              44            9           14             6
#> 25             30             227           82           70            43
#> 26             11             103           29           31            18
#> 27              9              45           16           17            12
#> 28              5              37           10           12             7
#> 29             NA              23            5           NA            NA
#> 30              8              53           17           10             7
#> 31             NA              35           13           NA            NA
#> 32             15              76           30           22            18
#> 33             19             132           46           28            24
#> 34              8              56           15           11             4
#> 35             NA              NA           NA           NA            NA
#> 36             13              79           29           12            16
#> 37              5              41           19           10             8
#> 38              9              64           18            9            15
#> 39             NA              28            5           NA            NA
#> 40             NA              NA           NA           NA            NA
#> 41             53             300           86          105            60
#> 42             39             250           70           74            56
#> 43             30             224           72           57            44
#> 44             16             160           44           31            29
#> 45             23             171           52           54            32
#> 46              5              42           13            9            12
#> 47              7              69           20           19            13
#> 48             14              99           22           32            11
#> 49             16             126           53           41            20
#> 50             18             151           50           30            23
#> 51             13             143           52           44            29
#> 52             24             130           43           41            23
#> 53             21             104           20           30            22
#> 54             17             103           27           25            23
#> 55             22             204           56           53            34
#> 56             13              75           18           16            19
#> 57             21             105           34           24            24
#> 58             20             107           38           38            16
#> 59             NA              22            7           NA            NA
#> 60              6              72           28           20            13
#> 61             NA              25           10           NA            NA
#> 62              6              27            8            6             5
#> 63              4              33            9           10             6
#> 64             NA              26           NA           NA            NA
#> 65             NA              42            6           NA            NA
#> 66             NA              15           NA            5            NA
#> 67             10              87           13           15             9
#> 68             NA              25            9           NA            NA
#> 69             NA              26            7           NA            NA
#> 70             NA               9            4           NA            NA
#> 71             NA              NA           NA           NA            NA
#> 72             NA              NA           NA           NA            NA
#> 73             NA               7           NA           NA            NA
#> 74             NA              21            7           NA            NA
#> 75             NA               4           NA           NA             5
#> 76             NA              NA           NA           NA            NA
#> 77             NA              NA           NA           NA            NA
#> 78              7              79           19           19            17
#> 79              9             130           27           34            25
#> 80             NA              10           NA           NA            NA
#> 81             NA              NA           NA           NA            NA
#> 82             NA               7           NA           NA            NA
#> 83             NA              NA           NA           NA            NA
#> 84             NA               7           NA           NA            NA
#> 85              5              21           NA           NA             6
#> 86             NA              28           NA           13            NA
#> 87             NA               8           NA           NA            NA
#> 88             NA               9           NA           NA            NA
#> 89             NA              18            6           NA            NA
#> 90             NA              12           NA           NA            NA
#> 91              5              32            9           13             5
#> 92              7              42           13            5             5
#> 93             NA              21            7           NA            NA
#> 94             NA              21           11           NA            NA
#> 95              4              44           19            8             8
#> 96             NA              11           NA            5            NA
#> 97             NA              31            8           NA            NA
#> 98             NA              18            7           NA            NA
#> 99             NA              NA           NA           NA            NA
#> 100            NA              47            8           NA            NA
#> 101            NA              10           NA           NA            NA
#> 102            NA               9           NA           NA            NA
#> 103            NA               6           NA           NA            NA
#> 104            25             185           61           60            36
#> 105             8              82           17           20             8
#> 106             9             128           35           27            22
#> 107            NA              16            8           NA            NA
#> 108            NA              NA           NA           NA            NA
#> 109            NA              12           NA           NA            NA
#> 110            NA              11            4           NA            NA
#> 111            NA               8           NA           NA            NA
#> 112            NA              12           NA           NA            NA
#> 113            NA               6           NA           NA            NA
#> 114            NA              10           NA           NA            NA
#> 115            NA               8           NA           NA            NA
#> 116             5              44           11            7            11
#> 117             5              55           15           10             9
#> 118             7              94           26           24            14
#> 119             4              70           27           17             9
#> 120             9              40            8           14             6
#> 121             5              30            9            9             6
#> 122            NA               8           NA           NA            NA
#> 123            NA              NA           NA           NA            NA
#> 124            NA               5           NA           NA            NA
#> 125            10             101           37           15            10
#> 126             8              27           14            6             4
#> 127            11             142           36           41            29
#> 128            26             174           57           36            29
#> 129            16             119           29           47            27
#> 130             4              25            7           11             5
#> 131            NA              18            4           NA            NA
#> 132            NA              NA            4           NA            NA
#> 133            NA              31           12           NA            NA
#> 134            NA               9           NA           NA            NA
#> 135             4              37           10           11             7
#> 136             5              79           25           16            11
#> 137            NA              15            6           NA            NA
#> 138            NA              NA           NA           NA            NA
#> 139            18             132           43           32            25
#> 140            34             189           49           52            38
#> 141             9              41           16            9             8
#> 142            NA              NA           NA           NA            NA
#> 143             8              55           14           17            12
#> 144             9              72           21           24            10
#> 145            NA              13            6           NA            NA
#> 146            NA              37            7           NA            NA
#> 147            NA              15            6           NA            NA
#> 148            NA              14           NA            4            NA
#> 149            NA              15            7           NA            NA
#> 150            NA               7           NA           NA            NA
#> 151            NA              12           NA           NA            NA
#> 152            NA              NA           NA           NA            NA
#> 153            NA              NA           NA           NA            NA
#> 154            NA              NA           NA           NA            NA
#> 155            NA               4           NA           NA            NA
#> 156            NA               8           NA           NA            NA
#> 157             9              33            9           13             6
#> 158             6              64           18           12            12
#> 159            25             174           56           57            37
#> 160            NA              17           NA           NA            NA
#> 161            NA              NA           NA           NA            NA
#> 162            NA              31           13           NA            NA
#> 163            NA              18           14           NA            NA
#> 164            NA              29            8           NA            NA
#> 165            NA              NA           NA           NA            NA
#> 166            NA               6           NA           NA            NA
#> 167            NA               5           NA           NA            NA
#> 168            NA              15            6           NA            NA
#> 169            NA              NA           NA           NA            NA
#> 170            NA              NA           NA           NA            NA
#> 171            NA              NA           NA           NA            NA
#> 172            34             256           84           73            59
#> 173            NA              29           13           NA            NA
#> 174            NA              17            6           NA            NA
#> 175            NA              24            4           NA            NA
#> 176             5              52           13            9            10
#> 177            NA              13            4           NA            NA
#> 178            NA              35            7           NA            NA
#> 179            NA               9           NA           NA            NA
#> 180            NA              NA           NA           NA            NA
#> 181            NA              NA           NA           NA            NA
#> 182            NA              10            6           NA            NA
#> 183            NA              31           14           NA            NA
#> 184            NA              NA           NA           NA            NA
#> 185            NA              12           NA           NA            NA
#> 186            NA              NA           NA           NA            NA
#> 187             6              41            9            4             7
#> 188            27             207           58           57            42
#> 189            46             374           87           98            83
#> 190            18             113           36           32            26
#> 191            NA              NA           NA           NA            NA
#> 192            NA               7           NA            4            NA
#> 193            NA              NA           NA           NA            NA
#> 194             4              40           11           10             5
#> 195             4              45           15            6             5
#> 196             7              58           14           22             9
#> 197             4              44            6            9             6
#> 198             7              60           15           11             5
#> 199            NA              36           13           NA            NA
#> 200            NA              NA           NA           NA            NA
#> 201             5              41           11            7             6
#> 202            NA              NA           NA           NA            NA
#> 203            NA              13            5           NA            NA
#> 204            NA               7           NA           NA            NA
#> 205            NA              NA           NA           NA            NA
#> 206            NA               9            6           NA            NA
#> 207            NA              NA           NA           NA            NA
#> 208            NA              NA           NA           NA            NA
#> 209             7              71           15           15             4
#> 210            10              92           21           15            16
#> 211            NA              NA           NA           NA            NA
#> 212            NA              14           NA            4            NA
#> 213            98             804          240          215           138
#> 214            NA              NA            4           NA            NA
#> 215            NA              14           NA           NA            NA
#> 216            NA              21           11           NA            NA
#> 217             5              54           16            6             7
#> 218            NA              NA           NA           NA            NA
#> 219            NA               5           NA           NA            NA
#> 220             4              28           13           13             6
#> 221            NA              NA           NA           NA            NA
#> 222             4              27            4            5             8
#> 223            NA              NA           NA           NA            NA
#> 224            NA              NA           NA           NA            NA
#> 225            NA              NA           NA           NA            NA
#> 226            NA              NA           NA           NA            NA
#> 227             5              29           10           15             5
#> 228             4              18           12            4             4
#> 229            NA              NA           NA           NA            NA
#> 230            NA              NA           NA           NA            NA
#> 231            11              45           11           11             7
#> 232            NA               5           NA           NA            NA
#> 233            NA              15           NA           NA            NA
#> 234            16              63           20           13            12
#> 235             4              50           26           15            13
#> 236            NA               5           NA           NA            NA
#> 237            NA              NA           NA           NA            NA
#> 238            NA              18            5           NA            NA
#> 239            NA              15            7           NA            NA
#> 240            NA              14            7           NA            NA
#> 241            NA              21            6           NA            NA
#> 242            NA              43           14           NA            NA
#> 243            NA              NA            5           NA            NA
#> 244            NA              NA           NA           NA            NA
#> 245            NA              NA           NA           NA            NA
#> 246            NA              37           14           NA            NA
#> 247            NA              NA           NA           NA            NA
#> 248            NA              NA           NA           NA            NA
#> 249            NA              NA           NA           NA            NA
#> 250            NA              11           NA           NA            NA
#> 251            NA              NA           NA           NA            NA
#> 252            NA              NA           NA           NA            NA
#> 253            NA               4           NA           NA            NA
#> 254            NA              19           NA           NA            NA
#> 255            NA              NA           NA           NA            NA
#> 256            NA              NA           NA           NA            NA
#> 257            NA              19           NA           NA             4
#> 258            NA               7           NA           NA            NA
#> 259            NA              NA           NA           NA            NA
#> 260            NA              NA           NA           NA            NA
#> 261            NA              NA           NA           NA            NA
#> 262             6              NA           NA           NA            NA
#> 263             6              44           15           20            10
#> 264            NA               4           NA           NA            NA
#> 265            NA               4            4           NA            NA
#> 266            NA              11           NA           NA            NA
#> 267            NA              NA           NA           NA            NA
#> 268            NA              11           NA            4            NA
#> 269            NA              NA           NA           NA            NA
#> 270            NA              12            8            4             4
#> 271            NA              21           NA            4            NA
#> 272             8              46           22           12            14
#> 273            14             116           32           39            13
#> 274            11              60           30            9            12
#> 275            NA              NA           NA           NA            NA
#> 276            NA              NA           NA           NA            NA
#> 277            NA              18           NA           NA            NA
#> 278            NA              31            9           NA            NA
#> 279            NA              13           NA           NA            NA
#> 280            NA              18            5           NA            NA
#> 281            NA              16            4           NA            NA
#> 282            NA              NA           NA           NA            NA
#> 283            NA              NA           NA           NA            NA
#> 284            NA              NA           NA           NA            NA
#> 285            NA              NA           NA           NA            NA
#> 286            NA              15           NA           NA            NA
#> 287            NA               4           NA           NA            NA
#> 288            NA              NA           NA           NA            NA
#> 289            NA              11           NA           NA            NA
#> 290            NA              11            5           NA            NA
#> 291            NA              20            5           NA            NA
#> 292            NA              NA           NA           NA            NA
#> 293            NA              NA           NA           NA            NA
#> 294            NA              NA           NA           NA            NA
#> 295            NA               6           NA           NA            NA
#> 296            NA              33            6           NA            NA
#> 297            NA              NA           NA           NA            NA
#> 298            NA              16            5           NA            NA
#> 299            NA              NA           NA           NA            NA
#> 300            NA              NA           NA           NA            NA
#> 301            NA              14           10           NA            NA
#> 302            NA               9           NA            5            NA
#> 303            NA              NA           NA           NA            NA
#> 304            NA              NA           NA           NA            NA
#> 305            NA              NA           NA           NA            NA
#> 306            NA              NA           NA           NA            NA
#> 307            88             525          144          157           108
#> 308            NA              10           NA           NA            NA
#> 309            NA              NA           NA           NA            NA
#> 310            NA              12           NA           NA            NA
#> 311            NA              16            6           NA            NA
#> 312            NA              NA           NA           NA            NA
#> 313            NA              23            5           NA            NA
#> 314            NA               8           NA           NA             4
#> 315            NA               7           NA           NA            NA
#> 316            NA              NA           NA           NA            NA
#> 317            NA              NA           NA           NA            NA
#> 318            NA              16            8           NA            NA
#> 319            NA              NA           NA           NA            NA
#> 320            NA               8           NA           NA            NA
#> 321             5              38            5           11            12
#> 322            NA              30           NA           NA            NA
#> 323            NA              NA           NA           NA            NA
#> 324            NA              15           10           NA            NA
#> 325            10              42           11           12            13
#> 326            NA              10           NA           NA            NA
#> 327            NA              20            4           NA            NA
#> 328             7              37           13           14            12
#> 329            NA               7           NA           NA            NA
#> 330            NA              NA           NA           NA            NA
#> 331            11              51           14           18            17
#> 332             7              25            9            6             4
#> 333            NA               8           NA           NA            NA
#> 334             6              38           13           15             9
#> 335            NA               9           NA           NA            NA
#> 336            NA              13            5           NA            NA
#> 337             4              50           20           10             8
#> 338             5              46           22            8            11
#> 339            NA               9            7           NA            NA
#> 340            NA              NA           NA           NA            NA
#> 341            NA               5           NA           NA            NA
#> 342            NA              NA           NA           NA            NA
#> 343            NA              NA           NA           NA            NA
#> 344            NA              NA           NA           NA            NA
#> 345            NA              13            4           NA            NA
#> 346            NA              NA           NA           NA            NA
#> 347            NA              12           NA            5            NA
#> 348            NA              NA           NA           NA            NA
#> 349            NA              NA           NA           NA            NA
#> 350            NA              11            5           NA            NA
#> 351            NA              14            5           NA            NA
#> 352            NA              NA           NA           NA            NA
#> 353            NA              20            6           NA            NA
#> 354            20             144           54           33            22
#> 355             9              68           15           21            15
#> 356            NA              NA           NA           NA            NA
#> 357            NA              10           NA           NA            NA
#> 358            NA              31           NA           18            NA
#> 359            NA              NA           NA           NA            NA
#> 360            NA              NA           NA           NA            NA
#> 361            NA               7           NA            4            NA
#> 362            NA              20           NA           NA            NA
#> 363            NA              14           NA           NA            NA
#> 364            NA              46           15           NA            NA
#> 365            NA              NA           NA           NA            NA
#> 366            NA               4           NA           NA            NA
#> 367            NA               5           NA           NA            NA
#> 368            NA               7           NA           NA            NA
#> 369            NA              22           NA            4            NA
#> 370            10              73           30           19            16
#> 371            NA              NA           NA           NA            NA
#> 372            NA              NA           NA           NA            NA
#> 373            NA              26            5           NA            NA
#> 374            NA               5           NA           NA            NA
#> 375            NA              NA           NA           NA            NA
#> 376            NA              29           NA            9            NA
#> 377            NA              31            5           NA            NA
#> 378            NA               7           NA           NA            NA
#> 379            NA              16            5           NA            NA
#> 380            NA              NA           NA           NA            NA
#> 381            NA              15           NA           10            NA
#> 382            NA               8           NA           NA            NA
#> 383            NA              NA           NA           NA            NA
#> 384            NA               4           NA           NA            NA
#> 385            NA               8           NA           NA            NA
#> 386            NA              NA           NA           NA            NA
#> 387            NA               6           NA           NA            NA
#> 388             4              23            8           10             7
#> 389             8              32           13           10            11
#> 390             4              31            6            5             8
#> 391            NA              16            4           NA            NA
#> 392            NA              10            4           NA            NA
#> 393             4              35           13           14             5
#> 394            NA              31           NA           NA            NA
#> 395            NA              NA           NA           NA            NA
#> 396            21             224           67           50            29
#> 397             7              70           23           19            15
#> 398            NA              11           NA           NA            NA
#> 399            NA              NA           NA           NA            NA
#> 400            NA               8           NA           NA            NA
#> 401            NA              NA           NA           NA            NA
#> 402            NA               9            4           NA            NA
#> 403            NA               5           NA           NA            NA
#> 404            NA              12           NA            4            NA
#> 405            NA              NA           NA           NA            NA
#> 406            NA              NA           NA           NA            NA
#> 407            NA               6           NA           NA            NA
#> 408            NA              NA           NA           NA            NA
#> 409            NA              NA           NA           NA            NA
#> 410            NA              26            6           NA            NA
#> 411            NA              13           NA           NA            NA
#> 412            NA               6           NA           NA            NA
#> 413            NA               7           NA           NA            NA
#> 414            NA               7           NA           NA            NA
#> 415            NA              NA           NA           NA            NA
#> 416            NA              15           NA           NA            NA
#> 417            NA              20            6           NA            NA
#> 418            NA              NA           NA           NA            NA
#> 419            NA               8           NA           NA            NA
#> 420             4              46           10            8            10
#> 421             4              40            8           15             7
#> 422            NA              12           NA           NA            NA
#> 423             8              79           17           20            13
#> 424            NA              NA           NA           NA            NA
#> 425            NA              10           NA           NA            NA
#> 426            NA              NA           NA           NA            NA
#> 427            NA              NA           NA           NA            NA
#> 428            NA               9           NA           NA            NA
#> 429            NA              29            6           NA            NA
#> 430            NA              20           NA           NA            NA
#> 431            NA              13            5           NA            NA
#> 432            NA               8           NA           NA            NA
#> 433            NA              NA           NA           NA            NA
#> 434            NA              28            5           NA            NA
#> 435            NA              NA           NA           NA            NA
#> 436            NA              10            4           NA            NA
#> 437            NA              27           10           NA            NA
#> 438           141            1000          284          213           182
#> 439           181            1294          432          322           232
#> 440           318            2276          680          645           431
#> 441           109             748          216          180           146
#> 442            63             630          170          151           108
#> 443            94             830          250          210           142
#> 444           100             791          248          206           135
#> 445            92             629          181          169           126
#> 446            49             411          135          118            75
#> 447            64             492          164          129           112
#> 448           164            1287          338          322           237
#> 449           188            1325          432          353           250
#> 450            38             236           79           66            54
#> 451            66             492          158          146            89
#> 452           131             847          231          238           185
#> 453            45             332          123           91            79
#> 454           110             816          229          210           141
#> 455            56             487          145          120            76
#> 456            37             367           89           84            56
#> 457          2046           15290         4584         3973          2856
#>     soshjelp_m06m09 trygd_m06m09 annet arbeid soshjelp trygd m01m05 m06m09
#> 1               100           50   155    126      595   321    792    196
#> 2                93           52   146    100      533   317    707    189
#> 3               122           79   213    159      718   433    966    269
#> 4               184           91   272    221     1002   594   1331    383
#> 5                11            6    11     11       55    36     69     20
#> 6                NA           NA    NA     NA       26    NA     32     NA
#> 7                 9            9    10     11       46    34     57     24
#> 8                NA           NA    NA     NA        9    NA     10     NA
#> 9                 7           NA    10      4       51    32     67     16
#> 10               11            8    26     21       84    53    115     31
#> 11               51           26    52     74      286   180    372     98
#> 12               45           10    47     37      233   121    287     72
#> 13                8           NA     9      8       56    31     73     15
#> 14               19           11    16     16      115    58    126     42
#> 15               17           NA    28     17      100    36    118     27
#> 16               34           27    69     47      224   149    315     84
#> 17                5           NA    13      8       41    34     60     14
#> 18               14           NA    19     13       74    39     85     31
#> 19               34           23    42     36      191   112    236     73
#> 20               68           38    88     68      365   206    439    143
#> 21               32           13    34     25      179   106    231     58
#> 22               25           10    23     18      113    82    165     40
#> 23               40           22    44     55      211   128    265     83
#> 24               27           13    38     41      180    97    230     60
#> 25              157           85   295    206      927   594   1274    355
#> 26               73           45   120     99      461   252    607    167
#> 27               36           24    58     52      224   157    324     89
#> 28               27           14    38     30      130   100    181     60
#> 29               20            4    24     12       86    48    106     31
#> 30               20           28    52     42      184   130    256     65
#> 31               23            9    47     21      127    79    169     48
#> 32               70           35   103     62      360   206    448    145
#> 33               98           49   141    111      555   353    738    199
#> 34               43           17    60     44      211   121    273     75
#> 35               NA           NA     9      6       35    31     55     16
#> 36               61           32    98     81      345   219    488    121
#> 37               29           11    49     42      182   114    258     58
#> 38               49           41    71     53      286   182    377    114
#> 39               12            8    26     21      110    55    145     28
#> 40               NA           NA    10      7       28    20     41     13
#> 41              230          113   414    296     1383   844   1926    508
#> 42              206          101   345    246     1163   691   1611    437
#> 43              167           80   272    203      896   533   1183    348
#> 44              125           63   189    123      647   330    795    248
#> 45              131           82   249    149      759   431   1009    299
#> 46               29           18    58     38      175   118    251     68
#> 47               54           34    75     53      290   165    352    120
#> 48               79           49   135     86      449   254    604    171
#> 49              100           44   142    102      583   338    742    205
#> 50              108           54   175    117      614   402    845    215
#> 51              103           50   173    129      608   378    821    226
#> 52               88           53   163    131      592   354    812    205
#> 53               91           50   160     96      508   289    692    193
#> 54               80           44   106     94      429   240    528    172
#> 55              144           79   247    149      822   489   1071    310
#> 56               47           28    94     63      337   164    427    110
#> 57               78           51   133    110      470   288    636    177
#> 58               75           42   128    104      477   316    666    171
#> 59               16           10    23     29       89    68    135     35
#> 60               54           21    86     70      290   162    378    108
#> 61               16            4    24     15       89    58    118     30
#> 62               25            7    30     21      119    70    152     43
#> 63               24           14    37     31      140    84    188     54
#> 64               14           NA    24     22      101    54    130     36
#> 65               21           20    31     29      167   106    223     59
#> 66               14           NA    29     18       66    42     95     29
#> 67               58           27    83     68      322   188    431    109
#> 68               15            6    25     23       96    66    143     28
#> 69               19            5    20     20       84    47     99     33
#> 70               10            5    16     13       49    35     75     21
#> 71               NA           NA    NA     NA       29    18     33      9
#> 72               NA           NA    NA     NA       16    11     24     NA
#> 73               10           NA    13      4       45    15     48     16
#> 74               14            4    16     19       68    42     89     23
#> 75                4           NA    14      7       29    16     42     12
#> 76               NA           NA    NA     NA       14    10     24     NA
#> 77               NA           NA    10      4       31    16     43     11
#> 78               49           20   100     69      355   175    472    105
#> 79               95           53   143    114      511   284    658    207
#> 80               10           NA     8     10       34    21     40     19
#> 81               NA           NA    NA     NA       13    12     21     NA
#> 82                5           NA     5      8       35    25     47     16
#> 83               NA           NA    NA     NA       17    13     33      5
#> 84                6           NA     9      7       29    21     38     12
#> 85               15           NA    20     25       90    43    116     29
#> 86               22           NA    43     20      126    80    172     51
#> 87                6           NA    10      6       42    24     60     12
#> 88                4           NA    NA     NA       42    29     57     13
#> 89               10           11    21     17       76    45    100     29
#> 90                6           NA    NA     NA       33    25     46     11
#> 91               33           21    50     31      162   103    216     72
#> 92               29           19    40     22      169    95    202     58
#> 93               15           11    18     14       80    46     93     34
#> 94               23           11    31     16       97    71    136     40
#> 95               37           20    49     41      186   130    253     73
#> 96                7           NA    13      5       45    20     54     15
#> 97               14           13    28     19       88    56    108     38
#> 98               12            4     8     22       66    43     90     20
#> 99               NA           NA     4      4       16    14     25     NA
#> 100              13           17    43     25      141   109    216     40
#> 101              10           NA    13     13       49    22     55     23
#> 102              10           NA     7     11       36    22     45     18
#> 103               5           NA     6      4       22    11     23     11
#> 104             155           93   283    204      912   578   1310    344
#> 105              61           30    94     64      349   171    434    119
#> 106              82           40   134    104      509   288    665    171
#> 107              13            6    22     18       75    42     96     27
#> 108              NA           NA    NA     NA       17    14     28     NA
#> 109               6           NA     7      8       45    24     53     12
#> 110              13            4    20     21       76    44    117     21
#> 111               6           NA     9      4       29    20     38     13
#> 112              18           NA     9     14       63    52     88     26
#> 113               4           NA    12      9       24    22     42     15
#> 114               4           NA    11      7       44    25     63     10
#> 115               7           NA     8      7       36    19     47      9
#> 116              37           19    46     36      222   116    282     74
#> 117              48           24    73     58      255   165    373     91
#> 118              63           22    98     65      415   197    510    123
#> 119              38           22    77     50      306   157    383     86
#> 120              40           14    50     38      181    96    230     74
#> 121              25           12    44     26      135    83    183     52
#> 122               7           NA    NA     NA       30    18     30     11
#> 123              NA           NA    NA     NA       19     9     24     NA
#> 124               5           NA     5      5       20    12     24     10
#> 125              84           45    97     76      426   239    511    154
#> 126              24           12    44     42      148   100    230     46
#> 127             124           56   180    114      644   377    844    250
#> 128             132           65   195    157      713   422    936    262
#> 129             101           44   181    108      553   319    743    219
#> 130              10           18    28     20       89    59    110     44
#> 131              17            6    28     17       94    54    137     27
#> 132              NA            4    12      9       34    19     45      9
#> 133              14           13    36     24      129    71    166     43
#> 134              12           NA    17     14       73    33     94     21
#> 135              32           13    49     29      162    94    210     63
#> 136              48           36   101     67      327   202    460    111
#> 137              13            9    17     10       74    50    101     27
#> 138              NA           NA     6      4       35    14     38     12
#> 139              86           60   143    129      561   336    753    203
#> 140             159           76   257    200      869   483   1183    325
#> 141              44           23    54     46      207   140    289     84
#> 142              NA           NA    NA     NA       16    15     27      7
#> 143              46           29    66     38      236   136    282    104
#> 144              32           18    92     53      236   149    331     84
#> 145              11           10    14     12       60    60     97     26
#> 146              30           24    32     31      166    93    199     73
#> 147              22            7    28     19       83    65    124     41
#> 148              11           NA    18     17       61    32     83     22
#> 149              18            5    24     20       72    51    110     30
#> 150              NA           NA    17      6       20    17     39     10
#> 151               4           NA    16      9       43    25     63     13
#> 152              NA           NA     6      6       34    20     45     10
#> 153              NA           NA     6      5       25    14     32     13
#> 154              NA           NA     5      6       12     8     16      8
#> 155               4           NA     6      4       22     8     27      8
#> 156               4           NA    NA     NA       38    15     49      6
#> 157              20           11    41     28      121    66    151     50
#> 158              54           28    70     51      271   134    323    106
#> 159             138           69   223    151      789   440   1018    301
#> 160              15           NA    18     10       60    25     62     26
#> 161              NA           NA    NA     NA       26    12     30      8
#> 162              25           10    35     22      139    87    184     47
#> 163              15           10    36     12       87    58    118     36
#> 164              19           11    28     23      134    80    183     35
#> 165              NA           NA    NA     NA       22    18     30     10
#> 166               4           NA     9      6       21    14     32      9
#> 167              NA           NA    NA     NA       17     9     20      6
#> 168              17            7    19     14       74    59    106     34
#> 169              NA           NA    NA     NA       18     6     21     NA
#> 170              NA           NA     8      7       18    19     32      8
#> 171              NA           NA    NA     NA        6     4     10     NA
#> 172             186          105   312    252     1128   670   1524    423
#> 173              32           18    48     30      154    97    211     66
#> 174              10            7    29     14       85    53    127     23
#> 175              10            8    26     14       82    35     93     32
#> 176              27           19    70     44      193   124    281     65
#> 177              11            8    25     13       71    37     98     26
#> 178              28           24    37     39      152    96    208     62
#> 179               8           NA    NA     NA       32    20     34     12
#> 180              NA           NA    NA     NA        4     5     11     NA
#> 181              NA           NA    NA     NA       18    11     28     NA
#> 182               7            4    17     17       58    29     84     17
#> 183              25           18    31     23      140    78    164     57
#> 184              NA           NA    NA     NA       15     8     22     NA
#> 185              11           NA    15     16       63    41     86     31
#> 186              NA           NA    NA     NA        4     4      9     NA
#> 187              39           16    39     31      178    96    214     66
#> 188             135           73   241    179      862   545   1190    307
#> 189             271          147   428    334     1637   934   2160    599
#> 190             130           72   141    107      617   333    758    260
#> 191              NA           NA    16      5       37    28     73     NA
#> 192               5           NA     8      9       32    21     43     11
#> 193              NA           NA     7      7       14     8     24      8
#> 194              38           18    50     33      180   113    241     71
#> 195              31           18    40     29      178    92    208     60
#> 196              40           21    78     45      233   140    314     92
#> 197              24           16    42     40      156    86    208     55
#> 198              41           19    64     49      253   138    338     76
#> 199              26           12    33     26      152    97    207     47
#> 200              NA           NA     5      7       21    13     28      7
#> 201              26           17    43     25      135    96    180     56
#> 202              NA           NA    NA     NA       22    22     45      8
#> 203              12            7     8     12       67    35     77     24
#> 204              12           NA    13     21       52    51     96     29
#> 205              NA           NA     6      5       11    24     34      7
#> 206               7            7    10     12       55    40     79     20
#> 207              NA           NA    NA     NA       NA    NA     NA     NA
#> 208              NA           NA    NA     NA        7     5     10     NA
#> 209              40           25    59     41      248   151    317     84
#> 210              82           39   101     64      411   254    539    152
#> 211              NA           NA    NA     NA       NA    NA     NA     NA
#> 212              11           NA    21      9       69    42     90     26
#> 213             596          325   992    672     3417  1964   4465   1274
#> 214              NA            4     8      4       46    41     77     14
#> 215               7           NA    13     12       44    25     58     14
#> 216              19           15    32     23       94    66    127     44
#> 217              33           21    47     40      206   115    261     67
#> 218              NA           NA     8     10       33    27     58     14
#> 219               4           NA     6      7       21    22     37     10
#> 220              19           16    49     22      138    91    195     54
#> 221              NA           NA    NA     NA        4    NA      5     NA
#> 222              23            5    33     22      118    54    146     41
#> 223              NA           NA    11      7       29    16     44     11
#> 224              NA           NA    NA     NA        5     4      7     NA
#> 225              NA           NA    NA     NA       15     6     19      4
#> 226              NA           NA    NA     NA       22    12     27      7
#> 227              26           14    43     27      139   103    202     60
#> 228              20            4    24     21      108    67    149     32
#> 229              NA           NA    NA     NA       21    13     26     NA
#> 230              NA           NA    NA     NA       24    14     39     NA
#> 231              35           24    56     37      203   118    260     77
#> 232               4           NA     9      8       16    15     37      5
#> 233               5           NA    19     17       60    42    101     15
#> 234              40           34    70     71      297   171    398     99
#> 235              30           21    53     37      210   123    255     79
#> 236               7           NA    12     13       46    36     77     20
#> 237              NA           NA    NA     NA        6     5      9     NA
#> 238              20            6    20     15       84    43    101     35
#> 239              13            7    28     25       87    49    131     30
#> 240               7           13    26     26      100    68    164     31
#> 241              16            6    22     19       95    46    118     31
#> 242              20            9    42     45      178   108    257     47
#> 243              NA            7     4     10       33    31     56     11
#> 244              NA           NA    NA     NA        5    NA      7     NA
#> 245              NA           NA    NA     NA        9     6     13     NA
#> 246              31           20    38     22      161    97    194     65
#> 247              NA           NA    NA     NA        9     6     12     NA
#> 248              NA           NA    NA     NA       NA    NA     NA     NA
#> 249              NA           NA    NA     NA        8    NA      8     NA
#> 250              10           NA    16     11       61    47    101     16
#> 251              NA           NA     4      6       24    16     37      8
#> 252              NA           NA     4      6       10     8     20     NA
#> 253               8           NA     4      8       30    18     40     12
#> 254              16           NA    21     11       97    35    112     25
#> 255              NA           NA    NA     NA       15    10     22      6
#> 256              NA           NA    14     11       31    30     66     12
#> 257               8           NA     7      6       47    24     47     15
#> 258               5           NA    NA     NA       35    18     40     11
#> 259              NA           NA     8      8       46    32     69     13
#> 260              NA           NA    NA     NA       20    16     41     NA
#> 261              NA           NA    NA     NA       17     8     23     NA
#> 262              NA           NA     8     12       27    24     49     10
#> 263              39           18    64     33      182   126    247     87
#> 264               5           NA     8     12       31    15     50     11
#> 265               5            5     8      9       40    28     62     13
#> 266               7           NA    21     10       41    24     62     14
#> 267              NA           NA    NA     NA       16    12     30     NA
#> 268               9           NA    21     10       76    34    100     22
#> 269              NA           NA    NA     NA       NA    NA     NA     NA
#> 270               7            5    17      8       51    30     61     20
#> 271               9           NA    14      8       61    22     54     23
#> 272              30           18    71     63      263   152    383     74
#> 273              89           30   152     87      502   278    661    171
#> 274              46           29    76     61      297   178    396     96
#> 275              NA           NA    NA     NA       16    15     29      9
#> 276              NA           NA     4     NA       NA     9     14     NA
#> 277               8           NA    19     13       59    45     83     25
#> 278              23            8    32     26      120    69    160     39
#> 279              10           NA    13      9       45    29     58     20
#> 280              16            9    21     19       88    54    116     40
#> 281              10            6    26     16       62    50    105     24
#> 282              NA           NA     6     10       20    11     34      6
#> 283              NA           NA    NA     NA       10    10     22     NA
#> 284              NA           NA    NA     NA       16    12     23     NA
#> 285              NA           NA    NA     NA        8    NA     NA     NA
#> 286               6           NA    16     12       55    28     70     17
#> 287               5           NA    10      9       33    31     60     17
#> 288              NA            5    12      9       20    10     29     12
#> 289               5           NA    12      8       44    26     64     12
#> 290              14            4    15     11       63    37     81     23
#> 291              11           11    22     14       82    43     98     30
#> 292              NA           NA     6      8       25    13     38     NA
#> 293              NA           NA     5      6       13     9     26     NA
#> 294              NA           NA    NA     NA       NA    NA     NA     NA
#> 295               5           NA    10      5       36    20     51     11
#> 296              22            7    26     19      106    62    124     39
#> 297              NA           NA     6      8       26    20     47      9
#> 298              10            7    11      9       71    36     82     21
#> 299              NA           NA    NA     NA        9     6     15     NA
#> 300              NA           NA     4      5       20    16     30     11
#> 301              15            6    31     13       66    44     95     31
#> 302               5           NA    NA     NA       47    15     56     11
#> 303              NA           NA    NA     NA        6     6     14     NA
#> 304              NA           NA     4      4       22    13     26     12
#> 305              NA           NA     5     13       23    17     45      8
#> 306              NA           NA     5      7       25    17     41      5
#> 307             405          231   658    533     2368  1363   3158    901
#> 308              12           NA    17     10       49    26     61     24
#> 309              NA           NA    NA     NA       NA    NA      6     NA
#> 310               6           NA     7     13       36    29     53      9
#> 311              11            6    24     11       69    37     92     22
#> 312              NA           NA    NA     NA       15    12     21      6
#> 313              14            9    29     18       82    49    106     34
#> 314               9           NA     5      8       43    35     61     16
#> 315               7           NA     7     13       28    16     46     10
#> 316              NA           NA    NA     NA        6     4      6     NA
#> 317              NA           NA    NA     NA       NA    NA      7     NA
#> 318              14           10    24     10       57    57     87     33
#> 319              NA           NA    NA     NA       18     8     21      5
#> 320               9           NA    15     11       34    17     46     20
#> 321              33           17    53     33      161    94    203     73
#> 322              16           NA    16     10      100    33     98     23
#> 323              NA           NA     7      4       30     5     28      5
#> 324              21            8    20     16       76    51     96     37
#> 325              34           11    61     46      167   100    233     70
#> 326              15           NA    17      9       62    39     80     34
#> 327              11            7    15     11       67    48     90     22
#> 328              23           14    57     42      154    86    208     63
#> 329               7           NA     9      8       36    19     47     14
#> 330              NA           NA    NA     NA       25    15     33     14
#> 331              47           30    69     54      239   163    326    112
#> 332              20           11    40     23      134    87    197     41
#> 333              10           NA     9      9       38    18     42     20
#> 334              40           24    64     46      207   148    309     88
#> 335               9           NA     7      9       39    31     57     15
#> 336              11            5    12      9       58    32     69     22
#> 337              34           23    59     39      223   139    300     75
#> 338              25           21    58     33      188   133    268     65
#> 339               8            5    14      7       48    37     70     18
#> 340              NA           NA    NA     NA        4    NA      5     NA
#> 341               9           NA     9      9       26    23     36     21
#> 342              NA           NA    NA     NA        8     8     11     NA
#> 343               4           NA    NA     NA        8    NA      8     NA
#> 344              NA           NA    NA     NA        9     9     20     NA
#> 345               7            4    10      9       56    21     61     15
#> 346              NA           NA     4     NA        4    NA      8     NA
#> 347               5           NA    19      7       44    25     62     15
#> 348              NA           NA    NA     NA        4     6     11     NA
#> 349              NA           NA    NA     NA       16     5     17     NA
#> 350               9            5    14     13       51    32     72     20
#> 351              12            6    17     16       83    45    114     23
#> 352              NA           NA    NA     NA        5    NA      4     NA
#> 353              11            7    24     17       81    52    115     29
#> 354             118           65   149    127      632   439    865    238
#> 355              41           25    86     65      276   158    379    102
#> 356              NA           NA    NA     NA       10     5     11      5
#> 357               5           NA    11      8       44    20     59     10
#> 358              18           NA    54     16      111    65    156     43
#> 359              NA           NA    NA     NA       23    16     35     10
#> 360              NA           NA    NA     NA        5     5     11     NA
#> 361               7           NA    11      5       24    20     33     15
#> 362              17           NA    27     18       89    37    114     27
#> 363               9           NA    10     12       62    28     74     15
#> 364              26           23    37     35      192   106    240     56
#> 365              NA           NA     4      4       13     4     15      5
#> 366              NA           NA    NA     NA       16    14     26      4
#> 367               7           NA     4      7       28    14     36      8
#> 368               9           NA     9      5       39    18     49     14
#> 369              16           NA    20     13       68    48     86     31
#> 370              60           37    89     65      343   216    455    132
#> 371              NA           NA     5      6       21     7     26      8
#> 372              NA           NA    NA     NA        8     4     12     NA
#> 373              15            8    23     16       97    65    139     27
#> 374              11           NA    11      4       34    24     42     16
#> 375              NA           NA     5      5       17    11     22      7
#> 376              18           NA    29     20      115    56    138     38
#> 377              24           15    28     23      141    83    183     50
#> 378               8           NA    NA     NA       25    15     24     15
#> 379               4            7    17     14       58    41     84     21
#> 380              NA           NA    17      6       29    16     46      9
#> 381               8           NA    14      8       42    15     37     21
#> 382               4           NA    12      6       32    14     45      7
#> 383              NA           NA    NA     NA       15     7     21     NA
#> 384              NA           NA    NA     NA        8     8     16     NA
#> 385               8           NA    13     11       47    23     65     16
#> 386               4           NA    NA     NA        7    NA      6     NA
#> 387               5           NA    NA     NA       23    10     23      9
#> 388              22           19    28     19      125    82    154     58
#> 389              23           15    35     37      149    82    184     59
#> 390              29            7    23     22      136    67    152     49
#> 391               9            5    11     10       55    26     59     17
#> 392              11            5    11      9       49    24     56     19
#> 393              28           11    41     32      152    78    186     58
#> 394              11           NA    14     23       91    36    102     25
#> 395              NA           NA    NA     NA        7    NA      9     NA
#> 396             130           88   261    155      900   550   1218    297
#> 397              40           30    72     58      320   203    438    104
#> 398               7           NA    19      7       61    31     86     11
#> 399              NA           NA    10     10       34    29     52     11
#> 400               5           NA    10     10       26    12     37     11
#> 401              NA           NA    NA     NA       21    12     26     NA
#> 402               6            4     8     10       28    24     41     12
#> 403               7           NA    10      5       39    17     46     16
#> 404               7           NA    14      7       43    36     65     18
#> 405              NA           NA    11      5       31    16     38      9
#> 406              NA           NA     7     NA       13    NA     13      5
#> 407               5           NA    10      8       28    16     41     13
#> 408              NA           NA     5     NA       12    NA     12      5
#> 409              NA           NA     8      5       12     5     15      5
#> 410              12           11    35     26      111    60    155     34
#> 411              10           NA    14     10       49    32     67     16
#> 412               6           NA    10      9       24    16     34     17
#> 413               7           NA     7     10       29    23     48      8
#> 414               6           NA    NA     NA       28    16     33     11
#> 415              NA           NA     8      6       27     9     40     NA
#> 416               7           NA    13      8       56    44     85     16
#> 417               7           11    24     15       70    49     99     24
#> 418              NA           NA     7      6       17    10     27      5
#> 419               8           NA    14      6       53    25     67     13
#> 420              19           18    41     26      151   104    199     55
#> 421              29           16    61     26      175    94    228     67
#> 422              10           NA    15      6       52    33     68     20
#> 423              62           28    94     69      343   209    469    123
#> 424              NA           NA    NA     NA        7     6     16     NA
#> 425               8           NA     7     11       39    18     44     18
#> 426              NA           NA    10      5       29    13     32     17
#> 427              NA           NA    NA     NA       14    10     18     NA
#> 428               8           NA     7      8       41    33     55     13
#> 429              13           10    40      8       85    63    119     31
#> 430              15           NA    14     11       76    40     80     35
#> 431               8            4     7      9       47    24     51     15
#> 432              10           NA     8     10       50    30     68     15
#> 433              NA           NA    NA     NA       32    11     32      7
#> 434              17            5    14      9       84    38     80     29
#> 435              NA           NA     5      4       20    10     25      5
#> 436               7            4    13      7       55    30     72     15
#> 437              28           11    28     18      126    69    145     51
#> 438             735          390  1103    877     4248  2485   5582   1520
#> 439             953          534  1470   1132     5490  3392   7306   2041
#> 440            1735          914  2903   2012     9918  5856  13242   3725
#> 441             528          270   849    679     3128  1876   4199   1124
#> 442             442          262   707    512     2560  1539   3376    963
#> 443             638          309  1010    752     3762  2152   5020   1299
#> 444             620          327   991    691     3501  2053   4625   1288
#> 445             494          278   796    610     2761  1667   3749   1067
#> 446             322          166   505    337     1803  1031   2320    681
#> 447             358          225   632    469     2199  1308   2980    824
#> 448             983          539  1460   1106     5629  3367   7476   2081
#> 449             973          567  1651   1205     5913  3505   7926   2143
#> 450             185           99   304    217     1143   685   1555    404
#> 451             375          198   657    486     2304  1383   3187    808
#> 452             664          354  1061    821     3689  2149   4887   1441
#> 453             274          159   443    307     1573  1025   2182    603
#> 454             598          313   888    676     3458  2014   4475   1262
#> 455             275          184   574    376     1979  1220   2716    655
#> 456             259          135   386    241     1479   860   1868    534
#> 457           11411         6223 18390  13506    66537 39567  88671  24463
#>     m10m12  Total
#> 1      209   1197
#> 2      200   1096
#> 3      288   1523
#> 4      375   2089
#> 5       24    113
#> 6       NA     49
#> 7       20    101
#> 8       NA     14
#> 9       14     97
#> 10      38    184
#> 11     122    592
#> 12      79    438
#> 13      16    104
#> 14      37    205
#> 15      36    181
#> 16      90    489
#> 17      22     96
#> 18      29    145
#> 19      72    381
#> 20     145    727
#> 21      55    344
#> 22      31    236
#> 23      90    438
#> 24      66    356
#> 25     393   2022
#> 26     158    932
#> 27      78    491
#> 28      57    298
#> 29      33    170
#> 30      87    408
#> 31      57    274
#> 32     138    731
#> 33     223   1160
#> 34      88    436
#> 35      10     81
#> 36     134    743
#> 37      71    387
#> 38     101    592
#> 39      39    212
#> 40      11     65
#> 41     503   2937
#> 42     397   2445
#> 43     373   1904
#> 44     246   1289
#> 45     280   1588
#> 46      70    389
#> 47     111    583
#> 48     149    924
#> 49     218   1165
#> 50     248   1308
#> 51     241   1288
#> 52     223   1240
#> 53     168   1053
#> 54     169    869
#> 55     326   1707
#> 56     121    658
#> 57     188   1001
#> 58     188   1025
#> 59      39    209
#> 60     122    608
#> 61      38    186
#> 62      45    240
#> 63      50    292
#> 64      35    201
#> 65      51    333
#> 66      31    155
#> 67     121    661
#> 68      39    210
#> 69      39    171
#> 70      17    113
#> 71      13     55
#> 72      NA     34
#> 73      13     77
#> 74      33    145
#> 75      12     66
#> 76      NA     31
#> 77       7     61
#> 78     122    699
#> 79     187   1052
#> 80      14     73
#> 81      NA     28
#> 82      10     73
#> 83       7     45
#> 84      16     66
#> 85      33    178
#> 86      46    269
#> 87      10     82
#> 88      14     84
#> 89      30    159
#> 90      17     74
#> 91      58    346
#> 92      66    326
#> 93      31    158
#> 94      39    215
#> 95      80    406
#> 96      14     83
#> 97      45    191
#> 98      29    139
#> 99      NA     38
#> 100     62    318
#> 101     19     97
#> 102     13     76
#> 103      9     43
#> 104    323   1977
#> 105    125    678
#> 106    199   1035
#> 107     34    157
#> 108     NA     38
#> 109     19     84
#> 110     23    161
#> 111     11     62
#> 112     24    138
#> 113     10     67
#> 114     14     87
#> 115     14     70
#> 116     64    420
#> 117     87    551
#> 118    142    775
#> 119    121    590
#> 120     61    365
#> 121     53    288
#> 122     17     58
#> 123     NA     33
#> 124      8     42
#> 125    173    838
#> 126     58    334
#> 127    221   1315
#> 128    289   1487
#> 129    199   1161
#> 130     42    196
#> 131     29    193
#> 132     20     74
#> 133     51    260
#> 134     22    137
#> 135     61    334
#> 136    126    697
#> 137     23    151
#> 138      9     59
#> 139    213   1169
#> 140    301   1809
#> 141     74    447
#> 142      6     40
#> 143     90    476
#> 144    115    530
#> 145     23    146
#> 146     50    322
#> 147     30    195
#> 148     23    128
#> 149     27    167
#> 150     11     60
#> 151     17     93
#> 152     11     66
#> 153      5     50
#> 154      7     31
#> 155      5     40
#> 156     10     65
#> 157     55    256
#> 158     97    526
#> 159    284   1603
#> 160     25    113
#> 161      8     46
#> 162     52    283
#> 163     39    193
#> 164     47    265
#> 165      9     49
#> 166      9     50
#> 167      7     33
#> 168     26    166
#> 169     NA     30
#> 170     12     52
#> 171     NA     11
#> 172    415   2362
#> 173     52    329
#> 174     31    181
#> 175     32    157
#> 176     85    431
#> 177     22    146
#> 178     54    324
#> 179     15     61
#> 180     NA     15
#> 181     NA     35
#> 182     20    121
#> 183     51    272
#> 184     NA     27
#> 185     18    135
#> 186     NA     12
#> 187     64    344
#> 188    330   1827
#> 189    574   3333
#> 190    180   1198
#> 191     NA     86
#> 192     16     70
#> 193      4     36
#> 194     64    376
#> 195     71    339
#> 196     90    496
#> 197     61    324
#> 198     90    504
#> 199     54    308
#> 200     11     46
#> 201     63    299
#> 202      7     60
#> 203     21    122
#> 204     12    137
#> 205      5     46
#> 206     18    117
#> 207     NA     NA
#> 208     NA     18
#> 209     98    499
#> 210    139    830
#> 211     NA     NA
#> 212     25    141
#> 213   1306   7045
#> 214      8     99
#> 215     22     94
#> 216     44    215
#> 217     80    408
#> 218      6     78
#> 219      9     56
#> 220     51    300
#> 221     NA      8
#> 222     40    227
#> 223      8     63
#> 224     NA     11
#> 225      4     27
#> 226      7     41
#> 227     50    312
#> 228     39    220
#> 229     NA     37
#> 230     NA     46
#> 231     77    414
#> 232      6     48
#> 233     22    138
#> 234    112    609
#> 235     89    423
#> 236     10    107
#> 237     NA     14
#> 238     26    162
#> 239     28    189
#> 240     25    220
#> 241     33    182
#> 242     69    373
#> 243     11     78
#> 244     NA     10
#> 245     NA     20
#> 246     59    318
#> 247     NA     18
#> 248     NA      7
#> 249     NA     15
#> 250     18    135
#> 251      5     50
#> 252     NA     28
#> 253      8     60
#> 254     27    164
#> 255      7     35
#> 256      8     86
#> 257     22     84
#> 258     10     61
#> 259     12     94
#> 260     NA     47
#> 261     NA     31
#> 262     12     71
#> 263     71    405
#> 264      5     66
#> 265     10     85
#> 266     20     96
#> 267     NA     37
#> 268     19    141
#> 269     NA      4
#> 270     25    106
#> 271     28    105
#> 272     92    549
#> 273    187   1019
#> 274    120    612
#> 275      5     43
#> 276     NA     18
#> 277     28    136
#> 278     48    247
#> 279     18     96
#> 280     26    182
#> 281     25    154
#> 282      7     47
#> 283     NA     26
#> 284     NA     36
#> 285     NA     12
#> 286     24    111
#> 287      6     83
#> 288     10     51
#> 289     14     90
#> 290     22    126
#> 291     33    161
#> 292     NA     52
#> 293     NA     33
#> 294     NA      4
#> 295      9     71
#> 296     50    213
#> 297      4     60
#> 298     24    127
#> 299     NA     20
#> 300      4     45
#> 301     28    154
#> 302     12     79
#> 303     NA     18
#> 304      5     43
#> 305      5     58
#> 306      8     54
#> 307    863   4922
#> 308     17    102
#> 309     NA      7
#> 310     23     85
#> 311     27    141
#> 312      8     35
#> 313     38    178
#> 314     14     91
#> 315      8     64
#> 316     NA     13
#> 317     NA      9
#> 318     28    148
#> 319      7     33
#> 320     11     77
#> 321     65    341
#> 322     38    159
#> 323     13     46
#> 324     30    163
#> 325     71    374
#> 326     13    127
#> 327     29    141
#> 328     68    339
#> 329     11     72
#> 330      6     53
#> 331     87    525
#> 332     46    284
#> 333     12     74
#> 334     68    465
#> 335     14     86
#> 336     20    111
#> 337     85    460
#> 338     79    412
#> 339     18    106
#> 340     NA      7
#> 341     10     67
#> 342     NA     18
#> 343     NA     14
#> 344     NA     25
#> 345     20     96
#> 346     NA     12
#> 347     18     95
#> 348     NA     13
#> 349     NA     24
#> 350     18    110
#> 351     24    161
#> 352     NA      9
#> 353     30    174
#> 354    244   1347
#> 355    104    585
#> 356      5     21
#> 357     14     83
#> 358     47    246
#> 359      6     51
#> 360     NA     15
#> 361     12     60
#> 362     30    171
#> 363     23    112
#> 364     74    370
#> 365      5     25
#> 366      5     35
#> 367      9     53
#> 368      8     71
#> 369     32    149
#> 370    126    713
#> 371      5     39
#> 372     NA     15
#> 373     35    201
#> 374     15     73
#> 375      9     38
#> 376     44    220
#> 377     42    275
#> 378     10     49
#> 379     25    130
#> 380     13     68
#> 381     21     79
#> 382     12     64
#> 383     NA     28
#> 384     NA     25
#> 385     13     94
#> 386     NA     12
#> 387      8     40
#> 388     42    254
#> 389     60    303
#> 390     47    248
#> 391     26    102
#> 392     18     93
#> 393     59    303
#> 394     37    164
#> 395     NA     12
#> 396    351   1866
#> 397    111    653
#> 398     21    118
#> 399     20     83
#> 400     10     58
#> 401     NA     37
#> 402     17     70
#> 403      9     71
#> 404     17    100
#> 405     16     63
#> 406      8     26
#> 407      8     62
#> 408      6     23
#> 409     10     30
#> 410     43    232
#> 411     22    105
#> 412      8     59
#> 413     13     69
#> 414     11     55
#> 415     NA     50
#> 416     20    121
#> 417     35    158
#> 418      8     40
#> 419     18     98
#> 420     68    322
#> 421     61    356
#> 422     18    106
#> 423    123    715
#> 424     NA     21
#> 425     13     75
#> 426      8     57
#> 427     NA     30
#> 428     21     89
#> 429     46    196
#> 430     26    141
#> 431     21     87
#> 432     15     98
#> 433      6     45
#> 434     36    145
#> 435      9     39
#> 436     18    105
#> 437     45    241
#> 438   1611   8713
#> 439   2137  11484
#> 440   3722  20689
#> 441   1209   6532
#> 442    979   5318
#> 443   1357   7676
#> 444   1323   7236
#> 445   1018   5834
#> 446    675   3676
#> 447    804   4608
#> 448   2005  11562
#> 449   2205  12274
#> 450    390   2349
#> 451    835   4830
#> 452   1392   7720
#> 453    563   3348
#> 454   1299   7036
#> 455    778   4149
#> 456    564   2966
#> 457  24866 138000

 # =====================================================================
 #    Examples with extra variables in output and several id variables
 # =====================================================================

 ProtectKostra(z3wb,idVar="region",strataVar=c("fylke","kostragr"),freqVar=4:15,fylke="fylke",kostragr="kostragr")
#> .....
#> GaussSuppression_anySum: .........................
#>    region annet_m01m05_M01M05 arbeid_m01m05_M01M05 soshjelp_m01m05_M01M05
#> 1  111100                  12                   NA                     32
#> 2  111200                  NA                   NA                     20
#> 3  111400                  NA                   NA                     NA
#> 4  113300                  NA                   NA                     16
#> 5  113500                   8                   13                     33
#> 6  114100                  NA                   NA                     NA
#> 7  114200                   5                    8                     39
#> 8  114400                  NA                   NA                     NA
#> 9  114500                  NA                   NA                      6
#> 10 115100                  NA                   NA                     NA
#> 11 121100                  NA                   NA                     36
#> 12 122200                  NA                   NA                     25
#> 13 122300                  NA                   NA                     NA
#> 14 123100                  NA                   NA                     22
#> 15 123400                  NA                   NA                     17
#> 16 124100                  NA                   NA                     15
#> 17 124200                  NA                   NA                     NA
#> 18 124400                  NA                   NA                     NA
#> 19 125100                  NA                   NA                     34
#> 20 125900                  NA                   NA                     79
#> 21 126400                  NA                   NA                     27
#> 22 126500                  NA                   NA                     NA
#> 23 141100                  NA                   NA                      5
#> 24 141200                  NA                   NA                     NA
#> 25 141300                  NA                   NA                     NA
#> 26 141600                  NA                   NA                     40
#> 27 141700                  NA                   NA                     20
#> 28 141800                  NA                   NA                      6
#> 29 141900                  NA                   NA                     18
#> 30 142800                   5                    4                     37
#> 31 142900                  NA                    6                     NA
#> 32 143000                  NA                   NA                     11
#> 33 143100                   4                   NA                     NA
#> 34 143300                  NA                   NA                     22
#> 35 144100                  NA                    6                     12
#> 36 144400                  NA                   NA                     NA
#> 37   1100                  35                   51                    165
#> 38   1200                  72                   56                    300
#> 39   1400                  45                   54                    221
#> 40     01                  23                   17                    117
#> 41     02                  45                   66                    248
#> 42     03                  71                   69                    276
#> 43     04                  NA                   NA                      6
#> 44     05                  NA                   NA                     39
#> 45  Total                 152                  161                    686
#>    trygd_m01m05_M01M05 annet_m10m12_M06M12 arbeid_m10m12_M06M12
#> 1                   NA                  NA                   NA
#> 2                   15                  NA                   NA
#> 3                   NA                  NA                   NA
#> 4                   16                  NA                   NA
#> 5                   42                  NA                   NA
#> 6                   19                  NA                   NA
#> 7                   27                  NA                   NA
#> 8                   NA                  NA                   NA
#> 9                   NA                  NA                   NA
#> 10                  NA                  NA                   NA
#> 11                  33                  NA                   NA
#> 12                  22                  NA                   NA
#> 13                  15                  NA                   NA
#> 14                  11                  NA                   NA
#> 15                  NA                  NA                   NA
#> 16                   9                  NA                   NA
#> 17                  NA                  NA                   NA
#> 18                  NA                  NA                   NA
#> 19                  28                  NA                   NA
#> 20                  48                  NA                   NA
#> 21                  19                  NA                   NA
#> 22                  NA                  NA                   NA
#> 23                  NA                  NA                   NA
#> 24                  NA                  NA                   NA
#> 25                  NA                  NA                   NA
#> 26                  42                  NA                   NA
#> 27                  12                  NA                   NA
#> 28                  NA                  NA                   NA
#> 29                  13                  NA                   NA
#> 30                  23                  NA                   NA
#> 31                  NA                  NA                   NA
#> 32                  NA                  NA                   NA
#> 33                  17                  NA                    6
#> 34                  NA                  NA                   NA
#> 35                  NA                  NA                   NA
#> 36                  NA                  NA                   NA
#> 37                 156                  11                   10
#> 38                 221                   8                   13
#> 39                 168                   7                   18
#> 40                  92                  NA                   NA
#> 41                 187                  13                   17
#> 42                 238                   9                   18
#> 43                  NA                  NA                   NA
#> 44                  NA                  NA                   NA
#> 45                 545                  26                   41
#>    soshjelp_m10m12_M06M12 trygd_m10m12_M06M12 annet_m06m09_M06M12
#> 1                      NA                  NA                  NA
#> 2                       7                  NA                   4
#> 3                      NA                  NA                  NA
#> 4                      NA                  NA                  NA
#> 5                       7                  NA                  NA
#> 6                      NA                  NA                  NA
#> 7                       9                   6                  NA
#> 8                      NA                  NA                  NA
#> 9                      NA                  NA                  NA
#> 10                     NA                  NA                  NA
#> 11                     NA                   4                  NA
#> 12                     NA                  NA                  NA
#> 13                      5                  NA                  NA
#> 14                     NA                  NA                   4
#> 15                     NA                  NA                  NA
#> 16                     NA                  NA                  NA
#> 17                     NA                  NA                  NA
#> 18                      5                  NA                  NA
#> 19                      5                  NA                  NA
#> 20                     14                   7                  NA
#> 21                     NA                   5                  NA
#> 22                     NA                  NA                  NA
#> 23                     NA                  NA                  NA
#> 24                     NA                  NA                  NA
#> 25                     NA                  NA                  NA
#> 26                     11                  NA                  NA
#> 27                     NA                  NA                  NA
#> 28                     NA                  NA                  NA
#> 29                      4                  NA                  NA
#> 30                     NA                  NA                  NA
#> 31                     NA                  NA                  NA
#> 32                     NA                  NA                  NA
#> 33                     NA                  NA                  NA
#> 34                      4                  NA                  NA
#> 35                     NA                  NA                  NA
#> 36                     NA                  NA                  NA
#> 37                     33                  16                  21
#> 38                     49                  30                  18
#> 39                     43                  16                  13
#> 40                     13                   8                  NA
#> 41                     47                  31                  21
#> 42                     51                  16                  17
#> 43                     NA                  NA                  NA
#> 44                     NA                  NA                  NA
#> 45                    125                  62                  52
#>    arbeid_m06m09_M06M12 soshjelp_m06m09_M06M12 trygd_m06m09_M06M12 annet_M01M05
#> 1                    NA                     NA                  NA           12
#> 2                    NA                      5                  NA           NA
#> 3                    NA                     NA                  NA           NA
#> 4                    NA                     NA                  NA           NA
#> 5                    NA                     12                  NA            8
#> 6                    NA                     NA                  NA           NA
#> 7                    NA                      7                   7            5
#> 8                    NA                     NA                  NA           NA
#> 9                    NA                     NA                  NA           NA
#> 10                   NA                     NA                  NA           NA
#> 11                   NA                     NA                   4           NA
#> 12                    4                     NA                  NA           NA
#> 13                   NA                     NA                  NA           NA
#> 14                   NA                     NA                  NA           NA
#> 15                   NA                     NA                  NA           NA
#> 16                   NA                     NA                  NA           NA
#> 17                   NA                     NA                  NA           NA
#> 18                   NA                     NA                  NA           NA
#> 19                    4                      7                  NA           NA
#> 20                   NA                      7                  13           NA
#> 21                   NA                     NA                   7           NA
#> 22                   NA                     NA                  NA           NA
#> 23                   NA                     NA                  NA           NA
#> 24                   NA                     NA                  NA           NA
#> 25                   NA                     NA                  NA           NA
#> 26                   NA                     10                  NA           NA
#> 27                   NA                     NA                  NA           NA
#> 28                   NA                     NA                  NA           NA
#> 29                   NA                      8                  NA           NA
#> 30                   NA                     NA                  NA            5
#> 31                   NA                     NA                  NA           NA
#> 32                   NA                     NA                  NA           NA
#> 33                   NA                     NA                  NA            4
#> 34                   NA                      5                  NA           NA
#> 35                   NA                     NA                  NA           NA
#> 36                   NA                     NA                  NA           NA
#> 37                   14                     34                  30           35
#> 38                   23                     47                  46           72
#> 39                   13                     42                  23           45
#> 40                   NA                     15                  13           23
#> 41                   19                     46                  38           45
#> 42                   23                     51                  42           71
#> 43                   NA                     NA                  NA           NA
#> 44                    5                     NA                  NA           NA
#> 45                   50                    123                  99          152
#>    annet_M06M12 annet arbeid_M01M05 arbeid_M06M12 arbeid soshjelp_M01M05
#> 1             4    16            NA            NA      5              32
#> 2            NA     8            NA            NA      9              20
#> 3            NA     7            NA            NA      7              NA
#> 4            NA    NA            NA            NA     NA              16
#> 5             5    13            13             8     21              33
#> 6            NA     6            NA            NA      5              NA
#> 7             5    10             8             4     12              39
#> 8            NA    NA            NA            NA     NA              NA
#> 9            NA    NA            NA            NA     NA               6
#> 10           NA    NA            NA            NA     NA              NA
#> 11           NA     8            NA            NA      4              36
#> 12           NA     8            NA            NA     10              25
#> 13           NA     6            NA            NA      7              NA
#> 14           NA    11            NA            NA      7              22
#> 15           NA     5            NA            NA     NA              17
#> 16           NA    NA            NA            NA     NA              15
#> 17           NA    NA            NA            NA     NA              NA
#> 18           NA     9            NA            NA      8              NA
#> 19           NA    12            NA            NA     13              34
#> 20           NA    26            NA            NA     26              79
#> 21           NA     4            NA            NA     10              27
#> 22           NA    NA            NA            NA     NA              NA
#> 23           NA    NA            NA            NA     NA               5
#> 24           NA    NA            NA            NA     NA              NA
#> 25           NA    NA            NA            NA     NA              NA
#> 26           NA    16            NA            NA     11              40
#> 27           NA     4            NA            NA      6              20
#> 28           NA     4            NA            NA      6               6
#> 29           NA     4            NA            NA      8              18
#> 30           NA    NA             4            NA     NA              37
#> 31           NA    NA             6            NA     NA              NA
#> 32           NA    NA            NA            NA     NA              11
#> 33            4     8            NA            NA     12              NA
#> 34           NA     8            NA            NA     12              22
#> 35           NA    NA             6            NA     NA              12
#> 36           NA    NA            NA            NA     NA              NA
#> 37           32    67            51            24     75             165
#> 38           26    98            56            36     92             300
#> 39           20    65            54            31     85             221
#> 40           11    34            17             7     24             117
#> 41           34    79            66            36    102             248
#> 42           26    97            69            41    110             276
#> 43           NA    NA            NA            NA     NA               6
#> 44           NA    NA            NA            NA     NA              39
#> 45           78   230           161            91    252             686
#>    soshjelp_M06M12 soshjelp trygd_M01M05 trygd_M06M12 trygd m01m05 m06m09
#> 1                5       37           NA           NA    28     73     NA
#> 2               12       32           15            6    21     43     11
#> 3               NA       14           NA           NA     8     24      8
#> 4                6       22           16            6    22     45      8
#> 5               19       52           42            9    51     96     29
#> 6               NA       11           19            5    24     34      7
#> 7               16       55           27           13    40     79     20
#> 8               NA       NA           NA           NA    NA     NA     NA
#> 9               NA       NA           NA           NA    NA     10     NA
#> 10              NA       NA           NA           NA    NA     NA     NA
#> 11              10       46           33            8    41     77     14
#> 12               8       33           22            5    27     58     14
#> 13              NA       21           15            7    22     37     10
#> 14               7       29           11            5    16     44     11
#> 15               5       22           NA           NA    NA     27     NA
#> 16               6       21            9            4    13     NA      8
#> 17              NA       24           NA           NA    14     39     NA
#> 18              NA       16           NA           NA    15     37     NA
#> 19              12       46           28            8    36     77     20
#> 20              21      100           48           20    68    164     31
#> 21               6       33           19           12    31     56     11
#> 22              NA        5           NA           NA    NA     NA     NA
#> 23               4        9           NA           NA     6     12     NA
#> 24              NA       NA           NA           NA    NA     NA     NA
#> 25              NA        8           NA           NA    NA     NA     NA
#> 26              21       61           42            5    47    101     16
#> 27               4       24           12            4    16     37      8
#> 28               4       10           NA           NA     8     20     NA
#> 29              12       30           13            5    18     40     12
#> 30               9       46           23            9    32     69     13
#> 31              NA       20           NA           NA    16     41     NA
#> 32               6       17           NA           NA     8     NA     NA
#> 33              NA       27           17            7    24     49     10
#> 34               9       31           NA           NA    15     50     11
#> 35               4       16           NA           NA    12     30     NA
#> 36              NA       NA           NA           NA    NA     NA     NA
#> 37              67      232          156           46   202    407     99
#> 38              96      396          221           76   297    649    134
#> 39              85      306          168           39   207    488     91
#> 40              28      145           92           21   113    249     41
#> 41              93      341          187           69   256    546    124
#> 42             102      378          238           58   296    654    133
#> 43               8       14           NA           NA     6     14      5
#> 44              17       56           NA           NA    35     81     21
#> 45             248      934          545          161   706   1544    324
#>    m10m12 M01M05 M06M12 Total fylke kostragr
#> 1      NA     73     13    86  1100       01
#> 2      16     43     27    70  1100       02
#> 3       4     24     12    36  1100       02
#> 4       7     45     15    60  1100       03
#> 5      12     96     41   137  1100       03
#> 6       5     34     12    46  1100       02
#> 7      18     79     38   117  1100       02
#> 8      NA     NA     NA    NA  1100       04
#> 9      NA     10      8    18  1100       05
#> 10     NA     NA     NA    NA  1100       04
#> 11      8     77     22    99  1200       01
#> 12      6     58     20    78  1200       03
#> 13      9     37     19    56  1200       02
#> 14      8     44     19    63  1200       02
#> 15     NA     27     14    41  1200       05
#> 16     NA     NA     NA    37  1200       02
#> 17     NA     39      7    46  1200       03
#> 18     NA     37     11    48  1200       03
#> 19     10     77     30   107  1200       03
#> 20     25    164     56   220  1200       03
#> 21     11     56     22    78  1200       02
#> 22     NA     NA     NA    10  1200       05
#> 23     NA     12      6    18  1400       05
#> 24     NA     NA     NA     7  1400       05
#> 25     NA     NA     NA    15  1400       04
#> 26     18    101     34   135  1400       03
#> 27      5     37     13    50  1400       03
#> 28     NA     20      8    28  1400       05
#> 29      8     40     20    60  1400       02
#> 30     12     69     25    94  1400       01
#> 31     NA     41      6    47  1400       02
#> 32      5     NA     NA    31  1400       02
#> 33     12     49     22    71  1400       02
#> 34      5     50     16    66  1400       02
#> 35     NA     30      7    37  1400       01
#> 36     NA     NA     NA     4  1400       04
#> 37     70    407    169   576  <NA>     <NA>
#> 38    100    649    234   883  <NA>     <NA>
#> 39     84    488    175   663  <NA>     <NA>
#> 40     26    249     67   316  <NA>     <NA>
#> 41    108    546    232   778  <NA>     <NA>
#> 42     94    654    227   881  <NA>     <NA>
#> 43      6     14     11    25  <NA>     <NA>
#> 44     20     81     41   122  <NA>     <NA>
#> 45    254   1544    578  2122  <NA>     <NA>

 # Same using DotWrap
 DotWrap("ProtectKostra",dots=c("fylke","kostragr"),z3wb,idVar="region",strataVar=c("fylke","kostragr"),freqVar=4:15)
#> .....
#> GaussSuppression_anySum: .........................
#>    region annet_m01m05_M01M05 arbeid_m01m05_M01M05 soshjelp_m01m05_M01M05
#> 1  111100                  12                   NA                     32
#> 2  111200                  NA                   NA                     20
#> 3  111400                  NA                   NA                     NA
#> 4  113300                  NA                   NA                     16
#> 5  113500                   8                   13                     33
#> 6  114100                  NA                   NA                     NA
#> 7  114200                   5                    8                     39
#> 8  114400                  NA                   NA                     NA
#> 9  114500                  NA                   NA                      6
#> 10 115100                  NA                   NA                     NA
#> 11 121100                  NA                   NA                     36
#> 12 122200                  NA                   NA                     25
#> 13 122300                  NA                   NA                     NA
#> 14 123100                  NA                   NA                     22
#> 15 123400                  NA                   NA                     17
#> 16 124100                  NA                   NA                     15
#> 17 124200                  NA                   NA                     NA
#> 18 124400                  NA                   NA                     NA
#> 19 125100                  NA                   NA                     34
#> 20 125900                  NA                   NA                     79
#> 21 126400                  NA                   NA                     27
#> 22 126500                  NA                   NA                     NA
#> 23 141100                  NA                   NA                      5
#> 24 141200                  NA                   NA                     NA
#> 25 141300                  NA                   NA                     NA
#> 26 141600                  NA                   NA                     40
#> 27 141700                  NA                   NA                     20
#> 28 141800                  NA                   NA                      6
#> 29 141900                  NA                   NA                     18
#> 30 142800                   5                    4                     37
#> 31 142900                  NA                    6                     NA
#> 32 143000                  NA                   NA                     11
#> 33 143100                   4                   NA                     NA
#> 34 143300                  NA                   NA                     22
#> 35 144100                  NA                    6                     12
#> 36 144400                  NA                   NA                     NA
#> 37   1100                  35                   51                    165
#> 38   1200                  72                   56                    300
#> 39   1400                  45                   54                    221
#> 40     01                  23                   17                    117
#> 41     02                  45                   66                    248
#> 42     03                  71                   69                    276
#> 43     04                  NA                   NA                      6
#> 44     05                  NA                   NA                     39
#> 45  Total                 152                  161                    686
#>    trygd_m01m05_M01M05 annet_m10m12_M06M12 arbeid_m10m12_M06M12
#> 1                   NA                  NA                   NA
#> 2                   15                  NA                   NA
#> 3                   NA                  NA                   NA
#> 4                   16                  NA                   NA
#> 5                   42                  NA                   NA
#> 6                   19                  NA                   NA
#> 7                   27                  NA                   NA
#> 8                   NA                  NA                   NA
#> 9                   NA                  NA                   NA
#> 10                  NA                  NA                   NA
#> 11                  33                  NA                   NA
#> 12                  22                  NA                   NA
#> 13                  15                  NA                   NA
#> 14                  11                  NA                   NA
#> 15                  NA                  NA                   NA
#> 16                   9                  NA                   NA
#> 17                  NA                  NA                   NA
#> 18                  NA                  NA                   NA
#> 19                  28                  NA                   NA
#> 20                  48                  NA                   NA
#> 21                  19                  NA                   NA
#> 22                  NA                  NA                   NA
#> 23                  NA                  NA                   NA
#> 24                  NA                  NA                   NA
#> 25                  NA                  NA                   NA
#> 26                  42                  NA                   NA
#> 27                  12                  NA                   NA
#> 28                  NA                  NA                   NA
#> 29                  13                  NA                   NA
#> 30                  23                  NA                   NA
#> 31                  NA                  NA                   NA
#> 32                  NA                  NA                   NA
#> 33                  17                  NA                    6
#> 34                  NA                  NA                   NA
#> 35                  NA                  NA                   NA
#> 36                  NA                  NA                   NA
#> 37                 156                  11                   10
#> 38                 221                   8                   13
#> 39                 168                   7                   18
#> 40                  92                  NA                   NA
#> 41                 187                  13                   17
#> 42                 238                   9                   18
#> 43                  NA                  NA                   NA
#> 44                  NA                  NA                   NA
#> 45                 545                  26                   41
#>    soshjelp_m10m12_M06M12 trygd_m10m12_M06M12 annet_m06m09_M06M12
#> 1                      NA                  NA                  NA
#> 2                       7                  NA                   4
#> 3                      NA                  NA                  NA
#> 4                      NA                  NA                  NA
#> 5                       7                  NA                  NA
#> 6                      NA                  NA                  NA
#> 7                       9                   6                  NA
#> 8                      NA                  NA                  NA
#> 9                      NA                  NA                  NA
#> 10                     NA                  NA                  NA
#> 11                     NA                   4                  NA
#> 12                     NA                  NA                  NA
#> 13                      5                  NA                  NA
#> 14                     NA                  NA                   4
#> 15                     NA                  NA                  NA
#> 16                     NA                  NA                  NA
#> 17                     NA                  NA                  NA
#> 18                      5                  NA                  NA
#> 19                      5                  NA                  NA
#> 20                     14                   7                  NA
#> 21                     NA                   5                  NA
#> 22                     NA                  NA                  NA
#> 23                     NA                  NA                  NA
#> 24                     NA                  NA                  NA
#> 25                     NA                  NA                  NA
#> 26                     11                  NA                  NA
#> 27                     NA                  NA                  NA
#> 28                     NA                  NA                  NA
#> 29                      4                  NA                  NA
#> 30                     NA                  NA                  NA
#> 31                     NA                  NA                  NA
#> 32                     NA                  NA                  NA
#> 33                     NA                  NA                  NA
#> 34                      4                  NA                  NA
#> 35                     NA                  NA                  NA
#> 36                     NA                  NA                  NA
#> 37                     33                  16                  21
#> 38                     49                  30                  18
#> 39                     43                  16                  13
#> 40                     13                   8                  NA
#> 41                     47                  31                  21
#> 42                     51                  16                  17
#> 43                     NA                  NA                  NA
#> 44                     NA                  NA                  NA
#> 45                    125                  62                  52
#>    arbeid_m06m09_M06M12 soshjelp_m06m09_M06M12 trygd_m06m09_M06M12 annet_M01M05
#> 1                    NA                     NA                  NA           12
#> 2                    NA                      5                  NA           NA
#> 3                    NA                     NA                  NA           NA
#> 4                    NA                     NA                  NA           NA
#> 5                    NA                     12                  NA            8
#> 6                    NA                     NA                  NA           NA
#> 7                    NA                      7                   7            5
#> 8                    NA                     NA                  NA           NA
#> 9                    NA                     NA                  NA           NA
#> 10                   NA                     NA                  NA           NA
#> 11                   NA                     NA                   4           NA
#> 12                    4                     NA                  NA           NA
#> 13                   NA                     NA                  NA           NA
#> 14                   NA                     NA                  NA           NA
#> 15                   NA                     NA                  NA           NA
#> 16                   NA                     NA                  NA           NA
#> 17                   NA                     NA                  NA           NA
#> 18                   NA                     NA                  NA           NA
#> 19                    4                      7                  NA           NA
#> 20                   NA                      7                  13           NA
#> 21                   NA                     NA                   7           NA
#> 22                   NA                     NA                  NA           NA
#> 23                   NA                     NA                  NA           NA
#> 24                   NA                     NA                  NA           NA
#> 25                   NA                     NA                  NA           NA
#> 26                   NA                     10                  NA           NA
#> 27                   NA                     NA                  NA           NA
#> 28                   NA                     NA                  NA           NA
#> 29                   NA                      8                  NA           NA
#> 30                   NA                     NA                  NA            5
#> 31                   NA                     NA                  NA           NA
#> 32                   NA                     NA                  NA           NA
#> 33                   NA                     NA                  NA            4
#> 34                   NA                      5                  NA           NA
#> 35                   NA                     NA                  NA           NA
#> 36                   NA                     NA                  NA           NA
#> 37                   14                     34                  30           35
#> 38                   23                     47                  46           72
#> 39                   13                     42                  23           45
#> 40                   NA                     15                  13           23
#> 41                   19                     46                  38           45
#> 42                   23                     51                  42           71
#> 43                   NA                     NA                  NA           NA
#> 44                    5                     NA                  NA           NA
#> 45                   50                    123                  99          152
#>    annet_M06M12 annet arbeid_M01M05 arbeid_M06M12 arbeid soshjelp_M01M05
#> 1             4    16            NA            NA      5              32
#> 2            NA     8            NA            NA      9              20
#> 3            NA     7            NA            NA      7              NA
#> 4            NA    NA            NA            NA     NA              16
#> 5             5    13            13             8     21              33
#> 6            NA     6            NA            NA      5              NA
#> 7             5    10             8             4     12              39
#> 8            NA    NA            NA            NA     NA              NA
#> 9            NA    NA            NA            NA     NA               6
#> 10           NA    NA            NA            NA     NA              NA
#> 11           NA     8            NA            NA      4              36
#> 12           NA     8            NA            NA     10              25
#> 13           NA     6            NA            NA      7              NA
#> 14           NA    11            NA            NA      7              22
#> 15           NA     5            NA            NA     NA              17
#> 16           NA    NA            NA            NA     NA              15
#> 17           NA    NA            NA            NA     NA              NA
#> 18           NA     9            NA            NA      8              NA
#> 19           NA    12            NA            NA     13              34
#> 20           NA    26            NA            NA     26              79
#> 21           NA     4            NA            NA     10              27
#> 22           NA    NA            NA            NA     NA              NA
#> 23           NA    NA            NA            NA     NA               5
#> 24           NA    NA            NA            NA     NA              NA
#> 25           NA    NA            NA            NA     NA              NA
#> 26           NA    16            NA            NA     11              40
#> 27           NA     4            NA            NA      6              20
#> 28           NA     4            NA            NA      6               6
#> 29           NA     4            NA            NA      8              18
#> 30           NA    NA             4            NA     NA              37
#> 31           NA    NA             6            NA     NA              NA
#> 32           NA    NA            NA            NA     NA              11
#> 33            4     8            NA            NA     12              NA
#> 34           NA     8            NA            NA     12              22
#> 35           NA    NA             6            NA     NA              12
#> 36           NA    NA            NA            NA     NA              NA
#> 37           32    67            51            24     75             165
#> 38           26    98            56            36     92             300
#> 39           20    65            54            31     85             221
#> 40           11    34            17             7     24             117
#> 41           34    79            66            36    102             248
#> 42           26    97            69            41    110             276
#> 43           NA    NA            NA            NA     NA               6
#> 44           NA    NA            NA            NA     NA              39
#> 45           78   230           161            91    252             686
#>    soshjelp_M06M12 soshjelp trygd_M01M05 trygd_M06M12 trygd m01m05 m06m09
#> 1                5       37           NA           NA    28     73     NA
#> 2               12       32           15            6    21     43     11
#> 3               NA       14           NA           NA     8     24      8
#> 4                6       22           16            6    22     45      8
#> 5               19       52           42            9    51     96     29
#> 6               NA       11           19            5    24     34      7
#> 7               16       55           27           13    40     79     20
#> 8               NA       NA           NA           NA    NA     NA     NA
#> 9               NA       NA           NA           NA    NA     10     NA
#> 10              NA       NA           NA           NA    NA     NA     NA
#> 11              10       46           33            8    41     77     14
#> 12               8       33           22            5    27     58     14
#> 13              NA       21           15            7    22     37     10
#> 14               7       29           11            5    16     44     11
#> 15               5       22           NA           NA    NA     27     NA
#> 16               6       21            9            4    13     NA      8
#> 17              NA       24           NA           NA    14     39     NA
#> 18              NA       16           NA           NA    15     37     NA
#> 19              12       46           28            8    36     77     20
#> 20              21      100           48           20    68    164     31
#> 21               6       33           19           12    31     56     11
#> 22              NA        5           NA           NA    NA     NA     NA
#> 23               4        9           NA           NA     6     12     NA
#> 24              NA       NA           NA           NA    NA     NA     NA
#> 25              NA        8           NA           NA    NA     NA     NA
#> 26              21       61           42            5    47    101     16
#> 27               4       24           12            4    16     37      8
#> 28               4       10           NA           NA     8     20     NA
#> 29              12       30           13            5    18     40     12
#> 30               9       46           23            9    32     69     13
#> 31              NA       20           NA           NA    16     41     NA
#> 32               6       17           NA           NA     8     NA     NA
#> 33              NA       27           17            7    24     49     10
#> 34               9       31           NA           NA    15     50     11
#> 35               4       16           NA           NA    12     30     NA
#> 36              NA       NA           NA           NA    NA     NA     NA
#> 37              67      232          156           46   202    407     99
#> 38              96      396          221           76   297    649    134
#> 39              85      306          168           39   207    488     91
#> 40              28      145           92           21   113    249     41
#> 41              93      341          187           69   256    546    124
#> 42             102      378          238           58   296    654    133
#> 43               8       14           NA           NA     6     14      5
#> 44              17       56           NA           NA    35     81     21
#> 45             248      934          545          161   706   1544    324
#>    m10m12 M01M05 M06M12 Total fylke kostragr
#> 1      NA     73     13    86  1100       01
#> 2      16     43     27    70  1100       02
#> 3       4     24     12    36  1100       02
#> 4       7     45     15    60  1100       03
#> 5      12     96     41   137  1100       03
#> 6       5     34     12    46  1100       02
#> 7      18     79     38   117  1100       02
#> 8      NA     NA     NA    NA  1100       04
#> 9      NA     10      8    18  1100       05
#> 10     NA     NA     NA    NA  1100       04
#> 11      8     77     22    99  1200       01
#> 12      6     58     20    78  1200       03
#> 13      9     37     19    56  1200       02
#> 14      8     44     19    63  1200       02
#> 15     NA     27     14    41  1200       05
#> 16     NA     NA     NA    37  1200       02
#> 17     NA     39      7    46  1200       03
#> 18     NA     37     11    48  1200       03
#> 19     10     77     30   107  1200       03
#> 20     25    164     56   220  1200       03
#> 21     11     56     22    78  1200       02
#> 22     NA     NA     NA    10  1200       05
#> 23     NA     12      6    18  1400       05
#> 24     NA     NA     NA     7  1400       05
#> 25     NA     NA     NA    15  1400       04
#> 26     18    101     34   135  1400       03
#> 27      5     37     13    50  1400       03
#> 28     NA     20      8    28  1400       05
#> 29      8     40     20    60  1400       02
#> 30     12     69     25    94  1400       01
#> 31     NA     41      6    47  1400       02
#> 32      5     NA     NA    31  1400       02
#> 33     12     49     22    71  1400       02
#> 34      5     50     16    66  1400       02
#> 35     NA     30      7    37  1400       01
#> 36     NA     NA     NA     4  1400       04
#> 37     70    407    169   576  <NA>     <NA>
#> 38    100    649    234   883  <NA>     <NA>
#> 39     84    488    175   663  <NA>     <NA>
#> 40     26    249     67   316  <NA>     <NA>
#> 41    108    546    232   778  <NA>     <NA>
#> 42     94    654    227   881  <NA>     <NA>
#> 43      6     14     11    25  <NA>     <NA>
#> 44     20     81     41   122  <NA>     <NA>
#> 45    254   1544    578  2122  <NA>     <NA>

 # Several id variable
 ProtectKostra(z3wb,idVar=c("region","fylke","kostragr"),strataVar=c("fylke","kostragr"),freqVar=4:15,region="region")
#> .....
#> GaussSuppression_anySum: .........................
#>    region_fylke_kostragr annet_m01m05_M01M05 arbeid_m01m05_M01M05
#> 1         111100_1100_01                  12                   NA
#> 2         111200_1100_02                  NA                   NA
#> 3         111400_1100_02                  NA                   NA
#> 4         113300_1100_03                  NA                   NA
#> 5         113500_1100_03                   8                   13
#> 6         114100_1100_02                  NA                   NA
#> 7         114200_1100_02                   5                    8
#> 8         114400_1100_04                  NA                   NA
#> 9         114500_1100_05                  NA                   NA
#> 10        115100_1100_04                  NA                   NA
#> 11        121100_1200_01                  NA                   NA
#> 12        122200_1200_03                  NA                   NA
#> 13        122300_1200_02                  NA                   NA
#> 14        123100_1200_02                  NA                   NA
#> 15        123400_1200_05                  NA                   NA
#> 16        124100_1200_02                  NA                   NA
#> 17        124200_1200_03                  NA                   NA
#> 18        124400_1200_03                  NA                   NA
#> 19        125100_1200_03                  NA                   NA
#> 20        125900_1200_03                  NA                   NA
#> 21        126400_1200_02                  NA                   NA
#> 22        126500_1200_05                  NA                   NA
#> 23        141100_1400_05                  NA                   NA
#> 24        141200_1400_05                  NA                   NA
#> 25        141300_1400_04                  NA                   NA
#> 26        141600_1400_03                  NA                   NA
#> 27        141700_1400_03                  NA                   NA
#> 28        141800_1400_05                  NA                   NA
#> 29        141900_1400_02                  NA                   NA
#> 30        142800_1400_01                   5                    4
#> 31        142900_1400_02                  NA                    6
#> 32        143000_1400_02                  NA                   NA
#> 33        143100_1400_02                   4                   NA
#> 34        143300_1400_02                  NA                   NA
#> 35        144100_1400_01                  NA                    6
#> 36        144400_1400_04                  NA                   NA
#> 37                  1100                  35                   51
#> 38                  1200                  72                   56
#> 39                  1400                  45                   54
#> 40                    01                  23                   17
#> 41                    02                  45                   66
#> 42                    03                  71                   69
#> 43                    04                  NA                   NA
#> 44                    05                  NA                   NA
#> 45                 Total                 152                  161
#>    soshjelp_m01m05_M01M05 trygd_m01m05_M01M05 annet_m10m12_M06M12
#> 1                      32                  NA                  NA
#> 2                      20                  15                  NA
#> 3                      NA                  NA                  NA
#> 4                      16                  16                  NA
#> 5                      33                  42                  NA
#> 6                      NA                  19                  NA
#> 7                      39                  27                  NA
#> 8                      NA                  NA                  NA
#> 9                       6                  NA                  NA
#> 10                     NA                  NA                  NA
#> 11                     36                  33                  NA
#> 12                     25                  22                  NA
#> 13                     NA                  15                  NA
#> 14                     22                  11                  NA
#> 15                     17                  NA                  NA
#> 16                     15                   9                  NA
#> 17                     NA                  NA                  NA
#> 18                     NA                  NA                  NA
#> 19                     34                  28                  NA
#> 20                     79                  48                  NA
#> 21                     27                  19                  NA
#> 22                     NA                  NA                  NA
#> 23                      5                  NA                  NA
#> 24                     NA                  NA                  NA
#> 25                     NA                  NA                  NA
#> 26                     40                  42                  NA
#> 27                     20                  12                  NA
#> 28                      6                  NA                  NA
#> 29                     18                  13                  NA
#> 30                     37                  23                  NA
#> 31                     NA                  NA                  NA
#> 32                     11                  NA                  NA
#> 33                     NA                  17                  NA
#> 34                     22                  NA                  NA
#> 35                     12                  NA                  NA
#> 36                     NA                  NA                  NA
#> 37                    165                 156                  11
#> 38                    300                 221                   8
#> 39                    221                 168                   7
#> 40                    117                  92                  NA
#> 41                    248                 187                  13
#> 42                    276                 238                   9
#> 43                      6                  NA                  NA
#> 44                     39                  NA                  NA
#> 45                    686                 545                  26
#>    arbeid_m10m12_M06M12 soshjelp_m10m12_M06M12 trygd_m10m12_M06M12
#> 1                    NA                     NA                  NA
#> 2                    NA                      7                  NA
#> 3                    NA                     NA                  NA
#> 4                    NA                     NA                  NA
#> 5                    NA                      7                  NA
#> 6                    NA                     NA                  NA
#> 7                    NA                      9                   6
#> 8                    NA                     NA                  NA
#> 9                    NA                     NA                  NA
#> 10                   NA                     NA                  NA
#> 11                   NA                     NA                   4
#> 12                   NA                     NA                  NA
#> 13                   NA                      5                  NA
#> 14                   NA                     NA                  NA
#> 15                   NA                     NA                  NA
#> 16                   NA                     NA                  NA
#> 17                   NA                     NA                  NA
#> 18                   NA                      5                  NA
#> 19                   NA                      5                  NA
#> 20                   NA                     14                   7
#> 21                   NA                     NA                   5
#> 22                   NA                     NA                  NA
#> 23                   NA                     NA                  NA
#> 24                   NA                     NA                  NA
#> 25                   NA                     NA                  NA
#> 26                   NA                     11                  NA
#> 27                   NA                     NA                  NA
#> 28                   NA                     NA                  NA
#> 29                   NA                      4                  NA
#> 30                   NA                     NA                  NA
#> 31                   NA                     NA                  NA
#> 32                   NA                     NA                  NA
#> 33                    6                     NA                  NA
#> 34                   NA                      4                  NA
#> 35                   NA                     NA                  NA
#> 36                   NA                     NA                  NA
#> 37                   10                     33                  16
#> 38                   13                     49                  30
#> 39                   18                     43                  16
#> 40                   NA                     13                   8
#> 41                   17                     47                  31
#> 42                   18                     51                  16
#> 43                   NA                     NA                  NA
#> 44                   NA                     NA                  NA
#> 45                   41                    125                  62
#>    annet_m06m09_M06M12 arbeid_m06m09_M06M12 soshjelp_m06m09_M06M12
#> 1                   NA                   NA                     NA
#> 2                    4                   NA                      5
#> 3                   NA                   NA                     NA
#> 4                   NA                   NA                     NA
#> 5                   NA                   NA                     12
#> 6                   NA                   NA                     NA
#> 7                   NA                   NA                      7
#> 8                   NA                   NA                     NA
#> 9                   NA                   NA                     NA
#> 10                  NA                   NA                     NA
#> 11                  NA                   NA                     NA
#> 12                  NA                    4                     NA
#> 13                  NA                   NA                     NA
#> 14                   4                   NA                     NA
#> 15                  NA                   NA                     NA
#> 16                  NA                   NA                     NA
#> 17                  NA                   NA                     NA
#> 18                  NA                   NA                     NA
#> 19                  NA                    4                      7
#> 20                  NA                   NA                      7
#> 21                  NA                   NA                     NA
#> 22                  NA                   NA                     NA
#> 23                  NA                   NA                     NA
#> 24                  NA                   NA                     NA
#> 25                  NA                   NA                     NA
#> 26                  NA                   NA                     10
#> 27                  NA                   NA                     NA
#> 28                  NA                   NA                     NA
#> 29                  NA                   NA                      8
#> 30                  NA                   NA                     NA
#> 31                  NA                   NA                     NA
#> 32                  NA                   NA                     NA
#> 33                  NA                   NA                     NA
#> 34                  NA                   NA                      5
#> 35                  NA                   NA                     NA
#> 36                  NA                   NA                     NA
#> 37                  21                   14                     34
#> 38                  18                   23                     47
#> 39                  13                   13                     42
#> 40                  NA                   NA                     15
#> 41                  21                   19                     46
#> 42                  17                   23                     51
#> 43                  NA                   NA                     NA
#> 44                  NA                    5                     NA
#> 45                  52                   50                    123
#>    trygd_m06m09_M06M12 annet_M01M05 annet_M06M12 annet arbeid_M01M05
#> 1                   NA           12            4    16            NA
#> 2                   NA           NA           NA     8            NA
#> 3                   NA           NA           NA     7            NA
#> 4                   NA           NA           NA    NA            NA
#> 5                   NA            8            5    13            13
#> 6                   NA           NA           NA     6            NA
#> 7                    7            5            5    10             8
#> 8                   NA           NA           NA    NA            NA
#> 9                   NA           NA           NA    NA            NA
#> 10                  NA           NA           NA    NA            NA
#> 11                   4           NA           NA     8            NA
#> 12                  NA           NA           NA     8            NA
#> 13                  NA           NA           NA     6            NA
#> 14                  NA           NA           NA    11            NA
#> 15                  NA           NA           NA     5            NA
#> 16                  NA           NA           NA    NA            NA
#> 17                  NA           NA           NA    NA            NA
#> 18                  NA           NA           NA     9            NA
#> 19                  NA           NA           NA    12            NA
#> 20                  13           NA           NA    26            NA
#> 21                   7           NA           NA     4            NA
#> 22                  NA           NA           NA    NA            NA
#> 23                  NA           NA           NA    NA            NA
#> 24                  NA           NA           NA    NA            NA
#> 25                  NA           NA           NA    NA            NA
#> 26                  NA           NA           NA    16            NA
#> 27                  NA           NA           NA     4            NA
#> 28                  NA           NA           NA     4            NA
#> 29                  NA           NA           NA     4            NA
#> 30                  NA            5           NA    NA             4
#> 31                  NA           NA           NA    NA             6
#> 32                  NA           NA           NA    NA            NA
#> 33                  NA            4            4     8            NA
#> 34                  NA           NA           NA     8            NA
#> 35                  NA           NA           NA    NA             6
#> 36                  NA           NA           NA    NA            NA
#> 37                  30           35           32    67            51
#> 38                  46           72           26    98            56
#> 39                  23           45           20    65            54
#> 40                  13           23           11    34            17
#> 41                  38           45           34    79            66
#> 42                  42           71           26    97            69
#> 43                  NA           NA           NA    NA            NA
#> 44                  NA           NA           NA    NA            NA
#> 45                  99          152           78   230           161
#>    arbeid_M06M12 arbeid soshjelp_M01M05 soshjelp_M06M12 soshjelp trygd_M01M05
#> 1             NA      5              32               5       37           NA
#> 2             NA      9              20              12       32           15
#> 3             NA      7              NA              NA       14           NA
#> 4             NA     NA              16               6       22           16
#> 5              8     21              33              19       52           42
#> 6             NA      5              NA              NA       11           19
#> 7              4     12              39              16       55           27
#> 8             NA     NA              NA              NA       NA           NA
#> 9             NA     NA               6              NA       NA           NA
#> 10            NA     NA              NA              NA       NA           NA
#> 11            NA      4              36              10       46           33
#> 12            NA     10              25               8       33           22
#> 13            NA      7              NA              NA       21           15
#> 14            NA      7              22               7       29           11
#> 15            NA     NA              17               5       22           NA
#> 16            NA     NA              15               6       21            9
#> 17            NA     NA              NA              NA       24           NA
#> 18            NA      8              NA              NA       16           NA
#> 19            NA     13              34              12       46           28
#> 20            NA     26              79              21      100           48
#> 21            NA     10              27               6       33           19
#> 22            NA     NA              NA              NA        5           NA
#> 23            NA     NA               5               4        9           NA
#> 24            NA     NA              NA              NA       NA           NA
#> 25            NA     NA              NA              NA        8           NA
#> 26            NA     11              40              21       61           42
#> 27            NA      6              20               4       24           12
#> 28            NA      6               6               4       10           NA
#> 29            NA      8              18              12       30           13
#> 30            NA     NA              37               9       46           23
#> 31            NA     NA              NA              NA       20           NA
#> 32            NA     NA              11               6       17           NA
#> 33            NA     12              NA              NA       27           17
#> 34            NA     12              22               9       31           NA
#> 35            NA     NA              12               4       16           NA
#> 36            NA     NA              NA              NA       NA           NA
#> 37            24     75             165              67      232          156
#> 38            36     92             300              96      396          221
#> 39            31     85             221              85      306          168
#> 40             7     24             117              28      145           92
#> 41            36    102             248              93      341          187
#> 42            41    110             276             102      378          238
#> 43            NA     NA               6               8       14           NA
#> 44            NA     NA              39              17       56           NA
#> 45            91    252             686             248      934          545
#>    trygd_M06M12 trygd m01m05 m06m09 m10m12 M01M05 M06M12 Total region
#> 1            NA    28     73     NA     NA     73     13    86 111100
#> 2             6    21     43     11     16     43     27    70 111200
#> 3            NA     8     24      8      4     24     12    36 111400
#> 4             6    22     45      8      7     45     15    60 113300
#> 5             9    51     96     29     12     96     41   137 113500
#> 6             5    24     34      7      5     34     12    46 114100
#> 7            13    40     79     20     18     79     38   117 114200
#> 8            NA    NA     NA     NA     NA     NA     NA    NA 114400
#> 9            NA    NA     10     NA     NA     10      8    18 114500
#> 10           NA    NA     NA     NA     NA     NA     NA    NA 115100
#> 11            8    41     77     14      8     77     22    99 121100
#> 12            5    27     58     14      6     58     20    78 122200
#> 13            7    22     37     10      9     37     19    56 122300
#> 14            5    16     44     11      8     44     19    63 123100
#> 15           NA    NA     27     NA     NA     27     14    41 123400
#> 16            4    13     NA      8     NA     NA     NA    37 124100
#> 17           NA    14     39     NA     NA     39      7    46 124200
#> 18           NA    15     37     NA     NA     37     11    48 124400
#> 19            8    36     77     20     10     77     30   107 125100
#> 20           20    68    164     31     25    164     56   220 125900
#> 21           12    31     56     11     11     56     22    78 126400
#> 22           NA    NA     NA     NA     NA     NA     NA    10 126500
#> 23           NA     6     12     NA     NA     12      6    18 141100
#> 24           NA    NA     NA     NA     NA     NA     NA     7 141200
#> 25           NA    NA     NA     NA     NA     NA     NA    15 141300
#> 26            5    47    101     16     18    101     34   135 141600
#> 27            4    16     37      8      5     37     13    50 141700
#> 28           NA     8     20     NA     NA     20      8    28 141800
#> 29            5    18     40     12      8     40     20    60 141900
#> 30            9    32     69     13     12     69     25    94 142800
#> 31           NA    16     41     NA     NA     41      6    47 142900
#> 32           NA     8     NA     NA      5     NA     NA    31 143000
#> 33            7    24     49     10     12     49     22    71 143100
#> 34           NA    15     50     11      5     50     16    66 143300
#> 35           NA    12     30     NA     NA     30      7    37 144100
#> 36           NA    NA     NA     NA     NA     NA     NA     4 144400
#> 37           46   202    407     99     70    407    169   576   <NA>
#> 38           76   297    649    134    100    649    234   883   <NA>
#> 39           39   207    488     91     84    488    175   663   <NA>
#> 40           21   113    249     41     26    249     67   316   <NA>
#> 41           69   256    546    124    108    546    232   778   <NA>
#> 42           58   296    654    133     94    654    227   881   <NA>
#> 43           NA     6     14      5      6     14     11    25   <NA>
#> 44           NA    35     81     21     20     81     41   122   <NA>
#> 45          161   706   1544    324    254   1544    578  2122   <NA>

 # ==================================
 #    Examples with freqVarGroup
 # ==================================

 # Generate example data for this function
 exData   <- KostraData("z3w")[,c(1:15,15,4:6)]
 names(exData)[12:19]=c("s1","s2","s3","s4","A","B","C","D")
 exData[,"s4"] <- rowSums(exData[,4:11]) - rowSums(exData[,12:14])

 # Create input parameter
 freqVarGroup <- c(1,1,1,1,1,1,1,1,2,2,2,2,3,4,-1,5) # Same as c(rep(1,8),rep(2,4),3,4,-1,5)

 a <- ProtectKostra(exData ,idVar="region",strataVar=c("fylke","kostragr"),freqVar=4:19,freqVarGroup=freqVarGroup)
#> .....
#> GaussSuppression_anySum: .........................
#> .....
#> GaussSuppression_anySum: .........................
#> .....
#> GaussSuppression_anySum: ....................
#> .....
#> GaussSuppression_anySum: ........................
#> .....
#> GaussSuppression_anySum: ........................................
 #  Now output of a$C is just missing since "-1"

 names(exData)[18] <- "arbeid" #  Rename from "C" to "arbeid"

 b <- ProtectKostra(exData ,idVar="region",strataVar=c("fylke","kostragr"),freqVar=4:19,freqVarGroup=freqVarGroup)
#> .....
#> GaussSuppression_anySum: .........................
#> .....
#> GaussSuppression_anySum: .........................
#> .....
#> GaussSuppression_anySum: ....................
#> .....
#> GaussSuppression_anySum: ........................
#> .....
#> GaussSuppression_anySum: ........................................
 # Now "arbeid" in output is still between "B" and "D" as in input. And b$arbeid is NOT just missing

 # singleTotal=TRUE
 ProtectKostra(exData ,idVar="region",strataVar=c("fylke","kostragr"),freqVar=4:19,freqVarGroup=freqVarGroup, singleTotal=TRUE)
#> .....
#> GaussSuppression_anySum: .........................
#> .....
#> GaussSuppression_anySum: .........................
#> .....
#> GaussSuppression_anySum: ....................
#> .....
#> GaussSuppression_anySum: ........................
#> .....
#> GaussSuppression_anySum: ........................................
#>    region annet_m01m05 arbeid_m01m05 soshjelp_m01m05 trygd_m01m05 annet_m10m12
#> 1  111100           NA            NA              NA           NA           NA
#> 2  111200           NA            NA              20           15           NA
#> 3  111400           NA            NA              NA           NA           NA
#> 4  113300           NA            11              16           NA           NA
#> 5  113500           NA            NA              33           NA           NA
#> 6  114100           NA            NA              NA           NA           NA
#> 7  114200           NA            NA              39           27           NA
#> 8  114400           NA            NA              NA           NA           NA
#> 9  114500           NA            NA              NA           NA           NA
#> 10 115100           NA            NA              NA           NA           NA
#> 11 121100            5            NA              NA           33           NA
#> 12 122200           NA            NA              NA           NA           NA
#> 13 122300           NA            NA              12           NA           NA
#> 14 123100           NA            NA              22           NA           NA
#> 15 123400           NA            NA              NA            8           NA
#> 16 124100           NA            NA              NA           NA           NA
#> 17 124200           NA            NA              NA           NA           NA
#> 18 124400           NA            NA               7           NA           NA
#> 19 125100           NA            NA              34           NA           NA
#> 20 125900           NA            NA              79           48           NA
#> 21 126400           NA            NA              27           19           NA
#> 22 126500           NA            NA              NA           NA           NA
#> 23 141100           NA            NA              NA           NA           NA
#> 24 141200           NA            NA              NA           NA           NA
#> 25 141300           NA            NA              NA           NA           NA
#> 26 141600           NA            NA              40           NA           NA
#> 27 141700           NA            NA              NA           NA           NA
#> 28 141800           NA            NA              NA           NA           NA
#> 29 141900           NA             6              18           NA           NA
#> 30 142800           NA            NA              37           NA           NA
#> 31 142900           NA             6              NA           NA           NA
#> 32 143000           NA            NA              NA           NA           NA
#> 33 143100           NA            NA              NA           NA           NA
#> 34 143300           NA            NA              22           NA           NA
#> 35 144100           NA             6              NA           NA           NA
#> 36 144400           NA            NA              NA           NA           NA
#> 37   1100           35            51             165          156           11
#> 38   1200           72            56             300          221            8
#> 39   1400           45            54             221          168            7
#> 40     01           NA            NA             117           92           NA
#> 41     02           45            66             248          187           13
#> 42     03           71            69             276          238            9
#> 43     04           NA            NA               6           NA           NA
#> 44     05           11            NA              39           NA           NA
#> 45  Total          152           161             686          545           26
#>    arbeid_m10m12 soshjelp_m10m12 trygd_m10m12 s1 s2  s3   s4  A   B arbeid   D
#> 1             NA              NA           NA  4 NA  NA   69 NA  12      5  32
#> 2             NA               7            4 NA NA   5   50 NA  NA      9  20
#> 3             NA              NA           NA NA NA  NA   20 NA  NA      4  11
#> 4             NA               4           NA NA NA  NA   49  5  NA     NA  16
#> 5             NA               7           NA  4  6  12   86  7   8     15  33
#> 6             NA              NA           NA NA NA  NA   35 NA   4      4   8
#> 7             NA               9            6 NA NA   7   84  7   5      9  39
#> 8             NA              NA           NA NA NA  NA   NA NA  NA     NA  NA
#> 9             NA              NA           NA NA NA  NA   NA NA  NA     NA  NA
#> 10            NA              NA           NA NA NA  NA   NA NA  NA     NA  NA
#> 11            NA              NA            4 NA NA   7   75  4   5     NA  36
#> 12            NA              NA           NA NA NA   6   54  4   6      6  25
#> 13            NA               5           NA NA NA   4   40  4   6      5  12
#> 14            NA               4           NA  4 NA  NA   44 NA   6      6  22
#> 15            NA              NA           NA NA NA  NA   29 NA  NA     NA  17
#> 16            NA              NA           NA NA NA   4   24 NA  NA     NA  15
#> 17            NA              NA           NA NA NA  NA   42 NA  NA     NA  22
#> 18            NA               5           NA NA NA   4   38 NA   7      8   7
#> 19            NA               5           NA NA NA   7   73  6   9      9  34
#> 20            NA              14            7 NA  8  NA  171 13  23     18  79
#> 21            NA               4            5 NA NA  NA   63  7  NA      8  27
#> 22            NA              NA           NA NA NA  NA    8 NA  NA     NA   4
#> 23            NA              NA           NA NA NA  NA   14 NA  NA     NA   5
#> 24            NA              NA           NA NA NA  NA   NA NA  NA     NA  NA
#> 25            NA              NA           NA NA NA  NA    9 NA  NA     NA  NA
#> 26            NA              11           NA NA NA  10  106 NA  10     11  40
#> 27            NA              NA           NA NA NA  NA   38  4  NA      5  20
#> 28            NA              NA           NA NA NA  NA   19 NA   4      4   6
#> 29            NA               4           NA NA NA   8   38 NA  NA     NA  18
#> 30            NA               7           NA NA NA  NA   74  6  NA      5  37
#> 31            NA              NA           NA NA NA  NA   44 NA  NA     NA  18
#> 32            NA              NA           NA NA NA  NA   25 NA  NA     NA  11
#> 33            NA              NA           NA NA NA  NA   55  4   4      9  25
#> 34            NA               4           NA NA NA   5   46 NA   6     10  22
#> 35            NA              NA           NA NA NA  NA   29 NA  NA     NA  12
#> 36            NA              NA           NA NA NA  NA   NA NA  NA     NA  NA
#> 37            10              33           16 21 14  34  408 30  35     61 165
#> 38            13              49           30 18 23  47  661 46  72     69 300
#> 39            18              43           16 13 13  42  504 23  45     72 221
#> 40            NA              13            8 NA NA  15  247 13  23     21 117
#> 41            17              47           31 21 19  46  568 38  45     83 248
#> 42            18              51           16 17 23  51  657 42  71     87 276
#> 43            NA               5           NA NA NA  NA   16 NA  NA     NA   6
#> 44            NA               9           NA NA  5  NA   85 NA  NA     NA  39
#> 45            41             125           62 52 50 123 1573 99 152    202 686
#>    annet soshjelp trygd m01m05 m10m12 Total
#> 1     12       34    25     NA     NA    76
#> 2      4       27    19     43     16    59
#> 3      4       12     8     24      4    28
#> 4     NA       20    17     45      7    52
#> 5      9       40    44     96     12   108
#> 6      5        9    21     34      5    39
#> 7      7       48    33     79     18    97
#> 8     NA       NA    NA     NA     NA    NA
#> 9     NA       NA    NA     NA     NA    13
#> 10    NA       NA    NA     NA     NA    NA
#> 11    NA       39    37     77      8    85
#> 12     8       27    23     58      6    64
#> 13     6       17    18     37      9    46
#> 14     7       26    13     44      8    52
#> 15    NA       20    NA     27      7    34
#> 16    NA       17    10     NA     NA    29
#> 17    NA       24    14     39      5    44
#> 18     8       12    15     37      6    43
#> 19     9       39    30     77     10    87
#> 20    23       93    55    164     25   189
#> 21     4       31    24     56     11    67
#> 22    NA        4    NA     NA     NA     9
#> 23    NA        8     6     12      4    16
#> 24    NA       NA    NA     NA     NA     6
#> 25    NA        6    NA      8     NA    NA
#> 26    13       51    44    101     18   119
#> 27     4       21    12     37      5    42
#> 28     4        8     7     NA     NA    23
#> 29    NA       22    16     40      8    48
#> 30     6       44    26     69     12    81
#> 31    NA       19    15     41      4    45
#> 32    NA       14     8     NA     NA    28
#> 33     5       27    20     49     12    61
#> 34     6       26    13     50      5    55
#> 35    NA       13    12     NA     NA    33
#> 36    NA       NA    NA     NA     NA    NA
#> 37    46      198   172    407     70   477
#> 38    80      349   251    649    100   749
#> 39    52      264   184    488     84   572
#> 40    24      130   100    249     26   275
#> 41    58      295   218    546    108   654
#> 42    80      327   254    654     94   748
#> 43    NA       11     5     14      6    20
#> 44    NA       48    30     81     20   101
#> 45   178      811   607   1544    254  1798

 exData[4,4] <- 3  # Warning will be produced
 ProtectKostra(exData ,idVar="region",strataVar=c("fylke","kostragr"),freqVar=4:19,freqVarGroup=freqVarGroup)
#> Warning: Not identical rowsums in all groups. First result named as Total
#> .....
#> GaussSuppression_anySum: .........................
#> .....
#> GaussSuppression_anySum: .........................
#> .....
#> GaussSuppression_anySum: ....................
#> .....
#> GaussSuppression_anySum: ........................
#> .....
#> GaussSuppression_anySum: ........................................
#>    region annet_m01m05 arbeid_m01m05 soshjelp_m01m05 trygd_m01m05 annet_m10m12
#> 1  111100           NA            NA              NA           NA           NA
#> 2  111200           NA            NA              20           15           NA
#> 3  111400           NA            NA              NA           NA           NA
#> 4  113300           NA            11              16           NA           NA
#> 5  113500           NA            NA              33           NA           NA
#> 6  114100           NA            NA              NA           NA           NA
#> 7  114200           NA            NA              39           27           NA
#> 8  114400           NA            NA              NA           NA           NA
#> 9  114500           NA            NA              NA           NA           NA
#> 10 115100           NA            NA              NA           NA           NA
#> 11 121100            5            NA              NA           33           NA
#> 12 122200           NA            NA              NA           NA           NA
#> 13 122300           NA            NA              12           NA           NA
#> 14 123100           NA            NA              22           NA           NA
#> 15 123400           NA            NA              NA            8           NA
#> 16 124100           NA            NA              NA           NA           NA
#> 17 124200           NA            NA              NA           NA           NA
#> 18 124400           NA            NA               7           NA           NA
#> 19 125100           NA            NA              34           NA           NA
#> 20 125900           NA            NA              79           48           NA
#> 21 126400           NA            NA              27           19           NA
#> 22 126500           NA            NA              NA           NA           NA
#> 23 141100           NA            NA              NA           NA           NA
#> 24 141200           NA            NA              NA           NA           NA
#> 25 141300           NA            NA              NA           NA           NA
#> 26 141600           NA            NA              40           NA           NA
#> 27 141700           NA            NA              NA           NA           NA
#> 28 141800           NA            NA              NA           NA           NA
#> 29 141900           NA             6              18           NA           NA
#> 30 142800           NA            NA              37           NA           NA
#> 31 142900           NA             6              NA           NA           NA
#> 32 143000           NA            NA              NA           NA           NA
#> 33 143100           NA            NA              NA           NA           NA
#> 34 143300           NA            NA              22           NA           NA
#> 35 144100           NA             6              NA           NA           NA
#> 36 144400           NA            NA              NA           NA           NA
#> 37   1100           36            51             165          156           11
#> 38   1200           72            56             300          221            8
#> 39   1400           45            54             221          168            7
#> 40     01           NA            NA             117           92           NA
#> 41     02           45            66             248          187           13
#> 42     03           72            69             276          238            9
#> 43     04           NA            NA               6           NA           NA
#> 44     05           11            NA              39           NA           NA
#> 45  Total          153           161             686          545           26
#>    arbeid_m10m12 soshjelp_m10m12 trygd_m10m12 s1 s2  s3   s4  A   B arbeid   D
#> 1             NA              NA           NA  4 NA  NA   69 NA  12      5  32
#> 2             NA               7            4 NA NA   5   50 NA  NA      9  20
#> 3             NA              NA           NA NA NA  NA   20 NA  NA      4  11
#> 4             NA               4           NA NA NA  NA   49  5  NA     NA  16
#> 5             NA               7           NA  4  6  12   86  7   8     15  33
#> 6             NA              NA           NA NA NA  NA   35 NA   4      4   8
#> 7             NA               9            6 NA NA   7   84  7   5      9  39
#> 8             NA              NA           NA NA NA  NA   NA NA  NA     NA  NA
#> 9             NA              NA           NA NA NA  NA   NA NA  NA     NA  NA
#> 10            NA              NA           NA NA NA  NA   NA NA  NA     NA  NA
#> 11            NA              NA            4 NA NA   7   75  4   5     NA  36
#> 12            NA              NA           NA NA NA   6   54  4   6      6  25
#> 13            NA               5           NA NA NA   4   40  4   6      5  12
#> 14            NA               4           NA  4 NA  NA   44 NA   6      6  22
#> 15            NA              NA           NA NA NA  NA   29 NA  NA     NA  17
#> 16            NA              NA           NA NA NA   4   24 NA  NA     NA  15
#> 17            NA              NA           NA NA NA  NA   42 NA  NA     NA  22
#> 18            NA               5           NA NA NA   4   38 NA   7      8   7
#> 19            NA               5           NA NA NA   7   73  6   9      9  34
#> 20            NA              14            7 NA  8  NA  171 13  23     18  79
#> 21            NA               4            5 NA NA  NA   63  7  NA      8  27
#> 22            NA              NA           NA NA NA  NA    8 NA  NA     NA   4
#> 23            NA              NA           NA NA NA  NA   14 NA  NA     NA   5
#> 24            NA              NA           NA NA NA  NA   NA NA  NA     NA  NA
#> 25            NA              NA           NA NA NA  NA    9 NA  NA     NA  NA
#> 26            NA              11           NA NA NA  10  106 NA  10     11  40
#> 27            NA              NA           NA NA NA  NA   38  4  NA      5  20
#> 28            NA              NA           NA NA NA  NA   19 NA   4      4   6
#> 29            NA               4           NA NA NA   8   38 NA  NA     NA  18
#> 30            NA               7           NA NA NA  NA   74  6  NA      5  37
#> 31            NA              NA           NA NA NA  NA   44 NA  NA     NA  18
#> 32            NA              NA           NA NA NA  NA   25 NA  NA     NA  11
#> 33            NA              NA           NA NA NA  NA   55  4   4      9  25
#> 34            NA               4           NA NA NA   5   46 NA   6     10  22
#> 35            NA              NA           NA NA NA  NA   29 NA  NA     NA  12
#> 36            NA              NA           NA NA NA  NA   NA NA  NA     NA  NA
#> 37            10              33           16 21 14  34  408 30  35     61 165
#> 38            13              49           30 18 23  47  661 46  72     69 300
#> 39            18              43           16 13 13  42  504 23  45     72 221
#> 40            NA              13            8 NA NA  15  247 13  23     21 117
#> 41            17              47           31 21 19  46  568 38  45     83 248
#> 42            18              51           16 17 23  51  657 42  71     87 276
#> 43            NA               5           NA NA NA  NA   16 NA  NA     NA   6
#> 44            NA               9           NA NA  5  NA   85 NA  NA     NA  39
#> 45            41             125           62 52 50 123 1573 99 152    202 686
#>    annet soshjelp trygd m01m05 m10m12 Total_1 Total_2 Total
#> 1     12       34    25     NA     NA      76      76    76
#> 2      4       27    19     43     16      59      59    59
#> 3      4       12     8     24      4      28      28    28
#> 4     NA       20    17     46      7      53      52    53
#> 5      9       40    44     96     12     108     108   108
#> 6      5        9    21     34      5      39      39    39
#> 7      7       48    33     79     18      97      97    97
#> 8     NA       NA    NA     NA     NA      NA      NA    NA
#> 9     NA       NA    NA     NA     NA      13      13    13
#> 10    NA       NA    NA     NA     NA      NA      NA    NA
#> 11    NA       39    37     77      8      85      85    85
#> 12     8       27    23     58      6      64      64    64
#> 13     6       17    18     37      9      46      46    46
#> 14     7       26    13     44      8      52      52    52
#> 15    NA       20    NA     27      7      34      34    34
#> 16    NA       17    10     NA     NA      29      29    29
#> 17    NA       24    14     39      5      44      44    44
#> 18     8       12    15     37      6      43      43    43
#> 19     9       39    30     77     10      87      87    87
#> 20    23       93    55    164     25     189     189   189
#> 21     4       31    24     56     11      67      67    67
#> 22    NA        4    NA     NA     NA       9       9     9
#> 23    NA        8     6     12      4      16      16    16
#> 24    NA       NA    NA     NA     NA       6       6     6
#> 25    NA        6    NA      8     NA      NA      NA    NA
#> 26    13       51    44    101     18     119     119   119
#> 27     4       21    12     37      5      42      42    42
#> 28     4        8     7     NA     NA      23      23    23
#> 29    NA       22    16     40      8      48      48    48
#> 30     6       44    26     69     12      81      81    81
#> 31    NA       19    15     41      4      45      45    45
#> 32    NA       14     8     NA     NA      28      28    28
#> 33     5       27    20     49     12      61      61    61
#> 34     6       26    13     50      5      55      55    55
#> 35    NA       13    12     NA     NA      33      33    33
#> 36    NA       NA    NA     NA     NA      NA      NA    NA
#> 37    47      198   172    408     70     478     477   478
#> 38    80      349   251    649    100     749     749   749
#> 39    52      264   184    488     84     572     572   572
#> 40    24      130   100    249     26     275     275   275
#> 41    58      295   218    546    108     654     654   654
#> 42    81      327   254    655     94     749     748   749
#> 43    NA       11     5     14      6      20      20    20
#> 44    NA       48    30     81     20     101     101   101
#> 45   179      811   607   1545    254    1799    1798  1799

 freqVarGroup <- c(11,11,11,11,11,11,11,11,2,2,2,2,3,4,0,5)  # Using this instead give same result in different order


 # ========================================
 #    Examples with a single freq-variable
 # ========================================

 z1w = KostraData("z1w")
 ProtectKostra(z1w,idVar="region",freqVar=2)  # wrong "name"
#> .....
#>   region suppressed
#> 1 011100         11
#> 2 043200          7
#> 3 051300          5
#> 4 051400         13
#> 5 061900          9
#> 6 062000         12
#> 7 083300          6
#> 8 083400          9
#> 9  Total         72
 ProtectKostra(z1w,idVar="region",freqVar=2, freqVarGroup=1) # same name as input
#> .....
#>   region annet
#> 1 011100    11
#> 2 043200     7
#> 3 051300     5
#> 4 051400    13
#> 5 061900     9
#> 6 062000    12
#> 7 083300     6
#> 8 083400     9
#> 9  Total    72
```
