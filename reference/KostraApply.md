# Apply a function with data frame output over data subsets

Apply a function with data frame output over data subsets

## Usage

``` r
KostraApply(data, by, Fun, ...)
```

## Arguments

- data:

  Data frame

- by:

  Vector of variable names (or variable numbers) defining the subsets or
  alternatively a named list or data frame (see examples).

- Fun:

  Function to be applied on data subsets

- ...:

  Arguments passed to Fun

## Value

A data.frame

## Note

The output ordering of the example depends on
getOption("stringsAsFactors")

## Examples

``` r
z1w <- KostraData("z1w")
zz  <- rbind(cbind(aar=2014,z1w),cbind(aar=2015,z1w))
zzz <- rbind(cbind(ab="b",zz),cbind(ab="a",zz))
zzz$annet[c(1,9,17,25)] =10*(1:4)

# All subset in data by vector input
KostraApply(zzz, by = c("aar", "ab"), Fun = ProtectKostra, idVar = "region", freqVar = 4:7)
#> .....
#> GaussSuppression_anySum: ..........................................
#> .....
#> GaussSuppression_anySum: ..........................................
#> .....
#> GaussSuppression_anySum: ..........................................
#> .....
#> GaussSuppression_anySum: ..........................................
#>     aar ab region annet arbeid soshjelp trygd Total
#> 1  2014  a 011100    30     11       55    36   132
#> 2  2014  a 043200    NA     NA       29    18    55
#> 3  2014  a 051300     5      8       35    25    73
#> 4  2014  a 051400    NA     NA       17    13    45
#> 5  2014  a 061900     9     14       63    52   138
#> 6  2014  a 062000    12      9       24    22    67
#> 7  2014  a 083300     6      4       22     8    40
#> 8  2014  a 083400    NA     NA       38    15    65
#> 9  2014  a  Total    91     52      283   189   615
#> 10 2014  b 011100    10     11       55    36   112
#> 11 2014  b 043200    NA     NA       29    18    55
#> 12 2014  b 051300     5      8       35    25    73
#> 13 2014  b 051400    NA     NA       17    13    45
#> 14 2014  b 061900     9     14       63    52   138
#> 15 2014  b 062000    12      9       24    22    67
#> 16 2014  b 083300     6      4       22     8    40
#> 17 2014  b 083400    NA     NA       38    15    65
#> 18 2014  b  Total    71     52      283   189   595
#> 19 2015  a 011100    40     11       55    36   142
#> 20 2015  a 043200    NA     NA       29    18    55
#> 21 2015  a 051300     5      8       35    25    73
#> 22 2015  a 051400    NA     NA       17    13    45
#> 23 2015  a 061900     9     14       63    52   138
#> 24 2015  a 062000    12      9       24    22    67
#> 25 2015  a 083300     6      4       22     8    40
#> 26 2015  a 083400    NA     NA       38    15    65
#> 27 2015  a  Total   101     52      283   189   625
#> 28 2015  b 011100    20     11       55    36   122
#> 29 2015  b 043200    NA     NA       29    18    55
#> 30 2015  b 051300     5      8       35    25    73
#> 31 2015  b 051400    NA     NA       17    13    45
#> 32 2015  b 061900     9     14       63    52   138
#> 33 2015  b 062000    12      9       24    22    67
#> 34 2015  b 083300     6      4       22     8    40
#> 35 2015  b 083400    NA     NA       38    15    65
#> 36 2015  b  Total    81     52      283   189   605

# Selected subsets by using list input to specify each variable
KostraApply(zzz, by = list(aar = c(2014, 2015), ab = "a"), Fun = ProtectKostra, idVar = "region", freqVar = 4:7)
#> .....
#> GaussSuppression_anySum: ..........................................
#> .....
#> GaussSuppression_anySum: ..........................................
#>     aar ab region annet arbeid soshjelp trygd Total
#> 1  2014  a 011100    30     11       55    36   132
#> 2  2014  a 043200    NA     NA       29    18    55
#> 3  2014  a 051300     5      8       35    25    73
#> 4  2014  a 051400    NA     NA       17    13    45
#> 5  2014  a 061900     9     14       63    52   138
#> 6  2014  a 062000    12      9       24    22    67
#> 7  2014  a 083300     6      4       22     8    40
#> 8  2014  a 083400    NA     NA       38    15    65
#> 9  2014  a  Total    91     52      283   189   615
#> 10 2015  a 011100    40     11       55    36   142
#> 11 2015  a 043200    NA     NA       29    18    55
#> 12 2015  a 051300     5      8       35    25    73
#> 13 2015  a 051400    NA     NA       17    13    45
#> 14 2015  a 061900     9     14       63    52   138
#> 15 2015  a 062000    12      9       24    22    67
#> 16 2015  a 083300     6      4       22     8    40
#> 17 2015  a 083400    NA     NA       38    15    65
#> 18 2015  a  Total   101     52      283   189   625

# Or use NULL when all values in data
KostraApply(zzz, by = list(aar = NULL, ab = "a"), Fun = ProtectKostra, idVar = "region", freqVar = 4:7)
#> .....
#> GaussSuppression_anySum: ..........................................
#> .....
#> GaussSuppression_anySum: ..........................................
#>     aar ab region annet arbeid soshjelp trygd Total
#> 1  2014  a 011100    30     11       55    36   132
#> 2  2014  a 043200    NA     NA       29    18    55
#> 3  2014  a 051300     5      8       35    25    73
#> 4  2014  a 051400    NA     NA       17    13    45
#> 5  2014  a 061900     9     14       63    52   138
#> 6  2014  a 062000    12      9       24    22    67
#> 7  2014  a 083300     6      4       22     8    40
#> 8  2014  a 083400    NA     NA       38    15    65
#> 9  2014  a  Total    91     52      283   189   615
#> 10 2015  a 011100    40     11       55    36   142
#> 11 2015  a 043200    NA     NA       29    18    55
#> 12 2015  a 051300     5      8       35    25    73
#> 13 2015  a 051400    NA     NA       17    13    45
#> 14 2015  a 061900     9     14       63    52   138
#> 15 2015  a 062000    12      9       24    22    67
#> 16 2015  a 083300     6      4       22     8    40
#> 17 2015  a 083400    NA     NA       38    15    65
#> 18 2015  a  Total   101     52      283   189   625

# Selected subsets by using data frame input to specify variable combinations
KostraApply(zzz, by = data.frame(aar = c(2014, 2015), ab = c("b", "a")), Fun = ProtectKostra, idVar = "region", freqVar = 4:7)
#> .....
#> GaussSuppression_anySum: ..........................................
#> .....
#> GaussSuppression_anySum: ..........................................
#>     aar ab region annet arbeid soshjelp trygd Total
#> 1  2014  b 011100    10     11       55    36   112
#> 2  2014  b 043200    NA     NA       29    18    55
#> 3  2014  b 051300     5      8       35    25    73
#> 4  2014  b 051400    NA     NA       17    13    45
#> 5  2014  b 061900     9     14       63    52   138
#> 6  2014  b 062000    12      9       24    22    67
#> 7  2014  b 083300     6      4       22     8    40
#> 8  2014  b 083400    NA     NA       38    15    65
#> 9  2014  b  Total    71     52      283   189   595
#> 10 2015  a 011100    40     11       55    36   142
#> 11 2015  a 043200    NA     NA       29    18    55
#> 12 2015  a 051300     5      8       35    25    73
#> 13 2015  a 051400    NA     NA       17    13    45
#> 14 2015  a 061900     9     14       63    52   138
#> 15 2015  a 062000    12      9       24    22    67
#> 16 2015  a 083300     6      4       22     8    40
#> 17 2015  a 083400    NA     NA       38    15    65
#> 18 2015  a  Total   101     52      283   189   625
```
