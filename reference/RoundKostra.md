# OUTDATED FUNCTION: Rounding à la Heldal following the standards in the Kostra project.

This function is outdated, has limited functionality, and uses a
specialized interface influenced by both the former KOSTRA/KOMPIS
standards and the design and capabilities of the R package sdcTable.
Instead, functions from the R package SmallCountRounding are
recommended.

## Usage

``` r
RoundKostra(
  data,
  idVar,
  strataVar = NULL,
  freqVar,
  freqVarGroup = NULL,
  roundBase = 3,
  method = "pls",
  formula = NULL,
  level = 0,
  allSmall = TRUE,
  singleTotal = TRUE,
  makeSums = TRUE,
  output = "rounded",
  total = "Total",
  split = "_",
  extraOutput = FALSE,
  seed = 12345,
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

- roundBase:

  Basis for rounding

- method:

  Algorithm for the rounding calculations. Currently "pls" or
  "singleRandom".

- formula:

  Model formula as a string defining cells to be published (additional
  to automation)

- level:

  Interaction level or 0 (all levels) defining complexity of model
  component created from strata

- allSmall:

  When TRUE all small values will be rounded (when a single freqVar)

- singleTotal:

  When TRUE identical rowsums in all freqVarGroups needed. When FALSE
  totals for each freqVarGroup will be in output.

- makeSums:

  When TRUE totals vil be made similar to ProtectKostra (in fact this is
  done by calling ProtectKostra)

- output:

  One of "rounded" (default), "original", "difference" or "status".
  Unrounded results are returned by “original” (same as roundBase=0) and
  `"difference" = "rounded" - “original”`. With output=”status” zero
  differences are set to “o” (original) and others are coded as “r”
  (rounded)

- total:

  String used to name totals.

- split:

  Parameter to `AutoSplit` - see varNames and rowData above. When NULL
  automatic splitting without needing a split string.

- extraOutput:

  When TRUE output is a list of several elements (makeSums ignored)

- seed:

  NULL or seed for random number generator (`set.seed(seed)` will be run
  at the beginning of the function)

- ...:

  Variables for formula and additional variables that will be included
  in output (name or number).

## Value

A data.frame unless extraOutput = TRUE

## Details

**A single freq variable and formula:** A formula defines all the
publishable cells. Rounding is performed so that all the publishable
cells are safe. When allSmall=TRUE all small cells are rounded. All
possible totals are then safe, but totals not defined by the formula can
be far from the original values.

**A single freq variable and strata:** Instead of a formula it is
assumed that the cells to be published are obtained by crossing all
strata variables. The parameter "level" may be used.

**Several freq variables without freqVarGroup:** The data is in a
unstacked form and stack/unstack will be performed in the background
similar to ProtectKostra. The original id-var will be considered as a
strata-var when stacked and rounding is performed similar to "A single
freq variable and strata".

**With freqVarGroup without single-groups:** Each group can be stacked
to form a separate data set, but a common data set is needed. An ad hoc
data set is created to match all the single data sets and this data set
will be used in the rounding process. This method will not work in
complicated cases. Use with care. Try extraOutput=TRUE to see what is
going on.

**With freqVarGroup with single-groups:** The single-groups are assumed
to be two-category groups (yes and no, but only yes is reported). The
remaining category will be computed in order to create ad hoc data set.

## Note

Even if freq-variables with `freqVarGroup<1` is not used they will be
read by
[`GetData`](https://statisticsnorway.github.io/ssb-kostra/reference/GetData.md)
together with the other the freq-variables variables into a matrix. Use
a common numeric type for all these variables to prevent change of data
type.

Parameter `namesAsInput` in ProtectTable() is not yet available in
RoundKostra and therefore very advanced variable name coding will give
insufficient results. Example:

`RoundKostra(KostraData("z3wb") ,idVar="region",strataVar=c("fylke","kostragr"),freqVar=4:15)`

But this will work:

`RoundKostra(KostraData("z3wb") ,idVar="region",strataVar=c("fylke","kostragr"),freqVar=4:15,split=NULL)`

See similar example in
[`ProtectTable`](https://statisticsnorway.github.io/ssb-kostra/reference/ProtectTable.md).

**NOTE:** Be sure to spell the input parameters correctly. Because of
the "..."-input misspelled parameters may give strange results instead
of error.

## See also

[`ProtectKostra`](https://statisticsnorway.github.io/ssb-kostra/reference/ProtectKostra.md),
`RoundViaDummy`, `makeroundtabs`, `Round2`, `FormulaSums`, `ModelMatrix`

## Examples

``` r

 # ========================================================
 #    Examples:  A single freq variable
 # =======================================================

 z2w <- KostraData("z2w")

 # ==== Without strataVar and  without formula ====
 RoundKostra(z2w ,idVar="region", freqVar="arbeid", roundBase=5)
#> [-**..:=]
#>    region arbeid
#> 1  011100     11
#> 2  043200      0
#> 3  051300      8
#> 4  051400      0
#> 5  061900     14
#> 6  062000      9
#> 7  083300      5
#> 8  083400      5
#> 9  012100      0
#> 10 102100      0
#> 11 102700      0
#> 12  Total     52
 RoundKostra(z2w ,idVar="region", freqVar="arbeid", makeSums=FALSE) # Without total
#> [-**..:=]
#>    region arbeid
#> 1  011100     11
#> 2  043200      0
#> 3  051300      8
#> 4  051400      3
#> 5  061900     14
#> 6  062000      9
#> 7  083300      4
#> 8  083400      3
#> 9  012100      0
#> 10 102100      0
#> 11 102700      3

 # ==== With strataVar and  without formula ====
 RoundKostra(z2w ,idVar="region", freqVar="arbeid", strataVar=c("fylke","kostragr"), roundBase=5)
#> [-**..:=]
#>    region arbeid
#> 1  011100     11
#> 2  043200      0
#> 3  051300      8
#> 4  051400      0
#> 5  061900     14
#> 6  062000      9
#> 7  083300      5
#> 8  083400      5
#> 9  012100      0
#> 10 102100      0
#> 11 102700      0
#> 12   0100     11
#> 13   0400      0
#> 14   0500      8
#> 15   0600     23
#> 16   0800     10
#> 17   1000      0
#> 18     03     52
#> 19     04      0
#> 20  Total     52
 RoundKostra(z2w ,idVar="region", freqVar="arbeid", strataVar=c("fylke","kostragr"), makeSums=FALSE)
#> [-**..:=]
#>    region fylke kostragr arbeid
#> 1  011100  0100       03     11
#> 2  043200  0400       03      0
#> 3  051300  0500       03      8
#> 4  051400  0500       03      3
#> 5  061900  0600       03     14
#> 6  062000  0600       03      9
#> 7  083300  0800       03      4
#> 8  083400  0800       03      3
#> 9  012100  0100       04      0
#> 10 102100  1000       04      0
#> 11 102700  1000       04      3
 RoundKostra(z2w ,idVar="region", freqVar="arbeid", strataVar= "fylke", allSmall=FALSE) # Warning when makeSums=TRUE
#> Warning: Since allSmall=FALSE all output is not safe
#> [-**.:=]
#>    region arbeid
#> 1  011100     11
#> 2  043200      0
#> 3  051300      8
#> 4  051400      2
#> 5  061900     14
#> 6  062000      9
#> 7  083300      4
#> 8  083400      3
#> 9  012100      0
#> 10 102100      0
#> 11 102700      3
#> 12   0100     11
#> 13   0400      0
#> 14   0500     10
#> 15   0600     23
#> 16   0800      7
#> 17   1000      3
#> 18  Total     54
 RoundKostra(z2w ,idVar="region", freqVar="arbeid", strataVar= "fylke", allSmall=FALSE, makeSums=FALSE)
#> [-**.:=]
#>    region fylke arbeid
#> 1  011100  0100     11
#> 2  043200  0400      0
#> 3  051300  0500      8
#> 4  051400  0500      2
#> 5  061900  0600     14
#> 6  062000  0600      9
#> 7  083300  0800      4
#> 8  083400  0800      3
#> 9  012100  0100      0
#> 10 102100  1000      0
#> 11 102700  1000      3
 RoundKostra(z2w ,idVar="region", freqVar="arbeid", strataVar=c("fylke","kostragr"), extraOutput=TRUE)$formula
#> [-**..:=]
#> [1] "~ region + fylke + kostragr"

 # ==== Without strataVar and  with formula ( makeSums ignored without warning)  ====
 RoundKostra(z2w ,idVar="region", freqVar="arbeid", formula="fylke", fylke="fylke")
#> [-**..:=]
#>    region arbeid fylke
#> 1  011100     11  0100
#> 2  043200      0  0400
#> 3  051300      8  0500
#> 4  051400      3  0500
#> 5  061900     14  0600
#> 6  062000      9  0600
#> 7  083300      4  0800
#> 8  083400      3  0800
#> 9  012100      0  0100
#> 10 102100      0  1000
#> 11 102700      3  1000
 RoundKostra(z2w ,idVar="region", freqVar="arbeid", formula="fylke", fylke="fylke",allSmall = FALSE)
#> [-**.:=]
#>    region arbeid fylke
#> 1  011100     11  0100
#> 2  043200      0  0400
#> 3  051300      8  0500
#> 4  051400      2  0500
#> 5  061900     14  0600
#> 6  062000      9  0600
#> 7  083300      4  0800
#> 8  083400      3  0800
#> 9  012100      0  0100
#> 10 102100      0  1000
#> 11 102700      3  1000
 RoundKostra(z2w ,idVar="region", freqVar="arbeid", formula="A+B", A="fylke",B="kostragr")
#> [-**..:=]
#>    region arbeid    A  B
#> 1  011100     11 0100 03
#> 2  043200      0 0400 03
#> 3  051300      8 0500 03
#> 4  051400      3 0500 03
#> 5  061900     14 0600 03
#> 6  062000      9 0600 03
#> 7  083300      4 0800 03
#> 8  083400      3 0800 03
#> 9  012100      0 0100 04
#> 10 102100      0 1000 04
#> 11 102700      3 1000 04
 RoundKostra(z2w ,idVar="region", freqVar="arbeid", formula="A*B+C", A="fylke",B="kostragr",C="annet")
#> [-**..:=]
#>    region arbeid    A  B  C
#> 1  011100     11 0100 03 11
#> 2  043200      0 0400 03  7
#> 3  051300      8 0500 03  5
#> 4  051400      3 0500 03 13
#> 5  061900     14 0600 03  9
#> 6  062000      9 0600 03 12
#> 7  083300      4 0800 03  6
#> 8  083400      3 0800 03  9
#> 9  012100      0 0100 04  3
#> 10 102100      0 1000 04  9
#> 11 102700      3 1000 04  4

 # =============================================================================
 #   Examples:  Several freq variables without freqVarGroup (allSmall ignored)
 # =============================================================================
 RoundKostra(z2w ,idVar="region",strataVar=c("fylke","kostragr"),freqVar=4:7)
#> [-**..:=]
#>    region annet arbeid soshjelp trygd Total
#> 1  011100    11     11       55    36   113
#> 2  043200     7      0       29    18    54
#> 3  051300     5      8       35    25    73
#> 4  051400    13      3       17    13    46
#> 5  061900     9     14       63    52   138
#> 6  062000    12      9       24    22    67
#> 7  083300     6      4       22     8    40
#> 8  083400     9      3       38    15    65
#> 9  012100     3      0        9     0    12
#> 10 102100     9      0       32    20    61
#> 11 102700     4      3       18    11    36
#> 12   0100    14     11       64    36   125
#> 13   0400     7      0       29    18    54
#> 14   0500    18     11       52    38   119
#> 15   0600    21     23       87    74   205
#> 16   0800    15      7       60    23   105
#> 17   1000    13      3       50    31    97
#> 18     03    72     52      283   189   596
#> 19     04    16      3       59    31   109
#> 20  Total    88     55      342   220   705
 RoundKostra(z2w ,idVar="region",strataVar=c("fylke","kostragr"),freqVar=4:7,makeSums=FALSE)
#> [-**..:=]
#>    region fylke kostragr annet arbeid soshjelp trygd
#> 1  011100  0100       03    11     11       55    36
#> 2  043200  0400       03     7      0       29    18
#> 3  051300  0500       03     5      8       35    25
#> 4  051400  0500       03    13      3       17    13
#> 5  061900  0600       03     9     14       63    52
#> 6  062000  0600       03    12      9       24    22
#> 7  083300  0800       03     6      4       22     8
#> 8  083400  0800       03     9      3       38    15
#> 9  012100  0100       04     3      0        9     0
#> 10 102100  1000       04     9      0       32    20
#> 11 102700  1000       04     4      3       18    11
 RoundKostra(z2w ,idVar="region",strataVar=c("fylke","kostragr"),freqVar=4:7,extraOutput=TRUE)$input
#> [-**..:=]
#>    region fylke kostragr annet_arbeid_soshjelp_trygd f_Re_Q
#> 1  011100  0100       03                       annet     11
#> 2  043200  0400       03                       annet      7
#> 3  051300  0500       03                       annet      5
#> 4  051400  0500       03                       annet     13
#> 5  061900  0600       03                       annet      9
#> 6  062000  0600       03                       annet     12
#> 7  083300  0800       03                       annet      6
#> 8  083400  0800       03                       annet      9
#> 9  012100  0100       04                       annet      3
#> 10 102100  1000       04                       annet      9
#> 11 102700  1000       04                       annet      4
#> 12 011100  0100       03                      arbeid     11
#> 13 043200  0400       03                      arbeid      1
#> 14 051300  0500       03                      arbeid      8
#> 15 051400  0500       03                      arbeid      2
#> 16 061900  0600       03                      arbeid     14
#> 17 062000  0600       03                      arbeid      9
#> 18 083300  0800       03                      arbeid      4
#> 19 083400  0800       03                      arbeid      3
#> 20 012100  0100       04                      arbeid      0
#> 21 102100  1000       04                      arbeid      0
#> 22 102700  1000       04                      arbeid      2
#> 23 011100  0100       03                    soshjelp     55
#> 24 043200  0400       03                    soshjelp     29
#> 25 051300  0500       03                    soshjelp     35
#> 26 051400  0500       03                    soshjelp     17
#> 27 061900  0600       03                    soshjelp     63
#> 28 062000  0600       03                    soshjelp     24
#> 29 083300  0800       03                    soshjelp     22
#> 30 083400  0800       03                    soshjelp     38
#> 31 012100  0100       04                    soshjelp      9
#> 32 102100  1000       04                    soshjelp     32
#> 33 102700  1000       04                    soshjelp     18
#> 34 011100  0100       03                       trygd     36
#> 35 043200  0400       03                       trygd     18
#> 36 051300  0500       03                       trygd     25
#> 37 051400  0500       03                       trygd     13
#> 38 061900  0600       03                       trygd     52
#> 39 062000  0600       03                       trygd     22
#> 40 083300  0800       03                       trygd      8
#> 41 083400  0800       03                       trygd     15
#> 42 012100  0100       04                       trygd      2
#> 43 102100  1000       04                       trygd     20
#> 44 102700  1000       04                       trygd     11
 RoundKostra(z2w ,idVar="region",strataVar=c("fylke","kostragr"),freqVar=4:7,extraOutput=TRUE)$formula
#> [-**..:=]
#> ~region * annet_arbeid_soshjelp_trygd + fylke * annet_arbeid_soshjelp_trygd + 
#>     kostragr * annet_arbeid_soshjelp_trygd
#> <environment: 0x55bbf834b1b8>

 # ===============================================================
 #   Examples: With freqVarGroup
 # ==========================================================================

 # ==========  With no single-groups  ================
 ex1 = Kostra:::exData1()   #  hack endre seinere
 freqVarGroup <- c(1,1,1,1,1,1,1,1,2,2,2,2)
 RoundKostra(ex1, idVar="region",strataVar=c("fylke","kostragr"),freqVar=4:15,freqVarGroup=freqVarGroup)
#> [-**.........=-**.:=(Check:28)-**..............:=]
#>    region annet_A arb_A soshj_A trygd_A annet_B arb_B soshj_B trygd_B s1 s2  s3
#> 1  111100      12     4      32      25       0     0       3       0  4  0   3
#> 2  111200       0     7      20      15       3     3       7       3  3  0   6
#> 3  111400       0     6      11       8       3     0       0       0  3  3   0
#> 4  113300       3    11      16      16       0     3       4       0  0  0   3
#> 5  113500       8    14      33      42       0     3       5       3  3  8  11
#> 6  114100       4     3       8      19       3     0       3       0  3  0   3
#> 7  114200       6     8      39      27       3     0       9       5  3  3   8
#> 8  114400       0     0       0       0       0     0       3       0  0  0   0
#> 9  114500       0     0       6       3       0     0       0       3  0  0   0
#> 10 115100       3     0       0       0       0     0       0       0  0  0   0
#> 11 121100       5     3      36      33       0     0       3       3  3  0   6
#> 12 122200       6     4      25      22       3     0       3       0  0  3   7
#> 13 122300       6     6      12      15       0     0       5       6  0  3   6
#> 14 123100       6     5      22      11       0     0       4       3  4  0   3
#> 15 123400       0     0      17       8       3     0       3       3  0  0   3
#> 16 124100       3     0      15       9       0     0       0       3  0  3   3
#> 17 124200       3     0      22      13       3     3       0       0  3  0   0
#> 18 124400       7     8       7      15       0     0       7       0  0  0   4
#> 19 125100       9     7      34      28       0     3       6       3  3  6   7
#> 20 125900      23    14      79      48       0     3      14       6  3  7   6
#> 21 126400       0     7      27      19       0     3       4       5  0  3   0
#> 22 126500       3     3       4       0       0     0       0       0  0  0   3
#> 23 141100       3     0       5       5       0     0       3       0  0  0   0
#> 24 141200       3     3       0       0       0     0       0       0  0  0   3
#> 25 141300       0     3       3       3       0     0       3       0  0  0   3
#> 26 141600      10     9      40      42       3     0      11       3  3  0   9
#> 27 141700       3     3      20      12       0     3       0       0  0  0   3
#> 28 141800       4     0       6       7       0     0       3       0  0  3   0
#> 29 141900       3     6      18      13       0     0       3       3  0  3   6
#> 30 142800       5     3      37      23       0     3       7       6  3  3   3
#> 31 142900       3     6      18      14       0     3       3       0  0  0   3
#> 32 143000       0     3      11       8       3     0       3       0  0  0   3
#> 33 143100       4     3      25      17       0     6       3       3  3  3   0
#> 34 143300       7    10      22      12       0     0       3       0  3  0   4
#> 35 144100       0     6      12      11       0     0       0       3  0  0   3
#> 36 144400       0     0       3       0       0     0       0       0  0  0   0
#> 37   1100      36    53     165     155      12     9      34      14 19 14  34
#> 38   1200      71    57     300     221       9    12      49      32 16 25  48
#> 39   1400      45    55     220     167       6    15      42      18 12 12  40
#> 40     01      22    16     117      92       0     3      13      12 10  3  15
#> 41     02      42    70     248     187      15    15      47      31 22 21  45
#> 42     03      72    70     276     238       9    18      50      15 15 24  50
#> 43     04       3     3       6       3       0     0       6       0  0  0   3
#> 44     05      13     6      38      23       3     0       9       6  0  3   9
#> 45  Total     152   165     685     543      27    36     125      64 47 51 122
#>      s4 annet arb soshj trygd    A   B Total
#> 1    69    12   4    35    25   73   3    76
#> 2    49     3  10    27    18   42  16    58
#> 3    22     3   6    11     8   25   3    28
#> 4    50     3  14    20    16   46   7    53
#> 5    86     8  17    38    45   97  11   108
#> 6    34     7   3    11    19   34   6    40
#> 7    83     9   8    48    32   80  17    97
#> 8     3     0   0     3     0    0   3     3
#> 9    12     0   0     6     6    9   3    12
#> 10    3     3   0     0     0    3   0     3
#> 11   74     5   3    39    36   77   6    83
#> 12   53     9   4    28    22   57   6    63
#> 13   41     6   6    17    21   39  11    50
#> 14   44     6   5    26    14   44   7    51
#> 15   31     3   0    20    11   25   9    34
#> 16   24     3   0    15    12   27   3    30
#> 17   41     6   3    22    13   38   6    44
#> 18   40     7   8    14    15   37   7    44
#> 19   74     9  10    40    31   78  12    90
#> 20  171    23  17    93    54  164  23   187
#> 21   62     0  10    31    24   53  12    65
#> 22    7     3   3     4     0   10   0    10
#> 23   16     3   0     8     5   13   3    16
#> 24    3     3   3     0     0    6   0     6
#> 25    9     0   3     6     3    9   3    12
#> 26  106    13   9    51    45  101  17   118
#> 27   38     3   6    20    12   38   3    41
#> 28   17     4   0     9     7   17   3    20
#> 29   37     3   6    21    16   40   6    46
#> 30   75     5   6    44    29   68  16    84
#> 31   44     3   9    21    14   41   6    47
#> 32   25     3   3    14     8   22   6    28
#> 33   55     4   9    28    20   49  12    61
#> 34   47     7  10    25    12   51   3    54
#> 35   29     0   6    12    14   29   3    32
#> 36    3     0   0     3     0    3   0     3
#> 37  411    48  62   199   169  409  69   478
#> 38  662    80  69   349   253  649 102   751
#> 39  504    51  70   262   185  487  81   568
#> 40  247    22  19   130   104  247  28   275
#> 41  567    57  85   295   218  547 108   655
#> 42  659    81  88   326   253  656  92   748
#> 43   18     3   3    12     3   15   6    21
#> 44   86    16   6    47    29   80  18    98
#> 45 1577   179 201   810   607 1545 252  1797
 RoundKostra(ex1, idVar="region",strataVar=c("fylke","kostragr"),freqVar=4:15,freqVarGroup=freqVarGroup, makeSums=FALSE)
#> [-**.........=-**.:=(Check:28)-**..............:=]
#>    region fylke kostragr annet_A arb_A soshj_A trygd_A annet_B arb_B soshj_B
#> 1  111100  1100       01      12     4      32      25       0     0       3
#> 2  111200  1100       02       0     7      20      15       3     3       7
#> 3  111400  1100       02       0     6      11       8       3     0       0
#> 4  113300  1100       03       3    11      16      16       0     3       4
#> 5  113500  1100       03       8    14      33      42       0     3       5
#> 6  114100  1100       02       4     3       8      19       3     0       3
#> 7  114200  1100       02       6     8      39      27       3     0       9
#> 8  114400  1100       04       0     0       0       0       0     0       3
#> 9  114500  1100       05       0     0       6       3       0     0       0
#> 10 115100  1100       04       3     0       0       0       0     0       0
#> 11 121100  1200       01       5     3      36      33       0     0       3
#> 12 122200  1200       03       6     4      25      22       3     0       3
#> 13 122300  1200       02       6     6      12      15       0     0       5
#> 14 123100  1200       02       6     5      22      11       0     0       4
#> 15 123400  1200       05       0     0      17       8       3     0       3
#> 16 124100  1200       02       3     0      15       9       0     0       0
#> 17 124200  1200       03       3     0      22      13       3     3       0
#> 18 124400  1200       03       7     8       7      15       0     0       7
#> 19 125100  1200       03       9     7      34      28       0     3       6
#> 20 125900  1200       03      23    14      79      48       0     3      14
#> 21 126400  1200       02       0     7      27      19       0     3       4
#> 22 126500  1200       05       3     3       4       0       0     0       0
#> 23 141100  1400       05       3     0       5       5       0     0       3
#> 24 141200  1400       05       3     3       0       0       0     0       0
#> 25 141300  1400       04       0     3       3       3       0     0       3
#> 26 141600  1400       03      10     9      40      42       3     0      11
#> 27 141700  1400       03       3     3      20      12       0     3       0
#> 28 141800  1400       05       4     0       6       7       0     0       3
#> 29 141900  1400       02       3     6      18      13       0     0       3
#> 30 142800  1400       01       5     3      37      23       0     3       7
#> 31 142900  1400       02       3     6      18      14       0     3       3
#> 32 143000  1400       02       0     3      11       8       3     0       3
#> 33 143100  1400       02       4     3      25      17       0     6       3
#> 34 143300  1400       02       7    10      22      12       0     0       3
#> 35 144100  1400       01       0     6      12      11       0     0       0
#> 36 144400  1400       04       0     0       3       0       0     0       0
#>    trygd_B s1 s2 s3  s4
#> 1        0  4  0  3  69
#> 2        3  3  0  6  49
#> 3        0  3  3  0  22
#> 4        0  0  0  3  50
#> 5        3  3  8 11  86
#> 6        0  3  0  3  34
#> 7        5  3  3  8  83
#> 8        0  0  0  0   3
#> 9        3  0  0  0  12
#> 10       0  0  0  0   3
#> 11       3  3  0  6  74
#> 12       0  0  3  7  53
#> 13       6  0  3  6  41
#> 14       3  4  0  3  44
#> 15       3  0  0  3  31
#> 16       3  0  3  3  24
#> 17       0  3  0  0  41
#> 18       0  0  0  4  40
#> 19       3  3  6  7  74
#> 20       6  3  7  6 171
#> 21       5  0  3  0  62
#> 22       0  0  0  3   7
#> 23       0  0  0  0  16
#> 24       0  0  0  3   3
#> 25       0  0  0  3   9
#> 26       3  3  0  9 106
#> 27       0  0  0  3  38
#> 28       0  0  3  0  17
#> 29       3  0  3  6  37
#> 30       6  3  3  3  75
#> 31       0  0  0  3  44
#> 32       0  0  0  3  25
#> 33       3  3  3  0  55
#> 34       0  3  0  4  47
#> 35       3  0  0  3  29
#> 36       0  0  0  0   3
 a1 <- RoundKostra(ex1, idVar="region",strataVar=c("fylke","kostragr"),freqVar=4:15,freqVarGroup=freqVarGroup, extraOutput=TRUE)
#> [-**.........=-**.:=(Check:28)-**..............:=]
 head(a1$input) # ad hoc created data
#>   region fylke kostragr annet_arb_soshj_trygd A_B s1_s2_s3_s4 f_Re_Q
#> 1 111100  1100       01                 annet   A          s4     12
#> 2 111100  1100       01                   arb   A          s1      4
#> 3 111100  1100       01                   arb   B          s3      1
#> 4 111100  1100       01                 soshj   A          s4     32
#> 5 111100  1100       01                 soshj   B          s3      2
#> 6 111100  1100       01                 trygd   A          s4     25
 a1$formula     # The formula used
#> ~region + annet_arb_soshj_trygd + A_B + fylke + kostragr + s1_s2_s3_s4 + 
#>     region:annet_arb_soshj_trygd + region:A_B + annet_arb_soshj_trygd:A_B + 
#>     annet_arb_soshj_trygd:fylke + A_B:fylke + annet_arb_soshj_trygd:kostragr + 
#>     A_B:kostragr + region:s1_s2_s3_s4 + fylke:s1_s2_s3_s4 + kostragr:s1_s2_s3_s4 + 
#>     region:annet_arb_soshj_trygd:A_B + annet_arb_soshj_trygd:A_B:fylke + 
#>     annet_arb_soshj_trygd:A_B:kostragr
#> <environment: 0x55bbe52f5e20>


 # ==========  With some single-groups  ================
 freqVarGroup <- c(1,1,1,1,1,1,1,1,2,2,2,2,3,4,-1,5)
 RoundKostra(ex1, idVar="region",strataVar=c("fylke","kostragr"),freqVar=4:19,freqVarGroup=freqVarGroup)
#> [-**.........=-**....:=-**..:=(Check:42)-**..........:=]
#>    region annet_A arb_A soshj_A trygd_A annet_B arb_B soshj_B trygd_B s1 s2  s3
#> 1  111100      12     4      32      25       0     0       3       0  4  0   3
#> 2  111200       0     7      20      15       3     3       7       3  3  0   6
#> 3  111400       0     6      11       9       3     0       0       0  3  3   0
#> 4  113300       0    11      16      16       0     3       4       0  0  0   0
#> 5  113500       8    11      33      42       0     3       8       3  6  5  11
#> 6  114100       4     3       8      19       3     0       3       0  3  0   3
#> 7  114200       6     8      39      27       3     0       9       5  3  3   8
#> 8  114400       0     0       0       0       0     0       3       0  0  0   0
#> 9  114500       3     0       6       3       0     0       0       3  3  0   0
#> 10 115100       3     0       0       0       0     0       0       0  0  0   0
#> 11 121100       5     3      36      33       0     0       3       3  0  0   9
#> 12 122200       6     4      25      22       0     3       3       0  0  3   7
#> 13 122300       6     3      12      15       0     3       4       3  0  3   3
#> 14 123100       6     5      22      11       0     0       3       3  3  0   3
#> 15 123400       0     3      17       8       0     0       3       3  3  0   3
#> 16 124100       3     0      15       9       0     0       3       0  0  3   3
#> 17 124200       3     0      22      13       3     0       0       3  3  0   0
#> 18 124400       6     8       7      15       0     0       7       0  0  0   4
#> 19 125100       9     7      34      28       0     3       3       3  3  3   7
#> 20 125900      23    16      79      48       0     3      14       9  3 11   6
#> 21 126400       3     7      27      19       3     0       6       5  0  3   3
#> 22 126500       3     0       4       0       0     0       0       0  0  0   0
#> 23 141100       3     0       5       5       0     0       3       0  0  0   0
#> 24 141200       0     3       0       0       0     0       3       0  0  0   0
#> 25 141300       0     3       3       3       0     0       3       0  0  0   0
#> 26 141600      10     9      40      42       3     0      11       3  0  0  12
#> 27 141700       3     3      20      10       3     3       0       0  0  3   3
#> 28 141800       4     3       6       7       0     0       3       0  0  0   3
#> 29 141900       3     6      18      12       0     0       3       3  0  0   9
#> 30 142800       5     6      37      23       0     3       7       0  3  3   0
#> 31 142900       3     6      18      14       0     3       0       0  0  0   0
#> 32 143000       3     0      11       9       0     3       3       0  0  0   3
#> 33 143100       4     3      25      17       0     6       3       3  3  3   0
#> 34 143300       4    10      22      14       0     0       3       3  3  0   7
#> 35 144100       3     6      12       9       0     0       0       3  3  0   3
#> 36 144400       0     0       3       0       0     0       0       0  0  0   3
#> 37   1100      36    50     165     156      12     9      37      14 25 11  31
#> 38   1200      73    56     300     221       6    12      49      32 15 26  48
#> 39   1400      45    58     220     165       6    18      42      15 12  9  43
#> 40     01      25    19     117      90       0     3      13       6 10  3  15
#> 41     02      45    64     248     190      15    18      47      28 21 18  48
#> 42     03      68    69     276     236       9    18      50      21 15 25  50
#> 43     04       3     3       6       3       0     0       6       0  0  0   3
#> 44     05      13     9      38      23       0     0      12       6  6  0   6
#> 45  Total     154   164     685     542      24    39     128      61 52 46 122
#>      s4  W   X  Y   Z nOOtW nOOtX nOOtZ annet arb soshj trygd    A   B Total
#> 1    69  3  12 NA  32    73    64    44    12   4    35    25   73   3    76
#> 2    49  0   0 NA  19    58    58    39     3  10    27    18   42  16    58
#> 3    23  0   0 NA  12    29    29    17     3   6    11     9   26   3    29
#> 4    50  3   0 NA  14    47    50    36     0  14    20    16   43   7    50
#> 5    86  6   9 NA  33   102    99    75     8  14    41    45   94  14   108
#> 6    34  6   6 NA   9    34    34    31     7   3    11    19   34   6    40
#> 7    83  9   3 NA  39    88    94    58     9   8    48    32   80  17    97
#> 8     3  0   0 NA   0     3     3     3     0   0     3     0    0   3     3
#> 9    12  3   3 NA   9    12    12     6     3   0     6     6   12   3    15
#> 10    3  0   3 NA   0     3     0     3     3   0     0     0    3   0     3
#> 11   74  3   3 NA  34    80    80    49     5   3    39    36   77   6    83
#> 12   53  3   6 NA  24    60    57    39     6   7    28    22   57   6    63
#> 13   40  3   6 NA  13    43    40    33     6   6    16    18   36  10    46
#> 14   44  3   6 NA  20    47    44    30     6   5    25    14   44   6    50
#> 15   28  0   0 NA  17    34    34    17     0   3    20    11   28   6    34
#> 16   24  3   3 NA  15    27    27    15     3   0    18     9   27   3    30
#> 17   41  0   3 NA  22    44    41    22     6   0    22    16   38   6    44
#> 18   39  0   7 NA   7    43    36    36     6   8    14    15   36   7    43
#> 19   74  6   9 NA  34    81    78    53     9  10    37    31   78   9    87
#> 20  172 15  25 NA  82   177   167   110    23  19    93    57  166  26   192
#> 21   64  9   3 NA  30    61    67    40     6   7    33    24   56  14    70
#> 22    7  0   0 NA   3     7     7     4     3   0     4     0    7   0     7
#> 23   16  0   3 NA   6    16    13    10     3   0     8     5   13   3    16
#> 24    6  0   0 NA   0     6     6     6     0   3     3     0    3   3     6
#> 25   12  0   0 NA   3    12    12     9     0   3     6     3    9   3    12
#> 26  106  3   9 NA  39   115   109    79    13   9    51    45  101  17   118
#> 27   36  6   3 NA  22    36    39    20     6   6    20    10   36   6    42
#> 28   20  0   3 NA   6    23    20    17     4   3     9     7   20   3    23
#> 29   36  0   0 NA  15    45    45    30     3   6    21    15   39   6    45
#> 30   75  6   6 NA  37    75    75    44     5   9    44    23   71  10    81
#> 31   44  0   3 NA  17    44    41    27     3   9    18    14   41   3    44
#> 32   26  0   3 NA  12    29    26    17     3   3    14     9   23   6    29
#> 33   55  3   3 NA  25    58    58    36     4   9    28    20   49  12    61
#> 34   46  3   6 NA  23    53    50    33     4  10    25    17   50   6    56
#> 35   27  0   3 NA  12    33    30    21     3   6    12    12   30   3    33
#> 36    0  0   0 NA   3     3     3     0     0   0     3     0    3   0     3
#> 37  412 30  36 NA 167   449   443   312    48  59   202   170  407  72   479
#> 38  660 45  71 NA 301   704   678   448    79  68   349   253  650  99   749
#> 39  505 21  42 NA 220   548   527   349    51  76   262   180  488  81   569
#> 40  245 12  24 NA 115   261   249   158    25  22   130    96  251  22   273
#> 41  568 39  42 NA 249   616   613   406    60  82   295   218  547 108   655
#> 42  657 42  71 NA 277   705   676   470    77  87   326   257  649  98   747
#> 43   18  0   3 NA   6    21    18    15     3   3    12     3   15   6    21
#> 44   89  3   9 NA  41    98    92    60    13   9    50    29   83  18   101
#> 45 1577 96 149 NA 688  1701  1648  1109   178 203   813   603 1545 252  1797
 RoundKostra(ex1, idVar="region",strataVar=c("fylke","kostragr"),freqVar=4:19,freqVarGroup=freqVarGroup, singleTotal = FALSE)
#> [-**.........=-**....:=-**..:=(Check:42)-**..........:=]
#>    region annet_A arb_A soshj_A trygd_A annet_B arb_B soshj_B trygd_B s1 s2  s3
#> 1  111100      12     4      32      25       0     0       3       0  4  0   3
#> 2  111200       0     7      20      15       3     3       7       3  3  0   6
#> 3  111400       0     6      11       9       3     0       0       0  3  3   0
#> 4  113300       0    11      16      16       0     3       4       0  0  0   0
#> 5  113500       8    11      33      42       0     3       8       3  6  5  11
#> 6  114100       4     3       8      19       3     0       3       0  3  0   3
#> 7  114200       6     8      39      27       3     0       9       5  3  3   8
#> 8  114400       0     0       0       0       0     0       3       0  0  0   0
#> 9  114500       3     0       6       3       0     0       0       3  3  0   0
#> 10 115100       3     0       0       0       0     0       0       0  0  0   0
#> 11 121100       5     3      36      33       0     0       3       3  0  0   9
#> 12 122200       6     4      25      22       0     3       3       0  0  3   7
#> 13 122300       6     3      12      15       0     3       4       3  0  3   3
#> 14 123100       6     5      22      11       0     0       3       3  3  0   3
#> 15 123400       0     3      17       8       0     0       3       3  3  0   3
#> 16 124100       3     0      15       9       0     0       3       0  0  3   3
#> 17 124200       3     0      22      13       3     0       0       3  3  0   0
#> 18 124400       6     8       7      15       0     0       7       0  0  0   4
#> 19 125100       9     7      34      28       0     3       3       3  3  3   7
#> 20 125900      23    16      79      48       0     3      14       9  3 11   6
#> 21 126400       3     7      27      19       3     0       6       5  0  3   3
#> 22 126500       3     0       4       0       0     0       0       0  0  0   0
#> 23 141100       3     0       5       5       0     0       3       0  0  0   0
#> 24 141200       0     3       0       0       0     0       3       0  0  0   0
#> 25 141300       0     3       3       3       0     0       3       0  0  0   0
#> 26 141600      10     9      40      42       3     0      11       3  0  0  12
#> 27 141700       3     3      20      10       3     3       0       0  0  3   3
#> 28 141800       4     3       6       7       0     0       3       0  0  0   3
#> 29 141900       3     6      18      12       0     0       3       3  0  0   9
#> 30 142800       5     6      37      23       0     3       7       0  3  3   0
#> 31 142900       3     6      18      14       0     3       0       0  0  0   0
#> 32 143000       3     0      11       9       0     3       3       0  0  0   3
#> 33 143100       4     3      25      17       0     6       3       3  3  3   0
#> 34 143300       4    10      22      14       0     0       3       3  3  0   7
#> 35 144100       3     6      12       9       0     0       0       3  3  0   3
#> 36 144400       0     0       3       0       0     0       0       0  0  0   3
#> 37   1100      36    50     165     156      12     9      37      14 25 11  31
#> 38   1200      73    56     300     221       6    12      49      32 15 26  48
#> 39   1400      45    58     220     165       6    18      42      15 12  9  43
#> 40     01      25    19     117      90       0     3      13       6 10  3  15
#> 41     02      45    64     248     190      15    18      47      28 21 18  48
#> 42     03      68    69     276     236       9    18      50      21 15 25  50
#> 43     04       3     3       6       3       0     0       6       0  0  0   3
#> 44     05      13     9      38      23       0     0      12       6  6  0   6
#> 45  Total     154   164     685     542      24    39     128      61 52 46 122
#>      s4  W   X  Y   Z nOOtW nOOtX nOOtZ annet arb soshj trygd    A   B Total_1
#> 1    69  3  12 NA  32    73    64    44    12   4    35    25   73   3      76
#> 2    49  0   0 NA  19    58    58    39     3  10    27    18   42  16      58
#> 3    23  0   0 NA  12    29    29    17     3   6    11     9   26   3      29
#> 4    50  3   0 NA  14    47    50    36     0  14    20    16   43   7      50
#> 5    86  6   9 NA  33   102    99    75     8  14    41    45   94  14     108
#> 6    34  6   6 NA   9    34    34    31     7   3    11    19   34   6      40
#> 7    83  9   3 NA  39    88    94    58     9   8    48    32   80  17      97
#> 8     3  0   0 NA   0     3     3     3     0   0     3     0    0   3       3
#> 9    12  3   3 NA   9    12    12     6     3   0     6     6   12   3      15
#> 10    3  0   3 NA   0     3     0     3     3   0     0     0    3   0       3
#> 11   74  3   3 NA  34    80    80    49     5   3    39    36   77   6      83
#> 12   53  3   6 NA  24    60    57    39     6   7    28    22   57   6      63
#> 13   40  3   6 NA  13    43    40    33     6   6    16    18   36  10      46
#> 14   44  3   6 NA  20    47    44    30     6   5    25    14   44   6      50
#> 15   28  0   0 NA  17    34    34    17     0   3    20    11   28   6      34
#> 16   24  3   3 NA  15    27    27    15     3   0    18     9   27   3      30
#> 17   41  0   3 NA  22    44    41    22     6   0    22    16   38   6      44
#> 18   39  0   7 NA   7    43    36    36     6   8    14    15   36   7      43
#> 19   74  6   9 NA  34    81    78    53     9  10    37    31   78   9      87
#> 20  172 15  25 NA  82   177   167   110    23  19    93    57  166  26     192
#> 21   64  9   3 NA  30    61    67    40     6   7    33    24   56  14      70
#> 22    7  0   0 NA   3     7     7     4     3   0     4     0    7   0       7
#> 23   16  0   3 NA   6    16    13    10     3   0     8     5   13   3      16
#> 24    6  0   0 NA   0     6     6     6     0   3     3     0    3   3       6
#> 25   12  0   0 NA   3    12    12     9     0   3     6     3    9   3      12
#> 26  106  3   9 NA  39   115   109    79    13   9    51    45  101  17     118
#> 27   36  6   3 NA  22    36    39    20     6   6    20    10   36   6      42
#> 28   20  0   3 NA   6    23    20    17     4   3     9     7   20   3      23
#> 29   36  0   0 NA  15    45    45    30     3   6    21    15   39   6      45
#> 30   75  6   6 NA  37    75    75    44     5   9    44    23   71  10      81
#> 31   44  0   3 NA  17    44    41    27     3   9    18    14   41   3      44
#> 32   26  0   3 NA  12    29    26    17     3   3    14     9   23   6      29
#> 33   55  3   3 NA  25    58    58    36     4   9    28    20   49  12      61
#> 34   46  3   6 NA  23    53    50    33     4  10    25    17   50   6      56
#> 35   27  0   3 NA  12    33    30    21     3   6    12    12   30   3      33
#> 36    0  0   0 NA   3     3     3     0     0   0     3     0    3   0       3
#> 37  412 30  36 NA 167   449   443   312    48  59   202   170  407  72     479
#> 38  660 45  71 NA 301   704   678   448    79  68   349   253  650  99     749
#> 39  505 21  42 NA 220   548   527   349    51  76   262   180  488  81     569
#> 40  245 12  24 NA 115   261   249   158    25  22   130    96  251  22     273
#> 41  568 39  42 NA 249   616   613   406    60  82   295   218  547 108     655
#> 42  657 42  71 NA 277   705   676   470    77  87   326   257  649  98     747
#> 43   18  0   3 NA   6    21    18    15     3   3    12     3   15   6      21
#> 44   89  3   9 NA  41    98    92    60    13   9    50    29   83  18     101
#> 45 1577 96 149 NA 688  1701  1648  1109   178 203   813   603 1545 252    1797
#>    Total_2 Total_3 Total_4 Total_5 Total
#> 1       76      76      76      76    76
#> 2       58      58      58      58    58
#> 3       29      29      29      29    29
#> 4       50      50      50      50    50
#> 5      108     108     108     108   108
#> 6       40      40      40      40    40
#> 7       97      97      97      97    97
#> 8        3       3       3       3     3
#> 9       15      15      15      15    15
#> 10       3       3       3       3     3
#> 11      83      83      83      83    83
#> 12      63      63      63      63    63
#> 13      46      46      46      46    46
#> 14      50      50      50      50    50
#> 15      34      34      34      34    34
#> 16      30      30      30      30    30
#> 17      44      44      44      44    44
#> 18      43      43      43      43    43
#> 19      87      87      87      87    87
#> 20     192     192     192     192   192
#> 21      70      70      70      70    70
#> 22       7       7       7       7     7
#> 23      16      16      16      16    16
#> 24       6       6       6       6     6
#> 25      12      12      12      12    12
#> 26     118     118     118     118   118
#> 27      42      42      42      42    42
#> 28      23      23      23      23    23
#> 29      45      45      45      45    45
#> 30      81      81      81      81    81
#> 31      44      44      44      44    44
#> 32      29      29      29      29    29
#> 33      61      61      61      61    61
#> 34      56      56      56      56    56
#> 35      33      33      33      33    33
#> 36       3       3       3       3     3
#> 37     479     479     479     479   479
#> 38     749     749     749     749   749
#> 39     569     569     569     569   569
#> 40     273     273     273     273   273
#> 41     655     655     655     655   655
#> 42     747     747     747     747   747
#> 43      21      21      21      21    21
#> 44     101     101     101     101   101
#> 45    1797    1797    1797    1797  1797

 # ====== With incorrect totals
 ex1b <- ex1
 ex1b$s1[1]=2L
 ex1b$arb_A[2]=5L
 RoundKostra(ex1b, idVar="region",strataVar=c("fylke","kostragr"),freqVar=4:19,freqVarGroup=freqVarGroup, singleTotal = FALSE)
#> Warning: Not identical rowsums in all groups
#> [-**.........:=-**.....::=(Check:45)-**...........=]
#> Warning: Not identical rowsums in all groups. First result named as Total
#>    region annet_A arb_A soshj_A trygd_A annet_B arb_B soshj_B trygd_B s1 s2  s3
#> 1  111100      12     3      32      25       0     3       0       0  3  0   3
#> 2  111200       0     5      20      15       6     3       7       3  6  0   6
#> 3  111400       0     6      11       9       3     0       0       0  3  3   0
#> 4  113300       3    11      16      16       0     0       4       3  3  0   3
#> 5  113500       8    14      33      42       0     3       5       3  3  8  11
#> 6  114100       4     3       8      19       0     0       3       0  0  0   3
#> 7  114200       3     8      39      27       3     0       9       8  3  3   5
#> 8  114400       0     0       0       0       0     0       3       0  0  0   0
#> 9  114500       0     0       6       3       3     0       0       0  0  0   3
#> 10 115100       3     0       0       0       0     0       0       0  0  0   0
#> 11 121100       5     0      36      33       0     0       3       3  0  0   6
#> 12 122200       6     4      25      22       3     0       3       0  0  3   7
#> 13 122300       6     3      12      15       0     3       4       3  0  3   3
#> 14 123100       6     5      22      11       0     0       3       3  3  0   3
#> 15 123400       0     3      17       8       0     0       3       3  3  0   0
#> 16 124100       3     0      15       9       0     0       3       0  0  3   3
#> 17 124200       3     0      22      13       3     0       0       3  3  0   0
#> 18 124400       6     8       7      15       0     0       7       0  0  0   4
#> 19 125100       9     7      34      28       0     3       6       0  3  3   7
#> 20 125900      23    16      79      48       0     6      14       6  3  8   9
#> 21 126400       3     7      27      19       0     3       6       5  0  3   3
#> 22 126500       0     3       4       0       0     0       0       3  0  0   3
#> 23 141100       3     0       5       5       0     0       3       0  0  0   0
#> 24 141200       3     0       0       3       0     0       0       0  0  0   3
#> 25 141300       0     3       3       3       0     0       3       0  0  0   0
#> 26 141600      10     9      40      42       3     3      11       3  3  0  12
#> 27 141700       3     3      20      10       0     3       0       0  0  0   3
#> 28 141800       4     3       6       7       0     0       3       0  0  3   0
#> 29 141900       3     6      18      12       0     0       3       3  0  0   9
#> 30 142800       5     6      37      23       0     3       7       3  3  3   3
#> 31 142900       3     6      18      14       0     3       0       0  0  0   0
#> 32 143000       3     0      11       9       3     0       3       0  0  0   3
#> 33 143100       4     3      25      17       0     6       3       3  3  3   0
#> 34 143300       7    10      22      11       0     0       3       3  3  0   7
#> 35 144100       0     6      12      12       0     0       3       0  0  0   3
#> 36 144400       0     0       3       0       0     0       0       0  0  0   0
#> 37   1100      33    50     165     156      15     9      31      17 21 14  34
#> 38   1200      70    56     300     221       6    15      52      29 15 23  48
#> 39   1400      48    55     220     168       6    18      42      15 12  9  43
#> 40     01      22    15     117      93       0     6      13       6  6  3  15
#> 41     02      45    62     248     187      15    18      47      31 21 18  45
#> 42     03      71    72     276     236       9    18      50      18 18 22  56
#> 43     04       3     3       6       3       0     0       6       0  0  0   0
#> 44     05      10     9      38      26       3     0       9       6  3  3   9
#> 45  Total     151   161     685     545      27    42     125      61 48 46 125
#>      s4  W   X  Y   Z nOOtW nOOtX nOOtZ annet arb soshj trygd    A   B Total_1
#> 1    67  3  11 NA  31    72    64    44    12   6    32    25   72   3      75
#> 2    47  3   0 NA  20    56    59    39     6   8    27    18   40  19      59
#> 3    23  0   0 NA  12    29    29    17     3   6    11     9   26   3      29
#> 4    47  6   3 NA  17    47    50    36     3  11    20    19   46   7      53
#> 5    86  9   9 NA  33    99    99    75     8  17    38    45   97  11     108
#> 6    34  3   3 NA   6    34    34    31     4   3    11    19   34   3      37
#> 7    86  6   6 NA  39    91    91    58     6   8    48    35   77  20      97
#> 8     3  0   0 NA   0     3     3     3     0   0     3     0    0   3       3
#> 9     9  3   0 NA   6     9    12     6     3   0     6     3    9   3      12
#> 10    3  0   3 NA   0     3     0     3     3   0     0     0    3   0       3
#> 11   74  0   0 NA  31    80    80    49     5   0    39    36   74   6      80
#> 12   53  0   6 NA  24    63    57    39     9   4    28    22   57   6      63
#> 13   40  3   6 NA  13    43    40    33     6   6    16    18   36  10      46
#> 14   44  3   6 NA  20    47    44    30     6   5    25    14   44   6      50
#> 15   31  0   0 NA  17    34    34    17     0   3    20    11   28   6      34
#> 16   24  3   3 NA  15    27    27    15     3   0    18     9   27   3      30
#> 17   41  0   3 NA  22    44    41    22     6   0    22    16   38   6      44
#> 18   39  0   7 NA   7    43    36    36     6   8    14    15   36   7      43
#> 19   74  9   9 NA  34    78    78    53     9  10    40    28   78   9      87
#> 20  172 15  25 NA  82   177   167   110    23  22    93    54  166  26     192
#> 21   64  9   3 NA  30    61    67    40     3  10    33    24   56  14      70
#> 22    7  0   3 NA   6    10     7     4     0   3     4     3    7   3      10
#> 23   16  0   3 NA   3    16    13    13     3   0     8     5   13   3      16
#> 24    3  0   3 NA   3     6     3     3     3   0     0     3    6   0       6
#> 25   12  0   0 NA   3    12    12     9     0   3     6     3    9   3      12
#> 26  106  3  12 NA  42   118   109    79    13  12    51    45  101  20     121
#> 27   36  3   0 NA  19    36    39    20     3   6    20    10   36   3      39
#> 28   20  0   3 NA   6    23    20    17     4   3     9     7   20   3      23
#> 29   36  0   0 NA  15    45    45    30     3   6    21    15   39   6      45
#> 30   75  9   6 NA  40    75    78    44     5   9    44    26   71  13      84
#> 31   44  0   3 NA  17    44    41    27     3   9    18    14   41   3      44
#> 32   26  0   3 NA  12    29    26    17     6   0    14     9   23   6      29
#> 33   55  3   3 NA  25    58    58    36     4   9    28    20   49  12      61
#> 34   46  3   9 NA  23    53    47    33     7  10    25    14   50   6      56
#> 35   30  0   0 NA  12    33    33    21     0   6    15    12   30   3      33
#> 36    3  0   0 NA   3     3     3     0     0   0     3     0    3   0       3
#> 37  405 33  35 NA 164   443   441   312    48  59   196   173  404  72     476
#> 38  663 42  71 NA 301   707   678   448    76  71   352   250  647 102     749
#> 39  508 21  45 NA 223   551   527   349    54  73   262   183  491  81     572
#> 40  246 12  17 NA 114   260   255   158    22  21   130    99  247  25     272
#> 41  569 36  45 NA 247   617   608   406    60  80   295   218  542 111     653
#> 42  654 45  74 NA 280   705   676   470    80  90   326   254  655  95     750
#> 43   21  0   3 NA   6    21    18    15     3   3    12     3   15   6      21
#> 44   86  3  12 NA  41    98    89    60    13   9    47    32   83  18     101
#> 45 1576 96 151 NA 688  1701  1646  1109   178 203   810   606 1542 255    1797
#>    Total_2 Total_3 Total_4 Total_5 Total
#> 1       73      75      75      75    75
#> 2       59      59      59      59    59
#> 3       29      29      29      29    29
#> 4       53      53      53      53    53
#> 5      108     108     108     108   108
#> 6       37      37      37      37    37
#> 7       97      97      97      97    97
#> 8        3       3       3       3     3
#> 9       12      12      12      12    12
#> 10       3       3       3       3     3
#> 11      80      80      80      80    80
#> 12      63      63      63      63    63
#> 13      46      46      46      46    46
#> 14      50      50      50      50    50
#> 15      34      34      34      34    34
#> 16      30      30      30      30    30
#> 17      44      44      44      44    44
#> 18      43      43      43      43    43
#> 19      87      87      87      87    87
#> 20     192     192     192     192   192
#> 21      70      70      70      70    70
#> 22      10      10      10      10    10
#> 23      16      16      16      16    16
#> 24       6       6       6       6     6
#> 25      12      12      12      12    12
#> 26     121     121     121     121   121
#> 27      39      39      39      39    39
#> 28      23      23      23      23    23
#> 29      45      45      45      45    45
#> 30      84      84      84      84    84
#> 31      44      44      44      44    44
#> 32      29      29      29      29    29
#> 33      61      61      61      61    61
#> 34      56      56      56      56    56
#> 35      33      33      33      33    33
#> 36       3       3       3       3     3
#> 37     474     476     476     476   476
#> 38     749     749     749     749   749
#> 39     572     572     572     572   572
#> 40     270     272     272     272   272
#> 41     653     653     653     653   653
#> 42     750     750     750     750   750
#> 43      21      21      21      21    21
#> 44     101     101     101     101   101
#> 45    1795    1797    1797    1797  1797
 a2 <- RoundKostra(ex1b, idVar="region",strataVar=c("fylke","kostragr"),freqVar=4:19,freqVarGroup=freqVarGroup, singleTotal = FALSE,extraOutput=TRUE)
#> Warning: Not identical rowsums in all groups
#> [-**.........:=-**.....::=(Check:45)-**...........=]
 table(a2$input$s1_s2_s3_s4,  useNA ="always") # Missing values in ad hoc created data when incorrect totals
#> 
#>   s1   s2   s3   s4 <NA> 
#>   39   35   66  180    1 

 # ===============================================================
 #   Examples  With parameter output
 # ===============================================================

 RoundKostra(z2w ,idVar="region", freqVar="arbeid",output="status")
#> [-**..:=]
#>    region arbeid
#> 1  011100      o
#> 2  043200      r
#> 3  051300      o
#> 4  051400      r
#> 5  061900      o
#> 6  062000      o
#> 7  083300      o
#> 8  083400      o
#> 9  012100      o
#> 10 102100      o
#> 11 102700      r
#> 12  Total      r
 RoundKostra(z2w ,idVar="region", freqVar="arbeid",output="difference")
#> [-**..:=]
#>    region arbeid
#> 1  011100      0
#> 2  043200     -1
#> 3  051300      0
#> 4  051400      1
#> 5  061900      0
#> 6  062000      0
#> 7  083300      0
#> 8  083400      0
#> 9  012100      0
#> 10 102100      0
#> 11 102700      1
#> 12  Total      1
 RoundKostra(z2w ,idVar="region",strataVar=c("fylke","kostragr"),freqVar=4:7,output="status")
#> [-**..:=]
#>    region annet arbeid soshjelp trygd Total
#> 1  011100     o      o        o     o     o
#> 2  043200     o      r        o     o     r
#> 3  051300     o      o        o     o     o
#> 4  051400     o      r        o     o     r
#> 5  061900     o      o        o     o     o
#> 6  062000     o      o        o     o     o
#> 7  083300     o      o        o     o     o
#> 8  083400     o      o        o     o     o
#> 9  012100     o      o        o     r     r
#> 10 102100     o      o        o     o     o
#> 11 102700     o      r        o     o     r
#> 12   0100     o      o        o     r     r
#> 13   0400     o      r        o     o     r
#> 14   0500     o      r        o     o     r
#> 15   0600     o      o        o     o     o
#> 16   0800     o      o        o     o     o
#> 17   1000     o      r        o     o     r
#> 18     03     o      o        o     o     o
#> 19     04     o      r        o     r     r
#> 20  Total     o      r        o     r     r
 RoundKostra(z2w ,idVar="region",strataVar=c("fylke","kostragr"),freqVar=4:7,output="difference")
#> [-**..:=]
#>    region annet arbeid soshjelp trygd Total
#> 1  011100     0      0        0     0     0
#> 2  043200     0     -1        0     0    -1
#> 3  051300     0      0        0     0     0
#> 4  051400     0      1        0     0     1
#> 5  061900     0      0        0     0     0
#> 6  062000     0      0        0     0     0
#> 7  083300     0      0        0     0     0
#> 8  083400     0      0        0     0     0
#> 9  012100     0      0        0    -2    -2
#> 10 102100     0      0        0     0     0
#> 11 102700     0      1        0     0     1
#> 12   0100     0      0        0    -2    -2
#> 13   0400     0     -1        0     0    -1
#> 14   0500     0      1        0     0     1
#> 15   0600     0      0        0     0     0
#> 16   0800     0      0        0     0     0
#> 17   1000     0      1        0     0     1
#> 18     03     0      0        0     0     0
#> 19     04     0      1        0    -2    -1
#> 20  Total     0      1        0    -2    -1

 # ===============================================================
 #   Micro data example  (":::" since functions not exported yet)
 # ===============================================================

 microData <- Kostra:::microEx1()                 # A micro data set
 freqData  <- MakeFreq(microData,"freq") # Make cross-classified data
 freqData$id <- 1:NROW(freqData)                  # Add id-variable

 #  Rounding with makeSums=FALSE
 freqRound <- RoundKostra(freqData, idVar="id", strataVar=c("region", "fylke", "kostragr", "hovedint"), freqVar="freq", makeSums=FALSE)
#> [-**..:=]

 microRound  <- MakeMicro(freqRound,"freq")  # Create micro data set from output
 microRound  <- microRound[,-c(1,6)]                        # Remove some variables

 # Alternative where only region sums and the cross-classifications fylke*hovedint and kostragr*hovedint are to be published
 freqRound2 <- RoundKostra(freqData, idVar="id", formula=("region +fylke*hovedint + kostragr*hovedint"), freqVar="freq",
                   makeSums=FALSE, allSmall=FALSE, region="region",fylke="fylke",kostragr="kostragr",hovedint="hovedint")
#> [-**.:=]
 microRound2  <- MakeMicro(freqRound2,"freq")
 microRound2  <- microRound2[,-c(1,2)]
```
