# ReplaceDimList

Replace list elements of sdcTable coded hierarchies. Replacement
elements can be sdcTable coded or TauArgus coded.

## Usage

``` r
ReplaceDimList(dimList, replaceList, total = "Total")
```

## Arguments

- dimList:

  Named list of data frames (sdcTable coded)

- replaceList:

  Named list where elements are data frames (sdcTable coded) or
  character vectors (TauArgus coded)

- total:

  String used to name totals when TauArgus coded input

## Value

Updated dimList where some or all elements are replaced

## Examples

``` r
# First generate dimLists
dimListA <- FindDimLists(SSBtoolsData("sprt_emp_withEU")[, c("geo", "eu", "age")])
dimListB <- FindDimLists(SSBtoolsData("sprt_emp_withEU")[, c("geo", "age")])
dimListC <- FindDimLists(SSBtoolsData("sprt_emp_withEU")[, c("geo", "eu")])

# dimListA1 and dimListA are identical
dimListA1 <- ReplaceDimList(dimListB, dimListC)
identical(dimListA, dimListA1)
#> [1] TRUE

# replaceList can be TauArgus coded
hcrC <- DimList2Hrc(dimListC)

# dimListA2 and dimListA are identical
dimListA2 <- ReplaceDimList(dimListB, hcrC)
identical(dimListA, dimListA2)
#> [1] TRUE

# Also possible when duplicated names
ReplaceDimList(FindDimLists(EasyData("z3")[, -7]), 
               FindDimLists(EasyData("z2")[, -5]))
#> $region
#>    levels codes
#> 1       @ Total
#> 2      @@     1
#> 3     @@@     A
#> 4     @@@     I
#> 5      @@     4
#> 6     @@@     B
#> 7      @@     5
#> 8     @@@     C
#> 9     @@@     D
#> 10     @@     6
#> 11    @@@     E
#> 12    @@@     F
#> 13     @@     8
#> 14    @@@     G
#> 15    @@@     H
#> 16     @@    10
#> 17    @@@     J
#> 18    @@@     K
#> 
#> $region
#>    levels codes
#> 1       @ Total
#> 2      @@   300
#> 3     @@@     A
#> 4     @@@     B
#> 5     @@@     C
#> 6     @@@     D
#> 7     @@@     E
#> 8     @@@     F
#> 9     @@@     G
#> 10    @@@     H
#> 11     @@   400
#> 12    @@@     I
#> 13    @@@     J
#> 14    @@@     K
#> 
#> $hovedint
#>   levels    codes
#> 1      @    Total
#> 2     @@    annet
#> 3     @@   arbeid
#> 4     @@ soshjelp
#> 5     @@    trygd
#> 
#> $mnd
#>   levels  codes
#> 1      @  Total
#> 2     @@ M01M05
#> 3    @@@ m01m05
#> 4     @@ M06M12
#> 5    @@@ m06m09
#> 6    @@@ m10m12
#> 
```
