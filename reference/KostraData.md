# Return a data set

Return a data set

## Usage

``` r
KostraData(dataset, path = NULL)
```

## Arguments

- dataset:

  Name of data set within the Kostra package (rateData, testdata)

- path:

  When non-NULL the data set is read from "path/dataset.RData"

## Value

The data set

## Examples

``` r
 z  <- KostraData("balanseRegnskapDataPen")
 z  <- KostraData("balanseRegnskapData")
 z  <- KostraData("kostraRegnskapDataPen")
 z  <- KostraData("kostraRegnskapData")
 z  <- KostraData("rateData")
 b1 <- KostraData("testdata")
 tull <- KostraData("tulledata")
```
