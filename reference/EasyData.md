# Function that returns a dataset

Function that returns a dataset

## Usage

``` r
EasyData(dataset, path = NULL)
```

## Arguments

- dataset:

  Name of data set within the easySdcTable package

- path:

  When non-NULL the data set is read from "path/dataset.RData"

## Value

The dataset

## Note

The function returns the same datasets as `SSBtoolsData`.

## Examples

``` r
 z  <- EasyData("sosialFiktiv")
```
