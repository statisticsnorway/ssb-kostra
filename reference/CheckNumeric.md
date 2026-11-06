# INTERNAL FUNCTION: Check values of a numeric vector or matrix according to limits.

Possibly changed data is retuned

## Usage

``` r
CheckNumeric(
  x,
  minLimit = -Inf,
  maxLimit = Inf,
  setNA = TRUE,
  allowMissing = TRUE,
  allowInf = FALSE,
  varName = "Variable"
)
```

## Arguments

- x:

  numeric vector or matrix

- minLimit:

  lower limit

- maxLimit:

  upper limit

- setNA:

  When TRUE values outside limits are set to NA instead of error

- allowMissing:

  When FALSE missing values cause error

- allowInf:

  When FALSE -Inf and Inf always considered outside limits

- varName:

  String used in warning or error message

## Value

x is retuned, possibly changed

## Examples

``` r
CheckNumeric(-1:10,minLimit=0,varName="Hello")
#> Warning: Hello outside limits set to NA
#>  [1] NA  0  1  2  3  4  5  6  7  8  9 10
```
