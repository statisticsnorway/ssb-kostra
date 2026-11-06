# INTERNAL FUNCTION: Replace missing values by zero

INTERNAL FUNCTION: Replace missing values by zero

## Usage

``` r
NaToZero(x, warn = NULL)
```

## Arguments

- x:

  Input data

- warn:

  Warning text when non-null

## Value

Input data where missing is replaced by zero

## Examples

``` r
NaToZero(c(1,2,NA,4))
#> [1] 1 2 0 4
```
