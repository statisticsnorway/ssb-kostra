# Assign named elements of a list or data.frame to caller's environment

Assign named elements of a list or data.frame to caller's environment

## Usage

``` r
Separate(x)
```

## Arguments

- x:

  A list or data.frame

## Value

The function returns only NULL, but breaks the rules for ordinary
functions. Objects are written to caller's environment.

## Examples

``` r
Separate(data.frame(aAa=1:3, bBb=100))
#> NULL
aAa*bBb
#> [1] 100 200 300
```
