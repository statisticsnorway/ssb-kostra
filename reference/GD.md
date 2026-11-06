# Extend an input parameter to GetData

Extend an input parameter to GetData

## Usage

``` r
GD(x, ...)
```

## Arguments

- x:

  Original input parameter

- ...:

  New elements to be added

## Value

Extended version of x

## Examples

``` r
GD("ola",aar=2014)
#> [[1]]
#> [1] "ola"
#> 
#> $aar
#> [1] 2014
#> 
GD(list("kari",expression(aar>2014)),function(x) x^2)
#> [[1]]
#> [1] "kari"
#> 
#> [[2]]
#> expression(aar > 2014)
#> 
#> [[3]]
#> function (x) 
#> x^2
#> <environment: 0x55bbc53a11b8>
#> 
```
