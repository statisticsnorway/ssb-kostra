# Wrapper when name and value of parameter is the same

Wrapper when name and value of parameter is the same

## Usage

``` r
DotWrap(fun, dots, ...)
```

## Arguments

- fun:

  Function to be called

- dots:

  vector of parameternames where value is name

- ...:

  other parameters

## Examples

``` r

Funksjon1 <- function(a = 3, b = "Hei", ...) {
  cat("a = ", a, "\n")
  cat("b = ", b, "\n")
  cat("dots = \n")
  print(list(...))
}

Funksjon2 <- function(aa = 33, bb = "Heia", ...) {
  sys.call()
}

DotWrap("Funksjon1", c("p1", "p2"), a = 15, tull = "tullball")
#> a =  15 
#> b =  Hei 
#> dots = 
#> $tull
#> [1] "tullball"
#> 
#> $p1
#> [1] "p1"
#> 
#> $p2
#> [1] "p2"
#> 
DotWrap("Funksjon2", c("p1", "p2"), a = 15, tull = "tullball")
#> Funksjon2(a = 15, tull = "tullball", p1 = "p1", p2 = "p2")

DotWrap("Funksjon2", NULL, a = 9)  # Use NULL when dots is not in use
#> Funksjon2(a = 9)

DotWrap("Funksjon2", strsplit("par1 par2", split = " ")[[1]], ho = "hoho")
#> Funksjon2(ho = "hoho", par1 = "par1", par2 = "par2")

DotWrap("Funksjon2", strsplit("parA;parB;parC", split = ";")[[1]], ho = "hoho", a = 88)
#> Funksjon2(ho = "hoho", a = 88, parA = "parA", parB = "parB", 
#>     parC = "parC")
DotWrap("Funksjon1", strsplit("parA;parB;parC", split = ";")[[1]], ho = "hoho", a = 88)
#> a =  88 
#> b =  Hei 
#> dots = 
#> $ho
#> [1] "hoho"
#> 
#> $parA
#> [1] "parA"
#> 
#> $parB
#> [1] "parB"
#> 
#> $parC
#> [1] "parC"
#> 
```
