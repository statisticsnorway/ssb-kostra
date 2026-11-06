# AutoFormel

Formler som avhenger av hverandre omskrives slik at alle begrep på
høyres side refererer til verdier fra beregning uten formler.

## Usage

``` r
AutoFormel(s, fra2til3 = TRUE, laAvhenge = FALSE)
```

## Arguments

- s:

  Vektor av formler

- fra2til3:

  Dupliserer formelbegreper uten kontoklasse ved TRUE

- laAvhenge:

  Erstatter IKKE avhengigheter med parenteser ved TRUE

## Value

Vektor av formler

## Examples

``` r
 AutoFormel(c("D:FG2:AGD13 = D:FG2:AGD13 + D:FG2:AG12","D:FG2:AGD18 = D:FG2:AGD18 + D:FG2:AGD13"))
#> [1] "D:FG2:AGD13 = D:FG2:AGD13 + D:FG2:AG12"                  
#> [2] "D:FG2:AGD18 = D:FG2:AGD18 + ( D:FG2:AGD13 + D:FG2:AG12 )"
```
