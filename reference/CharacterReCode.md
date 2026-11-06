# CharacterReCode

CharacterReCode

## Usage

``` r
CharacterReCode(x, oldLevels, newLevels)
```

## Arguments

- x:

  (character) vector

- oldLevels:

  oldLevels

- newLevels:

  newLevels

## Value

character vector

## Examples

``` r
CharacterReCode(rep(1:4,2),c("2","3"),c("B","C"))
#> [1] "1" "B" "C" "4" "1" "B" "C" "4"
```
