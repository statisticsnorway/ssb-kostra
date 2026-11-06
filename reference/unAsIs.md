# Getting rid of 'AsIs' class attribute

Getting rid of 'AsIs' class attribute

## Usage

``` r
unAsIs(X)
```

## Arguments

- X:

  an R object

## Value

X without 'AsIs'

## Details

Notice that I take care to preserve any other class attributes that the
vector might have.

## Note

The function is taken from
<http://stackoverflow.com/questions/12865218/getting-rid-of-asis-class-attribute>

## Author

Verena Haunschmid

## Examples

``` r
class(I(TRUE))
#> [1] "AsIs"
class(unAsIs(I(TRUE)))
#> [1] "logical"
class(data.frame(x=I(matrix(1:12,3,4)))$x)
#> [1] "AsIs"
class(unAsIs(data.frame(x=I(matrix(1:12,3,4)))$x))
#> [1] "matrix" "array" 
```
