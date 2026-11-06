# Default IncProgress function

Default IncProgress function

## Usage

``` r
IncDefault()
```

## Note

Instead of using `IncProgress = IncDefault` in
ProtectTable1/ProtectTable one could use  
`IncProgress = function(){cat("."); flush.console()}`  
but this results in wrong "usage line" in the documentation since ";" is
not included.

## Examples

``` r
 IncDefault() 
#> .
```
