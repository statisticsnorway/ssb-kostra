# Change numeric variables in data frame to integer

Only done when the integer values are exact the same as the original
values

## Usage

``` r
IntegerDataFrame(x, makeWarning = FALSE, allNumeric = TRUE)
```

## Arguments

- x:

  data frame

- makeWarning:

  When TRUE warning made when not all numeric variables could be changed

- allNumeric:

  When TRUE no change is made unless all could be changed

## Value

data frame
