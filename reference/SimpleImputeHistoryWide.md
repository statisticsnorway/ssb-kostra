# SimpleImputeHistoryWide

SimpleImputeHistoryWide

## Usage

``` r
SimpleImputeHistoryWide(
  data,
  idName,
  periodName,
  measureName,
  strataName = NULL,
  weightMethod,
  reverse
)
```

## Arguments

- data:

  Input data set of class data.frame

- idName:

  Name of id-variable

- periodName:

  Name of the period identification variable.

- measureName:

  Name of the variable to estimate.

- strataName:

  Name of starta-variable. Single strata when NULL (default)

- weightMethod:

  The weight method for error calculations coded as a string: "ordinary"
  (default) or "ratio".

- reverse:

  When TRUE most resent is last instead of first (see xName and yName).
  Default is FALSE.
