# SimpleRank2NumVar

SimpleRank2NumVar

## Usage

``` r
SimpleRank2NumVar(
  data,
  idName,
  measureName,
  periodName,
  period,
  periodCompare,
  strataName,
  antall,
  grense,
  identiske
)
```

## Arguments

- data:

  Input data set of class data.frame.

- idName:

  Name of the identification variable.

- measureName:

  Name of the variable to edit.

- periodName:

  Name of the period identification variable.

- period:

  value of period to edit.

- antall:

  Parameter specifying how many of the biggest units to be listed.
  Default 5.

- grense:

  Parameter specifying a threshold for the units to be listed. This
  parameter overrules antall. Optional.

- identiske:

  When TRUE, only units with value on both x and y are used in the
  calculations. Default FALSE.

- xVar:

  Name of the x variable to be compared.

- yVar:

  Name of the y variable to be compared.

- strataVar:

  Name of stratification variable. Optional. If strataVar is given, the
  calculation and listing is performed within each stratum.
