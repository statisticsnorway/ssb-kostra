# SimpleHb

SimpleHb

## Usage

``` r
SimpleHb(
  data,
  idName,
  periodName,
  measureName,
  period,
  periodCompare,
  pU,
  pA,
  pC,
  strataName
)
```

## Arguments

- data:

  Input of Hb is a data set of class data.frame.

- idName:

  Name of the identification variable.

- periodName:

  Name of the period identification variable.

- measureName:

  Name of the variable to edit.

- period:

  value of period to edit.

- periodCompare:

  value of period to edit against.

- pU:

  Parameter that adjusts for different level of the variables.Default
  value 0.5.

- pA:

  Parameter that adjusts for small differences between the median and
  the 1st or 3rd quartile. Default value 0.05.

- pC:

  Parameter that controls the length of the confidence interval.Default
  value 20.

- strataName:

  Name of stratification variable. Optional. If strataName is given, the
  HB method is performed within each stratum.

## Value

see Hb.r
