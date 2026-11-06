# SimpleDiff2NumVar

SimpleDiff2NumVar

## Usage

``` r
SimpleDiff2NumVar(
  data,
  idName,
  measureName,
  measureNameCompareAlt = NULL,
  periodName,
  period,
  periodCompare,
  strataName = NULL,
  antall,
  grense = NULL,
  kommentarName = NULL
)
```

## Arguments

- data:

  Input data set of class data.frame.

- idName:

  Name of id-variable

- measureName:

  Name of the variable to edit

- measureNameCompareAlt:

  Name of the alternative variable to edit against

- periodName:

  Name of the period identification variable.

- period:

  value of period to edit.

- periodCompare:

  value of period to edit against.

- strataName:

  Name of starta-variable. Single strata when NULL (default)

- antall:

  Parameter specifying how many units with the biggest difference to be
  listed. Default 5.

- grense:

  Parameter specifying a threshold for the units to be listed. This
  parameter overrules antall. Optional.

- kommentarName:

  Name of a variable giving information about the editing. Optional.
