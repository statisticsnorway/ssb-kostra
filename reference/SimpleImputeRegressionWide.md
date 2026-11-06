# SimpleImputeRegressionWide

SimpleImputeRegressionWide

## Usage

``` r
SimpleImputeRegressionWide(
  data,
  id,
  period,
  periodValue,
  measureName,
  compareName,
  strata,
  methodName = "ratio",
  limitModel,
  limitIterate,
  limitImpute
)
```

## Arguments

- data:

  Input data set of class data.frame

- period:

  value of period to edit.

- measureName:

  Name of the variable to edit.

- compareName:

  Name of the variable to edit against.

- methodName:

  The method (model and weight) coded as a string: "ordinary" (default),
  "ratio", "noconstant", "mean" or "ratioconstant".

- limitModel:

  Studentized residuals limit. Above limit -\> group 2.

- limitIterate:

  Studentized residuals limit for iterative calculation of studentized
  residuals.

- limitImpute:

  Studentized residuals limit. Above limit -\> group 3.

- idName:

  Name of id-variable

- periodName:

  Name of the period identification variable.

- strataName:

  Name of starta-variable. Single strata when NULL (default)

## Value

see ImputeRegression.R

## Note

Øyvind: Denne funksjonen i Kostra-pakken opprinnelig skrevet av Håkon
ser ikke ut til å være i bruk. Det brukes kode utenfor. Har endret
funksjonen til å være som siste emailutveksling med Bjørn-Andre Har ikke
oppdatert dokumentasjonen ellers.
