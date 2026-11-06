# AutoNetting

Ulike varianter som ble utprøvd. AutoNettingCopy er løsningen som ble
valgt.

## Usage

``` r
AutoNetting(
  nettinghierarki,
  hierarki,
  title = "nettinghierarki",
  toRemove = TRUE,
  singleLoop = FALSE
)

AutoNettingFF(
  nettinghierarki,
  hierarki,
  title = "nettinghierarki",
  toRemove = FALSE,
  singleLoop = FALSE
)

AutoNettingFT(
  nettinghierarki,
  hierarki,
  title = "nettinghierarki",
  toRemove = FALSE,
  singleLoop = TRUE
)

AutoNettingTF(
  nettinghierarki,
  hierarki,
  title = "nettinghierarki",
  toRemove = TRUE,
  singleLoop = FALSE
)

AutoNettingTT(
  nettinghierarki,
  hierarki,
  title = "nettinghierarki",
  toRemove = TRUE,
  singleLoop = TRUE
)

AutoNettingNy(
  nettinghierarki,
  hierarki,
  title = "nettinghierarki",
  loop = TRUE
)

AutoNettingNyFALSE(
  nettinghierarki,
  hierarki,
  title = "nettinghierarki",
  loop = FALSE
)

AutoNettingIngenEndering(nettinghierarki, hierarki = NULL, title = NULL)

AutoNettingCopy(
  nettinghierarki,
  hierarki,
  title = "nettinghierarki",
  copyKode = "COPY"
)
```

## Arguments

- nettinghierarki:

  nettinghierarki

- hierarki:

  hierarki

- title:

  title

## Value

Ny versjon av nettinghierarki
