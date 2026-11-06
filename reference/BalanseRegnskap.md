# UTDATERT FUNKSJON: BalanseRegnskap

UTDATERT FUNKSJON: Bruk funksjoner i pakken på
github.com/statisticsnorway/ssb-kostraregnskap/

## Usage

``` r
BalanseRegnskapBeregning(
  data,
  kapittelhierarki,
  kapitler,
  ...,
  perioder = NULL,
  output
)

BalanseRegnskapBeregningInput(...)

BalanseRegnskapBeregningHierarkiALT(...)

BalanseRegnskapBeregningHierarki(...)

BalanseRegnskapBeregningHierarkiInput(...)

BalanseRegnskap(data, kapittelhierarki, ..., perioder = NULL)

BalanseRegnskapEnPeriode(
  data,
  kapittelhierarki,
  kombinasjoner = NULL,
  regioner = NULL,
  storkombinasjoner = NULL,
  kapitler = NULL,
  omfang = NULL,
  output = "en",
  printData = TRUE,
  lag0300 = FALSE,
  fixRegionkode = TRUE
)
```

## Arguments

- data:

  inputdata

- kapittelhierarki:

  kapittelhierarki

- kapitler:

  kapitler som skal med i output

- ...:

  Parametere som sendes videre til BalanseRegnskapEnPeriode

- perioder:

  Perioder for beregningen. Ved NULL (default) brukes alle perioder som
  finnes i data.

- output:

  kode for type output som (brukes av BalanseRegnskapBeregningHierarki
  og BalanseRegnskapBeregningInput)

- kombinasjoner:

  kombinasjoner kapitler og regnskapsomfang som skal med i output

- regioner:

  regioner som skal med i output. Kan også gjøre bruk av \*, ?, ! og –

- storkombinasjoner:

  kombinasjoner kapitler,regnskapsomfang og region som skal med i output

- omfang:

  regnskapsomfang som skal med i output

- printData:

  Ved TRUE printes to første og to siste rader av alle inputdataene

- lag0300:

  Ved TRUE kopieres region 0301 til 0300 i inputdata

- fixRegionkode:

  Ved TRUE (default): Sørger for blanke i starten/slutten fjernes og at
  regionkoder får 4 eller 6 plasser og ledende nuller (gir warning ved
  endring av input)

## Value

Data frame med samme variabler som data i input og med
kapitler/regnskapsomfang i henhold til annen input

## Details

Funksjon som ligner på KostraRegnskap, men mye enklere.

## Note

BalanseRegnskapBeregningInput og BalanseRegnskapBeregningHierarki går
via funksjonen BalanseRegnskapBeregning. Parametere til den funksjonen
trengs bortsett fra «output». Spesielt må altså parameterne
kapittelhierarki og kapitler med. Det skal (i utgangspunktet) være en
verdi på hver av disse. Funksjonen BalanseRegnskapBeregningHierarkiALT
er alternativ til BalanseRegnskapBeregningHierarki med andre variabler i
output.

## See also

[`KostraRegnskap`](https://statisticsnorway.github.io/ssb-kostra/reference/KostraRegnskap.md),
`HierarchyCompute`, `HierarchicalWildcardGlobbing`

## Examples

``` r

# Ved FALSE brukes data som ligger i pakken der bare noen regioner er med.

lesDataFraS <- FALSE
if (!lesDataFraS){
  bData <- KostraData("balanseRegnskapDataPen")   # KostraData("balanseRegnskapDataPen") er alternativ der
  inputdata <- bData$data                         #    automatisk omkoding trengs (som fixRegionkode)
  hierarki <- bData$kapittelhierarki              #    Fungerer like bra, men med flere warning
} else {

  dataPath <- "S:/Prosjekt/2228 KOSTRA publisering/Metode/Regnskap/R_balanse"
  # dataPath <- 'C:\Users\oyl\Documents\work\Rworking\regnskap\R_balanse'

  hierarkiFil <- paste(dataPath, "/Kapittel_hierarki_2016_bal_KLASS", ".csv", sep = "")
  dataFil <- paste(dataPath, "/Inputdata_balanse_2016", ".csv", sep = "")
  hierarki <- read.csv2(hierarkiFil, colClasses = c("character"))
  inputdata <- read.csv2(dataFil, colClasses = c("integer", "character", "character", "character", "character", "integer"))
}

z <- BalanseRegnskap(inputdata, hierarki, regioner = c("2021", "2022", "0301", "1100"))
#> 
#> data
#>     periode region kapittel regnskapsomfang  belop
#> 1      2016   0100       10               B 559923
#> 2      2016   0100       10           sbedr   3215
#> 305    2016   2022     9200               B    465
#> 306    2016   2022     9999               B  -7663
#> 
#> 
#> kapittelhierarki
#>     periode   to from sign
#> 1      2016  KG2   10    +
#> 2      2016  KG2   11    +
#> 119    2016 KG37  KG8    +
#> 120    2016 K580  580    +
#> 
#> 
#> kombinasjoner
#> NULL
#> 
#> 
#> regioner
#>      x
#> 1 2021
#> 2 2022
#> 3 0301
#> 4 1100
#> 
#> 
#> storkombinasjoner
#> NULL
#> 
#> 
#> kapitler
#> NULL
#> 
#> 
#> omfang
#> NULL
#> 
#> 
#>  [ Kombinasjonsberegninger...] [ Lager kombinasjoner ...]
#> Warning: Items in colSelect not in data[,'region'] set to zero: 1100
#> 
#> output 
#>      periode region regnskapsomfang kapittel    belop
#> 1       2016   0301               A      KG2 14353696
#> 2       2016   0301               B      KG2 12441300
#> 1327    2016   2022           sbedr      580        0
#> 1328    2016   2022        lanefond      580        0
#> 
xA <- BalanseRegnskapBeregningInput(inputdata, hierarki, regioner = "0301", omfang = "A", kapitler = "KG3")
#> 
#> data
#>     periode region kapittel regnskapsomfang  belop
#> 1      2016   0100       10               B 559923
#> 2      2016   0100       10           sbedr   3215
#> 305    2016   2022     9200               B    465
#> 306    2016   2022     9999               B  -7663
#> 
#> 
#> kapittelhierarki
#>     periode   to from sign
#> 1      2016  KG2   10    +
#> 2      2016  KG2   11    +
#> 119    2016 KG37  KG8    +
#> 120    2016 K580  580    +
#> 
#> 
#> kombinasjoner
#> NULL
#> 
#> 
#> regioner
#>      x
#> 1 0301
#> 
#> 
#> storkombinasjoner
#> NULL
#> 
#> 
#> kapitler
#>     x
#> 1 KG3
#> 
#> 
#> omfang
#>   x
#> 1 A
#> 
#> 
#>  [ Kombinasjonsberegninger...] [ Lager kombinasjoner ...]
#>   regnskapsomfang kapittel
#> 1               A      KG3
xB <- BalanseRegnskapBeregningInput(inputdata, hierarki, regioner = "0301", omfang = "B", kapitler = "KG3")
#> 
#> data
#>     periode region kapittel regnskapsomfang  belop
#> 1      2016   0100       10               B 559923
#> 2      2016   0100       10           sbedr   3215
#> 305    2016   2022     9200               B    465
#> 306    2016   2022     9999               B  -7663
#> 
#> 
#> kapittelhierarki
#>     periode   to from sign
#> 1      2016  KG2   10    +
#> 2      2016  KG2   11    +
#> 119    2016 KG37  KG8    +
#> 120    2016 K580  580    +
#> 
#> 
#> kombinasjoner
#> NULL
#> 
#> 
#> regioner
#>      x
#> 1 0301
#> 
#> 
#> storkombinasjoner
#> NULL
#> 
#> 
#> kapitler
#>     x
#> 1 KG3
#> 
#> 
#> omfang
#>   x
#> 1 B
#> 
#> 
#>  [ Kombinasjonsberegninger...] [ Lager kombinasjoner ...]
#>   regnskapsomfang kapittel
#> 1               B      KG3
yA <- BalanseRegnskapBeregningHierarki(inputdata, hierarki, regioner = "0301", omfang = "A", kapitler = "KG3")
#> 
#> data
#>     periode region kapittel regnskapsomfang  belop
#> 1      2016   0100       10               B 559923
#> 2      2016   0100       10           sbedr   3215
#> 305    2016   2022     9200               B    465
#> 306    2016   2022     9999               B  -7663
#> 
#> 
#> kapittelhierarki
#>     periode   to from sign
#> 1      2016  KG2   10    +
#> 2      2016  KG2   11    +
#> 119    2016 KG37  KG8    +
#> 120    2016 K580  580    +
#> 
#> 
#> kombinasjoner
#> NULL
#> 
#> 
#> regioner
#>      x
#> 1 0301
#> 
#> 
#> storkombinasjoner
#> NULL
#> 
#> 
#> kapitler
#>     x
#> 1 KG3
#> 
#> 
#> omfang
#>   x
#> 1 A
#> 
#> 
#>  [ Kombinasjonsberegninger...] [ Lager kombinasjoner ...]
yB <- BalanseRegnskapBeregningHierarki(inputdata, hierarki, regioner = "0301", omfang = "B", kapitler = "KG3")
#> 
#> data
#>     periode region kapittel regnskapsomfang  belop
#> 1      2016   0100       10               B 559923
#> 2      2016   0100       10           sbedr   3215
#> 305    2016   2022     9200               B    465
#> 306    2016   2022     9999               B  -7663
#> 
#> 
#> kapittelhierarki
#>     periode   to from sign
#> 1      2016  KG2   10    +
#> 2      2016  KG2   11    +
#> 119    2016 KG37  KG8    +
#> 120    2016 K580  580    +
#> 
#> 
#> kombinasjoner
#> NULL
#> 
#> 
#> regioner
#>      x
#> 1 0301
#> 
#> 
#> storkombinasjoner
#> NULL
#> 
#> 
#> kapitler
#>     x
#> 1 KG3
#> 
#> 
#> omfang
#>   x
#> 1 B
#> 
#> 
#>  [ Kombinasjonsberegninger...] [ Lager kombinasjoner ...]
```
