# Sørger for at regiontall blir character med 4 eller 6 plasser og ledende nuller

Input som ikke representerer tall forblir uendret, bortsett fra
eventuell omgjøring ved
[`as.character`](https://rdrr.io/r/base/character.html).

## Usage

``` r
FixRegionkode(region, warningText = NULL)
```

## Arguments

- region:

  Vektor med regionkoder. Kan være integer, numeric, character eller
  factor.

- warningText:

  Ved ikke-NULL skrives warning med warningText dersom endring gjøres.

## Value

Endrede regionkoder som en character vektor

## Details

Funksjonen kaller `AddLeadingZeros` med `places=6` for store tall
(`>9999`) og ellers med `places=4`.

## Note

Funksjonen er ekstra grundig dokumentert siden den brukes som eksempel i
workshop i bygging av r-pakker.

## See also

`Number`

## Author

Øyvind Langsrud

## Examples

``` r
FixRegionkode(c(101, 301, 30107))
#> [1] "0101"   "0301"   "030107"
FixRegionkode(c("101", "301", "30107"))
#> [1] "0101"   "0301"   "030107"
FixRegionkode(c("101", "301", "30107", "Oslo"))
#> [1] "0101"   "0301"   "030107" "Oslo"  
FixRegionkode(c("101", "301", "030107"), "Regionskoder endret")
#> Warning: Regionskoder endret: 0101, 0301
#> [1] "0101"   "0301"   "030107"
FixRegionkode(c("0101", "0301", "030107"), "Regionskoder endret")
#> [1] "0101"   "0301"   "030107"
```
