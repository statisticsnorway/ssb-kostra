# Aggregering av regioner basert på output fra [`KostraRegnskap`](https://statisticsnorway.github.io/ssb-kostra/reference/KostraRegnskap.md)

Funksjonen generaliserer summering ved
[`aggregate`](https://rdrr.io/r/stats/aggregate.html) til å bruke
hierarki istedenfor en variabel (kostragruppe) i datasettet

## Usage

``` r
KostraRegnskapRegionAggregering(
  data,
  hierarki,
  slettInput0 = FALSE,
  slettOutput0 = FALSE,
  fixRegionkode = TRUE,
  region = "region",
  belop = "belop",
  dimvar = c("regnskapsomfang", "kontoklasse", "art", "funksjon"),
  drop = TRUE,
  verbose = TRUE
)
```

## Arguments

- data:

  data frame med output fra KostraRegnskap

- hierarki:

  aggregeringshierarki for region

- slettInput0:

  Ved TRUE fjernes rader med belop=0 i input

- slettOutput0:

  Ved TRUE fjernes rader med belop=0 i output

- fixRegionkode:

  Ved TRUE (default): Sørger for blanke i starten/slutten fjernes og at
  regionkoder (from) i hierarki får 4 eller 6 plasser og ledende nuller
  (gir warning ved endring av input)

- region:

  Variabelnavn for region

- belop:

  Variabelnavn for belop

- dimvar:

  De andre klassifiseringsvariablene ("regnskapsomfang", "kontoklasse",
  "art", "funksjon")

- drop:

  Ved TRUE begrenses output for hvert aggregat til dimvar-kombinasjoner
  som har rader i inputdata som bidrar. Ved FALSE genereres alle
  kombinasjoner av alle dimvar-variabler. Ved NA vil output for alle
  aggregater ha alle dimvar-kombinasjoner som finnes i inputdata.

- verbose:

  Ved TRUE printes informasjon om fremdrift

## Value

En data frame

## Details

Parameteren `slettOutput0` har mening i tillegg til `slettInput0` dersom
det er negative fortegn i hierarki slik at 0-er kan oppnås ved
differanser.

## Note

Kjernefunksjonalitet i funksjonen kan på sikt ende opp som en ny
funksjon i pakka SSBtools

## Examples

``` r
hierarki <- Kostra:::Get_litehierarki()           # Ta bort Kostra::: dersom funksjonen er hentet med source(....
data22 <- Kostra:::Get_bev_basis_R_36()[1:22, ]   # Ta bort Kostra::: dersom funksjonen er hentet med source(....
KostraRegnskapRegionAggregering(data22, hierarki)
#>  [ Generering av input til HierarchyCompute ....--------.]
#>  [ HierarchyCompute initial calculations...] [ SelectionByMultiplication...] [ HierarchyCompute initial calculations...] [ SelectionByMultiplication...]
#>  [ output='data.frame'...]
#>    region regnskapsomfang kontoklasse  art funksjon belop
#> 1    EAFK               A           D AG35     FGF1 37907
#> 2    EAFK               A           I AG35     FGF1   -43
#> 3    EAFK               B           D AG35     FGF1 37907
#> 4    EAFK               B           I AG35     FGF1     0
#> 5  EAFK04               A           D AG35     FGF1 37907
#> 6  EAFK04               A           I AG35     FGF1   -43
#> 7  EAFK04               B           D AG35     FGF1 37907
#> 8  EAFK04               B           I AG35     FGF1     0
#> 9  EAFKUO               A           D AG35     FGF1 37907
#> 10 EAFKUO               A           I AG35     FGF1   -43
#> 11 EAFKUO               B           D AG35     FGF1 37907
#> 12 EAFKUO               B           I AG35     FGF1     0
#> 13    EAK               A           D AG35      FG1     0
#> 14    EAK               A           I AG35      FG1     0
#> 15  EAKUO               A           D AG35      FG1     0
#> 16  EAKUO               A           I AG35      FG1     0
#> 17  EKA50               A           D AG35      FG1     0
#> 18  EKA50               A           I AG35      FG1     0
#> 19  EKG03               A           D AG35      FG1     0
#> 20  EKG03               A           I AG35      FG1     0
#> 21  EKG14               A           D AG35      FG1     0
#> 22  EKG14               A           I AG35      FG1     0
KostraRegnskapRegionAggregering(data22, hierarki, drop = NA)
#>  [ Generering av input til HierarchyCompute ....
#> ]
#>  [ HierarchyCompute initial calculations...] [ KhatriRao...] [ HierarchyCompute initial calculations...] [ SelectionByMultiplication...]
#>  [ output='data.frame'...]
#>    region regnskapsomfang kontoklasse  art funksjon belop
#> 1    EAFK               A           D AG35     FGF1 37907
#> 2    EAFK               A           I AG35     FGF1   -43
#> 3    EAFK               A           D AG35      FG1     0
#> 4    EAFK               A           I AG35      FG1     0
#> 5    EAFK               B           D AG35     FGF1 37907
#> 6    EAFK               B           I AG35     FGF1     0
#> 7  EAFK04               A           D AG35     FGF1 37907
#> 8  EAFK04               A           I AG35     FGF1   -43
#> 9  EAFK04               A           D AG35      FG1     0
#> 10 EAFK04               A           I AG35      FG1     0
#> 11 EAFK04               B           D AG35     FGF1 37907
#> 12 EAFK04               B           I AG35     FGF1     0
#> 13 EAFKUO               A           D AG35     FGF1 37907
#> 14 EAFKUO               A           I AG35     FGF1   -43
#> 15 EAFKUO               A           D AG35      FG1     0
#> 16 EAFKUO               A           I AG35      FG1     0
#> 17 EAFKUO               B           D AG35     FGF1 37907
#> 18 EAFKUO               B           I AG35     FGF1     0
#> 19    EAK               A           D AG35     FGF1     0
#> 20    EAK               A           I AG35     FGF1     0
#> 21    EAK               A           D AG35      FG1     0
#> 22    EAK               A           I AG35      FG1     0
#> 23    EAK               B           D AG35     FGF1     0
#> 24    EAK               B           I AG35     FGF1     0
#> 25  EAKUO               A           D AG35     FGF1     0
#> 26  EAKUO               A           I AG35     FGF1     0
#> 27  EAKUO               A           D AG35      FG1     0
#> 28  EAKUO               A           I AG35      FG1     0
#> 29  EAKUO               B           D AG35     FGF1     0
#> 30  EAKUO               B           I AG35     FGF1     0
#> 31  EKA50               A           D AG35     FGF1     0
#> 32  EKA50               A           I AG35     FGF1     0
#> 33  EKA50               A           D AG35      FG1     0
#> 34  EKA50               A           I AG35      FG1     0
#> 35  EKA50               B           D AG35     FGF1     0
#> 36  EKA50               B           I AG35     FGF1     0
#> 37  EKG03               A           D AG35     FGF1     0
#> 38  EKG03               A           I AG35     FGF1     0
#> 39  EKG03               A           D AG35      FG1     0
#> 40  EKG03               A           I AG35      FG1     0
#> 41  EKG03               B           D AG35     FGF1     0
#> 42  EKG03               B           I AG35     FGF1     0
#> 43  EKG14               A           D AG35     FGF1     0
#> 44  EKG14               A           I AG35     FGF1     0
#> 45  EKG14               A           D AG35      FG1     0
#> 46  EKG14               A           I AG35      FG1     0
#> 47  EKG14               B           D AG35     FGF1     0
#> 48  EKG14               B           I AG35     FGF1     0
KostraRegnskapRegionAggregering(data22, hierarki, drop = FALSE)
#>  [ Generering av input til HierarchyCompute ...]
#>  [ HierarchyCompute initial calculations...] [ KhatriRao...] [ HierarchyCompute initial calculations...] [ KhatriRao...]
#>  [ output='data.frame'...]
#>    region regnskapsomfang kontoklasse  art funksjon belop
#> 1    EAFK               A           D AG35      FG1     0
#> 2    EAFK               B           D AG35      FG1     0
#> 3    EAFK               A           I AG35      FG1     0
#> 4    EAFK               B           I AG35      FG1     0
#> 5    EAFK               A           D AG35     FGF1 37907
#> 6    EAFK               B           D AG35     FGF1 37907
#> 7    EAFK               A           I AG35     FGF1   -43
#> 8    EAFK               B           I AG35     FGF1     0
#> 9  EAFK04               A           D AG35      FG1     0
#> 10 EAFK04               B           D AG35      FG1     0
#> 11 EAFK04               A           I AG35      FG1     0
#> 12 EAFK04               B           I AG35      FG1     0
#> 13 EAFK04               A           D AG35     FGF1 37907
#> 14 EAFK04               B           D AG35     FGF1 37907
#> 15 EAFK04               A           I AG35     FGF1   -43
#> 16 EAFK04               B           I AG35     FGF1     0
#> 17 EAFKUO               A           D AG35      FG1     0
#> 18 EAFKUO               B           D AG35      FG1     0
#> 19 EAFKUO               A           I AG35      FG1     0
#> 20 EAFKUO               B           I AG35      FG1     0
#> 21 EAFKUO               A           D AG35     FGF1 37907
#> 22 EAFKUO               B           D AG35     FGF1 37907
#> 23 EAFKUO               A           I AG35     FGF1   -43
#> 24 EAFKUO               B           I AG35     FGF1     0
#> 25    EAK               A           D AG35      FG1     0
#> 26    EAK               B           D AG35      FG1     0
#> 27    EAK               A           I AG35      FG1     0
#> 28    EAK               B           I AG35      FG1     0
#> 29    EAK               A           D AG35     FGF1     0
#> 30    EAK               B           D AG35     FGF1     0
#> 31    EAK               A           I AG35     FGF1     0
#> 32    EAK               B           I AG35     FGF1     0
#> 33  EAKUO               A           D AG35      FG1     0
#> 34  EAKUO               B           D AG35      FG1     0
#> 35  EAKUO               A           I AG35      FG1     0
#> 36  EAKUO               B           I AG35      FG1     0
#> 37  EAKUO               A           D AG35     FGF1     0
#> 38  EAKUO               B           D AG35     FGF1     0
#> 39  EAKUO               A           I AG35     FGF1     0
#> 40  EAKUO               B           I AG35     FGF1     0
#> 41  EKA50               A           D AG35      FG1     0
#> 42  EKA50               B           D AG35      FG1     0
#> 43  EKA50               A           I AG35      FG1     0
#> 44  EKA50               B           I AG35      FG1     0
#> 45  EKA50               A           D AG35     FGF1     0
#> 46  EKA50               B           D AG35     FGF1     0
#> 47  EKA50               A           I AG35     FGF1     0
#> 48  EKA50               B           I AG35     FGF1     0
#> 49  EKG03               A           D AG35      FG1     0
#> 50  EKG03               B           D AG35      FG1     0
#> 51  EKG03               A           I AG35      FG1     0
#> 52  EKG03               B           I AG35      FG1     0
#> 53  EKG03               A           D AG35     FGF1     0
#> 54  EKG03               B           D AG35     FGF1     0
#> 55  EKG03               A           I AG35     FGF1     0
#> 56  EKG03               B           I AG35     FGF1     0
#> 57  EKG14               A           D AG35      FG1     0
#> 58  EKG14               B           D AG35      FG1     0
#> 59  EKG14               A           I AG35      FG1     0
#> 60  EKG14               B           I AG35      FG1     0
#> 61  EKG14               A           D AG35     FGF1     0
#> 62  EKG14               B           D AG35     FGF1     0
#> 63  EKG14               A           I AG35     FGF1     0
#> 64  EKG14               B           I AG35     FGF1     0
KostraRegnskapRegionAggregering(data22, hierarki, slettInput0 = TRUE, drop = NA)
#>  [ Generering av input til HierarchyCompute ....
#> ]
#>  [ HierarchyCompute initial calculations...] [ KhatriRao...] [ HierarchyCompute initial calculations...] [ SelectionByMultiplication...]
#>  [ output='data.frame'...]
#>    region regnskapsomfang kontoklasse  art funksjon belop
#> 1    EAFK               A           D AG35     FGF1 37907
#> 2    EAFK               A           I AG35     FGF1   -43
#> 3    EAFK               B           D AG35     FGF1 37907
#> 4  EAFK04               A           D AG35     FGF1 37907
#> 5  EAFK04               A           I AG35     FGF1   -43
#> 6  EAFK04               B           D AG35     FGF1 37907
#> 7  EAFKUO               A           D AG35     FGF1 37907
#> 8  EAFKUO               A           I AG35     FGF1   -43
#> 9  EAFKUO               B           D AG35     FGF1 37907
#> 10    EAK               A           D AG35     FGF1     0
#> 11    EAK               A           I AG35     FGF1     0
#> 12    EAK               B           D AG35     FGF1     0
#> 13  EAKUO               A           D AG35     FGF1     0
#> 14  EAKUO               A           I AG35     FGF1     0
#> 15  EAKUO               B           D AG35     FGF1     0
#> 16  EKA50               A           D AG35     FGF1     0
#> 17  EKA50               A           I AG35     FGF1     0
#> 18  EKA50               B           D AG35     FGF1     0
#> 19  EKG03               A           D AG35     FGF1     0
#> 20  EKG03               A           I AG35     FGF1     0
#> 21  EKG03               B           D AG35     FGF1     0
#> 22  EKG14               A           D AG35     FGF1     0
#> 23  EKG14               A           I AG35     FGF1     0
#> 24  EKG14               B           D AG35     FGF1     0
```
