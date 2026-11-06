# UTDATERT FUNKSJON: KostraRegnskap

UTDATERT FUNKSJON: Bruk funksjoner i pakken på
github.com/statisticsnorway/ssb-kostraregnskap/

## Usage

``` r
KostraRegnskapBeregning(
  data,
  funksjonshierarki,
  artshierarki,
  data_saer,
  artshierarki_nettinger,
  artshierarki_nettinger_kasse,
  formler = NULL,
  funksjoner,
  arter,
  kontoklasser,
  ...,
  perioder = NULL,
  output
)

KostraRegnskapBeregningInput(...)

KostraRegnskapBeregningHierarkiALT(...)

KostraRegnskapBeregningHierarki(...)

KostraRegnskapBeregningHierarkiInput(...)

KostraRegnskap(
  data,
  funksjonshierarki,
  artshierarki,
  data_saer = NULL,
  artshierarki_nettinger = NULL,
  artshierarki_nettinger_kasse = NULL,
  stjernetabell = NULL,
  formler = NULL,
  ...,
  perioder = NULL
)

KostraRegnskapEnPeriode(
  data,
  funksjonshierarki,
  artshierarki,
  data_saer = NULL,
  artshierarki_nettinger = NULL,
  artshierarki_nettinger_kasse = NULL,
  kombinasjoner = NULL,
  regioner = NULL,
  storkombinasjoner = NULL,
  stjernetabell = NULL,
  funksjoner = NULL,
  arter = NULL,
  kontoklasser = NULL,
  regnskapsomfang = NULL,
  output = "en",
  printInfo = TRUE,
  printData = TRUE,
  lag0300 = FALSE,
  formler = NULL,
  autoNetting = TRUE,
  useMatrixToDataFrame = TRUE,
  returnFormelData = FALSE,
  smartB = TRUE,
  fixRegionkode = TRUE,
  fixArtkode = TRUE,
  fixFunksjonkode = TRUE,
  autoFormel = TRUE
)
```

## Arguments

- data:

  inputdata

- funksjonshierarki:

  funksjonshierarki

- artshierarki:

  artshierarki

- data_saer:

  inputdata for særbedrifter

- artshierarki_nettinger:

  artshierarki for særbedrift-nettinger

- artshierarki_nettinger_kasse:

  artshierarki for kasse-nettinger

- formler:

  data frame med formler som brukes til korreksjoner

- funksjoner:

  funksjoner som skal med i output. Man kan også bruke \*, ?, ! og –
  tilsvarende som for regioner (se nedenfor).

- arter:

  arter som skal med i output. Man kan også bruke \*, ?, ! og –
  tilsvarende som for regioner (se nedenfor).

- kontoklasser:

  kontoklasser som skal med i output. Det er mulig å bruke \* istedenfor
  NULL til å velge alle kontoklosser.

- ...:

  Parametere som sendes videre til KostraRegnskapEnPeriode

- perioder:

  Perioder for beregningen. Ved NULL (default) brukes alle perioder som
  finnes i data.

- output:

  "en" (default), "fire" (men det bør endres til "fem"...), "enFactor",
  "fireFactor" "storkombinasjoner", "hierarkier" eller "barePrintData".
  Ved "fire" blir output et datasett med fire beløpskolonner: kasse,
  særbedrift, nettinger, konsern. Variantene med Factor gir outputdata
  med factor istedenfor character. Ved "hierarkier" returneres utflatede
  hierarkier. Sign for nettinghierarkiet er i kolonnen netting. Dette
  hierarkiet er omskrevet til å ta vanlige data som input. Ved
  "barePrintData" stopper funksjonen etter at input er printet
  (printData=TRUE).

- stjernetabell:

  tabell med \* og ? som definerer koder som skal med i output

- kombinasjoner:

  kombinasjoner av art, funksjon, kontoklasse som skal med i output

- regioner:

  regioner som skal med i output. Kan også settet til et tall eller
  gjøre bruk av \*, ?, ! og – (se nedenfor).

- storkombinasjoner:

  kombinasjoner av art, funksjon, kontoklasse og region

- regnskapsomfang:

  regnskapsomfang som skal med i output. Det er mulig å bruke \* og ?
  istedenfor NULL til å velge begge regnskapsomfang.

- printInfo:

  Ved TRUE printes informasjon om generering av kombinasjoner fra
  stjernetabell samt informasjon om omskriving av formler.

- printData:

  Ved TRUE printes to første og to siste rader av alle inputdataene

- lag0300:

  Ved TRUE kopieres region 0301 til 0300 i inputdata

- autoNetting:

  Ved TRUE (default) utvides nettinghierarkiene automatisk basert på
  from-koden COPY

- useMatrixToDataFrame:

  Ved TRUE (default) brukes spesiell funksjonalitet (MatrixToDataFrame
  og DataFrameToMatrix) som sparer tid og minne.

- returnFormelData:

  Ved TRUE returneres spesielt datasett med informasjon om
  formelberegninger (denne parameteren vil bli tatt bort og erstattet av
  nye output koder som kan brukes)

- smartB:

  Ved TRUE (default) og ved valg av ett regnskapsomfang velges en metode
  som sparer tid og minne. Istedenfor å plukke ut regnskapsomfang til
  slutt gjøres kun de beregninger som trengs. Spesielt ved valg av
  regnskapsomfang B kan data_saer og/eller artshierarki_nettinger
  sløyfes.

- fixRegionkode:

  Ved TRUE (default): Sørger for blanke i starten/slutten fjernes og at
  regionkoder får 4 eller 6 plasser og ledende nuller (gir warning ved
  endring av input)

- fixArtkode:

  Ved TRUE (default): Sørger for blanke i starten/slutten fjernes og at
  artskoder som er tall får 3 plasser og ledende nuller (gir warning ved
  endring av input)

- fixFunksjonkode:

  Ved TRUE (default): Sørger for blanke i starten/slutten fjernes og at
  funksjonskoder som er tall får 3 plasser og ledende nuller (gir
  warning ved endring av input)

- autoFormel:

  Ved TRUE (default) kan formlene avhenge av hverandre. Ved TRUE kan
  formlene avhenge av hverandre. Formlene vil bli korrigert ved hjelp av
  funksjonen
  [`AutoFormel`](https://statisticsnorway.github.io/ssb-kostra/reference/AutoFormel.md).

## Value

En data frame

## Details

KostraRegnskap er en wrapper til funksjonen KostraRegnskapEnPeriode som,
ved flere perioder, blir kalt flere ganger. I output-kolonnen
regnskapsomfang betyr A konsern og B kasse.

Regioner kan settes til et tall. F.eks regioner=5 betyr at de 5 første
regionene i data velges. Det kan også gjøres utvalg fra regionene i data
ved bruk av \*, ?, ! og –. Her brukes ! til å invertere enkeltvalg, mens
– brukes til fjerning fra det samlede utvalget. Det betyr at bruk av –
tilsvarer sign i stjernetabell. Spesielt kan man velge alle fylker
(regioner = "??00"), alle bydeler (regioner = "??????") eller alle
kommuner (regioner = c("????","-??00")).

## Note

KostraRegnskapBeregningInput og KostraRegnskapBeregningHierarki går via
funksjonen KostraRegnskapBeregning. Parametere til den funksjonen trengs
bortsett fra «output». Spesielt må altså parameterne funksjoner, arter,
kontoklasser med. Det skal (i utgangspunktet) være en verdi på hver av
disse. Funksjonen KostraRegnskapBeregningHierarkiALT er alternativ til
KostraRegnskapBeregningHierarki med andre variabler i output.

## See also

`HierarchyCompute`, `HierarchicalWildcardGlobbing`

## Examples

``` r

 #==========================================================================
 #     Innlesing av data og omkoding
 #===========================================================================
# Når lesDataFraS er TRUE kjøres eksempelkoden omtrent slik den var under utvikling av koden.
# Ved FALSE brukes data som ligger i pakken der bare noen regioner er med, men det kjøres da
# på to år samtidig. Kommentarer i koden om beregningstid og antall rader vil ikke stemme.
lesDataFraS <- FALSE
if (!lesDataFraS){
  kData = KostraData("kostraRegnskapDataPen")       # KostraData("kostraRegnskapData") er alternativ der
  inputdata <- kData$data                           #    automatisk omkoding trengs (som fixRegionkode)
  funksjonshierarki <- kData$funksjonshierarki      #    Fungerer like bra, men med flere warning
  artshierarki <- kData$artshierarki
  inputdata_saer <- kData$data_saer
  artshierarki_nettinger <- kData$artshierarki_nettinger
  artshierarki_nettinger_kasse <- kData$artshierarki_nettinger_kasse
  stjerne <- kData$stjernetabell
  formler <- kData$formler
} else {

dataPath <- "S:/Prosjekt/2228 KOSTRA publisering/Metode/Regnskap/Matrise-klare filer"
#dataPath <- "C:\Users\oyl\Documents\work\Rworking\regnskap\Matrise-klare filer"

aar = "2016" # "2015" kan også velges

dataPath <- paste(dataPath,aar,sep="/")

funksjonFil     <- paste(dataPath,"/Funksjonshierarki_",aar,".csv",sep="")
artFil          <- paste(dataPath,"/Artshierarki_",aar,".csv",sep="")
artFilnett      <- paste(dataPath,"/Artshierarki_nettinger_s\U00E6rbedrift_",aar,".csv",sep="")
artFilnettKasse      <- paste(dataPath,"/Artshierarki_nettinger_kasse_",aar,".csv",sep="")

dataFil         <- paste(dataPath,"/Inputdata_fylker_kommuner_bydeler_",aar,".csv",sep="")
dataFilsaer     <- paste(dataPath,"/Inputdata_s\U00E6rbedrifter_",aar,".csv",sep="")

kombinasjonerFil <- paste(dataPath,"/../../Alle kombinasjoner vi trenger som output.csv",sep="")
stjerneFil       <- paste(dataPath,"/StjerneKombinasjoner_",aar,".csv",sep="")
formelFil       <- paste(dataPath,"/Formler_",aar,".csv",sep="")

funksjonshierarki      <- read.csv2(funksjonFil,colClasses=c("integer","character","character","character"))
artshierarki           <-  read.csv2(artFil,colClasses=c("integer","character","character","character"))
artshierarki_nettinger <-  read.csv2(artFilnett,colClasses=c("integer","character","character","character"))
artshierarki_nettinger_kasse <-  read.csv2(artFilnettKasse,colClasses=c("integer","character","character","character"))

inputdata              <-  read.csv2(dataFil,colClasses=c("integer","character","character","character","character","integer"))
inputdata_saer <-  read.csv2(dataFilsaer,colClasses=c("integer","character","character","character","character","integer"))

stjerne    <- read.csv2(stjerneFil,colClasses="character")
formler   <- read.csv2(formelFil,colClasses="character")

# Endrer navn i data fra fylkesregion til region
names(inputdata)[names(inputdata)=="fylkesregion"] <- "region"
names(inputdata_saer)[names(inputdata_saer)=="fylkesregion"] <- "region"
names(stjerne)[names(stjerne)=="fylkesregion"] <- "region"

# Leser kombinasjoner og omkoder
kombinasjoner <- read.csv2(kombinasjonerFil,colClasses="character")
kombinasjonerD  <- kombinasjoner$kontoklasse %in% c("1","3")
kombinasjoner$kontoklasse <- "I"
kombinasjoner$kontoklasse[kombinasjonerD] <- "D"
kombinasjoner <- unique(kombinasjoner) #  25312 kombinasjoner av funksjon, art og kontoklasse


# Legger til manglende ledende 0-er
inputdata$region = FixRegionkode(inputdata$region)
inputdata_saer$region = FixRegionkode(inputdata_saer$region)
}

 #==========================================================================
 #   Eksempler med:   Konsern  = Saebedrift + Nettinger + Kasse
 #===========================================================================

# Med stjernetabell. Denne gir 28215120 rader. [Beregningstid: 1 minutt]
# MERK: Denne kan krasje ved lite tilgjengelig minne som ved 32 bit versjon av R
z <- KostraRegnskap(inputdata, funksjonshierarki, artshierarki, inputdata_saer, artshierarki_nettinger, stjernetabell = stjerne)
#> ================================================================
#> ==============    periode =  2015     ================
#> ==============================================================
#> 
#> data
#>     periode region kontoklasse funksjon art belop
#> 1      2015   0300           D      120 380   847
#> 2      2015   0300           D      121 380   288
#> 291    2015 030107           D      100 090   157
#> 292    2015 030107           D      100 099   364
#> 
#> 
#> funksjonshierarki
#>     periode   to  from sign
#> 1      2015 FGF1 FGF1a    +
#> 2      2015 FGF1 FGF1b    +
#> 225    2015  FG5   321    -
#> 226    2015  FG5   711    -
#> 
#> 
#> artshierarki
#>     periode    to from sign
#> 1      2015  LONN  010    +
#> 2      2015  LONN  020    +
#> 627    2015 AI660  660    +
#> 628    2015 AD990  990    +
#> 
#> 
#> data_saer
#>    periode region kontoklasse funksjon art belop
#> 1     2015   0100           D      515 375   573
#> 2     2015   0301           D      515 375   573
#> 61    2015   0301           D      100 099   100
#> 62    2015   0101           D      110 099   108
#> 
#> 
#> artshierarki_nettinger
#>    periode    to                   from sign
#> 1     2015  AGD9 KJOP_SALG_OVERFORINGER    -
#> 2     2015 AGD10 KJOP_SALG_OVERFORINGER    -
#> 83    2015 AGI31 KJOP_SALG_OVERFORINGER    -
#> 84    2015 AGI31                 RENTER    -
#> 
#> 
#> artshierarki_nettinger_kasse
#> NULL
#> 
#> 
#> kombinasjoner
#> NULL
#> 
#> 
#> regioner
#> NULL
#> 
#> 
#> storkombinasjoner
#> NULL
#> 
#> 
#> stjernetabell
#>     periode region kontoklasse funksjon   art sign
#> 1      2015 ??????                 FG0*          +
#> 2      2015 ??????                 FG1*          +
#> 159    2015                        !FG2 AGI26    -
#> 160    2015                        !FG2 AGI19    -
#> 
#> 
#> funksjoner
#> NULL
#> 
#> 
#> arter
#> NULL
#> 
#> 
#> kontoklasser
#> NULL
#> 
#> 
#> regnskapsomfang
#> NULL
#> 
#> 
#> formler
#> NULL
#> 
#> Warning: artshierarki_nettinger_kasse er ikke i input. Alle kasse-nettinger blir 0.
#> 
#>  AutoNetting:  artshierarki_nettinger  fra  84  til  99  rader
#> 
#>  [ Kombinasjonsberegninger...
#> 
#>  region:funksjon    1236 elements
#>            region+funksjon    932 elements
#>            region-funksjon    696 elements
#> 
#> 
#>  kontoklasse:art    458 elements
#>            kontoklasse+art    398 elements
#>            kontoklasse-art    298 elements
#> 
#> 
#>  region:funksjon:kontoklasse:art    207408 elements
#>  [ RowGroups...]           funksjon-art    183756 elements
#>            region-art    178556 elements
#> ] [ Fra til storkombinasjoner til kombinasjoner og regioner ... RowGroups(...)]
#> 
#>                       Kasse : 
#>      Saerbedrift + Nettinger: 
#>                                [ output='data.frame'...........::...]
#> 
#> 
#> output 
#>        periode regnskapsomfang region funksjon kontoklasse  art belop
#> 1         2015               A   0300     FGF1           D AG35     0
#> 2         2015               A   0100     FGF1           D AG35     0
#> 357111    2015               B 030107      290           I  950     0
#> 357112    2015               B   0101      290           I  950     0
#> 
#> ================================================================
#> ==============    periode =  2016     ================
#> ==============================================================
#> 
#> data
#>     periode region kontoklasse funksjon art belop
#> 1      2016   0300           D      120 380  2312
#> 2      2016   0300           D      121 380   337
#> 338    2016 030101           D      100 099   178
#> 339    2016 030107           D      100 099   215
#> 
#> 
#> funksjonshierarki
#>     periode   to  from sign
#> 1      2016 FGF1 FGF1a    +
#> 2      2016 FGF1 FGF1b    +
#> 224    2016  FG5   321    -
#> 225    2016  FG5   711    -
#> 
#> 
#> artshierarki
#>     periode    to  from sign
#> 1      2016  LONN   010    +
#> 2      2016  LONN   020    +
#> 542    2016 AGD65 AGD46    -
#> 543    2016 AGD65  AG18    -
#> 
#> 
#> data_saer
#>     periode region kontoklasse funksjon art belop
#> 1      2016   0101           D      110 775  1729
#> 2      2016   0101           D      120 895   296
#> 119    2016   0101           D      110 090   106
#> 120    2016   0101           D      110 099   109
#> 
#> 
#> artshierarki_nettinger
#>    periode    to                   from sign
#> 1     2016  AGD9 KJOP_SALG_OVERFORINGER    -
#> 2     2016 AGD10 KJOP_SALG_OVERFORINGER    -
#> 93    2016 AGD65                   COPY    -
#> 94    2016 AGD22                   COPY    -
#> 
#> 
#> artshierarki_nettinger_kasse
#> NULL
#> 
#> 
#> kombinasjoner
#> NULL
#> 
#> 
#> regioner
#> NULL
#> 
#> 
#> storkombinasjoner
#> NULL
#> 
#> 
#> stjernetabell
#>     periode region kontoklasse funksjon   art sign
#> 1      2016 ??????                 FG0*          +
#> 2      2016 ??????                 FG1*          +
#> 162    2016                        !FG2 AGI26    -
#> 163    2016                        !FG2 AGI19    -
#> 
#> 
#> funksjoner
#> NULL
#> 
#> 
#> arter
#> NULL
#> 
#> 
#> kontoklasser
#> NULL
#> 
#> 
#> regnskapsomfang
#> NULL
#> 
#> 
#> formler
#> NULL
#> 
#> Warning: artshierarki_nettinger_kasse er ikke i input. Alle kasse-nettinger blir 0.
#> 
#>  AutoNetting:  artshierarki_nettinger  fra  94  til  113  rader
#> 
#>  [ Kombinasjonsberegninger...
#> 
#>  region:funksjon    1230 elements
#>            region+funksjon    926 elements
#>            region-funksjon    692 elements
#> 
#> 
#>  kontoklasse:art    418 elements
#>            kontoklasse+art    362 elements
#>            kontoklasse-art    274 elements
#> 
#> 
#>  region:funksjon:kontoklasse:art    189608 elements
#>  [ RowGroups...]           funksjon-art    164034 elements
#>            region-art    158858 elements
#> ] [ Fra til storkombinasjoner til kombinasjoner og regioner ... RowGroups(...)]
#> 
#>                       Kasse : 
#>      Saerbedrift + Nettinger: 
#>                                [ output='data.frame'...........::...]
#> 
#> 
#> output 
#>        periode regnskapsomfang region funksjon kontoklasse   art belop
#> 1         2016               A   0300     FGF1           D  AG35     0
#> 2         2016               A   0100     FGF1           D  AG35     0
#> 317715    2016               B   0100      FG2           I AGI10     0
#> 317716    2016               B   0101      FG2           I AGI10     0
#> 

if(!lesDataFraS)
   kombinasjoner <- unique(z[, c("art", "funksjon", "kontoklasse")])

rm(z) # sletter z frigjoer minne

# Med kombinasjoner. Denne gir 23388288 rader. [Beregningstid: 15 sek] (25312 kombinasjoner * 462 regioner * 2 regnskapsomfang)
z <- KostraRegnskap(inputdata, funksjonshierarki, artshierarki, inputdata_saer, artshierarki_nettinger, kombinasjoner = kombinasjoner)
#> ================================================================
#> ==============    periode =  2015     ================
#> ==============================================================
#> 
#> data
#>     periode region kontoklasse funksjon art belop
#> 1      2015   0300           D      120 380   847
#> 2      2015   0300           D      121 380   288
#> 291    2015 030107           D      100 090   157
#> 292    2015 030107           D      100 099   364
#> 
#> 
#> funksjonshierarki
#>     periode   to  from sign
#> 1      2015 FGF1 FGF1a    +
#> 2      2015 FGF1 FGF1b    +
#> 225    2015  FG5   321    -
#> 226    2015  FG5   711    -
#> 
#> 
#> artshierarki
#>     periode    to from sign
#> 1      2015  LONN  010    +
#> 2      2015  LONN  020    +
#> 627    2015 AI660  660    +
#> 628    2015 AD990  990    +
#> 
#> 
#> data_saer
#>    periode region kontoklasse funksjon art belop
#> 1     2015   0100           D      515 375   573
#> 2     2015   0301           D      515 375   573
#> 61    2015   0301           D      100 099   100
#> 62    2015   0101           D      110 099   108
#> 
#> 
#> artshierarki_nettinger
#>    periode    to                   from sign
#> 1     2015  AGD9 KJOP_SALG_OVERFORINGER    -
#> 2     2015 AGD10 KJOP_SALG_OVERFORINGER    -
#> 83    2015 AGI31 KJOP_SALG_OVERFORINGER    -
#> 84    2015 AGI31                 RENTER    -
#> 
#> 
#> artshierarki_nettinger_kasse
#> NULL
#> 
#> 
#> kombinasjoner
#>         art funksjon kontoklasse
#> 1      AG35     FGF1           D
#> 2      AG35    FGF1a           D
#> 53884 AGD64      290           D
#> 53885 AGI10      FG2           I
#> 
#> 
#> regioner
#> NULL
#> 
#> 
#> storkombinasjoner
#> NULL
#> 
#> 
#> stjernetabell
#> NULL
#> 
#> 
#> funksjoner
#> NULL
#> 
#> 
#> arter
#> NULL
#> 
#> 
#> kontoklasser
#> NULL
#> 
#> 
#> regnskapsomfang
#> NULL
#> 
#> 
#> formler
#> NULL
#> 
#> Warning: artshierarki_nettinger_kasse er ikke i input. Alle kasse-nettinger blir 0.
#> 
#>  AutoNetting:  artshierarki_nettinger  fra  84  til  99  rader
#> 
#>  [ Kombinasjonsberegninger...]
#> 
#>                       Kasse : 
#>      Saerbedrift + Nettinger: 
#>                                [ output='data.frame'.....]
#> 
#> 
#> output 
#>        periode regnskapsomfang region   art funksjon kontoklasse belop
#> 1         2015               A   0100  AG35     FGF1           D     0
#> 2         2015               A   0100  AG35    FGF1a           D     0
#> 646619    2015               B 030107 AGD64      290           D     0
#> 646620    2015               B 030107 AGI10      FG2           I     0
#> 
#> ================================================================
#> ==============    periode =  2016     ================
#> ==============================================================
#> 
#> data
#>     periode region kontoklasse funksjon art belop
#> 1      2016   0300           D      120 380  2312
#> 2      2016   0300           D      121 380   337
#> 338    2016 030101           D      100 099   178
#> 339    2016 030107           D      100 099   215
#> 
#> 
#> funksjonshierarki
#>     periode   to  from sign
#> 1      2016 FGF1 FGF1a    +
#> 2      2016 FGF1 FGF1b    +
#> 224    2016  FG5   321    -
#> 225    2016  FG5   711    -
#> 
#> 
#> artshierarki
#>     periode    to  from sign
#> 1      2016  LONN   010    +
#> 2      2016  LONN   020    +
#> 542    2016 AGD65 AGD46    -
#> 543    2016 AGD65  AG18    -
#> 
#> 
#> data_saer
#>     periode region kontoklasse funksjon art belop
#> 1      2016   0101           D      110 775  1729
#> 2      2016   0101           D      120 895   296
#> 119    2016   0101           D      110 090   106
#> 120    2016   0101           D      110 099   109
#> 
#> 
#> artshierarki_nettinger
#>    periode    to                   from sign
#> 1     2016  AGD9 KJOP_SALG_OVERFORINGER    -
#> 2     2016 AGD10 KJOP_SALG_OVERFORINGER    -
#> 93    2016 AGD65                   COPY    -
#> 94    2016 AGD22                   COPY    -
#> 
#> 
#> artshierarki_nettinger_kasse
#> NULL
#> 
#> 
#> kombinasjoner
#>         art funksjon kontoklasse
#> 1      AG35     FGF1           D
#> 2      AG35    FGF1a           D
#> 53884 AGD64      290           D
#> 53885 AGI10      FG2           I
#> 
#> 
#> regioner
#> NULL
#> 
#> 
#> storkombinasjoner
#> NULL
#> 
#> 
#> stjernetabell
#> NULL
#> 
#> 
#> funksjoner
#> NULL
#> 
#> 
#> arter
#> NULL
#> 
#> 
#> kontoklasser
#> NULL
#> 
#> 
#> regnskapsomfang
#> NULL
#> 
#> 
#> formler
#> NULL
#> 
#> Warning: artshierarki_nettinger_kasse er ikke i input. Alle kasse-nettinger blir 0.
#> 
#>  AutoNetting:  artshierarki_nettinger  fra  94  til  113  rader
#> 
#>  [ Kombinasjonsberegninger...]
#> 
#>                       Kasse : 
#>      Saerbedrift + Nettinger: 
#>                                [ output='data.frame'.....]
#> 
#> 
#> output 
#>        periode regnskapsomfang region   art funksjon kontoklasse belop
#> 1         2016               A   0100  AG35     FGF1           D     0
#> 2         2016               A   0100  AG35    FGF1a           D     0
#> 646619    2016               B 030107 AGD64      290           D     0
#> 646620    2016               B 030107 AGI10      FG2           I     0
#> 

# plukker ut 12 rader og fjerner radnavn
z12a <- z[z$region %in% c("0101", "0100", "0301") & z$funksjon %in% c("100", "FG2") & z$art %in% c("AGI6", "AGD10"), ]
rownames(z12a) <- NULL

rm(z) # sletter z frigjoer minne

# Samme svar kan beregnes mer direkte
z12b <- KostraRegnskap(inputdata, funksjonshierarki, artshierarki, inputdata_saer, artshierarki_nettinger, kombinasjoner = kombinasjoner, regioner = c("0101", "0100", "0301"), funksjoner = c("100", "FG2"), arter = c("AGI6", "AGD10"))
#> ================================================================
#> ==============    periode =  2015     ================
#> ==============================================================
#> 
#> data
#>     periode region kontoklasse funksjon art belop
#> 1      2015   0300           D      120 380   847
#> 2      2015   0300           D      121 380   288
#> 291    2015 030107           D      100 090   157
#> 292    2015 030107           D      100 099   364
#> 
#> 
#> funksjonshierarki
#>     periode   to  from sign
#> 1      2015 FGF1 FGF1a    +
#> 2      2015 FGF1 FGF1b    +
#> 225    2015  FG5   321    -
#> 226    2015  FG5   711    -
#> 
#> 
#> artshierarki
#>     periode    to from sign
#> 1      2015  LONN  010    +
#> 2      2015  LONN  020    +
#> 627    2015 AI660  660    +
#> 628    2015 AD990  990    +
#> 
#> 
#> data_saer
#>    periode region kontoklasse funksjon art belop
#> 1     2015   0100           D      515 375   573
#> 2     2015   0301           D      515 375   573
#> 61    2015   0301           D      100 099   100
#> 62    2015   0101           D      110 099   108
#> 
#> 
#> artshierarki_nettinger
#>    periode    to                   from sign
#> 1     2015  AGD9 KJOP_SALG_OVERFORINGER    -
#> 2     2015 AGD10 KJOP_SALG_OVERFORINGER    -
#> 83    2015 AGI31 KJOP_SALG_OVERFORINGER    -
#> 84    2015 AGI31                 RENTER    -
#> 
#> 
#> artshierarki_nettinger_kasse
#> NULL
#> 
#> 
#> kombinasjoner
#>         art funksjon kontoklasse
#> 1      AG35     FGF1           D
#> 2      AG35    FGF1a           D
#> 53884 AGD64      290           D
#> 53885 AGI10      FG2           I
#> 
#> 
#> regioner
#>      x
#> 1 0101
#> 2 0100
#> 3 0301
#> 
#> 
#> storkombinasjoner
#> NULL
#> 
#> 
#> stjernetabell
#> NULL
#> 
#> 
#> funksjoner
#>     x
#> 1 100
#> 2 FG2
#> 
#> 
#> arter
#>       x
#> 1  AGI6
#> 2 AGD10
#> 
#> 
#> kontoklasser
#> NULL
#> 
#> 
#> regnskapsomfang
#> NULL
#> 
#> 
#> formler
#> NULL
#> 
#> Warning: artshierarki_nettinger_kasse er ikke i input. Alle kasse-nettinger blir 0.
#> 
#>  AutoNetting:  artshierarki_nettinger  fra  84  til  99  rader
#> 
#>  [ Kombinasjonsberegninger...]
#> 
#>                       Kasse : 
#>      Saerbedrift + Nettinger: 
#>                                [ output='data.frame'.....]
#> 
#> 
#> output 
#>    periode regnskapsomfang region   art funksjon kontoklasse  belop
#> 1     2015               A   0100 AGD10      100           D      0
#> 2     2015               A   0100  AGI6      FG2           I      0
#> 11    2015               B   0301 AGD10      100           D 101103
#> 12    2015               B   0301  AGI6      FG2           I      0
#> 
#> ================================================================
#> ==============    periode =  2016     ================
#> ==============================================================
#> 
#> data
#>     periode region kontoklasse funksjon art belop
#> 1      2016   0300           D      120 380  2312
#> 2      2016   0300           D      121 380   337
#> 338    2016 030101           D      100 099   178
#> 339    2016 030107           D      100 099   215
#> 
#> 
#> funksjonshierarki
#>     periode   to  from sign
#> 1      2016 FGF1 FGF1a    +
#> 2      2016 FGF1 FGF1b    +
#> 224    2016  FG5   321    -
#> 225    2016  FG5   711    -
#> 
#> 
#> artshierarki
#>     periode    to  from sign
#> 1      2016  LONN   010    +
#> 2      2016  LONN   020    +
#> 542    2016 AGD65 AGD46    -
#> 543    2016 AGD65  AG18    -
#> 
#> 
#> data_saer
#>     periode region kontoklasse funksjon art belop
#> 1      2016   0101           D      110 775  1729
#> 2      2016   0101           D      120 895   296
#> 119    2016   0101           D      110 090   106
#> 120    2016   0101           D      110 099   109
#> 
#> 
#> artshierarki_nettinger
#>    periode    to                   from sign
#> 1     2016  AGD9 KJOP_SALG_OVERFORINGER    -
#> 2     2016 AGD10 KJOP_SALG_OVERFORINGER    -
#> 93    2016 AGD65                   COPY    -
#> 94    2016 AGD22                   COPY    -
#> 
#> 
#> artshierarki_nettinger_kasse
#> NULL
#> 
#> 
#> kombinasjoner
#>         art funksjon kontoklasse
#> 1      AG35     FGF1           D
#> 2      AG35    FGF1a           D
#> 53884 AGD64      290           D
#> 53885 AGI10      FG2           I
#> 
#> 
#> regioner
#>      x
#> 1 0101
#> 2 0100
#> 3 0301
#> 
#> 
#> storkombinasjoner
#> NULL
#> 
#> 
#> stjernetabell
#> NULL
#> 
#> 
#> funksjoner
#>     x
#> 1 100
#> 2 FG2
#> 
#> 
#> arter
#>       x
#> 1  AGI6
#> 2 AGD10
#> 
#> 
#> kontoklasser
#> NULL
#> 
#> 
#> regnskapsomfang
#> NULL
#> 
#> 
#> formler
#> NULL
#> 
#> Warning: artshierarki_nettinger_kasse er ikke i input. Alle kasse-nettinger blir 0.
#> 
#>  AutoNetting:  artshierarki_nettinger  fra  94  til  113  rader
#> 
#>  [ Kombinasjonsberegninger...]
#> 
#>                       Kasse : 
#>      Saerbedrift + Nettinger: 
#>                                [ output='data.frame'.....]
#> 
#> 
#> output 
#>    periode regnskapsomfang region   art funksjon kontoklasse belop
#> 1     2016               A   0100 AGD10      100           D     0
#> 2     2016               A   0100  AGI6      FG2           I     0
#> 11    2016               B   0301 AGD10      100           D 72810
#> 12    2016               B   0301  AGI6      FG2           I     0
#> 
identical(z12a, z12b)  # Gir TRUE
#> [1] TRUE


# Uten kombinasjoner i input blir det 48 rader i output (2 regnskapsomfang) X (3 regioner) X (2 funksjoner) X (2 arter) X (2 kontoklasser)
z48 <- KostraRegnskap(inputdata, funksjonshierarki, artshierarki, inputdata_saer, artshierarki_nettinger, regioner = c("0101", "0100", "0301"), funksjoner = c("100", "FG2"), arter = c("AGI6", "AGD10"))
#> ================================================================
#> ==============    periode =  2015     ================
#> ==============================================================
#> 
#> data
#>     periode region kontoklasse funksjon art belop
#> 1      2015   0300           D      120 380   847
#> 2      2015   0300           D      121 380   288
#> 291    2015 030107           D      100 090   157
#> 292    2015 030107           D      100 099   364
#> 
#> 
#> funksjonshierarki
#>     periode   to  from sign
#> 1      2015 FGF1 FGF1a    +
#> 2      2015 FGF1 FGF1b    +
#> 225    2015  FG5   321    -
#> 226    2015  FG5   711    -
#> 
#> 
#> artshierarki
#>     periode    to from sign
#> 1      2015  LONN  010    +
#> 2      2015  LONN  020    +
#> 627    2015 AI660  660    +
#> 628    2015 AD990  990    +
#> 
#> 
#> data_saer
#>    periode region kontoklasse funksjon art belop
#> 1     2015   0100           D      515 375   573
#> 2     2015   0301           D      515 375   573
#> 61    2015   0301           D      100 099   100
#> 62    2015   0101           D      110 099   108
#> 
#> 
#> artshierarki_nettinger
#>    periode    to                   from sign
#> 1     2015  AGD9 KJOP_SALG_OVERFORINGER    -
#> 2     2015 AGD10 KJOP_SALG_OVERFORINGER    -
#> 83    2015 AGI31 KJOP_SALG_OVERFORINGER    -
#> 84    2015 AGI31                 RENTER    -
#> 
#> 
#> artshierarki_nettinger_kasse
#> NULL
#> 
#> 
#> kombinasjoner
#> NULL
#> 
#> 
#> regioner
#>      x
#> 1 0101
#> 2 0100
#> 3 0301
#> 
#> 
#> storkombinasjoner
#> NULL
#> 
#> 
#> stjernetabell
#> NULL
#> 
#> 
#> funksjoner
#>     x
#> 1 100
#> 2 FG2
#> 
#> 
#> arter
#>       x
#> 1  AGI6
#> 2 AGD10
#> 
#> 
#> kontoklasser
#> NULL
#> 
#> 
#> regnskapsomfang
#> NULL
#> 
#> 
#> formler
#> NULL
#> 
#> Warning: artshierarki_nettinger_kasse er ikke i input. Alle kasse-nettinger blir 0.
#> 
#>  AutoNetting:  artshierarki_nettinger  fra  84  til  99  rader
#> 
#>  [ Kombinasjonsberegninger...] [ Lager kombinasjoner ...]
#> 
#>                       Kasse : 
#>      Saerbedrift + Nettinger: 
#>                                [ output='data.frame'.....]
#> 
#> 
#> output 
#>    periode regnskapsomfang region   art funksjon kontoklasse   belop
#> 1     2015               A   0100  AGI6      100           D       0
#> 2     2015               A   0100 AGD10      100           D       0
#> 47    2015               B   0301  AGI6      FG2           I       0
#> 48    2015               B   0301 AGD10      FG2           I -136365
#> 
#> ================================================================
#> ==============    periode =  2016     ================
#> ==============================================================
#> 
#> data
#>     periode region kontoklasse funksjon art belop
#> 1      2016   0300           D      120 380  2312
#> 2      2016   0300           D      121 380   337
#> 338    2016 030101           D      100 099   178
#> 339    2016 030107           D      100 099   215
#> 
#> 
#> funksjonshierarki
#>     periode   to  from sign
#> 1      2016 FGF1 FGF1a    +
#> 2      2016 FGF1 FGF1b    +
#> 224    2016  FG5   321    -
#> 225    2016  FG5   711    -
#> 
#> 
#> artshierarki
#>     periode    to  from sign
#> 1      2016  LONN   010    +
#> 2      2016  LONN   020    +
#> 542    2016 AGD65 AGD46    -
#> 543    2016 AGD65  AG18    -
#> 
#> 
#> data_saer
#>     periode region kontoklasse funksjon art belop
#> 1      2016   0101           D      110 775  1729
#> 2      2016   0101           D      120 895   296
#> 119    2016   0101           D      110 090   106
#> 120    2016   0101           D      110 099   109
#> 
#> 
#> artshierarki_nettinger
#>    periode    to                   from sign
#> 1     2016  AGD9 KJOP_SALG_OVERFORINGER    -
#> 2     2016 AGD10 KJOP_SALG_OVERFORINGER    -
#> 93    2016 AGD65                   COPY    -
#> 94    2016 AGD22                   COPY    -
#> 
#> 
#> artshierarki_nettinger_kasse
#> NULL
#> 
#> 
#> kombinasjoner
#> NULL
#> 
#> 
#> regioner
#>      x
#> 1 0101
#> 2 0100
#> 3 0301
#> 
#> 
#> storkombinasjoner
#> NULL
#> 
#> 
#> stjernetabell
#> NULL
#> 
#> 
#> funksjoner
#>     x
#> 1 100
#> 2 FG2
#> 
#> 
#> arter
#>       x
#> 1  AGI6
#> 2 AGD10
#> 
#> 
#> kontoklasser
#> NULL
#> 
#> 
#> regnskapsomfang
#> NULL
#> 
#> 
#> formler
#> NULL
#> 
#> Warning: artshierarki_nettinger_kasse er ikke i input. Alle kasse-nettinger blir 0.
#> 
#>  AutoNetting:  artshierarki_nettinger  fra  94  til  113  rader
#> 
#>  [ Kombinasjonsberegninger...] [ Lager kombinasjoner ...]
#> 
#>                       Kasse : 
#>      Saerbedrift + Nettinger: 
#>                                [ output='data.frame'.....]
#> 
#> 
#> output 
#>    periode regnskapsomfang region   art funksjon kontoklasse  belop
#> 1     2016               A   0100  AGI6      100           D      0
#> 2     2016               A   0100 AGD10      100           D      0
#> 47    2016               B   0301  AGI6      FG2           I      0
#> 48    2016               B   0301 AGD10      FG2           I -76984
#> 

# Varianten med factor-output krever mindre minne. Denne forandringen gir ingen forskjell for resultat skrevet til csv-fil.
z <- KostraRegnskap(inputdata, funksjonshierarki, artshierarki, inputdata_saer, artshierarki_nettinger, stjernetabell = stjerne, output = "enFactor")
#> ================================================================
#> ==============    periode =  2015     ================
#> ==============================================================
#> 
#> data
#>     periode region kontoklasse funksjon art belop
#> 1      2015   0300           D      120 380   847
#> 2      2015   0300           D      121 380   288
#> 291    2015 030107           D      100 090   157
#> 292    2015 030107           D      100 099   364
#> 
#> 
#> funksjonshierarki
#>     periode   to  from sign
#> 1      2015 FGF1 FGF1a    +
#> 2      2015 FGF1 FGF1b    +
#> 225    2015  FG5   321    -
#> 226    2015  FG5   711    -
#> 
#> 
#> artshierarki
#>     periode    to from sign
#> 1      2015  LONN  010    +
#> 2      2015  LONN  020    +
#> 627    2015 AI660  660    +
#> 628    2015 AD990  990    +
#> 
#> 
#> data_saer
#>    periode region kontoklasse funksjon art belop
#> 1     2015   0100           D      515 375   573
#> 2     2015   0301           D      515 375   573
#> 61    2015   0301           D      100 099   100
#> 62    2015   0101           D      110 099   108
#> 
#> 
#> artshierarki_nettinger
#>    periode    to                   from sign
#> 1     2015  AGD9 KJOP_SALG_OVERFORINGER    -
#> 2     2015 AGD10 KJOP_SALG_OVERFORINGER    -
#> 83    2015 AGI31 KJOP_SALG_OVERFORINGER    -
#> 84    2015 AGI31                 RENTER    -
#> 
#> 
#> artshierarki_nettinger_kasse
#> NULL
#> 
#> 
#> kombinasjoner
#> NULL
#> 
#> 
#> regioner
#> NULL
#> 
#> 
#> storkombinasjoner
#> NULL
#> 
#> 
#> stjernetabell
#>     periode region kontoklasse funksjon   art sign
#> 1      2015 ??????                 FG0*          +
#> 2      2015 ??????                 FG1*          +
#> 159    2015                        !FG2 AGI26    -
#> 160    2015                        !FG2 AGI19    -
#> 
#> 
#> funksjoner
#> NULL
#> 
#> 
#> arter
#> NULL
#> 
#> 
#> kontoklasser
#> NULL
#> 
#> 
#> regnskapsomfang
#> NULL
#> 
#> 
#> formler
#> NULL
#> 
#> Warning: artshierarki_nettinger_kasse er ikke i input. Alle kasse-nettinger blir 0.
#> 
#>  AutoNetting:  artshierarki_nettinger  fra  84  til  99  rader
#> 
#>  [ Kombinasjonsberegninger...
#> 
#>  region:funksjon    1236 elements
#>            region+funksjon    932 elements
#>            region-funksjon    696 elements
#> 
#> 
#>  kontoklasse:art    458 elements
#>            kontoklasse+art    398 elements
#>            kontoklasse-art    298 elements
#> 
#> 
#>  region:funksjon:kontoklasse:art    207408 elements
#>  [ RowGroups...]           funksjon-art    183756 elements
#>            region-art    178556 elements
#> ] [ Fra til storkombinasjoner til kombinasjoner og regioner ... RowGroups(...)]
#> 
#>                       Kasse : 
#>      Saerbedrift + Nettinger: 
#>                                [ output='data.frame'...........::...]
#> 
#> 
#> output 
#>        periode regnskapsomfang region funksjon kontoklasse  art belop
#> 1         2015               A   0300     FGF1           D AG35     0
#> 2         2015               A   0100     FGF1           D AG35     0
#> 357111    2015               B 030107      290           I  950     0
#> 357112    2015               B   0101      290           I  950     0
#> 
#> ================================================================
#> ==============    periode =  2016     ================
#> ==============================================================
#> 
#> data
#>     periode region kontoklasse funksjon art belop
#> 1      2016   0300           D      120 380  2312
#> 2      2016   0300           D      121 380   337
#> 338    2016 030101           D      100 099   178
#> 339    2016 030107           D      100 099   215
#> 
#> 
#> funksjonshierarki
#>     periode   to  from sign
#> 1      2016 FGF1 FGF1a    +
#> 2      2016 FGF1 FGF1b    +
#> 224    2016  FG5   321    -
#> 225    2016  FG5   711    -
#> 
#> 
#> artshierarki
#>     periode    to  from sign
#> 1      2016  LONN   010    +
#> 2      2016  LONN   020    +
#> 542    2016 AGD65 AGD46    -
#> 543    2016 AGD65  AG18    -
#> 
#> 
#> data_saer
#>     periode region kontoklasse funksjon art belop
#> 1      2016   0101           D      110 775  1729
#> 2      2016   0101           D      120 895   296
#> 119    2016   0101           D      110 090   106
#> 120    2016   0101           D      110 099   109
#> 
#> 
#> artshierarki_nettinger
#>    periode    to                   from sign
#> 1     2016  AGD9 KJOP_SALG_OVERFORINGER    -
#> 2     2016 AGD10 KJOP_SALG_OVERFORINGER    -
#> 93    2016 AGD65                   COPY    -
#> 94    2016 AGD22                   COPY    -
#> 
#> 
#> artshierarki_nettinger_kasse
#> NULL
#> 
#> 
#> kombinasjoner
#> NULL
#> 
#> 
#> regioner
#> NULL
#> 
#> 
#> storkombinasjoner
#> NULL
#> 
#> 
#> stjernetabell
#>     periode region kontoklasse funksjon   art sign
#> 1      2016 ??????                 FG0*          +
#> 2      2016 ??????                 FG1*          +
#> 162    2016                        !FG2 AGI26    -
#> 163    2016                        !FG2 AGI19    -
#> 
#> 
#> funksjoner
#> NULL
#> 
#> 
#> arter
#> NULL
#> 
#> 
#> kontoklasser
#> NULL
#> 
#> 
#> regnskapsomfang
#> NULL
#> 
#> 
#> formler
#> NULL
#> 
#> Warning: artshierarki_nettinger_kasse er ikke i input. Alle kasse-nettinger blir 0.
#> 
#>  AutoNetting:  artshierarki_nettinger  fra  94  til  113  rader
#> 
#>  [ Kombinasjonsberegninger...
#> 
#>  region:funksjon    1230 elements
#>            region+funksjon    926 elements
#>            region-funksjon    692 elements
#> 
#> 
#>  kontoklasse:art    418 elements
#>            kontoklasse+art    362 elements
#>            kontoklasse-art    274 elements
#> 
#> 
#>  region:funksjon:kontoklasse:art    189608 elements
#>  [ RowGroups...]           funksjon-art    164034 elements
#>            region-art    158858 elements
#> ] [ Fra til storkombinasjoner til kombinasjoner og regioner ... RowGroups(...)]
#> 
#>                       Kasse : 
#>      Saerbedrift + Nettinger: 
#>                                [ output='data.frame'...........::...]
#> 
#> 
#> output 
#>        periode regnskapsomfang region funksjon kontoklasse   art belop
#> 1         2016               A   0300     FGF1           D  AG35     0
#> 2         2016               A   0100     FGF1           D  AG35     0
#> 317715    2016               B   0100      FG2           I AGI10     0
#> 317716    2016               B   0101      FG2           I AGI10     0
#> 

# plukker ut 24 rader og fjerner radnavn
z24a <- z[z$region %in% c("0101", "0100", "0301") & z$funksjon %in% c("100", "FG2") & z$art %in% c("AGI6", "AGD10"), ]
rownames(z24a) <- NULL

# Samme svar kan beregnes mer direkte [Beregningstid: 1 sekund]
z24b <- KostraRegnskap(inputdata, funksjonshierarki, artshierarki, inputdata_saer, artshierarki_nettinger, stjernetabell = stjerne, regioner = c("0101", "0100", "0301"), funksjoner = c("100", "FG2"), arter = c("AGI6", "AGD10"))
#> ================================================================
#> ==============    periode =  2015     ================
#> ==============================================================
#> 
#> data
#>     periode region kontoklasse funksjon art belop
#> 1      2015   0300           D      120 380   847
#> 2      2015   0300           D      121 380   288
#> 291    2015 030107           D      100 090   157
#> 292    2015 030107           D      100 099   364
#> 
#> 
#> funksjonshierarki
#>     periode   to  from sign
#> 1      2015 FGF1 FGF1a    +
#> 2      2015 FGF1 FGF1b    +
#> 225    2015  FG5   321    -
#> 226    2015  FG5   711    -
#> 
#> 
#> artshierarki
#>     periode    to from sign
#> 1      2015  LONN  010    +
#> 2      2015  LONN  020    +
#> 627    2015 AI660  660    +
#> 628    2015 AD990  990    +
#> 
#> 
#> data_saer
#>    periode region kontoklasse funksjon art belop
#> 1     2015   0100           D      515 375   573
#> 2     2015   0301           D      515 375   573
#> 61    2015   0301           D      100 099   100
#> 62    2015   0101           D      110 099   108
#> 
#> 
#> artshierarki_nettinger
#>    periode    to                   from sign
#> 1     2015  AGD9 KJOP_SALG_OVERFORINGER    -
#> 2     2015 AGD10 KJOP_SALG_OVERFORINGER    -
#> 83    2015 AGI31 KJOP_SALG_OVERFORINGER    -
#> 84    2015 AGI31                 RENTER    -
#> 
#> 
#> artshierarki_nettinger_kasse
#> NULL
#> 
#> 
#> kombinasjoner
#> NULL
#> 
#> 
#> regioner
#>      x
#> 1 0101
#> 2 0100
#> 3 0301
#> 
#> 
#> storkombinasjoner
#> NULL
#> 
#> 
#> stjernetabell
#>     periode region kontoklasse funksjon   art sign
#> 1      2015 ??????                 FG0*          +
#> 2      2015 ??????                 FG1*          +
#> 159    2015                        !FG2 AGI26    -
#> 160    2015                        !FG2 AGI19    -
#> 
#> 
#> funksjoner
#>     x
#> 1 100
#> 2 FG2
#> 
#> 
#> arter
#>       x
#> 1  AGI6
#> 2 AGD10
#> 
#> 
#> kontoklasser
#> NULL
#> 
#> 
#> regnskapsomfang
#> NULL
#> 
#> 
#> formler
#> NULL
#> 
#> Warning: artshierarki_nettinger_kasse er ikke i input. Alle kasse-nettinger blir 0.
#> 
#>  AutoNetting:  artshierarki_nettinger  fra  84  til  99  rader
#> 
#>  [ Kombinasjonsberegninger...
#> 
#>  region:funksjon    6 elements
#>            region+funksjon    6 elements
#>            region-funksjon    5 elements
#> 
#> 
#>  kontoklasse:art    4 elements
#>            kontoklasse+art    4 elements
#>            kontoklasse-art    2 elements
#> 
#> 
#>  region:funksjon:kontoklasse:art    10 elements
#>  [ RowGroups...]           funksjon-art    5 elements
#>            region-art    5 elements
#> ] [ Fra til storkombinasjoner til kombinasjoner og regioner ... RowGroups(...)]
#> 
#>                       Kasse : 
#>      Saerbedrift + Nettinger: 
#>                                [ output='data.frame'...........::...]
#> 
#> 
#> output 
#>    periode regnskapsomfang region funksjon kontoklasse   art  belop
#> 1     2015               A   0101      FG2           I  AGI6      0
#> 2     2015               A   0100      FG2           I  AGI6      0
#> 9     2015               B   0101      100           D AGD10   5116
#> 10    2015               B   0301      100           D AGD10 101103
#> 
#> ================================================================
#> ==============    periode =  2016     ================
#> ==============================================================
#> 
#> data
#>     periode region kontoklasse funksjon art belop
#> 1      2016   0300           D      120 380  2312
#> 2      2016   0300           D      121 380   337
#> 338    2016 030101           D      100 099   178
#> 339    2016 030107           D      100 099   215
#> 
#> 
#> funksjonshierarki
#>     periode   to  from sign
#> 1      2016 FGF1 FGF1a    +
#> 2      2016 FGF1 FGF1b    +
#> 224    2016  FG5   321    -
#> 225    2016  FG5   711    -
#> 
#> 
#> artshierarki
#>     periode    to  from sign
#> 1      2016  LONN   010    +
#> 2      2016  LONN   020    +
#> 542    2016 AGD65 AGD46    -
#> 543    2016 AGD65  AG18    -
#> 
#> 
#> data_saer
#>     periode region kontoklasse funksjon art belop
#> 1      2016   0101           D      110 775  1729
#> 2      2016   0101           D      120 895   296
#> 119    2016   0101           D      110 090   106
#> 120    2016   0101           D      110 099   109
#> 
#> 
#> artshierarki_nettinger
#>    periode    to                   from sign
#> 1     2016  AGD9 KJOP_SALG_OVERFORINGER    -
#> 2     2016 AGD10 KJOP_SALG_OVERFORINGER    -
#> 93    2016 AGD65                   COPY    -
#> 94    2016 AGD22                   COPY    -
#> 
#> 
#> artshierarki_nettinger_kasse
#> NULL
#> 
#> 
#> kombinasjoner
#> NULL
#> 
#> 
#> regioner
#>      x
#> 1 0101
#> 2 0100
#> 3 0301
#> 
#> 
#> storkombinasjoner
#> NULL
#> 
#> 
#> stjernetabell
#>     periode region kontoklasse funksjon   art sign
#> 1      2016 ??????                 FG0*          +
#> 2      2016 ??????                 FG1*          +
#> 162    2016                        !FG2 AGI26    -
#> 163    2016                        !FG2 AGI19    -
#> 
#> 
#> funksjoner
#>     x
#> 1 100
#> 2 FG2
#> 
#> 
#> arter
#>       x
#> 1  AGI6
#> 2 AGD10
#> 
#> 
#> kontoklasser
#> NULL
#> 
#> 
#> regnskapsomfang
#> NULL
#> 
#> 
#> formler
#> NULL
#> 
#> Warning: artshierarki_nettinger_kasse er ikke i input. Alle kasse-nettinger blir 0.
#> 
#>  AutoNetting:  artshierarki_nettinger  fra  94  til  113  rader
#> 
#>  [ Kombinasjonsberegninger...
#> 
#>  region:funksjon    6 elements
#>            region+funksjon    6 elements
#>            region-funksjon    5 elements
#> 
#> 
#>  kontoklasse:art    4 elements
#>            kontoklasse+art    4 elements
#>            kontoklasse-art    2 elements
#> 
#> 
#>  region:funksjon:kontoklasse:art    10 elements
#>  [ RowGroups...]           funksjon-art    5 elements
#>            region-art    5 elements
#> ] [ Fra til storkombinasjoner til kombinasjoner og regioner ... RowGroups(...)]
#> 
#>                       Kasse : 
#>      Saerbedrift + Nettinger: 
#>                                [ output='data.frame'...........::...]
#> 
#> 
#> output 
#>    periode regnskapsomfang region funksjon kontoklasse   art belop
#> 1     2016               A   0101      FG2           I  AGI6     0
#> 2     2016               A   0100      FG2           I  AGI6     0
#> 9     2016               B   0101      100           D AGD10  5769
#> 10    2016               B   0301      100           D AGD10 72810
#> 
# z24a og z24b er ikke identiske pga ulik ordning av rader

rm(z) # sletter z frigjoer minne

# Med stjernetabell og formler. Bare Oslo og Akershus, funksjon FG1 FG2 FG4 og arter som starter med AGD.
z <- KostraRegnskap(inputdata, funksjonshierarki, artshierarki, inputdata_saer, artshierarki_nettinger, stjernetabell = stjerne, regioner = c("02??","03??") , formler = formler, funksjoner = "FG?", arter = "AGD*")
#> ================================================================
#> ==============    periode =  2015     ================
#> ==============================================================
#> 
#> data
#>     periode region kontoklasse funksjon art belop
#> 1      2015   0300           D      120 380   847
#> 2      2015   0300           D      121 380   288
#> 291    2015 030107           D      100 090   157
#> 292    2015 030107           D      100 099   364
#> 
#> 
#> funksjonshierarki
#>     periode   to  from sign
#> 1      2015 FGF1 FGF1a    +
#> 2      2015 FGF1 FGF1b    +
#> 225    2015  FG5   321    -
#> 226    2015  FG5   711    -
#> 
#> 
#> artshierarki
#>     periode    to from sign
#> 1      2015  LONN  010    +
#> 2      2015  LONN  020    +
#> 627    2015 AI660  660    +
#> 628    2015 AD990  990    +
#> 
#> 
#> data_saer
#>    periode region kontoklasse funksjon art belop
#> 1     2015   0100           D      515 375   573
#> 2     2015   0301           D      515 375   573
#> 61    2015   0301           D      100 099   100
#> 62    2015   0101           D      110 099   108
#> 
#> 
#> artshierarki_nettinger
#>    periode    to                   from sign
#> 1     2015  AGD9 KJOP_SALG_OVERFORINGER    -
#> 2     2015 AGD10 KJOP_SALG_OVERFORINGER    -
#> 83    2015 AGI31 KJOP_SALG_OVERFORINGER    -
#> 84    2015 AGI31                 RENTER    -
#> 
#> 
#> artshierarki_nettinger_kasse
#> NULL
#> 
#> 
#> kombinasjoner
#> NULL
#> 
#> 
#> regioner
#>      x
#> 1 02??
#> 2 03??
#> 
#> 
#> storkombinasjoner
#> NULL
#> 
#> 
#> stjernetabell
#>     periode region kontoklasse funksjon   art sign
#> 1      2015 ??????                 FG0*          +
#> 2      2015 ??????                 FG1*          +
#> 159    2015                        !FG2 AGI26    -
#> 160    2015                        !FG2 AGI19    -
#> 
#> 
#> funksjoner
#>     x
#> 1 FG?
#> 
#> 
#> arter
#>      x
#> 1 AGD*
#> 
#> 
#> kontoklasser
#> NULL
#> 
#> 
#> regnskapsomfang
#> NULL
#> 
#>    periode
#> 1  2015   
#> 2  2015   
#> 3  2015   
#> 4  2015   
#> 5  2015   
#> 6  2015   
#> 7  2015   
#> 8  2015   
#> 9  2015   
#> 10 2015   
#> 11 2015   
#> 12 2015   
#> 13 2015   
#> 14 2015   
#>    formel                                                                     
#> 1  I:FG2:AGI2 = I:FG2:AGI2 - I:FG1:AGI1                                       
#> 2  FG2:AGD12 = FG5:AGD12 - 800:EIENDOMSSKATT_OG__ANDRE_SKATTER                
#> 3  FG2:AG11 = 840:AG11 - 840:AG12 + 800:AG12                                  
#> 4  D:FG2:AGID1 = D:FG2:AGID1 - D:FG2:990 + D:FG2:AGI3 + D:860:990 - I:FG2:AGI3
#> 5  D:FG2:AGID2 = D:FG2:AGID2 + I:FG2:AGID2                                    
#> 6  I:FG2:AGI21 = I:FG4:AGI1 - I:FG2:AGI17                                     
#> 7  D:FG2:AGD45 = D:FG2:AGD45 - D:FG2:A800 + D:840:A800                        
#> 8  FG5:AGD62 = FG5:AGD62 - 800:EIENDOMSSKATT_OG__ANDRE_SKATTER                
#> 9  D:FG2:AGD65 = D:FG2:AGD45 - D:FG2:AGD46 - D:FG2:AG18                       
#> 10 D:FG2:AGD22 = D:FG2:AGD30 - D:FG2:AGD31 - D:FG2:AG27                       
#> 11 D:FG2:AGD66 = D:FG2:AGD65 +  D:FG2:AGD22 + D:FG2:990                       
#> 12 D:FG2:AGD32 = D:FG2:AGD66 + D:FG2:AG14 - D:FG2:AG7                         
#> 13 I:FG2:AGI31 = I:FG2:AGI31 - I:FG1:AGI1                                     
#> 14 I:FG2:AGI16 = I:FG2:AGI20 + I:FG2:AGI15 - I:FG2:AGI14 + I:FG2:AG18         
#> 
#>  
#>  --- Kjører AutoFormel ----
#> 
#>  
#>  --- Etter AutoFormel ----
#>    periode
#> 1  2015   
#> 2  2015   
#> 3  2015   
#> 4  2015   
#> 5  2015   
#> 6  2015   
#> 7  2015   
#> 8  2015   
#> 9  2015   
#> 10 2015   
#> 11 2015   
#> 12 2015   
#> 13 2015   
#> 14 2015   
#> 15 2015   
#> 16 2015   
#> 17 2015   
#>    formel                                                                                                                                                                       
#> 1  I:FG2:AGI2 = I:FG2:AGI2 - I:FG1:AGI1                                                                                                                                         
#> 2  D:FG2:AGD12 = D:FG5:AGD12 - D:800:EIENDOMSSKATT_OG__ANDRE_SKATTER                                                                                                            
#> 3  D:FG2:AG11 = D:840:AG11 - D:840:AG12 + D:800:AG12                                                                                                                            
#> 4  D:FG2:AGID1 = D:FG2:AGID1 - D:FG2:990 + D:FG2:AGI3 + D:860:990 - I:FG2:AGI3                                                                                                  
#> 5  D:FG2:AGID2 = D:FG2:AGID2 + I:FG2:AGID2                                                                                                                                      
#> 6  I:FG2:AGI21 = I:FG4:AGI1 - I:FG2:AGI17                                                                                                                                       
#> 7  D:FG2:AGD45 = D:FG2:AGD45 - D:FG2:A800 + D:840:A800                                                                                                                          
#> 8  D:FG5:AGD62 = D:FG5:AGD62 - D:800:EIENDOMSSKATT_OG__ANDRE_SKATTER                                                                                                            
#> 9  D:FG2:AGD65 = ( D:FG2:AGD45 - D:FG2:A800 + D:840:A800 ) - D:FG2:AGD46 - D:FG2:AG18                                                                                           
#> 10 D:FG2:AGD22 = D:FG2:AGD30 - D:FG2:AGD31 - D:FG2:AG27                                                                                                                         
#> 11 D:FG2:AGD66 = ( ( D:FG2:AGD45 - D:FG2:A800 + D:840:A800 ) - D:FG2:AGD46 - D:FG2:AG18 ) +  ( D:FG2:AGD30 - D:FG2:AGD31 - D:FG2:AG27 ) + D:FG2:990                             
#> 12 D:FG2:AGD32 = ( ( ( D:FG2:AGD45 - D:FG2:A800 + D:840:A800 ) - D:FG2:AGD46 - D:FG2:AG18 ) +  ( D:FG2:AGD30 - D:FG2:AGD31 - D:FG2:AG27 ) + D:FG2:990 ) + D:FG2:AG14 - D:FG2:AG7
#> 13 I:FG2:AGI31 = I:FG2:AGI31 - I:FG1:AGI1                                                                                                                                       
#> 14 I:FG2:AGI16 = I:FG2:AGI20 + I:FG2:AGI15 - I:FG2:AGI14 + I:FG2:AG18                                                                                                           
#> 15 I:FG2:AGD12 = I:FG5:AGD12 - I:800:EIENDOMSSKATT_OG__ANDRE_SKATTER                                                                                                            
#> 16 I:FG2:AG11 = I:840:AG11 - I:840:AG12 + I:800:AG12                                                                                                                            
#> 17 I:FG5:AGD62 = I:FG5:AGD62 - I:800:EIENDOMSSKATT_OG__ANDRE_SKATTER                                                                                                            
#> Warning: artshierarki_nettinger_kasse er ikke i input. Alle kasse-nettinger blir 0.
#> 
#>  AutoNetting:  artshierarki_nettinger  fra  84  til  99  rader
#> 
#>  [ Kombinasjonsberegninger...
#> 
#>  region:funksjon    8 elements
#>            region+funksjon    8 elements
#>            region-funksjon    8 elements
#> 
#> 
#>  kontoklasse:art    136 elements
#>            kontoklasse+art    136 elements
#>            kontoklasse-art    68 elements
#> 
#> 
#>  region:funksjon:kontoklasse:art    544 elements
#>  [ RowGroups...]           funksjon-art    410 elements
#>            region-art    410 elements
#> ] [ Fra til storkombinasjoner til kombinasjoner og regioner ... RowGroups(...)]
#> 
#>                       Kasse : 
#>      Saerbedrift + Nettinger: 
#>                                [ output='data.frame'...........::...]
#> 
#> 
#> 
#> [--------------------------------------------------------> 
#> [-------- Egne beregninger for formelkorreksjoner --------->
#> [-------------------------------------------------------->
#> 
#> Warning: artshierarki_nettinger_kasse er ikke i input. Alle kasse-nettinger blir 0.
#> 
#>  [ Kombinasjonsberegninger...] [ Lager kombinasjoner ...]
#> 
#>                       Kasse : 
#>      Saerbedrift + Nettinger: 
#>                                [ output='data.frame'.....]
#> 
#> 
#> D:FG2:AGD12 = D:FG5:AGD12 - D:800:EIENDOMSSKATT_OG__ANDRE_SKATTER 
#>   periode regnskapsomfang region funksjon kontoklasse   art  belop1   belop
#> 1    2015               A   0300      FG2           D AGD12     254     169
#> 2    2015               A   0301      FG2           D AGD12 -513452 -513537
#> 3    2015               B   0300      FG2           D AGD12     254     169
#> 4    2015               B   0301      FG2           D AGD12     254     169
#>   D:FG5:AGD12 D:800:EIENDOMSSKATT_OG__ANDRE_SKATTER
#> 1         254                                    85
#> 2     -513452                                    85
#> 3         254                                    85
#> 4         254                                    85
#> 
#> 
#> D:FG2:AGD45 = D:FG2:AGD45 - D:FG2:A800 + D:840:A800 
#>   periode regnskapsomfang region funksjon kontoklasse   art   belop1    belop
#> 1    2015               A   0300      FG2           D AGD45 37227015 37227015
#> 2    2015               A   0301      FG2           D AGD45 37227015 37227015
#> 3    2015               B   0300      FG2           D AGD45 37227015 37227015
#> 4    2015               B   0301      FG2           D AGD45 37227015 37227015
#>   D:FG2:AGD45 D:FG2:A800 D:840:A800
#> 1    37227015   10133395   10133395
#> 2    37227015   10133395   10133395
#> 3    37227015   10133395   10133395
#> 4    37227015   10133395   10133395
#> 
#> 
#> D:FG5:AGD62 = D:FG5:AGD62 - D:800:EIENDOMSSKATT_OG__ANDRE_SKATTER 
#>   periode regnskapsomfang region funksjon kontoklasse   art  belop1   belop
#> 1    2015               A   0300      FG5           D AGD62     254     169
#> 2    2015               A   0301      FG5           D AGD62 -513452 -513537
#> 3    2015               B   0300      FG5           D AGD62     254     169
#> 4    2015               B   0301      FG5           D AGD62     254     169
#>   D:FG5:AGD62 D:800:EIENDOMSSKATT_OG__ANDRE_SKATTER
#> 1         254                                    85
#> 2     -513452                                    85
#> 3         254                                    85
#> 4         254                                    85
#> 
#> 
#> D:FG2:AGD65 = ( D:FG2:AGD45 - D:FG2:A800 + D:840:A800 ) - D:FG2:AGD46 - D:FG2:AG18 
#>   periode regnskapsomfang region funksjon kontoklasse   art   belop1    belop
#> 1    2015               A   0300      FG2           D AGD65 24391081 24391081
#> 2    2015               A   0301      FG2           D AGD65 22859014 23879850
#> 3    2015               B   0300      FG2           D AGD65 24391081 24391081
#> 4    2015               B   0301      FG2           D AGD65 24391081 24391081
#>   D:FG2:AGD45 D:FG2:A800 D:840:A800 D:FG2:AGD46 D:FG2:AG18
#> 1    37227015   10133395   10133395     4346102    8489832
#> 2    37227015   10133395   10133395     4346915    9000250
#> 3    37227015   10133395   10133395     4346102    8489832
#> 4    37227015   10133395   10133395     4346102    8489832
#> 
#> 
#> D:FG2:AGD22 = D:FG2:AGD30 - D:FG2:AGD31 - D:FG2:AG27 
#>   periode regnskapsomfang region funksjon kontoklasse   art belop1 belop
#> 1    2015               A   0300      FG2           D AGD22      0     0
#> 2    2015               A   0301      FG2           D AGD22      0     0
#> 3    2015               B   0300      FG2           D AGD22      0     0
#> 4    2015               B   0301      FG2           D AGD22      0     0
#>   D:FG2:AGD30 D:FG2:AGD31 D:FG2:AG27
#> 1           0           0          0
#> 2           0           0          0
#> 3           0           0          0
#> 4           0           0          0
#> 
#> 
#> D:FG2:AGD66 = ( ( D:FG2:AGD45 - D:FG2:A800 + D:840:A800 ) - D:FG2:AGD46 - D:FG2:AG18 ) +  ( D:FG2:AGD30 - D:FG2:AGD31 - D:FG2:AG27 ) + D:FG2:990 
#>   periode regnskapsomfang region funksjon kontoklasse   art   belop1    belop
#> 1    2015               A   0300      FG2           D AGD66 25549762 25549762
#> 2    2015               A   0301      FG2           D AGD66 24017695 25038531
#> 3    2015               B   0300      FG2           D AGD66 25549762 25549762
#> 4    2015               B   0301      FG2           D AGD66 25549762 25549762
#>   D:FG2:AGD45 D:FG2:A800 D:840:A800 D:FG2:AGD46 D:FG2:AG18 D:FG2:AGD30
#> 1    37227015   10133395   10133395     4346102    8489832           0
#> 2    37227015   10133395   10133395     4346915    9000250           0
#> 3    37227015   10133395   10133395     4346102    8489832           0
#> 4    37227015   10133395   10133395     4346102    8489832           0
#>   D:FG2:AGD31 D:FG2:AG27 D:FG2:990
#> 1           0          0   1158681
#> 2           0          0   1158681
#> 3           0          0   1158681
#> 4           0          0   1158681
#> 
#> 
#> D:FG2:AGD32 = ( ( ( D:FG2:AGD45 - D:FG2:A800 + D:840:A800 ) - D:FG2:AGD46 - D:FG2:AG18 ) +  ( D:FG2:AGD30 - D:FG2:AGD31 - D:FG2:AG27 ) + D:FG2:990 ) + D:FG2:AG14 - D:FG2:AG7 
#>   periode regnskapsomfang region funksjon kontoklasse   art   belop1    belop
#> 1    2015               A   0300      FG2           D AGD32 25549762 25549762
#> 2    2015               A   0301      FG2           D AGD32 24017695 25038531
#> 3    2015               B   0300      FG2           D AGD32 25549762 25549762
#> 4    2015               B   0301      FG2           D AGD32 25549762 25549762
#>   D:FG2:AGD45 D:FG2:A800 D:840:A800 D:FG2:AGD46 D:FG2:AG18 D:FG2:AGD30
#> 1    37227015   10133395   10133395     4346102    8489832           0
#> 2    37227015   10133395   10133395     4346915    9000250           0
#> 3    37227015   10133395   10133395     4346102    8489832           0
#> 4    37227015   10133395   10133395     4346102    8489832           0
#>   D:FG2:AGD31 D:FG2:AG27 D:FG2:990 D:FG2:AG14 D:FG2:AG7
#> 1           0          0   1158681          0         0
#> 2           0          0   1158681          0         0
#> 3           0          0   1158681          0         0
#> 4           0          0   1158681          0         0
#> 
#> 
#> output 
#>     periode regnskapsomfang region funksjon kontoklasse   art belop
#> 1      2015               A   0300      FG1           D AGD50     0
#> 2      2015               A   0301      FG1           D AGD50     0
#> 819    2015               B   0300      FG5           D AGD72     0
#> 820    2015               B   0301      FG5           D AGD72     0
#> 
#> ================================================================
#> ==============    periode =  2016     ================
#> ==============================================================
#> 
#> data
#>     periode region kontoklasse funksjon art belop
#> 1      2016   0300           D      120 380  2312
#> 2      2016   0300           D      121 380   337
#> 338    2016 030101           D      100 099   178
#> 339    2016 030107           D      100 099   215
#> 
#> 
#> funksjonshierarki
#>     periode   to  from sign
#> 1      2016 FGF1 FGF1a    +
#> 2      2016 FGF1 FGF1b    +
#> 224    2016  FG5   321    -
#> 225    2016  FG5   711    -
#> 
#> 
#> artshierarki
#>     periode    to  from sign
#> 1      2016  LONN   010    +
#> 2      2016  LONN   020    +
#> 542    2016 AGD65 AGD46    -
#> 543    2016 AGD65  AG18    -
#> 
#> 
#> data_saer
#>     periode region kontoklasse funksjon art belop
#> 1      2016   0101           D      110 775  1729
#> 2      2016   0101           D      120 895   296
#> 119    2016   0101           D      110 090   106
#> 120    2016   0101           D      110 099   109
#> 
#> 
#> artshierarki_nettinger
#>    periode    to                   from sign
#> 1     2016  AGD9 KJOP_SALG_OVERFORINGER    -
#> 2     2016 AGD10 KJOP_SALG_OVERFORINGER    -
#> 93    2016 AGD65                   COPY    -
#> 94    2016 AGD22                   COPY    -
#> 
#> 
#> artshierarki_nettinger_kasse
#> NULL
#> 
#> 
#> kombinasjoner
#> NULL
#> 
#> 
#> regioner
#>      x
#> 1 02??
#> 2 03??
#> 
#> 
#> storkombinasjoner
#> NULL
#> 
#> 
#> stjernetabell
#>     periode region kontoklasse funksjon   art sign
#> 1      2016 ??????                 FG0*          +
#> 2      2016 ??????                 FG1*          +
#> 162    2016                        !FG2 AGI26    -
#> 163    2016                        !FG2 AGI19    -
#> 
#> 
#> funksjoner
#>     x
#> 1 FG?
#> 
#> 
#> arter
#>      x
#> 1 AGD*
#> 
#> 
#> kontoklasser
#> NULL
#> 
#> 
#> regnskapsomfang
#> NULL
#> 
#>    periode
#> 15 2016   
#> 16 2016   
#> 17 2016   
#> 18 2016   
#> 19 2016   
#> 20 2016   
#> 21 2016   
#> 22 2016   
#> 23 2016   
#> 24 2016   
#> 25 2016   
#>    formel                                                                       
#> 15 I:FG2:AGI2 = I:FG2:AGI2 - I:FG1:AGI1                                         
#> 16 FG2:AGD12 = FG5:AGD12 - 800:EIENDOMSSKATT_OG__ANDRE_SKATTER                  
#> 17 FG2:AG11 = 840:AG11 - 840:AG12 + 800:AG12                                    
#> 18 D:FG2:AGID1 = D:FG2:AGID1 - D:FG2:990 + D:FG2:AGI3 + D:860:990 - I:FG2:AGI3  
#> 19 D:FG2:AGID2 = D:FG2:AGID2 + I:FG2:AGID2                                      
#> 20 I:FG2:AGI21 = I:FG4:AGI1 - I:FG2:AGI17                                       
#> 21 I:FG2:AGI28 = I:FG2:AG6 - I:FG2:AG13                                         
#> 22 D:FG2:AGD45 = D:FG2:AGD45 - D:FG2:A800 + D:840:A800 - D:FG2:AG10 + D:800:AG10
#> 23 FG5:AGD62 = FG5:AGD62 - 800:EIENDOMSSKATT_OG__ANDRE_SKATTER                  
#> 24 FG2:AGD64 = - FG2:AGD64                                                      
#> 25 D:FG2:AGD65 = D:FG2:AGD45 - D:FG2:AGD46 - D:FG2:AG18                         
#> 
#>  
#>  --- Kjører AutoFormel ----
#> 
#>  
#>  --- Etter AutoFormel ----
#>    periode
#> 1  2016   
#> 2  2016   
#> 3  2016   
#> 4  2016   
#> 5  2016   
#> 6  2016   
#> 7  2016   
#> 8  2016   
#> 9  2016   
#> 10 2016   
#> 11 2016   
#> 12 2016   
#> 13 2016   
#> 14 2016   
#> 15 2016   
#>    formel                                                                                                      
#> 1  I:FG2:AGI2 = I:FG2:AGI2 - I:FG1:AGI1                                                                        
#> 2  D:FG2:AGD12 = D:FG5:AGD12 - D:800:EIENDOMSSKATT_OG__ANDRE_SKATTER                                           
#> 3  D:FG2:AG11 = D:840:AG11 - D:840:AG12 + D:800:AG12                                                           
#> 4  D:FG2:AGID1 = D:FG2:AGID1 - D:FG2:990 + D:FG2:AGI3 + D:860:990 - I:FG2:AGI3                                 
#> 5  D:FG2:AGID2 = D:FG2:AGID2 + I:FG2:AGID2                                                                     
#> 6  I:FG2:AGI21 = I:FG4:AGI1 - I:FG2:AGI17                                                                      
#> 7  I:FG2:AGI28 = I:FG2:AG6 - I:FG2:AG13                                                                        
#> 8  D:FG2:AGD45 = D:FG2:AGD45 - D:FG2:A800 + D:840:A800 - D:FG2:AG10 + D:800:AG10                               
#> 9  D:FG5:AGD62 = D:FG5:AGD62 - D:800:EIENDOMSSKATT_OG__ANDRE_SKATTER                                           
#> 10 D:FG2:AGD64 = - D:FG2:AGD64                                                                                 
#> 11 D:FG2:AGD65 = ( D:FG2:AGD45 - D:FG2:A800 + D:840:A800 - D:FG2:AG10 + D:800:AG10 ) - D:FG2:AGD46 - D:FG2:AG18
#> 12 I:FG2:AGD12 = I:FG5:AGD12 - I:800:EIENDOMSSKATT_OG__ANDRE_SKATTER                                           
#> 13 I:FG2:AG11 = I:840:AG11 - I:840:AG12 + I:800:AG12                                                           
#> 14 I:FG5:AGD62 = I:FG5:AGD62 - I:800:EIENDOMSSKATT_OG__ANDRE_SKATTER                                           
#> 15 I:FG2:AGD64 = - I:FG2:AGD64                                                                                 
#> Warning: artshierarki_nettinger_kasse er ikke i input. Alle kasse-nettinger blir 0.
#> 
#>  AutoNetting:  artshierarki_nettinger  fra  94  til  113  rader
#> 
#>  [ Kombinasjonsberegninger...
#> 
#>  region:funksjon    8 elements
#>            region+funksjon    8 elements
#>            region-funksjon    8 elements
#> 
#> 
#>  kontoklasse:art    124 elements
#>            kontoklasse+art    124 elements
#>            kontoklasse-art    62 elements
#> 
#> 
#>  region:funksjon:kontoklasse:art    496 elements
#>  [ RowGroups...]           funksjon-art    350 elements
#>            region-art    350 elements
#> ] [ Fra til storkombinasjoner til kombinasjoner og regioner ... RowGroups(...)]
#> 
#>                       Kasse : 
#>      Saerbedrift + Nettinger: 
#>                                [ output='data.frame'...........::...]
#> 
#> 
#> 
#> [--------------------------------------------------------> 
#> [-------- Egne beregninger for formelkorreksjoner --------->
#> [-------------------------------------------------------->
#> 
#> Warning: artshierarki_nettinger_kasse er ikke i input. Alle kasse-nettinger blir 0.
#> 
#>  [ Kombinasjonsberegninger...] [ Lager kombinasjoner ...]
#> 
#>                       Kasse : 
#>      Saerbedrift + Nettinger: 
#>                                [ output='data.frame'.....]
#> 
#> 
#> D:FG2:AGD12 = D:FG5:AGD12 - D:800:EIENDOMSSKATT_OG__ANDRE_SKATTER 
#>   periode regnskapsomfang region funksjon kontoklasse   art  belop1   belop
#> 1    2016               A   0300      FG2           D AGD12 -270936 -525246
#> 2    2016               A   0301      FG2           D AGD12 -270936 -525246
#> 3    2016               B   0300      FG2           D AGD12  254452     142
#> 4    2016               B   0301      FG2           D AGD12  254452     142
#>   D:FG5:AGD12 D:800:EIENDOMSSKATT_OG__ANDRE_SKATTER
#> 1     -270936                                254310
#> 2     -270936                                254310
#> 3      254452                                254310
#> 4      254452                                254310
#> 
#> 
#> D:FG2:AGD45 = D:FG2:AGD45 - D:FG2:A800 + D:840:A800 - D:FG2:AG10 + D:800:AG10 
#>   periode regnskapsomfang region funksjon kontoklasse   art   belop1    belop
#> 1    2016               A   0300      FG2           D AGD45 40373040 40373040
#> 2    2016               A   0301      FG2           D AGD45 40373040 40373040
#> 3    2016               B   0300      FG2           D AGD45 40373040 40373040
#> 4    2016               B   0301      FG2           D AGD45 40373040 40373040
#>   D:FG2:AGD45 D:FG2:A800 D:840:A800 D:FG2:AG10 D:800:AG10
#> 1    40373040    9530850    9530850     254013     254013
#> 2    40373040    9530850    9530850     254013     254013
#> 3    40373040    9530850    9530850     254013     254013
#> 4    40373040    9530850    9530850     254013     254013
#> 
#> 
#> D:FG5:AGD62 = D:FG5:AGD62 - D:800:EIENDOMSSKATT_OG__ANDRE_SKATTER 
#>   periode regnskapsomfang region funksjon kontoklasse   art  belop1   belop
#> 1    2016               A   0300      FG5           D AGD62 -270936 -525246
#> 2    2016               A   0301      FG5           D AGD62 -270936 -525246
#> 3    2016               B   0300      FG5           D AGD62  254452     142
#> 4    2016               B   0301      FG5           D AGD62  254452     142
#>   D:FG5:AGD62 D:800:EIENDOMSSKATT_OG__ANDRE_SKATTER
#> 1     -270936                                254310
#> 2     -270936                                254310
#> 3      254452                                254310
#> 4      254452                                254310
#> 
#> 
#> D:FG2:AGD64 = - D:FG2:AGD64 
#>   periode regnskapsomfang region funksjon kontoklasse   art belop1 belop
#> 1    2016               A   0300      FG2           D AGD64      0     0
#> 2    2016               A   0301      FG2           D AGD64      0     0
#> 3    2016               B   0300      FG2           D AGD64      0     0
#> 4    2016               B   0301      FG2           D AGD64      0     0
#>   D:FG2:AGD64
#> 1           0
#> 2           0
#> 3           0
#> 4           0
#> 
#> 
#> D:FG2:AGD65 = ( D:FG2:AGD45 - D:FG2:A800 + D:840:A800 - D:FG2:AG10 + D:800:AG10 ) - D:FG2:AGD46 - D:FG2:AG18 
#>   periode regnskapsomfang region funksjon kontoklasse   art   belop1    belop
#> 1    2016               A   0300      FG2           D AGD65 25781844 25781844
#> 2    2016               A   0301      FG2           D AGD65 25781844 25781844
#> 3    2016               B   0300      FG2           D AGD65 26303535 26303535
#> 4    2016               B   0301      FG2           D AGD65 26303535 26303535
#>   D:FG2:AGD45 D:FG2:A800 D:840:A800 D:FG2:AG10 D:800:AG10 D:FG2:AGD46
#> 1    40373040    9530850    9530850     254013     254013     4739074
#> 2    40373040    9530850    9530850     254013     254013     4739074
#> 3    40373040    9530850    9530850     254013     254013     4738419
#> 4    40373040    9530850    9530850     254013     254013     4738419
#>   D:FG2:AG18
#> 1    9852122
#> 2    9852122
#> 3    9331086
#> 4    9331086
#> 
#> 
#> output 
#>     periode regnskapsomfang region funksjon kontoklasse   art    belop
#> 1      2016               A   0300      FG1           D AGD50        1
#> 2      2016               A   0301      FG1           D AGD50        1
#> 699    2016               B   0300      FG5           D AGD65 26303535
#> 700    2016               B   0301      FG5           D AGD65 26303535
#> 


# Kombinerer stjernetabell og kombinasjoner. Denne gir 14360352 rader. [Beregningstid: 1 minutt] MERK: igjen kreves minne
z <- KostraRegnskap(inputdata, funksjonshierarki, artshierarki, inputdata_saer, artshierarki_nettinger, kombinasjoner = kombinasjoner, stjernetabell = stjerne)
#> ================================================================
#> ==============    periode =  2015     ================
#> ==============================================================
#> 
#> data
#>     periode region kontoklasse funksjon art belop
#> 1      2015   0300           D      120 380   847
#> 2      2015   0300           D      121 380   288
#> 291    2015 030107           D      100 090   157
#> 292    2015 030107           D      100 099   364
#> 
#> 
#> funksjonshierarki
#>     periode   to  from sign
#> 1      2015 FGF1 FGF1a    +
#> 2      2015 FGF1 FGF1b    +
#> 225    2015  FG5   321    -
#> 226    2015  FG5   711    -
#> 
#> 
#> artshierarki
#>     periode    to from sign
#> 1      2015  LONN  010    +
#> 2      2015  LONN  020    +
#> 627    2015 AI660  660    +
#> 628    2015 AD990  990    +
#> 
#> 
#> data_saer
#>    periode region kontoklasse funksjon art belop
#> 1     2015   0100           D      515 375   573
#> 2     2015   0301           D      515 375   573
#> 61    2015   0301           D      100 099   100
#> 62    2015   0101           D      110 099   108
#> 
#> 
#> artshierarki_nettinger
#>    periode    to                   from sign
#> 1     2015  AGD9 KJOP_SALG_OVERFORINGER    -
#> 2     2015 AGD10 KJOP_SALG_OVERFORINGER    -
#> 83    2015 AGI31 KJOP_SALG_OVERFORINGER    -
#> 84    2015 AGI31                 RENTER    -
#> 
#> 
#> artshierarki_nettinger_kasse
#> NULL
#> 
#> 
#> kombinasjoner
#>         art funksjon kontoklasse
#> 1      AG35     FGF1           D
#> 2      AG35    FGF1a           D
#> 53884 AGD64      290           D
#> 53885 AGI10      FG2           I
#> 
#> 
#> regioner
#> NULL
#> 
#> 
#> storkombinasjoner
#> NULL
#> 
#> 
#> stjernetabell
#>     periode region kontoklasse funksjon   art sign
#> 1      2015 ??????                 FG0*          +
#> 2      2015 ??????                 FG1*          +
#> 159    2015                        !FG2 AGI26    -
#> 160    2015                        !FG2 AGI19    -
#> 
#> 
#> funksjoner
#> NULL
#> 
#> 
#> arter
#> NULL
#> 
#> 
#> kontoklasser
#> NULL
#> 
#> 
#> regnskapsomfang
#> NULL
#> 
#> 
#> formler
#> NULL
#> 
#> Warning: artshierarki_nettinger_kasse er ikke i input. Alle kasse-nettinger blir 0.
#> 
#>  AutoNetting:  artshierarki_nettinger  fra  84  til  99  rader
#> 
#>  [ Kombinasjonsberegninger... [ RowGroups...]
#> 
#>  region:funksjon    528 elements
#>            Matching combinations   528 elements
#>            region+funksjon    224 elements
#>            region-funksjon    224 elements
#> 
#> 
#>  art:kontoklasse    452 elements
#>            Matching combinations   289 elements
#>            art+kontoklasse    289 elements
#>            art-kontoklasse    289 elements
#> 
#> 
#>  region:funksjon:art:kontoklasse    64736 elements
#>            Matching combinations   56962 elements
#>  [ RowGroups...]           funksjon-art    56962 elements
#>            region-art    56962 elements
#> ] [ Fra til storkombinasjoner til kombinasjoner og regioner ... RowGroups(...)]
#> 
#>                       Kasse : 
#>      Saerbedrift + Nettinger: 
#>                                [ output='data.frame'...........::...]
#> 
#> 
#> output 
#>        periode regnskapsomfang region funksjon   art kontoklasse belop
#> 1         2015               A   0300     FGF1  AG35           D     0
#> 2         2015               A   0100     FGF1  AG35           D     0
#> 113923    2015               B   0100      FG2 AGI10           I     0
#> 113924    2015               B   0101      FG2 AGI10           I     0
#> 
#> ================================================================
#> ==============    periode =  2016     ================
#> ==============================================================
#> 
#> data
#>     periode region kontoklasse funksjon art belop
#> 1      2016   0300           D      120 380  2312
#> 2      2016   0300           D      121 380   337
#> 338    2016 030101           D      100 099   178
#> 339    2016 030107           D      100 099   215
#> 
#> 
#> funksjonshierarki
#>     periode   to  from sign
#> 1      2016 FGF1 FGF1a    +
#> 2      2016 FGF1 FGF1b    +
#> 224    2016  FG5   321    -
#> 225    2016  FG5   711    -
#> 
#> 
#> artshierarki
#>     periode    to  from sign
#> 1      2016  LONN   010    +
#> 2      2016  LONN   020    +
#> 542    2016 AGD65 AGD46    -
#> 543    2016 AGD65  AG18    -
#> 
#> 
#> data_saer
#>     periode region kontoklasse funksjon art belop
#> 1      2016   0101           D      110 775  1729
#> 2      2016   0101           D      120 895   296
#> 119    2016   0101           D      110 090   106
#> 120    2016   0101           D      110 099   109
#> 
#> 
#> artshierarki_nettinger
#>    periode    to                   from sign
#> 1     2016  AGD9 KJOP_SALG_OVERFORINGER    -
#> 2     2016 AGD10 KJOP_SALG_OVERFORINGER    -
#> 93    2016 AGD65                   COPY    -
#> 94    2016 AGD22                   COPY    -
#> 
#> 
#> artshierarki_nettinger_kasse
#> NULL
#> 
#> 
#> kombinasjoner
#>         art funksjon kontoklasse
#> 1      AG35     FGF1           D
#> 2      AG35    FGF1a           D
#> 53884 AGD64      290           D
#> 53885 AGI10      FG2           I
#> 
#> 
#> regioner
#> NULL
#> 
#> 
#> storkombinasjoner
#> NULL
#> 
#> 
#> stjernetabell
#>     periode region kontoklasse funksjon   art sign
#> 1      2016 ??????                 FG0*          +
#> 2      2016 ??????                 FG1*          +
#> 162    2016                        !FG2 AGI26    -
#> 163    2016                        !FG2 AGI19    -
#> 
#> 
#> funksjoner
#> NULL
#> 
#> 
#> arter
#> NULL
#> 
#> 
#> kontoklasser
#> NULL
#> 
#> 
#> regnskapsomfang
#> NULL
#> 
#> 
#> formler
#> NULL
#> 
#> Warning: artshierarki_nettinger_kasse er ikke i input. Alle kasse-nettinger blir 0.
#> 
#>  AutoNetting:  artshierarki_nettinger  fra  94  til  113  rader
#> 
#>  [ Kombinasjonsberegninger... [ RowGroups...]
#> 
#>  region:funksjon    528 elements
#>            Matching combinations   528 elements
#>            region+funksjon    224 elements
#>            region-funksjon    224 elements
#> 
#> 
#>  art:kontoklasse    452 elements
#>            Matching combinations   289 elements
#>            art+kontoklasse    289 elements
#>            art-kontoklasse    289 elements
#> 
#> 
#>  region:funksjon:art:kontoklasse    64736 elements
#>            Matching combinations   56526 elements
#>  [ RowGroups...]           funksjon-art    56526 elements
#>            region-art    56526 elements
#> ] [ Fra til storkombinasjoner til kombinasjoner og regioner ... RowGroups(...)]
#> 
#>                       Kasse : 
#>      Saerbedrift + Nettinger: 
#>                                [ output='data.frame'...........::...]
#> 
#> 
#> output 
#>        periode regnskapsomfang region funksjon   art kontoklasse belop
#> 1         2016               A   0300     FGF1  AG35           D     0
#> 2         2016               A   0100     FGF1  AG35           D     0
#> 113051    2016               B   0100      FG2 AGI10           I     0
#> 113052    2016               B   0101      FG2 AGI10           I     0
#> 

# Lite eksempel med output = "en" (som default)
KostraRegnskap(inputdata, funksjonshierarki, artshierarki, inputdata_saer, artshierarki_nettinger, regioner = "0101", funksjoner = "FG2", arter = "AGD10")
#> ================================================================
#> ==============    periode =  2015     ================
#> ==============================================================
#> 
#> data
#>     periode region kontoklasse funksjon art belop
#> 1      2015   0300           D      120 380   847
#> 2      2015   0300           D      121 380   288
#> 291    2015 030107           D      100 090   157
#> 292    2015 030107           D      100 099   364
#> 
#> 
#> funksjonshierarki
#>     periode   to  from sign
#> 1      2015 FGF1 FGF1a    +
#> 2      2015 FGF1 FGF1b    +
#> 225    2015  FG5   321    -
#> 226    2015  FG5   711    -
#> 
#> 
#> artshierarki
#>     periode    to from sign
#> 1      2015  LONN  010    +
#> 2      2015  LONN  020    +
#> 627    2015 AI660  660    +
#> 628    2015 AD990  990    +
#> 
#> 
#> data_saer
#>    periode region kontoklasse funksjon art belop
#> 1     2015   0100           D      515 375   573
#> 2     2015   0301           D      515 375   573
#> 61    2015   0301           D      100 099   100
#> 62    2015   0101           D      110 099   108
#> 
#> 
#> artshierarki_nettinger
#>    periode    to                   from sign
#> 1     2015  AGD9 KJOP_SALG_OVERFORINGER    -
#> 2     2015 AGD10 KJOP_SALG_OVERFORINGER    -
#> 83    2015 AGI31 KJOP_SALG_OVERFORINGER    -
#> 84    2015 AGI31                 RENTER    -
#> 
#> 
#> artshierarki_nettinger_kasse
#> NULL
#> 
#> 
#> kombinasjoner
#> NULL
#> 
#> 
#> regioner
#>      x
#> 1 0101
#> 
#> 
#> storkombinasjoner
#> NULL
#> 
#> 
#> stjernetabell
#> NULL
#> 
#> 
#> funksjoner
#>     x
#> 1 FG2
#> 
#> 
#> arter
#>       x
#> 1 AGD10
#> 
#> 
#> kontoklasser
#> NULL
#> 
#> 
#> regnskapsomfang
#> NULL
#> 
#> 
#> formler
#> NULL
#> 
#> Warning: artshierarki_nettinger_kasse er ikke i input. Alle kasse-nettinger blir 0.
#> 
#>  AutoNetting:  artshierarki_nettinger  fra  84  til  99  rader
#> 
#>  [ Kombinasjonsberegninger...] [ Lager kombinasjoner ...]
#> 
#>                       Kasse : 
#>      Saerbedrift + Nettinger: 
#>                                [ output='data.frame'.....]
#> 
#> 
#> output 
#>   periode regnskapsomfang region   art funksjon kontoklasse belop
#> 1    2015               A   0101 AGD10      FG2           D  4064
#> 2    2015               A   0101 AGD10      FG2           I  1076
#> 3    2015               B   0101 AGD10      FG2           D  5117
#> 4    2015               B   0101 AGD10      FG2           I  1076
#> 
#> ================================================================
#> ==============    periode =  2016     ================
#> ==============================================================
#> 
#> data
#>     periode region kontoklasse funksjon art belop
#> 1      2016   0300           D      120 380  2312
#> 2      2016   0300           D      121 380   337
#> 338    2016 030101           D      100 099   178
#> 339    2016 030107           D      100 099   215
#> 
#> 
#> funksjonshierarki
#>     periode   to  from sign
#> 1      2016 FGF1 FGF1a    +
#> 2      2016 FGF1 FGF1b    +
#> 224    2016  FG5   321    -
#> 225    2016  FG5   711    -
#> 
#> 
#> artshierarki
#>     periode    to  from sign
#> 1      2016  LONN   010    +
#> 2      2016  LONN   020    +
#> 542    2016 AGD65 AGD46    -
#> 543    2016 AGD65  AG18    -
#> 
#> 
#> data_saer
#>     periode region kontoklasse funksjon art belop
#> 1      2016   0101           D      110 775  1729
#> 2      2016   0101           D      120 895   296
#> 119    2016   0101           D      110 090   106
#> 120    2016   0101           D      110 099   109
#> 
#> 
#> artshierarki_nettinger
#>    periode    to                   from sign
#> 1     2016  AGD9 KJOP_SALG_OVERFORINGER    -
#> 2     2016 AGD10 KJOP_SALG_OVERFORINGER    -
#> 93    2016 AGD65                   COPY    -
#> 94    2016 AGD22                   COPY    -
#> 
#> 
#> artshierarki_nettinger_kasse
#> NULL
#> 
#> 
#> kombinasjoner
#> NULL
#> 
#> 
#> regioner
#>      x
#> 1 0101
#> 
#> 
#> storkombinasjoner
#> NULL
#> 
#> 
#> stjernetabell
#> NULL
#> 
#> 
#> funksjoner
#>     x
#> 1 FG2
#> 
#> 
#> arter
#>       x
#> 1 AGD10
#> 
#> 
#> kontoklasser
#> NULL
#> 
#> 
#> regnskapsomfang
#> NULL
#> 
#> 
#> formler
#> NULL
#> 
#> Warning: artshierarki_nettinger_kasse er ikke i input. Alle kasse-nettinger blir 0.
#> 
#>  AutoNetting:  artshierarki_nettinger  fra  94  til  113  rader
#> 
#>  [ Kombinasjonsberegninger...] [ Lager kombinasjoner ...]
#> 
#>                       Kasse : 
#>      Saerbedrift + Nettinger: 
#>                                [ output='data.frame'.....]
#> 
#> 
#> output 
#>   periode regnskapsomfang region   art funksjon kontoklasse belop
#> 1    2016               A   0101 AGD10      FG2           D  4630
#> 2    2016               A   0101 AGD10      FG2           I -2568
#> 3    2016               B   0101 AGD10      FG2           D  5769
#> 4    2016               B   0101 AGD10      FG2           I -2568
#> 
#>   periode regnskapsomfang region   art funksjon kontoklasse belop
#> 1    2015               A   0101 AGD10      FG2           D  4064
#> 2    2015               A   0101 AGD10      FG2           I  1076
#> 3    2015               B   0101 AGD10      FG2           D  5117
#> 4    2015               B   0101 AGD10      FG2           I  1076
#> 5    2016               A   0101 AGD10      FG2           D  4630
#> 6    2016               A   0101 AGD10      FG2           I -2568
#> 7    2016               B   0101 AGD10      FG2           D  5769
#> 8    2016               B   0101 AGD10      FG2           I -2568

# Samme eksempel med output = "fire"
KostraRegnskap(inputdata, funksjonshierarki, artshierarki, inputdata_saer, artshierarki_nettinger, regioner = "0101", funksjoner = "FG2", arter = "AGD10", output = "fire")
#> ================================================================
#> ==============    periode =  2015     ================
#> ==============================================================
#> 
#> data
#>     periode region kontoklasse funksjon art belop
#> 1      2015   0300           D      120 380   847
#> 2      2015   0300           D      121 380   288
#> 291    2015 030107           D      100 090   157
#> 292    2015 030107           D      100 099   364
#> 
#> 
#> funksjonshierarki
#>     periode   to  from sign
#> 1      2015 FGF1 FGF1a    +
#> 2      2015 FGF1 FGF1b    +
#> 225    2015  FG5   321    -
#> 226    2015  FG5   711    -
#> 
#> 
#> artshierarki
#>     periode    to from sign
#> 1      2015  LONN  010    +
#> 2      2015  LONN  020    +
#> 627    2015 AI660  660    +
#> 628    2015 AD990  990    +
#> 
#> 
#> data_saer
#>    periode region kontoklasse funksjon art belop
#> 1     2015   0100           D      515 375   573
#> 2     2015   0301           D      515 375   573
#> 61    2015   0301           D      100 099   100
#> 62    2015   0101           D      110 099   108
#> 
#> 
#> artshierarki_nettinger
#>    periode    to                   from sign
#> 1     2015  AGD9 KJOP_SALG_OVERFORINGER    -
#> 2     2015 AGD10 KJOP_SALG_OVERFORINGER    -
#> 83    2015 AGI31 KJOP_SALG_OVERFORINGER    -
#> 84    2015 AGI31                 RENTER    -
#> 
#> 
#> artshierarki_nettinger_kasse
#> NULL
#> 
#> 
#> kombinasjoner
#> NULL
#> 
#> 
#> regioner
#>      x
#> 1 0101
#> 
#> 
#> storkombinasjoner
#> NULL
#> 
#> 
#> stjernetabell
#> NULL
#> 
#> 
#> funksjoner
#>     x
#> 1 FG2
#> 
#> 
#> arter
#>       x
#> 1 AGD10
#> 
#> 
#> kontoklasser
#> NULL
#> 
#> 
#> regnskapsomfang
#> NULL
#> 
#> 
#> formler
#> NULL
#> 
#> Warning: artshierarki_nettinger_kasse er ikke i input. Alle kasse-nettinger blir 0.
#> 
#>  AutoNetting:  artshierarki_nettinger  fra  84  til  99  rader
#> 
#>  [ Kombinasjonsberegninger...] [ Lager kombinasjoner ...]
#> 
#>                       Kasse : 
#>      Saerbedrift + Nettinger: 
#>                   Nettinger : 
#>                                [ output='data.frame'.....]
#> 
#> 
#> output 
#>   periode region   art funksjon kontoklasse kasse saerbedrift nettinger_kasse
#> 1    2015   0101 AGD10      FG2           D  5117         932               0
#> 2    2015   0101 AGD10      FG2           I  1076           0               0
#>   nettinger_saer konsern
#> 1          -1985    4064
#> 2              0    1076
#> 
#> ================================================================
#> ==============    periode =  2016     ================
#> ==============================================================
#> 
#> data
#>     periode region kontoklasse funksjon art belop
#> 1      2016   0300           D      120 380  2312
#> 2      2016   0300           D      121 380   337
#> 338    2016 030101           D      100 099   178
#> 339    2016 030107           D      100 099   215
#> 
#> 
#> funksjonshierarki
#>     periode   to  from sign
#> 1      2016 FGF1 FGF1a    +
#> 2      2016 FGF1 FGF1b    +
#> 224    2016  FG5   321    -
#> 225    2016  FG5   711    -
#> 
#> 
#> artshierarki
#>     periode    to  from sign
#> 1      2016  LONN   010    +
#> 2      2016  LONN   020    +
#> 542    2016 AGD65 AGD46    -
#> 543    2016 AGD65  AG18    -
#> 
#> 
#> data_saer
#>     periode region kontoklasse funksjon art belop
#> 1      2016   0101           D      110 775  1729
#> 2      2016   0101           D      120 895   296
#> 119    2016   0101           D      110 090   106
#> 120    2016   0101           D      110 099   109
#> 
#> 
#> artshierarki_nettinger
#>    periode    to                   from sign
#> 1     2016  AGD9 KJOP_SALG_OVERFORINGER    -
#> 2     2016 AGD10 KJOP_SALG_OVERFORINGER    -
#> 93    2016 AGD65                   COPY    -
#> 94    2016 AGD22                   COPY    -
#> 
#> 
#> artshierarki_nettinger_kasse
#> NULL
#> 
#> 
#> kombinasjoner
#> NULL
#> 
#> 
#> regioner
#>      x
#> 1 0101
#> 
#> 
#> storkombinasjoner
#> NULL
#> 
#> 
#> stjernetabell
#> NULL
#> 
#> 
#> funksjoner
#>     x
#> 1 FG2
#> 
#> 
#> arter
#>       x
#> 1 AGD10
#> 
#> 
#> kontoklasser
#> NULL
#> 
#> 
#> regnskapsomfang
#> NULL
#> 
#> 
#> formler
#> NULL
#> 
#> Warning: artshierarki_nettinger_kasse er ikke i input. Alle kasse-nettinger blir 0.
#> 
#>  AutoNetting:  artshierarki_nettinger  fra  94  til  113  rader
#> 
#>  [ Kombinasjonsberegninger...] [ Lager kombinasjoner ...]
#> 
#>                       Kasse : 
#>      Saerbedrift + Nettinger: 
#>                   Nettinger : 
#>                                [ output='data.frame'.....]
#> 
#> 
#> output 
#>   periode region   art funksjon kontoklasse kasse saerbedrift nettinger_kasse
#> 1    2016   0101 AGD10      FG2           D  5769         886               0
#> 2    2016   0101 AGD10      FG2           I -2568           0               0
#>   nettinger_saer konsern
#> 1          -2025    4630
#> 2              0   -2568
#> 
#>   periode region   art funksjon kontoklasse kasse saerbedrift nettinger_kasse
#> 1    2015   0101 AGD10      FG2           D  5117         932               0
#> 2    2015   0101 AGD10      FG2           I  1076           0               0
#> 3    2016   0101 AGD10      FG2           D  5769         886               0
#> 4    2016   0101 AGD10      FG2           I -2568           0               0
#>   nettinger_saer konsern
#> 1          -1985    4064
#> 2              0    1076
#> 3          -2025    4630
#> 4              0   -2568

 #===============================================================================
 #   Eksempler med:   Konsern  = Saebedrift + Nettinger + Kasse + Kassenettinger
 #===============================================================================


   zEn   <- KostraRegnskap(inputdata,funksjonshierarki,artshierarki,inputdata_saer,artshierarki_nettinger,artshierarki_nettinger_kasse,stjernetabell=stjerne,regioner = c("0101", "0100",  "0301"),formler=formler)
#> ================================================================
#> ==============    periode =  2015     ================
#> ==============================================================
#> 
#> data
#>     periode region kontoklasse funksjon art belop
#> 1      2015   0300           D      120 380   847
#> 2      2015   0300           D      121 380   288
#> 291    2015 030107           D      100 090   157
#> 292    2015 030107           D      100 099   364
#> 
#> 
#> funksjonshierarki
#>     periode   to  from sign
#> 1      2015 FGF1 FGF1a    +
#> 2      2015 FGF1 FGF1b    +
#> 225    2015  FG5   321    -
#> 226    2015  FG5   711    -
#> 
#> 
#> artshierarki
#>     periode    to from sign
#> 1      2015  LONN  010    +
#> 2      2015  LONN  020    +
#> 627    2015 AI660  660    +
#> 628    2015 AD990  990    +
#> 
#> 
#> data_saer
#>    periode region kontoklasse funksjon art belop
#> 1     2015   0100           D      515 375   573
#> 2     2015   0301           D      515 375   573
#> 61    2015   0301           D      100 099   100
#> 62    2015   0101           D      110 099   108
#> 
#> 
#> artshierarki_nettinger
#>    periode    to                   from sign
#> 1     2015  AGD9 KJOP_SALG_OVERFORINGER    -
#> 2     2015 AGD10 KJOP_SALG_OVERFORINGER    -
#> 83    2015 AGI31 KJOP_SALG_OVERFORINGER    -
#> 84    2015 AGI31                 RENTER    -
#> 
#> 
#> artshierarki_nettinger_kasse
#>    periode    to from sign
#> 1     2015 AGD51 A375    -
#> 2     2015 AGD51  380    -
#> 59    2015 AGI14 COPY    -
#> 60    2015 AGI19 COPY    -
#> 
#> 
#> kombinasjoner
#> NULL
#> 
#> 
#> regioner
#>      x
#> 1 0101
#> 2 0100
#> 3 0301
#> 
#> 
#> storkombinasjoner
#> NULL
#> 
#> 
#> stjernetabell
#>     periode region kontoklasse funksjon   art sign
#> 1      2015 ??????                 FG0*          +
#> 2      2015 ??????                 FG1*          +
#> 159    2015                        !FG2 AGI26    -
#> 160    2015                        !FG2 AGI19    -
#> 
#> 
#> funksjoner
#> NULL
#> 
#> 
#> arter
#> NULL
#> 
#> 
#> kontoklasser
#> NULL
#> 
#> 
#> regnskapsomfang
#> NULL
#> 
#>    periode
#> 1  2015   
#> 2  2015   
#> 3  2015   
#> 4  2015   
#> 5  2015   
#> 6  2015   
#> 7  2015   
#> 8  2015   
#> 9  2015   
#> 10 2015   
#> 11 2015   
#> 12 2015   
#> 13 2015   
#> 14 2015   
#>    formel                                                                     
#> 1  I:FG2:AGI2 = I:FG2:AGI2 - I:FG1:AGI1                                       
#> 2  FG2:AGD12 = FG5:AGD12 - 800:EIENDOMSSKATT_OG__ANDRE_SKATTER                
#> 3  FG2:AG11 = 840:AG11 - 840:AG12 + 800:AG12                                  
#> 4  D:FG2:AGID1 = D:FG2:AGID1 - D:FG2:990 + D:FG2:AGI3 + D:860:990 - I:FG2:AGI3
#> 5  D:FG2:AGID2 = D:FG2:AGID2 + I:FG2:AGID2                                    
#> 6  I:FG2:AGI21 = I:FG4:AGI1 - I:FG2:AGI17                                     
#> 7  D:FG2:AGD45 = D:FG2:AGD45 - D:FG2:A800 + D:840:A800                        
#> 8  FG5:AGD62 = FG5:AGD62 - 800:EIENDOMSSKATT_OG__ANDRE_SKATTER                
#> 9  D:FG2:AGD65 = D:FG2:AGD45 - D:FG2:AGD46 - D:FG2:AG18                       
#> 10 D:FG2:AGD22 = D:FG2:AGD30 - D:FG2:AGD31 - D:FG2:AG27                       
#> 11 D:FG2:AGD66 = D:FG2:AGD65 +  D:FG2:AGD22 + D:FG2:990                       
#> 12 D:FG2:AGD32 = D:FG2:AGD66 + D:FG2:AG14 - D:FG2:AG7                         
#> 13 I:FG2:AGI31 = I:FG2:AGI31 - I:FG1:AGI1                                     
#> 14 I:FG2:AGI16 = I:FG2:AGI20 + I:FG2:AGI15 - I:FG2:AGI14 + I:FG2:AG18         
#> 
#>  
#>  --- Kjører AutoFormel ----
#> 
#>  
#>  --- Etter AutoFormel ----
#>    periode
#> 1  2015   
#> 2  2015   
#> 3  2015   
#> 4  2015   
#> 5  2015   
#> 6  2015   
#> 7  2015   
#> 8  2015   
#> 9  2015   
#> 10 2015   
#> 11 2015   
#> 12 2015   
#> 13 2015   
#> 14 2015   
#> 15 2015   
#> 16 2015   
#> 17 2015   
#>    formel                                                                                                                                                                       
#> 1  I:FG2:AGI2 = I:FG2:AGI2 - I:FG1:AGI1                                                                                                                                         
#> 2  D:FG2:AGD12 = D:FG5:AGD12 - D:800:EIENDOMSSKATT_OG__ANDRE_SKATTER                                                                                                            
#> 3  D:FG2:AG11 = D:840:AG11 - D:840:AG12 + D:800:AG12                                                                                                                            
#> 4  D:FG2:AGID1 = D:FG2:AGID1 - D:FG2:990 + D:FG2:AGI3 + D:860:990 - I:FG2:AGI3                                                                                                  
#> 5  D:FG2:AGID2 = D:FG2:AGID2 + I:FG2:AGID2                                                                                                                                      
#> 6  I:FG2:AGI21 = I:FG4:AGI1 - I:FG2:AGI17                                                                                                                                       
#> 7  D:FG2:AGD45 = D:FG2:AGD45 - D:FG2:A800 + D:840:A800                                                                                                                          
#> 8  D:FG5:AGD62 = D:FG5:AGD62 - D:800:EIENDOMSSKATT_OG__ANDRE_SKATTER                                                                                                            
#> 9  D:FG2:AGD65 = ( D:FG2:AGD45 - D:FG2:A800 + D:840:A800 ) - D:FG2:AGD46 - D:FG2:AG18                                                                                           
#> 10 D:FG2:AGD22 = D:FG2:AGD30 - D:FG2:AGD31 - D:FG2:AG27                                                                                                                         
#> 11 D:FG2:AGD66 = ( ( D:FG2:AGD45 - D:FG2:A800 + D:840:A800 ) - D:FG2:AGD46 - D:FG2:AG18 ) +  ( D:FG2:AGD30 - D:FG2:AGD31 - D:FG2:AG27 ) + D:FG2:990                             
#> 12 D:FG2:AGD32 = ( ( ( D:FG2:AGD45 - D:FG2:A800 + D:840:A800 ) - D:FG2:AGD46 - D:FG2:AG18 ) +  ( D:FG2:AGD30 - D:FG2:AGD31 - D:FG2:AG27 ) + D:FG2:990 ) + D:FG2:AG14 - D:FG2:AG7
#> 13 I:FG2:AGI31 = I:FG2:AGI31 - I:FG1:AGI1                                                                                                                                       
#> 14 I:FG2:AGI16 = I:FG2:AGI20 + I:FG2:AGI15 - I:FG2:AGI14 + I:FG2:AG18                                                                                                           
#> 15 I:FG2:AGD12 = I:FG5:AGD12 - I:800:EIENDOMSSKATT_OG__ANDRE_SKATTER                                                                                                            
#> 16 I:FG2:AG11 = I:840:AG11 - I:840:AG12 + I:800:AG12                                                                                                                            
#> 17 I:FG5:AGD62 = I:FG5:AGD62 - I:800:EIENDOMSSKATT_OG__ANDRE_SKATTER                                                                                                            
#> 
#>  AutoNetting:  artshierarki_nettinger_kasse  fra  60  til  75  rader
#> 
#>  AutoNetting:  artshierarki_nettinger  fra  84  til  99  rader
#> 
#>  [ Kombinasjonsberegninger...
#> 
#>  region:funksjon    618 elements
#>            region+funksjon    466 elements
#>            region-funksjon    348 elements
#> 
#> 
#>  kontoklasse:art    458 elements
#>            kontoklasse+art    398 elements
#>            kontoklasse-art    298 elements
#> 
#> 
#>  region:funksjon:kontoklasse:art    103704 elements
#>  [ RowGroups...]           funksjon-art    91878 elements
#>            region-art    87726 elements
#> ] [ Fra til storkombinasjoner til kombinasjoner og regioner ... RowGroups(...)]
#> 
#>                       Kasse : 
#>      Saerbedrift + Nettinger: 
#>              Kasse-nettinger: 
#>                                [ output='data.frame'...........::...]
#> 
#> 
#> 
#> [--------------------------------------------------------> 
#> [-------- Egne beregninger for formelkorreksjoner --------->
#> [-------------------------------------------------------->
#> 
#> Warning: Formelen I:FG2:AGD12 = I:FG5:AGD12 - I:800:EIENDOMSSKATT_OG__ANDRE_SKATTER samsvarer med 0 rader i data
#> Warning: Formelen I:FG5:AGD62 = I:FG5:AGD62 - I:800:EIENDOMSSKATT_OG__ANDRE_SKATTER samsvarer med 0 rader i data
#> 
#>  [ Kombinasjonsberegninger...] [ Lager kombinasjoner ...]
#> 
#>                       Kasse : 
#>      Saerbedrift + Nettinger: 
#>              Kasse-nettinger: 
#>                                [ output='data.frame'.....]
#> 
#> 
#> I:FG2:AGI2 = I:FG2:AGI2 - I:FG1:AGI1 
#>   periode regnskapsomfang region funksjon kontoklasse  art  belop1   belop
#> 1    2015               A   0101      FG2           I AGI2   11396   11396
#> 2    2015               A   0100      FG2           I AGI2   96128   96128
#> 5    2015               B   0100      FG2           I AGI2   96128   96128
#> 6    2015               B   0301      FG2           I AGI2 2863193 2863193
#>   I:FG2:AGI2 I:FG1:AGI1
#> 1      11396          0
#> 2      96128          0
#> 5      96128          0
#> 6    2863193          0
#> 
#> 
#> D:FG2:AGD12 = D:FG5:AGD12 - D:800:EIENDOMSSKATT_OG__ANDRE_SKATTER 
#>   periode regnskapsomfang region funksjon kontoklasse   art belop1 belop
#> 1    2015               A   0101      FG2           D AGD12  81734     0
#> 2    2015               A   0100      FG2           D AGD12   -585  -585
#> 5    2015               B   0100      FG2           D AGD12      0     0
#> 6    2015               B   0301      FG2           D AGD12    254   169
#>   D:FG5:AGD12 D:800:EIENDOMSSKATT_OG__ANDRE_SKATTER
#> 1       81734                                 81734
#> 2        -585                                     0
#> 5           0                                     0
#> 6         254                                    85
#> 
#> 
#> D:FG2:AG11 = D:840:AG11 - D:840:AG12 + D:800:AG12 
#>   periode regnskapsomfang region funksjon kontoklasse  art   belop1    belop
#> 1    2015               A   0101      FG2           D AG11  1453498  1453498
#> 2    2015               A   0100      FG2           D AG11  2888387  2888387
#> 5    2015               B   0100      FG2           D AG11  2888387  2888387
#> 6    2015               B   0301      FG2           D AG11 37226846 37226846
#>   D:840:AG11 D:840:AG12 D:800:AG12
#> 1     830619          0     622879
#> 2    1522478          0    1365909
#> 5    1522478          0    1365909
#> 6   10133395          0   27093451
#> 
#> 
#> D:FG2:AGID1 = D:FG2:AGID1 - D:FG2:990 + D:FG2:AGI3 + D:860:990 - I:FG2:AGI3 
#>   periode regnskapsomfang region funksjon kontoklasse   art   belop1    belop
#> 1    2015               A   0101      FG2           D AGID1  3152779  1623908
#> 2    2015               A   0100      FG2           D AGID1  5963780  3138781
#> 5    2015               B   0100      FG2           D AGID1  5958374  3136078
#> 6    2015               B   0301      FG2           D AGID1 66920507 36750203
#>   D:FG2:AGID1 D:FG2:990 D:FG2:AGI3 D:860:990 I:FG2:AGI3
#> 1     3152779     74397   -1539191     74397     -10320
#> 2     5963780    149592   -2907094    149592     -82095
#> 5     5958374    149592   -2904391    149592     -82095
#> 6    66920507   1158681  -32880913   1158681   -2710609
#> 
#> 
#> D:FG2:AGID2 = D:FG2:AGID2 + I:FG2:AGID2 
#>   periode regnskapsomfang region funksjon kontoklasse   art belop1 belop
#> 1    2015               A   0101      FG2           D AGID2      0     0
#> 2    2015               A   0100      FG2           D AGID2      0     0
#> 5    2015               B   0100      FG2           D AGID2      0     0
#> 6    2015               B   0301      FG2           D AGID2      0     0
#>   D:FG2:AGID2 I:FG2:AGID2
#> 1           0           0
#> 2           0           0
#> 5           0           0
#> 6           0           0
#> 
#> 
#> I:FG2:AGI21 = I:FG4:AGI1 - I:FG2:AGI17 
#>   periode regnskapsomfang region funksjon kontoklasse   art belop1  belop
#> 1    2015               A   0101      FG2           I AGI21   1076   1076
#> 2    2015               A   0100      FG2           I AGI21  14033  14033
#> 5    2015               B   0100      FG2           I AGI21  14033  14033
#> 6    2015               B   0301      FG2           I AGI21 152584 152584
#>   I:FG4:AGI1 I:FG2:AGI17
#> 1       1076           0
#> 2      14033           0
#> 5      14033           0
#> 6     152584           0
#> 
#> 
#> D:FG2:AGD45 = D:FG2:AGD45 - D:FG2:A800 + D:840:A800 
#>   periode regnskapsomfang region funksjon kontoklasse   art   belop1    belop
#> 1    2015               A   0101      FG2           D AGD45  1543255  1543255
#> 2    2015               A   0100      FG2           D AGD45  2904391  2904391
#> 5    2015               B   0100      FG2           D AGD45  2904391  2904391
#> 6    2015               B   0301      FG2           D AGD45 37227015 37227015
#>   D:FG2:AGD45 D:FG2:A800 D:840:A800
#> 1     1543255     830619     830619
#> 2     2904391    1522478    1522478
#> 5     2904391    1522478    1522478
#> 6    37227015   10133395   10133395
#> 
#> 
#> D:FG5:AGD62 = D:FG5:AGD62 - D:800:EIENDOMSSKATT_OG__ANDRE_SKATTER 
#>   periode regnskapsomfang region funksjon kontoklasse   art belop1 belop
#> 1    2015               A   0101      FG5           D AGD62  81734     0
#> 2    2015               A   0100      FG5           D AGD62   -585  -585
#> 5    2015               B   0100      FG5           D AGD62      0     0
#> 6    2015               B   0301      FG5           D AGD62    254   169
#>   D:FG5:AGD62 D:800:EIENDOMSSKATT_OG__ANDRE_SKATTER
#> 1       81734                                 81734
#> 2        -585                                     0
#> 5           0                                     0
#> 6         254                                    85
#> 
#> 
#> D:FG2:AGD65 = ( D:FG2:AGD45 - D:FG2:A800 + D:840:A800 ) - D:FG2:AGD46 - D:FG2:AG18 
#>   periode regnskapsomfang region funksjon kontoklasse   art   belop1    belop
#> 1    2015               A   0101      FG2           D AGD65  1543161  1539191
#> 2    2015               A   0100      FG2           D AGD65  2912500  2907094
#> 5    2015               B   0100      FG2           D AGD65  2904391  2904391
#> 6    2015               B   0301      FG2           D AGD65 24391081 24391081
#>   D:FG2:AGD45 D:FG2:A800 D:840:A800 D:FG2:AGD46 D:FG2:AG18
#> 1     1543255     830619     830619        6049      -1985
#> 2     2904391    1522478    1522478           0      -2703
#> 5     2904391    1522478    1522478           0          0
#> 6    37227015   10133395   10133395     4346102    8489832
#> 
#> 
#> D:FG2:AGD22 = D:FG2:AGD30 - D:FG2:AGD31 - D:FG2:AG27 
#>   periode regnskapsomfang region funksjon kontoklasse   art belop1 belop
#> 1    2015               A   0101      FG2           D AGD22      0     0
#> 2    2015               A   0100      FG2           D AGD22      0     0
#> 5    2015               B   0100      FG2           D AGD22      0     0
#> 6    2015               B   0301      FG2           D AGD22      0     0
#>   D:FG2:AGD30 D:FG2:AGD31 D:FG2:AG27
#> 1           0           0          0
#> 2           0           0          0
#> 5           0           0          0
#> 6           0           0          0
#> 
#> 
#> D:FG2:AGD66 = ( ( D:FG2:AGD45 - D:FG2:A800 + D:840:A800 ) - D:FG2:AGD46 - D:FG2:AG18 ) +  ( D:FG2:AGD30 - D:FG2:AGD31 - D:FG2:AG27 ) + D:FG2:990 
#>   periode regnskapsomfang region funksjon kontoklasse   art   belop1    belop
#> 1    2015               A   0101      FG2           D AGD66  1617558  1613588
#> 2    2015               A   0100      FG2           D AGD66  3062092  3056686
#> 5    2015               B   0100      FG2           D AGD66  3053983  3053983
#> 6    2015               B   0301      FG2           D AGD66 25549762 25549762
#>   D:FG2:AGD45 D:FG2:A800 D:840:A800 D:FG2:AGD46 D:FG2:AG18 D:FG2:AGD30
#> 1     1543255     830619     830619        6049      -1985           0
#> 2     2904391    1522478    1522478           0      -2703           0
#> 5     2904391    1522478    1522478           0          0           0
#> 6    37227015   10133395   10133395     4346102    8489832           0
#>   D:FG2:AGD31 D:FG2:AG27 D:FG2:990
#> 1           0          0     74397
#> 2           0          0    149592
#> 5           0          0    149592
#> 6           0          0   1158681
#> 
#> 
#> D:FG2:AGD32 = ( ( ( D:FG2:AGD45 - D:FG2:A800 + D:840:A800 ) - D:FG2:AGD46 - D:FG2:AG18 ) +  ( D:FG2:AGD30 - D:FG2:AGD31 - D:FG2:AG27 ) + D:FG2:990 ) + D:FG2:AG14 - D:FG2:AG7 
#>   periode regnskapsomfang region funksjon kontoklasse   art   belop1    belop
#> 1    2015               A   0101      FG2           D AGD32  1617558  1613588
#> 2    2015               A   0100      FG2           D AGD32  3062092  3056686
#> 5    2015               B   0100      FG2           D AGD32  3053983  3053983
#> 6    2015               B   0301      FG2           D AGD32 25549762 25549762
#>   D:FG2:AGD45 D:FG2:A800 D:840:A800 D:FG2:AGD46 D:FG2:AG18 D:FG2:AGD30
#> 1     1543255     830619     830619        6049      -1985           0
#> 2     2904391    1522478    1522478           0      -2703           0
#> 5     2904391    1522478    1522478           0          0           0
#> 6    37227015   10133395   10133395     4346102    8489832           0
#>   D:FG2:AGD31 D:FG2:AG27 D:FG2:990 D:FG2:AG14 D:FG2:AG7
#> 1           0          0     74397          0         0
#> 2           0          0    149592          0         0
#> 5           0          0    149592          0         0
#> 6           0          0   1158681          0         0
#> 
#> 
#> I:FG2:AGI31 = I:FG2:AGI31 - I:FG1:AGI1 
#>   periode regnskapsomfang region funksjon kontoklasse   art belop1 belop
#> 1    2015               A   0101      FG2           I AGI31    609   609
#> 2    2015               A   0100      FG2           I AGI31  54785 54785
#> 5    2015               B   0100      FG2           I AGI31  54785 54785
#> 6    2015               B   0301      FG2           I AGI31  52830 52830
#>   I:FG2:AGI31 I:FG1:AGI1
#> 1         609          0
#> 2       54785          0
#> 5       54785          0
#> 6       52830          0
#> 
#> 
#> I:FG2:AGI16 = I:FG2:AGI20 + I:FG2:AGI15 - I:FG2:AGI14 + I:FG2:AG18 
#>   periode regnskapsomfang region funksjon kontoklasse   art   belop1    belop
#> 1    2015               A   0101      FG2           I AGI16   -10320   -10320
#> 2    2015               A   0100      FG2           I AGI16   -82095   -82095
#> 5    2015               B   0100      FG2           I AGI16   -82095   -82095
#> 6    2015               B   0301      FG2           I AGI16 -2816269 -2816269
#>   I:FG2:AGI20 I:FG2:AGI15 I:FG2:AGI14 I:FG2:AG18
#> 1        1076           0       11396          0
#> 2       14033           0       96128          0
#> 5       14033           0       96128          0
#> 6      152584           0     2863193    -105660
#> 
#> 
#> I:FG2:AG11 = I:840:AG11 - I:840:AG12 + I:800:AG12 
#>   periode regnskapsomfang region funksjon kontoklasse  art belop1 belop
#> 1    2015               A   0101      FG2           I AG11      0     0
#> 2    2015               A   0100      FG2           I AG11      0     0
#> 5    2015               B   0100      FG2           I AG11      0     0
#> 6    2015               B   0301      FG2           I AG11      0     0
#>   I:840:AG11 I:840:AG12 I:800:AG12
#> 1          0          0          0
#> 2          0          0          0
#> 5          0          0          0
#> 6          0          0          0
#> 
#> 
#> output 
#>        periode regnskapsomfang region funksjon kontoklasse  art belop
#> 1         2015               A   0100     FGF1           D AG35     0
#> 2         2015               A   0100    FGF1a           D AG35     0
#> 175451    2015               B   0101      290           I  950     0
#> 175452    2015               B   0301      290           I  950     0
#> 
#> ================================================================
#> ==============    periode =  2016     ================
#> ==============================================================
#> 
#> data
#>     periode region kontoklasse funksjon art belop
#> 1      2016   0300           D      120 380  2312
#> 2      2016   0300           D      121 380   337
#> 338    2016 030101           D      100 099   178
#> 339    2016 030107           D      100 099   215
#> 
#> 
#> funksjonshierarki
#>     periode   to  from sign
#> 1      2016 FGF1 FGF1a    +
#> 2      2016 FGF1 FGF1b    +
#> 224    2016  FG5   321    -
#> 225    2016  FG5   711    -
#> 
#> 
#> artshierarki
#>     periode    to  from sign
#> 1      2016  LONN   010    +
#> 2      2016  LONN   020    +
#> 542    2016 AGD65 AGD46    -
#> 543    2016 AGD65  AG18    -
#> 
#> 
#> data_saer
#>     periode region kontoklasse funksjon art belop
#> 1      2016   0101           D      110 775  1729
#> 2      2016   0101           D      120 895   296
#> 119    2016   0101           D      110 090   106
#> 120    2016   0101           D      110 099   109
#> 
#> 
#> artshierarki_nettinger
#>    periode    to                   from sign
#> 1     2016  AGD9 KJOP_SALG_OVERFORINGER    -
#> 2     2016 AGD10 KJOP_SALG_OVERFORINGER    -
#> 93    2016 AGD65                   COPY    -
#> 94    2016 AGD22                   COPY    -
#> 
#> 
#> artshierarki_nettinger_kasse
#>    periode    to from sign
#> 1     2016 AGD51 A375    -
#> 2     2016 AGD51  380    -
#> 73    2016 AGD65 COPY    -
#> 74    2016 AGD22 COPY    -
#> 
#> 
#> kombinasjoner
#> NULL
#> 
#> 
#> regioner
#>      x
#> 1 0101
#> 2 0100
#> 3 0301
#> 
#> 
#> storkombinasjoner
#> NULL
#> 
#> 
#> stjernetabell
#>     periode region kontoklasse funksjon   art sign
#> 1      2016 ??????                 FG0*          +
#> 2      2016 ??????                 FG1*          +
#> 162    2016                        !FG2 AGI26    -
#> 163    2016                        !FG2 AGI19    -
#> 
#> 
#> funksjoner
#> NULL
#> 
#> 
#> arter
#> NULL
#> 
#> 
#> kontoklasser
#> NULL
#> 
#> 
#> regnskapsomfang
#> NULL
#> 
#>    periode
#> 15 2016   
#> 16 2016   
#> 17 2016   
#> 18 2016   
#> 19 2016   
#> 20 2016   
#> 21 2016   
#> 22 2016   
#> 23 2016   
#> 24 2016   
#> 25 2016   
#>    formel                                                                       
#> 15 I:FG2:AGI2 = I:FG2:AGI2 - I:FG1:AGI1                                         
#> 16 FG2:AGD12 = FG5:AGD12 - 800:EIENDOMSSKATT_OG__ANDRE_SKATTER                  
#> 17 FG2:AG11 = 840:AG11 - 840:AG12 + 800:AG12                                    
#> 18 D:FG2:AGID1 = D:FG2:AGID1 - D:FG2:990 + D:FG2:AGI3 + D:860:990 - I:FG2:AGI3  
#> 19 D:FG2:AGID2 = D:FG2:AGID2 + I:FG2:AGID2                                      
#> 20 I:FG2:AGI21 = I:FG4:AGI1 - I:FG2:AGI17                                       
#> 21 I:FG2:AGI28 = I:FG2:AG6 - I:FG2:AG13                                         
#> 22 D:FG2:AGD45 = D:FG2:AGD45 - D:FG2:A800 + D:840:A800 - D:FG2:AG10 + D:800:AG10
#> 23 FG5:AGD62 = FG5:AGD62 - 800:EIENDOMSSKATT_OG__ANDRE_SKATTER                  
#> 24 FG2:AGD64 = - FG2:AGD64                                                      
#> 25 D:FG2:AGD65 = D:FG2:AGD45 - D:FG2:AGD46 - D:FG2:AG18                         
#> 
#>  
#>  --- Kjører AutoFormel ----
#> 
#>  
#>  --- Etter AutoFormel ----
#>    periode
#> 1  2016   
#> 2  2016   
#> 3  2016   
#> 4  2016   
#> 5  2016   
#> 6  2016   
#> 7  2016   
#> 8  2016   
#> 9  2016   
#> 10 2016   
#> 11 2016   
#> 12 2016   
#> 13 2016   
#> 14 2016   
#> 15 2016   
#>    formel                                                                                                      
#> 1  I:FG2:AGI2 = I:FG2:AGI2 - I:FG1:AGI1                                                                        
#> 2  D:FG2:AGD12 = D:FG5:AGD12 - D:800:EIENDOMSSKATT_OG__ANDRE_SKATTER                                           
#> 3  D:FG2:AG11 = D:840:AG11 - D:840:AG12 + D:800:AG12                                                           
#> 4  D:FG2:AGID1 = D:FG2:AGID1 - D:FG2:990 + D:FG2:AGI3 + D:860:990 - I:FG2:AGI3                                 
#> 5  D:FG2:AGID2 = D:FG2:AGID2 + I:FG2:AGID2                                                                     
#> 6  I:FG2:AGI21 = I:FG4:AGI1 - I:FG2:AGI17                                                                      
#> 7  I:FG2:AGI28 = I:FG2:AG6 - I:FG2:AG13                                                                        
#> 8  D:FG2:AGD45 = D:FG2:AGD45 - D:FG2:A800 + D:840:A800 - D:FG2:AG10 + D:800:AG10                               
#> 9  D:FG5:AGD62 = D:FG5:AGD62 - D:800:EIENDOMSSKATT_OG__ANDRE_SKATTER                                           
#> 10 D:FG2:AGD64 = - D:FG2:AGD64                                                                                 
#> 11 D:FG2:AGD65 = ( D:FG2:AGD45 - D:FG2:A800 + D:840:A800 - D:FG2:AG10 + D:800:AG10 ) - D:FG2:AGD46 - D:FG2:AG18
#> 12 I:FG2:AGD12 = I:FG5:AGD12 - I:800:EIENDOMSSKATT_OG__ANDRE_SKATTER                                           
#> 13 I:FG2:AG11 = I:840:AG11 - I:840:AG12 + I:800:AG12                                                           
#> 14 I:FG5:AGD62 = I:FG5:AGD62 - I:800:EIENDOMSSKATT_OG__ANDRE_SKATTER                                           
#> 15 I:FG2:AGD64 = - I:FG2:AGD64                                                                                 
#> 
#>  AutoNetting:  artshierarki_nettinger_kasse  fra  74  til  93  rader
#> 
#>  AutoNetting:  artshierarki_nettinger  fra  94  til  113  rader
#> 
#>  [ Kombinasjonsberegninger...
#> 
#>  region:funksjon    615 elements
#>            region+funksjon    463 elements
#>            region-funksjon    346 elements
#> 
#> 
#>  kontoklasse:art    418 elements
#>            kontoklasse+art    362 elements
#>            kontoklasse-art    274 elements
#> 
#> 
#>  region:funksjon:kontoklasse:art    94804 elements
#>  [ RowGroups...]           funksjon-art    82017 elements
#>            region-art    77889 elements
#> ] [ Fra til storkombinasjoner til kombinasjoner og regioner ... RowGroups(...)]
#> 
#>                       Kasse : 
#>      Saerbedrift + Nettinger: 
#>              Kasse-nettinger: 
#>                                [ output='data.frame'...........::...]
#> 
#> 
#> 
#> [--------------------------------------------------------> 
#> [-------- Egne beregninger for formelkorreksjoner --------->
#> [-------------------------------------------------------->
#> 
#> Warning: Formelen I:FG2:AGD12 = I:FG5:AGD12 - I:800:EIENDOMSSKATT_OG__ANDRE_SKATTER samsvarer med 0 rader i data
#> Warning: Formelen I:FG5:AGD62 = I:FG5:AGD62 - I:800:EIENDOMSSKATT_OG__ANDRE_SKATTER samsvarer med 0 rader i data
#> Warning: Formelen I:FG2:AGD64 = - I:FG2:AGD64 samsvarer med 0 rader i data
#> 
#>  [ Kombinasjonsberegninger...] [ Lager kombinasjoner ...]
#> 
#>                       Kasse : 
#>      Saerbedrift + Nettinger: 
#>              Kasse-nettinger: 
#>                                [ output='data.frame'.....]
#> 
#> 
#> I:FG2:AGI2 = I:FG2:AGI2 - I:FG1:AGI1 
#>   periode regnskapsomfang region funksjon kontoklasse  art belop1  belop
#> 1    2016               A   0101      FG2           I AGI2  25727  25727
#> 2    2016               A   0100      FG2           I AGI2 102914 102914
#> 5    2016               B   0100      FG2           I AGI2 102914 102914
#> 6    2016               B   0301      FG2           I AGI2 329593 329593
#>   I:FG2:AGI2 I:FG1:AGI1
#> 1      25727          0
#> 2     102914          0
#> 5     102914          0
#> 6     329593          0
#> 
#> 
#> D:FG2:AGD12 = D:FG5:AGD12 - D:800:EIENDOMSSKATT_OG__ANDRE_SKATTER 
#>   periode regnskapsomfang region funksjon kontoklasse   art belop1 belop
#> 1    2016               A   0101      FG2           D AGD12  78151     0
#> 2    2016               A   0100      FG2           D AGD12   -591  -591
#> 5    2016               B   0100      FG2           D AGD12      0     0
#> 6    2016               B   0301      FG2           D AGD12 254452   142
#>   D:FG5:AGD12 D:800:EIENDOMSSKATT_OG__ANDRE_SKATTER
#> 1       78151                                 78151
#> 2        -591                                     0
#> 5           0                                     0
#> 6      254452                                254310
#> 
#> 
#> D:FG2:AG11 = D:840:AG11 - D:840:AG12 + D:800:AG12 
#>   periode regnskapsomfang region funksjon kontoklasse  art   belop1    belop
#> 1    2016               A   0101      FG2           D AG11  1540819  1540819
#> 2    2016               A   0100      FG2           D AG11  3004400  3004400
#> 5    2016               B   0100      FG2           D AG11  3004400  3004400
#> 6    2016               B   0301      FG2           D AG11 40118885 40118885
#>   D:840:AG11 D:840:AG12 D:800:AG12
#> 1     846004          0     694815
#> 2    1515453          0    1488947
#> 5    1515453          0    1488947
#> 6    9530850          0   30588035
#> 
#> 
#> D:FG2:AGID1 = D:FG2:AGID1 - D:FG2:990 + D:FG2:AGI3 + D:860:990 - I:FG2:AGI3 
#>   periode regnskapsomfang region funksjon kontoklasse   art   belop1    belop
#> 1    2016               A   0101      FG2           D AGID1  3318831  1720007
#> 2    2016               A   0100      FG2           D AGID1  6200061  3265075
#> 5    2016               B   0100      FG2           D AGID1  6192539  3261314
#> 6    2016               B   0301      FG2           D AGID1 72454088 36926256
#>   D:FG2:AGID1 D:FG2:990 D:FG2:AGI3 D:860:990 I:FG2:AGI3
#> 1     3318831     76201   -1621315     76201     -22491
#> 2     6200061    157759   -3021151    157759     -86165
#> 5     6192539    157759   -3017390    157759     -86165
#> 6    72454088   1184846  -35634621   1184846    -106789
#> 
#> 
#> D:FG2:AGID2 = D:FG2:AGID2 + I:FG2:AGID2 
#>   periode regnskapsomfang region funksjon kontoklasse   art belop1 belop
#> 1    2016               A   0101      FG2           D AGID2      0     0
#> 2    2016               A   0100      FG2           D AGID2      0     0
#> 5    2016               B   0100      FG2           D AGID2      0     0
#> 6    2016               B   0301      FG2           D AGID2      0     0
#>   D:FG2:AGID2 I:FG2:AGID2
#> 1           0           0
#> 2           0           0
#> 5           0           0
#> 6           0           0
#> 
#> 
#> I:FG2:AGI21 = I:FG4:AGI1 - I:FG2:AGI17 
#>   periode regnskapsomfang region funksjon kontoklasse   art belop1  belop
#> 1    2016               A   0101      FG2           I AGI21   3236   3236
#> 2    2016               A   0100      FG2           I AGI21  16749  16749
#> 5    2016               B   0100      FG2           I AGI21  16749  16749
#> 6    2016               B   0301      FG2           I AGI21 222804 222804
#>   I:FG4:AGI1 I:FG2:AGI17
#> 1       3236           0
#> 2      16749           0
#> 5      16749           0
#> 6     222804           0
#> 
#> 
#> I:FG2:AGI28 = I:FG2:AG6 - I:FG2:AG13 
#>   periode regnskapsomfang region funksjon kontoklasse   art belop1 belop
#> 1    2016               A   0101      FG2           I AGI28      0     0
#> 2    2016               A   0100      FG2           I AGI28      0     0
#> 5    2016               B   0100      FG2           I AGI28      0     0
#> 6    2016               B   0301      FG2           I AGI28      0     0
#>   I:FG2:AG6 I:FG2:AG13
#> 1         0          0
#> 2         0          0
#> 5         0          0
#> 6         0          0
#> 
#> 
#> D:FG2:AGD45 = D:FG2:AGD45 - D:FG2:A800 + D:840:A800 - D:FG2:AG10 + D:800:AG10 
#>   periode regnskapsomfang region funksjon kontoklasse   art   belop1    belop
#> 1    2016               A   0101      FG2           D AGD45  1625945  1625945
#> 2    2016               A   0100      FG2           D AGD45  3017390  3017390
#> 5    2016               B   0100      FG2           D AGD45  3017390  3017390
#> 6    2016               B   0301      FG2           D AGD45 40373040 40373040
#>   D:FG2:AGD45 D:FG2:A800 D:840:A800 D:FG2:AG10 D:800:AG10
#> 1     1625945     846004     846004      78110      78110
#> 2     3017390    1515453    1515453          0          0
#> 5     3017390    1515453    1515453          0          0
#> 6    40373040    9530850    9530850     254013     254013
#> 
#> 
#> D:FG5:AGD62 = D:FG5:AGD62 - D:800:EIENDOMSSKATT_OG__ANDRE_SKATTER 
#>   periode regnskapsomfang region funksjon kontoklasse   art belop1 belop
#> 1    2016               A   0101      FG5           D AGD62  78151     0
#> 2    2016               A   0100      FG5           D AGD62   -591  -591
#> 5    2016               B   0100      FG5           D AGD62      0     0
#> 6    2016               B   0301      FG5           D AGD62 254452   142
#>   D:FG5:AGD62 D:800:EIENDOMSSKATT_OG__ANDRE_SKATTER
#> 1       78151                                 78151
#> 2        -591                                     0
#> 5           0                                     0
#> 6      254452                                254310
#> 
#> 
#> D:FG2:AGD64 = - D:FG2:AGD64 
#>   periode regnskapsomfang region funksjon kontoklasse   art belop1 belop
#> 1    2016               A   0101      FG2           D AGD64      0     0
#> 2    2016               A   0100      FG2           D AGD64      0     0
#> 5    2016               B   0100      FG2           D AGD64      0     0
#> 6    2016               B   0301      FG2           D AGD64      0     0
#>   D:FG2:AGD64
#> 1           0
#> 2           0
#> 5           0
#> 6           0
#> 
#> 
#> D:FG2:AGD65 = ( D:FG2:AGD45 - D:FG2:A800 + D:840:A800 - D:FG2:AG10 + D:800:AG10 ) - D:FG2:AGD46 - D:FG2:AG18 
#>   periode regnskapsomfang region funksjon kontoklasse   art   belop1    belop
#> 1    2016               A   0101      FG2           D AGD65  1621315  1621315
#> 2    2016               A   0100      FG2           D AGD65  3021151  3021151
#> 5    2016               B   0100      FG2           D AGD65  3017390  3017390
#> 6    2016               B   0301      FG2           D AGD65 26303535 26303535
#>   D:FG2:AGD45 D:FG2:A800 D:840:A800 D:FG2:AG10 D:800:AG10 D:FG2:AGD46
#> 1     1625945     846004     846004      78110      78110        6655
#> 2     3017390    1515453    1515453          0          0           0
#> 5     3017390    1515453    1515453          0          0           0
#> 6    40373040    9530850    9530850     254013     254013     4738419
#>   D:FG2:AG18
#> 1      -2025
#> 2      -3761
#> 5          0
#> 6    9331086
#> 
#> 
#> I:FG2:AG11 = I:840:AG11 - I:840:AG12 + I:800:AG12 
#>   periode regnskapsomfang region funksjon kontoklasse  art belop1 belop
#> 1    2016               A   0101      FG2           I AG11      0     0
#> 2    2016               A   0100      FG2           I AG11      0     0
#> 5    2016               B   0100      FG2           I AG11      0     0
#> 6    2016               B   0301      FG2           I AG11      0     0
#>   I:840:AG11 I:840:AG12 I:800:AG12
#> 1          0          0          0
#> 2          0          0          0
#> 5          0          0          0
#> 6          0          0          0
#> 
#> 
#> output 
#>        periode regnskapsomfang region funksjon kontoklasse   art belop
#> 1         2016               A   0100     FGF1           D  AG35     0
#> 2         2016               A   0100    FGF1a           D  AG35     0
#> 155777    2016               B   0100      FG2           I AGI10     0
#> 155778    2016               B   0301      FG2           I AGI10     0
#> 

   zFem  <- KostraRegnskap(inputdata,funksjonshierarki,artshierarki,inputdata_saer,artshierarki_nettinger,artshierarki_nettinger_kasse,stjernetabell=stjerne,regioner = c("0101", "0100",  "0301"),output="fire",formler=formler)
#> ================================================================
#> ==============    periode =  2015     ================
#> ==============================================================
#> 
#> data
#>     periode region kontoklasse funksjon art belop
#> 1      2015   0300           D      120 380   847
#> 2      2015   0300           D      121 380   288
#> 291    2015 030107           D      100 090   157
#> 292    2015 030107           D      100 099   364
#> 
#> 
#> funksjonshierarki
#>     periode   to  from sign
#> 1      2015 FGF1 FGF1a    +
#> 2      2015 FGF1 FGF1b    +
#> 225    2015  FG5   321    -
#> 226    2015  FG5   711    -
#> 
#> 
#> artshierarki
#>     periode    to from sign
#> 1      2015  LONN  010    +
#> 2      2015  LONN  020    +
#> 627    2015 AI660  660    +
#> 628    2015 AD990  990    +
#> 
#> 
#> data_saer
#>    periode region kontoklasse funksjon art belop
#> 1     2015   0100           D      515 375   573
#> 2     2015   0301           D      515 375   573
#> 61    2015   0301           D      100 099   100
#> 62    2015   0101           D      110 099   108
#> 
#> 
#> artshierarki_nettinger
#>    periode    to                   from sign
#> 1     2015  AGD9 KJOP_SALG_OVERFORINGER    -
#> 2     2015 AGD10 KJOP_SALG_OVERFORINGER    -
#> 83    2015 AGI31 KJOP_SALG_OVERFORINGER    -
#> 84    2015 AGI31                 RENTER    -
#> 
#> 
#> artshierarki_nettinger_kasse
#>    periode    to from sign
#> 1     2015 AGD51 A375    -
#> 2     2015 AGD51  380    -
#> 59    2015 AGI14 COPY    -
#> 60    2015 AGI19 COPY    -
#> 
#> 
#> kombinasjoner
#> NULL
#> 
#> 
#> regioner
#>      x
#> 1 0101
#> 2 0100
#> 3 0301
#> 
#> 
#> storkombinasjoner
#> NULL
#> 
#> 
#> stjernetabell
#>     periode region kontoklasse funksjon   art sign
#> 1      2015 ??????                 FG0*          +
#> 2      2015 ??????                 FG1*          +
#> 159    2015                        !FG2 AGI26    -
#> 160    2015                        !FG2 AGI19    -
#> 
#> 
#> funksjoner
#> NULL
#> 
#> 
#> arter
#> NULL
#> 
#> 
#> kontoklasser
#> NULL
#> 
#> 
#> regnskapsomfang
#> NULL
#> 
#>    periode
#> 1  2015   
#> 2  2015   
#> 3  2015   
#> 4  2015   
#> 5  2015   
#> 6  2015   
#> 7  2015   
#> 8  2015   
#> 9  2015   
#> 10 2015   
#> 11 2015   
#> 12 2015   
#> 13 2015   
#> 14 2015   
#>    formel                                                                     
#> 1  I:FG2:AGI2 = I:FG2:AGI2 - I:FG1:AGI1                                       
#> 2  FG2:AGD12 = FG5:AGD12 - 800:EIENDOMSSKATT_OG__ANDRE_SKATTER                
#> 3  FG2:AG11 = 840:AG11 - 840:AG12 + 800:AG12                                  
#> 4  D:FG2:AGID1 = D:FG2:AGID1 - D:FG2:990 + D:FG2:AGI3 + D:860:990 - I:FG2:AGI3
#> 5  D:FG2:AGID2 = D:FG2:AGID2 + I:FG2:AGID2                                    
#> 6  I:FG2:AGI21 = I:FG4:AGI1 - I:FG2:AGI17                                     
#> 7  D:FG2:AGD45 = D:FG2:AGD45 - D:FG2:A800 + D:840:A800                        
#> 8  FG5:AGD62 = FG5:AGD62 - 800:EIENDOMSSKATT_OG__ANDRE_SKATTER                
#> 9  D:FG2:AGD65 = D:FG2:AGD45 - D:FG2:AGD46 - D:FG2:AG18                       
#> 10 D:FG2:AGD22 = D:FG2:AGD30 - D:FG2:AGD31 - D:FG2:AG27                       
#> 11 D:FG2:AGD66 = D:FG2:AGD65 +  D:FG2:AGD22 + D:FG2:990                       
#> 12 D:FG2:AGD32 = D:FG2:AGD66 + D:FG2:AG14 - D:FG2:AG7                         
#> 13 I:FG2:AGI31 = I:FG2:AGI31 - I:FG1:AGI1                                     
#> 14 I:FG2:AGI16 = I:FG2:AGI20 + I:FG2:AGI15 - I:FG2:AGI14 + I:FG2:AG18         
#> 
#>  
#>  --- Kjører AutoFormel ----
#> 
#>  
#>  --- Etter AutoFormel ----
#>    periode
#> 1  2015   
#> 2  2015   
#> 3  2015   
#> 4  2015   
#> 5  2015   
#> 6  2015   
#> 7  2015   
#> 8  2015   
#> 9  2015   
#> 10 2015   
#> 11 2015   
#> 12 2015   
#> 13 2015   
#> 14 2015   
#> 15 2015   
#> 16 2015   
#> 17 2015   
#>    formel                                                                                                                                                                       
#> 1  I:FG2:AGI2 = I:FG2:AGI2 - I:FG1:AGI1                                                                                                                                         
#> 2  D:FG2:AGD12 = D:FG5:AGD12 - D:800:EIENDOMSSKATT_OG__ANDRE_SKATTER                                                                                                            
#> 3  D:FG2:AG11 = D:840:AG11 - D:840:AG12 + D:800:AG12                                                                                                                            
#> 4  D:FG2:AGID1 = D:FG2:AGID1 - D:FG2:990 + D:FG2:AGI3 + D:860:990 - I:FG2:AGI3                                                                                                  
#> 5  D:FG2:AGID2 = D:FG2:AGID2 + I:FG2:AGID2                                                                                                                                      
#> 6  I:FG2:AGI21 = I:FG4:AGI1 - I:FG2:AGI17                                                                                                                                       
#> 7  D:FG2:AGD45 = D:FG2:AGD45 - D:FG2:A800 + D:840:A800                                                                                                                          
#> 8  D:FG5:AGD62 = D:FG5:AGD62 - D:800:EIENDOMSSKATT_OG__ANDRE_SKATTER                                                                                                            
#> 9  D:FG2:AGD65 = ( D:FG2:AGD45 - D:FG2:A800 + D:840:A800 ) - D:FG2:AGD46 - D:FG2:AG18                                                                                           
#> 10 D:FG2:AGD22 = D:FG2:AGD30 - D:FG2:AGD31 - D:FG2:AG27                                                                                                                         
#> 11 D:FG2:AGD66 = ( ( D:FG2:AGD45 - D:FG2:A800 + D:840:A800 ) - D:FG2:AGD46 - D:FG2:AG18 ) +  ( D:FG2:AGD30 - D:FG2:AGD31 - D:FG2:AG27 ) + D:FG2:990                             
#> 12 D:FG2:AGD32 = ( ( ( D:FG2:AGD45 - D:FG2:A800 + D:840:A800 ) - D:FG2:AGD46 - D:FG2:AG18 ) +  ( D:FG2:AGD30 - D:FG2:AGD31 - D:FG2:AG27 ) + D:FG2:990 ) + D:FG2:AG14 - D:FG2:AG7
#> 13 I:FG2:AGI31 = I:FG2:AGI31 - I:FG1:AGI1                                                                                                                                       
#> 14 I:FG2:AGI16 = I:FG2:AGI20 + I:FG2:AGI15 - I:FG2:AGI14 + I:FG2:AG18                                                                                                           
#> 15 I:FG2:AGD12 = I:FG5:AGD12 - I:800:EIENDOMSSKATT_OG__ANDRE_SKATTER                                                                                                            
#> 16 I:FG2:AG11 = I:840:AG11 - I:840:AG12 + I:800:AG12                                                                                                                            
#> 17 I:FG5:AGD62 = I:FG5:AGD62 - I:800:EIENDOMSSKATT_OG__ANDRE_SKATTER                                                                                                            
#> 
#>  AutoNetting:  artshierarki_nettinger_kasse  fra  60  til  75  rader
#> 
#>  AutoNetting:  artshierarki_nettinger  fra  84  til  99  rader
#> 
#>  [ Kombinasjonsberegninger...
#> 
#>  region:funksjon    618 elements
#>            region+funksjon    466 elements
#>            region-funksjon    348 elements
#> 
#> 
#>  kontoklasse:art    458 elements
#>            kontoklasse+art    398 elements
#>            kontoklasse-art    298 elements
#> 
#> 
#>  region:funksjon:kontoklasse:art    103704 elements
#>  [ RowGroups...]           funksjon-art    91878 elements
#>            region-art    87726 elements
#> ] [ Fra til storkombinasjoner til kombinasjoner og regioner ... RowGroups(...)]
#> 
#>                       Kasse : 
#>      Saerbedrift + Nettinger: 
#>              Kasse-nettinger: 
#>                   Nettinger : 
#>                                [ output='data.frame'...........::...]
#> 
#> 
#> 
#> [--------------------------------------------------------> 
#> [-------- Egne beregninger for formelkorreksjoner --------->
#> [-------------------------------------------------------->
#> 
#> Warning: Formelen I:FG2:AGD12 = I:FG5:AGD12 - I:800:EIENDOMSSKATT_OG__ANDRE_SKATTER samsvarer med 0 rader i data
#> Warning: Formelen I:FG5:AGD62 = I:FG5:AGD62 - I:800:EIENDOMSSKATT_OG__ANDRE_SKATTER samsvarer med 0 rader i data
#> 
#>  [ Kombinasjonsberegninger...] [ Lager kombinasjoner ...]
#> 
#>                       Kasse : 
#>      Saerbedrift + Nettinger: 
#>              Kasse-nettinger: 
#>                   Nettinger : 
#>                                [ output='data.frame'.....]
#> 
#> 
#> output 
#>       periode region funksjon kontoklasse  art kasse saerbedrift
#> 1        2015   0100     FGF1           D AG35     0           0
#> 2        2015   0100    FGF1a           D AG35     0           0
#> 87725    2015   0101      290           I  950     0           0
#> 87726    2015   0301      290           I  950     0           0
#>       nettinger_kasse nettinger_saer konsern
#> 1                   0              0       0
#> 2                   0              0       0
#> 87725               0              0       0
#> 87726               0              0       0
#> 
#> ================================================================
#> ==============    periode =  2016     ================
#> ==============================================================
#> 
#> data
#>     periode region kontoklasse funksjon art belop
#> 1      2016   0300           D      120 380  2312
#> 2      2016   0300           D      121 380   337
#> 338    2016 030101           D      100 099   178
#> 339    2016 030107           D      100 099   215
#> 
#> 
#> funksjonshierarki
#>     periode   to  from sign
#> 1      2016 FGF1 FGF1a    +
#> 2      2016 FGF1 FGF1b    +
#> 224    2016  FG5   321    -
#> 225    2016  FG5   711    -
#> 
#> 
#> artshierarki
#>     periode    to  from sign
#> 1      2016  LONN   010    +
#> 2      2016  LONN   020    +
#> 542    2016 AGD65 AGD46    -
#> 543    2016 AGD65  AG18    -
#> 
#> 
#> data_saer
#>     periode region kontoklasse funksjon art belop
#> 1      2016   0101           D      110 775  1729
#> 2      2016   0101           D      120 895   296
#> 119    2016   0101           D      110 090   106
#> 120    2016   0101           D      110 099   109
#> 
#> 
#> artshierarki_nettinger
#>    periode    to                   from sign
#> 1     2016  AGD9 KJOP_SALG_OVERFORINGER    -
#> 2     2016 AGD10 KJOP_SALG_OVERFORINGER    -
#> 93    2016 AGD65                   COPY    -
#> 94    2016 AGD22                   COPY    -
#> 
#> 
#> artshierarki_nettinger_kasse
#>    periode    to from sign
#> 1     2016 AGD51 A375    -
#> 2     2016 AGD51  380    -
#> 73    2016 AGD65 COPY    -
#> 74    2016 AGD22 COPY    -
#> 
#> 
#> kombinasjoner
#> NULL
#> 
#> 
#> regioner
#>      x
#> 1 0101
#> 2 0100
#> 3 0301
#> 
#> 
#> storkombinasjoner
#> NULL
#> 
#> 
#> stjernetabell
#>     periode region kontoklasse funksjon   art sign
#> 1      2016 ??????                 FG0*          +
#> 2      2016 ??????                 FG1*          +
#> 162    2016                        !FG2 AGI26    -
#> 163    2016                        !FG2 AGI19    -
#> 
#> 
#> funksjoner
#> NULL
#> 
#> 
#> arter
#> NULL
#> 
#> 
#> kontoklasser
#> NULL
#> 
#> 
#> regnskapsomfang
#> NULL
#> 
#>    periode
#> 15 2016   
#> 16 2016   
#> 17 2016   
#> 18 2016   
#> 19 2016   
#> 20 2016   
#> 21 2016   
#> 22 2016   
#> 23 2016   
#> 24 2016   
#> 25 2016   
#>    formel                                                                       
#> 15 I:FG2:AGI2 = I:FG2:AGI2 - I:FG1:AGI1                                         
#> 16 FG2:AGD12 = FG5:AGD12 - 800:EIENDOMSSKATT_OG__ANDRE_SKATTER                  
#> 17 FG2:AG11 = 840:AG11 - 840:AG12 + 800:AG12                                    
#> 18 D:FG2:AGID1 = D:FG2:AGID1 - D:FG2:990 + D:FG2:AGI3 + D:860:990 - I:FG2:AGI3  
#> 19 D:FG2:AGID2 = D:FG2:AGID2 + I:FG2:AGID2                                      
#> 20 I:FG2:AGI21 = I:FG4:AGI1 - I:FG2:AGI17                                       
#> 21 I:FG2:AGI28 = I:FG2:AG6 - I:FG2:AG13                                         
#> 22 D:FG2:AGD45 = D:FG2:AGD45 - D:FG2:A800 + D:840:A800 - D:FG2:AG10 + D:800:AG10
#> 23 FG5:AGD62 = FG5:AGD62 - 800:EIENDOMSSKATT_OG__ANDRE_SKATTER                  
#> 24 FG2:AGD64 = - FG2:AGD64                                                      
#> 25 D:FG2:AGD65 = D:FG2:AGD45 - D:FG2:AGD46 - D:FG2:AG18                         
#> 
#>  
#>  --- Kjører AutoFormel ----
#> 
#>  
#>  --- Etter AutoFormel ----
#>    periode
#> 1  2016   
#> 2  2016   
#> 3  2016   
#> 4  2016   
#> 5  2016   
#> 6  2016   
#> 7  2016   
#> 8  2016   
#> 9  2016   
#> 10 2016   
#> 11 2016   
#> 12 2016   
#> 13 2016   
#> 14 2016   
#> 15 2016   
#>    formel                                                                                                      
#> 1  I:FG2:AGI2 = I:FG2:AGI2 - I:FG1:AGI1                                                                        
#> 2  D:FG2:AGD12 = D:FG5:AGD12 - D:800:EIENDOMSSKATT_OG__ANDRE_SKATTER                                           
#> 3  D:FG2:AG11 = D:840:AG11 - D:840:AG12 + D:800:AG12                                                           
#> 4  D:FG2:AGID1 = D:FG2:AGID1 - D:FG2:990 + D:FG2:AGI3 + D:860:990 - I:FG2:AGI3                                 
#> 5  D:FG2:AGID2 = D:FG2:AGID2 + I:FG2:AGID2                                                                     
#> 6  I:FG2:AGI21 = I:FG4:AGI1 - I:FG2:AGI17                                                                      
#> 7  I:FG2:AGI28 = I:FG2:AG6 - I:FG2:AG13                                                                        
#> 8  D:FG2:AGD45 = D:FG2:AGD45 - D:FG2:A800 + D:840:A800 - D:FG2:AG10 + D:800:AG10                               
#> 9  D:FG5:AGD62 = D:FG5:AGD62 - D:800:EIENDOMSSKATT_OG__ANDRE_SKATTER                                           
#> 10 D:FG2:AGD64 = - D:FG2:AGD64                                                                                 
#> 11 D:FG2:AGD65 = ( D:FG2:AGD45 - D:FG2:A800 + D:840:A800 - D:FG2:AG10 + D:800:AG10 ) - D:FG2:AGD46 - D:FG2:AG18
#> 12 I:FG2:AGD12 = I:FG5:AGD12 - I:800:EIENDOMSSKATT_OG__ANDRE_SKATTER                                           
#> 13 I:FG2:AG11 = I:840:AG11 - I:840:AG12 + I:800:AG12                                                           
#> 14 I:FG5:AGD62 = I:FG5:AGD62 - I:800:EIENDOMSSKATT_OG__ANDRE_SKATTER                                           
#> 15 I:FG2:AGD64 = - I:FG2:AGD64                                                                                 
#> 
#>  AutoNetting:  artshierarki_nettinger_kasse  fra  74  til  93  rader
#> 
#>  AutoNetting:  artshierarki_nettinger  fra  94  til  113  rader
#> 
#>  [ Kombinasjonsberegninger...
#> 
#>  region:funksjon    615 elements
#>            region+funksjon    463 elements
#>            region-funksjon    346 elements
#> 
#> 
#>  kontoklasse:art    418 elements
#>            kontoklasse+art    362 elements
#>            kontoklasse-art    274 elements
#> 
#> 
#>  region:funksjon:kontoklasse:art    94804 elements
#>  [ RowGroups...]           funksjon-art    82017 elements
#>            region-art    77889 elements
#> ] [ Fra til storkombinasjoner til kombinasjoner og regioner ... RowGroups(...)]
#> 
#>                       Kasse : 
#>      Saerbedrift + Nettinger: 
#>              Kasse-nettinger: 
#>                   Nettinger : 
#>                                [ output='data.frame'...........::...]
#> 
#> 
#> 
#> [--------------------------------------------------------> 
#> [-------- Egne beregninger for formelkorreksjoner --------->
#> [-------------------------------------------------------->
#> 
#> Warning: Formelen I:FG2:AGD12 = I:FG5:AGD12 - I:800:EIENDOMSSKATT_OG__ANDRE_SKATTER samsvarer med 0 rader i data
#> Warning: Formelen I:FG5:AGD62 = I:FG5:AGD62 - I:800:EIENDOMSSKATT_OG__ANDRE_SKATTER samsvarer med 0 rader i data
#> Warning: Formelen I:FG2:AGD64 = - I:FG2:AGD64 samsvarer med 0 rader i data
#> 
#>  [ Kombinasjonsberegninger...] [ Lager kombinasjoner ...]
#> 
#>                       Kasse : 
#>      Saerbedrift + Nettinger: 
#>              Kasse-nettinger: 
#>                   Nettinger : 
#>                                [ output='data.frame'.....]
#> 
#> 
#> output 
#>       periode region funksjon kontoklasse   art kasse saerbedrift
#> 1        2016   0100     FGF1           D  AG35     0           0
#> 2        2016   0100    FGF1a           D  AG35     0           0
#> 77888    2016   0100      FG2           I AGI10     0           0
#> 77889    2016   0301      FG2           I AGI10     0           0
#>       nettinger_kasse nettinger_saer konsern
#> 1                   0              0       0
#> 2                   0              0       0
#> 77888               0              0       0
#> 77889               0              0       0
#> 


 # ==========================================================================
 #   Gamle eksempler uten data_saer og artshierarki_nettinger
 #===========================================================================

if (!lesDataFraS){
  dat <- kData$data
  datS <- kData$data_saer
  dat$kontoklasse <- CharacterReCode(dat$kontoklasse, c("D", "I"), c("1", "2"))
  datS$kontoklasse <- CharacterReCode(datS$kontoklasse, c("D", "I"), c("3", "4"))
  inputdata <- rbind(dat, datS)
  funksjonshierarki <- kData$funksjonshierarki
  artshierarki <- kData$artshierarki
  z <- KostraRegnskap(inputdata,funksjonshierarki,artshierarki)
  kombinasjoner <- unique(z[, c("art", "funksjon", "kontoklasse")])
} else {
dataPath <- "S:/Prosjekt/2228 KOSTRA publisering/Metode/Regnskap"
#dataPath <- "C:\Users\oyl\Documents\work\Rworking\regnskap"

funksjonFil <- paste(dataPath,"Funksjonshierarki.csv",sep="/")
artFil      <- paste(dataPath,"Artshierarki_uten_nettinger.csv",sep="/")
dataFil     <- paste(dataPath,"Inputdata_kommuner_fylker_2016.csv",sep="/")
kombinasjonerFil <- paste(dataPath,"Alle kombinasjoner vi trenger som output.csv",sep="/")

funksjonshierarki <- read.csv2(funksjonFil,colClasses=c("integer","character","character","character"))
artshierarki <-  read.csv2(artFil,colClasses=c("integer","character","character","character","integer"))
inputdata <-  read.csv2(dataFil,colClasses=c("integer","character","character","character","character","integer"))
kombinasjoner <- read.csv2(kombinasjonerFil,colClasses="character")

# Endrer navn i data fra fylkesregion til region
names(inputdata)[2] <- "region"

inputdata$region = FixRegionkode(inputdata$region) # ledende 0-er legges til
}
#> ================================================================
#> ==============    periode =  2015     ================
#> ==============================================================
#> 
#> data
#>     periode region kontoklasse funksjon art belop
#> 1      2015   0300           1      120 380   847
#> 2      2015   0300           1      121 380   288
#> 353    2015   0301           3      100 099   100
#> 354    2015   0101           3      110 099   108
#> 
#> 
#> funksjonshierarki
#>     periode   to  from sign
#> 1      2015 FGF1 FGF1a    +
#> 2      2015 FGF1 FGF1b    +
#> 225    2015  FG5   321    -
#> 226    2015  FG5   711    -
#> 
#> 
#> artshierarki
#>     periode    to from sign
#> 1      2015  LONN  010    +
#> 2      2015  LONN  020    +
#> 627    2015 AI660  660    +
#> 628    2015 AD990  990    +
#> 
#> 
#> data_saer
#> NULL
#> 
#> 
#> artshierarki_nettinger
#> NULL
#> 
#> 
#> artshierarki_nettinger_kasse
#> NULL
#> 
#> 
#> kombinasjoner
#> NULL
#> 
#> 
#> regioner
#> NULL
#> 
#> 
#> storkombinasjoner
#> NULL
#> 
#> 
#> stjernetabell
#> NULL
#> 
#> 
#> funksjoner
#> NULL
#> 
#> 
#> arter
#> NULL
#> 
#> 
#> kontoklasser
#> NULL
#> 
#> 
#> regnskapsomfang
#> NULL
#> 
#> 
#> formler
#> NULL
#> 
#> 
#>  [ Kombinasjonsberegninger...]
#> 
#> output 
#>         periode region kontoklasse   art funksjon belop
#> 1          2015   0100           1  A075      100     0
#> 2          2015   0100           2  A075      100     0
#> 1196447    2015 030107           3 AGD70      FG5     0
#> 1196448    2015 030107           4 AGD70      FG5     0
#> 
#> ================================================================
#> ==============    periode =  2016     ================
#> ==============================================================
#> 
#> data
#>     periode region kontoklasse funksjon art belop
#> 1      2016   0300           1      120 380  2312
#> 2      2016   0300           1      121 380   337
#> 458    2016   0101           3      110 090   106
#> 459    2016   0101           3      110 099   109
#> 
#> 
#> funksjonshierarki
#>     periode   to  from sign
#> 1      2016 FGF1 FGF1a    +
#> 2      2016 FGF1 FGF1b    +
#> 224    2016  FG5   321    -
#> 225    2016  FG5   711    -
#> 
#> 
#> artshierarki
#>     periode    to  from sign
#> 1      2016  LONN   010    +
#> 2      2016  LONN   020    +
#> 542    2016 AGD65 AGD46    -
#> 543    2016 AGD65  AG18    -
#> 
#> 
#> data_saer
#> NULL
#> 
#> 
#> artshierarki_nettinger
#> NULL
#> 
#> 
#> artshierarki_nettinger_kasse
#> NULL
#> 
#> 
#> kombinasjoner
#> NULL
#> 
#> 
#> regioner
#> NULL
#> 
#> 
#> storkombinasjoner
#> NULL
#> 
#> 
#> stjernetabell
#> NULL
#> 
#> 
#> funksjoner
#> NULL
#> 
#> 
#> arter
#> NULL
#> 
#> 
#> kontoklasser
#> NULL
#> 
#> 
#> regnskapsomfang
#> NULL
#> 
#> 
#> formler
#> NULL
#> 
#> 
#>  [ Kombinasjonsberegninger...]
#> 
#> output 
#>         periode region kontoklasse   art funksjon belop
#> 1          2016   0100           1  A075      100     0
#> 2          2016   0100           2  A075      100     0
#> 1087319    2016 030107           3 AGD32      FG5     0
#> 1087320    2016 030107           4 AGD32      FG5     0
#> 

# Output data som definert av kombinasjonerFil
z <- KostraRegnskap(inputdata,funksjonshierarki,artshierarki,kombinasjoner=kombinasjoner)
#> ================================================================
#> ==============    periode =  2015     ================
#> ==============================================================
#> 
#> data
#>     periode region kontoklasse funksjon art belop
#> 1      2015   0300           1      120 380   847
#> 2      2015   0300           1      121 380   288
#> 353    2015   0301           3      100 099   100
#> 354    2015   0101           3      110 099   108
#> 
#> 
#> funksjonshierarki
#>     periode   to  from sign
#> 1      2015 FGF1 FGF1a    +
#> 2      2015 FGF1 FGF1b    +
#> 225    2015  FG5   321    -
#> 226    2015  FG5   711    -
#> 
#> 
#> artshierarki
#>     periode    to from sign
#> 1      2015  LONN  010    +
#> 2      2015  LONN  020    +
#> 627    2015 AI660  660    +
#> 628    2015 AD990  990    +
#> 
#> 
#> data_saer
#> NULL
#> 
#> 
#> artshierarki_nettinger
#> NULL
#> 
#> 
#> artshierarki_nettinger_kasse
#> NULL
#> 
#> 
#> kombinasjoner
#>          art funksjon kontoklasse
#> 1       A075      100           1
#> 2       A075      100           2
#> 201047 AGD35      FG5           3
#> 201048 AGD35      FG5           4
#> 
#> 
#> regioner
#> NULL
#> 
#> 
#> storkombinasjoner
#> NULL
#> 
#> 
#> stjernetabell
#> NULL
#> 
#> 
#> funksjoner
#> NULL
#> 
#> 
#> arter
#> NULL
#> 
#> 
#> kontoklasser
#> NULL
#> 
#> 
#> regnskapsomfang
#> NULL
#> 
#> 
#> formler
#> NULL
#> 
#> 
#>  [ Kombinasjonsberegninger...]
#> 
#> output 
#>         periode region   art funksjon kontoklasse belop
#> 1          2015   0100  A075      100           1     0
#> 2          2015   0100  A075      100           2     0
#> 1206287    2015 030107 AGD35      FG5           3     0
#> 1206288    2015 030107 AGD35      FG5           4     0
#> 
#> ================================================================
#> ==============    periode =  2016     ================
#> ==============================================================
#> 
#> data
#>     periode region kontoklasse funksjon art belop
#> 1      2016   0300           1      120 380  2312
#> 2      2016   0300           1      121 380   337
#> 458    2016   0101           3      110 090   106
#> 459    2016   0101           3      110 099   109
#> 
#> 
#> funksjonshierarki
#>     periode   to  from sign
#> 1      2016 FGF1 FGF1a    +
#> 2      2016 FGF1 FGF1b    +
#> 224    2016  FG5   321    -
#> 225    2016  FG5   711    -
#> 
#> 
#> artshierarki
#>     periode    to  from sign
#> 1      2016  LONN   010    +
#> 2      2016  LONN   020    +
#> 542    2016 AGD65 AGD46    -
#> 543    2016 AGD65  AG18    -
#> 
#> 
#> data_saer
#> NULL
#> 
#> 
#> artshierarki_nettinger
#> NULL
#> 
#> 
#> artshierarki_nettinger_kasse
#> NULL
#> 
#> 
#> kombinasjoner
#>          art funksjon kontoklasse
#> 1       A075      100           1
#> 2       A075      100           2
#> 201047 AGD35      FG5           3
#> 201048 AGD35      FG5           4
#> 
#> 
#> regioner
#> NULL
#> 
#> 
#> storkombinasjoner
#> NULL
#> 
#> 
#> stjernetabell
#> NULL
#> 
#> 
#> funksjoner
#> NULL
#> 
#> 
#> arter
#> NULL
#> 
#> 
#> kontoklasser
#> NULL
#> 
#> 
#> regnskapsomfang
#> NULL
#> 
#> 
#> formler
#> NULL
#> 
#> 
#>  [ Kombinasjonsberegninger...]
#> 
#> output 
#>         periode region   art funksjon kontoklasse belop
#> 1          2016   0100  A075      100           1     0
#> 2          2016   0100  A075      100           2     0
#> 1206287    2016 030107 AGD35      FG5           3     0
#> 1206288    2016 030107 AGD35      FG5           4     0
#> 

# Trekkker 500 tilfeldige kombinasjoner og kjorer med det
kombinasjoner500 = kombinasjoner[sample(NROW(kombinasjoner),500),]
z <- KostraRegnskap(inputdata,funksjonshierarki,artshierarki,kombinasjoner=kombinasjoner500)
#> ================================================================
#> ==============    periode =  2015     ================
#> ==============================================================
#> 
#> data
#>     periode region kontoklasse funksjon art belop
#> 1      2015   0300           1      120 380   847
#> 2      2015   0300           1      121 380   288
#> 353    2015   0301           3      100 099   100
#> 354    2015   0101           3      110 099   108
#> 
#> 
#> funksjonshierarki
#>     periode   to  from sign
#> 1      2015 FGF1 FGF1a    +
#> 2      2015 FGF1 FGF1b    +
#> 225    2015  FG5   321    -
#> 226    2015  FG5   711    -
#> 
#> 
#> artshierarki
#>     periode    to from sign
#> 1      2015  LONN  010    +
#> 2      2015  LONN  020    +
#> 627    2015 AI660  660    +
#> 628    2015 AD990  990    +
#> 
#> 
#> data_saer
#> NULL
#> 
#> 
#> artshierarki_nettinger
#> NULL
#> 
#> 
#> artshierarki_nettinger_kasse
#> NULL
#> 
#> 
#> kombinasjoner
#>                               art funksjon kontoklasse
#> 1                           AGD59      339           3
#> 2                            AG38      255           2
#> 499                         AI620      231           4
#> 500 INTERN_SALG_BYDELER_BARNEVERN      420           4
#> 
#> 
#> regioner
#> NULL
#> 
#> 
#> storkombinasjoner
#> NULL
#> 
#> 
#> stjernetabell
#> NULL
#> 
#> 
#> funksjoner
#> NULL
#> 
#> 
#> arter
#> NULL
#> 
#> 
#> kontoklasser
#> NULL
#> 
#> 
#> regnskapsomfang
#> NULL
#> 
#> 
#> formler
#> NULL
#> 
#> 
#>  [ Kombinasjonsberegninger...]
#> 
#> output 
#>      periode region                           art funksjon kontoklasse belop
#> 1       2015   0100                         AGD59      339           3     0
#> 2       2015   0100                          AG38      255           2     0
#> 2999    2015 030107                         AI620      231           4     0
#> 3000    2015 030107 INTERN_SALG_BYDELER_BARNEVERN      420           4     0
#> 
#> ================================================================
#> ==============    periode =  2016     ================
#> ==============================================================
#> 
#> data
#>     periode region kontoklasse funksjon art belop
#> 1      2016   0300           1      120 380  2312
#> 2      2016   0300           1      121 380   337
#> 458    2016   0101           3      110 090   106
#> 459    2016   0101           3      110 099   109
#> 
#> 
#> funksjonshierarki
#>     periode   to  from sign
#> 1      2016 FGF1 FGF1a    +
#> 2      2016 FGF1 FGF1b    +
#> 224    2016  FG5   321    -
#> 225    2016  FG5   711    -
#> 
#> 
#> artshierarki
#>     periode    to  from sign
#> 1      2016  LONN   010    +
#> 2      2016  LONN   020    +
#> 542    2016 AGD65 AGD46    -
#> 543    2016 AGD65  AG18    -
#> 
#> 
#> data_saer
#> NULL
#> 
#> 
#> artshierarki_nettinger
#> NULL
#> 
#> 
#> artshierarki_nettinger_kasse
#> NULL
#> 
#> 
#> kombinasjoner
#>                               art funksjon kontoklasse
#> 1                           AGD59      339           3
#> 2                            AG38      255           2
#> 499                         AI620      231           4
#> 500 INTERN_SALG_BYDELER_BARNEVERN      420           4
#> 
#> 
#> regioner
#> NULL
#> 
#> 
#> storkombinasjoner
#> NULL
#> 
#> 
#> stjernetabell
#> NULL
#> 
#> 
#> funksjoner
#> NULL
#> 
#> 
#> arter
#> NULL
#> 
#> 
#> kontoklasser
#> NULL
#> 
#> 
#> regnskapsomfang
#> NULL
#> 
#> 
#> formler
#> NULL
#> 
#> 
#>  [ Kombinasjonsberegninger...]
#> 
#> output 
#>      periode region                           art funksjon kontoklasse belop
#> 1       2016   0100                         AGD59      339           3     0
#> 2       2016   0100                          AG38      255           2     0
#> 2999    2016 030107                         AI620      231           4     0
#> 3000    2016 030107 INTERN_SALG_BYDELER_BARNEVERN      420           4     0
#> 

if (lesDataFraS){
  # Trekkker 100 tilfeldige regioner og kjorer med det
  regioner100 = sample(unique(inputdata$region),100)
  z <- KostraRegnskap(inputdata,funksjonshierarki,artshierarki,kombinasjoner=kombinasjoner500,regioner=regioner100)

  # Minidata
  z <- KostraRegnskap(inputdata,funksjonshierarki,artshierarki,kombinasjoner=kombinasjoner500[1:5,],regioner=regioner100[1:5])
  print(z)
}

```
