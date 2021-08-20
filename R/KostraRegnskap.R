# Hack since bug in Matrix
# https://r-forge.r-project.org/tracker/index.php?func=detail&aid=6728&group_id=61&atid=294
Mult <- SSBtools:::Mult



#' @rdname KostraRegnskap
#' @note KostraRegnskapBeregningInput og KostraRegnskapBeregningHierarki går via funksjonen KostraRegnskapBeregning.
#' Parametere til den funksjonen trengs bortsett fra «output».
#' Spesielt må altså parameterne funksjoner, arter, kontoklasser med.
#' Det skal (i utgangspunktet) være en verdi på hver av disse.
#' Funksjonen KostraRegnskapBeregningHierarkiALT er alternativ til KostraRegnskapBeregningHierarki med andre variabler i output.
#' @export
KostraRegnskapBeregning = function(data,funksjonshierarki,artshierarki,data_saer,
                          artshierarki_nettinger,
                          artshierarki_nettinger_kasse,
                          formler = NULL,
                          funksjoner,
                          arter,
                          kontoklasser,
                          ...,
                          perioder = NULL, output){

  KostraRegnskap(data=data,funksjonshierarki=funksjonshierarki,artshierarki=artshierarki,
                 data_saer=data_saer,artshierarki_nettinger=artshierarki_nettinger, artshierarki_nettinger_kasse = artshierarki_nettinger_kasse,
                 formler=formler, funksjoner=funksjoner, arter=arter, kontoklasser=kontoklasser, ..., perioder=perioder,
                 output = output)
}

CharacterReCodeType = function(x){
  x$type = CharacterReCode(x$type,
    c("OUTPUT_Konsern", "OUTPUT_Kasse", "OUTPUT_Saer",  "OUTPUT_Nett_Saer",  "OUTPUT_Nett_Kasse", "Kasse", "Saer",  "Nett_Saer",  "Nett_Kasse", "Input_Kasse", "Input_Saer",  "Konsern", "formel"),
    c("OUTPUT_konsern", "OUTPUT_kasse", "OUTPUT_sbedr", "OUTPUT_nett_sbedr", "OUTPUT_nett_kasse", "kasse", "sbedr", "nett_sbedr", "nett_kasse", "input_kasse", "input_sbedr", "konsern", "formel"))
  names(x)[names(x)=="type"] = "regnskapsomfang"
  x
}



#' @rdname KostraRegnskap
#' @export
KostraRegnskapBeregningInput = function(...){
  CharacterReCodeType(KostraRegnskapBeregning(output = "beregningInput", ...))
}

#' @rdname KostraRegnskap
#' @export
KostraRegnskapBeregningHierarkiALT = function(...){
  KostraRegnskapBeregning(output = "beregningHierarki", ...) # "beregningHierarkiMedFormelHierarki" er alternativ
}


# Funksjonen ble omskrevet til å bruke "beregningHierarkiMedFormelHierarki" etter at ønske om hvordan det skulle være ble endret
# Da ble også koden som kjøres ved "beregningHierarkiInput" endret tilsvarende

#' @rdname KostraRegnskap
#' @export
KostraRegnskapBeregningHierarki = function(...){
  CharacterReCodeType(SelectAndRename(KostraRegnskapBeregning(output = "beregningHierarkiMedFormelHierarki", ...), # "beregningHierarki", ...),
                  oldNames= c("periode", "region", "kontoklasse_from", "funksjon_from", "art_from", "to", "from", "sign", "type_from", "belop"),
                  newNames= c("periode", "region", "kontoklasse",      "funksjon",      "art",      "to", "from", "sign", "type",    "belop")))
}


#' @rdname KostraRegnskap
#' @export
KostraRegnskapBeregningHierarkiInput = function(...){
  CharacterReCodeType(KostraRegnskapBeregning(output = "beregningHierarkiInput", ...)$hierarkiInput)
}





#' @rdname KostraRegnskap
#' @param perioder Perioder for beregningen. Ved NULL (default) brukes alle perioder som finnes i data.
#' @param ... Parametere som sendes videre til KostraRegnskapEnPeriode
#' @export
KostraRegnskap = function(data,funksjonshierarki,artshierarki,data_saer=NULL,
                          artshierarki_nettinger=NULL,
                          artshierarki_nettinger_kasse=NULL,
                          stjernetabell=NULL,
                          formler = NULL,
                          ...,
                          perioder = NULL){
  if(is.null(perioder)){
    perioder = sort(unique(as.character(data$periode)))
    if(length(perioder) == 0)
      stop("periode finnes ikke i data")
  } else {
    perioder = unique(as.character(perioder))
    SjekkDataPerioder(data, "data", perioder)
  }

  SjekkDataPerioder(data_saer, "data_saer", perioder)
  SjekkDataPerioder(funksjonshierarki, "funksjonshierarki", perioder)
  SjekkDataPerioder(artshierarki, "artshierarki", perioder)
  SjekkDataPerioder(artshierarki_nettinger, "artshierarki_nettinger", perioder)
  SjekkDataPerioder(artshierarki_nettinger_kasse, "artshierarki_nettinger_kasse", perioder)
  SjekkDataPerioder(stjernetabell, "stjernetabell", perioder, okNULL = TRUE)
  SjekkDataPerioder(formler, "formler", perioder, okNULL = TRUE)


  n = length(perioder)
  a <- vector("list",n)
  for(i in seq_len(n)){
    if(n>1){
      cat("================================================================\n")
      cat("==============    periode = ",perioder[i],"    ================\n")
      cat("==============================================================\n")
    }
    ai =  KostraRegnskapEnPeriode(data = DataPeriode(data,perioder[i]),
                   funksjonshierarki = DataPeriode(funksjonshierarki,perioder[i]),
                   artshierarki = DataPeriode(artshierarki,perioder[i]),
                   data_saer = DataPeriode(data_saer,perioder[i]),
                   artshierarki_nettinger = DataPeriode(artshierarki_nettinger,perioder[i]),
                   artshierarki_nettinger_kasse  = DataPeriode(artshierarki_nettinger_kasse,perioder[i]),
                   stjernetabell = DataPeriode(stjernetabell,perioder[i],TRUE),
                   formler = DataPeriode(formler,perioder[i],TRUE),
                   ...)
    if(n>1) a[[i]] = ai
  }
  if(n>1)
    return(RbindAll(a))
  ai
}


DataPeriode = function(data,periode,okNULL = FALSE){
  if(is.null(data))
    return(NULL)
  if(okNULL)
    if(is.null(data$periode))
      return(data)
  rows = as.character(data$periode) == periode
  if(any(!rows))
    return(data[rows, ,drop=FALSE])
  data
}


SjekkDataPerioder = function(data,navn,perioder,okNULL = FALSE){
  if(is.null(data))
    return(NULL)
  if(okNULL)
    if(is.null(data$periode))
      return(NULL)
  dataPerioder = unique(as.character(data$periode))
  if(any(!(perioder %in% dataPerioder)))
    stop(paste(navn,": Det er periode(r) som mangler, ",paste(perioder[!(perioder %in% dataPerioder)] ,collapse=", "),sep=""))
  NULL
}



Fgrepl = function(pattern, x){  # grepl with fixed = TRUE
  grepl(pattern, x, fixed = TRUE)
}




#' KostraRegnskap
#'
#' KostraRegnskap er en wrapper til funksjonen KostraRegnskapEnPeriode som, ved flere perioder, blir kalt flere ganger.
#' I output-kolonnen regnskapsomfang betyr A konsern og B kasse.
#'
#' Regioner kan settes til et tall. F.eks regioner=5 betyr at de 5 første regionene i data velges.
#' Det kan også gjøres utvalg fra regionene i data ved bruk av *, ?, ! og –.
#' Her brukes ! til å invertere enkeltvalg, mens – brukes til fjerning fra det samlede utvalget. Det betyr at bruk av – tilsvarer sign i stjernetabell.
#' Spesielt kan man velge alle fylker (regioner = "??00"),
#' alle bydeler (regioner = "??????") eller alle kommuner
#' (regioner = c("????","-??00")).
#'
#' @rdname KostraRegnskap
#'
#' @encoding UTF8
#'
#' @param data inputdata
#' @param funksjonshierarki funksjonshierarki
#' @param artshierarki artshierarki
#' @param data_saer inputdata for særbedrifter
#' @param artshierarki_nettinger artshierarki for særbedrift-nettinger
#' @param artshierarki_nettinger_kasse artshierarki for kasse-nettinger
#' @param kombinasjoner kombinasjoner av art, funksjon, kontoklasse som skal med i output
#' @param regioner regioner som skal med i output. Kan også settet til et tall eller gjøre bruk av *, ?, ! og – (se nedenfor).
#' @param storkombinasjoner kombinasjoner av art, funksjon, kontoklasse og region
#' @param stjernetabell tabell med *  og ? som definerer koder som skal med i output
#' @param output  "en" (default), "fire" (men det bør endres til "fem"...), "enFactor", "fireFactor" "storkombinasjoner", "hierarkier" eller "barePrintData".
#'              Ved "fire" blir output et datasett med fire beløpskolonner: kasse, særbedrift, nettinger, konsern.
#'              Variantene med Factor gir outputdata med factor istedenfor character.
#'              Ved "hierarkier" returneres utflatede hierarkier.
#'              Sign for nettinghierarkiet er i kolonnen netting. Dette hierarkiet er omskrevet til å ta vanlige data som input.
#'              Ved "barePrintData" stopper funksjonen etter at input er printet (printData=TRUE).
#' @param funksjoner funksjoner som skal med i output. Man kan også bruke  *, ?, ! og – tilsvarende som for regioner (se nedenfor).
#' @param arter arter som skal med i output. Man kan også bruke  *, ?, ! og – tilsvarende som for regioner (se nedenfor).
#' @param kontoklasser kontoklasser som skal med i output. Det er mulig å bruke * istedenfor NULL til å velge alle kontoklosser.
#' @param regnskapsomfang regnskapsomfang som skal med i output. Det er mulig å bruke * og ? istedenfor NULL til å velge begge regnskapsomfang.
#' @param printInfo Ved TRUE printes informasjon om generering av kombinasjoner fra stjernetabell samt informasjon om omskriving av formler.
#' @param printData Ved TRUE printes to første og to siste rader av alle inputdataene
#' @param lag0300 Ved TRUE kopieres region 0301 til 0300 i inputdata
#' @param formler data frame med formler som brukes til korreksjoner
#' @param autoNetting Ved TRUE (default) utvides nettinghierarkiene automatisk basert på from-koden COPY
#' @param useMatrixToDataFrame Ved TRUE (default) brukes spesiell funksjonalitet (MatrixToDataFrame og DataFrameToMatrix) som sparer tid og minne.
#' @param returnFormelData Ved TRUE returneres spesielt datasett med informasjon om formelberegninger (denne parameteren vil bli tatt bort og erstattet av nye output koder som kan brukes)
#' @param smartB Ved TRUE (default) og ved valg av ett regnskapsomfang velges en metode som sparer tid og minne.
#'               Istedenfor å plukke ut regnskapsomfang til slutt gjøres kun de beregninger som trengs.
#'              Spesielt ved valg av regnskapsomfang B kan data_saer og/eller artshierarki_nettinger sløyfes.
#' @param fixRegionkode   Ved TRUE (default): Sørger for blanke i starten/slutten fjernes og at regionkoder får 4 eller 6 plasser og ledende nuller (gir warning ved endring av input)
#' @param fixArtkode      Ved TRUE (default): Sørger for blanke i starten/slutten fjernes og at artskoder som er tall får 3 plasser og ledende nuller (gir warning ved endring av input)
#' @param fixFunksjonkode Ved TRUE (default): Sørger for blanke i starten/slutten fjernes og at funksjonskoder som er tall får 3 plasser og ledende nuller (gir warning ved endring av input)
#' @param autoFormel Ved TRUE (default) kan formlene avhenge av hverandre. Ved TRUE kan formlene avhenge av hverandre.
#'                   Formlene vil bli korrigert ved hjelp av funksjonen \code{\link{AutoFormel}}.
#'
#' @return En data frame
#' @export
#' @importFrom SSBtools HierarchicalWildcardGlobbing ForceFactorDataFrame DataFrameToMatrix MatrixToDataFrame
#' @importFrom Matrix Matrix
#'
#' @seealso \code{\link{HierarchyCompute}}, \code{\link{HierarchicalWildcardGlobbing}}
#'
#' @examples
#'
#'  #==========================================================================
#'  #     Innlesing av data og omkoding
#'  #===========================================================================
#' # Når lesDataFraS er TRUE kjøres eksempelkoden omtrent slik den var under utvikling av koden.
#' # Ved FALSE brukes data som ligger i pakken der bare noen regioner er med, men det kjøres da
#' # på to år samtidig. Kommentarer i koden om beregningstid og antall rader vil ikke stemme.
#' lesDataFraS <- FALSE
#' if (!lesDataFraS){
#'   kData = KostraData("kostraRegnskapDataPen")       # KostraData("kostraRegnskapData") er alternativ der
#'   inputdata <- kData$data                           #    automatisk omkoding trengs (som fixRegionkode)
#'   funksjonshierarki <- kData$funksjonshierarki      #    Fungerer like bra, men med flere warning
#'   artshierarki <- kData$artshierarki
#'   inputdata_saer <- kData$data_saer
#'   artshierarki_nettinger <- kData$artshierarki_nettinger
#'   artshierarki_nettinger_kasse <- kData$artshierarki_nettinger_kasse
#'   stjerne <- kData$stjernetabell
#'   formler <- kData$formler
#' } else {
#'
#' dataPath <- "S:/Prosjekt/2228 KOSTRA publisering/Metode/Regnskap/Matrise-klare filer"
#' #dataPath <- "C:\\Users\\oyl\\Documents\\work\\Rworking\\regnskap\\Matrise-klare filer"
#'
#' aar = "2016" # "2015" kan også velges
#'
#' dataPath <- paste(dataPath,aar,sep="/")
#'
#' funksjonFil     <- paste(dataPath,"/Funksjonshierarki_",aar,".csv",sep="")
#' artFil          <- paste(dataPath,"/Artshierarki_",aar,".csv",sep="")
#' artFilnett      <- paste(dataPath,"/Artshierarki_nettinger_s\U00E6rbedrift_",aar,".csv",sep="")
#' artFilnettKasse      <- paste(dataPath,"/Artshierarki_nettinger_kasse_",aar,".csv",sep="")
#'
#' dataFil         <- paste(dataPath,"/Inputdata_fylker_kommuner_bydeler_",aar,".csv",sep="")
#' dataFilsaer     <- paste(dataPath,"/Inputdata_s\U00E6rbedrifter_",aar,".csv",sep="")
#'
#' kombinasjonerFil <- paste(dataPath,"/../../Alle kombinasjoner vi trenger som output.csv",sep="")
#' stjerneFil       <- paste(dataPath,"/StjerneKombinasjoner_",aar,".csv",sep="")
#' formelFil       <- paste(dataPath,"/Formler_",aar,".csv",sep="")
#'
#' funksjonshierarki      <- read.csv2(funksjonFil,colClasses=c("integer","character","character","character"))
#' artshierarki           <-  read.csv2(artFil,colClasses=c("integer","character","character","character"))
#' artshierarki_nettinger <-  read.csv2(artFilnett,colClasses=c("integer","character","character","character"))
#' artshierarki_nettinger_kasse <-  read.csv2(artFilnettKasse,colClasses=c("integer","character","character","character"))
#'
#' inputdata              <-  read.csv2(dataFil,colClasses=c("integer","character","character","character","character","integer"))
#' inputdata_saer <-  read.csv2(dataFilsaer,colClasses=c("integer","character","character","character","character","integer"))
#'
#' stjerne    <- read.csv2(stjerneFil,colClasses="character")
#' formler   <- read.csv2(formelFil,colClasses="character")
#'
#' # Endrer navn i data fra fylkesregion til region
#' names(inputdata)[names(inputdata)=="fylkesregion"] <- "region"
#' names(inputdata_saer)[names(inputdata_saer)=="fylkesregion"] <- "region"
#' names(stjerne)[names(stjerne)=="fylkesregion"] <- "region"
#'
#' # Leser kombinasjoner og omkoder
#' kombinasjoner <- read.csv2(kombinasjonerFil,colClasses="character")
#' kombinasjonerD  <- kombinasjoner$kontoklasse %in% c("1","3")
#' kombinasjoner$kontoklasse <- "I"
#' kombinasjoner$kontoklasse[kombinasjonerD] <- "D"
#' kombinasjoner <- unique(kombinasjoner) #  25312 kombinasjoner av funksjon, art og kontoklasse
#'
#'
#' # Legger til manglende ledende 0-er
#' inputdata$region = FixRegionkode(inputdata$region)
#' inputdata_saer$region = FixRegionkode(inputdata_saer$region)
#' }
#'
#'  #==========================================================================
#'  #   Eksempler med:   Konsern  = Saebedrift + Nettinger + Kasse
#'  #===========================================================================
#'
#' # Med stjernetabell. Denne gir 28215120 rader. [Beregningstid: 1 minutt]
#' # MERK: Denne kan krasje ved lite tilgjengelig minne som ved 32 bit versjon av R
#' z <- KostraRegnskap(inputdata, funksjonshierarki, artshierarki, inputdata_saer, artshierarki_nettinger, stjernetabell = stjerne)
#'
#' if(!lesDataFraS)
#'    kombinasjoner <- unique(z[, c("art", "funksjon", "kontoklasse")])
#'
#' rm(z) # sletter z frigjoer minne
#'
#' # Med kombinasjoner. Denne gir 23388288 rader. [Beregningstid: 15 sek] (25312 kombinasjoner * 462 regioner * 2 regnskapsomfang)
#' z <- KostraRegnskap(inputdata, funksjonshierarki, artshierarki, inputdata_saer, artshierarki_nettinger, kombinasjoner = kombinasjoner)
#'
#' # plukker ut 12 rader og fjerner radnavn
#' z12a <- z[z$region %in% c("0101", "0100", "0301") & z$funksjon %in% c("100", "FG2") & z$art %in% c("AGI6", "AGD10"), ]
#' rownames(z12a) <- NULL
#'
#' rm(z) # sletter z frigjoer minne
#'
#' # Samme svar kan beregnes mer direkte
#' z12b <- KostraRegnskap(inputdata, funksjonshierarki, artshierarki, inputdata_saer, artshierarki_nettinger, kombinasjoner = kombinasjoner, regioner = c("0101", "0100", "0301"), funksjoner = c("100", "FG2"), arter = c("AGI6", "AGD10"))
#' identical(z12a, z12b)  # Gir TRUE
#'
#'
#' # Uten kombinasjoner i input blir det 48 rader i output (2 regnskapsomfang) X (3 regioner) X (2 funksjoner) X (2 arter) X (2 kontoklasser)
#' z48 <- KostraRegnskap(inputdata, funksjonshierarki, artshierarki, inputdata_saer, artshierarki_nettinger, regioner = c("0101", "0100", "0301"), funksjoner = c("100", "FG2"), arter = c("AGI6", "AGD10"))
#'
#' # Varianten med factor-output krever mindre minne. Denne forandringen gir ingen forskjell for resultat skrevet til csv-fil.
#' z <- KostraRegnskap(inputdata, funksjonshierarki, artshierarki, inputdata_saer, artshierarki_nettinger, stjernetabell = stjerne, output = "enFactor")
#'
#' # plukker ut 24 rader og fjerner radnavn
#' z24a <- z[z$region %in% c("0101", "0100", "0301") & z$funksjon %in% c("100", "FG2") & z$art %in% c("AGI6", "AGD10"), ]
#' rownames(z24a) <- NULL
#'
#' # Samme svar kan beregnes mer direkte [Beregningstid: 1 sekund]
#' z24b <- KostraRegnskap(inputdata, funksjonshierarki, artshierarki, inputdata_saer, artshierarki_nettinger, stjernetabell = stjerne, regioner = c("0101", "0100", "0301"), funksjoner = c("100", "FG2"), arter = c("AGI6", "AGD10"))
#' # z24a og z24b er ikke identiske pga ulik ordning av rader
#'
#' rm(z) # sletter z frigjoer minne
#'
#' # Med stjernetabell og formler. Bare Oslo og Akershus, funksjon FG1 FG2 FG4 og arter som starter med AGD.
#' z <- KostraRegnskap(inputdata, funksjonshierarki, artshierarki, inputdata_saer, artshierarki_nettinger, stjernetabell = stjerne, regioner = c("02??","03??") , formler = formler, funksjoner = "FG?", arter = "AGD*")
#'
#'
#' # Kombinerer stjernetabell og kombinasjoner. Denne gir 14360352 rader. [Beregningstid: 1 minutt] MERK: igjen kreves minne
#' z <- KostraRegnskap(inputdata, funksjonshierarki, artshierarki, inputdata_saer, artshierarki_nettinger, kombinasjoner = kombinasjoner, stjernetabell = stjerne)
#'
#' # Lite eksempel med output = "en" (som default)
#' KostraRegnskap(inputdata, funksjonshierarki, artshierarki, inputdata_saer, artshierarki_nettinger, regioner = "0101", funksjoner = "FG2", arter = "AGD10")
#'
#' # Samme eksempel med output = "fire"
#' KostraRegnskap(inputdata, funksjonshierarki, artshierarki, inputdata_saer, artshierarki_nettinger, regioner = "0101", funksjoner = "FG2", arter = "AGD10", output = "fire")
#'
#'  #===============================================================================
#'  #   Eksempler med:   Konsern  = Saebedrift + Nettinger + Kasse + Kassenettinger
#'  #===============================================================================
#'
#'
#'    zEn   <- KostraRegnskap(inputdata,funksjonshierarki,artshierarki,inputdata_saer,artshierarki_nettinger,artshierarki_nettinger_kasse,stjernetabell=stjerne,regioner = c("0101", "0100",  "0301"),formler=formler)
#'
#'    zFem  <- KostraRegnskap(inputdata,funksjonshierarki,artshierarki,inputdata_saer,artshierarki_nettinger,artshierarki_nettinger_kasse,stjernetabell=stjerne,regioner = c("0101", "0100",  "0301"),output="fire",formler=formler)
#'
#'
#'  # ==========================================================================
#'  #   Gamle eksempler uten data_saer og artshierarki_nettinger
#'  #===========================================================================
#'
#' if (!lesDataFraS){
#'   dat <- kData$data
#'   datS <- kData$data_saer
#'   dat$kontoklasse <- CharacterReCode(dat$kontoklasse, c("D", "I"), c("1", "2"))
#'   datS$kontoklasse <- CharacterReCode(datS$kontoklasse, c("D", "I"), c("3", "4"))
#'   inputdata <- rbind(dat, datS)
#'   funksjonshierarki <- kData$funksjonshierarki
#'   artshierarki <- kData$artshierarki
#'   z <- KostraRegnskap(inputdata,funksjonshierarki,artshierarki)
#'   kombinasjoner <- unique(z[, c("art", "funksjon", "kontoklasse")])
#' } else {
#' dataPath <- "S:/Prosjekt/2228 KOSTRA publisering/Metode/Regnskap"
#' #dataPath <- "C:\\Users\\oyl\\Documents\\work\\Rworking\\regnskap"
#'
#' funksjonFil <- paste(dataPath,"Funksjonshierarki.csv",sep="/")
#' artFil      <- paste(dataPath,"Artshierarki_uten_nettinger.csv",sep="/")
#' dataFil     <- paste(dataPath,"Inputdata_kommuner_fylker_2016.csv",sep="/")
#' kombinasjonerFil <- paste(dataPath,"Alle kombinasjoner vi trenger som output.csv",sep="/")
#'
#' funksjonshierarki <- read.csv2(funksjonFil,colClasses=c("integer","character","character","character"))
#' artshierarki <-  read.csv2(artFil,colClasses=c("integer","character","character","character","integer"))
#' inputdata <-  read.csv2(dataFil,colClasses=c("integer","character","character","character","character","integer"))
#' kombinasjoner <- read.csv2(kombinasjonerFil,colClasses="character")
#'
#' # Endrer navn i data fra fylkesregion til region
#' names(inputdata)[2] <- "region"
#'
#' inputdata$region = FixRegionkode(inputdata$region) # ledende 0-er legges til
#' }
#'
#' # Output data som definert av kombinasjonerFil
#' z <- KostraRegnskap(inputdata,funksjonshierarki,artshierarki,kombinasjoner=kombinasjoner)
#'
#' # Trekkker 500 tilfeldige kombinasjoner og kjorer med det
#' kombinasjoner500 = kombinasjoner[sample(NROW(kombinasjoner),500),]
#' z <- KostraRegnskap(inputdata,funksjonshierarki,artshierarki,kombinasjoner=kombinasjoner500)
#'
#' if (lesDataFraS){
#'   # Trekkker 100 tilfeldige regioner og kjorer med det
#'   regioner100 = sample(unique(inputdata$region),100)
#'   z <- KostraRegnskap(inputdata,funksjonshierarki,artshierarki,kombinasjoner=kombinasjoner500,regioner=regioner100)
#'
#'   # Minidata
#'   z <- KostraRegnskap(inputdata,funksjonshierarki,artshierarki,kombinasjoner=kombinasjoner500[1:5,],regioner=regioner100[1:5])
#'   print(z)
#' }
#'
#'
KostraRegnskapEnPeriode = function(data,funksjonshierarki,artshierarki,data_saer=NULL,
                          artshierarki_nettinger=NULL,
                          artshierarki_nettinger_kasse=NULL,
                          kombinasjoner=NULL,
                          regioner=NULL,
                          storkombinasjoner=NULL,
                          stjernetabell=NULL,
                          funksjoner = NULL,
                          arter = NULL,
                          kontoklasser = NULL,
                          regnskapsomfang = NULL,
                          output="en",
                          printInfo = TRUE,
                          printData = TRUE,
                          lag0300 = FALSE,
                          formler = NULL,
                          autoNetting = TRUE,
                          useMatrixToDataFrame = TRUE,
                          returnFormelData =FALSE,
                          smartB = TRUE,
                          fixRegionkode = TRUE,
                          fixArtkode = TRUE,
                          fixFunksjonkode = TRUE,
                          autoFormel = TRUE
){
  #  Denne koden er resultat av utvikling over tid i en prosess der hva som skulle gjøres ble bestemt i iterasjon med
  #  fagseksjon.
  #  Enklere kode kan oppnås ved å implementere på nytt, men denne koden virker stabil og er grundig testet.
  #  Gjenbrukbare generiske funksjoner er lagt inn i pakken SSBtools (HierarchyCompute, HierarchicalWildcardGlobbing).
  #  Utgangspunktet for denne koden var å løse en krise i Kostra der førsøk på annen implementering (VTL) hadde vist
  #  seg å ikke fungere. På et stadium var det en balansering på datasystemets grense på hvor store datamengder som
  #  kunne overføres fra R til systemet.
  #
  #  Det startet med funksjonalitet som fremgår av «Gamle eksempler uten data_saer og artshierarki_nettinger».
  #  Siden disse beregningen fungerte ble det satset på å videreutvikle R-koden.
  #  Det neste ble «Konsern  = Saebedrift + Nettinger + Kasse». Men egentlig startet det med formelen
  #  «Konsern  = Saebedrift - Nettinger + Kasse» som måtte forandres siden dataene ikke var slik. Dette gale
  #  utgangspunktet har medført merkelig variabelnavn (differanse som egentlig er sum).  Kassenettinger kom inn på et
  #  sent tidspunkt. Det var mye frem og tilbake hvordan delkomponenter og nettinger skulle håndteres og det endelige
  #  resultatet fremgår av parameteren autoNetting. Spesielle unntak fra vanlig beregning ble tatt inn ved bruken av
  #  formler. Dette startet med formler med ett kolon. Etter hvert kom også behovet for to kolon. Det var også litt
  #  frem og tilbake hvordan formler skulle forholde seg til hverandre og det endte med autoFormel.
  #
  #  Utvikling av beregningstester var noe som ble gjort senere. Hvordan disse skulle være var også i en prosess i
  #  iterasjon med fag. At beregningstestene kan håndtere formler gjør dette ekstra avansert. Det var behov for ny kode
  #  inni hovedfunksjonen for å ta ut akkurat det som var ønskelig.

  AddLeadingZeros <-  function(codes, ...){
    if (!is.character(codes)) {
      codes <- as.character(codes)
    }
    SSBtools::AddLeadingZeros(codes, ...)
  }

  StopOrWarning = stop

  if(printData){
    PrintHeadTail(data)
    PrintHeadTail(funksjonshierarki)
    PrintHeadTail(artshierarki)
    PrintHeadTail(data_saer)
    PrintHeadTail(artshierarki_nettinger)
    PrintHeadTail(artshierarki_nettinger_kasse)
    PrintHeadTail(kombinasjoner)
    PrintHeadTail(regioner)
    PrintHeadTail(storkombinasjoner)
    PrintHeadTail(stjernetabell)
    PrintHeadTail(funksjoner)
    PrintHeadTail(arter)
    PrintHeadTail(kontoklasser)
    PrintHeadTail(regnskapsomfang)
    if(is.null(formler))PrintHeadTail(formler)
    else
      print(as.matrix(formler,rownames=TRUE),quote=FALSE) # Printer alle og dessuten på en penere måte
  }


  warningTextRegionskoder = "Regionskoder endret"
  warningTextArtskoder = "Artskoder endret"
  warningTextFunksjonskoder = "Funkjonskoder endret"

  if(output== "beregningHierarkiInput"){
    output = "beregningHierarkiMedFormelHierarki"     #output ="beregningHierarki"
    beregningHierarkiInput = TRUE
  } else {
    beregningHierarkiInput = FALSE
  }



  # Måtte gjøre om. Ville bare ha flatt formelhieraki men beholder gammel mulighet som beregningHierarkiMedFormelHierarki.
  if(output== "beregningHierarkiMedFormelHierarki"){   # if(output=="beregningHierarki"){
    output ="beregningHierarki"
    autoFormel = FALSE
    bareEnFormel = FALSE
  } else {
    if(output=="beregningHierarki"){
      bareEnFormel = !is.null(formler)      # Dersom autoformel er kjørt blir det galt å konstruere  formelhierarki, men ok ved en formel.
      # autoFormel = TRUE, autoFormel som i input
    }
  }

  if(!is.null(formler)){
    if(autoFormel){
      if(printInfo) cat("\n \n --- Kjører AutoFormel ----\n")
      #formler$formel = AutoFormel(formler$formel)
      formler_formel = AutoFormel(formler$formel)
      formler = formler[rep_len(seq_len(NROW(formler)),length(formler_formel)), ,drop=FALSE]
      formler$formel = formler_formel
      rownames(formler) = NULL
      if(printInfo) cat("\n \n --- Etter AutoFormel ----\n")
      if(printInfo) print(as.matrix(formler,rownames=TRUE),quote=FALSE) # Printer alle igjen
    } else {    ### Kjører omskriving av formler siden mye kode utviklet etter at autoFormel=TRUE ble default, men tilbake til FALSE seinere
      formler_formel = AutoFormel(formler$formel, laAvhenge = TRUE)
      formler = formler[rep_len(seq_len(NROW(formler)),length(formler_formel)), ,drop=FALSE]
      formler$formel = formler_formel
      rownames(formler) = NULL
      if(printInfo) cat("\n \n --- Etter omskriving av formler (fra2til3) ----\n")
      if(printInfo) print(as.matrix(formler,rownames=TRUE),quote=FALSE) # Printer alle igjen
    }
  }

  if(!is.null(storkombinasjoner)){
    if(sum(names(storkombinasjoner)=="region")==0){
      if(any(names(storkombinasjoner)=="fylkesregion")){
        names(storkombinasjoner)[names(storkombinasjoner)=="fylkesregion"] <- "region"
        warning("'fylkesregion' omkodet til 'region' i storkombinasjoner")
      }
    }
    if(fixRegionkode)
      storkombinasjoner$region = FixRegionkode(storkombinasjoner$region,warningTextRegionskoder)
    if(fixArtkode)
      storkombinasjoner$art = AddLeadingZeros(storkombinasjoner$art,3,warningTextArtskoder)
    if(fixFunksjonkode)
      storkombinasjoner$funksjon = AddLeadingZeros(storkombinasjoner$funksjon,3,warningTextFunksjonskoder)
  }


  if(!is.null(kombinasjoner)){
    if(fixArtkode)
      kombinasjoner$art = AddLeadingZeros(kombinasjoner$art,3,warningTextArtskoder)
    if(fixFunksjonkode)
      kombinasjoner$funksjon = AddLeadingZeros(kombinasjoner$funksjon,3,warningTextFunksjonskoder)
  }


  if(!is.null(stjernetabell)) {
    if(sum(names(stjernetabell)=="region")==0){
      if(any(names(stjernetabell)=="fylkesregion")){
        names(stjernetabell)[names(stjernetabell)=="fylkesregion"] <- "region"
        warning("'fylkesregion' omkodet til 'region' i stjernetabell")
      }
    }
    if(fixRegionkode)
      stjernetabell$region = FixRegionkode(stjernetabell$region,warningTextRegionskoder)
    if(fixArtkode)
      stjernetabell$art = AddLeadingZeros(stjernetabell$art,3,warningTextArtskoder)
    if(fixFunksjonkode)
      stjernetabell$funksjon = AddLeadingZeros(stjernetabell$funksjon,3,warningTextFunksjonskoder)
  }

  if(fixRegionkode & !is.null(regioner))
    regioner = FixRegionkode(regioner,warningTextRegionskoder)


  if(fixArtkode & !is.null(arter))
    arter =  AddLeadingZeros(arter,3,warningTextArtskoder)


  if(fixFunksjonkode & !is.null(funksjoner))
    funksjoner = AddLeadingZeros(funksjoner,3,warningTextFunksjonskoder)



  #toRemove = TRUE # Parameter til AutoNetting

  # Endrer navn fordi regnskapsomfang allerede var ibruk som variabel da dette ble lagt inn
  regnskapsomfanger = regnskapsomfang
  rm(regnskapsomfang)



  if(is.null(regnskapsomfanger)){   # Hindre kjøringen med isNullSum==2
    if((!is.null(formler)) | (!is.null(artshierarki_nettinger_kasse)))
      regnskapsomfanger = "*"
  }


  if(!is.null(regnskapsomfanger))
    if(smartB)
      if(!(length(regnskapsomfanger)==1 & regnskapsomfanger[1]=="B") ){
        if(is.null(data_saer))
          stop('data_saer må med unntatt ved bare regnskapsomfang "B" ')
        if(is.null(artshierarki_nettinger))
          stop('artshierarki_nettinger må med unntatt ved bare regnskapsomfang "B" ')
      }

  if(!is.null(regnskapsomfanger))
    if("?" %in% regnskapsomfanger)
      regnskapsomfanger = NULL
  if(!is.null(regnskapsomfanger))
    if("*" %in% regnskapsomfanger)
      regnskapsomfanger = NULL

  if(!is.null(regnskapsomfanger)){
    regnskapsomfanger = unique(regnskapsomfanger)
    if(any(!(regnskapsomfanger %in% c("A","B"))))
      stop(paste("Feil regnskapsomfang:",
                 paste(regnskapsomfanger[!(regnskapsomfanger %in% c("A","B"))],collapse=", ")))
    if(length(regnskapsomfanger)==2)
      regnskapsomfanger = NULL
  }


  if(!is.null(regnskapsomfanger))
    if(!is.null(output))
      if(!(output %in% c("en", "enFactor", "beregningHierarki", "beregningInput", "matrixComponents"))){
        regnskapsomfanger = NULL
        warning('Inputparameter regnskapsomfang ignoreres ved dette valg av output."')
      }

  onlyB = FALSE
  if(!is.null(regnskapsomfanger))
    if(regnskapsomfanger=="B")
      onlyB = TRUE


  if(!is.null(regnskapsomfanger))
    if(regnskapsomfanger=="B")
      if(smartB){
        if(is.null(data_saer))
          data_saer = data[1, ,drop=FALSE]
        data_saer$belop = 0L
        if(is.null(artshierarki_nettinger)) # Velger en linje til artshierarki_nettinger
          artshierarki_nettinger = artshierarki[which(artshierarki$from %in% data$art)[1], ,drop=FALSE]
      }



  # Legger inn bruk av * for kontoklasser også
  if(!is.null(kontoklasser))
    if("*" %in% kontoklasser)
      kontoklasser = NULL



  rader0warning = TRUE # Brukes til FormelKorreksjoner

  # Ved utvalgte kombinasjoner blir 0 rader normalt og vil unngå warning
  if(!is.null(kontoklasser))
    if(length(kontoklasser)==1)
      rader0warning = FALSE

  if(!is.null(funksjoner))
    rader0warning = FALSE

  if(!is.null(arter))
    rader0warning = FALSE





  if(NROW(data)==0)
    stop("Det er ingen rader i data")

  if(output == "barePrintData")
    return(NULL)





  # Bruk av forceStringsAsFactors er ikke implementert generelt. Bare ved  useMatrixToDataFrame, storkOrder, isNullSum==1
  forceStringsAsFactors=FALSE

  if(output=="enFactor"){
    output="en"
    forceStringsAsFactors=TRUE
  }
  if(output=="fireFactor"){
    output="fire"
    forceStringsAsFactors=TRUE
  }



  # matrixComponents er utganspunkt for å se detaljert hvordan outputtall er dannet fra inputtall.
  # Det er også mulig å kjøre HierarchyFromDummy på dataDummyHierarchy.
  # Men den matrisen er uten rad og kolonnenavn.
  # Info er istenden i toCrossCode og fromCrossCode
  # valueMatrix har regioner som colnames
  returnMatrixComponents=FALSE
  beregningInput = FALSE

  if(output=="matrixComponents"){
    output="fire"
    returnMatrixComponents=TRUE
  }
  if(output=="beregningInput"){
    output="fire"
    beregningInput=TRUE
  }




  if(returnFormelData)
    forceStringsAsFactors=TRUE

  if(sum(names(data)=="region")==0){
    if(any(names(data)=="fylkesregion")){
      names(data)[names(data)=="fylkesregion"] <- "region"
      warning("'fylkesregion' omkodet til 'region' i data")
    }
  }
  if(fixRegionkode)
    data$region = FixRegionkode(data$region,warningTextRegionskoder)
  if(fixArtkode)
    data$art = AddLeadingZeros(data$art,3,warningTextArtskoder)
  if(fixFunksjonkode)
    data$funksjon = AddLeadingZeros(data$funksjon,3,warningTextFunksjonskoder)




  #if(sum(names(data)=="region")==0)
  #  stop("'region' mangler")

  if(!is.null(stjernetabell))
    stjernetabell = stjernetabell[ , colnames(stjernetabell)!="periode" ,drop=FALSE]



  periode = unique(c(as.character(data$periode),
                     as.character(funksjonshierarki$periode),
                     as.character(artshierarki$periode),
                     as.character(data_saer$periode),
                     as.character(artshierarki_nettinger_kasse$periode),
                     as.character(artshierarki_nettinger$periode)))

  if(length(periode) == 0)
    stop("periode finnes ikke i input")
  if(length(periode) > 1)
    stop(paste("periode er ikke unik:",paste(periode,collapse=", ")))

  isNullSum = as.integer(is.null(data_saer))+as.integer(is.null(artshierarki_nettinger))
  if(isNullSum==1)
    stop("Enten må både 'data_saer' og 'artshierarki_nettinger' med eller ingen av dem.")


  varIdata = c("region","kontoklasse", "funksjon", "art", "belop") %in% names(data)
  if(any(!varIdata)){
    stop(paste("Disse variablene mangler i data:",
               paste(c("region","kontoklasse", "funksjon", "art", "belop")[!varIdata],collapse=", ")))
  }



  integerInOutput = TRUE

  if(!is.integer(data$belop))
    integerInOutput = FALSE


  #
  #  Bedre å passe på en gang (eller 10 ganger) for mye dette med factor og character enn en for lite ...
  #
  data[,c("region","kontoklasse", "funksjon", "art")] = CharacterDataFrame(data[,c("region","kontoklasse", "funksjon", "art")])


  warning0301 = FALSE

  if(lag0300 & !is.numeric(regioner)){
    if("0300" %in% data$region)
      warning("0300 finnes allerede i data. Bedre med lag0300=FALSE?")


    data0300 = data[data$region=="0301", ,drop=FALSE]

    if(NROW(data0300)==0)
      warning0301 = TRUE
    else{
      data0300$region = "0300"
      data = rbind(data,data0300)
    }
  }

  if(is.numeric(regioner)){
    uregioner = unique(data$region)
    if(regioner[1] < length(uregioner))
      regioner = uregioner[seq_len(regioner[1])]
    else
      regioner = NULL
  } else {
    if(any(c(Fgrepl("*",regioner), Fgrepl("?",regioner),Fgrepl("!",regioner),Fgrepl("-",regioner)))){
      regioner = WildcardGlobbingVector(unique(data$region),regioner)
    }
  }

  if(is.null(autoNetting))
    autoNetting = FALSE


  if(is.logical(autoNetting)){
    if(autoNetting){
      autoNetting = "AutoNettingCopy"
    } else {
      autoNetting = "AutoNettingIngenEndering"
    }
  }

  if(fixArtkode){
    artshierarki$to = AddLeadingZeros(artshierarki$to,3,warningTextArtskoder)
    artshierarki$from = AddLeadingZeros(artshierarki$from,3,warningTextArtskoder)
  }
  if(fixFunksjonkode){
    funksjonshierarki$to = AddLeadingZeros(funksjonshierarki$to,3,warningTextArtskoder)
    funksjonshierarki$from = AddLeadingZeros(funksjonshierarki$from,3,warningTextArtskoder)
  }

  if(isNullSum!=2){

    isnk = !is.null(artshierarki_nettinger_kasse)

    if(!isnk){
      warning("artshierarki_nettinger_kasse er ikke i input. Alle kasse-nettinger blir 0.")
    } else {

      #if(autoNetting)
      #  artshierarki_nettinger_kasse = AutoNetting(artshierarki_nettinger_kasse,
      #                                             artshierarki,"artshierarki_nettinger_kasse",
      #                                             toRemove = toRemove)
      if(fixArtkode){
        artshierarki_nettinger_kasse$to = AddLeadingZeros(artshierarki_nettinger_kasse$to,3,warningTextArtskoder)
        artshierarki_nettinger_kasse$from = AddLeadingZeros(artshierarki_nettinger_kasse$from,3,warningTextArtskoder)
      }

      artshierarki_nettinger_kasse =
        eval(parse(text=autoNetting))(
          nettinghierarki = artshierarki_nettinger_kasse,
          hierarki = artshierarki,
          title = "artshierarki_nettinger_kasse")
    }

    #if(autoNetting)
    #  artshierarki_nettinger = AutoNetting(artshierarki_nettinger,
    #                                       artshierarki,"artshierarki_nettinger",
    #                                       toRemove = toRemove)
    if(fixArtkode){
      artshierarki_nettinger$to = AddLeadingZeros(artshierarki_nettinger$to,3,warningTextArtskoder)
      artshierarki_nettinger$from = AddLeadingZeros(artshierarki_nettinger$from,3,warningTextArtskoder)
    }

    artshierarki_nettinger =
      eval(parse(text=autoNetting))(
        nettinghierarki = artshierarki_nettinger,
        hierarki = artshierarki,
        title = "artshierarki_nettinger")



    if(NROW(data_saer)==0){
      warning("Det er ingen rader i data_saer. Alt derfra blir 0.")
      data_saer = data[1, ,drop=FALSE]
      data_saer$belop = 0L
    }

    if(sum(names(data_saer)=="region")==0){
      if(any(names(data_saer)=="fylkesregion")){
        names(data_saer)[names(data_saer)=="fylkesregion"] <- "region"
        warning("'fylkesregion' omkodet til 'region' i data_saer")
      }
    }
    if(fixRegionkode)
      data_saer$region = FixRegionkode(data_saer$region,warningTextRegionskoder)
    if(fixArtkode)
      data_saer$art = AddLeadingZeros(data_saer$art,3,warningTextArtskoder)
    if(fixFunksjonkode)
      data_saer$funksjon = AddLeadingZeros(data_saer$funksjon,3,warningTextFunksjonskoder)


    varIdata = c("region","kontoklasse", "funksjon", "art", "belop") %in% names(data_saer)
    if(any(!varIdata)){
      stop(paste("Disse variablene mangler i data_saer:",
                 paste(c("region","kontoklasse", "funksjon", "art", "belop")[!varIdata],collapse=", ")))
    }

    if(!is.integer(data_saer$belop))
      integerInOutput = FALSE


    data_saer[,c("region","kontoklasse", "funksjon", "art")] = CharacterDataFrame(data_saer[,c("region","kontoklasse", "funksjon", "art")])

    if(lag0300){
      if("0300" %in% data_saer$region)
        warning("0300 finnes allerede i data_saer. Bedre med lag0300=FALSE?")

      data0300 = data_saer[data_saer$region=="0301", ,drop=FALSE]

      if(!NROW(data0300)==0){
        warning0301 = FALSE
        data0300$region = "0300"
        data_saer = rbind(data_saer,data0300)
      }
    }

  }


  if(warning0301)
    warning("0301 finnes ikke i input. 0300 blir ikke laget")


  #cat("\n                  Nettinger : ")

  # fjerner eventuelle dublikater og gir warning
  if(!is.null(arter)){
    arter = unique(arter)

    arterAll = unique(c(artshierarki$to,artshierarki$from,
                        artshierarki_nettinger$to,artshierarki_nettinger$from,
                        artshierarki_nettinger_kasse$to,artshierarki_nettinger_kasse$from,
                        unique(c(unique(data$art),unique(data_saer$art)))))


    if(any(c(Fgrepl("*",arter), Fgrepl("?",arter),Fgrepl("!",arter),Fgrepl("-",arter)))){
      arter = WildcardGlobbingVector(arterAll,arter)
    } else {
      arterIn =  arter %in% arterAll
      if(any(!arterIn)){
        StopOrWarning(paste("Disse artene finnes ikke i data eller hierarkier:",
                      paste(arter[!arterIn],collapse=", ")))
      }
    }

  }
  if(!is.null(funksjoner)){
    funksjoner = unique(funksjoner)
    funksjonerAll = unique(c(funksjonshierarki$to,funksjonshierarki$from,
                             unique(c(unique(data$funksjon),unique(data_saer$funksjon)))))

    if(any(c(Fgrepl("*",funksjoner), Fgrepl("?",funksjoner),Fgrepl("!",funksjoner),Fgrepl("-",funksjoner)))){
      funksjoner = WildcardGlobbingVector(funksjonerAll,funksjoner)
    } else {
      funksjonerIn = funksjoner  %in% funksjonerAll
      if(any(!funksjonerIn)){
        StopOrWarning(paste("Disse funksjonene finnes ikke i data eller hierarkier:",
                      paste(funksjoner[!funksjonerIn],collapse=", ")))
      }
    }

  }
  if(!is.null(kontoklasser)){
    kontoklasser = unique(kontoklasser)

    kontoklasserIn = kontoklasser %in% unique(c(unique(data$kontoklasse),unique(data_saer$kontoklasse)))

    if(any(!kontoklasserIn)){
      StopOrWarning(paste("Disse kontoklassene finnes ikke i data:",
                    paste(kontoklasser[!kontoklasserIn],collapse=", "),
                    ". Hvorvidt det blir med i output kommer an på annen input."))
    }

  }




  storkOrder = NULL
  cat("\n [ Kombinasjonsberegninger...")
  flush.console()

  # Reduserer storkombinasjoner dersom regioner,kombinasjoner, arter , funksjoner, kontoklasser finnes
  if(!is.null(storkombinasjoner)) {
    if(!is.null(regioner)){
      rr = storkombinasjoner$region %in% regioner
      storkombinasjoner = storkombinasjoner[rr, ,drop=FALSE]
    }
    if(!is.null(arter)){
      rr = storkombinasjoner$art %in% arter
      storkombinasjoner = storkombinasjoner[rr, ,drop=FALSE]
    }
    if(!is.null(funksjoner)){
      rr = storkombinasjoner$funksjon %in% funksjoner
      storkombinasjoner = storkombinasjoner[rr, ,drop=FALSE]
    }
    if(!is.null(kontoklasser)){
      rr = storkombinasjoner$kontoklasse %in% kontoklasser
      storkombinasjoner = storkombinasjoner[rr, ,drop=FALSE]
    }

    if(!is.null(kombinasjoner)){
      rr = !is.na(Match(storkombinasjoner[,names(kombinasjoner) ,drop=FALSE],kombinasjoner))
      storkombinasjoner = storkombinasjoner[rr, ,drop=FALSE]
    }
    if(NROW(storkombinasjoner)==0)
      stop("Matching av storkombinasjoner med annen input gir 0 rader")
  } else{
    if(!is.null(kombinasjoner)) { # Reduserer kombinasjoner dersom arter , funksjoner, kontoklasser finnes
      if(!is.null(arter)){
        rr = kombinasjoner$art %in% arter
        kombinasjoner = kombinasjoner[rr, ,drop=FALSE]
      }
      if(!is.null(funksjoner)){
        rr = kombinasjoner$funksjon %in% funksjoner
        kombinasjoner = kombinasjoner[rr, ,drop=FALSE]
      }
      if(!is.null(kontoklasser)){
        rr = kombinasjoner$kontoklasse %in% kontoklasser
        kombinasjoner = kombinasjoner[rr, ,drop=FALSE]
      }
      if(NROW(kombinasjoner)==0)
        stop("Matching av kombinasjoner med annen input gir 0 rader")
    }
  }



  if((!is.null(stjernetabell)) | output=="storkombinasjoner"){

    if(is.null(storkombinasjoner)){ # Kode opprinnelig i funksjon LesKostraRegnskapData
      storkombinasjoner = vector("list",4)
      names(storkombinasjoner) = c("region","kontoklasse","funksjon","art")
      if(is.null(regioner))
        storkombinasjoner$region = unique(c(unique(data$region),unique(data_saer$region)))
      else                          # FixRegionkode kan tas bort ######################################
      storkombinasjoner$region = unique(regioner)
      if(is.null(kombinasjoner)){

        if(!is.null(kontoklasser))
          storkombinasjoner$kontoklasse = kontoklasser
        else
          storkombinasjoner$kontoklasse = unique(c(unique(data$kontoklasse),unique(data_saer$kontoklasse)))

        if(!is.null(funksjoner))
          storkombinasjoner$funksjon = funksjoner
        else
          storkombinasjoner$funksjon = unique(c(funksjonshierarki$to,funksjonshierarki$from,
                                                unique(c(unique(data$funksjon),unique(data_saer$funksjon)))))
        if(!is.null(arter))
          storkombinasjoner$art = arter
        else
          storkombinasjoner$art = unique(c(artshierarki$to,artshierarki$from,
                                           artshierarki_nettinger$to,artshierarki_nettinger$from,
                                           artshierarki_nettinger_kasse$to,artshierarki_nettinger_kasse$from,
                                           unique(c(unique(data$art),unique(data_saer$art)))))
      } else {
        storkombinasjoner =CrossCodeFrames( # HierarchicalWildcardGlobbing en gang først kan være effektivt
          #HierarchicalWildcardGlobbing(as.data.frame(storkombinasjoner[1]),stjernetabell,makeWarning=FALSE,printInfo=FALSE,
          #                             useMatrixToDataFrame=useMatrixToDataFrame),
          CharacterDataFrame(as.data.frame(storkombinasjoner[1])),
          HierarchicalWildcardGlobbing(kombinasjoner,stjernetabell,makeWarning=FALSE,printInfo=FALSE,
                                       useMatrixToDataFrame=useMatrixToDataFrame),
          useMatrixToDataFrame=useMatrixToDataFrame)
        #storkombinasjoner =CrossCodeFrames(as.data.frame(storkombinasjoner[1]),kombinasjoner)
      }

    }


    #return(storkombinasjoner)

    #print(dim(storkombinasjoner))
    if(!is.null(stjernetabell))
      storkombinasjoner = HierarchicalWildcardGlobbing(storkombinasjoner,stjernetabell,printInfo=printInfo,
                                                       useMatrixToDataFrame=useMatrixToDataFrame)
    if(output=="storkombinasjoner"){
      if(!is.data.frame(storkombinasjoner)){
        x=CrossCodeFrames(as.data.frame(storkombinasjoner[1]),as.data.frame(storkombinasjoner[2]),
                          useMatrixToDataFrame=useMatrixToDataFrame)
        x=CrossCodeFrames(x,as.data.frame(storkombinasjoner[3]),
                          useMatrixToDataFrame=useMatrixToDataFrame)
        x=CrossCodeFrames(x,as.data.frame(storkombinasjoner[4]),
                          useMatrixToDataFrame=useMatrixToDataFrame)

        if(printData)
          PrintHeadTail(x, title = "output")
        return(x)
      }
      cat("]\n")
      flush.console()

      if(printData)
        PrintHeadTail(storkombinasjoner, title = "output")


      if(forceStringsAsFactors)
        return(ForceFactorDataFrame(storkombinasjoner))


      return(storkombinasjoner)
    }

  }

  if(!is.null(storkombinasjoner)){
    if(NROW(storkombinasjoner)==0)
      stop("Bruk av stjernetabell (og annen input) har gitt 0 storkombinasjoner.")

    cat("] [ Fra til storkombinasjoner til kombinasjoner og regioner ...")
    flush.console()

    regioner = as.character(storkombinasjoner$region)
    regionerInt = as.integer(factor(regioner))
    regioner = unique(regioner)

    cat(" RowGroups(...")
    flush.console()

    rgStork = RowGroups(storkombinasjoner[, !(names(storkombinasjoner) %in% "region")],returnGroups = TRUE)
    cat(")")
    flush.console()

    storkOrder = matrix(NaN,NROW(rgStork$groups),length(regioner))
    storkOrder[cbind(rgStork$idx,regionerInt)] = seq_len(NROW(storkombinasjoner))
    storkOrder = order(as.vector(storkOrder))[seq_len(NROW(storkombinasjoner))]
    kombinasjoner = rgStork$groups
    rm(rgStork)
    #datacolvar = as.character(data[,colVar])
    #colSelect = unique(as.character(colSelect))
    #colSelect = unique(as.charact

    # Sparer minne og mer effektiv på slutten

    if(isNullSum==2)
      rm(storkombinasjoner) # trengs ikke mer
    else{
      if(useMatrixToDataFrame)
        storkombinasjoner = DataFrameToMatrix(storkombinasjoner)
    }

  }


  if(is.null(kombinasjoner) & (!isNullSum==2) ){    # Bare ved netting-input
    #if(is.null(kombinasjoner) ){
    cat("] [ Lager kombinasjoner ...")
    flush.console()
    if(is.null(kontoklasser))
      kontoklasser = unique(c(unique(data$kontoklasse),unique(data_saer$kontoklasse)))

    if(is.null(funksjoner))
      funksjoner = unique(c(funksjonshierarki$to,funksjonshierarki$from,
                            unique(c(unique(data$funksjon),unique(data_saer$funksjon)))))
    if(is.null(arter))
      arter = unique(c(artshierarki$to,artshierarki_nettinger$to,artshierarki_nettinger_kasse$to))

    kontoklasser = data.frame(kontoklasse = kontoklasser, stringsAsFactors = FALSE)
    funksjoner = data.frame(funksjon = funksjoner, stringsAsFactors = FALSE)
    arter = data.frame(art = arter, stringsAsFactors = FALSE)

    kombinasjoner = CrossCodeFrames(arter,funksjoner, useMatrixToDataFrame=useMatrixToDataFrame)
    kombinasjoner = CrossCodeFrames(kombinasjoner,kontoklasser, useMatrixToDataFrame=useMatrixToDataFrame)


  }

  if(!is.null(kombinasjoner))
    if(NROW(kombinasjoner)==0)
      stop("Noe er galt det er 0 kombinasjoner")

  if(!is.null(regioner))
    if(length(regioner)==0)
      stop("Noe er galt det er 0 regioner")



  cat("]\n")
  flush.console()



  if(isNullSum==2){

    if(output == "hierarkier"){

      w = KostraRegnskap1(data,funksjonshierarki,artshierarki,
                          regioner=regioner,kombinasjoner=kombinasjoner,
                          inputInOutput=FALSE,output="dummyHierarchies",
                          useMatrixToDataFrame = useMatrixToDataFrame )

      hdf = HierarchyFromDummy(w$art)
      hdf$hierarki = "art"
      hd2 = HierarchyFromDummy(w$funksjon)
      hd2$hierarki = "funksjon"
      hdf = rbind(hdf,hd2)
      rownames(hdf) = NULL
      return(hdf)
    }

    w = KostraRegnskap1(data,funksjonshierarki,artshierarki,
                        regioner=regioner,kombinasjoner=kombinasjoner,
                        output="data.frame", useMatrixToDataFrame = useMatrixToDataFrame )

    if(integerInOutput)
      w$belop = LagInteger(w$belop)

    if(!is.null(storkOrder)){ # DataFrameToMatrix hjelper ikke her
      cat(" [ utvalg til storkombinasjoner...")
      flush.console()
      w = w[storkOrder, ,drop=FALSE]
      cat("]\n")
      flush.console()
    }
    rownames(w) = NULL
    flush.console()

    if(printData)
      PrintHeadTail(w, title = "output")

    if(forceStringsAsFactors)
      return(ForceFactorDataFrame(w))


    return(w)
  }




  if(FALSE){   # Kode brukt til å generere små datasett
    if(is.null(regioner) & ntilkomb>0)
      regioner = rep_len(rev( unique(data$region)),ntilkomb)
    #regioner = NULL

    if(is.null(kombinasjoner) & ntilkomb>0){
      kombinasjoner = CrossCodeFrames(data.frame(kontoklasse = unique(data$kontoklasse)),
                                      CrossCodeFrames(data.frame(  funksjon  =  rep_len(rev(unique(funksjonshierarki$to)),ntilkomb)),
                                                      data.frame( art =c( rep_len(rev(unique(artshierarki_nettinger$to)),ntilkomb),rep_len(rev(unique(artshierarki$to)),ntilkomb))    ),
                                                      useMatrixToDataFrame=useMatrixToDataFrame),
                                      useMatrixToDataFrame=useMatrixToDataFrame)
      rownames(kombinasjoner) = NULL
    }
  }




  artshierarki =  RemoveDuplicated(artshierarki,1:3)     #artshierarki[!duplicated(artshierarki[,1:3]),]
  artshierarki_nettinger = RemoveDuplicated(artshierarki_nettinger,1:3) #artshierarki_nettinger[!duplicated(artshierarki_nettinger[,1:3]),]
  if(isnk) artshierarki_nettinger_kasse = RemoveDuplicated(artshierarki_nettinger_kasse,1:3)
  funksjonshierarki = RemoveDuplicated(funksjonshierarki,1:3) #funksjonshierarki[!duplicated(funksjonshierarki[,1:3]),]

  if(FALSE){ # Etter Autonetting kan det ikke gis warning på dette. Nå er det slik det skal være.
    # Warning dersom et problem oppdages
    selvdefinert = unique(artshierarki_nettinger$from)[unique(artshierarki_nettinger$from) %in% unique(artshierarki_nettinger$to)]
    fromSelvdefinert = artshierarki_nettinger$from %in% selvdefinert

    # Warning dersom et problem oppdages
    problemselvdefinert = selvdefinert[selvdefinert %in% unique(c(data_saer$art,artshierarki$from,artshierarki$to))]
    if(length(problemselvdefinert))
      warning(paste("nettinger_saer: Koder som finnes fra før erstattes av nye beregninger. Dette gjelder:",paste(problemselvdefinert,collapse=", ")))

    if(isnk){
      # Warning dersom et problem oppdages
      selvdefinert = unique(artshierarki_nettinger_kasse$from)[unique(artshierarki_nettinger_kasse$from) %in% unique(artshierarki_nettinger_kasse$to)]
      fromSelvdefinert = artshierarki_nettinger_kasse$from %in% selvdefinert

      # Warning dersom et problem oppdages
      problemselvdefinert = selvdefinert[selvdefinert %in% unique(c(data$art,artshierarki$from,artshierarki$to))]
      if(length(problemselvdefinert))
        warning(paste("nettinger_saer_kasse: Koder som finnes fra før erstattes av nye beregninger. Dette gjelder:",paste(problemselvdefinert,collapse=", ")))

    }

  }


  allearter = unique(c(unique(data_saer$art),unique(data$art),
                       artshierarki_nettinger$to,artshierarki_nettinger$from,
                       artshierarki_nettinger_kasse$to,artshierarki_nettinger_kasse$from,
                       artshierarki$to,artshierarki$from))

  fjernarter = allearter %in% unique(artshierarki_nettinger$to)
  allearterredusert = allearter[!fjernarter]

  if(isnk){
    fjernarter_kasse = allearter %in% unique(artshierarki_nettinger_kasse$to)
    allearterredusert_kasse = allearter[!fjernarter_kasse]
  }



  if(is.null(regioner))
    regioner= unique(c(unique(data_saer$region),unique(data$region)))



  kombinasjonerIinput = !is.null(kombinasjoner)
  if(!kombinasjonerIinput)  # Lager "tulle"-kombinsjoner i første omgang siden første rad blir brukt nedenfor
    kombinasjoner =  KostraRegnskap1(data[1,],funksjonshierarki[1, ,drop=FALSE],artshierarki[1, ,drop=FALSE],regioner=unique(data_saer$region)[1],inputInOutput=TRUE,output="crossCode",
                                     useMatrixToDataFrame = useMatrixToDataFrame, colNotInDataWarning = FALSE )


  if(isnk){
    artkomb4 = kombinasjoner[rep(1,length(allearterredusert_kasse)), ,drop=FALSE]
    artkomb4$art = allearterredusert_kasse
    artkomb4$funksjon = data$funksjon[1]
  }


  artkomb3 = kombinasjoner[rep(1,length(allearterredusert)), ,drop=FALSE]
  artkomb3$art = allearterredusert
  artkomb3$funksjon = data$funksjon[1]


  # allearter2 = unique(c(unique(data_saer$art),artshierarki$from)) denne gikk ikkr

  allearter2 = unique(data_saer$art)
  artkomb2 = kombinasjoner[rep(1,length(allearter2)), ,drop=FALSE]
  artkomb2$art = allearter2
  artkomb2$funksjon = data$funksjon[1]

  if(isnk){
    allearter1 = unique(data$art)
    artkomb1 = kombinasjoner[rep(1,length(allearter1)), ,drop=FALSE]
    artkomb1$art = allearter1
    artkomb1$funksjon = data$funksjon[1]
  }

  # Etter endring er kombinasjoner her aldri null, men lar koden stå
  if(!kombinasjonerIinput) # setter kombinasjoner tilbake til NULL
    kombinasjoner = NULL
  else
    kombinasjoner = CharacterDataFrame(kombinasjoner)


  #mat2H <- KostraRegnskap1(artkomb2,funksjonshierarki[1, ,drop=FALSE] ,artshierarki,regioner=regioner2,inputInOutput=TRUE,output="dummyHierarchies")$art
  #mat3H <- KostraRegnskap1(artkomb3,funksjonshierarki[1, ,drop=FALSE],artshierarki_nettinger,regioner=regioner2,inputInOutput=TRUE,output="dummyHierarchies")$art  # dvs mat3a


  ikkeendring0404 = FALSE  # Kan brukes for å gjenskape tidligere resultat



  if(output=="beregningFunksjonsHierarki"){
    return(BeregnetFunksjonsHierarki(data,funksjonshierarki,artshierarki,
                                  data_saer,artshierarki_nettinger,artshierarki_nettinger_kasse,
                                  periode = periode,
                                  regnskapsomfang = "A", #regnskapsomfanger,
                                  kombinasjoner=kombinasjoner,
                                  regioner=regioner,
                                  form=form,
                                  toKoder = TRUE, doStack = TRUE,
                                  fixHierarchy = FALSE))
  }


  if(output=="beregningHierarki"){
    if(bareEnFormel){
      kode = apply(kombinasjoner[,c(3,2,1)],1,function(x) paste(x,collapse=":"))
      formelKode = match(kode, VenstreKoder(formler$formel)) #

      if(sum(!is.na(formelKode))>1)
        stop("Bare en formelkombinasjon ved beregningHierarki")

      if(any(!is.na(formelKode))){
        form = formler$formel[formelKode[!is.na(formelKode)]]
      }
      else{
        form = NULL
      }
    } else {
      form=formler$formel
    }

    beregnetHierarki <-  BeregnetFormelHierarki(data,funksjonshierarki,artshierarki,
                                  data_saer,artshierarki_nettinger,artshierarki_nettinger_kasse,
                                  periode = periode,
                                  kombinasjoner=kombinasjoner,
                                  regioner=regioner,
                                  form=form,
                                  toKoder = TRUE, doStack = TRUE,
                                  fixHierarchy = FALSE, onlyB=onlyB)
    if(!beregningHierarkiInput)
      return(beregnetHierarki)    # bruk returnMatrixComponents

    if(length(unique(beregnetHierarki$region)) != 1)
      stop("Må være 1 region når beregningHierarkiInput")

    beregnetHierarki_type = CharacterReCode(beregnetHierarki$type_from, c("Input_Kasse", "Input_Saer"), c("Kasse", "Saer"))


    beregnetHierarki =
      SelectAndRename(beregnetHierarki,
                               oldNames= c("periode", "region", "kontoklasse_from", "funksjon_from", "art_from", "from", "sign", "type_from", "belop"),
                               newNames= c("periode", "region", "kontoklasse",      "funksjon",      "art",      "from", "sign", "type",    "belop"))


    rgBH =RowGroups(beregnetHierarki[,c("kontoklasse", "funksjon", "art")], returnGroups = TRUE, returnGroupsId = FALSE)

    krMC = KostraRegnskap(data, funksjonshierarki, artshierarki, data_saer,
                          artshierarki_nettinger, artshierarki_nettinger_kasse,
                          kombinasjoner = rgBH$groups,
                          regioner = unique(beregnetHierarki$region),
                          regnskapsomfang = regnskapsomfanger,
                          output = "matrixComponents",
                          printData = FALSE, autoNetting = FALSE, lag0300 = FALSE,
                          fixRegionkode = FALSE, fixArtkode = FALSE, fixFunksjonkode = FALSE)


    factorType = factor(beregnetHierarki_type)
    integerType <- as.integer(factorType)
    fromMatrix <- matrix("", dim(rgBH$groups)[1], max(integerType))
    colnames(fromMatrix) <- levels(factorType)
    fromMatrix[cbind(rgBH$idx, integerType)] <- beregnetHierarki$from




    RowMC = function(a, row){
      a$toCrossCode = a$toCrossCode[row,  ,drop=FALSE]
      a$dataDummyHierarchy = a$dataDummyHierarchy[row,  ,drop=FALSE]
      a
    }
    LagBeregningInputRow = function(a1, a2, a3, a4, periode, row){
      LagBeregningInput(a1= RowMC(a1,row), a2= RowMC(a2,row), a3= RowMC(a3,row), a4= RowMC(a4,row), periode=periode)
    }

    LagBeregningInputRowkrMC = function(krMC, periode, row){
      LagBeregningInputRow(krMC$a1, krMC$a2, krMC$a3, krMC$a4, periode, row)
    }


    hierarkiInput = NULL

    if("formel" %in% colnames(fromMatrix)){
      for(i in which(fromMatrix[, "formel"] != "")){
        krBI = KostraRegnskap(data, funksjonshierarki, artshierarki, data_saer,
                              artshierarki_nettinger, artshierarki_nettinger_kasse,
                              kombinasjoner = rgBH$groups[i, ,drop=FALSE],
                              regioner = unique(beregnetHierarki$region),
                              regnskapsomfang = regnskapsomfanger,
                              formler = formler,
                              output = "beregningInput",
                              printInfo = FALSE,
                              printData = FALSE, autoNetting = FALSE, lag0300 = FALSE,
                              fixRegionkode = FALSE, fixArtkode = FALSE, fixFunksjonkode = FALSE)
        krBI = cbind(krBI, from = fromMatrix[i, "formel"], stringsAsFactors = FALSE)
        hierarkiInput = rbind(hierarkiInput,krBI, stringsAsFactors = FALSE)
      }

    }





    if("Konsern" %in% colnames(fromMatrix)){
      for(i in which(fromMatrix[, "Konsern"] != "")){
        krBI = cbind(LagBeregningInputRowkrMC(krMC, periode, i), from = fromMatrix[i, "Konsern"], stringsAsFactors = FALSE)
        hierarkiInput = rbind(hierarkiInput,krBI, stringsAsFactors = FALSE)
      }
    }

    type1to4 = c("Kasse",  "Saer", "Nett_Saer", "Nett_Kasse")
    a1to4 = c("a1", "a2", "a3", "a4")


    for(i in 1:4) if(type1to4[i] %in% colnames(fromMatrix)){
      rows = which(fromMatrix[, type1to4[i]] != "")
      krBI = MultiRowHierarchyComputations(RowMC(krMC[[a1to4[i]]], rows), valueName = "belop", indName = "from", ind = fromMatrix[rows, type1to4[i]])
      krBI = cbind(periode=periode, region= unique(beregnetHierarki$region),krBI, type=type1to4[i], stringsAsFactors = FALSE)
      krBI$type[krBI$sign==0] = paste("OUTPUT",type1to4[i], sep="_")
      hierarkiInput = rbind(hierarkiInput,krBI, stringsAsFactors = FALSE)
    }

    #periode region kontoklasse funksjon   art sign              type    belop        from
    #kontoklasse art funksjon sign   belop        from

    #colnames(fromMatrix)

    #output = "beregningInput"


    return(list(beregnetHierarki=beregnetHierarki,  #krMC=krMC, fromMatrix=fromMatrix,
                hierarkiInput= IntegerDataFrame(hierarkiInput[, c("periode", "region", "kontoklasse", "funksjon", "art", "sign", "type", "belop", "from")],
                                                makeWarning = TRUE)))
  }



  if(beregningInput){
    if(NROW(kombinasjoner) != 1)
      stop("Kun en kombinasjon mulig ved beregningInput")

  kombinasjonerDI = kombinasjoner    ## Denne kode er laget før AutoFormel ble endret til å gjøre om to-komponent-formler til tre - derfor "DI"
  kombinasjonerDI$kontoklasse = "DI"
  fm = FormelMatrise(formler$formel)
  matchFormel = Match(kombinasjoner,fm$codesLeft)
  if(is.na(matchFormel))
    matchFormel = Match(kombinasjonerDI,fm$codesLeft)
  if(!is.na(matchFormel)){
    fm = FormelMatrise(formler$formel[matchFormel])
    if(fm$codesLeft$kontoklasse=="DI")
      fm$codesRight$kontoklasse = kombinasjoner$kontoklasse
    fm$codesLeft = kombinasjoner
    kombinasjoner = fm$codesRight
    formelMatrise = fm[c("codesLeft","codesRight","formelMatrise")]
  }
  else
    formelMatrise = NULL

    #return(list(kombinasjoner=kombinasjoner,fm=fm))
    #Match(a$kombinasjoner,fm$codesLeft)
  }


  #return(kombinasjoner)

  if(isnk)
    mat1H <- KostraRegnskap1(artkomb1,funksjonshierarki[1, ,drop=FALSE] ,artshierarki,inputInOutput=TRUE,output="dummyHierarchies",
                             useMatrixToDataFrame = useMatrixToDataFrame, colNotInDataWarning = FALSE )$art



  mat2H <- KostraRegnskap1(artkomb2,funksjonshierarki[1, ,drop=FALSE] ,artshierarki,inputInOutput=TRUE,output="dummyHierarchies",
                           useMatrixToDataFrame = useMatrixToDataFrame, colNotInDataWarning = FALSE )$art


  mat3H <- KostraRegnskap1(artkomb3,funksjonshierarki[1, ,drop=FALSE],artshierarki_nettinger,inputInOutput=FALSE,output="dummyHierarchies",
                           useMatrixToDataFrame = useMatrixToDataFrame, colNotInDataWarning = FALSE )$art  # dvs mat3a
  if(isnk)
    mat4H <- KostraRegnskap1(artkomb4,funksjonshierarki[1, ,drop=FALSE],artshierarki_nettinger_kasse,inputInOutput=FALSE,output="dummyHierarchies",
                             useMatrixToDataFrame = useMatrixToDataFrame, colNotInDataWarning = FALSE )$art  # dvs mat3a



  if(ikkeendring0404)
    mat3H <- KostraRegnskap1(artkomb3,funksjonshierarki[1, ,drop=FALSE],artshierarki_nettinger,inputInOutput=TRUE,output="dummyHierarchies",
                             useMatrixToDataFrame = useMatrixToDataFrame, colNotInDataWarning = FALSE )$art  # dvs mat3a


  nam = colnames(mat3H)[colnames(mat3H) %in% rownames(mat2H)]  # "EnKodeSoMiKkEfiNneS" tas bort
  ix3 = match(nam,colnames(mat3H))
  ix2 = match(nam,rownames(mat2H))

  if(isnk){
    nam_kasse = colnames(mat4H)[colnames(mat4H) %in% rownames(mat1H)]  # "EnKodeSoMiKkEfiNneS" tas bort
    ix4 = match(nam_kasse,colnames(mat4H))
    ix1 = match(nam_kasse,rownames(mat1H))
  }

  # Kan legge inn warning dersom noe måtte fjernes (dimnsjon redusert)

  mat32 = Mult(mat3H[,ix3,drop=FALSE], mat2H[ix2, ,drop=FALSE]) #mat3H[,ix3,drop=FALSE] %*% mat2H[ix2, ,drop=FALSE]
  if(isnk) mat41 = Mult(mat4H[,ix4,drop=FALSE], mat1H[ix1, ,drop=FALSE]) #mat4H[,ix4,drop=FALSE] %*% mat1H[ix1, ,drop=FALSE]



  arts32 =  HierarchyFromDummy(mat32 )
  colnames(arts32) = c("from","to","sign")
  arts32 = cbind(periode=periode,arts32)
  arts32 = arts32[arts32$from!=arts32$to,]


  if(isnk){
    arts41 =  HierarchyFromDummy(mat41 )
    colnames(arts41) = c("from","to","sign")
    arts41 = cbind(periode=periode,arts41)
    arts41 = arts41[arts41$from!=arts41$to,]
  }


  allRows = unique(c(rownames(mat32),rownames(mat2H)))
  allCols = unique(c(colnames(mat32),colnames(mat2H)))


  mat2all = Matrix(0,length(allRows),length(allCols))
  rownames(mat2all) = allRows
  colnames(mat2all) = allCols
  mat2all[match(rownames(mat2H),allRows),match(colnames(mat2H),allCols)] = mat2H
  mat32all = 0*mat2all


  if(ikkeendring0404)
    mat32all = mat2all

  mat32all[match(rownames(mat32),allRows),match(colnames(mat32),allCols)] = mat32

  mat2diff =  mat2all + mat32all     # Navn er diff pga formelfeil fra start

  artsDiff =  HierarchyFromDummy(mat2diff)
  colnames(artsDiff) = c("from","to","sign")
  artsDiff = cbind(periode=periode,artsDiff)

  artsDiff = artsDiff[artsDiff$from!=artsDiff$to,]


  if(output == "hierarkier4"){
    return(list(
      art  = HierarchyFromDummy(mat2all),
      artNett  = HierarchyFromDummy(mat32),
      artNettKasse  = HierarchyFromDummy(mat41),
      fun = HierarchyFromDummy(KostraRegnskap1(artkomb2,funksjonshierarki ,artshierarki[1, ,drop=FALSE],inputInOutput=FALSE,output="dummyHierarchies",
                                               useMatrixToDataFrame = useMatrixToDataFrame, colNotInDataWarning = FALSE )$funksjon)))
  }


  if(output == "hierarkier"){


    cat('\n output == "hierarkier" \n')



    mat2spes = as.matrix(mat2all) + as.matrix(mat32all)*1i


    hdf = HierarchyFromDummy(mat2spes)

    hdfsign = hdf$sign



    hdf$sign =NULL



    hdf$sign     = as.integer(base::Re(hdfsign))

    # return(list(hdf,hdfsign))

    hdf$netting =  as.integer(base::Im(hdfsign)) # Gir warning uten base::
    #hdf$netting =  base::Im(hdfsign)


    #hdf$region = reg

    hdf$hierarki = "art"

    hd2 = HierarchyFromDummy(KostraRegnskap1(artkomb2,funksjonshierarki ,artshierarki[1, ,drop=FALSE],inputInOutput=FALSE,output="dummyHierarchies",
                                             useMatrixToDataFrame = useMatrixToDataFrame , colNotInDataWarning = FALSE)$funksjon)

    hd2$netting = as.integer(NA)
    hd2$hierarki = "funksjon"



    hdf = rbind(hdf,hd2)


    rownames(hdf) = NULL


    return(list(hdf=hdf,arts41=arts41))
  }

  if(returnMatrixComponents|beregningInput){


    if(!onlyB)
      rowsInputArt = kombinasjoner$art %in% unique(c(unique(data$art),unique(data_saer$art)))


    #a1pluss2  <- KostraRegnskap1(data_saer,funksjonshierarki,artsDiff,regioner=regioner,kombinasjoner=kombinasjoner,output="matrixComponents",colNotInDataWarning=FALSE,
    #                            useMatrixToDataFrame = useMatrixToDataFrame )

    a1 <- KostraRegnskap1(data,funksjonshierarki,artshierarki,regioner=regioner,kombinasjoner=kombinasjoner,output="matrixComponents",
                          useMatrixToDataFrame = useMatrixToDataFrame )

    if(!onlyB){
    a2 <- KostraRegnskap1(data_saer,funksjonshierarki,artshierarki,regioner=regioner,kombinasjoner=kombinasjoner,output="matrixComponents", colNotInDataWarning=FALSE,
                          useMatrixToDataFrame = useMatrixToDataFrame )


    a3 <- KostraRegnskap1(data_saer,funksjonshierarki,arts32,regioner=regioner,kombinasjoner=kombinasjoner,output="matrixComponents",colNotInDataWarning=FALSE,
                          useMatrixToDataFrame = useMatrixToDataFrame )

    if(any(rowsInputArt))
      a3$dataDummyHierarchy[rowsInputArt, ] <- 0

    if(isnk){
      a4 <- KostraRegnskap1(data,     funksjonshierarki,arts41,regioner=regioner,kombinasjoner=kombinasjoner,output="matrixComponents",colNotInDataWarning=FALSE,
                            useMatrixToDataFrame = useMatrixToDataFrame )

      if(any(rowsInputArt))
       a4$dataDummyHierarchy[rowsInputArt, ] <- 0
    }
    } # end if(!onlyB)

    if(beregningInput){
      if(!is.null(formelMatrise)){
        a1$dataDummyHierarchy = Mult(formelMatrise$formelMatrise, a1$dataDummyHierarchy) #formelMatrise$formelMatrise %*% a1$dataDummyHierarchy
        a1$toCrossCode = formelMatrise$codesLeft
        rownames(a1$dataDummyHierarchy) = NULL

        if(!onlyB){
        a2$dataDummyHierarchy = Mult(formelMatrise$formelMatrise, a2$dataDummyHierarchy) #formelMatrise$formelMatrise %*% a2$dataDummyHierarchy
        a3$dataDummyHierarchy = Mult(formelMatrise$formelMatrise, a3$dataDummyHierarchy) #formelMatrise$formelMatrise %*% a3$dataDummyHierarchy
        a4$dataDummyHierarchy = Mult(formelMatrise$formelMatrise, a4$dataDummyHierarchy) #formelMatrise$formelMatrise %*% a4$dataDummyHierarchy


        a2$toCrossCode = formelMatrise$codesLeft
        a3$toCrossCode = formelMatrise$codesLeft
        a4$toCrossCode = formelMatrise$codesLeft

        rownames(a2$dataDummyHierarchy) = NULL
        rownames(a3$dataDummyHierarchy) = NULL
        rownames(a4$dataDummyHierarchy) = NULL
        } # end if(!onlyB)
      }
      if(onlyB)
        return(LagBeregningInputB(a1=a1,periode=periode))
      return(LagBeregningInput(a1=a1,a2=a2,a3=a3,a4=a4, periode=periode))
    }

    if(onlyB)
      return(list(a1=a1))
    return(list(a1=a1,a2=a2,a3=a3,a4=a4))

  }


  if(smartB){
    cat("\n                      Kasse : ")
    flush.console()

    if(is.null(kombinasjoner)){ # Etter endring er kombinasjoner her aldri null, men lar koden stå

      mat1AndCrossCode <- KostraRegnskap1(data,funksjonshierarki,artshierarki,regioner=regioner,kombinasjoner=kombinasjoner,output="outputMatrixWithCrossCode",
                                          useMatrixToDataFrame = useMatrixToDataFrame )

      mat1 = mat1AndCrossCode[[1]]
      kombinasjoner = CharacterDataFrame(mat1AndCrossCode[[2]])
      mat1AndCrossCode = NULL
    } else
      mat1 <- KostraRegnskap1(data,funksjonshierarki,artshierarki,regioner=regioner,kombinasjoner=kombinasjoner,output="outputMatrix",
                              useMatrixToDataFrame = useMatrixToDataFrame )



    makeMatDiff = TRUE
    if(!is.null(regnskapsomfanger))
      if(regnskapsomfanger=="B")
        makeMatDiff = FALSE


    if(makeMatDiff){
      rowsInputArt = kombinasjoner$art %in% unique(c(unique(data$art),unique(data_saer$art)))


      cat("\n     Saerbedrift + Nettinger: ")
      flush.console()
      matDiff  <- KostraRegnskap1(data_saer,funksjonshierarki,artsDiff,regioner=regioner,kombinasjoner=kombinasjoner,output="outputMatrix",colNotInDataWarning=FALSE,
                                  useMatrixToDataFrame = useMatrixToDataFrame )

      if(isnk){
        cat("\n             Kasse-nettinger: ")
        flush.console()

        mat4 <- KostraRegnskap1(data,funksjonshierarki,arts41,regioner=regioner,kombinasjoner=kombinasjoner,output="outputMatrix",colNotInDataWarning=FALSE,
                                useMatrixToDataFrame = useMatrixToDataFrame )
        if(any(rowsInputArt))
          mat4[rowsInputArt, ] <- 0

        matDiff = matDiff + mat4
      } else {
        mat4 = 0*matDiff   # Settes til 0 når kasse-nettinger ikke er i input
      }

    }
    else
      matDiff =NULL

  } else {  # ikke smartB


    cat("\n     Saerbedrift + Nettinger: ")
    flush.console()

    if(is.null(kombinasjoner)){ # Etter endring er kombinasjoner her aldri null, men lar koden stå
      matDiffAndCrossCode <- KostraRegnskap1(data_saer,funksjonshierarki,artsDiff,regioner=regioner,kombinasjoner=kombinasjoner,output="outputMatrixWithCrossCode",colNotInDataWarning=FALSE,
                                             useMatrixToDataFrame = useMatrixToDataFrame )
      matDiff = matDiffAndCrossCode[[1]]
      kombinasjoner = CharacterDataFrame(matDiffAndCrossCode[[2]])
      matDiffAndCrossCode = NULL
    } else
      matDiff  <- KostraRegnskap1(data_saer,funksjonshierarki,artsDiff,regioner=regioner,kombinasjoner=kombinasjoner,output="outputMatrix",colNotInDataWarning=FALSE,
                                  useMatrixToDataFrame = useMatrixToDataFrame )

    rowsInputArt = kombinasjoner$art %in% unique(c(unique(data$art),unique(data_saer$art)))


    cat("\n                      Kasse : ")
    flush.console()

    mat1 <- KostraRegnskap1(data,funksjonshierarki,artshierarki,regioner=regioner,kombinasjoner=kombinasjoner,output="outputMatrix",
                            useMatrixToDataFrame = useMatrixToDataFrame )


    if(isnk){
      cat("\n             Kasse-nettinger: ")
      flush.console()

      mat4 <- KostraRegnskap1(data,funksjonshierarki,arts41,regioner=regioner,kombinasjoner=kombinasjoner,output="outputMatrix",colNotInDataWarning=FALSE,
                              useMatrixToDataFrame = useMatrixToDataFrame )

      if(any(rowsInputArt))
        mat4[rowsInputArt, ] <- 0

      matDiff = matDiff + mat4
    } else {
      mat4 = 0*matDiff   # Settes til 0 når kasse-nettinger ikke er i input
    }

  } # end ikke smartB



  if(!is.null(matDiff))
    if(any(colnames(mat1)!= colnames(matDiff)))
      stop("Noe gikk galt")


  if(output=="fire"){
    cat("\n                  Nettinger : ")
    flush.console()
    #  mat2 <- KostraRegnskap1(data_saer,funksjonshierarki,artshierarki,regioner=regioner,kombinasjoner=kombinasjoner,output="outputMatrix")
    #  Den trengs ikke å beregnes direkte
    #  Dessuten er den indirekte bedre hvis det mangle i hierakitabell ..... se slack-diskusjon ... gamle data ..
    mat3 <- KostraRegnskap1(data_saer,funksjonshierarki,arts32,regioner=regioner,kombinasjoner=kombinasjoner,output="outputMatrix",colNotInDataWarning=FALSE,
                            useMatrixToDataFrame = useMatrixToDataFrame )

    if(any(rowsInputArt))
      mat3[rowsInputArt, ] <- 0

    #(Matrise 2 – Matrise 3) + Matrise 1 = Konsern
    if(any(colnames(mat1)!= colnames(mat3)))
      stop("Noe gikk galt")
    #mat2 = matDiff + mat3


    #if(returnMatriser){
    #
    #  return(list(kombinasjoner=kombinasjoner,
    #              mat1 = mat1, mat2 = matDiff - mat3, mat3 = mat3))
    #
    #}


  }




  valueVar = "belop"
  colVar="region"
  stringsAsFactors = FALSE
  cat("\n                               [ output='data.frame'...")
  flush.console()

  #kasse   = as.matrix(mat1)
  #konsern = as.matrix(matDiff+mat1)


  if(output=="fire"){
    z=data.frame(kasse = as.vector(as.matrix(mat1)),
                 saerbedrift = as.vector(as.matrix(matDiff - mat3 - mat4)),
                 nettinger_kasse = as.vector(as.matrix(mat4)),
                 nettinger_saer = as.vector(as.matrix(mat3)),
                 konsern = as.vector(as.matrix(matDiff+mat1)),
                 stringsAsFactors=stringsAsFactors)

  } else {

    #x = c(as.vector(as.matrix(matDiff+mat1)),as.vector(as.matrix(mat1)))

    if(smartB & !is.null(regnskapsomfanger)){
      if(regnskapsomfanger=="A"){
        z=data.frame(a=as.vector(as.vector(as.matrix(matDiff+mat1))),stringsAsFactors=stringsAsFactors)
        regnskapsomfang =  data.frame(a=rep(c("A"), times = 1, each = cumprod(dim(mat1))[2]  ),stringsAsFactors=stringsAsFactors)
      }
      if(regnskapsomfanger=="B"){
        z=data.frame(a=as.vector(as.vector(as.matrix(mat1))),stringsAsFactors=stringsAsFactors)
        regnskapsomfang =  data.frame(a=rep(c("B"), times = 1, each = cumprod(dim(mat1))[2]  ),stringsAsFactors=stringsAsFactors)
      }

    } else{
      z=data.frame(a=as.vector(c(as.vector(as.matrix(matDiff+mat1)),as.vector(as.matrix(mat1)))),stringsAsFactors=stringsAsFactors)
      regnskapsomfang =  data.frame(a=rep(c("A","B"), times = 1, each = cumprod(dim(mat1))[2]  ),stringsAsFactors=stringsAsFactors)
    }
    names(z) = valueVar

    names(regnskapsomfang) = "regnskapsomfang"
    rownames(regnskapsomfang) = NULL

  }

  cat(".")
  flush.console()
  if(integerInOutput){
    for(i in seq_len(length(z))){
      z[[i]] = LagInteger(z[[i]])
    }
  }
  cat(".")
  flush.console()


  constantsInOutput=data.frame(periode=periode,stringsAsFactors=stringsAsFactors)
  #return(list(x=x,colVar=colVar))

  colDataSelected =  data.frame(a=rep(colnames(mat1), times = 1, each =dim(mat1)[1]),stringsAsFactors=stringsAsFactors)
  names(colDataSelected) = colVar





  # Sikre mot rownames warning
  rownames(constantsInOutput) = NULL
  rownames(colDataSelected) = NULL
  rownames(kombinasjoner) = NULL
  rownames(z) = NULL



  if(is.null(storkOrder)){
    if(output=="fire")
      w=cbind(constantsInOutput,colDataSelected,kombinasjoner,z)
    else
      w=cbind(constantsInOutput,regnskapsomfang,colDataSelected,kombinasjoner,z)
  } else{

    #lss = ls()
    #rm(list=lss[!(lss %in% c("rader0warning","smartB","regnskapsomfanger","returnFormelData","formler","printData","useMatrixToDataFrame","output","storkombinasjoner","w","z","storkOrder","constantsInOutput","regnskapsomfang","forceStringsAsFactors"))])

    if(useMatrixToDataFrame) {
      #
      #  Prøvde å gjemme dette i egen funksjon, men det krever mer minne
      #
      #
      #return(outputMatrixToDataFrame(output,
      #                                   storkombinasjoner,
      #                                   w,z,storkOrder,
      #                                   constantsInOutput,
      #                                   regnskapsomfang,
      #                                   forceStringsAsFactors))

      namesDFs = attr(storkombinasjoner,"namesDF")
      classDFs = attr(storkombinasjoner,"classDF")
      levelsDFs = attr(storkombinasjoner,"levelsDF")

      namesDFz = names(z)
      z = as.matrix(z)
      classDFz = rep(class(z[1,1]),length(namesDFz))
      levelsDFz = vector("list",length(namesDFz))

      constantsInOutput =  DataFrameToMatrix(constantsInOutput)

      if(output=="fire"){
        w=cbind(constantsInOutput[rep(1,NROW(z)), ,drop=FALSE])
        namesDFw = attr(constantsInOutput,"namesDF")
        classDFw = attr(constantsInOutput,"classDF")
        levelsDFw = attr(constantsInOutput,"levelsDF")

      }
      else{
        regnskapsomfang = DataFrameToMatrix(regnskapsomfang)
        w=cbind(constantsInOutput[rep(1,NROW(z)), ,drop=FALSE],regnskapsomfang)
        namesDFw = c(attr(constantsInOutput,"namesDF"),attr(regnskapsomfang,"namesDF"))
        classDFw = c(attr(constantsInOutput,"classDF"),attr(regnskapsomfang,"classDF"))
        levelsDFw = c(attr(constantsInOutput,"levelsDF"),attr(regnskapsomfang,"levelsDF"))
        if( !(smartB & !is.null(regnskapsomfanger)) )
          storkOrder = c(storkOrder,round(NROW(w)/2)+storkOrder)
      }


      namesDFs = c(namesDFw,namesDFs,namesDFz)
      classDFs = c(classDFw,classDFs,classDFz)
      levelsDFs = c(levelsDFw,levelsDFs,levelsDFz)

      cat("..")
      flush.console()

      ##lss = ls()
      ##rm(list=lss[!(lss %in% c("rader0warning","smartB","regnskapsomfanger","returnFormelData","formler","printData","storkombinasjoner","w","z","storkOrder","namesDFs","classDFs","levelsDFs","forceStringsAsFactors"))])


      w  = w[storkOrder, ,drop=FALSE]
      cat("..")
      flush.console()
      z  = z[storkOrder, ,drop=FALSE]


      rm("storkOrder")

      cat("..")
      flush.console()


      nCOLstorkombinasjoner = NCOL(storkombinasjoner)

      storkombinasjoner = storkombinasjoner[ , c(rep(1,NCOL(w)),seq_len(nCOLstorkombinasjoner),rep(1,NCOL(z))) , drop=FALSE]


      reprow = round(NROW(w)/NROW(storkombinasjoner))

      if( !(reprow==1 | reprow==2))
        stop("Noe er galt")

      cat("::")
      flush.console()

      if(reprow==2){
        storkombinasjoner = rbind(storkombinasjoner,storkombinasjoner)
        #storkombinasjoner = storkombinasjoner[rep(seq_len(NROW(storkombinasjoner)),2), ,drop=FALSE]

      }
      cat(".")
      flush.console()

      ncolw = NCOL(w)

      storkombinasjoner[,seq_len(ncolw)] = w
      rm("w")
      cat(".")
      flush.console()


      storkombinasjoner[, (nCOLstorkombinasjoner+ncolw +seq_len(NCOL(z)))  ]  = z
      rm("z")

      cat(".")
      flush.console()



      attr(storkombinasjoner,"namesDF") = namesDFs
      attr(storkombinasjoner,"classDF") = classDFs
      attr(storkombinasjoner,"levelsDF") = levelsDFs



      storkombinasjoner = MatrixToDataFrame(storkombinasjoner,forceStringsAsFactors=forceStringsAsFactors)



      cat("]\n\n")
      flush.console()




      if(!smartB & !is.null(regnskapsomfanger))
        storkombinasjoner = storkombinasjoner[storkombinasjoner$regnskapsomfang==regnskapsomfanger , ,drop=FALSE]


      rownames(storkombinasjoner) = NULL



      if(!is.null(formler)){
        fk = FormelKorreksjoner(formler$formel,storkombinasjoner,data, funksjonshierarki, artshierarki, data_saer,
                                artshierarki_nettinger, artshierarki_nettinger_kasse,regioner, returnFormelData, rader0warning, printData)
        if(returnFormelData)
          return(fk)

        #return(fk)
        for(i in seq_len(length(fk$nyedata))){
          #cat("\n=============================== Bytter ut:",i)
          storkombinasjoner[fk$rader[[i]], ] = fk$nyedata[[i]]
        }
      }



      if(printData)
        PrintHeadTail(storkombinasjoner, title = "output")


      return(storkombinasjoner)
    }  # Ferdig med if(useMatrixToDataFrame)



    if(output=="fire"){
      w = cbind(constantsInOutput[rep(1,NROW(z)), ,drop=FALSE])
    }
    else{
      w=cbind(constantsInOutput[rep(1,NROW(z)), ,drop=FALSE],regnskapsomfang)
      if( !(smartB & !is.null(regnskapsomfanger)) )
        storkOrder = c(storkOrder,round(NROW(w)/2)+storkOrder)
    }

    w  = w[storkOrder, ,drop=FALSE]
    cat("..")
    flush.console()
    z  = z[storkOrder, ,drop=FALSE]


    rm("storkOrder")
    cat("..")
    flush.console()


    nCOLstorkombinasjoner = NCOL(storkombinasjoner)

    namesStorkombinasjoner = names(storkombinasjoner)

    storkombinasjoner = storkombinasjoner[ , c(rep(1,NCOL(w)),seq_len(nCOLstorkombinasjoner),rep(1,NCOL(z))) , drop=FALSE]

    ncolw = NCOL(w)

    names(storkombinasjoner)[seq_len(ncolw)] = names(w)
    names(storkombinasjoner)[(nCOLstorkombinasjoner+ncolw +seq_len(NCOL(z)))  ]  = names(z)
    names(storkombinasjoner)[ncolw+seq_len(nCOLstorkombinasjoner)] = namesStorkombinasjoner


    reprow = round(NROW(w)/NROW(storkombinasjoner))

    if( !(reprow==1 | reprow==2))
      stop("Noe er galt")

    cat(":")
    flush.console()

    if(reprow==2){
      storkombinasjoner = rbind(storkombinasjoner,storkombinasjoner)

    }
    cat(".")
    flush.console()



    storkombinasjoner[,seq_len(ncolw)] = w
    rm("w")
    cat(".")
    flush.console()


    storkombinasjoner[, (nCOLstorkombinasjoner+ncolw +seq_len(NCOL(z)))  ]  = z
    rm("z")

    cat(".")
    flush.console()



    cat("]\n\n")
    flush.console()


    if(!smartB & !is.null(regnskapsomfanger))
      storkombinasjoner = storkombinasjoner[storkombinasjoner$regnskapsomfang==regnskapsomfanger , ,drop=FALSE]

    rownames(storkombinasjoner) = NULL

    if(!is.null(formler)){
      fk = FormelKorreksjoner(formler$formel,storkombinasjoner,data, funksjonshierarki, artshierarki, data_saer,
                              artshierarki_nettinger, artshierarki_nettinger_kasse,regioner, returnFormelData, rader0warning, printData)
      if(returnFormelData)
        return(fk)
      for(i in seq_len(length(fk$nyedata)))
        storkombinasjoner[fk$rader[[i]], ] = fk$nyedata[[i]]
    }



    if(printData)
      PrintHeadTail(storkombinasjoner, title = "output")

    if(forceStringsAsFactors)
      return(ForceFactorDataFrame(storkombinasjoner))

    return(storkombinasjoner)

  }

  cat("]\n\n")
  flush.console()


  if(!smartB & !is.null(regnskapsomfanger))
    w = w[w$regnskapsomfang==regnskapsomfanger , ,drop=FALSE]


  if(!is.null(formler)){
    fk = FormelKorreksjoner(formler$formel,w,data, funksjonshierarki, artshierarki, data_saer,
                            artshierarki_nettinger,artshierarki_nettinger_kasse,regioner, returnFormelData, rader0warning, printData)
    if(returnFormelData)
      return(fk)
    for(i in seq_len(length(fk$nyedata)))
      w[fk$rader[[i]], ] = fk$nyedata[[i]]
  }



  rownames(w) = NULL

  if(printData)
    PrintHeadTail(w, title = "output")

  #list(constantsInOutput,regnskapsomfang,colDataSelected,xCrossCode,z)
  if(forceStringsAsFactors)
    return(ForceFactorDataFrame(w))

  w
}




#' KostraRegnskap
#'
#' Intern funksjon som kan bruke parametere i HierarchyCompute
#'
#' @encoding UTF8
#'
#' @param data data
#' @param funksjonshierarki funksjonshierarki
#' @param artshierarki artshierarki
#' @param kombinasjoner kombinasjoner
#' @param regioner regioner
#'
#' @return output fra HierarchyCompute
#' @export
#' @keywords internal
#'
KostraRegnskap1 = function(data,funksjonshierarki,artshierarki,kombinasjoner=NULL,regioner=NULL,inputInOutput=NULL, ...){

  periode = unique(c(as.character(data$periode),
                     as.character(funksjonshierarki$periode),
                     as.character(artshierarki$periode)))
  if(length(periode) != 1)
    stop("Ikke unik periode")

  #hierarkier = list(funksjon=funksjonshierarki, art = artshierarki, kontoklasse = "rowFactor", region = "colFactor")
  #inputInOutput=c(TRUE,FALSE)

  # Bedre rekkefølge for reductionKhatriRao
  hierarkier = list(kontoklasse = "rowFactor", art = artshierarki, funksjon=funksjonshierarki, region = "colFactor")

  if(is.null(inputInOutput))
    inputInOutput=c(TRUE,FALSE,TRUE,FALSE)


  HierarchyCompute(data=data,
                   hierarchies=hierarkier,
                   valueVar = "belop",
                   rowSelect = kombinasjoner,
                   colSelect = regioner,
                   autoLevel = TRUE,
                   unionComplement=FALSE,
                   constantsInOutput=data.frame(periode=periode,stringsAsFactors=FALSE),
                   hierarchyVarNames=c(mapsFrom="from", mapsTo ="to", sign="sign", level="level"),
                   inputInOutput=inputInOutput,
                   ...)
}

