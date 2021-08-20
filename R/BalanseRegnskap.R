#' @rdname BalanseRegnskap
#' @note BalanseRegnskapBeregningInput og BalanseRegnskapBeregningHierarki går via funksjonen BalanseRegnskapBeregning.
#' Parametere til den funksjonen trengs bortsett fra «output».
#' Spesielt må altså parameterne kapittelhierarki og kapitler med.
#' Det skal (i utgangspunktet) være en verdi på hver av disse.
#' Funksjonen BalanseRegnskapBeregningHierarkiALT er alternativ til BalanseRegnskapBeregningHierarki med andre variabler i output.
#' @export
BalanseRegnskapBeregning = function(data,kapittelhierarki,
                                   kapitler,
                                   ...,
                                   perioder = NULL, output){

  BalanseRegnskap(data=data,kapittelhierarki=kapittelhierarki, kapitler=kapitler, ..., perioder=perioder,
                 output = output)
}


CharacterReCodeRegnskapsomfang = function(x){
  x$regnskapsomfang = CharacterReCode(x$regnskapsomfang, c("A","B"), c("konsern", "kasse"))
  x
}


#' @rdname BalanseRegnskap
#' @export
BalanseRegnskapBeregningInput = function(...){
  CharacterReCodeRegnskapsomfang(BalanseRegnskapBeregning(output = "beregningInput", ...))
}

#' @rdname BalanseRegnskap
#' @export
BalanseRegnskapBeregningHierarkiALT = function(...){
  BalanseRegnskapBeregning(output = "beregningHierarki", ...)
}



#' @rdname BalanseRegnskap
#' @export
BalanseRegnskapBeregningHierarki = function(...){
  CharacterReCodeRegnskapsomfang(SelectAndRename(BalanseRegnskapBeregning(output = "beregningHierarki", ...),
                  oldNames= c("periode", "region", "kapittel_from", "regnskapsomfang_from", "to", "from", "sign", "belop"),
                  newNames= c("periode", "region", "kapittel",      "regnskapsomfang",      "to", "from", "sign",    "belop")))
}


#' @rdname BalanseRegnskap
#' @export
BalanseRegnskapBeregningHierarkiInput = function(...){
  CharacterReCodeRegnskapsomfang(BalanseRegnskapBeregning(output = "beregningHierarkiInput", ...)$hierarkiInput)
}



#' @rdname BalanseRegnskap
#' @param perioder Perioder for beregningen. Ved NULL (default) brukes alle perioder som finnes i data.
#' @param ... Parametere som sendes videre til BalanseRegnskapEnPeriode
#' @export
BalanseRegnskap = function(data,kapittelhierarki,
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

  SjekkDataPerioder(kapittelhierarki, "kapittelhierarki", perioder)

  n = length(perioder)
  a <- vector("list",n)
  for(i in seq_len(n)){
    if(n>1){
      cat("================================================================\n")
      cat("==============    periode = ",perioder[i],"    ================\n")
      cat("==============================================================\n")
    }
    ai =  BalanseRegnskapEnPeriode(data = DataPeriode(data,perioder[i]),
                                  kapittelhierarki = DataPeriode(kapittelhierarki,perioder[i]),
                                  ...)
    if(n>1) a[[i]] = ai
  }
  if(n>1)
    return(RbindAll(a))
  ai
}



#' BalanseRegnskap
#'
#' Funksjon som ligner på KostraRegnskap, men mye enklere.
#'
#' @rdname BalanseRegnskap
#'
#' @encoding UTF8
#'
#' @param data inputdata
#' @param kapittelhierarki kapittelhierarki
#' @param kombinasjoner kombinasjoner kapitler og regnskapsomfang som skal med i output
#' @param regioner regioner som skal med i output. Kan også gjøre bruk av *, ?, ! og –
#' @param storkombinasjoner  kombinasjoner kapitler,regnskapsomfang og region som skal med i output
#' @param kapitler kapitler som skal med i output
#' @param omfang  regnskapsomfang som skal med i output
#' @param output kode for type output som (brukes av BalanseRegnskapBeregningHierarki og BalanseRegnskapBeregningInput)
#' @param printData Ved TRUE printes to første og to siste rader av alle inputdataene
#' @param lag0300 Ved TRUE kopieres region 0301 til 0300 i inputdata
#' @param fixRegionkode Ved TRUE (default): Sørger for blanke i starten/slutten fjernes og at regionkoder får 4 eller 6 plasser og ledende nuller (gir warning ved endring av input)
#'
#' @return Data frame med samme variabler som data i input og med kapitler/regnskapsomfang i henhold til annen input
#' @export
#' @importFrom SSBtools CharacterDataFrame WildcardGlobbingVector Match RowGroups CrossCodeFrames HierarchyCompute HierarchyFix ForceCharacterDataFrame DummyHierarchy Number matlabColon
#' @importFrom utils flush.console head tail
#' @importFrom stringr str_replace_all str_replace
#'
#' @seealso \code{\link{KostraRegnskap}}, \code{\link{HierarchyCompute}}, \code{\link{HierarchicalWildcardGlobbing}}
#'
#' @examples
#'
#' # Ved FALSE brukes data som ligger i pakken der bare noen regioner er med.
#'
#' lesDataFraS <- FALSE
#' if (!lesDataFraS){
#'   bData <- KostraData("balanseRegnskapDataPen")   # KostraData("balanseRegnskapDataPen") er alternativ der
#'   inputdata <- bData$data                         #    automatisk omkoding trengs (som fixRegionkode)
#'   hierarki <- bData$kapittelhierarki              #    Fungerer like bra, men med flere warning
#' } else {
#'
#'   dataPath <- "S:/Prosjekt/2228 KOSTRA publisering/Metode/Regnskap/R_balanse"
#'   # dataPath <- 'C:\\Users\\oyl\\Documents\\work\\Rworking\\regnskap\\R_balanse'
#'
#'   hierarkiFil <- paste(dataPath, "/Kapittel_hierarki_2016_bal_KLASS", ".csv", sep = "")
#'   dataFil <- paste(dataPath, "/Inputdata_balanse_2016", ".csv", sep = "")
#'   hierarki <- read.csv2(hierarkiFil, colClasses = c("character"))
#'   inputdata <- read.csv2(dataFil, colClasses = c("integer", "character", "character", "character", "character", "integer"))
#' }
#'
#' z <- BalanseRegnskap(inputdata, hierarki, regioner = c("2021", "2022", "0301", "1100"))
#' xA <- BalanseRegnskapBeregningInput(inputdata, hierarki, regioner = "0301", omfang = "A", kapitler = "KG3")
#' xB <- BalanseRegnskapBeregningInput(inputdata, hierarki, regioner = "0301", omfang = "B", kapitler = "KG3")
#' yA <- BalanseRegnskapBeregningHierarki(inputdata, hierarki, regioner = "0301", omfang = "A", kapitler = "KG3")
#' yB <- BalanseRegnskapBeregningHierarki(inputdata, hierarki, regioner = "0301", omfang = "B", kapitler = "KG3")
BalanseRegnskapEnPeriode = function(data,kapittelhierarki,
                          kombinasjoner=NULL,
                          regioner=NULL,
                          storkombinasjoner=NULL,
                          kapitler  = NULL,
                          omfang = NULL,
                          output="en",
                          printData = TRUE,
                          lag0300 = FALSE,
                          fixRegionkode = TRUE
){

  StopOrWarning = stop

  if(printData){
    PrintHeadTail(data)
    PrintHeadTail(kapittelhierarki)
    PrintHeadTail(kombinasjoner)
    PrintHeadTail(regioner)
    PrintHeadTail(storkombinasjoner)
    PrintHeadTail(kapitler)
    PrintHeadTail(omfang)
  }


  if(is.null(omfang))
    omfang = "*"


  warningTextRegionskoder = "Regionskoder endret"
  useMatrixToDataFrame =TRUE




  if(!is.null(storkombinasjoner)){
    if(sum(names(storkombinasjoner)=="region")==0){
      if(any(names(storkombinasjoner)=="fylkesregion")){
        names(storkombinasjoner)[names(storkombinasjoner)=="fylkesregion"] <- "region"
        warning("'fylkesregion' omkodet til 'region' i storkombinasjoner")
      }
    }
    if(fixRegionkode)
      storkombinasjoner$region = FixRegionkode(storkombinasjoner$region,warningTextRegionskoder)
  }


  if(fixRegionkode & !is.null(regioner))
    regioner = FixRegionkode(regioner,warningTextRegionskoder)



  if(NROW(data)==0)
    stop("Det er ingen rader i data")

  if(output == "barePrintData")
    return(NULL)


  returnMatrixComponents=FALSE
  beregningInput = FALSE


  if(sum(names(data)=="region")==0){
    if(any(names(data)=="fylkesregion")){
      names(data)[names(data)=="fylkesregion"] <- "region"
      warning("'fylkesregion' omkodet til 'region' i data")
    }
  }
  if(fixRegionkode)
    data$region = FixRegionkode(data$region,warningTextRegionskoder)


  periode = unique(c(as.character(data$periode),
                     as.character(kapittelhierarki$periode)))

  if(length(periode) == 0)
    stop("periode finnes ikke i input")
  if(length(periode) > 1)
    stop(paste("periode er ikke unik:",paste(periode,collapse=", ")))


  varIdata = c("region", "kapittel", "regnskapsomfang", "belop") %in% names(data)
  if(any(!varIdata)){
    stop(paste("Disse variablene mangler i data:",
               paste(c("region", "kapittel", "regnskapsomfang", "belop")[!varIdata],collapse=", ")))
  }


  ####omfangAll  = unique(data$regnskapsomfang)#### Hardkoder isteden  siden beregnigstest kan feile hvis alle ikke er med
  omfangAll  =  c("B","sbedr","lanefond")    # A skal ikke med her.
  omfangshierarki <- kapittelhierarki[rep(1,length(omfangAll)),c("periode","to","from","sign"),drop=FALSE]
  omfangshierarki$to = "A"
  omfangshierarki$from = omfangAll
  omfangshierarki$sign = "+"

  omfangAll = c("A",omfangAll) # Brukes nedenfor


  integerInOutput = TRUE

  if(!is.integer(data$belop))
    integerInOutput = FALSE


  #
  #  Bedre å passe på en gang (eller 10 ganger) for mye dette med factor og character enn en for lite ...
  #
  data[,c("region", "kapittel", "regnskapsomfang")] = CharacterDataFrame(data[,c("region", "kapittel", "regnskapsomfang")])


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


  if(warning0301)
    warning("0301 finnes ikke i input. 0300 blir ikke laget")


  if(!is.null(omfang)){
    omfang = unique(omfang)


    if(any(c(Fgrepl("*",omfang), Fgrepl("?",omfang),Fgrepl("!",omfang),Fgrepl("-",omfang)))){
      omfang = WildcardGlobbingVector(omfangAll,omfang)
    } else {
      omfangIn =  omfang %in% omfangAll
      if(any(!omfangIn)){
        StopOrWarning(paste("Disse regnskapsomfangene finnes ikke i data eller hierarkier:",
                      paste(omfang[!omfangIn],collapse=", ")))
      }
    }

  }
  if(!is.null(kapitler)){
    kapitler = unique(kapitler)
    kapitlerAll = unique(c(kapittelhierarki$to,kapittelhierarki$from,
                             unique(c(unique(data$kapittel)))))

    if(any(c(Fgrepl("*",kapitler), Fgrepl("?",kapitler),Fgrepl("!",kapitler),Fgrepl("-",kapitler)))){
      kapitler = WildcardGlobbingVector(kapitlerAll,kapitler)
    } else {
      kapitlerIn = kapitler  %in% kapitlerAll
      if(any(!kapitlerIn)){
        StopOrWarning(paste("Disse kapittelene finnes ikke i data eller hierarkier:",
                      paste(kapitler[!kapitlerIn],collapse=", ")))
      }
    }

  }



  storkOrder = NULL
  cat("\n [ Kombinasjonsberegninger...")
  flush.console()

  # Reduserer storkombinasjoner dersom regioner,kombinasjoner, omfang , kapitler, kontoklasser finnes
  if(!is.null(storkombinasjoner)) {
    if(!is.null(regioner)){
      rr = storkombinasjoner$region %in% regioner
      storkombinasjoner = storkombinasjoner[rr, ,drop=FALSE]
    }
    if(!is.null(omfang)){
      rr = storkombinasjoner$regnskapsomfang %in% omfang
      storkombinasjoner = storkombinasjoner[rr, ,drop=FALSE]
    }
    if(!is.null(kapitler)){
      rr = storkombinasjoner$kapittel %in% kapitler
      storkombinasjoner = storkombinasjoner[rr, ,drop=FALSE]
    }


    if(!is.null(kombinasjoner)){
      rr = !is.na(Match(storkombinasjoner[,names(kombinasjoner) ,drop=FALSE],kombinasjoner))
      storkombinasjoner = storkombinasjoner[rr, ,drop=FALSE]
    }
    if(NROW(storkombinasjoner)==0)
      stop("Matching av storkombinasjoner med annen input gir 0 rader")
  } else{
    if(!is.null(kombinasjoner)) { # Reduserer kombinasjoner dersom omfang , kapitler, kontoklasser finnes
      if(!is.null(omfang)){
        rr = kombinasjoner$regnskapsomfang %in% omfang
        kombinasjoner = kombinasjoner[rr, ,drop=FALSE]
      }
      if(!is.null(kapitler)){
        rr = kombinasjoner$kapittel %in% kapitler
        kombinasjoner = kombinasjoner[rr, ,drop=FALSE]
      }

      if(NROW(kombinasjoner)==0)
        stop("Matching av kombinasjoner med annen input gir 0 rader")
    }
  }


  if(!is.null(storkombinasjoner)){
    if(NROW(storkombinasjoner)==0)
      stop(" 0 storkombinasjoner.")

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
    rm(storkombinasjoner) # trengs ikke mer

  }


  if(is.null(kombinasjoner) ){
    cat("] [ Lager kombinasjoner ...")
    flush.console()


    if(is.null(kapitler))
      kapitler = unique(c(kapittelhierarki$to,kapittelhierarki$from,
                            unique(c(unique(data$kapittel)))))
    if(is.null(omfang))
      omfang = unique(c(omfangshierarki$to))  # input ike her


    kapitler = data.frame(kapittel = kapitler, stringsAsFactors = FALSE)
    omfang = data.frame(regnskapsomfang = omfang, stringsAsFactors = FALSE)

    kombinasjoner = CrossCodeFrames(omfang,kapitler, useMatrixToDataFrame=useMatrixToDataFrame)

  }

  if(!is.null(kombinasjoner))
    if(NROW(kombinasjoner)==0)
      stop("Noe er galt det er 0 kombinasjoner")

  if(!is.null(regioner))
    if(length(regioner)==0)
      stop("Noe er galt det er 0 regioner")



  cat("]\n")
  flush.console()



  if(output=="beregningInput"){ #if(beregningInput){
    print(kombinasjoner)
    if(NROW(kombinasjoner) %in% 2:4 ){
      kombinasjoner[, "regnskapsomfang"] = "A"
      kombinasjoner = unique(kombinasjoner)
    }
    if(NROW(kombinasjoner) != 1)
      stop("Kun en kombinasjon mulig ved beregningInput")
  }

  if(output=="beregningHierarki" | output== "beregningHierarkiInput"){
    beregnetHierarki = BeregnetKapittelHierarki(data=data,kapittelhierarki=kapittelhierarki,
                 kombinasjoner=kombinasjoner,
                 periode = periode,
                 regioner=regioner)
    if(output=="beregningHierarki")
      return(beregnetHierarki)

    if(TRUE){

    beregnetHierarki =  SelectAndRename(beregnetHierarki,
                    oldNames= c("periode", "region", "kapittel_from", "regnskapsomfang_from", "to", "from", "sign", "belop"),
                    newNames= c("periode", "region", "kapittel",      "regnskapsomfang",      "to", "from", "sign",    "belop"))

    if(length(unique(beregnetHierarki$region)) != 1)
      stop("Må være 1 region når beregningHierarkiInput")
    #  beregnetHierarki  = zBH[[2]][zBH[[2]]$region=="0301", ]
    rgBH =RowGroups(beregnetHierarki[,c("kapittel", "regnskapsomfang")], returnGroups = TRUE, returnGroupsId = TRUE)


    krMC = BalanseRegnskap(data, kapittelhierarki,    # krMC = BalanseRegnskap(a$data, a$kapittelhierarki,
                          kombinasjoner = rgBH$groups,
                          regioner = unique(beregnetHierarki$region),
                          output = "matrixComponents",
                          printData = FALSE, lag0300 = FALSE,
                          fixRegionkode = FALSE)
    hierarkiInput = cbind(periode=periode, region= unique(beregnetHierarki$region),
                          MultiRowHierarchyComputations(krMC, valueName = "belop", indName = "from",
                                                        ind =  beregnetHierarki$from[rgBH$idg]),
                          stringsAsFactors = FALSE)
    return(list(beregnetHierarki=beregnetHierarki,
                hierarkiInput= IntegerDataFrame(hierarkiInput[, c("periode", "region", "kapittel", "regnskapsomfang", "sign", "belop", "from")],
                                                makeWarning = TRUE)))

}
  } # end if(output=="beregningHierarki" | output== "beregningHierarkiInput")

    if(output=="matrixComponents" | output=="beregningInput")
      output1 = "matrixComponents"
    else
      output1 = "data.frame"


    hierarkier = list(kapittel = kapittelhierarki, regnskapsomfang=omfangshierarki, region = "colFactor")


    w = HierarchyCompute(data=data,
                     hierarchies=hierarkier,
                     valueVar = "belop",
                     rowSelect = kombinasjoner,
                     colSelect = regioner,
                     autoLevel = TRUE,
                     unionComplement=FALSE,
                     constantsInOutput=data.frame(periode=periode,stringsAsFactors=FALSE),
                     hierarchyVarNames=c(mapsFrom="from", mapsTo ="to", sign="sign", level="level"),
                     inputInOutput=TRUE,
                     output=output1)

    if(output=="matrixComponents")
      return(w)

    if(output=="beregningInput")
      return(LagBeregningInputBalanse(a1=w, periode=periode))

    if(integerInOutput)
      w$belop = LagInteger(w$belop)

    if(!is.null(storkOrder)){
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


    return(w)


}


LagBeregningInputBalanse = function(a1, periode, doStack = TRUE){
  x = SingleRowHierarchyComputations(a1, doStack = doStack,  valueName = "belop", indName = "region")
  x$region <- FixRegionkode(x$region, warningText = "Uventet problem med regionkoder etter Stack ved beregningstester")
  cbind(periode=periode, IntegerDataFrame(x, makeWarning = TRUE), stringsAsFactors = FALSE)[,c("periode", "region", "kapittel", "regnskapsomfang",  "sign", "belop")]
}


LagBeregningInputBalanseOld = function(a1, periode, doStack = TRUE){
  print("Gammel funksjon")
  fromCrossCode = a1$fromCrossCode

  dataDummyHierarchy =
    t(as.matrix(a1$dataDummyHierarchy))

  valueMatrix =
    as.matrix(a1$valueMatrix)

  nonZero = rowSums(abs(valueMatrix))>0  & abs(dataDummyHierarchy)>0

  z = cbind(fromCrossCode, sign = dataDummyHierarchy, valueMatrix)

  #output1 = as.matrix(a1$dataDummyHierarchy%*%a1$valueMatrix)
  output1 = as.matrix(Mult(a1$dataDummyHierarchy, a1$valueMatrix))

  out1 = cbind(a1$toCrossCode, sign = 0,output1, stringsAsFactors = FALSE)

  x = cbind(periode=periode, IntegerDataFrame(rbind(
    out1, #
    z[nonZero, ,drop=FALSE]),makeWarning = TRUE), stringsAsFactors = FALSE)

  rownames(x) = NULL


  if(doStack){
    x <- Stack(x,stackVar=5:NCOL(x),blockVar = 1:4,  valueName = "belop",indName = "region")[,c("periode", "region", "kapittel", "regnskapsomfang",  "sign", "belop")]
  }
  x$region <- FixRegionkode(x$region, warningText = "Uventet problem med regionkoder etter Stack ved beregningstester")
  x

}



NyNavnHierarki = function(hierarki, pre="A",sep="_"){
  hierarki$kapittel_to = hierarki$mapsTo
  hierarki$kapittel_from = hierarki$mapsFrom
  hierarki$mapsTo = paste(pre, hierarki$mapsTo, sep=sep)
  hierarki$mapsFrom = paste(pre, hierarki$mapsFrom, sep=sep)
  hierarki$regnskapsomfang_to = pre
  hierarki$regnskapsomfang_from = pre
  hierarki$hierarki = pre
  hierarki
}


EttBalanseHierarki = function(hierarki, fixHierarchy = TRUE, omfanger  = c("A","B","sbedr","lanefond")){

  if(fixHierarchy)
  hierarki = HierarchyFix(hierarki,c(mapsFrom="from", mapsTo ="to", sign="sign", level="level"), autoLevel = FALSE)

  hierarkiC = NULL
  hierarkiA = NULL


  for(i in seq_len(length(omfanger)-1)){
    hierarkiC = rbind(hierarkiC ,NyNavnHierarki(hierarki,omfanger[i+1]))
    hierarkiA = rbind(hierarkiA ,NyNavnHierarki(hierarki,omfanger[1]))
  }

  hierarkiA$mapsFrom = hierarkiC$mapsTo
  hierarkiA$kapittel_from = hierarkiC$kapittel_to
  hierarkiA$regnskapsomfang_from = hierarkiC$regnskapsomfang_to
  hierarkiA$sign = 1L
  hierarkiA = unique(hierarkiA)


  hi_Kon_mapsTo = unique(hierarki$mapsTo)
  hihi_Kon = hierarkiA[rep(1,length(hi_Kon_mapsTo)), ,drop=FALSE]
  hihi_Kon$hierarki = "OUTPUT"
  hihi_Kon$regnskapsomfang_from = "A"
  hihi_Kon$regnskapsomfang_to = "A"
  hihi_Kon$kapittel_to = hi_Kon_mapsTo
  hihi_Kon$kapittel_from = hi_Kon_mapsTo
  hihi_Kon$mapsTo = paste("KONSERN_",hi_Kon_mapsTo,sep="")
  hihi_Kon$mapsFrom   = paste("A_",hi_Kon_mapsTo,sep="")
  hihi_Kon$sign = 1L


  rbind(hierarkiC,hierarkiA,hihi_Kon)
}

EttBalanseHierarkiB = function(hierarki, fixHierarchy = TRUE){

  if(fixHierarchy)
    hierarki = HierarchyFix(hierarki,c(mapsFrom="from", mapsTo ="to", sign="sign", level="level"), autoLevel = FALSE)

  hierarkiB = NyNavnHierarki(hierarki,"B")

  hi_Kon_mapsTo = unique(hierarki$mapsTo)
  hihi_Kon = hierarkiB[rep(1,length(hi_Kon_mapsTo)), ,drop=FALSE]
  hihi_Kon$hierarki = "OUTPUT"

  hihi_Kon$kapittel_to = hi_Kon_mapsTo
  hihi_Kon$kapittel_from = hi_Kon_mapsTo
  hihi_Kon$mapsTo = paste("KASSE_",hi_Kon_mapsTo,sep="")
  hihi_Kon$mapsFrom   = paste("B_",hi_Kon_mapsTo,sep="")
  hihi_Kon$sign = 1L

  rbind(hierarkiB,hihi_Kon)
}


BeregnetKapittelHierarki =
  function(data,kapittelhierarki,
           kombinasjoner=NULL,
           kapitler= NULL,
           periode = "2016",
           regioner=NULL)
  {

  if(is.null(kapitler))
    kapitler= unique(kombinasjoner$kapittel)


  regnskapsomfang = unique(kombinasjoner$regnskapsomfang)

  onlyB = FALSE
  if(length(regnskapsomfang)==1)
    if(regnskapsomfang=="B")
      onlyB = TRUE

  onlyB = FALSE #################   Kutter bruk av EttBalanseHierarkiB ....  det virker for "sbedr" og "lanefond" også (filtrerer til slutt)

  if(onlyB)
    hiRelevant = RelevantHierarki(EttBalanseHierarkiB(kapittelhierarki),paste("KASSE",kapitler,sep="_"))
  else
    hiRelevant = RelevantHierarki(EttBalanseHierarki(kapittelhierarki),paste("KONSERN",kapitler,sep="_"))

  if(NROW(hiRelevant)==0)
    stop(paste("Det blir ingenting med utgangspunkt i", paste(HeadEnd(kapitler),collapse=", ")))

  #return(hiRelevant)

  data$kapittel = paste(data$regnskapsomfang,data$kapittel,sep="_")


  w=HierarchyCompute(data, list(kapittel = hiRelevant, region = "colFactor"), "belop",
                     rowSelect = data.frame(kapittel = hiRelevant$mapsFrom),colSelect = regioner,
                     unionComplement = FALSE,
                     output = "outputMatrixWithCrossCode", colNotInDataWarning=FALSE)


  ma = match(hiRelevant$mapsFrom,w[[2]]$kapittel)



  hierarki_from = paste("Input",hiRelevant$hierarki,sep="_")



  hierarki_from_ind = match(hiRelevant$mapsFrom,hiRelevant$mapsTo)

  hierarki_from[!is.na(hierarki_from_ind)] =  hiRelevant$hierarki[hierarki_from_ind[!is.na(hierarki_from_ind)]]


  z = cbind(cbind(periode=periode,hierarki_from=hierarki_from,hiRelevant,stringsAsFactors=FALSE ))



  #### LAger output med samme variabler som formelhierarki..
  names(z)[names(z)=="mapsFrom"] = "from"
  names(z)[names(z)=="mapsTo"] = "to"

  names(z)[names(z)=="hierarki"] = "type_to"
  names(z)[names(z)=="hierarki_from"] = "type_from"



  z$sign[z$sign=="1"] = "+"
  z$sign[z$sign=="-1"] = "-"

  z = cbind(z,as.matrix(w[[1]][ma, ,drop=FALSE]) ,stringsAsFactors=FALSE)

  z = IntegerDataFrame(z)
  rownames(z) = NULL

  doStack = TRUE
  if(doStack){   # level er ikke med
      z <- Stack(z,stackVar=11:NCOL(z),blockVar = 1:10,  valueName = "belop",indName = "region") [,c("periode", "region",
                                                                                                   "kapittel_to", "kapittel_from",
                                                                                                   "regnskapsomfang_to", "regnskapsomfang_from",
                                                                                                   "to", "from",
                                                                                                   "sign",
                                                                                                   "type_to", "type_from",
                                                                                                   "belop")]

  }
  z$region <- FixRegionkode(z$region, warningText = "Uventet problem med regionkoder etter Stack ved beregningstester")

  if("A" %in% regnskapsomfang)
    return(z)

  z[z$regnskapsomfang_from %in% regnskapsomfang, ]
}













