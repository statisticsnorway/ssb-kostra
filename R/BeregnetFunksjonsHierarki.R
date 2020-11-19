BeregnetFunksjonsHierarki = function(data,funksjonshierarki,artshierarki,
                                  data_saer,artshierarki_nettinger,artshierarki_nettinger_kasse,
                                  periode,
                                  regnskapsomfang = "A",
                                  kombinasjoner = NULL,
                                  kontoklasser="D",
                                  arter="AGD66",
                                  funksjoner="FG2",regioner=c("0101", "0300","0301", "030101"),
                                  form=NULL,
                                  toKoder = TRUE, doStack = TRUE,
                                  fixHierarchy = TRUE){

  # Funskjonen gjøres slik at kombinasjoner er "normal" input
  # Lager kombinasjoner om det ikke finnes
  if(is.null(kombinasjoner)){
    kontoklasser = data.frame(kontoklasse = kontoklasser, stringsAsFactors = FALSE)
    funksjoner = data.frame(funksjon = funksjoner, stringsAsFactors = FALSE)
    arter = data.frame(art = arter, stringsAsFactors = FALSE)
    kombinasjoner = CrossCodeFrames(arter,funksjoner)
    kombinasjoner = CrossCodeFrames(kombinasjoner,kontoklasser)
  }


  kontoklasser = unique(kombinasjoner$kontoklasse)
  funksjoner = unique(kombinasjoner$funksjon)
  arter = unique(kombinasjoner$art)

  if(length(funksjoner)!=1)
    stop("Kun en funksjon mulig")


  if(length(kontoklasser)!=1)
    stop("Kun en kontoklasser mulig")


  if(length(arter)!=1)
    stop("Kun en arter mulig")


  if(length(regnskapsomfang)!=1)
    stop("Kun en regnskapsomfang mulig")


  dataKS   <- KostraRegnskap(data,funksjonshierarki,artshierarki,
                                data_saer,artshierarki_nettinger,artshierarki_nettinger_kasse,
                                kontoklasser=kontoklasser,
                                arter=arter,funksjoner = unique(c(unique(data$funksjon),unique(data_saer$funksjon))),
                                regioner= regioner,
                                regnskapsomfang = regnskapsomfang,
                                printData = FALSE, autoNetting = FALSE, lag0300 = FALSE, fixRegionkode = FALSE, fixArtkode = FALSE, fixFunksjonkode = FALSE)



  funksjonshierarki = HierarchyFix(funksjonshierarki,c(mapsFrom="from", mapsTo ="to", sign="sign", level="level"), autoLevel = FALSE)

  funksjonshierarki$hierarki = "funksjon"
  funksjonshierarki$funksjon_to = funksjonshierarki$mapsTo

  hihi = funksjonshierarki[rep(1,length(funksjoner)) , , drop=FALSE ]

  hihi$hierarki = "OUTPUT"
  hihi$funksjon_to = funksjoner

  #hihi$mapsTo = paste("OUTPUT_",funksjoner,sep="")
  hihi$mapsFrom   = funksjoner
  hihi$sign = 1L

  if(regnskapsomfang=="A"){
    hihi$mapsTo = paste("KONSERN_",funksjoner,sep="")
  }
  else if(regnskapsomfang=="B"){
    hihi$mapsTo = paste("KASSE_",funksjoner,sep="")
  }

  hiRelevant = RelevantHierarki(rbind(funksjonshierarki,hihi),hihi$mapsTo)


  #hiRelevant = RelevantHierarki(rbind(funksjonshierarki,hihi),paste("OUTPUT",funksjoner,sep="_"))


  w=HierarchyCompute(dataKS, list(funksjon = hiRelevant, region = "colFactor"), "belop",
                     rowSelect = data.frame(funksjon = hiRelevant$mapsFrom),
                     output = "outputMatrixWithCrossCode", colNotInDataWarning=FALSE)


  ma = match(hiRelevant$mapsFrom,w[[2]]$funksjon)

  hierarki_from = rep("Input",NROW(hiRelevant))   # Alle vil bli overskrevet seinere


  hierarki_from_ind = match(hiRelevant$mapsFrom,hiRelevant$mapsTo)

  hierarki_from[!is.na(hierarki_from_ind)] =  hiRelevant$hierarki[hierarki_from_ind[!is.na(hierarki_from_ind)]]


  #return(list(list(periode=periode,hierarki_from=hierarki_from,hiRelevant,stringsAsFactors=FALSE ),as.matrix(w[[1]][ma, ,drop=FALSE])))
  z = cbind(cbind(periode=periode,hierarki_from=hierarki_from,hiRelevant,stringsAsFactors=FALSE ))

  z = cbind(kontoklasse=kontoklasser,art=arter, z,stringsAsFactors=FALSE)


  #### LAger output med samme variabler som formelhierarki..
  names(z)[names(z)=="mapsFrom"] = "from"
  names(z)[names(z)=="mapsTo"] = "to"

  names(z)[names(z)=="hierarki"] = "type_to"
  names(z)[names(z)=="hierarki_from"] = "type_from"

  z$kontoklasse_from = z$kontoklasse
  names(z)[names(z)=="kontoklasse"] = "kontoklasse_to"


  z$art_from = z$art
  names(z)[names(z)=="art"] = "art_to"

  #z$funksjon_to = z$to
  z$funksjon_from = z$from



  z$sign[z$sign=="1"] = "+"
  z$sign[z$sign=="-1"] = "-"

  z = cbind(z,as.matrix(w[[1]][ma, ,drop=FALSE]) ,stringsAsFactors=FALSE)

  z = IntegerDataFrame(z)
  rownames(z) = NULL

  # Rekkefølgen på kolonnene fra venstre til høyre: Periode, Region, Kontoklasse, Funksjon, Art_to, Art_from, Sign, Type, Beløp
  # - Du kan fjerne kolonnen med level
  # - Kolonnen to bør hete "art_to" og kolonnen from bør hete "art_from"
  # - Kolonnen hierarki bør hete "Type" på samme måte som i den andre filen

  #return(list(z=z,hiRelevant=hiRelevant,dataKS=dataKS))
  #return(z)
  doStack = TRUE
  if(doStack){   # level er ikke med
    z <- Stack(z,stackVar=13:NCOL(z),blockVar = 1:12,  valueName = "belop",indName = "region") [,c("periode", "region",
                                                                                                   "kontoklasse_to", "kontoklasse_from",
                                                                                                   "funksjon_to", "funksjon_from",
                                                                                                   "art_to", "art_from",
                                                                                                   "to", "from",
                                                                                                   "sign",
                                                                                                   "type_to", "type_from",
                                                                                                   "belop")]

  }
  z$region <- FixRegionkode(z$region, warningText = "Uventet problem med regionkoder etter Stack ved beregningstester")
  z

}
