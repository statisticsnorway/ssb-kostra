
FormelMatrise = function(formler){
  x = vector("list",length(formler))
  varLeft =   character(0)  # vector("list",length(formler))
  varRight =   character(0) #  vector("list",length(formler))
  codesLeft = NULL
  codesRight = NULL
  for(i in seq_len(length(formler))){
    x[[i]] = ExprCrossVar(formler[i])
    codes = as.data.frame(x[[i]]$codes)
    if(NCOL(codes)==2)
      codes = cbind("DI",codes)
    colnames(codes) = c("kontoklasse", "funksjon", "art")
    codes = CharacterDataFrame(codes)
    #colnames(codesLeft) = NULL
    #colnames(codesRight) = NULL
    #print(codes)
    #print(codesLeft)
    #print(codesRight)
    codesLeft = rbind(codesLeft,codes[1L, ,drop=FALSE])
    codesRight = rbind(codesRight,codes[-1L, ,drop=FALSE])
    varLeft  = c(varLeft,x[[i]]$varnames[1L])
    varRight = c(varRight,x[[i]]$varnames[-1L])
  }
  dpR = duplicated(varRight)
  varRight = varRight[!dpR]
  codesRight = codesRight[!dpR, ,drop=FALSE]
  dia = as.data.frame(diag(length(varRight)))
  colnames(dia) = varRight
  rownames(dia) = varRight
  formelMatrise = matrix(0,nrow = length(formler), ncol = length(varRight))
  rownames(formelMatrise) = varLeft
  colnames(formelMatrise) = varRight
  for(i in seq_len(length(formler)))
    formelMatrise[i, ] =
       with(dia, eval(parse(text=x[[i]]$express)))
  list(formelMatrise=formelMatrise,x=x,varLeft=varLeft,varRight=varRight,codesLeft=codesLeft,codesRight=codesRight)
}





FlereOutput = function(..., regioner, output = c("en", "beregningInput", "beregningHierarki", "beregningHierarki" ) ){
  n <- length(output)
  a <- vector("list",2*n+1)
  for(i in seq_len(n)){
    cat("===========================================",i,"==============   START =============\n")
    flush.console()
    a[[i]] = KostraRegnskap(..., regioner=regioner, regnskapsomfang= "A", output = output[[i]])
    cat("===========================================",i,"==============   MIDT =============\n")
    a[[n+i]] = KostraRegnskap(..., regioner=regioner[1],regnskapsomfang= "A", output = output[[i]])
    cat("===========================================",i,"==============   SLUTT =============\n")
  }

  #a[[2*n+1]] = as.matrix(a[[2]][1,-(1:6)]) - a[[1]]$belop   # Skal bli null (ikke ved formel foreløpig)

  #a[[2*n+1]] = a[[2]][a[[2]]$type=="OUTPUT_Konsern","belop"] - a[[1]]$belop   # Skal bli null (ikke ved formel foreløpig)

  a[[2*n+1]] = cbind(a[[1]]$belop,a[[2]][a[[2]]$type=="OUTPUT_Konsern","belop"],a[[3]][a[[3]]$type_to=="OUTPUT","belop"])
  colnames(a[[2*n+1]]) = output[1:3]
  differanser = apply(a[[2*n+1]],1,function(x)(max(x) - min(x)))
  if(max(differanser)!=0)
    stop("Ikke like resultater")
  for(i in c(4,8)){
    a[[i]] = SelectAndRename(a[[i]],
                    oldNames= c("periode", "region", "kontoklasse_from", "funksjon_from", "art_from", "to", "from", "sign", "type_from", "belop"),
                    newNames= c("periode", "region", "kontoklasse",      "funksjon",      "art",      "to", "from", "sign", "type",    "belop"))
  }
  a
}


LagBeregningInputB = function(a1, periode, doStack = TRUE){
  a1$fromCrossCode = cbind(a1$fromCrossCode, type = "Kasse"[NROW(a1$fromCrossCode)>0], stringsAsFactors = FALSE)
  a1$toCrossCode = cbind(a1$toCrossCode, type = "OUTPUT_Kasse", stringsAsFactors = FALSE)

  x = SingleRowHierarchyComputations(a1, doStack = doStack,  valueName = "belop", indName = "region")
  x$region <- FixRegionkode(x$region, warningText = "Uventet problem med regionkoder etter Stack ved beregningstester")
  cbind(periode=periode, IntegerDataFrame(x, makeWarning = TRUE), stringsAsFactors = FALSE)[,c("periode", "region", "kontoklasse", "funksjon", "art",  "sign", "type", "belop")]
}



LagBeregningInputBold = function(a1, periode, doStack = TRUE){
  print("Gammel funksjon")
  fromCrossCode =
    cbind(a1$fromCrossCode, type = "Kasse"[NROW(a1$fromCrossCode)>0], stringsAsFactors = FALSE)

  #må fikse = type blir faktor
  dataDummyHierarchy =
    t(as.matrix(a1$dataDummyHierarchy))

  valueMatrix =
    as.matrix(a1$valueMatrix)

  nonZero = rowSums(abs(valueMatrix))>0  & abs(dataDummyHierarchy)>0

  z = cbind(fromCrossCode, sign = dataDummyHierarchy, valueMatrix)

  output1 = as.matrix(a1$dataDummyHierarchy%*%a1$valueMatrix)

  #output = output1 + output2 + output3 + output4

  #out = cbind(a1$toCrossCode,type="OUTPUT_Konsern", sign = 0,output, stringsAsFactors = FALSE)
  out1 = cbind(a1$toCrossCode,type="OUTPUT_Kasse", sign = 0,output1, stringsAsFactors = FALSE)
  #out2 = cbind(a1$toCrossCode,type="OUTPUT_Saer", sign = 0,output2, stringsAsFactors = FALSE)
  #out3 = cbind(a1$toCrossCode,type="OUTPUT_Nett_Saer", sign = 0,output3, stringsAsFactors = FALSE)
  #out4 = cbind(a1$toCrossCode,type="OUTPUT_Nett_Kasse", sign = 0,output4, stringsAsFactors = FALSE)


  #return(list(periode=periode,out1=out1,z=z,nonZero=nonZero))
  x = cbind(periode=periode, IntegerDataFrame(rbind(
    out1, # out, out1, out2, out3, out4,
    z[nonZero, ,drop=FALSE]),makeWarning = TRUE), stringsAsFactors = FALSE)

  rownames(x) = NULL

  if(doStack){
    x <- Stack(x,stackVar=7:NCOL(x),blockVar = 1:6,  valueName = "belop",indName = "region")[,c("periode", "region", "kontoklasse", "funksjon", "art",  "sign", "type", "belop")]
  }
  x$region <- FixRegionkode(x$region, warningText = "Uventet problem med regionkoder etter Stack ved beregningstester")
  x

}



LagBeregningInput = function(a1,a2,a3,a4, periode, doStack = TRUE){
  fromCrossCode = rbind(
    cbind(a1$fromCrossCode, type = "Kasse"[NROW(a1$fromCrossCode)>0], stringsAsFactors = FALSE),
    cbind(a2$fromCrossCode, type = "Saer"[NROW(a2$fromCrossCode)>0], stringsAsFactors = FALSE),
    cbind(a3$fromCrossCode, type = "Nett_Saer"[NROW(a3$fromCrossCode)>0], stringsAsFactors = FALSE),
    cbind(a4$fromCrossCode, type = "Nett_Kasse"[NROW(a4$fromCrossCode)>0], stringsAsFactors = FALSE),
    stringsAsFactors = FALSE)

  #må fikse = type blir faktor
  dataDummyHierarchy = rbind(
    t(as.matrix(a1$dataDummyHierarchy)),
    t(as.matrix(a2$dataDummyHierarchy)),
    t(as.matrix(a3$dataDummyHierarchy)),
    t(as.matrix(a4$dataDummyHierarchy)))

  valueMatrix = rbind(
    as.matrix(a1$valueMatrix),
    as.matrix(a2$valueMatrix),
    as.matrix(a3$valueMatrix),
    as.matrix(a4$valueMatrix))

  nonZero = rowSums(abs(valueMatrix))>0  & abs(dataDummyHierarchy)>0

  z = cbind(fromCrossCode, sign = as.vector(dataDummyHierarchy), valueMatrix)

  output1 = as.matrix(a1$dataDummyHierarchy%*%a1$valueMatrix)
  output2 = as.matrix(a2$dataDummyHierarchy%*%a2$valueMatrix)
  output3 = as.matrix(a3$dataDummyHierarchy%*%a3$valueMatrix)
  output4 = as.matrix(a4$dataDummyHierarchy%*%a4$valueMatrix)

  output = output1 + output2 + output3 + output4

  out = cbind(a1$toCrossCode,type="OUTPUT_Konsern", sign = 0,output, stringsAsFactors = FALSE)
  out1 = cbind(a1$toCrossCode,type="OUTPUT_Kasse", sign = 0,output1, stringsAsFactors = FALSE)
  out2 = cbind(a1$toCrossCode,type="OUTPUT_Saer", sign = 0,output2, stringsAsFactors = FALSE)
  out3 = cbind(a1$toCrossCode,type="OUTPUT_Nett_Saer", sign = 0,output3, stringsAsFactors = FALSE)
  out4 = cbind(a1$toCrossCode,type="OUTPUT_Nett_Kasse", sign = 0,output4, stringsAsFactors = FALSE)


  x = cbind(periode=periode, IntegerDataFrame(rbind(
    out, out1, out2, out3, out4,
    z[nonZero, ,drop=FALSE]),makeWarning = TRUE), stringsAsFactors = FALSE)

  rownames(x) = NULL

  # Kommentarer til "Beregning fra input"-filen:
  #
  #  - Under kolonnen type bør det stå "OUTPUT_Konsern" og ikke bare "OUTPUT"
  # - Vi vil gjerne ha en egen kolonne med Region og ikke alle regionene vi velger i en egen kolonne
  # - Rekkefølgen på kolonnene fra venstre til høyre: Periode, Region, Kontoklasse, Funksjon, Art, Sign, Type, Beløp
  # - Vi vil beholde de øverste OUTPUT-radene.
  # - Kanskje vi kan ha med en egen kolonne med org.numrene til særbedriftene, men dette tar vi eventuelt senere.
  if(doStack){
     x <- Stack(x,stackVar=7:NCOL(x),blockVar = 1:6,  valueName = "belop",indName = "region")[,c("periode", "region", "kontoklasse", "funksjon", "art",  "sign", "type", "belop")]
  }
  x$region <- FixRegionkode(x$region, warningText = "Uventet problem med regionkoder etter Stack ved beregningstester")
  x

}



FormelHierarki = function(formelMatrise,varLeft,varRight,codesLeft,codesRight){
  hd = HierarchyFromDummy(formelMatrise)
  hd$level = 1
  hdLik = hd[,1] == hd[,2]
  if(sum(!hdLik))
  hd[!hdLik, ] =HierarchyFix(hd[!hdLik, ,drop=FALSE])
  rownames(hd) = NULL
  hd
}

#fm =FormelMatrise(formler$formel)
#FormelHierarki(fm$formelMatrise,fm$varLeft,fm$varRight,fm$codesLeft,fm$codesRight)

RelevantHierarki = function(hierarki,koder=NULL){
  hierarki[RelevantHierarkiRows(hierarki,koder=koder), ,drop=FALSE]
}



RelevantHierarkiRows = function(hierarki,koder=NULL,rows=NULL){
  if(is.null(rows))
    rows = hierarki$mapsTo %in% koder

  nyeRows = hierarki$mapsTo %in% unique(hierarki$mapsFrom[rows])
  nyeRows[rows] = FALSE
  alleRows = rows
  alleRows[nyeRows] = TRUE
  if(sum(nyeRows))
    return(RelevantHierarkiRows(hierarki,rows=alleRows))
  alleRows
}


NyNavnNetting = function(hierarki,pre = "Nett_"){
  rows = (hierarki$mapsFrom %in% unique(hierarki$mapsTo))
  hierarki$mapsTo = paste(pre, hierarki$mapsTo, sep="")
  hierarki$mapsFrom[rows] = paste(pre, hierarki$mapsFrom[rows], sep="")
  hierarki
}

EttHierarki = function(artshierarki, artshierarki_nettinger, artshierarki_nettinger_kasse, fixHierarchy = TRUE){
  if(fixHierarchy){
    artshierarki_nettinger_kasse$from = AddLeadingZeros(artshierarki_nettinger_kasse$from,3)
    artshierarki_nettinger$from = AddLeadingZeros(artshierarki_nettinger$from,3)
    artshierarki$from = AddLeadingZeros(artshierarki$from,3)

    artshierarki_nettinger = AutoNettingCopy(artshierarki_nettinger,artshierarki)
    artshierarki_nettinger_kasse = AutoNettingCopy(artshierarki_nettinger_kasse,artshierarki)
  }
  artshierarki = HierarchyFix(artshierarki,c(mapsFrom="from", mapsTo ="to", sign="sign", level="level"), autoLevel = FALSE)
  artshierarki_nettinger  = HierarchyFix(artshierarki_nettinger ,c(mapsFrom="from", mapsTo ="to", sign="sign", level="level"), autoLevel = FALSE)
  artshierarki_nettinger_kasse  = HierarchyFix(artshierarki_nettinger_kasse ,c(mapsFrom="from", mapsTo ="to", sign="sign", level="level"), autoLevel = FALSE)

  hi = artshierarki
  hi_nett = NyNavnNetting(artshierarki_nettinger,"Nett_")
  hi_nett_kasse = NyNavnNetting(artshierarki_nettinger_kasse,"Nett_")

  hi$hierarki = "Saer"
  hi_nett$hierarki = "Nett_Saer"

  hiSaer = rbind(hi,hi_nett)


  hiSaer$mapsTo = paste("Saer_", hiSaer$mapsTo, sep="")
  hiSaer$mapsFrom = paste("Saer_", hiSaer$mapsFrom, sep="")

  #hiKasse = HierarchyFix(rbind(hi,hi_nett_kasse))

  hi$hierarki = "Kasse"
  hi_nett_kasse$hierarki = "Nett_Kasse"

  hiKasse = rbind(hi,hi_nett_kasse)




  #RelevantHierarki(hiSaer,"Nett_AGD70")

  # Brukte mapsTo som varnavn siden det var starten, men endret for å få med enkeltarter også men ikke på netting


  #hi_mapsTo = unique(hi$mapsTo)
  hi_mapsTo = unique(c(hi$mapsFrom, hi$mapsTo))
  hi_n_mapsTo = unique(artshierarki_nettinger$mapsTo)
  hi_n_k_mapsTo = unique(artshierarki_nettinger_kasse$mapsTo)



  #hi_Kon_mapsTo = unique(c(hi$mapsTo,artshierarki_nettinger$mapsTo,artshierarki_nettinger_kasse$mapsTo))

  hi_Kon_mapsTo = unique(c(hi_mapsTo, hi_n_mapsTo, hi_n_k_mapsTo))


  hihi = hiSaer[rep(1,length(hi_mapsTo)), ,drop=FALSE]


  hihi_s = hihi
  hihi_n = hiSaer[rep(1,length(hi_n_mapsTo)), ,drop=FALSE]
  hihi_n_k = hiSaer[rep(1,length(hi_n_k_mapsTo)), ,drop=FALSE]
  hihi_Kon = hiSaer[rep(1,length(hi_Kon_mapsTo)), ,drop=FALSE]

  hihi$hierarki = "Konsern"
  hihi_s$hierarki = "Konsern"
  hihi_n$hierarki = "Konsern"
  hihi_n_k$hierarki = "Konsern"
  hihi_Kon$hierarki = "OUTPUT"

  hihi$mapsTo = paste("Konsern_",hi_mapsTo,sep="")
  hihi$mapsFrom = hi_mapsTo
  hihi$sign = 1L

  hihi_s$mapsTo     = paste("Konsern_",hi_mapsTo,sep="")
  hihi_s$mapsFrom   = paste("Saer_",hi_mapsTo,sep="")
  hihi_s$sign = 1L

  hihi_n$mapsTo     = paste("Konsern_",hi_n_mapsTo,sep="")
  hihi_n$mapsFrom   = paste("Saer_Nett_",hi_n_mapsTo,sep="")
  hihi_n$sign = 1L

  hihi_n_k$mapsTo = paste("Konsern_",hi_n_k_mapsTo,sep="")
  hihi_n_k$mapsFrom   = paste("Nett_",hi_n_k_mapsTo,sep="")
  hihi_n_k$sign = 1L


  hihi_Kon$mapsTo = paste("KONSERN_",hi_Kon_mapsTo,sep="")
  hihi_Kon$mapsFrom   = paste("Konsern_",hi_Kon_mapsTo,sep="")
  hihi_Kon$sign = 1L

  hiKasse$saer  = FALSE
  hiSaer$saer   = TRUE
  hihi$saer     = FALSE
  hihi_s$saer   = TRUE
  hihi_n$saer   = TRUE
  hihi_n_k$saer = FALSE
  hihi_Kon$saer = FALSE

  hiKonsern1 = rbind(hiKasse,hiSaer,hihi,hihi_s,hihi_n,hihi_n_k,hihi_Kon)
  hiKonsern = HierarchyFix(hiKonsern1)
  hiKonsern$hierarki  =hiKonsern1$hierarki
  hiKonsern$saer  =hiKonsern1$saer
  hiKonsern
}


EttHierarkiB = function(artshierarki, fixHierarchy = TRUE){
  if(fixHierarchy){
    artshierarki$from = AddLeadingZeros(artshierarki$from,3)
  }
  hi = HierarchyFix(artshierarki,c(mapsFrom="from", mapsTo ="to", sign="sign", level="level"), autoLevel = FALSE)

  hi_mapsTo = unique(c(hi$mapsFrom, hi$mapsTo))

  hihi = hi[rep(1,length(hi_mapsTo)), ,drop=FALSE]

  hi$hierarki = "Kasse"
  hihi$hierarki = "OUTPUT"

  hihi$mapsTo = paste("KASSE_",hi_mapsTo,sep="")
  hihi$mapsFrom   = hi_mapsTo
  hihi$sign = 1L

  hiKonsern1 = rbind(hi,hihi)
  hiKonsern = HierarchyFix(hiKonsern1)
  hiKonsern$hierarki  = hiKonsern1$hierarki
  hiKonsern$saer = FALSE
  hiKonsern
}











#RelevantHierarki(FormelHierarki(fm$formelMatrise,fm$varLeft,fm$varRight,fm$codesLeft,fm$codesRight),"w_D_wWMWw_FG2_wWMWw_AGD66")

# hf = BeregnetFormelHierarki(inputdata,funksjonshierarki,artshierarki,inputdata_saer,artshierarki_nettinger,artshierarki_nettinger_kasse)


BeregnetFormelHierarki = function(data,funksjonshierarki,artshierarki,
                                  data_saer,artshierarki_nettinger,artshierarki_nettinger_kasse,
                                  periode,
                                  artshierarki_konsern=NULL,
                                  kombinasjoner = NULL,
                                  kontoklasser="D",
                                  arter="AGD66",
                                  funksjoner="FG2",regioner=c("0101", "0300","0301", "030101"),
                                  form=NULL,
                                  toKoder = TRUE, doStack = TRUE,
                                  fixHierarchy = TRUE,
                                  onlyB = FALSE){
  if(is.null(artshierarki_konsern)){
    if(onlyB)
      artshierarki_konsern = EttHierarkiB(artshierarki, fixHierarchy = fixHierarchy)
    else
      artshierarki_konsern = EttHierarki(artshierarki, artshierarki_nettinger, artshierarki_nettinger_kasse,  fixHierarchy = fixHierarchy)
  }


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

  ########


  #kode = paste(kontoklasser,funksjoner,arter,sep=":")
  kode = apply(kombinasjoner[,c(3,2,1)],1,function(x) paste(x,collapse=":"))


  if(!is.null(form)){
    form = AutoFormel(form, fra2til3 = TRUE, laAvhenge=TRUE)   # Overflødig når kjørt før ...
    formelKode = kode %in% VenstreKoder(form) # formelKode = !is.null(kode)
  } else
  formelKode = FALSE

  if(any(formelKode)){
    if(any(!formelKode)){
      stop("Enten må alle kombinasjoner defineres av formel eller ingen")
    }
    formelKode = TRUE
  } else {
    if(length(funksjoner)!=1)
      stop("Kun en funksjon mulig ved ikke-formel")
    if(length(kontoklasser)!=1)
      stop("Kun en kontoklasse mulig ved ikke-formel")
    formelKode = FALSE
  }


  #unlist(lapply(str_split(fo,"="),function(x) x[1]))

  fm=NULL
  if(formelKode){
    fm = FormelMatrise(form)
    fm
    fh = FormelHierarki(fm$formelMatrise,fm$varLeft,fm$varRight,fm$codesLeft,fm$codesRight) ## Annen inpuy brukes ikke ... ennå
    fh$mapsFrom = gsub("_wWMWw_",":",fh$mapsFrom)
    fh$mapsFrom = gsub("w_","",fh$mapsFrom)
    fh$mapsTo = gsub("_wWMWw_",":",fh$mapsTo)
    fh$mapsTo = gsub("w_","",fh$mapsTo)
    fh = RelevantHierarki(fh,kode)
    fh


    innRows = !(fh$mapsFrom %in% fh$mapsTo)
    innRows[fh$mapsFrom==fh$mapsTo] = TRUE

    innKoder = fh$mapsFrom[innRows]

    innKombinasjoner = CharacterDataFrame(as.data.frame(str_split(innKoder,":",simplify=TRUE)))

    kontoklasser = unique(innKombinasjoner[,1])
    funksjoner = unique(innKombinasjoner[,2])
    colnames(innKombinasjoner) = c("kontoklasse", "funksjon", "art")
  }

  dataKS   <- KostraRegnskap(data,funksjonshierarki,artshierarki,
                                               data_saer,artshierarki_nettinger,artshierarki_nettinger_kasse,
                                               kontoklasser=kontoklasser,
                                               arter=unique(data$art),funksjoner=funksjoner,
                                               regioner= regioner,
                                               regnskapsomfang="B",
                                printData = FALSE, autoNetting = FALSE, lag0300 = FALSE, fixRegionkode = FALSE, fixArtkode = FALSE, fixFunksjonkode = FALSE)

  if(!onlyB){
  # suppressWarnings pga colNotInDataWarning
  dataSaer   <- suppressWarnings(KostraRegnskap(data_saer,funksjonshierarki,artshierarki,
                                              data_saer,artshierarki_nettinger,artshierarki_nettinger_kasse,
                                              kontoklasser=kontoklasser,
                                              arter=unique(data_saer$art),funksjoner=funksjoner,
                                              regioner= regioner,
                                              regnskapsomfang="B",
                               printData = FALSE, autoNetting = FALSE, lag0300 = FALSE, fixRegionkode = FALSE, fixArtkode = FALSE, fixFunksjonkode = FALSE))



  dataSaer$art = paste("Saer_",dataSaer$art,sep="")
  dataKS = RbindAll(dataKS,dataSaer)
  } # end if(!onlyB)

  if(formelKode){
    rg = RowGroups(innKombinasjoner[, 1:2,drop=FALSE],TRUE)
    n = max(rg$idx)
    hiRelevant = vector("list",n)
    for(i in seq_len(n)){
      if(onlyB)
        hiRelevant[[i]] = RelevantHierarki(artshierarki_konsern,paste("KASSE", unique(innKombinasjoner$art[rg$idx==i]),sep="_"))
      else
        hiRelevant[[i]] = RelevantHierarki(artshierarki_konsern,paste("KONSERN", unique(innKombinasjoner$art[rg$idx==i]),sep="_"))
      hiRelevant[[i]]$art = hiRelevant[[i]]$mapsFrom
      hiRelevant[[i]]$kontoklasse = rg$groups$kontoklasse[i]
      hiRelevant[[i]]$funksjon = rg$groups$funksjon[i]
      hiRelevant[[i]]$mapsFrom = paste(rg$groups$kontoklasse[i],rg$groups$funksjon[i],hiRelevant[[i]]$mapsFrom,sep=".")
      hiRelevant[[i]]$mapsTo = paste(rg$groups$kontoklasse[i],rg$groups$funksjon[i],hiRelevant[[i]]$mapsTo,sep=".")
    }
    fh1 = fh[rep(1,length(kode)), ,drop=FALSE]
    fh1$mapsFrom = kode
    fh1$mapsTo = kode
    fh1$sign="1"
    fha = rbind(fh,fh1)
    kombinasjoner = CharacterDataFrame(as.data.frame(str_split(fha$mapsFrom,":",simplify=TRUE)))
    colnames(kombinasjoner) = c("kontoklasse", "funksjon", "art")
    fhk = cbind(fha,kombinasjoner)
    #return(fhk)
    if(onlyB)
        fhk$mapsFrom[c(innRows,rep(FALSE,length(kode)))] = paste(fhk$kontoklasse,".",fhk$funksjon,".",fhk$art,sep="")[c(innRows,rep(FALSE,length(kode)))]
      else
        fhk$mapsFrom[c(innRows,rep(FALSE,length(kode)))] = paste(fhk$kontoklasse,".",fhk$funksjon,".Konsern_",fhk$art,sep="")[c(innRows,rep(FALSE,length(kode)))]
    fhk$hierarki = "formel"
    rowsKodeOUTPUT = (NROW(fh)+1):NROW(fhk)
    fhk$hierarki[rowsKodeOUTPUT]="OUTPUT"
    if(onlyB)
      kodeOUTPUT = paste("KASSE",fhk$mapsTo[rowsKodeOUTPUT],sep="_")
    else
      kodeOUTPUT = paste("KONSERN",fhk$mapsTo[rowsKodeOUTPUT],sep="_")
    fhk$mapsTo[rowsKodeOUTPUT] = kodeOUTPUT
    hiRelevant = RbindAll(c(hiRelevant,list(fhk)))
    hiRelevant = RelevantHierarki(hiRelevant,kodeOUTPUT)
    hiRelevant$level = HierarchyFix2(hiRelevant)$level
    dataKS$art = paste(dataKS$kontoklasse,dataKS$funksjon,dataKS$art,sep=".")
  }
  else {
    if(onlyB)
      hiRelevant = RelevantHierarki(artshierarki_konsern,paste("KASSE",arter,sep="_"))
    else
      hiRelevant = RelevantHierarki(artshierarki_konsern,paste("KONSERN",arter,sep="_"))
  }


  #dataKS$art = paste(dataKS$kontoklasse,dataKS$funksjon,dataKS$art,sep=".")

  if(NROW(hiRelevant)==0)
    stop(paste("Det blir ingenting med utgangspunkt i",
               paste(HeadEnd(kontoklasser),collapse=", "), "-",
               paste(HeadEnd(funksjoner),collapse=", "),"-",
               paste(HeadEnd(arter),collapse=", ")
               ))




  w=HierarchyCompute(dataKS, list(art = hiRelevant, region = "colFactor"), "belop",
                     rowSelect = data.frame(art = hiRelevant$mapsFrom),
                     output = "outputMatrixWithCrossCode", colNotInDataWarning=FALSE)

  #return(cbind(w[[2]],as.matrix(w[[1]])))



  ma = match(hiRelevant$mapsFrom,w[[2]]$art)

  #hierarki_from = rep("Input",NROW(hiRelevant))   # Alle vil bli overskrevet seinere

  #hierarki_from[hiRelevant$hierarki == "Nett_Kasse"] = "Input_Kasse"
  #hierarki_from[hiRelevant$hierarki == "Kasse"]      = "Input_Kasse"
  #hierarki_from[hiRelevant$hierarki == "Nett_Saer"]  = "Input_Saer"
  #hierarki_from[hiRelevant$hierarki == "Saer"]       = "Input_Saer"

  hierarki_from = rep("Input_Kasse",NROW(hiRelevant))   # Mange vil bli overskrevet seinere ... ikke bare linje under
  hierarki_from[hiRelevant$saer]  = "Input_Saer"




  #print(hiRelevant[hierarki_from=="Input", ])
  #hir <<- hiRelevant[hierarki_from=="Input", ]


  hierarki_from_ind = match(hiRelevant$mapsFrom,hiRelevant$mapsTo)

  hierarki_from[!is.na(hierarki_from_ind)] =  hiRelevant$hierarki[hierarki_from_ind[!is.na(hierarki_from_ind)]]



  #  z = cbind(kontoklasse=kontoklasser, funksjon=funksjoner, hiRelevant,as.matrix(w[[1]][ma, ,drop=FALSE] ))
  z = cbind(cbind(periode=periode,hierarki_from=hierarki_from,hiRelevant,stringsAsFactors=FALSE ),as.matrix(w[[1]][ma, ,drop=FALSE]))


  if(!formelKode){
    artER = GrabArt(z$mapsFrom, tre=FALSE,addto=FALSE, addfrom=FALSE)[,1,drop=TRUE]
    z = cbind(kontoklasse=kontoklasser, funksjon=funksjoner,art=artER, z,stringsAsFactors=FALSE)
  }
  else
    z$art = GrabArt(z$art, tre=FALSE,addto=FALSE, addfrom=FALSE)[,1,drop=TRUE]


  if(toKoder){
    if(formelKode){
      z = cbind(GrabArt(z$mapsTo, tre=TRUE,addto=TRUE),z)
    } else{
      artERto = GrabArt(z$mapsTo, tre=FALSE,addto=TRUE, addfrom=FALSE)[,1,drop=TRUE]
      z = cbind(kontoklasse_to=kontoklasser, funksjon_to=funksjoner,art_to=artERto, z,stringsAsFactors=FALSE)
    }
    names(z)[names(z)=="kontoklasse"] = "kontoklasse_from"
    names(z)[names(z)=="funksjon"] = "funksjon_from"
    names(z)[names(z)=="art"] = "art_from"
  }


  names(z)[names(z)=="mapsFrom"] = "from"
  names(z)[names(z)=="mapsTo"] = "to"

  names(z)[names(z)=="hierarki"] = "type_to"
  names(z)[names(z)=="hierarki_from"] = "type_from"



  z$sign[z$sign=="1"] = "+"
  z$sign[z$sign=="-1"] = "-"

  z = IntegerDataFrame(z)
  rownames(z) = NULL

  # Rekkefølgen på kolonnene fra venstre til høyre: Periode, Region, Kontoklasse, Funksjon, Art_to, Art_from, Sign, Type, Beløp
  # - Du kan fjerne kolonnen med level
  # - Kolonnen to bør hete "art_to" og kolonnen from bør hete "art_from"
  # - Kolonnen hierarki bør hete "Type" på samme måte som i den andre filen

  #return(list(z=z,hiRelevant=hiRelevant,dataKS=dataKS))
  #return(z)
  if(doStack){
    z <- Stack(z,stackVar=15:NCOL(z),blockVar = 1:14,  valueName = "belop",indName = "region") [,c("periode", "region",
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


GrabArt = function(x,tre=FALSE,addto=FALSE, addfrom=FALSE){
  n = NROW(x)
  if(tre){
    z= str_split(str_replace_all(x,":","."),"\\.",simplify=TRUE)
    #z[n,1] = str_replace(z[n,1],"KONSERN_","")
    z[ ,1] = str_replace(z[ ,1],"KONSERN_","")
    z[ ,1] = str_replace(z[ ,1],"KASSE_","")
    x = z[,3]
  }
  x = str_replace(x,"Saer_","")
  x = str_replace(x,"Nett_","")
  x = str_replace(x,"Konsern_","")
  #x[n] = str_replace(x[n],"KONSERN_","") # Ser bare på siste pga KONSERN_ kan tenkes å være med i kode fra før
  x = str_replace(x,"KONSERN_","") #
  x = str_replace(x,"KASSE_","") #
  #x
  if(tre){
    if(addfrom)
      return(data.frame(kontoklasse_from=z[,1], funksjon_from=z[,2], art_from=x, stringsAsFactors = FALSE ))
    if(addto)
      return(data.frame(kontoklasse_to=z[,1], funksjon_to=z[,2], art_to=x, stringsAsFactors = FALSE ))
    else
      return(data.frame(kontoklasse=z[,1], funksjon=z[,2], art=x, stringsAsFactors = FALSE ))
  }
  if(addfrom)
    return(data.frame(art_from=x, stringsAsFactors = FALSE ))
  if(addto)
    return(data.frame(art_to=x, stringsAsFactors = FALSE ))
  else
    return(data.frame(art=x, stringsAsFactors = FALSE ))
}



HierarchyFix2 = function(hd){
  hd$level = 1
  hdLik = hd$mapsFrom == hd$mapsTo
  if(sum(!hdLik))
    hd[!hdLik, ] =HierarchyFix(hd[!hdLik, ,drop=FALSE])
  rownames(hd) = NULL
  hd
}



