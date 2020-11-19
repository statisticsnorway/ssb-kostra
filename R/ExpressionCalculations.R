#' AutoFormel
#'
#' Formler som avhenger av hverandre omskrives slik at alle begrep på høyres side refererer til verdier fra beregning uten formler.
#'
#' @param s Vektor av formler
#' @param fra2til3  Dupliserer formelbegreper uten kontoklasse ved TRUE
#' @param laAvhenge Erstatter IKKE avhengigheter med parenteser ved TRUE
#'
#' @return Vektor av formler
#' @examples
#'  AutoFormel(c("D:FG2:AGD13 = D:FG2:AGD13 + D:FG2:AG12","D:FG2:AGD18 = D:FG2:AGD18 + D:FG2:AGD13"))
#' @export
#' @importFrom stringr str_split
AutoFormel = function(s, fra2til3 = TRUE, laAvhenge=FALSE){

  n = length(s)
  a=lapply(str_split(s,"="),trimws)

  left   = rep("",n)
  right  = rep("",n)

  for(i in seq_len(n)){
    left[i] = a[[i]][1]
    right[i] = a[[i]][2]
  }



  if(fra2til3){
    er2 = cbind(str_split(left,":",simplify=TRUE),"")[,3]==""
    sum_er2 = sum(er2)
    left  = c(left,rep("",sum_er2))
    right  = c(right,rep("",sum_er2))
    for(i in seq_len(sum_er2)){
      ix = seq_len(n)[er2][i]
      s1 = gsub("[^0-9A-Za-z_.:' ]", " ", right[ix])
      rightx = str_split(s1," ")[[1]]
      rightix = rightx[grepl(":",rightx)]
      left[n+i] = left[ix]
      left[ix] = paste("D",left[ix],sep=":")
      left[n+i] = paste("I",left[n+i],sep=":")
      right[n+i] = right[ix]
      for(j in seq_len(length(rightix))){
        right[ix] = sub(rightix[j],paste("D",rightix[j],sep=":"),right[ix])
        right[n+i] = sub(rightix[j],paste("I",rightix[j],sep=":"),right[n+i])
      }
    }
    n = n + sum_er2

  }
  if(laAvhenge)
    return(paste(left,right,sep=" = "))

  s1 = gsub("[^0-9A-Za-z_.:' ]", " ", right) # Fjerner alle tegn som ikke kan brukes i variabelnavn bortsett fra :
  rightx = str_split(s1," ")

  for(i in seq_len(n)){
    rightx[[i]] = rightx[[i]][grepl(":",rightx[[i]])]
  }
  depend = matrix(FALSE,n,n)



  for(i in seq_len(n))
    for(j in seq_len(n)){
      if(i!=j)
        depend[i,j] = left[j] %in%rightx[[i]]
    }


  FixOneDepend = function(left,right,depend,i){
    for(j in  which(depend[,i])){
      #cat(left[i],"@",left[j]," ",sep="")
      right[j] = gsub(left[i],paste("(",right[i],")"),right[j])
    }
    right
  }


  dependRow = rowSums(depend)>0
  nDepend = sum(dependRow)
  while(nDepend>0){
    #cat("\n", nDepend," formelavhengigheter igjen: ")
    for( i in which(!dependRow)){
      right = FixOneDepend(left,right,depend,i)
      depend[,i] = FALSE
    }
    dependRow = rowSums(depend)>0
    nDependOld = nDepend
    nDepend = sum(dependRow)
    if(nDepend==nDependOld)
      stop("AutoFormel fungerer ikke p.g.a sirkelavhengighet")
  }
  #cat("\n")
  #flush.console()
  paste(left,right,sep=" = ")
}


VenstreKoder = function(formler){
  unlist(lapply(str_split(formler,"="),function(x) trimws(x[1])))
}



FormelKoder = function(formler){
  x = NULL
  for(k in seq_len(length(formler))){
    a= as.data.frame(ExprCrossVar(formler[k])$codes[1, ,drop=FALSE])
    if(NCOL(a)==2)
    a = cbind(c("D","I"),a)
    names(a) = c("kontoklasse", "funksjon", "art")
    x = rbind(x,a)
  }
  CharacterDataFrame(x)
}


Li = function(x){  # LagInteger uten warning med if(is.integer(x))
  if(is.integer(x))
    return(x)
  z = as.integer(x)
  if(identical( as.numeric(as.vector(z)),as.numeric(as.vector(x))))
    return(z)
  warning("Ikke integer i output siden omgjøring av formeloutput til integer ikke ga identisk resultat")
  return(x)
}


ExprCrossVar = function(s="FG2:AGD12 = tull + 8.342*x + FG2:AGD12 - 800:EIENDOMSSKATT_OG__ANDRE_SKATTER - 5*6^1322 + 34.2343",crossSplit="_wWMWw_",startString="w_",isInteger=FALSE){

  s1 = gsub("[^0-9A-Za-z_.:' ]", " ", s) # Fjerner alle tegn som ikke kan brukes i variabelnavn bortsett fra :
  x = str_split(s1," ")[[1]]
  x = x[grepl(":",x)]
  codes    = str_split(x,":",simplify=TRUE)
  varnames = paste(startString,gsub(":",crossSplit,x),sep="")

  s2 = s
  for(i in seq_len(length(x))){
    s2 = gsub(x[i],varnames[i],s2)
  }

  if(isInteger)
    express = paste("Li(",str_split(s2,"=")[[1]][2],")")
  else
    express = str_split(s2,"=")[[1]][2]

  list(x=x,codes=codes,varnames=varnames,express=express)

}


DataOgRaderMmOld = function(data, funksjon="FG2", art = "AGD12"){
  rader = which(data$funksjon== funksjon & data$art ==art)
  data=data[rader, ,drop=FALSE]
  regnskapsomfang = unique(data$regnskapsomfang)
  region = unique(data$region)
  kontoklasse = unique(data$kontoklasse)
  list(data=data,rader=rader,regnskapsomfang=regnskapsomfang, region=region, kontoklasse=kontoklasse)
}


DataOgRaderMm = function(data, funksjon="FG2", art = "AGD12", kontoklasse = NULL){

  if(is.null(kontoklasse))
    rader = which(data$funksjon== as.character(funksjon) & data$art ==as.character(art))
  else
    rader = which(data$funksjon== as.character(funksjon) & data$art ==as.character(art) & data$kontoklasse == as.character(kontoklasse) )
  data=ForceCharacterDataFrame(data[rader, ,drop=FALSE])
  regnskapsomfang = unique(data$regnskapsomfang)
  region = unique(data$region)
  #kontoklasse = unique(data$kontoklasse)
  list(data=data,rader=rader,regnskapsomfang=regnskapsomfang, region=region)
}

FormelKorreksjonerDataFrameOutput = function(a,belop,formler){

#return(list(a,belop,formler))

  m = length(belop[[1]])

  en =  m==1


  #n = length(a[[k]])

  n = length(belop)



  if(en){
    x = vector("list",n)
    for(k in seq_len(n)){
      x1 =a[[k]]$data
      x1$formel = formler[k]
      x2 = belop[[k]][[1]]
      names(x1)[names(x1)=="belop"] = "belop1"
      x[[k]] = (cbind(x1,x2))
    }
    #return(x)
  }
  else
  {
    x = vector("list",n)
    for(k in seq_len(n)){
      x2 = belop[[k]]
      x1 = a[[k]]$data
      rnx = rev(names(x1))
      nam4 = rev(rnx[1:4])
      rnx[1:5] = paste(  rnx[1:5],"1",sep="") ############################# 4 til 5 her

      names(x1) = rev(rnx)

      nam5 = c(nam4,"formel")

      for(i in 1:m){
        x2i = x2[[i]]
        nam = names(x2i)
        nam[nam=="belop"] = ""
        nam = paste(names(x2)[i],nam,sep=":")
        nam[1] = names(x2)[i]
        names(x2i) = nam
        if(i==1)
          xb = x2i
        else
          xb = cbind(xb,x2i)
      }
      xb$formel = formler[k]
      nb5 = names(xb) %in% nam5
      x[[k]] = cbind(x1,xb[,nb5 ,drop=FALSE],xb[,!nb5 ,drop=FALSE])
    }
  }
  x = RbindAll(x)
  x = ForceCharacterDataFrame(x)
  x
}





FormelKorreksjoner = function(formler,outputdata ,inputdata, funksjonshierarki, artshierarki, inputdata_saer,
                              artshierarki_nettinger, artshierarki_nettinger_kasse, regioner ,frameOutput=FALSE, rader0warning=TRUE, printData=TRUE){

  #>   #b = ExprCrossVar("FG2:AGD12 = FG2:AGD12 - 800:EIENDOMSSKATT_OG__ANDRE_SKATTER")
  #  >   #b = ExprCrossVar("FG2:AGD12 = FG2:AGD12 - 800:EIENDOMSSKATT_OG__ANDRE_SKATTER + 3.14*FGK18:SALGSINNTEKTER")
  #  >   b = ExprCrossVar("FG2:AGI2 = FG2:AGI2 - FG1:AGI1")

  #a = DataOgRaderMm(z, art = "AGI2")
  #a = DataOgRaderMm(z)
  #b = ExprCrossVar("FG2:AGD12 = FG2:AGD12 - 800:EIENDOMSSKATT_OG__ANDRE_SKATTER")
  #b = ExprCrossVar("FG2:AGD12 = 800:EIENDOMSSKATT_OG__ANDRE_SKATTER + FG2:AGD12 - FG2:AGI2 - FG1:AGI1 + 3.14*FGK18:SALGSINNTEKTER")


  # Det meste gjøres ved å loope over alle formlene med data i tilsvarende lister. Dermed blir det mye [[k]] i koden
  # KostraRegnskap kjøres kun en gang med koder samlet fra alle formlene


  cat("\n\n[--------------------------------------------------------> \n")
  cat("[-------- Egne beregninger for formelkorreksjoner --------->\n")
  cat("[-------------------------------------------------------->\n\n")




  b = vector("list",length(formler))
  a = vector("list",length(formler))
  newC = vector("list",length(formler))
  out = vector("list",length(formler))
  nyedata = vector("list",length(formler))
  rader = vector("list",length(formler))

  belop = vector("list",length(formler))

  funksjonCodes = NULL
  artCodes = NULL
  regionCodes = NULL
  alleRader = NULL


  if(is.null(outputdata$regnskapsomfang)){
    belopNavn = c("kasse", "saerbedrift", "nettinger_kasse","nettinger_saer", "konsern")
    outp = "fire"
  } else {
    belopNavn = "belop"
    outp = "en"
  }


  isInteger=is.integer(outputdata[,belopNavn[1]])


  nullData = outputdata[integer(0) , ,drop=FALSE]

  for(k in seq_len(length(formler))){

    nyedata[[k]] = nullData
    rader[[k]]   = integer(0) # null rader


    b[[k]] = ExprCrossVar(formler[k],isInteger=isInteger)

    if(any(b[[k]]$codes==""))
      stop("Noe er galt. Ulikt antal : ?")

    if(NCOL(b[[k]]$codes)>2){
      b[[k]]$codes = b[[k]]$codes[,c(2,3,1)] #  bytter rekkefolge for å slippe omskrive gammel kode
      a[[k]] = DataOgRaderMm(outputdata, funksjon=b[[k]]$codes[1,1] , art = b[[k]]$codes[1,2],
                             kontoklasse = b[[k]]$codes[1,3])

    }
    else
      a[[k]] = DataOgRaderMm(outputdata, funksjon=b[[k]]$codes[1,1] , art = b[[k]]$codes[1,2])


    if(NROW(a[[k]]$data)==0)
      if(rader0warning)
      warning(paste("Formelen",formler[k],"samsvarer med 0 rader i data"))

    newC[[k]] = is.na(Match(as.data.frame(b[[k]]$codes),as.data.frame(b[[k]]$codes)[1, ,drop=FALSE]))

    belop[[k]] = vector("list",length(belopNavn))
    names(belop[[k]]) = belopNavn




    for(j in seq_len(length(belopNavn))){
      belop[[k]][[j]] = matrix(a[[k]]$data[,belopNavn[j]],ncol=1)
    }


    funksjonCodes = unique(c(funksjonCodes,  unique(b[[k]]$codes[newC[[k]], ,drop=FALSE][,1]) ))
         artCodes = unique(c(     artCodes,  unique(b[[k]]$codes[newC[[k]], ,drop=FALSE][,2]) ))
         regionCodes = unique(c(regionCodes, a[[k]]$region))

    alleRader = c(alleRader,a[[k]]$rader)
  }


  if(any(duplicated(alleRader)))
    stop("Overlappende formler")



  if(length(regionCodes)==0){

    cat("[-------------> Nei, det var visst ikke nødvendig allikevel ..... \n\n\n")
    if(is.null(frameOutput))
      return(NULL)

    if(frameOutput)
      return(NULL)
    return(list(nyedata=nyedata,rader=rader))
  }




  if(length(artCodes)==0  | length(funksjonCodes)==0){
    wW = outputdata[integer(0), ,drop=FALSE]

    cat("[-----> Nei, dette var så enkelt at nye beregninger ikke var nødvendig ... \n\n\n")
  }
  else
    wW = KostraRegnskap(inputdata, funksjonshierarki, artshierarki, inputdata_saer, artshierarki_nettinger, artshierarki_nettinger_kasse,
                      funksjoner = funksjonCodes,
                      arter = artCodes ,
                      regioner = regionCodes, output = outp,  printData = FALSE, autoNetting = FALSE, lag0300 = FALSE, fixRegionkode = FALSE, fixArtkode = FALSE, fixFunksjonkode = FALSE)



  for(k in seq_len(length(formler))){
    for(i in seq_len(length(b[[k]]$x)-1)){

    if(newC[[k]][i+1]){

      if(NCOL(b[[k]]$codes)>2)
        mr1 =  which(wW$funksjon == b[[k]]$codes[i+1,1] &  wW$art== b[[k]]$codes[i+1,2] &  wW$kontoklasse== b[[k]]$codes[i+1,3] )
        else
          mr1 =  which(wW$funksjon == b[[k]]$codes[i+1,1] &  wW$art== b[[k]]$codes[i+1,2])

    if(length(belopNavn)==1){
      if(NCOL(b[[k]]$codes)>2)
        mr2 = Match(a[[k]]$data[, c("regnskapsomfang", "region")],wW[mr1, c("regnskapsomfang", "region")])
        else
      mr2 = Match(a[[k]]$data[, c("regnskapsomfang", "region", "kontoklasse")],wW[mr1, c("regnskapsomfang", "region", "kontoklasse")])
    } else {
      if(NCOL(b[[k]]$codes)>2)
        mr2 = Match(a[[k]]$data[, c("region"),drop=FALSE],wW[mr1, c("region"),drop=FALSE])
        else
      mr2 = Match(a[[k]]$data[, c("region", "kontoklasse")],wW[mr1, c("region", "kontoklasse")])
    }

    mr = mr1[mr2]



    if(any(is.na(mr)))
      warning(paste("Formeltermen",b[[k]]$x[i+1],"fører til NA"))



    } else
    mr = NULL




    for(j in seq_len(length(belopNavn))){
      if(newC[[k]][i+1]){
        belop[[k]][[j]] = cbind(belop[[k]][[j]],matrix(wW[mr,belopNavn[j]],ncol=1))
      } else {
        belop[[k]][[j]] = cbind(belop[[k]][[j]],belop[[k]][[j]][ ,1 , drop=FALSE])
      }
    }




  }


  nyedata[[k]] = a[[k]]$data

  rader[[k]] = a[[k]]$rader

  for(j in seq_len(length(belopNavn))){
    colnames(belop[[k]][[j]])[seq_len(length(b[[k]]$x))[-1]] = b[[k]]$varnames[-1]
    colnames(belop[[k]][[j]])[1] = "belop"
    belop[[k]][[j]]= as.data.frame(belop[[k]][[j]])
    belop[[k]][[j]][,"belop"] = with(belop[[k]][[j]],eval(parse(text=b[[k]]$express)))


    colnames(belop[[k]][[j]])[seq_len(length(b[[k]]$x))[-1]] = b[[k]]$x[-1]
    nyedata[[k]][,belopNavn[j]] = belop[[k]][[j]][,"belop"]
    rownames(belop[[k]][[j]]) = NULL
  }

  if(printData)
  if(length(belopNavn)==1)
    if(NROW(a[[k]]$data)>0){
      aKdata = a[[k]]$data
      names(aKdata)[names(aKdata)=="belop"] = "belop1"
      PrintHeadTail(cbind(aKdata,belop[[k]][[1]]),title = formler[k])
      flush.console()

      #print("----------------------------------------------------")
      #print(a[[k]])
      #print(cbind(a[[k]]$data,belop[[k]][[1]]),title = formler[k])
    }


  #res = cbind(a$data[,c(1,2,3,5,4,6)],belop[[1]])
  #rownames(res) = NULL
  }


  if(is.null(frameOutput))
    return(list(a=a,b=b,nyedata=nyedata,belop=belop, rader=rader))

  if(frameOutput)
    return(FormelKorreksjonerDataFrameOutput(a,belop,formler))

  list(nyedata=nyedata,rader=rader)
}










