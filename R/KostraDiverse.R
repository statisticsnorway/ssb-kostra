


HierarchyFromDummy = function(d){
  x  = data.frame(
    mapsFrom   = colnames(d)[as.vector(col(d))],
    mapsTo = rownames(d)[as.vector(row(d))],
    sign = as.vector(d),
    stringsAsFactors = FALSE)
  x[x$sign!=0 , ,drop=FALSE]
}

SetOne = function(x){
  x[x>1] = 1
  x[x<-1] = -1
  x
}



RemoveDuplicated = function(x,cols,printAndWarning=TRUE){
  if(printAndWarning){
    dp1 = duplicated(x[,cols, drop=FALSE])
    dp2 = rev(duplicated(x[rev(seq_len(NROW(x))),cols, drop=FALSE]))
    if(any(dp1 | dp2)){
      cat("\n\n:::::::::::::: Warning: Duplicates removed. First in this table kept :::::::::::::::\n")
      print(x[dp2 | dp1,])
      cat("\n::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::\n\n\n")
      warning("Duplicates removed (first in printed table kept)")
    }
  }
  x[!duplicated(x[,cols, drop=FALSE]),]
}




PrintAllDuplicated = function(x,cols){
  dp1 = duplicated(x[,cols, drop=FALSE])
  dp2 = rev(duplicated(x[rev(seq_len(NROW(x))),cols, drop=FALSE]))
  print(x[dp1 | dp2,])
}






#   # Funksjonen virker men ser ikke ut til å virke effektivt
#   StepwiseSelectionCrossDataDummyHierarchy = function(dataDummyHierarchies,codeFrame,step=1000){
#     print("StepwiseSelectionCrossDataDummyHierarchy")
#     n=length(dataDummyHierarchies)
#     if(n==0)
#       return(dataDummyHierarchies)
#     m = NROW(codeFrame)
#     k=0L
#     z=Matrix(0,m,NCOL(dataDummyHierarchies[[1]]))
#     colnames(z) = colnames(dataDummyHierarchies[[1]])
#     while(k<m){
#       sel  = k+seq_len(min(step,m-k))
#       zSel = SelectionCrossDataDummyHierarchy(dataDummyHierarchies,codeFrame[sel, ,drop=TRUE])
#       z[sel,] = zSel
#       k = max(sel)
#     }
#     z
#   }


#   # Funksjonen virker men ser ikke ut til å virke effektivt
#   StepwiseSelectionCrossDataDummyHierarchyMultiplyWithValueMatrix = function(dataDummyHierarchies,codeFrame,step=1000,valueMatrix){
#     print("StepwiseSelectionCrossDataDummyHierarchyMultiplyWithY")
#     n=length(dataDummyHierarchies)
#     if(n==0)
#       return(Mult(dataDummyHierarchies, valueMatrix)) #return(dataDummyHierarchies %*% valueMatrix)
#     m = NROW(codeFrame)
#     k=0L
#     #z=Matrix(0,m,NCOL(dataDummyHierarchies[[1]]))
#     #z=Matrix(0,m,NCOL(valueMatrix))
#     z=matrix(0,m,NCOL(valueMatrix))
#   
#     while(k<m){
#       sel  = k+seq_len(min(step,m-k))
#       xSel = SelectionCrossDataDummyHierarchy(dataDummyHierarchies,codeFrame[sel, ,drop=TRUE])
#       #print(dim(xSel))
#       #print(dim(valueMatrix))
#       #print(sel)
#   
#       z[sel,] = as.matrix(Mult(xSel, valueMatrix)) #as.matrix(xSel %*% valueMatrix)
#       k = max(sel)
#     }
#     colnames(z) = colnames(valueMatrix)
#     z
#   }




DummyHierarchy2 = function(x,inputInOutput=FALSE){
  hierarchies = list(x)
  i=1
  DummyHierarchy(mapsFrom=hierarchies[[i]]$mapsFrom,
                 mapsTo=hierarchies[[i]]$mapsTo,
                 mapsInput= attr(hierarchies[[i]],"mapsInput"),    # Må med siden: 'NA' indices are not (yet?) supported for sparse Matrices
                 keepCodes = attr(hierarchies[[i]],"keepCodes"),
                 sign=hierarchies[[i]]$sign,
                 level=hierarchies[[i]]$level,
                 inputInOutput=inputInOutput[i])
}

LagInteger = function(x){
  z = as.integer(x)
  if(identical( as.numeric(as.vector(z)),as.numeric(as.vector(x))))
    return(z)
  warning("Ikke integer i output siden omgjøring til integer ikke ga identisk resultat")
  return(x)
}



#' SortRows with rownames removed
#'
#' @param x as input to SortRows
#'
#' @return sorted version of x
#' @export
#' @importFrom SSBtools SortRows
#' @keywords internal
#'
Srn = function(x){
  x =SortRows(x)
  rownames(x) = NULL
  x
}


#' PrintHeadTail
#'
#' @param x data.frame
#' @param n n
#' @param title title
#'
#' @return None (invisible NULL)
#' @export
#'
PrintHeadTail = function(x,n=2, title = NULL){
  cat("\n")

  if(!is.null(title))
    cat(title,"\n")
  else
    print(as.list(match.call())[[2]])

  if(is.null(x))
    print(NULL)
  else{
    if(!is.data.frame(x)  & is.list(x)){
      z = vector("list", length(x))
      names(z) = names(x)
      for(i in seq_len(length(x))){
        if(is.null(x[[i]])){
          z[[i]] = NULL
        } else {
          z[[i]] = as.data.frame(x[[i]])
          rownames(z[[i]]) = NULL
          rows = seq_len(NROW(x[[i]]))
          rows = unique(c(head(rows,n),tail(rows,n)))
          z[[i]] = z[[i]][rows, ,drop=FALSE]
        }
      }
      print(z)
    } else {
      x = as.data.frame(x)
      rownames(x) = NULL
      rows = seq_len(NROW(x))
      rows = unique(c(head(rows,n),tail(rows,n)))
      print(x[rows, ,drop=FALSE])
    }
  }
  cat("\n")
}


#' AutoNetting
#'
#' Ulike varianter som ble utprøvd. AutoNettingCopy er løsningen som ble valgt.
#'
#' @param nettinghierarki nettinghierarki
#' @param hierarki hierarki
#' @param title title
#'
#' @return Ny versjon av nettinghierarki
#' @export
#' @keywords internal
#'
AutoNetting = function(nettinghierarki,hierarki, title ="nettinghierarki", toRemove = TRUE, singleLoop=FALSE){
  if(toRemove)
    hierarki  = hierarki[!(hierarki$to %in% nettinghierarki$to), ,drop=FALSE]

  n1 = NROW(nettinghierarki)

  loop = TRUE
  while(loop){
    hierarkiA  = hierarki[(hierarki$from %in% nettinghierarki$to), ]
    hierarkiB  = hierarki[!(hierarki$from %in% nettinghierarki$to), ]
    #print(hierarkiA)
    loop = NROW(hierarkiA)>0
    hierarki = hierarkiB
    nettinghierarki =rbind(nettinghierarki,hierarkiA)
    if(singleLoop) loop=FALSE
  }
  n2 = NROW(nettinghierarki)

  cat("\n AutoNetting: ", title, " fra ", n1, " til ", n2, " rader\n")
  nettinghierarki
}


#' @rdname AutoNetting
#' @export
#' @keywords internal
AutoNettingFF = function(nettinghierarki,hierarki, title ="nettinghierarki", toRemove = FALSE,singleLoop=FALSE)
  AutoNetting(nettinghierarki,hierarki,title,toRemove,singleLoop)

#' @rdname AutoNetting
#' @export
#' @keywords internal
AutoNettingFT = function(nettinghierarki,hierarki, title ="nettinghierarki", toRemove = FALSE,singleLoop=TRUE)
  AutoNetting(nettinghierarki,hierarki,title,toRemove,singleLoop)

#' @rdname AutoNetting
#' @export
#' @keywords internal
AutoNettingTF = function(nettinghierarki,hierarki, title ="nettinghierarki", toRemove = TRUE,singleLoop=FALSE)
  AutoNetting(nettinghierarki,hierarki,title,toRemove,singleLoop)

#' @rdname AutoNetting
#' @export
#' @keywords internal
AutoNettingTT = function(nettinghierarki,hierarki, title ="nettinghierarki", toRemove = TRUE,singleLoop=TRUE)
  AutoNetting(nettinghierarki,hierarki,title,toRemove,singleLoop)




#' @rdname AutoNetting
#' @export
#' @keywords internal
AutoNettingNy = function(nettinghierarki,hierarki, title ="nettinghierarki",loop=TRUE){

  n1 = NROW(nettinghierarki)

  hierarkiW  = hierarki[(hierarki$from %in% nettinghierarki$to), ,drop=FALSE]
  hierarkiW$from = paste("w",hierarkiW$from,sep="_")

  hTo = unique(nettinghierarki$to)
  netthTo = nettinghierarki[rep(1,length(hTo)), ,drop=FALSE]
  netthTo$from = paste("w",hTo,sep="_")
  netthTo$to   = hTo
  netthTo$sign   = "+"

  nettinghierarki$to= paste("w",nettinghierarki$to,sep="_")

  nettinghierarki = rbind(nettinghierarki,netthTo,hierarkiW)


  hierarki  = hierarki[!(hierarki$to %in% nettinghierarki$to), ,drop=FALSE]


  #loop = TRUE
  while(loop){
    hierarkiA  = hierarki[(hierarki$from %in% nettinghierarki$to), ]
    hierarkiB  = hierarki[!(hierarki$from %in% nettinghierarki$to), ]
    #print(hierarkiA)
    loop = NROW(hierarkiA)>0
    hierarki = hierarkiB
    nettinghierarki =rbind(nettinghierarki,hierarkiA)
  }
  n2 = NROW(nettinghierarki)

  cat("\n AutoNetting: ", title, " fra ", n1, " til ", n2, " rader\n")
  nettinghierarki
}



#' @rdname AutoNetting
#' @export
#' @keywords internal
AutoNettingNyFALSE = function(nettinghierarki,hierarki, title ="nettinghierarki",loop=FALSE)
  AutoNettingNy(nettinghierarki,hierarki,title,loop)


#' @rdname AutoNetting
#' @export
#' @keywords internal
AutoNettingIngenEndering = function(nettinghierarki,hierarki=NULL, title = NULL){
  nettinghierarki
}



#' @rdname AutoNetting
#' @export
#' @keywords internal
AutoNettingCopy = function(nettinghierarki,hierarki, title ="nettinghierarki",copyKode = "COPY"){

  n1 = NROW(nettinghierarki)

  rCopy = nettinghierarki$from == copyKode
  toCopy = nettinghierarki$to[rCopy]

  nettinghierarkiTo = unique(nettinghierarki$to)

  nettinghierarki = nettinghierarki[!rCopy, ,drop=FALSE]

  hierarki  =  hierarki[(hierarki$to %in% toCopy), ,drop=FALSE]

  hierarki  =  hierarki[(hierarki$from %in% nettinghierarkiTo), ]

  nettinghierarki =rbind(nettinghierarki,hierarki)

  n2 = NROW(nettinghierarki)

  cat("\n AutoNetting: ", title, " fra ", n1, " til ", n2, " rader\n")
  nettinghierarki
}


AutoNettingCopyOld = function(nettinghierarki,hierarki, title ="nettinghierarki",copyKode = "COPY"){

  n1 = NROW(nettinghierarki)

  rCopy = nettinghierarki$from == copyKode
  toCopy = nettinghierarki$to[rCopy]

  nettinghierarki = nettinghierarki[!rCopy, ,drop=FALSE]

  hierarki  =  hierarki[(hierarki$to %in% toCopy), ,drop=FALSE]

  hierarki  =  hierarki[(hierarki$from %in% nettinghierarki$to), ]

  nettinghierarki =rbind(nettinghierarki,hierarki)

  n2 = NROW(nettinghierarki)

  cat("\n AutoNetting: ", title, " fra ", n1, " til ", n2, " rader\n")
  nettinghierarki
}





FixRegionkodeNy = function(region,warningText=NULL,viaFactor=TRUE){
  if(is.numeric(region))
    region= as.character(region)
  if(is.factor(region)){
    viaFactor = TRUE
  } else {
    if(viaFactor){
      region = as.factor(region)
    }
  }
  if(viaFactor)
    x= as.integer(attr(region,"levels"))
  else
    x= as.integer(region)
  z= Number(x,6)
  z[x<=9999] = Number(x[x<=9999],4)
  if(!viaFactor)
    return(z)
  attr(region,"levels") = z
  as.character(region)
}







#' Sørger for at regiontall blir character med 4 eller 6 plasser og ledende nuller
#'
#' Input som ikke representerer tall forblir uendret, bortsett fra eventuell omgjøring ved \code{\link{as.character}}.
#'
#' Funksjonen kaller \code{\link{AddLeadingZeros}} med \code{places=6} for store tall (\code{>9999}) og ellers med \code{places=4}.
#'
#' @param region Vektor med regionkoder. Kan være  integer, numeric, character eller factor.
#' @param warningText Ved ikke-NULL skrives warning med warningText dersom endring gjøres.
#'
#' @return Endrede regionkoder som en character vektor
#'
#' @note Funksjonen er ekstra grundig dokumentert siden den brukes som eksempel i workshop i bygging av  r-pakker.
#'
#' @seealso \code{\link{Number}}
#'
#' @importFrom SSBtools AddLeadingZeros
#'
#' @export
#' @author Øyvind Langsrud
#' @examples
#' FixRegionkode(c(101, 301, 30107))
#' FixRegionkode(c("101", "301", "30107"))
#' FixRegionkode(c("101", "301", "30107", "Oslo"))
#' FixRegionkode(c("101", "301", "030107"), "Regionskoder endret")
#' FixRegionkode(c("0101", "0301", "030107"), "Regionskoder endret")
#'
FixRegionkode <- function(region, warningText = NULL) {
  region <- as.character(region)
  seksSiffer <- suppressWarnings(as.integer(region)) > 9999
  fireSiffer <- !seksSiffer
  seksSiffer[is.na(seksSiffer)] <- FALSE
  fireSiffer[is.na(fireSiffer)] <- FALSE
  region[fireSiffer] <- AddLeadingZeros(region[fireSiffer], 4, warningText = warningText, viaFactor = TRUE)
  region[seksSiffer] <- AddLeadingZeros(region[seksSiffer], 6, warningText = warningText, viaFactor = TRUE)
  region
}





#FixRegionkodeGammelFunksjon = function(region){
#  x= as.integer(region)
#  z= Number(x,6)
#  z[x<=9999] = Number(x[x<=9999],4)
#  z
#}



#  WildcardGlobbingOld = function(x,wg,sign=TRUE){
#    #print("WildcardGlobbing")
#    #print(dim(x))
#    #print(dim(wg))
#    #print(wg)
#    sel = rep(FALSE,dim(x)[1])
#    for(i in 1:NROW(wg)){
#      seli = rep(TRUE,dim(x)[1])
#      for(j in 1:NCOL(wg)){
#        seli = seli&grepl(glob2rx(wg[i,j]) , x[,names(wg)[j]])
#      }
#      sel = sel|seli
#    }
#    #if(!sign)
#    #  return(x[!sel, ,drop=FALSE])
#    #x[sel, ,drop=FALSE]
#    if(!sign)
#      return(!sel)
#    sel
#  }




















