#' Table suppression according to a frequency rule following the standards in the Kostra project.
#'
#'
#' @encoding UTF8
#'
#' @param data Input data set of class data.frame
#' @param idVar Id-variable (name or number)
#' @param strataVar Strata-variable(s) (name or number)
#' @param freqVar Variable(s) holding counts (name or number)
#' @param freqVarGroup  NULL (default) or integer representing groups of variables (see details)
#' @param protectZeros When TRUE empty cells (count=0) is considered sensitive
#' @param maxN All cells having counts <= maxN are set as primary suppressed
#' @param method Parameter "method" in ProtectTable: "SIMPLEHEURISTIC", "Simple", "SimpleSingle", "OPT", "HITAS" or  "HYPERCUBE"
#' @param output One of "suppressed" (default), "freq", "sdcStatus" or "extraWide" (only when  freqVarGroup is NULL)
#' @param total	String used to name totals.
#' @param split Parameter to \code{\link{AutoSplit}} - see varNames and rowData above.
#'    When NULL automatic splitting without needing a split string.
#' @param singleTotal When TRUE identical rowsums in all freqVarGroups needed.
#'    When FALSE totals for each freqVarGroup will be in output.
#' @param ... Additional variables that will be included in output (name or number).
#'
#' @details
#'
#'   \strong{When freqVarGroup is NULL:}
#'
#'   This function is a wrapper to \code{\link{ProtectTable}} with dimVar=c(idVar, strataVar).
#'   The function \code{\link{GetData}} is used.
#'
#'   Note that the names of output variables are strange when a single freqVar variable is input.
#'   This can be fixed by using \code{freqVarGroup=1} instead of NULL.
#'
#'   \strong{When freqVarGroup is NOT NULL:}
#'
#'   The suppression function (as when freqVarGroup is NULL) is run several times according to the groups with \code{freqVarGroup>0}.
#'   We have to types of groups: Single variables and several variables.
#'   All groups of  several variables must have identical rowsums.
#'
#'   Variables with \code{freqVarGroup<1} will be included in output sorted as input.
#'
#'   A warning is produced if generated total-output is not unique. Only the first result is then returned.
#'   In the case of \code{output="suppressed"} this means that the suppressions of the total has been is different.
#'   In the case of \code{output="sdcStatus"} only coding may have been different.
#'
#'
#' @note
#'
#' Even if freq-variables with \code{freqVarGroup<1} is not used they will be read by \code{\link{GetData}}  together with the other
#' the freq-variables variables into a matrix. Use a common numeric type for all these variables to prevent change of data type.
#'
#' All codes in \code{idVar} and \code{strataVar} must be unique. If not, automatic re-coding will be done with a warning.
#' Using \code{addName=TRUE} in input will prevent this warning.
#' Anyway, when \code{output="extraWide"} non-unique codes produce problematic output.
#'
#' Normally a value is only safe if sdcStatus="s". When using tau-argus sdcStatus="z" is also safe when protectZeros="FALSE".
#' But currently tau-argus methods are not allowed in ProtectKostra. Use a simpler (binary) coding of "sdcStatus" in future version?
#' When the underlying function \code{\link{ProtectTable}} results in error: sdcStatus="e".
#'
#'
#' @return A data.frame with as many rows as input
#'
#' @importFrom easySdcTable ProtectTable
#'
#' @author Øyvind Langsrud
#'
#' @export
#'
#' @examples
#'
#'  # ==================================
#'  #    Examples without freqVarGroup
#'  # ==================================
#'
#'  # ==== Example 1 , 8 regions ====
#'  z1w = KostraData("z1w")
#'  ProtectKostra(z1w,idVar="region",freqVar=2:5)
#'
#'  # ==== Example 2 , 11 regions ====
#'  z2w <- KostraData("z2w")
#'  ProtectKostra(z2w,idVar="region",strataVar=c("fylke","kostragr"),freqVar=4:7)
#'
#'  # ==== Example 3 , 36 regions ====
#'  z3w <- KostraData("z3w")
#'  ProtectKostra(z3w,idVar="region",strataVar=c("fylke","kostragr"),freqVar=4:15)
#'
#'  #  ==== Example 3b , 36 regions == with three level column name coding
#'  z3wb <- KostraData("z3wb")
#'  ProtectKostra(z3wb,idVar="region",strataVar=c("fylke","kostragr"),freqVar=4:15)
#'
#'  #  ==== Example 4 , 437 regions ====
#'  z4w <- KostraData("z4w")
#'  ProtectKostra(z4w,idVar="region",strataVar="fylke",freqVar=4:15)
#'
#'  # =====================================================================
#'  #    Examples with extra variables in output and several id variables
#'  # =====================================================================
#'
#'  ProtectKostra(z3wb,idVar="region",strataVar=c("fylke","kostragr"),freqVar=4:15,fylke="fylke",kostragr="kostragr")
#'
#'  # Same using DotWrap
#'  DotWrap("ProtectKostra",dots=c("fylke","kostragr"),z3wb,idVar="region",strataVar=c("fylke","kostragr"),freqVar=4:15)
#'
#'  # Several id variable
#'  ProtectKostra(z3wb,idVar=c("region","fylke","kostragr"),strataVar=c("fylke","kostragr"),freqVar=4:15,region="region")
#'
#'  # ==================================
#'  #    Examples with freqVarGroup
#'  # ==================================
#'
#'  # Generate example data for this function
#'  exData   <- KostraData("z3w")[,c(1:15,15,4:6)]
#'  names(exData)[12:19]=c("s1","s2","s3","s4","A","B","C","D")
#'  exData[,"s4"] <- rowSums(exData[,4:11]) - rowSums(exData[,12:14])
#'
#'  # Create input parameter
#'  freqVarGroup <- c(1,1,1,1,1,1,1,1,2,2,2,2,3,4,-1,5) # Same as c(rep(1,8),rep(2,4),3,4,-1,5)
#'
#'  a <- ProtectKostra(exData ,idVar="region",strataVar=c("fylke","kostragr"),freqVar=4:19,freqVarGroup=freqVarGroup)
#'  #  Now output of a$C is just missing since "-1"
#'
#'  names(exData)[18] <- "arbeid" #  Rename from "C" to "arbeid"
#'
#'  b <- ProtectKostra(exData ,idVar="region",strataVar=c("fylke","kostragr"),freqVar=4:19,freqVarGroup=freqVarGroup)
#'  # Now "arbeid" in output is still between "B" and "D" as in input. And b$arbeid is NOT just missing
#'
#'  # singleTotal=TRUE
#'  ProtectKostra(exData ,idVar="region",strataVar=c("fylke","kostragr"),freqVar=4:19,freqVarGroup=freqVarGroup, singleTotal=TRUE)
#'
#'  exData[4,4] <- 3  # Warning will be produced
#'  ProtectKostra(exData ,idVar="region",strataVar=c("fylke","kostragr"),freqVar=4:19,freqVarGroup=freqVarGroup)

#'
#'  freqVarGroup <- c(11,11,11,11,11,11,11,11,2,2,2,2,3,4,0,5)  # Using this instead give same result in different order
#'
#'
#'  # ========================================
#'  #    Examples with a single freq-variable
#'  # ========================================
#'
#'  z1w = KostraData("z1w")
#'  ProtectKostra(z1w,idVar="region",freqVar=2)  # wrong "name"
#'  ProtectKostra(z1w,idVar="region",freqVar=2, freqVarGroup=1) # same name as input
#'
ProtectKostra <- function(data,idVar = 1, strataVar = NULL,
                          freqVar = 2, freqVarGroup = NULL,
                          protectZeros = TRUE, maxN = 3, method = "SimpleSingle",
                          output="suppressed",
                          total = "Total",
                          split = "_",
                          singleTotal = FALSE,
                          ...){
  CheckInput(idVar, type = "varNrName", data = data, okSeveral = TRUE)
  CheckInput(strataVar, type = "varNrName", data = data, okNULL = TRUE, okSeveral=TRUE)
  CheckInput(freqVar, type = "varNrName", data = data, okSeveral=TRUE)
  CheckInput(freqVarGroup, type = "integer", okSeveral=TRUE, okNULL = TRUE)
  CheckInput(protectZeros, type = "logical")
  CheckInput(maxN, type = "integer", min=0)
  CheckInput(method,type = "character", alt = c("SIMPLEHEURISTIC","Simple","SimpleSingle","OPT", "HITAS", "HYPERCUBE", "Gauss"))
  CheckInput(total, type = "character")
  CheckInput(split, type = "character",  okNULL = TRUE)

  if(!is.null(freqVarGroup) & output=="extraWide")
    stop("extraWide only possible when freqVarGroup is NULL")

  checkGroupTotal = TRUE

  default_stringsAsFactors = default.stringsAsFactors()
  options(stringsAsFactors = FALSE)

  ## Littel "hack" is needed to combine possibility of using GetData()
  ## and at the same time keep original names
  ## In addition
  ##   freqVar is recreated since advanced GetData input allowed
  ##   dimVar is created from idVar and strataVar
  z <- GetData(data=data, id = GD(idVar,MatrixPaste), strata = strataVar,freq=freqVar, ...)

  # Get oriiginal names
  origCols <- attr(z,"origCols")[length(idVar):(length(idVar)+length(strataVar)+length(freqVar))]
  origCols[1] <- attr(z,"origVars")[1]


  idVar = 1
  dimVar = 1 + seq_len(Ncol(z$strata))
  # Get rid of matrix in data.frame
  z$strata=unAsIs(z$strata)
  z$freq=unAsIs(z$freq)

  standardVar = names(z) %in% c("id","strata","freq")
  extraVar = !standardVar
  if(any(extraVar)){
    extraVar[1] <- TRUE  # Include id for matching
    extraData = do.call(data.frame, z[extraVar])
    names(extraData)[1] <- origCols[1]
  } else
    extraData <- NULL
  # Finished creating extra data from ...

  z = do.call(data.frame, z[standardVar])
  freqVar= matlabColon(max(c(dimVar,1))+1,NCOL(z))
  names(z) <- origCols # Original names
  # Finished creating z from data + freqVar, idVar, strataVar

  if(is.null(freqVarGroup)){
    a <- ProtectKostra1(data=z, idVar=idVar, dimVar=dimVar, freqVar=freqVar,
                          protectZeros=protectZeros, maxN=maxN, method =method,
                          output = output, total = total, split = split )
      if(!is.null(extraData)) return(suppressWarnings(CbindIdMatch(a,extraData)))
      options(stringsAsFactors = default_stringsAsFactors)
      return(a)
    }

  if(length(freqVar) != length(freqVarGroup))
    stop("freqVar and freqVarGroup must have same length")

  uniqueGroup = unique(freqVarGroup[freqVarGroup>0])
  uniqueGroup = sort(uniqueGroup)
  n = length(uniqueGroup)
  outputList = vector("list", n+2)
  freqVarNames = names(z[1,freqVar,drop=FALSE])   #  Allow both numbers and names here, but know that freqVAr is number (see above)
  # totalMatrix = NULL
  totalFrame = NULL
  groupTotal = NULL
  if(checkGroupTotal) # Check in separate loop to avoid spending time before error
    for(i in 1:n){
      gr = uniqueGroup[i]
      freqVarGroupgr = freqVarGroup==gr
      freqVari = freqVar[freqVarGroupgr]
      if(sum(freqVarGroupgr)>1 ){
        if(is.null(groupTotal))
          groupTotal         = rowSums(z[,freqVari],na.rm = TRUE)
        else
          if(any(groupTotal != rowSums(z[,freqVari],na.rm = TRUE))){
            if(singleTotal) stop("Not identical rowsums in all groups.")
            warning(paste("Not identical rowsums in all groups. First result named as",total))
          }
      }
    }

  for(i in 1:n){
    gr = uniqueGroup[i]
    freqVarGroupgr = freqVarGroup==gr
    freqVari = freqVar[freqVarGroupgr]

    x = ProtectKostra1(data=z, idVar=idVar, dimVar=dimVar,
                       freqVar=freqVari,
                       protectZeros=protectZeros,
                       maxN=maxN, method =method, output = output, total = total,  split = split )
    if(i==1){
      naFrame = x[,1:2]
      naFrame[,2] = NA
      names(naFrame)[2]=""
    }
    if(NCOL(x)==2) {
      names(x)[2] =  freqVarNames[freqVarGroupgr]  # Rename since different naming in this case
      outputList[[i]] = x
    } else {
      namesIStotal = names(x)==total
      # totalMatrix = cbind(totalMatrix,x[,namesIStotal])
      # totalMatrix = cbind(totalMatrix,matrix(x[,namesIStotal],ncol=1,dimnames=list(NULL,paste(total,gr,sep="_"))))
      if(is.null(totalFrame) & sum(namesIStotal)) {
        namesIStotal[1] = TRUE
        totalFrame = x[,namesIStotal]
      }
      if(singleTotal){
        outputList[[i]] = x[,names(x)!=total,drop=FALSE] # Remove groupTotal to avoid repeating
      } else {
          names(x)[names(x)==total] = paste(total,gr,sep="_")
          outputList[[i]] = x
        }
    }
  }
  # Now first n elements are filled with output from n calls to ProtectKostra1
  # Add to more elements
  outputList[[n+1]] = totalFrame # The groupTotal variable (was removed above: names(x)!=total )
  outputList[[n+2]] = naFrame    # Just NA. Will be last col. Used below: matched[isnamatched] = NCOL(a)
  if(is.null(totalFrame)) outputList[n+1] = NULL # Element will be removed form list

  #if(!is.null(totalMatrix))
  #  if(dim(unique(t(totalMatrix)))[1]>1)
  #    warning(paste(total,"is not unique. First result named as",total))

  a = CbindIdMatch(outputList)
  aNames = names(a)[-NCOL(a)] # remove name of "NA"

  # Complicated to allow mix of numbers and names in idVar and freqVar
  varNames = names(cbind(z[1,idVar,drop=FALSE],z[1,freqVar,drop=FALSE]))

  matched = match(varNames,aNames)  # Columns in output corresponding to input
  oneTOnNames = seq_len(length(aNames))
  notMatched = oneTOnNames[!(oneTOnNames %in% matched)] # Columns in output not in input
  isnamatched = is.na(matched)
  matched[isnamatched] = NCOL(a) # Columns in input not found in output is set to NA
  a = a[,c(matched,notMatched) ,drop=FALSE] # The final data frame
  names(a)[isnamatched] = varNames[isnamatched] # Name the NA columns corretly
  options(stringsAsFactors = default_stringsAsFactors)
  if(!is.null(extraData)) return(suppressWarnings(CbindIdMatch(a,extraData)))
  a
}

## Replace NA by when unique values # NOT IN USE
#FixNA <- function(x,ix=seq_len(NCOL(x))){
#  for(i in ix){
#    uniquex = unique(x[,i])
#    uniquex = uniquex[!is.na(uniquex)]
#    if(length(uniquex)==1)
#      x[,i] = uniquex
#  }
#  x
#}


# Function that fixes problem caused by NCOL(NULL)=1
Ncol <- function(x){
  if(length(x)) return(NCOL(x))
  return(0)
}


ProtectKostra1 <- function(data,idVar = 1, dimVar = 2:NCOL(data), freqVar = NULL,
                           protectZeros = TRUE, maxN = 3, method = "SIMPLEHEURISTIC", output, ...){


  allNA = FALSE

  if(!is.null(freqVar))
    if(!any(!is.na(data[,freqVar])))
      allNA = TRUE

  allNAstatus = NA_character_  # eller "" ?

  allInteger =  !any(!(sapply(data,class)[freqVar]=="integer"))


  if(!allNA)
    a = try(ProtectTable(data=data,dimVar=c(idVar,dimVar),freqVar=freqVar,protectZeros=protectZeros,
                   maxN=maxN,method=method,  singleOutput = FALSE, ...),silent = TRUE)
  else
    a = "tull"  # class(a)=="try-error" is FALSE


  if(class(a)=="try-error"){   # Try setting maxN=0 and protectZeros=FALSE. Freq can be computed correctly.
    warning(paste("ProtectTable caused ",a))
    a = try({
    a =ProtectTable(data=data,dimVar=c(idVar,dimVar),freqVar=freqVar,protectZeros=FALSE,
                         maxN=0,method="SIMPLEHEURISTIC",  singleOutput = FALSE, ...)
    a$suppressed[-1] = NA
    a$sdcStatus[-1] = "e"
    a
    },silent = TRUE)
  }


  if(class(a)=="try-error" | allNA){  # Try changing data. Variables can be created correctly.
    if(allNA)
      warning("All data missing")
    else
      warning(paste("Extra ProtectTable run caused ",a))
    data2 = data
    data2[,freqVar] = 1
    a = try({
    a = ProtectTable(data=data2,dimVar=c(idVar,dimVar),freqVar=freqVar,protectZeros=FALSE,
                     maxN=0,method="SIMPLEHEURISTIC",  singleOutput = FALSE, ...)
    a$suppressed[-1] = NA
    if(allNA)
      a$sdcStatus[-1] = allNAstatus
    else
      a$sdcStatus[-1] = "e"
    a$freq[-1] = NA
    a$freq[match(as.character(data[,idVar]),a$freq[[1]]),match(colnames(data[,freqVar]),names(a$freq))] =  data[,freqVar,drop=FALSE]
    a
    },silent = TRUE)
  }


  if(class(a)=="try-error"){ # Give up using ProtectTable
    warning(paste("Extra ProtectTable run caused ",a))
    a = NULL
    a$freq = data[,c(idVar,freqVar),drop=FALSE]
    a$suppressed = a$freq
    a$suppressed[-1] = NA
    a$sdcStatus = a$suppressed
    if(allNA)
      a$sdcStatus[-1] = allNAstatus
    else
      a$sdcStatus[-1] = "e"
  }

  if(allInteger){
    if(output == "suppressed") return( IntegerDataFrame(a$suppressed, makeWarning = TRUE, allNumeric = TRUE) )
    if(output == "freq")       return( IntegerDataFrame(a$freq,       makeWarning = TRUE, allNumeric = TRUE) )
  }

  if(output == "suppressed") return(a$suppressed)
  if(output == "freq") return(a$freq)
  if(output == "sdcStatus") return(a$sdcStatus)


  if(allInteger){
    a$suppressed = IntegerDataFrame(a$suppressed, makeWarning = TRUE, allNumeric = TRUE)
    a$freq =       IntegerDataFrame(a$freq,       makeWarning = FALSE, allNumeric = TRUE)
  }

  ### Code below is for output="extraWide"

  b = cbind(CharacterDataFrame(data[,c(idVar,dimVar),drop=FALSE]),total=ProtectKostraTotal())
  #print(b)


  bN = NCOL(b)
  freq = vector("list",bN)
  sdcStatus = vector("list",bN)

  bnam = colnames(b)
  for(i in seq_len(bN)) {
    rows = a$freq[,1] %in% unique(b[,i])
    freq[[i]] = a$freq[rows, ,drop=FALSE]
    sdcStatus[[i]] = a$sdcStatus[rows, ,drop=FALSE]
    if(i>1){
      nam = paste(bnam[i],colnames(freq[[i]]),sep="_")
      namsdcStatus = paste(bnam[i],colnames(sdcStatus[[i]]),ProtectKostraSdcStatus(),sep=ProtectKostraSep())
      nam[1] = bnam[i]
      namsdcStatus[1] = bnam[i]
      colnames(freq[[i]]) = nam
    } else{
      namsdcStatus = paste(colnames(sdcStatus[[i]]),ProtectKostraSdcStatus(),sep=ProtectKostraSep())
      namsdcStatus[1] = bnam[i]
    }
    colnames(sdcStatus[[i]]) = namsdcStatus
  }
  CbindIdMatch(c(list(b),freq,sdcStatus))
}


