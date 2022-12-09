# Endret versjon av ProtectKostra (uten ProtectTable/sdctable) som brukes til å beregne summer i
# RoundKostra etter samme mønster som ProtectKostra.
#
ProtectKostraForRound <- function(data,idVar = 1, strataVar = NULL,
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
  CheckInput(method,type = "character", alt = c("SIMPLEHEURISTIC","Simple","SimpleSingle","OPT", "HITAS", "HYPERCUBE"))
  CheckInput(total, type = "character")
  CheckInput(split, type = "character",  okNULL = TRUE)

  if(!is.null(freqVarGroup) & output=="extraWide")
    stop("extraWide only possible when freqVarGroup is NULL")

  checkGroupTotal = TRUE

  default_stringsAsFactors <- getOption("stringsAsFactors", default = FALSE)  #default.stringsAsFactors()
  
  if (default_stringsAsFactors) {
    options(stringsAsFactors = FALSE)
  }

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
    a <- RoundKostra1(data=z, idVar=idVar, dimVar=dimVar, freqVar=freqVar,
                          protectZeros=protectZeros, maxN=maxN, method =method,
                          output = output, total = total, split = split )
      if(!is.null(extraData)) return(suppressWarnings(CbindIdMatch(a,extraData)))
    
      if (default_stringsAsFactors) {
        options(stringsAsFactors = default_stringsAsFactors)
      }
    
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
          groupTotal         = rowSums(z[,freqVari])
        else
          if(any(groupTotal != rowSums(z[,freqVari]))){
            if(singleTotal) stop("Not identical rowsums in all groups.")
            warning(paste("Not identical rowsums in all groups. First result named as",total))
          }
      }
    }

  for(i in 1:n){
    gr = uniqueGroup[i]
    freqVarGroupgr = freqVarGroup==gr
    freqVari = freqVar[freqVarGroupgr]

    x = RoundKostra1(data=z, idVar=idVar, dimVar=dimVar,
                       freqVar=freqVari,
                       protectZeros=protectZeros,
                       maxN=maxN, method =method, output = output, total = total,  split = split )
    if(i==1){
      naFrame = x[,1:2]
      naFrame[,2] = as.integer(NA)
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
  
  if (default_stringsAsFactors) {
    options(stringsAsFactors = default_stringsAsFactors)
  }
  
  if(!is.null(extraData)) return(suppressWarnings(CbindIdMatch(a,extraData)))
  a
}



