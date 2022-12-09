#' Extracting a data.frame from a data.frame
#'
#' @param data A data frame
#' @param ... Input specifying how to extract data (see examples).
#' @param removeNULL When TRUE (default) variables specified as NULL are completely removed.
#'                  Otherwise zero column matrices will be embedded. It is possible to specify
#'  removeNULL as a vector - one element for each variable.
#' @param returnAsDataFrame When TRUE (default) a data.frame is returned. Otherwise a list is retuned.
#'
#' @return GetData returns a data frame by default (see details).
#'
#' @details
#' \code{GetData} returns a data frame with extra attributes (see examples).
#'
#' To create data according to id matching:
#' The id variable must be the first variable. This variable must be specified using the list type input.
#' As opposed to other variable the first element of this list must be named an this name must be \code{"id"}.
#' See the examples. (I nyere versjoner trenger det ikke være første variabel??)
#'
#' \code{GetData1} is a single variable variant which returns the variable instead of a data.frame.
#'
#' \code{GetData2me} returns only NULL, but breaks the rules for ordinary functions.
#' That is, each variable is written to caller's environment (no data.frame).
#'
#' @export
#'
#' @examples
#' ### Example data
#' z <- data.frame(aar = c(2014, 2015, 2016),
#'                 ola = c(4.4, 6.6, 2.2, 3.2, 8.8, 9.9),
#'                 kari = 10 * (1:6),
#'                 tull = c("A", "A", "B", "B", "C", "C"))
#' print(z)
#'
#' ### Ordinary use: names or numbers
#' GetData(z, x = "kari", y = "ola")
#' GetData(z, A = 3, B  = 2, C = 1)
#'
#' ### With matrix embedded in one variable
#' a = GetData(z, x = c("kari","ola"), y = "aar")
#' print(a)
#' print(as.list(a)[-99]) # 99 tric to avoid printing of attributes
#'
#' ### Looking at attributes stored in output
#' attr(a,"origVars") # Original names corresponding to variables
#' attr(a,"origCols") # Original names corresponding to columns
#'
#' ### Using a named list to specify equality
#' GetData(z, x = list("kari",aar=2014), y = list("ola", aar=2015))
#' GetData(z, x = list("kari",aar=2016, tull="B"),  y = list("kari",aar=2014, tull="B"))
#'
#' ### With matrix input to obtain matrix embedded in output
#' a = GetData(z, x = list("kari",aar=t(c(2014,2015))), y = list("ola", aar=t(2015:2016)))
#' print(a);
#' print(as.list(a)[-99])
#' GetData(z, x = list("kari",aar=t(1:3000))) # Impossible values ignored, warning produced
#'
#' ### Effect of removeNULL
#' a = GetData(z, x = NULL, y = "ola")
#' print(a);
#' print(as.list(a)[-99])
#' a = GetData(z, x = NULL, y = "ola", removeNULL = FALSE)
#' print(a);
#' print(as.list(a)[-99])  # x is a 6x0 matrix
#'
#' ### Using "expression"
#' GetData(z, x = list("kari",expression(aar>2014)), y = list("kari", expression(tull != "B")))
#' GetData(z, x = list("kari",expression(aar>2014 & tull=="B" | tull=="C" )))
#' GetData(z, x = list("kari",expression(aar==min(aar))), y = list("kari", expression(aar==max(aar)-1)))
#'
#' ### Using names as list elements instead of named list
#' GetData(z, x = list("kari","aar", "2014"), y = list("ola", "aar", "2015"))
#' GetData(z, x = list("kari","aar", "2016", "tull", "B"))
#'
#' ### Using function to be run on each variable
#' GetData(z, x = list("kari",aar=2014:2015,function(x)(x+1))) # One function
#' GetData(z, x = list("kari",aar=2014:2015,function(x)(x+1),function(x)(x*10))) # Tow functions
#' GetData(z, x = list(c("kari","ola"),function(x)apply(x,1,paste,collapse="-")), y = "aar")
#'
#' ### Advanced examples
#' GetData(z, x = list(c("kari","ola"),aar=t(2014:2015)), y = list("ola", aar=2015))
#' GetData(z, x = list("kari",expression(aar==max(aar)),tull=t(c("B","C"))))
#' GetData(z, x = list("kari",expression(eval(as.symbol("aar"))>2014 & eval(as.symbol("tull"))=="B")))
#' GetData(z, x = list("ola",aar=cbind(2014:2015,2015:2016)))
#' GetData(z, x = list("kari",aar=2014:2015,function(x)(cbind(a=x,b=1000))))
#' GetData(z, x = list("kari",aar=2014:2015,function(x)(cbind(x=x,tid=date()))))
#'
#' ### GetData1
#' aAa <- GetData1(z, "kari")
#' bBb <- GetData1(z, x = c("kari","ola"))
#'
#' ### GetData2me
#' GetData2me(z, cCc = "kari", dDd = "ola")
#' cCc + dDd
#' GetData2me(z, eEe = list(c("kari","ola"),aar=t(2014:2015)))
#' print(eEe)
#'
#' ######  Using id  #######
#'
#' #### Make new example data
#' z2 <- rbind(z,z)
#' z2$ola <- c(z$ola,2*z$ola)
#' z2 <- SortRows(z2)[1:11,]
#' rownames(z2) <- NULL
#' z2$ID=c(1:3,4,1:3,5,1:2,6)
#' print(z2)
#'
#' # All possible ID-values in data
#' GetData(z2, iD = list(id="ID"), x = list("kari",aar=2014), y = list("ola", aar=2015))
#'
#' # ID-values in union of 2014 and 2015
#' GetData(z2, iD = list(id="ID",aar=c(2014,2015)), x = list("kari",aar=2014), y = list("ola", aar=2015))
#'
#' # ID-values in intersection of 2014 and 2015
#' # (matrix input similar to above but no matrix in output, instead intersection "of columns" created)
#' GetData(z2, iD = list(id="ID",aar=t(c(2014,2015))), x = list("kari",aar=2014), y = list("ola", aar=2015))
#'
#' # Only ID-values in 2016
#' GetData(z2, iD = list(id="ID",aar=2016), x = list("kari",aar=2014), y = list("ola", aar=2015))
#'
#' # ID-values in 2016 +intersection of 2014 and 2015
#' GetData(z2, iD = list(id="ID",aar=cbind(c(2014,2016),c(2015,2016))), x = list("kari",aar=2014), y = list("ola", aar=2015))
#'
#' # Only first value used hven multiple id
#' GetData(z2, iD = list(id="ID"), x = "kari", y = "ola")
#'
#' # Construct a single id from two variables
#' GetData(z2, iD = list(id=c("tull", "ID")), x = "kari", y = "ola")
#'
#'
GetData <- function(data, ..., removeNULL=TRUE,
                    returnAsDataFrame=TRUE) {
  stringsAsFactors <- FALSE
  default_stringsAsFactors <- getOption("stringsAsFactors", default = FALSE)  # default.stringsAsFactors()
  
  if (default_stringsAsFactors) {
    options(stringsAsFactors = stringsAsFactors)
  }
  
  a <- list(...)
  aInput <- a

  test=FALSE  # Kan brukes for å sjekke at gammel og ny kode gir det samme .....

  #if (!is.list(a[[1]]))
  for(i in seq_len(length(a))) a[[i]] = AsList(a[[i]])

  aNULL <- sapply(a, function(x) is.null(x[[1]]))
  aNULL <- removeNULL & aNULL
  if(any(aNULL)) a <- a[!aNULL]

  for(i in seq_len(length(a)))
    a[[i]] = FixNoNamed(a[[i]])

  n = length(a)
  z = vector("list",n)
  names(z) = names(a)

  useId = 0

  #print(is.null(names(names(a[[1]]))))

  if(!is.null(names(a[[1]])))
    if(!is.na(names(a[[1]])[1]))
      if(names(a[[1]])[1]=="id")
       useId = 1

  if(useId){
   idAll = data[,a[[useId]][[1]]]
   if(NCOL(idAll) != 1) idAll <- MatrixPaste(idAll)  # slår sammen
   id  = GetDinterUnique(idAll,GetRows(data,a[[useId]][-1]))
  }
  else{
    idAll = NULL
    id = NULL
  }

  if(test) useId=TRUE

  for(i in seq_len(n)){
    # z[[i]] = Im(GetD(data[,a[[i]][[1]]],GetRows(data,a[[i]][-1])))
    if(useId)
	   if(is.numeric(useId) & useId==i) zi = id   ## is.numeric pga mulig test over
      else zi = Im(GetDNr(data[,a[[i]][[1]]],RowsForId(GetRows(data,a[[i]][-1]),idAll,id)))
    else
      zi = Im(GetD(data[,a[[i]][[1]]],GetRows(data,a[[i]][-1])))
    z[[i]] = RunFunctions(zi,a[[i]][-1])
  }
  if(returnAsDataFrame)
    z = as.data.frame(z)
  a1 <- lapply(a, function(x) x[[1]])
  namesData = names(data)
  for(i in seq_len(length(a1)))
    if(is.numeric(a1[[i]])) a1[[i]] = namesData[a1[[i]]]
  #attr(z,"origin")   <- aInput
  attr(z,"origVars") <- namesVars(a1)
  attr(z,"origCols") <- namesCols(a1)
  
  if (default_stringsAsFactors) {
    options(stringsAsFactors = default_stringsAsFactors)
  }
  
  z
}



#' @rdname GetData
#' @export
GetData1 <- function(...){
  z = GetData(...)
  if(length(z)!=1) stop("GetData1 only for a single variable")
  unAsIs(z[[1]])
}

#' @rdname GetData
#' @export
GetData2me <- function(...){
  z = as.list(GetData(...))
  for(i in seq_len(length(z))) z[[i]] = unAsIs(z[[i]])
  list2env(z,envir=parent.frame())
  NULL
}


namesCols <- function(a){
  unlist(a)
}

namesVars <- function(a){
  for(i in seq_len(length(a))){
    if(length(a[[i]])>1) a[[i]] = paste(HeadEnd(a[[i]],n=11L),collapse="_")
    if(is.null(a[[i]])) a[[i]] = NA
  }
  unlist(a)
}

FixNoNamed <- function(x){
  if(length(x)<3) return(x)
  isCharacter = sapply(x,is.character)
  if(is.null(names(x))) noName = rep(TRUE,length(x))
  else noName = (names(x)=="" | is.na(names(x)))
  i = 2
  keep = rep(TRUE,length(x))
  while(i < length(x)){
    if(isCharacter[i] & noName[i] & noName[i+1]){
      keep[i] = FALSE
      names(x)[i+1] = x[[i]]
      i = i+1
    }
    i = i+1
  }
  if(!any(!keep)) return(x)
  x[keep]
}

RunFunctions <- function(x,aList){
  for(i in seq_len(length(aList)))
    if(is.function(aList[[i]])) x = aList[[i]](x)
  x
}

Im <- function(x){
  if(is.matrix(x)) return(I(x))
  x
}


GetRows <- function(data,vList=NULL){
  n = length(vList)
  if(n==0)
    return(rep(TRUE,NROW(data)))
  x = NULL
  vNames = names(vList)
  for(i in seq_len(n))
    x = CombiRows(x,GetRows1(data,vNames[i],vList[[i]]))
  x
}

GetRows1 <- function(data,variable=NULL,values=NULL){
  if(is.expression(values)) return(with(data,eval(values)))  #### Obs her
  if(is.null(values) | is.null(variable) | is.function(values)) # function ignored here
    return(rep(TRUE,NROW(data)))
  if(is.vector(values)) return(data[,variable] %in% values)
  if(!is.matrix(values)) stop("Input must be vector or matrix")
  cs = colSums(matrix(values %in% data[,variable],NROW(values)))>0

  if(any(!cs)){
    values = values[,cs, drop=FALSE]
    warning("Empty dropped")
  }

  nRow = NROW(data)
  nCol = NCOL(values)
  x = matrix(FALSE,nRow,nCol)
  colnames(x) = seq_len(nCol)
  for(i in seq_len(nCol)){
      x[,i] = data[,variable] %in% values[,i]
      colnames(x)[i] = paste(values[,i],collapse="_")
  }
  x
}

CombiRows <- function(a,b){
  if(is.null(a)) return(b)
  if(is.null(b)) return(a)
  if(is.vector(a)|is.vector(b))
    return( a & b)
  nCola = NCOL(a)
  nColb = NCOL(b)
  m = matrix(a,NROW(a),nCola*nColb)
  colnames(m) = seq_len(nCola*nColb)
  for(i in seq_len(nColb)){
    m[,(i-1)*nCola + seq_len(nCola)] = a & b[,i]
    colnames(m)[(i-1)*nCola + seq_len(nCola)] = paste(colnames(a),colnames(b)[i],sep="_")
  }
  m
}



GetDinterUnique <- function(x,m){
  if(NCOL(x)!=1) stop("A single column needed")
  nCol = NCOL(m)
  if(nCol<=1) return(unique(x[m]))
  a = unique(x[m[,1]])
  for(i in matlabColon(2,nCol)){
    a = c(a,unique(x[m[,i]]))
    a = a[duplicated(a)]
  }
  a
}



GetD <- function(x,m){
  nColx <- NCOL(x)
  if(is.vector(m)){
    #if(is.vector(x)) return(x[m])
    if(nColx==1) return(x[m])
    return(as.matrix(x[m, ,drop=FALSE]))
  }
  if(nColx==1) # if(is.vector(x))
    return(GetMatrix(x,m))
  z = NULL
  for(i in seq_len(nColx)){
    zi = GetMatrix(x[,i],m)
    colnames(zi) = paste(colnames(x)[i],colnames(zi),sep="_")
    z = cbind(z,zi)
  }
  as.matrix(z)
}

GetMatrix <- function(x,m){
  if(length(x) != NROW(m))            stop("Cannot create matrix - NROW() error")
  nRow <- unique(colSums(m))
  if(length(nRow) != 1)               stop("Cannot create matrix - nRow error")
  x = matrix(x[row(m)[m]],nRow)
  colnames(x) = colnames(m)
  x
}

# As GetD but integer instead of logical
GetDNr <- function(x,m){
  nColx <- NCOL(x)
  if(is.vector(m)){
    if(nColx==1) return(x[m])
    return(as.matrix(x[m, ,drop=FALSE]))
  }
  if(nColx==1)
    return(GetMatrixNr(x,m))
  z = NULL
  for(i in seq_len(nColx)){
    zi = GetMatrixNr(x[,i],m)
    colnames(zi) = paste(colnames(x)[i],colnames(zi),sep="_")
    z = cbind(z,zi)
  }
  as.matrix(z)
}

# As GetMatrix but integer instead of logical
GetMatrixNr <- function(x,m){
  nRow <- NROW(m)
  x = matrix(x[m],nRow)
  colnames(x) = colnames(m)
  x
}


RowsForId <- function(rows,idALL=NULL,id=NULL){
  if(is.matrix(rows)){
    nCol <- NCOL(rows)
    col1 <- RowsForId(rows[,1],idALL,id)
    m = matrix(col1,nrow=length(col1),ncol=nCol)
    colnames(m) = colnames(rows)
    for(i in matlabColon(2,nCol))
      m[,i] <- RowsForId(rows[,i],idALL,id)
    return(m)
  }
  if(is.null(idALL) | is.null(id))
    return(seq_len(length(rows))[rows])

  (seq_len(length(rows))[rows])[match(id,idALL[rows])]
}






