#' Imputation of a several variables (y's) by a regression model using a single explanatory variable (x).
#'
#' Impute missing and wrong values (group 3) by the model based on representative data (group 1).
#' Some data are considered correct but not representative (group 2). This grouping is common for all y-variables.
#'
#' @encoding UTF8
#'
#' @param data Input data set of class data.frame
#'
#' @param idName Name of id-variable(s)
#' @param strataName Name of starta-variable. Single strata when NULL (default)
#' @param xName  Name of x-variable
#' @param yNames Names of y-variables
#' @param ySelect Indices of yNames to extract a single component from
#' @param methodOneComp Method used to extract a single component coded as a string: "mean" (default),"pca","pcaMedian" or "pcaStd"
#' @param method The method (model and weight) coded as a string: "ordinary" (default), "ratio", "noconstant",
#'        "mean" or "ratioconstant".
#' @param limitModel  Studentized residuals limit. Above limit -> group 2.
#' @param limitIterate Studentized residuals limit for iterative calculation of studentized residuals.
#' @param limitImpute  Studentized residuals limit. Above limit -> group 3.
#' @param returnSameType When TRUE (default) and when the type of input y variables is integer, the output type of
#'          imputations and estimates from the final run is also integer.
#'          Those estimates/sums are then calculated from rounded imputed values.
#'
#'
#' @details Multivariate imputations are performed by running an imputation model within each strata.
#'          Thus, the three groups (category123) are the same for all y-variables.
#'          Division into the three groups are based on studentized residuals from a inititial run with a single variable.
#'          Calculations of studentized residuals
#'          are performed by iterativily throwing out observations from the model fitting.
#'          The single initial variable can be a original variable (ySelect is a single number) or a component extracted
#'          according to  methodOneComp.
#'
#'          Calculations of studentized residuals
#'          are performed by iterativily throwing out observations from the model fitting.
#'
#'          Missing x-values are not allowed in this version.
#'
#'
#' @return Output of the alternative variants of the function
#' (Tall, Wide, Small) are constructed similar
#' to the variants of \code{\link{ImputeRegression}}.
#'
#' Output of \strong{\code{ImputeRegressionMulti}} and \strong{\code{ImputeRegressionMultiNewNames}} (using the names after
#'         \code{\emph{or}}  below) is a list where the first three elements are ouput from the initial run with a single variable:
#'         micro has as many rows as input, aggregates has one row for each strata
#'         and total has a single row. The individual variables of these three elements are:
#'
#' \strong{\code{micro}} consists of the following elements:
#'    \item{id}{id from input}
#'    \item{x}{The input x variable}
#'    \item{y}{The input y variable}
#'    \item{strata}{The input strata variable (can be NULL)}
#'    \item{category123}{The three imputation groups: representative (1), correct but not representative (2), wrong (3). }
#'    \item{yHat \emph{or estimateYHat}}{Fitted values}
#'    \item{yImputed \emph{or estimate}}{Imputed y-data}
#'    \item{rStud}{The final studentized residuals}
#'    \item{dffits}{The final DFFITS statistic}
#'    \item{hii}{The final leverages (diagonal elements of hat matrix)}
#'    \item{leaveOutResid}{The final outside-model residual}
#'
#' \strong{\code{aggregates}} consists of the following elements:
#'    \item{N}{Number of observations in each strata}
#'    \item{nImputed}{Number of imputed observations in each strata}
#'    \item{estimate}{Total estimates from imputed data}
#'    \item{cv}{Coefficient of variation = seEstimate/estimate}
#'    \item{estimateYhat}{Totale estimate based on model fits}
#'    \item{estimateOrig \emph{or y}}{Estimate based on original data with missing set to zero}
#'    \item{coef}{The final first model coefficient}
#'    \item{coefB}{The final second model coefficient or zeros when only one coefficient in model.}
#'    \item{n}{The final number of observations in model.}
#'    \item{sigmaHat}{The final square root of the estimated variance parameter}
#'    \item{seEstimate}{The final standard error estimate of the total estimate from imputed data}
#'    \item{seRobust}{Robust variant of seEstimate (experimental)}
#'
#' \strong{\code{total}} consists of the following elements:
#'    \item{Ntotal \emph{or N}}{Number of observations}
#'    \item{nImputedTotal \emph{or nImputed}}{Total number of imputed observations}
#'    \item{estimateTotal \emph{or estimate}}{Total estimate for all strata}
#'    \item{cvTotal or \emph{cv}}{Total cv for all strata}
#'
#'    The other output elements are from the final run with all y-variables. These elements are:
#'    \item{\strong{MyImputed}}{Matrix of imputed y-data}
#'    \item{\strong{Mestimate}}{Matrix of total estimates from imputed data}
#'    \item{\strong{Mcv}}{Matrix of coefficient of variation = seEstimate/estimate}
#'    \item{\strong{MestimateTotal}}{Matrix of total estimates for all strata (a single row)}
#'    \item{\strong{McvTotal}}{Matrix of total cvs for all strata (a single row)}
#'
#'
#' @export
#'
#' @examples
#'
#' z=KostraData("ratioTest")
#' z2=cbind(id=10*(1:NROW(z)),z[,c(3,1,2)],y2=z$y+z$x)
#' ImputeRegressionMulti(z2,strataName="k",method="ratio")
#' ImputeRegressionMultiNewNames(z2,strataName="k")
#' ImputeRegressionMultiTall(z2,strataName="k")
#' ImputeRegressionMultiTallSmall(z2,strataName="k")
#' ImputeRegressionMultiWide(z2,strataName="k")
#' ImputeRegressionMultiWideSmall(z2,strataName="k")
#'
#' rateData <- KostraData("rateData")               # Real Kostra data set
#' w <- rateData$data[, c(17,19,16,5:10)]           # Data with id, strata, x and many ys
#' w <- w[is.finite(w[,"Ny.kostragruppe"]), ]       # Remove Longyearbyen
#' w[w[,"Ny.kostragruppe"]>13,"Ny.kostragruppe"]=13 # Combine small strata
#' ImputeRegressionMulti(w, strataName = names(w)[2])
#' names(w)[4:9] = paste("y",1:6,sep="")           # rename for nicer output
#' ImputeRegressionMulti(w, strataName = names(w)[2], method="ratio")
#' ImputeRegressionMulti(w, method="ratioconstant") # No strata
ImputeRegressionMulti <- function(data,
                                  idName = names(data)[1],
                                  strataName = NULL,
                                  xName = names(data)[3],
                                   yNames = names(data)[4:NCOL(data)],
                                ySelect = 1:length(yNames),
                             methodOneComp = "mean",
                             method = "ordinary",
                             limitModel = 2.5, limitIterate = 4.5, limitImpute = 50,
                             returnSameType = TRUE) {
  CheckInput(idName, type = "varName", data = data)
  CheckInput(strataName,   type = "varName", data = data, okNULL = TRUE)
  CheckInput(xName,        type = "varName", data = data)
  CheckInput(yNames,       type = "varName", data = data, okSeveral = TRUE)
  CheckInput(ySelect,      type = "integer", min = 1, max = length(yNames), okSeveral = TRUE)
  CheckInput(methodOneComp,type = "character", alt =  c("mean", "pca", "pcaMedian", "pcaStd"))
  CheckInput(method,       type = "character", alt = c("ordinary","ratio", "noconstant", "mean","ratioconstant"))
  CheckInput(limitModel,   type = "numeric", min = 0)
  CheckInput(limitIterate, type = "numeric", min = 0)
  CheckInput(limitImpute,  type = "numeric", min = 0)
  CheckInput(returnSameType,  type = "logical")
  #CheckInput(sepId, type = "character", okNULL = TRUE)


  if(method=="ordinary")     {xModel = "x";   weights = NULL}
  if(method=="ratio")        {xModel = "x-1"; weights = "1/x"}
  if(method=="noconstant")   {xModel = "x-1"; weights = NULL}
  if(method=="mean")         {xModel = "1";   weights = NULL;  xName = NULL}
  if(method=="ratioconstant"){xModel = "x";   weights = "1/x"}


  yModel=c("y","yMany")
  BackTransform = NULL

  #if(method=="nulltest")     {xModel = "0";   weights = NULL;
  #yModel=c("I(y-NaToZero(x))","I(yMany-NaToZero(x))")
  #BackTransform = function(y){
  #  return(y+NaToZero(dynGet("data")$x))
  #}
  #}

  returnIter  = FALSE
  returnYHat  = TRUE

  xLimits = c(-Inf, Inf)
  yLimits = c(-Inf, Inf)
  allowMissingX = FALSE

  if(method=="ratio" | method=="ratioconstant" ) xLimits = c(1E-200, Inf)
  ##if(method=="ratio")   yLimits = c(0, Inf)


  if(FALSE){ # Kode før GetData
  z = GetIdxyStrataMany(data=data,idName = idName, xName = xName, yNames = yNames, strataName = strataName,
                                ySelect = ySelect,
                                methodOneComp = methodOneComp,
                                sepId="_")
  }
  if(TRUE){
  z <- GetData(data=data, id = GD(idName,MatrixPaste), x = GD(xName,FixEmpty), yMany = yNames, strata = GD(strataName, MatrixPaste1),
                     removeNULL = FALSE) #removeNULL = c(TRUE,TRUE,TRUE,FALSE))
  z$y <- OneComp(z$yMany, ySelect,method=methodOneComp)
  }

  z$x     = CheckNumeric(z$x , minLimit = xLimits[1], maxLimit = xLimits[2], setNA = FALSE, allowMissing = allowMissingX, varName = "x")
  z$y     = CheckNumeric(z$y , minLimit = yLimits[1], maxLimit = yLimits[2], varName = "single y")
  z$yMany = CheckNumeric(z$yMany , minLimit = yLimits[1], maxLimit = yLimits[2], varName = "multiple y")

  #return(z)

  a=StrataApply(z,"strata",copyVar=c("id","x","y"),
             LmImputeOne2Many, multiUnlist=TRUE,
             FunTotal=MakeTotal,FunMultiTotal=MakeTotalmulti,
             returnLast = FALSE, returnFinal = TRUE, keepMulti=c("yImputed","estimate","cv"),unfoldCoef=2,
             yModel = yModel, BackTransform = BackTransform,
             xModel = xModel, weights = weights,
             limitModel = limitModel, limitIterate = limitIterate, limitImpute=limitImpute, returnIter=returnIter,
             returnYHat =  returnYHat,
             returnSameType = returnSameType)
  a
}


#' @rdname ImputeRegressionMulti
#' @export
ImputeRegressionMultiNewNames <- function(...){
  ImputeRegressionNewNames(..., Fun = ImputeRegressionMulti)
}



#' @rdname ImputeRegressionMulti
#' @export
#' @importFrom SSBtools RbindAll CbindIdMatch
ImputeRegressionMultiTall <- function(..., iD=TalliD()){
  a <- ImputeRegressionMultiNewNames(...,iD = iD)
  a$micro <- cbind(a$micro,N=1,nImputed= as.numeric(a$micro$category123==3))
  RbindAll(
    CbindIdMatch(a$micro,a$MyImputed , addName = c("","Mestimate"), sep = "_", idNames = NULL),
    CbindIdMatch(a$aggregates,a$Mestimate,a$Mcv, addName = c("","Mestimate","Mcv"), sep = "_", idNames = NULL),
    CbindIdMatch(a$total,a$MestimateTotal,a$McvTotal, addName = c("","Mestimate","Mcv"), sep = "_", idNames = NULL))
}

#' @rdname ImputeRegressionMulti
#' @export
#' @importFrom SSBtools RbindAll
ImputeRegressionMultiTallSmall <- function(...,
                                      iD=TalliD(),
                                      keep=c("ID","nImputed")){
  a <- ImputeRegressionMultiNewNames(...,iD = iD, keep = c(keep,"category123"))
  a$micro$category123 <- as.numeric(a$micro$category123==3)
  names(a$micro)[match("category123",names(a$micro))] <- "nImputed"
  RbindAll(
    CbindIdMatch(a$micro,a$MyImputed , addName = c("",""), sep = "_", idNames = NULL),
    CbindIdMatch(a$aggregates,a$Mestimate,a$Mcv, addName = c("","","cv"), sep = "_", idNames = NULL),
    CbindIdMatch(a$total,a$MestimateTotal,a$McvTotal, addName = c("","","cv"), sep = "_", idNames = NULL))
}


#' @rdname ImputeRegressionMulti
#' @export
#' @importFrom SSBtools CbindIdMatch
ImputeRegressionMultiWide <- function(...,addName=WideAddName(), sep = WideSep(),
                                 idNames=c("","strata",""), addLast = FALSE){
  a <- ImputeRegressionMultiNewNames(...)
  CbindIdMatch(
    CbindIdMatch(a$micro,a$MyImputed , addName = c("","Mestimate"), sep =  WideSep2(), idNames = NULL),
    CbindIdMatch(a$aggregates,a$Mestimate,a$Mcv, addName = c("","Mestimate","Mcv"), sep =  WideSep2(), idNames = NULL),
    CbindIdMatch(a$total,a$MestimateTotal,a$McvTotal, addName = c("","Mestimate","Mcv"), sep =  WideSep2(), idNames = NULL),
    addName=addName,  idNames = idNames, sep = sep,  addLast = addLast)
}


#' @rdname ImputeRegressionMulti
#' @export
#' @importFrom SSBtools CbindIdMatch
ImputeRegressionMultiWideSmall <- function(..., keep=c("id","strata","nImputed"),
                                      addName=WideAddName(), sep = WideSep(),
                                      idNames=c("","strata",""), addLast = FALSE){
  a <- ImputeRegressionMultiNewNames(..., keep=keep)
  CbindIdMatch(
    CbindIdMatch(a$micro,a$MyImputed , addName = c("",""), sep =  WideSep2(), idNames = NULL),
    CbindIdMatch(a$aggregates,a$Mestimate,a$Mcv, addName = c("","","cv"), sep =  WideSep2(), idNames = NULL),
    CbindIdMatch(a$total,a$MestimateTotal,a$McvTotal, addName = c("","","cv"), sep =  WideSep2(), idNames = NULL),
    addName=addName, idNames = idNames, sep = sep,  addLast = addLast)
}



#' INTERNAL FUNCTION: Check values of a numeric vector or matrix according to limits.
#'
#' Possibly changed data is retuned
#'
#' @param x  numeric vector or matrix
#' @param minLimit lower limit
#' @param maxLimit upper limit
#' @param setNA When TRUE values outside limits are set to NA instead of error
#' @param allowMissing When FALSE missing values cause error
#' @param allowInf When FALSE -Inf and Inf always considered outside limits
#' @param varName String used in warning or error message
#'
#' @return x is retuned, possibly changed
#' @export
#'
#' @examples
#' CheckNumeric(-1:10,minLimit=0,varName="Hello")
CheckNumeric <- function(x,minLimit=-Inf,maxLimit=Inf,setNA=TRUE,allowMissing=TRUE,allowInf=FALSE,varName="Variable"){
  if(is.null(x)) return(x)
  if(!allowMissing)
    if(sum(is.na(x))) stop(paste(varName,"must be non-missing"))
  rangex <- range(x,na.rm = TRUE)
  if ((!allowInf & !is.finite(sum(rangex))) | rangex[1] < minLimit | rangex[2] > maxLimit){
    if(setNA){
      x[x<minLimit | x>maxLimit | (!allowInf & !is.finite(x))] = NA
      warning(paste(varName,"outside limits set to NA"))
    } else stop(paste(varName,"outside limits"))
  }
  x
}




# Special variant. Takes multiple y as input
# A single x is created based on first non-missing value in each row


GetIdxyStrataMany_IKKEiBRUK <- function(data,idName = NULL, xName = "x", yNames = "y", strataName = "strata",
                              ySelect = 1:length(yNames),
                              methodOneComp = "pcaMedian",
                              sepId="_",xNULL = 0) {
  if(is.null(idName)) id = seq_len(NROW(data))
  else{
    if(length(idName)==1) id = data[ ,idName]
    else id=apply(data[ ,idName,drop=FALSE] , 1 , paste , collapse = sepId )
  }

  if(is.null(strataName)) strata=1
  else strata = data[ ,strataName]


  yInd = match(yNames,colnames(data))

  if(is.null(xName) & !is.null(xNULL)) x=xNULL
  else x = data[ ,xName]

  data.frame(
   id = id,
   strata = strata,
    x = x,
    y = OneComp(data,yInd[ySelect],method=methodOneComp),
    yMany = I(as.matrix(data[ ,yInd,drop=FALSE]))
  )
}

#' INTERNAL FUNCTION: Extract a single component from multivariate data
#'
#' Rows with any missing elements give corresponding missing values in output.
#'
#' @param data Data frame or matrix containing the variables
#' @param variables  Indices of variables to extract component from
#' @param method Method coded as a string: "mean" (default),"pca","pcaMedian" or "pcaStd" where:
#' "mean" means rowMeans.
#' "pcaMedian" means pca/svd on data scaled by median of absolute values.
#' "pcaStd" means pca/svd on data centered and scaled by st.dev.
#'
#' @return A single component is retuned as a vector. This component is scaled to be a weighted
#'         row-sum of input so that sum(abs(weights))=1.
#' @export
#' @importFrom stats median
#'
#' @examples
#'  OneComp(matrix(rnorm(15),5,3),method="pca")
OneComp <- function(data,variables=1:dim(data)[2],method=c("mean","pca","pcaMedian","pcaStd")){
    method <- match.arg(method)
    z=rep(NaN,dim(data)[1])
    rows <- !IsNaMulti(data)
    x <- as.matrix(data[rows,variables,drop=FALSE])
    if(dim(x)[2]==1){
      z[rows]=x
      return(z)
    }
    if(method=="mean"){
      z[rows] = rowMeans(x)
      return(z)
    }
    #a= rep(0,dim(x)[2])
    b= rep(1,dim(x)[2])
    x0 = x
    if(method=="pcaMedian"){
      x = NaToZero(scale(x, center = FALSE, scale = apply(abs(x),2,median))) # median of abs
      b = attr(x,"scaled:scale")
    }
    if(method=="pcaStd"){
      x = NaToZero(scale(x, center = TRUE, scale = TRUE)) # NaToZero to handle std=0
      #a = attr(x,"scaled:center")
      b = attr(x,"scaled:scale")
    }

    svdx <- svd(x, nu = 1, nv = 1)
    sumvd <- sum(abs(svdx$v)) / svdx$d[1]


    # Now svdx$u/sumvd  = x %*% (svdx$v/svdx$d[1])/sumvd

    s = sign(sum(svdx$v))
    w = (svdx$v/svdx$d[1])/sumvd    # weights transformed data
    w1=NaToZero(w/b)                # weights untransformed data
    w1=t(t(w1/sum(abs(w1))))        # as matrix

    pc1 = x0 %*% w1*s

    # Check use of w1 and x0 is ok
    # pc1a = svdx$u/sumvd   # u scaled
    # pc1b = scale(x0, center = TRUE, scale = FALSE) %*% w1*s # as pc but center
    # print(pc1a/pc1)   # Equal values when pca and pcaMedian
    # print(pc1a/pc1b)  # Equal values when pcaSyd

    z[rows] = pc1
    z
}

MakeTotal = function(x){
  total = NULL
  if(!is.null(x$N)) total$Ntotal <- sum(x$N)
  if(!is.null(x$nImputed)) total$nImputedTotal <- sum(x$nImputed)
  if(!is.null(x$AnImputed)) total$AnImputedTotal <- sum(x$AnImputed) # Needed in Imputeregression2
  if(!is.null(x$BnImputed)) total$BnImputedTotal <- sum(x$BnImputed) # ------------ // ---------
  if(!is.null(x$estimate)) total$estimateTotal <- sum(x$estimate)
  if(!is.null(x$cv)) total$cvTotal <- sqrt(sum((x$cv*x$estimate)^2))/total$estimateTotal
  data.frame(total)
}

MakeTotalmulti = function(x){
  total = NULL
  if(!is.null(x$Mestimate)) total$MestimateTotal <- t(colSumsSameType(x$Mestimate))
  if(!is.null(x$Mcv)) total$McvTotal <- sqrt(t(colSums((x$Mcv*x$Mestimate)^2)))/total$MestimateTotal
  total
}






