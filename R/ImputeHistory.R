#' Imputation of a single variable (y) by the most recent available historical value
#'
#' A single x variable is created so that each element is the most resent non-missing historical value.
#' Missing y-values are imputed directly by this x without any model. Standard error estimates are based on
#' the naive model where (y-x) is assumed to be pure error.
#'
#' @encoding UTF8
#
#' @param data Input data set of class data.frame
#'
#' @param idName Name of id-variable(s)
#' @param strataName Name of starta-variable. Single strata when NULL (default)
#' @param xName Name of variables with historical y-value(s) (most resent first). Can be set to NULL (see yName).
#' @param yName Name of y-variable. When xName is NULL yName is a vector of current and historical variables (most resent first).
#' @param weightMethod The weight method for error calculations coded as a string: "ordinary" (default) or "ratio".
#' @param reverse When TRUE most resent is last instead of first (see xName and yName). Default is FALSE.
#' @param returnSameType When TRUE (default) and when the type of input y variable(s) is integer, the output type of
#'          yImputed/estimate/estimateTotal is also integer. Estimates/sums are then calculated from rounded imputed values.
#' @param forceIdMatching  When TRUE id matching in underlying GetData is forced (id as named list is not needed).
#' @param ... Used in wrappers .... Can also be used to specify additional variable names that will be included in output (micro).
#'
#'
#' @details This function is related to \code{\link{ImputeRegression}}
#'          and the structure and the names of output are similar.
#'          Note that missing values of x is allowed here.
#'          In cases were both x and y are missing a warning will occur (zero is used in total estimates).
#'
#'
#' @return Output
#'
#' \strong{\code{micro}} consists of the following elements:
#'    \item{id}{id from input}
#'    \item{x}{The x variable created from input according to xName}
#'    \item{y}{The input y variable}
#'    \item{strata}{The input strata variable (can be NULL)}
#'    \item{category123}{Imputation groups: Not imputed (1), Imputed (3) and missing (0).
#'               Group 2 never happen with this function.}
#'    \item{yHat \emph{or estimateYHat}}{Fitted values}
#'    \item{yImputed \emph{or estimate}}{Imputed y-data}
#'    \item{rStud}{The final studentized residuals}
#'    \item{leaveOutResid}{The final outside-model residual}
#'    \item{varImputed}{Name of origin variable}
#'
#' \strong{\code{aggregates}} consists of the following elements:
#'   \item{N}{Number of observations in each strata}
#'    \item{nImputed}{Number of imputed observations in each strata}
#'    \item{estimate}{Total estimates from imputed data}
#'    \item{cv}{Coefficient of variation = seEstimate/estimate}
#'    \item{estimateYhat}{Totale estimate based on model fits}
#'    \item{estimateOrig \emph{or y}}{Estimate based on original data with missing set to zero}
#'    \item{n}{The final number of observations in model.}
#'    \item{sigmaHat}{The final square root of the estimated variance parameter}
#'    \item{seEstimate}{The final standard error estimate of the total estimate from imputed data}
#'
#' \strong{\code{total}} consists of the following elements:
#'    \item{Ntotal \emph{or N}}{Number of observations}
#'    \item{nImputedTotal \emph{or nImputed}}{Total number of imputed observations}
#'    \item{estimateTotal \emph{or estimate}}{Total estimate for all strata}
#'    \item{cvTotal or \emph{cv}}{Total cv for all strata}
#'
#'
#' @export
#'
#' @author Øyvind Langsrud
#'
#'
#' @examples
#'
#' rateData <- KostraData("rateData")             # Real Kostra data set
#' w <- rateData$data[, c(17,19,16,5)]        # Data with id, strata, x and y
#'
#' w <- w[is.finite(w[,"Ny.kostragruppe"]), ]       # Remove Longyearbyen
#' w[w[,"Ny.kostragruppe"]>13,"Ny.kostragruppe"]=13 # Combine small strata
#'
#' # Create historical data by modifying the "original x-variable"
#' w2=cbind(w,x1=1.2*w[,3]*rep(c(NA,NA,1,1),107),x2=1.1*w[,3]*rep(c(NA,1),214))
#' ImputeHistory(w2, strataName = names(w2)[2], xName=names(w2)[c(5,6,3)])  # Example with three historical variables - the last is complete
#' ImputeHistory(w2, strataName = names(w2)[2], xName=names(w2)[c(5,6)]) # Incomplete x and a warning is produced
#' ImputeHistoryTall(w2, strataName = names(w2)[2], xName=names(w2)[c(5,6,3)])
#' ImputeHistoryTallSmall(w2, strataName = names(w2)[2], xName=names(w2)[c(5,6,3)])
#' ImputeHistoryWide(w2, strataName = names(w2)[2], xName=names(w2)[c(5,6,3)])
#' ImputeHistoryWideSmall(w2, strataName = names(w2)[2], xName=names(w2)[c(5,6,3)])
#'
#' # Numbers instead of names works.
#' # Four equivalent variants using reverse and xName=NULL
#' ImputeHistory(w2, strataName = 2, xName=c(5,6,3), yName=4)
#' ImputeHistory(w2, strataName = 2, xName=c(3,6,5), yName=4, reverse=TRUE)
#' ImputeHistory(w2, strataName = 2, xName=NULL, yName = c(4,5,6,3))
#' ImputeHistory(w2, strataName = 2, xName=NULL, yName = c(3,6,5,4), reverse=TRUE)
#'
ImputeHistory <- function(data,
                                  idName = names(data)[1],
                                  strataName = NULL,
                                  xName = names(data)[3],
                                  yName = names(data)[4],
                                  weightMethod = "ordinary",
                                  reverse = FALSE,
                                  returnSameType = TRUE,
                          forceIdMatching = TRUE, ...) {
  CheckInput(idName, type = "varNrName", data = data, okSeveral = TRUE)
  CheckInput(strataName,   type = "varNrName", data = data, okNULL = TRUE)
  CheckInput(xName,        type = "varNrName", data = data, okSeveral = TRUE, okNULL = TRUE)
  CheckInput(yName,        type = "varNrName", data = data, okSeveral = is.null(xName))
  CheckInput(weightMethod, type = "character", alt = c("ordinary","ratio"))
  CheckInput(returnSameType,  type = "logical")


  model = "I(y-NaToZero(x))~0" # That is (y-x) is the error-term in a model without other terms
  # NaToZero to avoid "y" being missing when x is missing
  # LmImpute recognises this special model sets "x" to missing correctly

  BackTransform = function(y){
    return(y+NaToZero(dynGet("data")$x))
  }
  # Backtransfor is a function of y only, but a trick is used so that y+x is returned.
  # dynGet finds x in the environment were the function was called
  # NaToZero again to avoid "y" being missing when x is missing

  if(weightMethod=="ordinary") weights = NULL
  if(weightMethod=="ratio")    weights = "1/x"

  returnIter = FALSE
  returnYHat = TRUE

  xLimits = c(-Inf, Inf)
  yLimits = c(-Inf, Inf)

  if(weightMethod=="ratio") xLimits = c(1E-200, Inf)

  limitModel = Inf
  limitIterate = Inf
  limitImpute= Inf

  allowMissingX=TRUE
  historyX =TRUE # Thus, a single x is created based on first non-missing value in each row


################


  if(is.null(xName)){
    if(reverse){
      xInput <- GD(yName,LastFinite2Names)
      yInput <- GD(yName,LastColNames)
    }
    else{
      xInput <- GD(yName,FirstFinite2Names)
      yInput <- GD(yName,FirstColNames)
    }
  }
  else{
    if(reverse)
      xInput <- GD(xName,LastFiniteNames)
    else
      xInput <- GD(xName,FirstFiniteNames)
    yInput <- yName
  }

  dotNames <- names(list(...))

  if(forceIdMatching)
    names(idName)[1] <- "id"

  #z = GetIdxyStrataHistory(data=data,idName = idName, xName = xName, yName = yName, strataName = strataName)
  z <- GetData(data=data, id = GD(idName,MatrixPaste), x = xInput, y = yInput, strata = GD(strataName,MatrixPaste1),
               removeNULL = FALSE, returnAsDataFrame = FALSE, ...) #removeNULL = c(TRUE,TRUE,TRUE,FALSE))

  #  returnAsDataFrame=FALSE used so that names(z$x) is possible.
  #  data.frame created "manually" instead


  # Names available when x/y created by "Last(or First)Finite(2 or Col)Names"
  namesX <-   names(z$x)
  nameY <-   names(z$y)[1]

  # But if only single variable to these functions. Name will be "MiSsInGnAme".
  # When only single variable. Name can be taken from attr(z,"origVars").
  namesX[namesX=="MiSsInGnAme"] =  attr(z,"origVars")["x"]


  if(is.null(nameY))
    nameY <-  attr(z,"origVars")["y"]

  if(nameY=="MiSsInGnAme")
    nameY <-  attr(z,"origVars")["y"]



  z <- as.data.frame(z,stringsAsFactors = FALSE)


  z$x     = CheckNumeric(z$x , minLimit = xLimits[1], maxLimit = xLimits[2], setNA = FALSE, allowMissing = allowMissingX, varName = "x")
  z$y     = CheckNumeric(z$y , minLimit = yLimits[1], maxLimit = yLimits[2], varName = "y")

  #return(z)

  a=StrataApply(z,"strata",copyVar=c("id","x","y",dotNames),
                LmImpute,
                FunTotal=MakeTotal,
                returnLast = FALSE, returnFinal = TRUE, removeEmpty = TRUE,  #unfoldCoef=2,
                model = model,
                weights = weights,
                limitModel = limitModel, limitIterate = limitIterate, limitImpute=limitImpute, returnIter=returnIter,
                returnYHat =  returnYHat, BackTransform = BackTransform, returnSameType = returnSameType)

  # Remove meaningless output in this situation
  a$micro        <- a$micro[,!(colnames(a$micro) %in% c("dffits","hii")), drop=FALSE]

  # Change category from 0 (missing x) to 1 when y exist
  a$micro$category123[a$micro$category123==0 & !is.na(a$micro$y)] = 1L

  # Add varImputed
  namesX[a$micro$category123==1] <- nameY
  a$micro$varImputed <- namesX


  a$aggregates   <- a$aggregates[,!(colnames(a$aggregates) %in% "seRobust"), drop=FALSE] # seRobust is only 0
  a
}



#' @rdname ImputeHistory
#' @export
ImputeHistoryNewNames <- function(...){
  ImputeRegressionNewNames(..., Fun = ImputeHistory)
}

#' @rdname ImputeHistory
#' @export
#' @importFrom SSBtools RbindAll
ImputeHistoryTall <- function(..., iD=TalliD()){
  z <- ImputeHistoryNewNames(...,iD = iD)
  z$micro <- cbind(z$micro,N=1,nImputed= as.numeric(z$micro$category123==3))
  RbindAll(z)
}

#' @rdname ImputeHistory
#' @export
#' @importFrom SSBtools RbindAll
ImputeHistoryTallSmall <- function(...,
                                       iD=TalliD(),
                                   keep=c("ID","estimate","cv","nImputed")){
  z <- ImputeHistoryNewNames(...,iD = iD, keep = c(keep,"category123"))
  z$micro$category123 <- as.numeric(z$micro$category123==3)
  names(z$micro)[match("category123",names(z$micro))] <- "nImputed"
  RbindAll(z)
}


#' @rdname ImputeHistory
#' @export
#' @importFrom SSBtools CbindIdMatch
ImputeHistoryWide <- function(...,addName=WideAddName(), sep = WideSep(),
                                  idNames=c("","strata",""), addLast = FALSE){
  CbindIdMatch(ImputeHistoryNewNames(...),addName=addName, sep = sep, idNames = idNames, addLast = addLast)
}

#' @rdname ImputeHistory
#' @export
#' @importFrom SSBtools CbindIdMatch
ImputeHistoryWideSmall <- function(..., keep=c("id","strata","estimate","cv"),
                                       addName=WideAddName(), sep = WideSep(),
                                       idNames=c("","strata",""), addLast = FALSE){
  CbindIdMatch(ImputeHistoryNewNames(..., keep=keep),addName=addName, sep = sep, idNames = idNames, addLast = addLast)
}


# Special "history" variant. Takes multiple x as input (= histrorical y's).
# A single x is created based on first non-missing value in each row
GetIdxyStrataHistory_IKKEiBRUK <- function(data,idName = NULL, xName = "x", yName = "y", strataName = "strata") {
  x <- data[,xName, drop = FALSE]
  x <- x[cbind(1:dim(x)[1],WhereFirst(x))]
  cbind(x=x,GetIdxyStrata(data=data,idName=idName, xName=NULL,yName=yName,strataName=strataName, xNULL=NULL))
}




