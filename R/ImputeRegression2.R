#' Imputation of a sigle variable (y) by a regression model using
#' a primary explanatory variable (x1) and a secondary explanatory variable (x2) for cases where the primary is missing.
#'
#' @encoding UTF8
#'
#' @param data Input data set of class data.frame
#'
#' @param idName Name of id-variable(s)
#' @param strataName Name of starta-variable. Single strata when NULL (default)
#' @param x1Name Name of x1-variable
#' @param x2Name Name of x2-variable
#' @param yName Name of y-variable
#' @param method1  The method (model and weight) coded as a string: "ordinary" (default), "ratio", "noconstant",
#'        "mean" or "ratioconstant". I addition "ratio2" and "ratioconstant2" are alternatives where the weights
#'         are based on the other x-variable (x1<->x2).
#' @param method2 Similar to method2 above.
#' @param limitModel  Studentized residuals limit. Above limit -> group 2.
#' @param limitIterate Studentized residuals limit for iterative calculation of studentized residuals.
#' @param limitImpute  Studentized residuals limit. Above limit -> group 3.
#' @param returnSameType When TRUE (default) and when the type of input y variable(s) is integer, the output type of
#'          yImputed/estimate/estimateTotal is also integer. Estimates/sums are then calculated from rounded imputed values.
#'
#' @details Imputations are initially performed by running method1 using x1 within each strata.
#'          Division into three groups are based on studentized residuals. Calculations of studentized residuals
#'          are performed by iterativily throwing out observations from the model fitting.
#'          Missing imputed values caused by missing x1-values are thereafter imputed by running method2
#'          using x2 within each strata. Combined estimates of seRobust,seEStimate and cv are calculated.
#'
#' @return  Output of the alternative variants of the function
#'          are constructed similar
#'          to the variants of \code{\link{ImputeRegression}}.
#'
#'          Output of \strong{\code{ImputeRegression2}} and \strong{\code{ImputeRegression2NewNames}} (using the names after
#'         \code{\emph{or}}  below) is a list of three data sets. micro has as many rows as input, aggregates has one row for each strata
#'         and total has a single row. Variables from the two imputations are named using "A" and "B".
#'         The individual variables (dropping "A" and "B") are:
#'
#' \strong{\code{micro}} consists of the following elements:
#'    \item{id}{id from input}
#'    \item{x1}{The input x1 variable}
#'    \item{x2}{The input x2 variable}
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
#' @export
#'
#' @examples
#'
#' rateData <- KostraData("rateData")               # Real Kostra data set
#' w <- rateData$data[, c(17,19,3,16,5)]              # Data with id, strata, x1, x2 and y
#' w <- w[is.finite(w[,"Ny.kostragruppe"]), ]       # Remove Longyearbyen
#' ImputeRegression2(w, strataName = names(w)[2])    # Works without combining strata
#' w[w[,"Ny.kostragruppe"]>13,"Ny.kostragruppe"]=13 # Combine small strata
#' ImputeRegression2(w, strataName = names(w)[2]) # Ordinary regressions
#' ImputeRegression2(w, strataName = names(w)[2],x1Name = names(w)[4], method1="ratio") # x1=x2 and no imputation in round 2
#' ImputeRegression2(w, strataName = names(w)[2],method1="ratio2",method2="ratio") # ratio2 needed since x1=0
#' ImputeRegression2(w, strataName = names(w)[2],method1="ratioconstant2",method2="ratioconstant")
#' ImputeRegression2Tall(w, strataName = names(w)[2])
#' ImputeRegression2TallSmall(w, strataName = names(w)[2])
#' ImputeRegression2Wide(w, strataName = names(w)[2])
#' ImputeRegression2WideSmall(w, strataName = names(w)[2])
ImputeRegression2 <- function(data,
                             idName = names(data)[1],
                             strataName = NULL,
                             x1Name = names(data)[3],
                             x2Name = names(data)[4],
                             yName = names(data)[5],
                             method1 = "ordinary",
                             method2 = "ordinary",
                             limitModel = 2.5, limitIterate = 4.5, limitImpute = 50,
                             returnSameType = TRUE) {
  CheckInput(idName, type = "varName", data = data)
  CheckInput(strataName,   type = "varName", data = data, okNULL = TRUE)
  CheckInput(x1Name,        type = "varName", data = data)
  CheckInput(x2Name,        type = "varName", data = data)
  CheckInput(yName,        type = "varName", data = data)
  CheckInput(method1,       type = "character", alt = c("ordinary","ratio", "noconstant", "mean","ratioconstant","ratio2","ratioconstant2"))
  CheckInput(method2,       type = "character", alt = c("ordinary","ratio", "noconstant", "mean","ratioconstant","ratio2","ratioconstant2"))
  CheckInput(limitModel,   type = "numeric", min = 0)
  CheckInput(limitIterate, type = "numeric", min = 0)
  CheckInput(limitImpute,  type = "numeric", min = 0)
  CheckInput(returnSameType,  type = "logical")


  xModel=c("x1","x2")
  weights = c("NULL","NULL")

  if(method1=="ordinary")      {xModel[1] = "x1";   weights[1] = "NULL"}
  if(method1=="ratio")         {xModel[1] = "x1-1"; weights[1] = "1/x1"}
  if(method1=="noconstant")    {xModel[1] = "x1-1"; weights[1] = "NULL"}
  if(method1=="mean")          {xModel[1] = "1";    weights[1] = "NULL"}
  if(method1=="ratioconstant") {xModel[1] = "x1";   weights[1] = "1/x1"}
  if(method1=="ratio2")        {xModel[1] = "x1-1"; weights[1] = "1/x2"}
  if(method1=="ratioconstant2"){xModel[1] = "x1";   weights[1] = "1/x2"}
  if(method2=="ordinary")      {xModel[2] = "x2";   weights[2] = "NULL"}
  if(method2=="ratio")         {xModel[2] = "x2-1"; weights[2] = "1/x2"}
  if(method2=="noconstant")    {xModel[2] = "x2-1"; weights[2] = "NULL"}
  if(method2=="mean")          {xModel[2] = "1";    weights[2] = "NULL"}
  if(method2=="ratioconstant") {xModel[2] = "x2";   weights[2] = "1/x2"}
  if(method2=="ratio2")        {xModel[2] = "x2-1"; weights[2] = "1/x1"}
  if(method2=="ratioconstant2"){xModel[2] = "x2";   weights[2] = "1/x1"}


  returnIter  = FALSE
  returnYHat  = TRUE

  x1Limits = c(-Inf, Inf)
  x2Limits = c(-Inf, Inf)
  yLimits = c(-Inf, Inf)

  limitImpute = c(limitImpute,Inf)

  ############### Holder på her

  allowMissingX =c(TRUE,FALSE)

  if(method1=="ratio" | method1=="ratioconstant" ) x1Limits = c(1E-200, Inf)
  if(method2=="ratio" | method2=="ratioconstant" ) x2Limits = c(1E-200, Inf)
  ##if(method1=="ratio")   yLimits = c(0, Inf)
  ##if(method2=="ratio")   yLimits = c(0, Inf)

  #z = GetIdx1x2yStrata(data=data,idName = idName, x1Name = x1Name, x2Name = x2Name, yName = yName, strataName = strataName)

  if(TRUE){
  z <- GetData(data=data, id = GD(idName,MatrixPaste),
               x1 = GD(x1Name,FixEmpty), x2 = GD(x2Name,FixEmpty),
               y = yName, strata = GD(strataName,MatrixPaste),
               removeNULL = FALSE) #
  }
  z$x1  = CheckNumeric(z$x1 , minLimit = x1Limits[1], maxLimit = x1Limits[2], setNA = FALSE, allowMissing = allowMissingX[1], varName = "x1")
  z$x2  = CheckNumeric(z$x2 , minLimit = x2Limits[1], maxLimit = x2Limits[2], setNA = FALSE, allowMissing = allowMissingX[2], varName = "x2")
  z$y   = CheckNumeric(z$y , minLimit = yLimits[1], maxLimit = yLimits[2], varName = "y")

  #return(z)

  a=StrataApply(z,"strata",copyVar=c("id","x1","x2","y"),
                LmImpute2,
                FunTotal=MakeTotal,
                returnLast = FALSE, returnFinal = TRUE, unfoldCoef=2,
                xModel = xModel,
                weights = weights,
                limitModel = limitModel, limitIterate = limitIterate, limitImpute=limitImpute, returnIter=returnIter,
                returnYHat =  returnYHat, returnSameType = returnSameType)
  a
}

#' @rdname ImputeRegression2
#' @export
ImputeRegression2NewNames <- function(...,
                                      oldNames=c("yImputed","Ntotal","nImputedTotal","AnImputedTotal","BnImputedTotal","estimateTotal","AyHat","ByHat","AestimateOrig","cvTotal"),
                                      newNames=c("estimate","N","nImputed","AnImputed","BnImputed","estimate","AestimateYHat","BestimateYHat","y","cv")){
  ImputeRegressionNewNames(..., Fun = ImputeRegression2,
                           oldNames = oldNames,
                           newNames = newNames)
}

#' @rdname ImputeRegression2
#' @export
#' @importFrom SSBtools RbindAll
ImputeRegression2Tall <- function(..., iD=TalliD()){
  z <- ImputeRegression2NewNames(...,iD = iD)
  AnImputed <- as.numeric(z$micro$Acategory123==3)
  BnImputed <- as.numeric(z$micro$Bcategory123==3)
  nImputed  <- AnImputed + BnImputed
  z$micro <- cbind(z$micro,N=1,nImputed = nImputed, AnImputed = AnImputed, BnImputed = BnImputed)
  RbindAll(z)
}

#' @rdname ImputeRegression2
#' @export
#' @importFrom SSBtools RbindAll
ImputeRegression2TallSmall <- function(...,
                                      iD=TalliD(),
                                      keep=c("ID","estimate","cv","nImputed")){
  z <- ImputeRegression2NewNames(...,iD = iD, keep = c(keep,"Acategory123","Bcategory123"))
  z$micro$Acategory123 <- as.numeric(z$micro$Acategory123==3) + as.numeric(z$micro$Bcategory123==3)
  z$micro$Bcategory123 <- NULL
  names(z$micro)[match("Acategory123",names(z$micro))] <- "nImputed"
  RbindAll(z)
}


#' @rdname ImputeRegression2
#' @export
#' @importFrom SSBtools CbindIdMatch
ImputeRegression2Wide <- function(...,addName=WideAddName(), sep = WideSep(),
                                 idNames=c("","strata",""), addLast = FALSE){
  CbindIdMatch(ImputeRegression2NewNames(...),addName=addName, sep = sep, idNames = idNames, addLast = addLast)
}

#' @rdname ImputeRegression2
#' @export
#' @importFrom SSBtools CbindIdMatch
ImputeRegression2WideSmall <- function(..., keep=c("id","strata","estimate","cv","nImputed"),
                                      addName=WideAddName(), sep = WideSep(),
                                      idNames=c("","strata",""), addLast = FALSE){
  CbindIdMatch(ImputeRegression2NewNames(..., keep=keep),addName=addName, sep = sep, idNames = idNames, addLast = addLast)
}


















