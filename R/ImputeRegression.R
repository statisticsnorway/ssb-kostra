#' Imputation of a sigle variable (y) by a regression model using a single explanatory variable (x).
#'
#' Impute missing and wrong values (group 3) by the model based on representative data (group 1).
#' Some data are considered correct but not representative (group 2).
#'
#' @encoding UTF8
#'
#' @param data Input data set of class data.frame
#'
#' @param idName Name of id-variable(s)
#' @param strataName Name of starta-variable. Single strata when NULL (default)
#' @param xName  Name of x-variable
#' @param yName  Name of y-variable
#' @param method The method (model and weight) coded as a string: "ordinary" (default), "ratio", "noconstant",
#'        "mean" or "ratioconstant".
#' @param limitModel  Studentized residuals limit. Above limit -> group 2.
#' @param limitIterate Studentized residuals limit for iterative calculation of studentized residuals.
#' @param limitImpute  Studentized residuals limit. Above limit -> group 3.
#' @param returnSameType When TRUE (default) and when the type of input y variable(s) is integer, the output type of
#'          yImputed/estimate/estimateTotal is also integer. Estimates/sums are then calculated from rounded imputed values.
#' @param ... Simplified specification of the above arguments and possibly the five arguments below.
#'        Can also be used to specify additional variable names that will be included in output (micro).
#' @param Fun Function as input to ImputeRegressionNewNames for more general applications.
#' @param oldNames Vector of output names to be changed.
#' @param newNames Corresponding vector of new names.
#' @param iD When non-NULL a new variable ID will be created (see details).
#' @param keep When non-NULL Only variables listed in keep will be kept.
#'        This is input to ImputeRegressionNewNames and for more general applications keep apply to the three
#'        first list elements.
#' @param addName NULL or vector of strings used to name columns according to origin frame.
#' @param sep A character string to separate when addName apply
#' @param idNames Names of a id variable within each data frame
#' @param addLast When TRUE addName will be at end
#'
#' @details
#'
#'          Imputations are performed by running an imputation model within each strata.
#'          Division into three groups are based on studentized residuals. Calculations of studentized residuals
#'          are performed by iterativily throwing out observations from the model fitting.
#'
#'  Below (Value) the names before \code{\emph{or}} are unique and the names after \code{\emph{or}} can be used to combine the
#'  data by stacking (rbind). The latter is the basis for the Tall/Wide/Small functions which has a single data frame as output.
#'
#'  More specifically ImputeRegressionNewNames is a wrapper to ImputeRegression and the Tall/Wide/Small functions
#'  are wrappers to ImputeRegressionNewNames.
#'
#'  The last four parameters (addName, sep, idNames addLast) are parameters to \code{\link{CbindIdMatch}} used by
#'  ImputeRegressionWide and ImputeRegressionWideSmall.
#'
#'  The parameter iD is used by ImputeRegressionTall and ImputeRegressionTallSmall.
#'  A character variable ID is created using the input names ("id" "strata" and "Landet"). If the input name correspond to av variable name
#'  this variable is used. If not, the input name is used direvtly (possibly replicated).
#'
#' @return Output of \strong{\code{ImputeRegression}} and \strong{\code{ImputeRegressionNewNames}} (using the names after
#'         \code{\emph{or}}  below) is a list of three data sets. micro has as many rows as input, aggregates has one row for each strata
#'         and total has a single row. The individual variables are:
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
#'    \item{nModel}{The final number of observations in model.}
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
#' @importFrom SSBtools MatrixPaste MatrixPaste1
#' @export
#'
#' @author Øyvind Langsrud
#'
#'
#' @examples
#'
#' z = cbind(id=1:34,KostraData("ratioTest")[,c(3,1,2)])
#' ImputeRegression(z,strataName="k")
#'
#' # Datasett med kjonn som eksta id
#' zkjonn  <- rbind(cbind(z,kjonn="mann"),cbind(z,kjonn="kvinne"))
#' zkjonn$y[1:34] <- zkjonn$y[1:34]  + 1:34
#'
#' # Kjøring der id egentlig ikke blir brukt. Kjønn i output.
#' ImputeRegression(zkjonn,idName="id",strataName= "k",kjonnOutput="kjonn")
#'
#' # Kjøring der id er kodet med id i list. Da lages data med unik id (første treff) uten feilmelding eller warning (kan endres)
#' ImputeRegression(zkjonn,idName=list(id="id"),strataName= "k",kjonnOutput="kjonn")
#'
#' # Kjøring med sammensatt id + tar med enkelvariabler i output.
#' ImputeRegression(zkjonn,idName=c("id","kjonn"),strataName= "k",kjonnOutput="kjonn",idOutput="id")
#'
#' # Kjøring med sammensatt id og samnnesat strata  + tar med enkelvariabler i output.
#' ImputeRegression(zkjonn,idName=c("id","kjonn"),strataName= c("k","kjonn"),kjonnOutput="kjonn",idOutput="id")
#'
#' # Tilsvarende ved bruk av liste
#' ImputeRegression(zkjonn,idName=list(id=c("id","kjonn")),strataName= list(c("k","kjonn")),kjonnOutput=list("kjonn"))
#'
#' # Bruker liste til å snevre inn til ett kjønn
#' ImputeRegression(zkjonn,idName=list(id="id",kjonn="mann"),strataName= list("k",kjonn="mann"),kjonnOutput=list("kjonn",kjonn="mann"))
#'
#' ImputeRegression(z,strataName="k",method="ratio")
#' ImputeRegressionNewNames(z,strataName="k",method="ratio")
#' ImputeRegressionTall(z,strataName="k",method="ratio")
#' ImputeRegressionTallSmall(z,strataName="k",method="ratio")
#' ImputeRegressionWide(z,strataName="k",method="ratio")
#' ImputeRegressionWideSmall(z,strataName="k",method="ratio")
#'
#'
#' rateData <- KostraData("rateData")               # Real Kostra data set
#' w <- rateData$data[, c(17,19,16,5)]              # Data with id, strata, x and y
#' w <- w[is.finite(w[,"Ny.kostragruppe"]), ]       # Remove Longyearbyen
#' ImputeRegression(w, strataName = names(w)[2])    # Works without combining strata
#' w[w[,"Ny.kostragruppe"]>13,"Ny.kostragruppe"]=13 # Combine small strata
#' ImputeRegression(w, strataName = names(w)[2], method="ratio")
#' ImputeRegressionTallSmall(w, strataName = names(w)[2], method="ratio")
#' ImputeRegressionWideSmall(w, strataName = names(w)[2], method="ratio")
#'
ImputeRegression <- function(data,
                                  idName = names(data)[1],
                                  strataName = NULL,
                                  xName = names(data)[3],
                                  yName = names(data)[4],
                                  method = "ordinary",
                                  limitModel = 2.5, limitIterate = 4.5, limitImpute = 50,
                                  returnSameType = TRUE,
                             ...) {  # 28. mars 2017 Tester bruk av "..."
  CheckInput(idName, type = "varName", data = data, okSeveral = TRUE)
  CheckInput(strataName,   type = "varNrName", data = data, okNULL = TRUE, okSeveral = TRUE)
  CheckInput(xName,        type = "varNrName", data = data)
  CheckInput(yName,        type = "varNrName", data = data)
  CheckInput(method,       type = "character", alt = c("ordinary","ratio", "noconstant", "mean","ratioconstant"))
  CheckInput(limitModel,   type = "numeric", min = 0)
  CheckInput(limitIterate, type = "numeric", min = 0)
  CheckInput(limitImpute,  type = "numeric", min = 0)
  CheckInput(returnSameType,  type = "logical")
  #CheckInput(sepId, type = "character", okNULL = TRUE)


  if(method=="ordinary")     {model = "y~x";   weights = NULL}
  if(method=="ratio")        {model = "y~x-1"; weights = "1/x"}
  if(method=="noconstant")   {model = "y~x-1"; weights = NULL}
  if(method=="mean")         {model = "y~1";   weights = NULL;  xName = NULL}
  if(method=="ratioconstant"){model = "y~x";   weights = "1/x"}
  #if(method=="ratio1")        {model = "y~I(x+1)-1"; weights = "1/(x+1)"}

  returnIter  = FALSE
  returnYHat  = TRUE

  xLimits = c(-Inf, Inf)
  yLimits = c(-Inf, Inf)
  allowMissingX = TRUE #allowMissingX = FALSE

  if(method=="ratio" | method=="ratioconstant" ) xLimits = c(1E-200, Inf)
  ##if(method=="ratio")   yLimits = c(0, Inf)


  #z = GetIdxyStrata(data=data,idName = idName, xName = xName, yName = yName, strataName = strataName)

  dotNames <- names(list(...)) # 28. mars 2017 Tester bruk av "..."

  z <- GetData(data=data, id = GD(idName,MatrixPaste), x = GD(xName,FixEmpty), y = yName, strata = GD(strataName, MatrixPaste1),
               removeNULL = FALSE, ...)  # 28. mars 2017 Tester bruk av "..."


  z$x     = CheckNumeric(z$x , minLimit = xLimits[1], maxLimit = xLimits[2], setNA = FALSE, allowMissing = allowMissingX, varName = "x")
  z$y     = CheckNumeric(z$y , minLimit = yLimits[1], maxLimit = yLimits[2], varName = "y")

  #return(z)

  a=StrataApply(z,"strata",copyVar=c("id","x","y",dotNames), # 28. mars 2017 Tester bruk av "..."
                LmImpute,
                FunTotal=MakeTotal,
                returnLast = FALSE, returnFinal = TRUE, unfoldCoef=2,
                model = model,
                weights = weights,
                limitModel = limitModel, limitIterate = limitIterate, limitImpute=limitImpute, returnIter=returnIter,
                returnYHat =  returnYHat,
                returnSameType = returnSameType)
  a
}




#' @rdname ImputeRegression
#' @export
ImputeRegressionNewNames <- function(..., Fun = ImputeRegression,
                                          oldNames=c("yImputed","Ntotal","nImputedTotal","estimateTotal","yHat","estimateOrig","cvTotal"),
                                          newNames=c("estimate","N","nImputed","estimate","estimateYHat","y","cv"),
                                          iD = NULL, keep=NULL){
  a <- Fun(...)
  for(i in 1:min(3,length(a))){
    if(length(newNames)){
      matc = match(oldNames,names(a[[i]]))
      names(a[[i]])[matc[!is.na(matc)]] = newNames[!is.na(matc)]
    }
    if(!is.null(iD)){
      a[[i]] <- cbind(ID= nameOrVector(a[[i]],iD[i]),a[[i]],stringsAsFactors=FALSE)
    }
    if(!is.null(keep)){
      ind <- match(keep,names(a[[i]]))
      ind <- ind[!is.na(ind)]
      a[[i]] <- a[[i]][,ind, drop=FALSE]
    }
  }
  a
}

#' @rdname ImputeRegression
#' @export
#' @importFrom SSBtools RbindAll
ImputeRegressionTall <- function(..., iD=TalliD()){
  z <- ImputeRegressionNewNames(...,iD = iD)
  z$micro <- cbind(z$micro,N=1,nImputed= as.numeric(z$micro$category123==3))
  RbindAll(z)
}

#' @rdname ImputeRegression
#' @export
#' @importFrom SSBtools RbindAll
ImputeRegressionTallSmall <- function(...,
                                      iD=TalliD(),
                                      keep=c("ID","estimate","cv","nImputed")){
  z <- ImputeRegressionNewNames(...,iD = iD, keep = c(keep,"category123"))
  z$micro$category123 <- as.numeric(z$micro$category123==3)
  names(z$micro)[match("category123",names(z$micro))] <- "nImputed"
  RbindAll(z)
}


#' @rdname ImputeRegression
#' @export
#' @importFrom SSBtools CbindIdMatch
ImputeRegressionWide <- function(...,addName=WideAddName(), sep = WideSep(),
                                 idNames=c("","strata",""), addLast = FALSE){
  CbindIdMatch(ImputeRegressionNewNames(...),addName=addName, sep = sep, idNames = idNames,  addLast = addLast)
}

#' @rdname ImputeRegression
#' @export
#' @importFrom SSBtools CbindIdMatch
ImputeRegressionWideSmall <- function(..., keep=c("id","strata","estimate","cv","nImputed"),
                                 addName=WideAddName(), sep = WideSep(),
                                 idNames=c("","strata",""), addLast = FALSE){
  CbindIdMatch(ImputeRegressionNewNames(..., keep=keep),addName=addName, sep = sep, idNames = idNames,  addLast = addLast)
}



GetIdxyStrata <- function(data,idName = NULL, xName = "x", yName = "y", strataName = "strata",
                              xNULL = 0, sepId="_") {
  if(is.null(idName)) id = seq_len(NROW(data))
  else{
    if(length(idName)==1) id = data[ ,idName]
    else id=apply(data[ ,idName,drop=FALSE] , 1 , paste , collapse = sepId )
  }

  if(is.null(strataName)) strata=1
  else strata = data[ ,strataName]


  if(is.null(xName) & !is.null(xNULL)) x=xNULL
  else x = data[ ,xName]

  data.frame(
    id = id,
    strata = strata,
    x = x,
    y = data[ ,yName],
    stringsAsFactors = FALSE)
}


nameOrVector = function(data, name){
  ma <- match(name,names(data))
  if(!is.na(ma)) return(as.character(data[,name]))
  return(name)
}


