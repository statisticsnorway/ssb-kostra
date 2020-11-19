#' Finding outliers of a sigle variable (y) by a regression model using a single explanatory variable (x).
#'
#' outliers are found by using a limit for studentized residuals.
#'
#' @encoding UTF8
#'
#' @param data Input data set of class data.frame
#'
#' @param idName Name of id-variable(s)
#' @param strataName Name of starta-variable. Single strata when NULL (default)
#' @param xName  Name of x-variable
#' @param yName Name of y-variable
#' @param method The method (model and weight) coded as a string: "ordinary" (default), "ratio", "noconstant",
#'        "mean" or "ratioconstant".
#' @param limitModel  Studentized residuals limit. Above limit -> outlier.
#' @param limitIterate Studentized residuals limit for iterative calculation of studentized residuals.
#'
#' @details Imputations are performed by running an imputation model within each strata.
#'          Division into three groups are based on studentized residuals. Calculations of studentized residuals
#'          are performed by iterativily throwing out observations from the model fitting.
#'
#'          Output is a list of two data sets, micro has as many rows as input and
#'          aggregates has one row for each strata.
#'
#'          This function is related to \code{\link{ImputeRegression}}
#'          and the structure and the names of output are very similar.
#'          Note that missing values of x are allowed here.
#'
#' @return Output of \strong{OutlierRegression} is a list of two data frames.  The micro data frame has as many rows as input and aggregates data frame has one row for each strata. The individual variables are:
#'
#' \strong{\code{micro}} consists of the following elements:
#'    \item{id}{id from input}
#'    \item{x}{The input x variable}
#'    \item{y}{The input y variable}
#'    \item{strata}{The input strata variable (can be NULL)}
#'    \item{outlier}{Dummy variable: outlier (1) or not (0).}
#'    \item{category123}{The three imputation groups: representative (1), correct but not representative (2), wrong (3). }
#'    \item{yHat}{Fitted values}
#'    \item{rStud}{The studentized residuals from last iteration}
#'    \item{dffits}{The DFFITS statistic from last iteration}
#'    \item{hii}{The leverages (diagonal elements of hat matrix) from last iteration}
#'    \item{leaveOutResid}{The outside-model residual from last iteration}
#'    \item{limLo}{-limitModel}
#'    \item{limUp}{limitModel}
#'
#' \strong{\code{aggregates}} consists of the following elements:
#'    \item{N}{Number of observations in each strata}
#'    \item{coef}{The final first model coefficient}
#'    \item{coefB}{The final second model coefficient or zeros when only one coefficient in model.}
#'    \item{nModel}{The final number of observations in model.}
#'    \item{sigmaHat}{The final square root of the estimated variance parameter}
#'
#'    Output of \strong{OutlierRegressionMicro} is the single data frame micro above.
#'
#'    Output of \strong{OutlierRegressionTall} and \strong{OutlierRegressionWide} are similiar to the
#'    functions in \code{\link{ImputeRegression}}.
#'
#' @export
#'
#' @author Øyvind Langsrud
#'
#'
#' @examples
#'
#' z = cbind(id=1:34,KostraData("ratioTest")[,c(3,1,2)])
#' OutlierRegression(z,strataName="k")
#' OutlierRegressionMicro(z,strataName="k")
#' OutlierRegressionTall(z,strataName="k")
#' OutlierRegressionWide(z,strataName="k")
#'
#' rateData <- KostraData("rateData")               # Real Kostra data set
#' w <- rateData$data[, c(17,19,16,5)]              # Data with id, strata, x and y
#' w <- w[is.finite(w[,"Ny.kostragruppe"]), ]       # Remove Longyearbyen
#' w[w[,"Ny.kostragruppe"]>13,"Ny.kostragruppe"]=13 # Combine small strata
#' OutlierRegression(w, strataName = names(w)[2], method="ratio")
#'
OutlierRegression <- function(data,
                                  idName = names(data)[1],
                                  strataName = NULL,
                                  xName = names(data)[3],
                                  yName = names(data)[4],
                                  method = "ordinary",
                                  limitModel = 2.5, limitIterate = 4.5) {
  CheckInput(idName, type = "varName", data = data)
  CheckInput(strataName,   type = "varNrName", data = data, okNULL = TRUE)
  CheckInput(xName,        type = "varNrName", data = data)
  CheckInput(yName,        type = "varNrName", data = data)
  CheckInput(method,       type = "character", alt = c("ordinary","ratio", "noconstant", "mean","ratioconstant"))
  CheckInput(limitModel,   type = "numeric", min = 0)
  CheckInput(limitIterate, type = "numeric", min = 0)

  if(method=="ordinary")      {model = "y~x";   weights = NULL}
  if(method=="ratio")         {model = "y~x-1"; weights = "1/x"}
  if(method=="noconstant")    {model = "y~x-1"; weights = NULL}
  if(method=="mean")          {model = "y~1";   weights = NULL;  xName = NULL}
  if(method=="ratioconstant") {model = "y~x"; weights = "1/x"}

  returnIter = FALSE
  returnYHat = TRUE

  xLimits = c(-Inf, Inf)
  yLimits = c(-Inf, Inf)
  allowMissingX = TRUE
  limitImpute = 0

  if(method=="ratio" | method=="ratioconstant" ) xLimits = c(1E-200, Inf)
  ## if(method=="ratio")   yLimits = c(0, Inf)

  #z = GetIdxyStrata(data=data,idName = idName, xName = xName, yName = yName, strataName = strataName)
  z <- GetData(data=data, id = GD(idName,MatrixPaste), x = GD(xName,FixEmpty), y = yName, strata = GD(strataName, MatrixPaste1),
               removeNULL = FALSE)

  z$x     = CheckNumeric(z$x , minLimit = xLimits[1], maxLimit = xLimits[2], setNA = FALSE, allowMissing = allowMissingX, varName = "x")
  z$y     = CheckNumeric(z$y , minLimit = yLimits[1], maxLimit = yLimits[2], varName = "y")

  a=StrataApply(z,"strata",copyVar=c("id","x","y"),
                LmImpute,
                FunTotal=MakeTotal,
                returnLast = TRUE, returnFinal = FALSE, unfoldCoef=2,
                model = model,
                weights = weights,
                limitModel = limitModel, limitIterate = limitIterate, limitImpute=limitImpute, returnIter=returnIter,
                returnYHat =  returnYHat)
  a$micro <- cbind(a$micro,limLo = -limitModel, limUp = limitModel)
  a
}

#' @rdname OutlierRegression
#' @export
OutlierRegressionMicro <- function(...){
  OutlierRegression(...)$micro
}


#' @rdname OutlierRegression
#' @export
#' @importFrom SSBtools RbindAll
OutlierRegressionTall <- function(..., iD=TalliD()){
  RbindAll(ImputeRegressionNewNames(...,Fun = OutlierRegression,iD = iD, newNames = NULL))
}


#' @rdname OutlierRegression
#' @export
#' @importFrom SSBtools CbindIdMatch
OutlierRegressionWide <- function(...,addName=WideAddName(), sep = WideSep(),
                                 idNames=c("","strata",""), addLast = FALSE){
  CbindIdMatch(OutlierRegression(...),addName=addName, sep = sep, idNames = idNames, addLast = addLast)
}



