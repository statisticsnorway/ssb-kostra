#' INTERNAL FUNCTION: Repeated regression imputation.
#'
#' LmImpute is run several times using different versions of input parameters. Output from each run of
#' LmImpute is in output of this function. In each run it is possible to update the interest variable (y)
#' with available imputed values. Thus the function can be used to impute using a primary x-variable and
#' a secondary x-variable for cases where the primary is missing.
#'
#' @encoding UTF8
#'
#' @param data Input data set (data.frame, data.table or list)
#' @param yName Name of interest variable in data set
#' @param xModel  Vector of strings with the right part model formula.
#' @param yModel  String with left part model formula  (vector possible)
#' @param weights NULL or string with weight expression  (vector possible)
#' @param limitModel Studentized residuals limit. Above limit -> category 2.(vector possible)
#' @param limitIterate Studentized residuals limit for iterative calculation of studentized residuals.(vector possible)
#' @param limitImpute  Studentized residuals limit. Above limit -> category 3. No imputation when 0.(vector possible)
#' @param maxiter Maximum number of iterations.(vector possible)
#' @param returnIter   When TRUE, iteration when observation was thrown outin output.(vector possible)
#' @param returnYHat   When TRUE, fitted values and corresponding estimates in output. (vector possible)
#' @param returnFirst  When TRUE, studentized residuals from first iteration in output. (vector possible)
#' @param returnLast   When TRUE, some results from last iteration in output.(vector possible)
#' @param returnFinal  When TRUE, extra results from final model in output. (vector possible)
#' @param MultiFuction Transforming rStud for several responses into a single positive value.
#' @param estimationGroup Total estimates will be computed within each group. Default (and TRUE) is a single group (estimationGroup <- rep(1, N) ).
#' @param unfoldCoef  When TRUE several elements of coef will be spilt as several ouput elements.
#' @param BackTransform When model contains transformation of y (e.g: "log(y)~x") a function (e.g: exp) can be supplied to transform back
#'         to original scale before calculation of leaveOutResid, yHat, yImputed, estimate, estimateYHat, estimateOrig and seRobust.
#'         (list possible)
#' @param warningEstimate Warning text when missing values. Use NULL to avoid warning. (list possible)
#' @param removeEmpty When TRUE empty elements will be removed from output.
#' @param replaceByImputed When TRUE missing values of the interest variable (y) is replaced by imputed values in each round.
#' @param imputedInModel   When FALSE above imputed values are omitted from subsequent models (category 2 forced).
#' @param category123FromFirst Whene TRUE category123 from first run is input to subsequent LmImpute calls.
#' @param cvPercent When TRUE (default) cv output is in percent
#' @param returnSameType When TRUE and when the type of input y variable(s) is integer, the output type of yImputed
#'          and estimate is also integer. Estimates/sums are then calculated from rounded imputed values.
#' @param keepSinge When non-NULL only output elements with names in keepSinge are kept from first LmImputeOne2Many run.
#' @param keepMulti When non-NULL only output elements with names in keepMulti are kept from second LmImputeOne2Many run.
#'
#' @details LmImputeMulti performs several calls to LmImpute and the number of calls is the length of xModel.
#'          Other parameters can also change between calls by specifying them as vectors or lists.
#'
#'          LmImpute2 is a specialized variant for two LmImpute runs only
#'          and combined estimates of seRobust, seEStimate and cv are calculated when replaceByImputed = TRUE.
#'
#'          LmImputeOne2Many is another specialized variant meant for two runs using the same model except
#'          that the first run has a single y and the next run several y's. Category123 from the first run is used in the second.
#'
#' @return Output of LmImputeMulti is a list where each element is the output of \code{\link{LmImpute}}.
#'         Output of LmImpute2 is not such a list. Instead the names are changed using "A" and "B".
#'         Output of LmImputeOne2Many is not such a list. Instead the names from second round are changed using "M".
#'
#' @keywords intern
#' @export
#'
#'
LmImputeMulti <- function(data,yName="y",xModel=c("x1","x2"), yModel=yName,     #model = c("y~x1","~x2") ,
                      weights = NULL, limitModel = 2.5, limitIterate = 4.5, limitImpute = 50, maxiter = 10,
                     returnIter = TRUE, returnYHat = FALSE, returnFirst = FALSE, returnLast = TRUE, returnFinal = FALSE,
                     MultiFuction = function(x) {max(abs(x))},
                     estimationGroup = TRUE, unfoldCoef = FALSE,
                     BackTransform = list(NULL), #vector("list",n),
                     warningEstimate = vector("list",n),
                     removeEmpty=FALSE,
                     replaceByImputed = TRUE,
                     imputedInModel=FALSE, # ratio=FALSE,
                     category123FromFirst=FALSE,
                     cvPercent=TRUE,
                     returnSameType=FALSE) {

  if(!is.list(BackTransform)) BackTransform <- list(BackTransform)
  data <- as.data.frame(data)
  n = length(xModel)
  if(length(yModel)==1) yModel = rep(yModel,n)
  N = NROW(data[yName])
  z <- vector("list",n)
  forceCategory2 = rep(FALSE, N)

  #if(ratio){
  #  dataInput = copy(data)
  #  data = data.frame(y = dataInput[yName], x=NaN)
  #  model = "Ratio"
  #}
  category123 = NULL
  for(i in 1:n){
    #if(ratio){
    #  data[,"x"] = dataInput[,xModel[i]]
    #  if(replaceByImputed & i>1)
    #  data[,"y"] <- z[[i-1]]$yImputed
    #} else {
      model = paste(yModel[i],"~",xModel[i]) # paste(yName,"~",xModel[i])
      if(replaceByImputed & i>1)
        data[yName] <- z[[i-1]]$yImputed
    #}
    if(i==2) if(category123FromFirst) category123 = z[[1]]$category123
    if(i>1)
      if(replaceByImputed & !imputedInModel) forceCategory2 = forceCategory2|(z[[i-1]]$category123==3)
    z[[i]] <-
      LmImpute(data=data,
             model = model,  #print(update(formula(model[i]), paste(yName," ~ .")),showEnv=FALSE),
             weights = d(weights,i),
             limitModel = d(limitModel,i),
             limitIterate = d(limitIterate,i),
             limitImpute = d(limitImpute,i),
             maxiter = d(maxiter,i),
             returnIter = d(returnIter,i),
             returnYHat = d(returnYHat,i),
             returnFirst = d(returnFirst,i),
             returnLast = d(returnLast,i),
             returnFinal = d(returnFinal,i),
             MultiFuction = MultiFuction,
             estimationGroup = estimationGroup,
             unfoldCoef = unfoldCoef,
             category123 = category123,
             forceCategory2 =forceCategory2,
             BackTransform = BackTransform[[min(i,length(BackTransform))]],
             warningEstimate = warningEstimate[[i]],
             removeEmpty=removeEmpty,
             cvPercent=cvPercent,
             returnSameType = returnSameType)
  }
 z
}



#' @rdname LmImputeMulti
#' @export
#'
#' @examples
#'
#' # -----  LmImpute2 and LmImputeMulti -----
#' set.seed(123)  # same results each time
#' z = data.frame(  # Small test data set
#' x1 = c(NA,2:19,NA),
#' x2 = rep(1:20),
#' y = runif(20)+c(rep(0,15),NA,1E3,1E5,NA,NA))
#' LmImpute2(z)
#' LmImputeMulti(z)
#' LmImputeMulti(z,xModel=c("x1","x2","x2"),limitImpute=c(50,50,3), replaceByImputed=FALSE)
#'
LmImpute2 <- function(data, warningEstimate=list(NULL,"estimate: Missing yImputed replaced by zero"),
                      replaceByImputed = TRUE, cvPercent = TRUE, ...){
  a <- LmImputeMulti(data = data, warningEstimate = warningEstimate, replaceByImputed = replaceByImputed, cvPercent = cvPercent, ...)

  a[[1]]["yImputed"] <- NULL
  a[[1]]["estimate"] <- NULL
  a[[1]]["cv"]       <- NULL
  a[[2]]["cv"]       <- NULL
  a[[2]]["N"]        <- NULL

  cola <- !(names(a[[1]]) %in% c("N"))
  names(a[[1]])[cola] <- paste("A",names(a[[1]])[cola],sep="")

  colb <- !(names(a[[2]]) %in% c("yImputed","estimate"))
  names(a[[2]])[colb] <- paste("B",names(a[[2]])[colb],sep="")

  a <- c(a[[1]],a[[2]])

  if(replaceByImputed){
    a$nImputed <- a$AnImputed + a$BnImputed
    a$seRobust   <- sqrt(a$AseRobust^2 + a$BseRobust^2)
    a$seEstimate   <- sqrt(a$AseEstimate^2 + a$BseEstimate^2)
    a$cv <- (99*as.numeric(cvPercent) + 1) * a$seEstimate/a$estimate
  }
  a
}





# http://stackoverflow.com/questions/29878449/data-frame-with-matrix-embedded-in-one-variable

#' @rdname LmImputeMulti
#' @export
#'
#' @examples
#'
#'  # -----  LmImputeOne2Many -----
#'  # Create data fram z with matrix embedded in one variable
#'  # The single y is also the first variable in the matrix yMany
#'  set.seed(123)  # same results each time
#'  y = runif(20)+c(rep(0,15),NA,1E3,1E5,NA,NA)
#'  z = data.frame(  # Small test data set
#'    x = c(1,1,1:10,3,3,3,4,4,5,5,5),
#'    y = y,
#'    yMany = I(cbind(y,matrix(1:60,20,3,dimnames=list(NULL,c("A","B","C"))))))
#'  a=LmImputeOne2Many(z,limitModel=1)
#'  print(a)
#'  a$MrStud[, 1] -  a$rStud # Not equal since no iteration and no "last" when multiple y
#'  a=LmImputeOne2Many(z,limitModel=1,returnFinal=TRUE)
#'  a$MrStud[, 1] -  a$rStud # Now equal
#'
#'
#'
LmImputeOne2Many <- function(data,yName="y",xModel="x",
                                    yModel=c(yName,"yMany"),
                                    category123FromFirst = TRUE,
                             keepSinge=NULL, keepMulti=NULL,
                             ...){
  if(length(xModel)==1) xModel = rep(xModel,length(yModel))
  a <- LmImputeMulti(data = data, yName = yName, xModel = xModel, yModel = yModel,
                category123FromFirst = category123FromFirst,...)
  ## Mor here to restructure output later
  if(!is.null(keepSinge))
    indSingle <- match(keepSinge,names(a[[1]]))
  else
    indSingle <- seq_len(length(a[[1]]))
  if(!is.null(keepMulti))
    indMulti <- match(keepMulti,names(a[[2]]))
  else
    indMulti <- seq_len(length(a[[2]]))

  names(a[[2]]) <- paste("M",names(a[[2]]),sep="")
  c(a[[1]][indSingle],a[[2]][indMulti])
}

#################



## Dobbel anvendelse av LmImputeMulti
##  replaceByImputed = TRUE: Imputer ettehverandre med flere x-er som kan ha missing
##  replaceByImputed = FALSE: Prøv flere modeller på de sammme dataene
##                   ..... men når det bare er ett strata her er det kanskje bedre å loope på høyere nivå


# Husk at I(x^2) fungerer, men ikke I(y^2). Det må isteden lages funksjoner for y.



# Function not in use
# Can be useful for log transformation
PosLog = function(x){
  log(max(x,1))
}



d <- function(x,i){
  j <- min(i,length(x))
  if(!j) return(NULL)
  x[j]
}



if(FALSE){
  z = data.frame(  # Same example as in Thorud et.al (2010).
    x1 = c(NA,2:19,NA),
    x2 = rep(1:20),
    y = runif(20)+c(rep(0,15),NA,1E3,1E5,NA,NA))
    LmImpute2(z)

  # Data frame kan lages med matrise
  m=data.frame(x=1:3,y=I(matrix(2,3,4)))
  m["y"]
  m["x"]
}


GetYname <- function(model) {
  s <- as.character(as.formula(model))
  if (length(s) == 3)
    if (s[1] == "~")
      return(s[2])
  stop("as.character(as.formula NOT working")
}




