#' INTERNAL FUNCTION: Regeression imputation.
#'
#' Imputation by weighted regeression, using lm, allowing multiple explanatory
#' variables and multiple response variables.
#' Impute missing and wrong values (category 3) by the model based on representative data (category 1).
#' Some data are considered correct but not representative (category 2).
#'
#' @encoding UTF8
#'
#' @param data  Input data set (data.frame, data.table or list)
#' @param model String with model formula
#' @param weights  NULL or string with weight expression
#' @param limitModel  Studentized residuals limit. Above limit -> category 2.
#' @param limitIterate Studentized residuals limit for iterative calculation of studentized residuals.
#' @param limitImpute  Studentized residuals limit. Above limit -> category 3. No imputation when 0.
#' @param maxiter Maximum number of iterations.
#' @param returnIter   When TRUE, iteration when observation was thrown outin output.
#' @param returnYHat   When TRUE, fitted values and corresponding estimates in output.
#' @param returnFirst  When TRUE, studentized residuals from first iteration in output.
#' @param returnLast   When TRUE, some results from last iteration in output.
#' @param returnFinal  When TRUE, extra results from final model in output.
#' @param MultiFuction Transforming rStud for several responses into a single positive value.
#' @param estimationGroup Total estimates will be be computed within each group. Default (and TRUE) is a single group (estimationGroup <- rep(1, N) ).
#' @param unfoldCoef  When TRUE several elements of coef will be spilt as several ouput elements.
#'        unfoldCoef=2 is a specialised variant used to ensure two coefficients in output (extra coefficient zero).
#' @param category123 When non-NULL, this is used directly with no iteration.
#' @param forceCategory2 Force category 2 (can be useful for elements imputed by another method)
#' @param BackTransform When model contains transformation of y (e.g: "log(y)~x") a function (e.g: exp) can be supplied to transform back
#'         to original scale before calculation of leaveOutResid, yHat, yImputed, estimate, estimateYHat, estimateOrig and seRobust.
#' @param warningEstimate Warning text when missing values. Use NULL to avoid warning.
#' @param removeEmpty When TRUE empty elements will be removed from output.
#' @param NArStudHandling Function (warning or stop) taking a message as input. Used when rStud in model (category 1) is missing.
#' @param cvPercent When TRUE (default) cv output is in percent
#' @param returnSameType When TRUE and when the type of input y variable(s) is integer, the output type of yImputed
#'          and estimate is also integer. Estimates/sums are then calculated from rounded imputed values.
#'
#'
#' @return A list with separate elements. Each element can be a scalar, vector or a matrix.
#' Possible elements are:
#'    \item{x}{The input x variable}
#'    \item{y}{The input y variable}
#'    \item{strata}{The input strata variable}
#'    \item{category123}{The three imputation groups: representative (1), correct but not representative (2), wrong (3) and zero
#'    when x is missing.}
#'    \item{yHat}{Fitted values}
#'    \item{yImputed}{Imputed y-data}
#'    \item{rStudFirst}{Initial studentized residuals}
#'    \item{rStud}{The final (or last) studentized residuals}
#'    \item{dffits}{The final (or last) DFFITS statistic}
#'    \item{hii}{The final (or last) leverages (diagonal elements of hat matrix)}
#'    \item{leaveOutResid}{The final (or last) outside-model residual}
#'    \item{iter}{Iteration when observation was thrown out}
#'    \item{N}{Total number of observations (rows in data)}
#'    \item{nImputed}{Number of imputed observations}
#'    \item{estimate}{Totale estimate from imputed data}
#'    \item{cv}{Coefficient of variation = seEstimate/estimate. In percent when cvPercent=TRUE (default)}
#'    \item{estimateYhat}{Totale estimate based on model fits}
#'    \item{estimateOrig}{Estimate based on original data with missing set to zero}
#'    \item{coef}{The final (or last) model coefficient(s). Several variables when several parameters ("coef..Intercept.", "coef.x").}
#'    \item{nModel}{The final (or last) number of observations in model.}
#'    \item{sigmaFirst}{Initial square root of the estimated variance parameter}
#'    \item{sigmaHat}{The final (or last)  square root of the estimated variance parameter}
#'    \item{seEstimate}{The final (or last) standard error estimate of the total estimate from imputed data}
#'    \item{seRobust}{Robust variant of seEstimate (experimental)}
#'
#' @keywords intern
#' @export
#' @importFrom MASS ginv
#' @importFrom stats as.formula model.frame na.pass rstudent dffits residuals hatvalues model.matrix weights lm update coef predict formula resid
#'
#' @examples
#' z = data.frame(  # Same example as in Thorud et.al (2010).
#'         x = c(1.1, 2.2, 3.3, 4.4, 5.5),
#'         y = c(2.3, 3.1, 3.2, 3.7, 4.5))
#' LmImpute(z)  # Simple regression
#' LmImpute(z, model = "y~x-1", weights = "1/x")  # Ratio model
#'
#' rateData <- KostraData("rateData")               # Real Kostra data set
#' w <- rateData$data[, c(16, 5, 14, 15, 19)]
#' w <- w[is.finite(w[,"Ny.kostragruppe"]), ]       # Remove Longyearbyen
#' w[w[,"Ny.kostragruppe"]>13,"Ny.kostragruppe"]=13 # Combine small strata
#' names(w) = c("x", "y", "y14", "y15", "k")
#'
#' # Ratio model within each strata assumming common variance
#' LmImpute(w, "y~x:factor(k)-1", weights = "1/x", estimationGroup = w$k)
#'
#' # Similar to above, but two y variables
#' LmImpute(w, "cbind(y14,y15)~x:factor(k)-1", weights = "1/x", estimationGroup = w$k)
#'
#' # Using transformation and "BackTransform"
#' LmImpute(w, "sqrt(y)~x",BackTransform = function(x) x^2,returnYHat = TRUE)
#'
#' # Direct imputation of x
#' LmImpute(w, "I(y-x)~0",weights = "1/x",
#'   BackTransform = function(y){return(y+dynGet("data")$x)},
#'   limitModel = Inf, limitIterate = Inf, limitImpute = Inf,
#'   returnYHat = TRUE)
#'
#'
#'
LmImpute <- function(data, model = "y~x", weights = NULL, limitModel = 2.5, limitIterate = 4.5, limitImpute = 50, maxiter = 10,
                     returnIter = TRUE, returnYHat = FALSE, returnFirst = FALSE, returnLast = TRUE, returnFinal = FALSE,
                     MultiFuction = function(x) {max(abs(x))},
                     estimationGroup = TRUE, unfoldCoef = FALSE, category123 = NULL,
                     forceCategory2 = rep(FALSE, N), BackTransform=NULL,
                     warningEstimate = "estimate: Missing yImputed replaced by zero",
                     removeEmpty=FALSE,
                     NArStudHandling=warning,
                     cvPercent=TRUE,
                     returnSameType=FALSE
                     ){

  if(limitModel>limitIterate){
    limitModel <- limitIterate
    warning("limitModel set equal to limitIterate, bigger not allowed.")
  }

  if(limitImpute){
    if(limitModel>limitImpute){  # Need to change since category123
      limitModel <- limitImpute
      warning("limitModel set equal to limitImpute, bigger not allowed.")
    }
  }
  else
    estimationGroup <- FALSE


  # allow NA instead of NULL (needed for data.table .... )
  if (!is.null(category123))
    if (!sum(!is.na(category123)))
      category123 <- NULL


    #if (model == "Ratio") {
    #  N <- NROW(data$y)
    #  if (!is.logical(estimationGroup))
    #    stop("estimationGroup (returnEstimate) must be logical when \"Ratio\"")
    #  if (!is.null(category123))
    #    stop("category123 must be NULL when \"Ratio\"")
    #  if (!is.null(weights))
    #    stop("weights must be NULL when \"Ratio\"")
    #  if (!is.null(BackTransform))
    #    stop("BackTransform must be NULL when \"Ratio\"")
    #  return(RatioImpute(x = data$x, y = data$y, limitModel = limitModel, limitIterate = limitIterate, limitImpute = limitImpute,
    #                     maxiter = maxiter, returnIter = returnIter, returnFirst = returnFirst, returnYHat = returnYHat,
    #                     returnLast = returnLast, returnFinal = returnFinal,
    #                     returnEstimate=estimationGroup,
    #                     forceCategory2 =forceCategory2))
    #}
    #if (model == "CommonMean") {
    #  N <- NROW(data$y)
    #  if (!is.logical(estimationGroup))
    #    stop("estimationGroup (returnEstimate) must be logical when \"CommonMean\"")
    #  if (!is.null(category123))
    #    stop("category123 must be NULL when \"CommonMean\"")
    #  if (!is.null(weights))
    #    stop("weights must be NULL when \"CommonMean\"")
    #  if (!is.null(BackTransform))
    #    stop("BackTransform must be NULL when \"CommonMean\"")
    #  return(RatioImpute(x = NULL, y = data$y, limitModel = limitModel, limitIterate = limitIterate, limitImpute = limitImpute,
    #                     maxiter = maxiter, returnIter = returnIter, returnFirst = returnFirst, returnYHat = returnYHat,
    #                     returnLast = returnLast, returnFinal = returnFinal,
    #                     returnEstimate=estimationGroup,
    #                     forceCategory2 =forceCategory2))
    #}

    if (is.null(weights))
      weights <- "NULL"  # since parse


    # In the general case data$x and data$y cannot be used
    y <- GetY(data, model)

    N <- NROW(y)
    nY <- NCOL(y)

    yIsNa <- IsNaMulti(y)
    xIsNa <- IsNaMulti(GetX(data, model))

    # Hack to detect when model is "I(y-NaToZero(x))~0"
    historyModel <- grepl("NaToZero\\(x\\)",gsub(" ","",model))

    if(!historyModel){
      if(!any(!yIsNa))
        stop("No nonmissing y observations")
    }

    emptyX = !length(xIsNa)


    if(emptyX){
      if(historyModel)
        xIsNa <- IsNaMulti(data$x) # Hack to detect missing x when model is "I(y-NaToZero(x))~0"
      else
        xIsNa=rep(FALSE,length(yIsNa))
    }


    if (is.logical(estimationGroup)) {
      if (estimationGroup)
        estimationGroup <- rep(1, N) else estimationGroup <- NULL
    }





    if (is.null(category123)) {
      lmSubset <- !(yIsNa|xIsNa|forceCategory2)

      # Avoid warning when no imputation
      if(any(!lmSubset))
        SW <- function(x) x
      else SW <- suppressWarnings

      # The iteration part
      z <- SW(LmIterate(data = data, model = model, weights = weights, subset = lmSubset,
                     limit = limitIterate, maxiter = maxiter, returnIter = returnIter,
                     returnFirst = returnFirst, MultiFuction = MultiFuction, NArStudHandling=NArStudHandling))

      nY <- NCOL(z$rStud)


      if (!is.null(estimationGroup))
        estimationGroup <- droplevels(as.factor(estimationGroup))

      p <- SW(MlmSePredict(z$m, newdata = data, weights = weights))


      rStudExtern <- (y - p$fit)/p$se.pred


      z$rStud <- FillSubset(GetSubset(rStudExtern, !z$subset), subset = !z$subset, fillInn = z$rStud)


      z$absrStud[!z$subset] <- AbsMulti(GetSubset(z$rStud,!z$subset), MultiFuction)


      d <- NULL
      d$category123 <- rep(1, N)
      d$category123[z$absrStud >= limitModel] <- 2
      if(limitImpute)
        d$category123[z$absrStud >= limitImpute] <- 3
      d$category123[yIsNa] <- 3
      d$category123[xIsNa] <- 0
      d$category123[forceCategory2] <- 2
      cat3 <- d$category123 == 3


      # Below is several if statments to avoid unnecessary computations Is a new regression computation needed?  What is needed in output

      if (returnLast | ((z$maxrStud < limitModel) & returnFinal)) {
        a <- GetlmResults(z$m, rStud = z$rStud, subset = z$subset, unfoldCoef = unfoldCoef)
        a$leaveOutResid <- FillSubset(GetSubset((y - p$fit), !z$subset), subset = !z$subset, fillInn = a$leaveOutResid)

        if(!is.null(BackTransform)){
          yBack           <- BackTransform(y)
          yFitBack        <- BackTransform(y-a$leaveOutResid)
          a$leaveOutResid <- yBack - yFitBack
        }

        a$sigmaHat <- p$residual.scale
        a$rStud <- z$rStud

        if (!is.null(estimationGroup)) {
          ps <- PredictSum(z$m, data, estimationGroup[cat3], returnEstimate = FALSE, subset = cat3, sePredictObject = p,
                           adjresid=GetSubset(a$leaveOutResid,z$subset))     #adjresid=a$leaveOutResid[z$subset])
          a$seEstimate <- ps$seEstimate
          a$seRobust <- ps$seRobust
        }
      } else a <- NULL
      z$rStud <- NULL  # copy in a
    } else {
      # END if(is.null(category123))
      z <- NULL
      z$maxrStud <- Inf
      d <- NULL
      d$category123 <- category123
      cat3 <- d$category123 == 3
      returnFinal <- (returnFinal | returnLast)
      returnLast <- FALSE

    }


    # Avoid warning when no imputation
    if(any(cat3))
      SW <- function(x) x
    else SW <- suppressWarnings


    if (z$maxrStud >= limitModel) {
      zsubset <- d$category123 == 1
      zm <- try(SW(lm(as.formula(model), data = data, subset = zsubset, weights = eval(parse(text = weights)), na.action = na.pass)))
      lmError=FALSE
      if(class(zm)[1]=="try-error") lmError=TRUE
      else{
        if("m" %in% names(z)) if(length(coef(zm)) != length(coef(z$m))) lmError=TRUE  #is.null(z$m) not working since z$maxrStud
      }
      if(!lmError){
          b <- GetlmResults(zm, subset = zsubset, unfoldCoef = unfoldCoef)
          if(any(!is.finite(GetSubset(b$rStud,zsubset)))) {
            if("m" %in% names(z)) lmError=TRUE else
              SW(NArStudHandling("Missing values of rStudent with this category123 input."))
          }
      }
      #print(lmError)
      if(lmError){
        if(!("m" %in% names(z))) stop("lm not working with this category123 input.")
        limitModel=Inf
        d$category123[z$subset] <- 1
        warning("To avoid estimation problems limitModel is not used.")
      }  else {
        z$subset <- zsubset
        z$m <- zm
        p <- NULL
      }
    }


    if (returnFinal) {
      if (z$maxrStud < limitModel)
        b <- a else {
          #b <- GetlmResults(z$m, subset = z$subset, unfoldCoef = unfoldCoef) ... Moved up
          #print(z$m)
          #print("hei")
          p <- SW(MlmSePredict(z$m, newdata = data, weights = weights))
          b$leaveOutResid <- FillSubset(GetSubset((y - p$fit), !z$subset), subset = !z$subset, fillInn = b$leaveOutResid)

          if(!is.null(BackTransform)){
            yBack           <- BackTransform(y)
            yFitBack        <- BackTransform(y-b$leaveOutResid)
            b$leaveOutResid <- yBack - yFitBack
          }

          b$sigmaHat <- p$residual.scale
          rStudExtern <- (y - p$fit)/p$se.pred
          b$rStud <- FillSubset(GetSubset(rStudExtern, !z$subset), subset = !z$subset, fillInn = b$rStud)
          if (!is.null(estimationGroup)) {
            ps <- PredictSum(z$m, data, estimationGroup[cat3], returnEstimate = FALSE, subset = cat3, sePredictObject = p,
                             adjresid=GetSubset(b$leaveOutResid,z$subset))     #adjresid=a$leaveOutResid[z$subset])
            b$seEstimate <- ps$seEstimate
            b$seRobust <- ps$seRobust
          }
        }
    } else b <- NULL


    if (!returnLast)
      a <- NULL

    if (returnLast & returnFinal)
      names(a) <- paste(names(a), "Last", sep = "")


    if (is.null(p)){
      if(emptyX & nY>1) p = SW(MlmSePredict(z$m, newdata = data, weights = weights))
      else p$fit <- SW(predict(z$m, newdata = data))  # predict works when se.fit = FALSE But not when empty model
    }

    # Remove elements not wanted in output (can be changed)
    z$m <- NULL
    z$absrStud <- NULL
    z$subset <- NULL
    z$maxrStud <- NULL

    # Add elements
    d$N <- N


    if (returnYHat){
      if(!is.null(BackTransform)) d$yHat <- BackTransform(p$fit)
      else d$yHat <- p$fit
    }

    yIsInteger <- is.integer(y[1])


    if(limitImpute){
      d$yImputed <- FillSubset(GetSubset(p$fit, cat3), subset = cat3, fillInn = y)
      if(!is.null(BackTransform)) d$yImputed <- BackTransform(d$yImputed)

      yImputedIsInteger <- is.integer(d$yImputed[1])
      if(yIsInteger){
        if(returnSameType){
          if(!yImputedIsInteger)
            d$yImputed <- Round2Integer(d$yImputed)
        }else{
          if(yImputedIsInteger)
            d$yImputed[] <- as.numeric(d$yImputed)
        }
      }

      d$nImputed <- sum(cat3)
    }
    else{
      d$outlier <- as.integer(d$category123 > 1) # as.integer
      d$category123 <- NULL
    }

    if (!is.null(estimationGroup)) {
      estMatrix <- makeDummy(estimationGroup)  #
      #d$estimate <- DropCol(t(estMatrix) %*% d$yImputed)
      d$estimate <- DropCol(t(estMatrix) %*% NaToZero(d$yImputed,warningEstimate))

      if(yIsInteger & returnSameType) d$estimate <- Round2Integer(d$estimate)

      if (returnFinal) d$cv <-       (99*as.numeric(cvPercent) + 1) *(b$seEstimate/d$estimate)
      else if (returnLast) d$cv <-   (99*as.numeric(cvPercent) + 1) *(a$seEstimate/d$estimate)
      if (returnYHat)
        d$estimateYHat <- DropCol(t(estMatrix) %*% d$yHat)
      if(!is.null(BackTransform)) y <- BackTransform(y)
      y[is.na(y)] <- 0
      d$estimateOrig <- DropCol(t(estMatrix) %*% y)
    }
    #options(na.action = old.na.action) # Denne brukes ikke og er heller ikke riktig...

    if(!removeEmpty) return(c(d, a, b, z))
    out = c(d, a, b, z)
    ll = lapply(out,length)==0
    if(any(ll)) out = out[!ll]
    out
}

Round2Integer <- function(x){
  m <- is.finite(x) # correct dimension and col/row names when matrix
  m[] <- as.integer(round(x))
  if(!is.integer(m[1])) stop("Something went wrong when rounding to integer")
  m
}



LmIterate <- function(data, model = "y~x", weights = NULL, subset = !(IsNaMulti(data$y)|IsNaMulti(data$x)),
                      limit = 4.5, maxiter = 10, returnFirst = FALSE, returnIter = TRUE,
                      MultiFuction = function(x) {max(abs(x))}, NArStudHandling = warning) {
  subsetInput=subset
  if (!is.logical(subset))
    stop("subset must be logical")
  if (is.null(weights))
    weights <- "NULL"  # since parse below
  N <- length(subset)
  z <- NULL
  if (returnIter)
    z$iter <- as.numeric(!subset)
  if (returnFirst)
    z$rStudFirst <- rep(NaN)
  z$maxrStud <- Inf
  i <- 1
  if (returnIter)
    z$iter <- as.numeric(!subset)
  rFALSE <- NULL  # Needed in first iteration


  while (z$maxrStud >= limit & i <= maxiter) {
    subsetOld=subset
    subset[subset][rFALSE] <- FALSE
    #z$m <- lm(as.formula(model), data = data, subset = subset, weights = eval(parse(text = weights)), na.action = na.pass)  # Missing will cause error.
    z$m <- try(lm(as.formula(model), data = data, subset = subset, weights = eval(parse(text = weights)), na.action = na.pass), silent = TRUE)

    lmError=FALSE
    if(class(z$m)[1]=="try-error"){
      if(i==1) stop(paste("Linear regression failed:",as.character(z$m)))
      lmError=TRUE
    } else {
      rStud <- MlmFunction(rstudent, z$m)  #rstudent(z$m) is wrong when multiple responses
      if (i == 1 & returnFirst) {
        z$rStudFirst <- FillSubset(rStud, subset)
        z$sigmaFirst <- GetSigma(z$m)
      }
      absrStud <- AbsMulti(rStud, MultiFuction)
      if(length(absrStud)==0) z$maxrStud=0  # avoid warning
      else
        z$maxrStud <- max(c(0,absrStud), na.rm = TRUE) #max(0, max(absrStud, na.rm = TRUE)) # avoid warning
      if(i==1) nCoef = length(coef(z$m))
      else
        if(length(coef(z$m)) != nCoef) lmError=TRUE
      if(any(!is.finite(absrStud))){
        if(i==1) NArStudHandling("Missing values of rStudent.")
        else lmError=TRUE
      }
    }
    if(lmError){
      if(i==1) stop(paste("Linear regression failed"))
      warning("Iteration limit changed and limitModel not used to avoid estimation problems.")
      zReturn = LmIterate(data=data,model=model,weights=weights,subset=subsetInput,limit=limit,maxiter=i-1,
                       returnFirst=returnFirst,returnIter=returnIter,MultiFuction=MultiFuction,NArStudHandling=NArStudHandling)
      zReturn$absrStud[subsetOld]=0
      return(zReturn)
    }

    rFALSE <- absrStud >= limit  #### Omskrives til mult
    i <- i + 1
    if (returnIter)
      z$iter[subset][rFALSE] <- i
  }

  z$rStud <- FillSubset(rStud, subset)
  z$absrStud <- FillSubset(absrStud, subset)
  z$subset <- subset
  if (z$maxrStud > limit)
    warning("Iteration limit exceeded.")
  z
}


# Several functions created to handle multiple responses (mlm object instead of lm object)


#' INTERNAL FUNCTION: Replace missing values by zero
#'
#' @param x Input data
#' @param warn Warning text when non-null
#'
#' @return Input data where missing is replaced by zero
#' @export
#'
#' @examples
#' NaToZero(c(1,2,NA,4))
#'
NaToZero <- function(x,warn=NULL){
  if(!anyNA(x)) return(x)
  if(!is.null(warn)) warning(warn)
  x[is.na(x)] <- 0
  x
}


AbsMulti <- function(x, MultiFuction = function(x) {
  max(abs(x))
}) {
  if (NCOL(x) <= 1)
    return(abs(x))
  apply(x, 1, MultiFuction)
}

IsNaMulti <- function(x) {
  if (NCOL(x) <= 1)
    return(as.vector(is.na(x)))   ### as.vector ny
  apply(x, 1, anyNA)
}

GetSigma <- function(lmObject) {
  if (class(lmObject)[1] == "lm")
    return(summary(lmObject)$sigma)
  sapply(summary(lmObject), function(x) {
    x$sigma
  })
}

# http://grokbase.com/t/r/r-help/042s54v7py/r-structure-of-mlm-objects
Mlm2lm <- function(lmObject, i) {
  for (k in c("coefficients", "residuals", "effects", "fitted.values")) {
    if(!is.null(lmObject[[k]])) lmObject[[k]] <- lmObject[[k]][, i]
  }
  class(lmObject) <- "lm"
  lmObject
}

# Run a 'lm-function' on mlm object
MlmFunction <- function(f, lmObject) {
  classlmObject <- class(lmObject)[1]
  if (classlmObject == "lm")
    return(f(lmObject))
  x <- 1:NCOL(lmObject[["residuals"]])
  names(x) <- colnames(lmObject[["residuals"]])
  z <- sapply(x, function(i) f(Mlm2lm(lmObject, i)))
  if(!is.matrix(z)) z <- t(z) # handle on obsevation
  z
}

# mlm variant of SePredict below
MlmSePredict <- function(lmObject, newdata = NULL, weights = "1", returnMatrix = FALSE, residual.scale2matrix=TRUE) {
  classlmObject <- class(lmObject)[1]
  if (classlmObject == "lm"){
    return(SePredict(lmObject, newdata = newdata, weights = weights))
  }
  x <- 1:NCOL(lmObject[["residuals"]])
  names(x) <- colnames(lmObject[["residuals"]])
  m <- sapply(x, function(i) SePredict(Mlm2lm(lmObject, i), newdata = newdata, weights = weights))
  if (returnMatrix)
    return(m)
  z <- vector("list", dim(m)[1])
  names(z) <- rownames(m)
  for (j in 1:dim(m)[1]) z[[j]] <- sapply(x, function(i) m[j, ][[i]])
  if(residual.scale2matrix) z$residual.scale <- t(z$residual.scale) # Row matrix when multiple y
  z
}

# Run predict and add 'se.pred'
SePredict <- function(lmObject, newdata = NULL, weights = "1") {
  z <- predict(lmObject, newdata = newdata, weights = eval(parse(text = weights)), se.fit = TRUE)
  weig <- with(newdata, eval(parse(text = weights)))
  if (is.null(weig))
    weig <- 1
  if(length(z$fit) != length(z$se.fit)){ # Solve bug in "predict" when empty model
    ## if(max(c(0,z$se.fit),na.rm=TRUE)>0) stop("Something wrong empty model bug...")
    z$se.fit = max(c(0,z$se.fit),na.rm=TRUE)*z$fit  # 0 men tar max for sikkerhetsskyld
  }
  z$se.pred <- sqrt(z$se.fit^2 + (z$residual.scale)^2/weig)
  z
}

# Fill x (vector or matrix) into subset of fillInn When fillInn=NULL NA vector or matrix is created
FillSubset <- function(x, subset = NULL, fillInn = NULL) {
  if (is.null(subset))
    return(x)
  if (!is.logical(subset))
    stop("subset must be logical")
  N <- length(subset)
  n <- sum(subset)
  if (n != NROW(x))
    stop("sum(subset)!=NROW(x)")
  if (n == N)
    return(x)
  if (!is.null(fillInn)) {
    if(n==0)  return(fillInn)  ### Handle some problems here when empty subset ..
    if (N != NROW(fillInn))
      stop("length(subset) !=NROW(fillInn)")
    if (NCOL(x) != NCOL(fillInn))
      stop("NCOL(x) !=NCOL(fillInn)")
  }
  if(is.null(ncol(x))) x=as.vector(x) ## Handle problem with AsIs-class in special cases
  if (is.vector(x)) {
    if (is.null(fillInn))
      fillInn <- rep(NaN, N)
    fillInn[subset] <- x
    return(fillInn)
  }
  if (is.null(fillInn)){
    fillInn <- matrix(NaN, N, ncol(x))
    colnames(fillInn) <- colnames(x)
  }
  fillInn[subset, ] <- x
  fillInn
}

GetSubset <- function(x, subset) {
  if (is.null(subset))
    return(x)
  if(is.null(ncol(x))) x=as.vector(x) ## Handle problem with AsIs-class in special cases
  if (is.vector(x))
    return(x[subset])
  return(x[subset, , drop = FALSE])
}

# Get some results from ml or mlm obejct using FillSubset
GetlmResults <- function(lmObject, rStud = NULL, subset = NULL, unfoldCoef = FALSE) {
  coeflmObject <- coef(lmObject)
  if(is.logical(unfoldCoef)){
    if (unfoldCoef & NROW(coeflmObject) > 1) {
      z <- as.list(data.frame(coef = t(coeflmObject)))
    } else {
      z <- NULL
      z$coef <- coeflmObject
    }
  } else{
    if(unfoldCoef==2){
      if(NROW(coeflmObject)<=1){
        if(NROW(coeflmObject)==0)
          coef  <- 0
        else
          coef  <- coeflmObject
        coefB <- 0
        } else {
          coeflmObject <- as.matrix(coeflmObject)
          coef  <- coeflmObject[1,]
          coefB <- coeflmObject[2,]
          }
      z <- as.list(data.frame(coef=coef,coefB=coefB))
      } else stop("This variant of unfoldCoef is not implemented")
  }
  if (!is.null(subset))
    z$nModel <- sum(subset)
  if (is.null(rStud))
    z$rStud <- FillSubset(MlmFunction(rstudent, lmObject), subset) else z$rStud <- rStud
    z$dffits <- FillSubset(MlmFunction(dffits, lmObject), subset)
    if(NROW(coeflmObject)==0) z$hii <- FillSubset(0*residuals(lmObject), subset) # Fix bug in hatvalues when empty model
    else z$hii <- FillSubset(hatvalues(lmObject), subset)
    z$leaveOutResid <- FillSubset(residuals(lmObject), subset)/(1 - z$hii)
    z
}

# Get response from model formula Kan bruke model[[1]] fra lm-objekt ... endret til X
GetX <- function(data, model = "y~x",getY=FALSE) {
  s <- as.character(as.formula(model))
  if (length(s) == 3)
    if (s[1] == "~")
      #return(with(data, eval(parse(text = s[2+as.numeric(!getY)]))))   ## Denne virker ikke på "x:strata"
      return(model.frame(paste("~",s[2+as.numeric(!getY)]),data=data,na.action = na.pass))

  stop("as.character(as.formula NOT working")
}

GetY <- function(data, model = "y~x") {
  m   <- GetX(data,model,TRUE)
  cm1 <- colnames(m[[1]])
  m   <- as.matrix(m)
  if(!is.null(cm1)) colnames(m) <- cm1 # Fix names when matrix embedded in one variable
  m[,,drop=(NCOL(m)==1)]
}


DropCol <- function(x) {
  if (NCOL(x) <= 1)
    x <- x[, , drop = TRUE]
  if (length(x) <= 1)
    names(x) <- NULL  # Prevent Warning: row names were found from a short variable and have been discarded
  x
}

# Function not in use Calculate by matrix operation instead of lm functions
HatMatrix <- function(m) {
  inv <- function(x) ginv(x)
  x <- model.matrix(m)
  weightsm <- weights(m)
  if (is.null(weightsm))
    sqrt_w <- 1 else sqrt_w <- sqrt(weightsm)
  x %*% inv(x * sqrt_w) * sqrt_w
}

# Function not in use Code integrated in PredictSum newX må være data.frame list kan fungere, men kna da ikke bruke dim
TotalWeights <- function(m, newX, group = rep(1, dim(newX)[1]), estMatrix = makeDummy(group)) {
  inv <- function(x) ginv(x)
  x <- model.matrix(m)
  weightsm <- weights(m)
  if (is.null(weightsm))
    sqrt_w <- 1 else sqrt_w <- sqrt(weightsm)
  txTotals <- t(model.matrix(m, data = newX, na.action = NULL)) %*% estMatrix
  tinvw <- t(inv(x * sqrt_w)) * sqrt_w
  tinvw %*% txTotals
}


RobustVarOld <- function(w, r, wwplus1 = TRUE) {   # Not in use
  w <- as.matrix(w)
  r <- as.matrix(r)
  plus1 <- as.numeric(wwplus1)
  x <- matrix(NaN, NCOL(w), NCOL(r))
  for (i in 1:NCOL(w)) for (j in 1:NCOL(r)) x[i, j] <- sum(w[, i] * (w[, i] + plus1) * r[, j]^2)
  return(x)
}



RobustVar <- function(w, r, wwplus1 = TRUE, fixNegative = TRUE) {

  w <- as.matrix(w)
  ww <- w*w
  rr <- (as.matrix(r))^2
  #print(dim(ww))
  x <- matrix(0, NCOL(ww), NCOL(rr))
  if(NROW(ww)==0) return(x)
  if(NROW(rr)==0) return(x)
  if(wwplus1){
    if(fixNegative & min(w)<0){
      s1 <- colSums(w)
      w[w<0]=0
      s2 <- colSums(w)
      w <- w*t(matrix(s1/s2,dim(w)[2],dim(w)[1]))
    }
    ww <- ww +w   # Bare siste ledd endret p.g.a negative vekter
  }
  for (i in 1:NCOL(ww)) for (j in 1:NCOL(rr)) x[i, j] <- sum(ww[, i]* rr[, j])
  return(x)
}





# Find difference between 'se.pred'^2 and 'se.fit'^2
PredVarComponent <- function(estMatrix, sePredictObject, subset) {
  if (!sum(subset))
    return(0)
  if (!length(estMatrix))
    return(0)
  if(length(as.matrix((sePredictObject$se.pred)))==0) return(0)
  t(estMatrix) %*% as.matrix(sePredictObject$se.pred^2 - sePredictObject$se.fit^2)[subset, ]
}


# Predict a sum(s) whitin groups or more generally linear combinatins using 'estMatrix'
# Calculate se of these estimates Robust se also possible
# subset is subset of newData not group
PredictSum <- function(m, newData, group = rep(1, nNew), estMatrix = makeDummy(group), returnEstimate = TRUE,
                       returnRobust = TRUE, returnwMatrix = FALSE, subset = NULL, sePredictObject,adjresid=NULL) {
  inv <- function(x) {
    if(length(x)==0) return(t(x))
    ginv(x)  # generalized inverse
  }
  a <- NULL
  x <- model.matrix(m)
  # remove y from model to avoid observation removed since NA
  # but "na.action = na.pass" inclued later since missing x allowed
  #xNew <- GetSubset(model.matrix(update(formula(m), NULL ~ .), data = newData, na.action = NULL), subset)

  #xNew <- GetSubset(model.matrix(update(formula(m), NULL ~ .), data = newData,na.action = na.pass), subset)

  # Her settes na.pass to steder. Alternativt kan
  formulaNoY <- update(formula(m), NULL ~ .)
  xNew <- GetSubset(model.matrix(formulaNoY, data =
                                   model.frame(formulaNoY,newData,na.action = na.pass),
                                 na.action = na.pass), subset)

  if(!identical(names(x),names(xNew)))
    stop("Something wrong happened when using the function model.matrix")


  weightsm <- weights(m)
  if (is.null(weightsm))
    sqrt_w <- 1 else sqrt_w <- sqrt(weightsm)
  nNew <- dim(xNew)[1]

  txTotals <- t(xNew) %*% estMatrix  # x-totals of for estimation groups


  if(length(x)==0) tinvw <- t(inv(x))   # Avoid dimension "bug"
  else tinvw <- t(inv(x * sqrt_w)) * sqrt_w
  a$wMatrix <- tinvw %*% txTotals  # weigths for calculation of y-totals

  res <- as.matrix(resid(m) * sqrt_w)
  covMatrix <- (t(res) %*% res)/m$df

  if (returnEstimate)
    a$estimate <- t(a$wMatrix) %*% m$model[[1]]  # m$model[[1]] = y


  a$seEstimate <- DropCol(sqrt(as.matrix(colSums((a$wMatrix/sqrt_w)^2)) %*%
                                 t(as.matrix(diag(covMatrix))) + PredVarComponent(estMatrix, sePredictObject, subset)))

  if(nNew==0) # fix NA problem
    a$seEstimate[is.na(a$seEstimate)] = 0

  if (returnRobust) {
    if(is.null(adjresid)){
      # Denne brukes ikke og er ikke sjekket ..
      hii <- rowSums(tinvw * x)  # adjresid as input instead  of repeating calculation here?
      adjresid <- resid(m)/(1 - hii)  # nan-problemer her Hva med dim(hii)
    }
    # a$seRobust = DropCol(sqrt(RobustVar(a$wMatrix,adjresid)))
    a$seRobust <- DropCol(Sqrt0(RobustVar(a$wMatrix, adjresid)))
  }


  if (!returnwMatrix)
    a$wMatrix <- NULL
  a
}


# Dummy matrix from factor variable
makeDummy <- function(x) {
  x <- as.factor(x)
  if (length(levels(x)) <= 1)
    m <- matrix(as.numeric(x), length(x), 1) else m <- model.matrix(~x - 1)[, ]
    colnames(m) <- levels(x)
  NaToZero(m) # Mulig å sette NA på observasjoner som ikke skal bli med
}

# Avoid sqrt of neg value
Sqrt0 <- function(x) sqrt(pmax(x, 0))





















