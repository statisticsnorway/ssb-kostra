
#' Examples of wrappers based on GetData
#'
#' Only partly tested
#'
#' @return See the other functions
#'
#' @details
#'
#' \strong{\code{ImputeHistoryW}} and similar functions takes yVar and yearName as input insetad of xName and yName.
#'
#' \strong{\code{ImputeRegressionW}} and \strong{\code{OutlierRegressionW}} and similar functions takes yearName as extra parameter.
#' Output is from last year.
#'
#' \strong{\code{ImputeRegression2W}} and similar functions takes xName and yearName as input insetad of x1Name and x2Name. The
#' function takes y using yName from last year, x1 using yName from the previous year and x2 using yName from last year.
#'
#' @details
#' Here yearName must be a variable name. For the other variables variable numbers will also work.
#'
#' @export
#'
#' @examples
#' rateData <- KostraData("rateData")             # Real Kostra data set
#' w <- rateData$data[, c(17,19,16,5)]        # Data with id, strata, x and y
#'
#' w <- w[is.finite(w[,"Ny.kostragruppe"]), ]       # Remove Longyearbyen
#' w[w[,"Ny.kostragruppe"]>13,"Ny.kostragruppe"]=13 # Combine small strata
#'
#' # Create historical data by modifying the "original x-variable"
#' w2=cbind(w,x1=1.2*w[,3]*rep(c(NA,NA,1,1),107),x2=1.1*w[,3]*rep(c(NA,1),214))
#'
#' # Create stacked variant of data
#' w3 = Stack(w2,c(4,5,6,3),1:2,data.frame(aar=2013:1026),"y")
#'
#' a <- ImputeHistory(w2, strataName = 2, xName=c(5,6,3), yName=4)
#' b <- ImputeHistoryW(w3, idName=1, strataName = 2,   yVar="y", yearName="aar")
#' identical(a,b)
#'
#' a = ImputeHistoryTallSmall(w2, strataName = 2, xName=c(5,6,3), yName=4)
#' b = ImputeHistoryTallSmallW(w3, idName=1, strataName = 2,   yVar="y", yearName="aar")
#' identical(a,b)
#'
#' a <- ImputeRegression(w, strataName = 2, method="ratio")
#' b <- ImputeRegressionW(cbind(w,aar=2016), idName=1, strataName = 2, xName=3, yName=4, yearName="aar", method="ratio")
#' identical(a,b)
#'
#' a <- ImputeRegressionTallSmall(w, strataName = 2, method="ratio")
#' b <- ImputeRegressionTallSmallW(cbind(w,aar=2016), idName=1, strataName = 2, xName=3, yName=4, yearName="aar", method="ratio")
#' identical(a,b)
#'
#' a <- OutlierRegressionMicro(w, strataName = 2, method="ratio")
#' b <- OutlierRegressionMicroW(cbind(w,aar=2016), idName=1, strataName = 2, xName=3, yName=4, yearName="aar", method="ratio")
#' identical(a,b)
#'
#'
GetDataWrappers = function(){
  print("See the other functions ...")
}

# Forklaring:
# c(strataName,lastyear) blir en liste med to elementer siden lastyear er liste (selv om strataName ikke er liste).
# lastyear er en liste med ett element med verdi max(years) og navn yearName.
# Bruk av [[yearName]] er en vei å få til navn. c(strataName,lastyear) blir ca lik list(strataName,lastyear=max(years)),
# men da tenker man ikke på hva som er variabel og hva som er variabelnavn.
#
# oldyears er nesten tilsvarende, men isteddenfor en verdi er det mange og istendenfor at disse verdiene er en vanlig
# vektor så er det en en-rads matrise. Dette oppnås med t(). Da vil GetData lage lage flere variabler/kolonner.
#

#' @rdname GetDataWrappers
#' @export
ImputeHistoryW = function(data, idName, strataName = NULL,
                          yVar, yearName,fun = ImputeHistory, ...){
  years <-  sort(unique(data[, yearName]), decreasing = TRUE)
  lastyear <- list()
  oldyears <- list()
  lastyear[[yearName]] <- max(years)
  oldyears[[yearName]] <- t(years[years!=lastyear])
  idName = c(idName,lastyear)
  if(!is.null(strataName)) strataName  <-  c(strataName,lastyear)
  xName <- c(yVar,oldyears)
  yName <- c(yVar,lastyear)
  fun(data=data,idName = idName, strataName = strataName,
      xName = xName, yName = yName, ...)
}

#' @rdname GetDataWrappers
#' @export
ImputeHistoryNewNamesW = function(..., fun = ImputeHistoryNewNames){
  ImputeHistoryW(..., fun=fun)
}

#' @rdname GetDataWrappers
#' @export
ImputeHistoryTallW = function(..., fun = ImputeHistoryTall){
  ImputeHistoryW(..., fun=fun)
}

#' @rdname GetDataWrappers
#' @export
ImputeHistoryTallSmallW = function(..., fun = ImputeHistoryTallSmall){
  ImputeHistoryW(..., fun=fun)
}

#' @rdname GetDataWrappers
#' @export
ImputeHistoryWideW = function(..., fun = ImputeHistoryWide){
  ImputeHistoryW(..., fun=fun)
}

#' @rdname GetDataWrappers
#' @export
ImputeHistoryTallWideWSmall = function(..., fun = ImputeHistoryWideSmall){
  ImputeHistoryW(..., fun=fun)
}

#' @rdname GetDataWrappers
#' @export
ImputeRegressionW <- function(data, idName, strataName, xName, yName, yearName, fun = ImputeRegression, ...){
  lastyear <- list()
  lastyear[[yearName]] <- max(data[, yearName])
  fun(data=data,
      idName = c(idName,lastyear),
      strataName = c(strataName,lastyear),
      xName = c(xName,lastyear),
      yName = c(yName,lastyear), ...)
}

#' @rdname GetDataWrappers
#' @export
ImputeRegressionNewNamesW = function(..., fun = ImputeRegressionNewNames){
  ImputeRegressionW(..., fun=fun)
}

#' @rdname GetDataWrappers
#' @export
ImputeRegressionTallW = function(..., fun = ImputeRegressionTall){
  ImputeRegressionW(..., fun=fun)
}

#' @rdname GetDataWrappers
#' @export
ImputeRegressionTallSmallW = function(..., fun = ImputeRegressionTallSmall){
  ImputeRegressionW(..., fun=fun)
}

#' @rdname GetDataWrappers
#' @export
ImputeRegressionWideW = function(..., fun = ImputeRegressionWide){
  ImputeRegressionW(..., fun=fun)
}


#' @rdname GetDataWrappers
#' @export
ImputeRegressionWideSmallW = function(..., fun = ImputeRegressionWideSmall){
  ImputeRegressionW(..., fun=fun)
}


#' @rdname GetDataWrappers
#' @export
OutlierRegressionW  = function(..., fun = OutlierRegression){
  ImputeRegressionW(..., fun=fun)
}


#' @rdname GetDataWrappers
#' @export
OutlierRegressionTallW  = function(..., fun = OutlierRegressionTall){
  ImputeRegressionW(..., fun=fun)
}


#' @rdname GetDataWrappers
#' @export
OutlierRegressionWideW  = function(..., fun = OutlierRegressionWide){
  ImputeRegressionW(..., fun=fun)
}

#' @rdname GetDataWrappers
#' @export
OutlierRegressionMicroW  = function(..., fun = OutlierRegressionMicro){
  ImputeRegressionW(..., fun=fun)
}

#' @rdname GetDataWrappers
#' @export
ImputeRegression2W <- function(data, idName, strataName, xName, yName, yearName, fun = ImputeRegression2, ...){
  years <- unique(data[, yearName])
  lastyear <- list()
  oldyears <- list()
  lastyear[[yearName]] <- max(years)
  oldyear[[yearName]]  <- max(years[years!=lastyear])
  fun(data=data,
      idName = c(idName,lastyear),
      strataName = c(strataName,lastyear),
      x1Name = c(yName, oldyear),
      x2Name = c(xName,lastyear),
      yName = c(yName,lastyear), ...)
}

#' @rdname GetDataWrappers
#' @export
ImputeRegression2NewNamesW = function(..., fun = ImputeRegression2NewNames){
  ImputeRegression2W(..., fun=fun)
}

#' @rdname GetDataWrappers
#' @export
ImputeRegression2TallW = function(..., fun = ImputeRegression2Tall){
  ImputeRegression2W(..., fun=fun)
}

#' @rdname GetDataWrappers
#' @export
ImputeRegression2TallSmallW = function(..., fun = ImputeRegression2TallSmall){
  ImputeRegression2W(..., fun=fun)
}

#' @rdname GetDataWrappers
#' @export
ImputeRegression2WideW = function(..., fun = ImputeRegression2Wide){
  ImputeRegression2W(..., fun=fun)
}


#' @rdname GetDataWrappers
#' @export
ImputeRegression2WideSmallW = function(..., fun = ImputeRegression2WideSmall){
  ImputeRegression2W(..., fun=fun)
}











