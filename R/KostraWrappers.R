#' SimpleHb
#'
#' @encoding UTF8
#'
#' @param data Input of Hb is a data set of class data.frame.
#' @param idName Name of the identification variable.
#' @param periodName Name of the period identification variable.
#' @param measureName Name of the variable to edit.
#' @param period value of period to edit.
#' @param periodCompare value of period to edit against.
#' @param pA Parameter that adjusts for small differences between the median and the 1st or 3rd quartile. Default value 0.05.
#' @param pU Parameter that adjusts for different level of the variables.Default value 0.5.
#' @param pC Parameter that controls the length of the confidence interval.Default value 20.
#' @param strataName Name of stratification variable. Optional. If strataName is given, the HB method is performed within each stratum.
#'
#' @export
#'
#' @return see Hb.r
#'
# Methodlibrary invocation:
# SimpleHb(data,
#          idName,
#          periodName,
#          measureName,
#          period,
#          periodCompare,
#          get0("pU", ifnotfound = 0.5),
#          get0("pA", ifnotfound = 0.05),
#          get0("pC", ifnotfound = 20),
#          strataName = NULL);
SimpleHb <- function(data, idName, periodName, measureName, period, periodCompare, pU, pA, pC, strataName) {
  idList <- list(idName, period);
  names(idList) <- c("id", periodName);
  measureList <- list(measureName, period);
  names(measureList) <- c("id", periodName);
  measureListCompare <- list(measureName, periodCompare);
  names(measureListCompare) <- c("id", periodName);
  Hb(data, idList, measureList, measureListCompare, pU, pA, pC, strataName);
}


#' SimpleImputeRegressionWide
#'
#' @note Øyvind: Denne funksjonen i Kostra-pakken opprinnelig skrevet av Håkon ser ikke ut til å være i bruk.
#' Det brukes kode utenfor. Har endret funksjonen til å være som siste emailutveksling med Bjørn-Andre
#' Har ikke oppdatert dokumentasjonen ellers.
#'
#'
#' @encoding UTF8
#'
#' @param data Input data set of class data.frame
#' @param idName Name of id-variable
#' @param periodName Name of the period identification variable.
#' @param period value of period to edit.
#' @param measureName Name of the variable to edit.
#' @param compareName Name of the variable to edit against.
#' @param strataName Name of starta-variable. Single strata when NULL (default)
#' @param methodName  The method (model and weight) coded as a string: "ordinary" (default), "ratio", "noconstant",
#'        "mean" or "ratioconstant".
#' @param limitModel  Studentized residuals limit. Above limit -> group 2.
#' @param limitIterate Studentized residuals limit for iterative calculation of studentized residuals.
#' @param limitImpute  Studentized residuals limit. Above limit -> group 3.
#'
#' @export
#'
#' @return see ImputeRegression.R
#'
# Methodlibrary invocation:
#
# SimpleImputeRegressionWide(data,
#                            idName,
#                            periodName,
#                            period,
#                            measureName,
#                            compareName,
#                            strataName,
#                            methodName,
#                            get0("limitModel", ifnotfound = 2.5),
#                            get0("limitIterate", ifnotfound = 4.5),
#                            get0("limitImpute", ifnotfound = 50));
SimpleImputeRegressionWide <- function(data, id, period, periodValue, measureName, compareName, strata, methodName = "ratio", limitModel, limitIterate, limitImpute) {
  if(!is.character(strata)) {
    strata <- NULL;
  }

  idList <- list(id, periodValue);

  names(idList) <- c("id", period);

  measureList <- list(measureName, periodValue);
  names(measureList) <- c(NA, period);

  measureListCompare <- list(compareName, periodValue);
  names(measureListCompare) <- c(NA, period);

  periodList <- list(period, periodValue);
  names(periodList) <- c(NA, period);

  KostraApply(data = data[ data[, period] %in%  periodValue, ,drop=FALSE]  , by = period, Fun = ImputeRegressionWide,idName = idList, strataName = strata, xName = measureListCompare,
              yName = measureList, method = methodName, limitModel = limitModel, limitIterate = limitIterate,
              limitImpute = limitImpute, returnSameType = TRUE)
}


#' SimpleImputeHistoryWide
#'
#' @encoding UTF8
#
#' @param data Input data set of class data.frame
#' @param idName Name of id-variable
#' @param periodName Name of the period identification variable.
#' @param measureName Name of the variable to estimate.
#' @param strataName Name of starta-variable. Single strata when NULL (default)
#' @param weightMethod The weight method for error calculations coded as a string: "ordinary" (default) or "ratio".
#' @param reverse When TRUE most resent is last instead of first (see xName and yName). Default is FALSE.
#'
#' @export
#'
# Methodlibrary invocation:
#
# SimpleImputeHistoryWide(data,
#                         idName,
#                         periodName,
#                         measureName,
#                         strataName,
#                         get0("weightMethod", ifnotfound = "ordinary"),
#                         get0("reverse", ifnotfound = "FALSE"));
SimpleImputeHistoryWide <- function(data, idName, periodName, measureName, strataName = NULL, weightMethod, reverse) {
  periods <-  sort(unique(data[, periodName]), decreasing = TRUE);
  lastyear <- list();
  oldyears <- list();
  lastyear[[periodName]] <- max(periods);
  oldyears[[periodName]] <- t(periods[periods!=lastyear]);
  idName = c(idName,lastyear);
  if(!is.null(strataName)) {
    strataName  <-  c(strataName,lastyear);
  }
  xName <- c(measureName,oldyears);
  yName <- c(measureName,lastyear);

  reverseBool <- FALSE;
  if(reverse == "TRUE") {
    reverseBool <- TRUE;
  }

  ImputeHistoryWide(data, idName, strataName, xName, yName, weightMethod, reverseBool);
}


#' SimpleDiff2NumVar
#'
#' @encoding UTF8
#'
#' @param data Input data set of class data.frame.
#' @param idName Name of id-variable
#' @param measureName Name of the variable to edit
#' @param measureNameCompareAlt Name of the alternative variable to edit against
#' @param periodName Name of the period identification variable.
#' @param period value of period to edit.
#' @param periodCompare value of period to edit against.
#' @param strataName Name of starta-variable. Single strata when NULL (default)
#' @param antall Parameter specifying how many units with the biggest difference to be listed. Default 5.
#' @param grense Parameter specifying a threshold for the units to be listed. This parameter overrules antall. Optional.
#' @param kommentarName Name of a variable giving information about the editing. Optional.
#'
#' @export
#'
# Methodlibrary invocation:
#
# SimpleDiff2NumVar(data,
#                   idName,
#                   measureName,
#                   measureNameCompareAlt,
#                   periodName,
#                   period,
#                   periodCompare,
#                   strataName,
#                   antall,
#                   get0("grense", ifnotfound = NULL),
#                   get0("kommentarName", ifnotfound = NULL));
SimpleDiff2NumVar <- function(data, idName, measureName, measureNameCompareAlt = NULL, periodName, period, periodCompare, strataName = NULL, antall, grense = NULL, kommentarName = NULL) {
  idList <- list(idName, period);
  names(idList) <- c("id", periodName);

  measureList <- list(measureName, period);
  names(measureList) <- c("id", periodName);

  measureListCompare <- list(measureName, periodCompare);
  names(measureListCompare) <- c("id", periodName);

  measureListCompareAlt <- NULL;
  if(!is.null(measureNameCompareAlt)) {
    measureListCompareAlt <- list(measureNameCompareAlt, periodCompare);
    names(measureListCompareAlt) <- c("id", periodName);
  }

  kommentarList <- NULL;
  if(!is.null(kommentarName)) {
    kommentarList <- list(kommentarName, periodCompare);
    names(kommentarList) <- c("id", periodName);
  }

  strataList <- list(strataName, period);
  names(strataList) <- c("id", periodName);

  Diff2NumVar(data, idList, measureList, measureListCompare, strataList, antall, grense, measureListCompareAlt, kommentarList);
}


#' SimpleRank2NumVar
#'
#' @encoding UTF8
#'
#' @param data Input data set of class data.frame.
#' @param idName Name of the identification variable.
#' @param measureName Name of the variable to edit.
#' @param periodName Name of the period identification variable.
#' @param period value of period to edit.
#' @param xVar Name of the x variable to be compared.
#' @param yVar Name of the y variable to be compared.
#' @param strataVar Name of stratification variable. Optional. If strataVar is given, the calculation and listing is
#'        performed within each stratum.
#' @param antall Parameter specifying how many of the biggest units to be listed. Default 5.
#' @param grense Parameter specifying a threshold for the units to be listed. This parameter overrules antall. Optional.
#' @param identiske When TRUE, only units with value on both x and y are used in the calculations. Default FALSE.
#'
#' @export
#'
# Methodlibrary invocation:
#
# SimpleRank2NumVar(data,
#                   idName,
#                   measureName,
#                   periodName,
#                   period,
#                   periodCompare,
#                   strataName,
#                   get0("antall", ifnotfound = 5),
#                   get0("grense", ifnotfound = 1000),
#                   get0("identiske", ifnotfound = "FALSE"));
SimpleRank2NumVar <- function(data, idName, measureName, periodName, period, periodCompare, strataName, antall, grense, identiske) {
  print(grense)
  idList <- list(idName, period);
  names(idList) <- c("id", periodName);

  measureList <- list(measureName, period);
  names(measureList) <- c("id", periodName);

  measureListCompare <- list(measureName, periodCompare);
  names(measureListCompare) <- c("id", periodName);

  strataList <- list(strataName, period);
  names(strataList) <- c("id", periodName);

  identiskeBool <- FALSE;
  if(identiske == "TRUE") {
    identiskeBool <- TRUE;
  }

  Rank2NumVar(data, idList, measureList, measureListCompare, strataList, antall, grense, identiskeBool);
}
