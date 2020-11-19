#' Comparing the biggest units with respect to two numerical variables
#'
#' Calculating rank and share for two numerical variables, and the ratio between the variables
#' Listing big units, either the k biggest units or units with value greater than a threshold
#'
#' @encoding UTF8
#'
#' @param data Input data set of class data.frame.
#' @param idVar Name of an identification variable.
#' @param xVar Name of the x variable to be compared.
#' @param yVar Name of the y variable to be compared.
#' @param strataVar Name of stratification variable. Optional. If strataVar is given, the calculation and listing is
#'        performed within each stratum.
#' @param antall Parameter specifying how many of the biggest units to be listed. Default 5.
#' @param grense Parameter specifying a threshold for the units to be listed. This parameter overrules antall. Optional.
#' @param identiske When TRUE, only units with value on both x and y are used in the calculations. Default FALSE.
#'
#' @return Output of Rank2NumVar is a data set of class data.frame. The variables in the data frame are:
#'    \item{id}{The input identification variable}
#'    \item{x}{The input x variable}
#'    \item{y}{The input y variable}
#'    \item{strata}{The input strata variable if strataVar is given, "1" otherwise}
#'    \item{forh}{The ratio between x and y: y/x}
#'    \item{xRank}{The rank of x}
#'    \item{yRank}{The rank of y}
#'    \item{xProsAvSumx}{x in percent of the total/stratum total for x}
#'    \item{yProsAvSumy}{y in percent of the total/stratum total for y}
#' @export
#'
#'
#' @author Anna Mevik
#'
#' @examples
#' testdata <- KostraData("testdata")
#'
#' # lager en grupperingsvariabel
#' testdata$gr <- as.character(c(rep(3, 30), rep(5, 40), rep(1, 61), rep(2, 91), rep(3, 68),
#'                               rep(4, 61), rep(5, 45),  rep(4, 20)))
#'
#' # uten strata
#' Rank2NumVar(data = testdata, idVar = "Region", xVar = "areal_130_eier_2014", yVar = "areal_130_eier_2015",
#'             strataVar = NULL, antall = 10, grense = NULL, identiske = FALSE)
#'
#' # med strata
#' Rank2NumVar(data = testdata, idVar = "Region", xVar = "areal_130_eier_2014", yVar = "areal_130_eier_2015",
#'             strataVar = "gr", antall = 10, grense = NULL, identiske = FALSE)
#'
#' # med identiske = TRUE
#' Rank2NumVar(data = testdata, idVar = "Region", xVar = "areal_130_eier_2014", yVar = "areal_130_eier_2015",
#'             strataVar = "gr", antall = 10, grense = NULL, identiske = TRUE)
#'
#' # med grense (overstyrer antall)
#' Rank2NumVar(data = testdata, idVar = "Region", xVar = "areal_130_eier_2014", yVar = "areal_130_eier_2015",
#'             strataVar = "gr", antall = 10, grense = 10000, identiske = FALSE)
#'
Rank2NumVar <- function (data, idVar, xVar, yVar, strataVar = NULL, antall = 5, grense = NULL, identiske = FALSE) {

  CheckInput(idVar, type = "varName", data = data)
  CheckInput(xVar, type = "varName", data = data)
  CheckInput(yVar, type = "varName", data = data)
  CheckInput(strataVar, type = "varName", data = data, okNULL = TRUE)
  CheckInput(antall, type = "integer", min = 1)
  CheckInput(grense, type = "numeric", okNULL = TRUE)
  CheckInput(identiske, type = "logical")

  d <- GetData(data = data, id = idVar, x = xVar, y = yVar, strata = strataVar)

  if (is.null(strataVar)) {
    d <- data.frame(d, strata = NA)
    d$strata <- as.character(d$strata)
  }

  # teller opp ant. strata
  h <- unique(d$strata)
  h <- sort(h, na.last = TRUE)
  H <- length(h)

  # hvis identiske = TRUE settes x og y til NA når minst en av dem er NA
  if (identiske) {
    drop <- is.na(d$x) | is.na(d$y)
    d$x[drop] <- NA
    d$y[drop] <- NA
  }

  res <- NULL

  for (i in 1:H) {
    if (i == H & sum(is.na(h)) == 1)
      di <- d[is.na(d$strata), ]
    else di <- d[!is.na(d$strata) & d$strata == h[i], ]

    di$forh <- di$y / di$x

    di <- di[order(-di$y), ]
    di$xRank <- rank(-di$x, ties.method = "first")
    di$xRank[is.na(di$x)] <- NA                      # fjerner rangeringen for x = NA
    di <- di[order(-di$x), ]
    di$yRank <- rank(-di$y, ties.method = "first")
    di$yRank[is.na(di$y)] <- NA                      # fjerner rangeringen for y = NA

    di$xProsAvSumx <- (di$x / Sum(di$x)) * 100
    di$yProsAvSumy <- (di$y / Sum(di$y)) * 100

    di <- di[order(di$yRank, di$xRank), ]

    if (is.null(grense))
      keep <- di$xRank <= antall | di$yRank <= antall
    else
      keep <- di$x > grense | di$y > grense
    keep[is.na(keep)] <- FALSE
    di <- di[keep, ]

    res <- rbind(res, di)
  }
  
  if (is.null(strataVar))
    res$strata <- "1"

  rownames(res) <- NULL
  res

}



TestkjoringRank2NumVar <- FALSE
if (TestkjoringRank2NumVar) {
  library(Kostra)
  Sum <- Kostra:::Sum

  testdata <- KostraData("testdata")

  # lager en grupperingsvariabel
  testdata$gr <- as.character(c(rep(3, 30), rep(5, 40), rep(1, 61), rep(2, 91), rep(3, 68), rep(4, 61), rep(5, 45),  rep(4, 20)))

  Rank2NumVar(data = testdata, idVar = "Region", xVar = "areal_130_eier_2014", yVar = "areal_130_eier_2015", strataVar = "gr", antall = 10, grense = NULL, identiske = FALSE)

} # END if(TestkjoringRank2NumVar)


