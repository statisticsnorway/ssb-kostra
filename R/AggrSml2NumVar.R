#' Aggregated comparison of two numerical variables
#'
#' Calculating aggregated values for two numerical variables, useful for comparison of the variables
#'
#' @encoding UTF8
#'
#' @param data Input data set of class data.frame.
#' @param xVar Name of the x variable to be compared.
#' @param yVar Name of the y variable to be compared.
#' @param strataVar Name of stratification variable. Optional.
#' @param identiske When TRUE, only units with value on both x and y are used in the calculations. Default FALSE.
#'
#' @return Output of AggrSml2NumVar is a data set of class data.frame. The variables in the data frame are:
#'  \item{strata}{The stratum (if strataVar is given, "1" otherwise)}
#'  \item{Antx}{The number of units with x not missing used in the aggregation}
#'  \item{Anty}{The number of units with y not missing used in the aggregation}
#'  \item{Sumx}{The total of x in the stratum}
#'  \item{Sumy}{The total of y in the stratum}
#'  \item{SumxProsAvTotx}{The stratum total for x in percent of the population total for x: (Sumx / Totx) * 100}
#'  \item{SumyProsAvToty}{The stratum total for y in percent of the population total for y: (Sumy / Toty) * 100}
#'  \item{Diff}{The difference between the stratum totales of x and y: Sumy - Sumx}
#'  \item{AbsDiff}{The absolute difference: |Diff|}
#'  \item{DiffProsAvSumx}{The difference in percent of the stratum total for x: (Diff / Sumx) * 100}
#'  \item{AbsDiffProsAvSumx}{The absolute value of DiffProsAvSumx: |DiffProsAvSumx|}
#'  \item{DiffProsAvTotx}{The difference in percent of the population total for x: (Diff / Totx) * 100}
#'  \item{AbsDiffProsAvTotx}{The absolute value of DiffProsAvTotx: |DiffProsAvTotx|}
#' @export
#'
#' @author Anna Mevik
#'
#' @examples
#' testdata <- KostraData("testdata")
#'
#' # lager en grupperingsvariabel
#' testdata$gr <- c(rep(3, 30), rep(5, 40), rep(1, 61), rep(2, 91), rep(3, 68), rep(4, 61),
#'                  rep(5, 45), rep(4, 20))
#'
#' AggrSml2NumVar(data = testdata, xVar = "areal_130_eier_2014", yVar = "areal_130_eier_2015",
#'                strataVar = "gr", identiske = FALSE)
#'


AggrSml2NumVar <- function (data, xVar, yVar, strataVar = NULL, identiske = FALSE) {

  CheckInput(xVar, type = "varName", data = data)
  CheckInput(yVar, type = "varName", data = data)
  CheckInput(strataVar, type = "varName", data = data, okNULL = TRUE)
  CheckInput(identiske, type = "logical")

  d <- GetData(data = data, x = xVar, y = yVar, strata = strataVar)

  if (is.null(strataVar)) {
    d <- data.frame(d, strata = NA)
    d$strata <- as.character(d$strata)
  }

  # hvis identiske = TRUE settes x og y til NA når minst en av dem er NA
  if (identiske) {
    drop <- is.na(d$x) | is.na(d$y)
    d$x[drop] <- NA
    d$y[drop] <- NA
  }

  # lager totalsummer
  Totx <- Sum(d$x)
  Toty <- Sum(d$y)

  # teller opp ant. strata
  h <- unique(d$strata)
  h <- sort(h, na.last = TRUE)
  H <- length(h)

  res <- NULL

  for (i in 1:H) {
    if (i == H & sum(is.na(h)) == 1)
      di <- d[is.na(d$strata), ]
    else di <- d[!is.na(d$strata) & d$strata == h[i], ]

    Antx <- sum(!is.na(di$x))
    Anty <- sum(!is.na(di$y))
    Sumx <- Sum(di$x)
    Sumy <- Sum(di$y)
    SumxProsAvTotx <- (Sumx / Totx) * 100
    SumyProsAvToty <- (Sumy / Toty) * 100
    Diff <- Sumy - Sumx
    AbsDiff <- abs(Diff)
    DiffProsAvSumx <- (Diff / Sumx) * 100
    AbsDiffProsAvSumx <- abs(DiffProsAvSumx)
    DiffProsAvTotx <- (Diff / Totx) * 100
    AbsDiffProsAvTotx <- abs(DiffProsAvTotx)

    resi <- data.frame(strata = h[i], Antx, Anty, Sumx, Sumy, SumxProsAvTotx, SumyProsAvToty, Diff, AbsDiff, DiffProsAvSumx, AbsDiffProsAvSumx, DiffProsAvTotx, AbsDiffProsAvTotx, stringsAsFactors = FALSE)

    res <- rbind(res, resi)
  }

  if (is.null(strataVar))
    res$strata <- "1"

  res
}



# funskjon som brukes i bl.a. AggrSml2NumVar
Sum <- function(x){
   if(!sum(!is.na(x))) return(NA)
   sum(x, na.rm = TRUE)
}



TestkjoringAggrSml2NumVar <- FALSE
if (TestkjoringAggrSml2NumVar) {
  library(Kostra)
  #  Sum <- Kostra:::Sum    # Definerer igjen slipper check warning (:::)
  Sum <- function(x){
    if(!sum(!is.na(x))) return(NA)
    sum(x, na.rm = TRUE)
  }

  testdata <- KostraData("testdata")

  # lager en grupperingsvariabel
  testdata$gr <- c(rep(3, 30), rep(5, 40), rep(1, 61), rep(2, 91), rep(3, 68), rep(4, 61), rep(5, 45),  rep(4, 20))

  AggrSml2NumVar(data = testdata, xVar = "areal_130_eier_2014", yVar = "areal_130_eier_2015", strataVar = "gr", identiske = FALSE)

} # END if(TestkjoringAggrSml2NumVar)

