TestkjoringThError <- FALSE
if (TestkjoringThError) {
  setwd("S:/Organisasjon/A800/S820/Oppdrag og anbud/KOSTRA Modernisering")
  library(data.table)
  source("Data/KostraData.R")
  minedata <- KostraData("testdata")
  barnehage <- KostraData("barnehageData")
  fysio_edit_lang <- KostraData("fysioEditLang")
} # END if (TestkjoringThError)


#' Detection of 1000-error
#'
#' Detects units with possible 1000-error by comparing values in period t
#' with revised values from period t-1
#'
#' @encoding UTF8
#'
#' @param data Input data set of class data.frame.
#' @param id Name of identification variable.
#' @param x1 Name of variable in period t.
#' @param x2 Name of variable in period t-1.
#' @param ll Lower limit of log10(x1/x2) = log10(x1) - log10(x2). Default -2.5
#' @param ul Upper limit of log10(x1/x2) = log10(x1) - log10(x2). Default +2.5
#'
#' @return Output of ThError is a data set of class data.frame. Thousand Error is only checked if 
#' both x1 and x2 are not missing and not zero. The variables are:
#'    \item{id}{The input identification variable. }
#'    \item{x1}{The input x1 variable}
#'    \item{x2}{The input x2 variable}
#'    \item{outlier}{A binary (1/0) variable indicating whether the we suspect a 1000 error or not}
#'    \item{diffLog10}{The difference log10(x1) - log10(x2)}
#'    \item{lowerLimit}{The input parameter ll}
#'    \item{upperLimit}{The input parameter ul}
#'
#' @export
#'
#' @author Magnar Lillegård
#'
#' @references
#'    Datarevisjon: Kontroll, granskning og retting av data. Anbefalt praksis,
#'    Statistisk sentralbyrås håndbøker, 2005/84.
#'
##' @examples
##'  minedata <- KostraData("testdata")
##'  minedata$areal_381_eier_2015[c(1, 4, 5)] <- 1000 * minedata$areal_381_eier_2015[c(1, 4, 5)]
##'  ThError(data = minedata, id = "Region", x1 = "areal_381_eier_2015", x2 = "areal_381_eier_2014")
##'  ThError(data = minedata, id = "Region", x1 = "areal_381_eier_2015", x2 = "areal_381_eier_2014",
##'          ll = -2, ul = 2)
##'
ThError <- function (data, id, x1, x2, ll = -2.5, ul = 2.5) {
  CheckInput(id, type = "varName", data = data)
  CheckInput(x1, type = "varName", data = data)
  CheckInput(x2, type = "varName", data = data)
  CheckInput(ll, min = -3, max = 0, type = "numeric")
  CheckInput(ul, min = 0, max = 3, type = "numeric")

  d <- GetData(data, id = id, x1 = x1, x2 = x2)

  keep <- apply(cbind(d$x1, d$x2), 1, function (x) {!anyNA(x) & sum(x > 0) == 2})
  dK <- d[keep, ]
  dN <- d[!is.element(d$id, dK$id), ]
  diffs <- apply(cbind(dK$x2, dK$x1), 1, function (x) {diff(log10(x))})
  outlier <- ifelse(diffs < ll | diffs > ul, 1, 0)
  outK <- data.frame(id = dK$id, x1 = dK$x1, x2 = dK$x2, outlier = outlier, diffLog10 = diffs,
                     lowerLimit = ll, upperLimit = ul, stringsAsFactors = FALSE, row.names = NULL)
  if (nrow(dN) > 0) {
    outN <- data.frame(id = dN$id, x1 = dN$x1, x2 = dN$x2, outlier = NA, diffLog10 = NA,
                       lowerLimit = ll, upperLimit = ul, stringsAsFactors = FALSE, row.names = NULL)
    return(rbind(outK, outN))
  } else return(outK)
}

if (TestkjoringThError) {
  ## Testkjøringer
  ## legger inn noen 1000-feil i datasettene 
  minedata$areal_381_eier_2015[c(1, 4, 5)] <- 1000 * minedata$areal_381_eier_2015[c(1, 4, 5)]
  fysio_edit_lang$Bruttodriftsutgifter[c(10, 14, 20)] <- 1000 * fysio_edit_lang$Bruttodriftsutgifter[c(10, 14, 20)]
  fysio_edit_lang$Bruttodriftsutgifter[c(8, 31)] <- fysio_edit_lang$Bruttodriftsutgifter[c(8, 31)] / 1000
  ThError(data = minedata, id = "Region", x1 = "areal_381_eier_2015", x2 = "areal_381_eier_2014")
  ThError(data = minedata, id = "Region", x1 = "areal_381_eier_2015", x2 = "areal_381_eier_2014", ll = -2, ul = 2)
  ThError(data = fysio_edit_lang, id = list("Region", Periode = 2015), x1 = list("Bruttodriftsutgifter", Periode = 2015),
          x2 = list("Bruttodriftsutgifter", Periode = 2014))
  ThError(data = fysio_edit_lang, id = list("Region", Periode = 2015), x1 = list("Bruttoinvesteringsutgifter", Periode = 2015),
          x2 = list("Bruttoinvesteringsutgifter", Periode = 2014), ll = -2, ul = 1.5)
} # END if (TestkjoringThError)

