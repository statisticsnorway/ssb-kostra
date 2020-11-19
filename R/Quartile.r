TestkjoringQuartile <- FALSE
if (TestkjoringQuartile) {
  setwd("S:/Organisasjon/A800/S820/Oppdrag og anbud/KOSTRA Modernisering")
  library(data.table)
  source("Data/KostraData.R")
  minedata <- KostraData("testdata")
  minedata$strata <- c(rep(1, 61), rep(2, 91), rep(3, 98), rep(4, 81), rep(5, 85))
  barnehage <- KostraData("barnehageData")
  fysio_edit_lang <- KostraData("fysioEditLang")
} # END if (TestkjoringQuartile)

#' Detection of outliers using quartiles and by comparing with other data in same or previous period.
#'
#' @encoding UTF8
#'
#' @param data Input of Quartile is a data set of class data.frame.
#' @param id Name of the identification variable.
#' @param x1 Name of x variable in period t.
#' @param y1 Name of y variable in period t.
#' @param x2 Name of x variable in period t-1. Optional
#' @param y2 Name of y variable in period t-1. Optional
#' @param strataName Name of the stratification variable. Optional
#' @param pKL Parameter for lower limit.
#' @param pKU Parameter for upper limit.
#'
#' @return Output of Quartile is a data set of class data.frame. Only units with both x1 and y1 not missing
#' and greater than zero are included. The variables are:
#'    \item{id}{The input identification variable}
#'    \item{x1}{The input x1 variable}
#'    \item{y1}{The input y1 variable}
#'    \item{x2}{The input x2 variable}
#'    \item{y2}{The input y2 variable}
#'    \item{ratio}{The ratio between x1 and y1}
#'    \item{ratio2}{The ratio between x2 and y2}
#'    \item{ratioAll}{The ratio between the sum of x1 and the sum of y1 aggregated over the whole data set}
#'    \item{ratioAll2}{The ratio between the sum of x2 and the sum of y2 aggregated over the whole data set}
#'    \item{ratioStr}{The ratio between the sum of x1 and the sum of y1 aggregated over the stratum}
#'    \item{ratioStr2}{The ratio between the sum of x2 and the sum of y2 aggregated over the stratum}
#'    \item{lowerLimit}{The lower limit of the ratio}
#'    \item{upperLimit}{The upper limit of the ratio}
#'    \item{outlier}{A binary variable indicating whether the observation is outside the limits [q1 - pKL*(M - q1), q3 + pKU*(q3 - M)]},
#'    where M is the median and q1 and q3 the 1st an 3rd quartile respectively.
#'    \item{strata}{Strata name or number}
#'    \item{ranking}{The rank of ratio. For plotting purposes}
#'
#' @export
#'
#' @author Magnar Lillegård
#'
#'
#' @references
#'    Datarevisjon: Kontroll, granskning og retting av data. Anbefalt praksis,
#'    Statistisk sentralbyrås håndbøker, 2005/84.
#'
##' @examples
##'  minedata <- KostraData("testdata")
##'  minedata$strata <- as.character(c(rep(1, 61), rep(2, 91), rep(3, 98), rep(4, 81), rep(5, 85)))
##'  Quartile(data = minedata, id = "Region", x1 = "areal_130_eier_2015", y1 = "areal_130_leier_2015", pKL = 2, pKU = 2)
##'  Quartile(data = minedata, id = "Region", x1 = "areal_130_eier_2015", y1 = "areal_130_leier_2015", strataName = "strata")
##'  Quartile(data = minedata, id = "Region", x1 = "areal_130_eier_2015", y1 = "areal_130_leier_2015",
##'           x2 = "areal_130_eier_2014", y2 = "areal_130_leier_2014", strataName = "strata")
Quartile <- function (data, id, x1, y1, x2 = NULL, y2 = NULL, strataName = NULL, pKL = 1.5, pKU = 1.5) {
  CheckInput(id, type = "varName", data = data)
  CheckInput(x1, type = "varName", data = data)
  CheckInput(y1, type = "varName", data = data)
  CheckInput(x2, type = "varName", data = data, okNULL = TRUE)
  CheckInput(y2, type = "varName", data = data, okNULL = TRUE)
  CheckInput(strataName, type = "varName", data = data, okNULL = TRUE)
  CheckInput(pKL, min = 0, max = Inf, type = "numeric")
  CheckInput(pKU, min = 0, max = Inf, type = "numeric")

  dat <- GetData(data, id = id, x1 = x1, y1 = y1, x2 = x2, y2 = y2, strataName = strataName)
  
  if (!is.null(dat$strataName)) {
    s <- as.character(sort(unique(dat$strataName)))
    h <- length(s)
  }
  
  keep <- apply(cbind(dat$x1, dat$y1), 1, function (x) {sum(!is.na(x)) == 2 & sum(x > 0) == 2})
  if (is.null(dat$x2) | is.null(dat$y2)) {
    d <- dat[keep, ]
    ratAll <- sum(d$x1) / sum(d$y1)
    rat <- d$x1 / d$y1
    ran <- rank(rat)
    qr <- quantile(rat)
    ll <- qr[2] - pKL * (qr[3] - qr[2])
    ul <- qr[4] + pKU * (qr[4] - qr[3])
    outlier <- ifelse(rat < ll | rat > ul, 1, 0)
    if (is.null(dat$strataName)) {
      return(data.frame(id = d$id, x1 = d$x1, y1 = d$y1, x2 = NA, y2 = NA, ratio = rat, ratio2 = NA,
                        ratioAll = ratAll, ratioAll2 = NA, ratioStr = NA, ratioStr2 = NA, 
                        lowerLimit = as.numeric(ll), upperLimit = as.numeric(ul),
                        outlier = outlier, strata = "1", ranking = ran, stringsAsFactors = FALSE, row.names = NULL))
    } else {
      qres <- NULL
      for (i in 1:h) {
        d <- dat[keep & dat$strataName == s[i], ]
        rat <- d$x1 / d$y1
        outlier <- ifelse(rat < ll | rat > ul, 1, 0)
        ratStr <- sum(d$x1) / sum(d$y1)
        outStr <- data.frame(id = d$id, x1 = d$x1, y1 = d$y1, x2 = NA, y2 = NA, ratio = rat, ratio2 = NA,
                             ratioAll = ratAll, ratioAll2 = NA, ratioStr = ratStr, ratioStr2 = NA,
                             lowerLimit = as.numeric(ll), upperLimit = as.numeric(ul),
                             outlier = outlier, strata = s[i], stringsAsFactors = FALSE, row.names = NULL)
        qres <- rbind(qres, outStr)
      }
      qres$ranking <- rank(qres$ratio)
      row.names(qres) <- NULL
      return(qres)
    }
  } else if (!is.null(dat$x2) & !is.null(dat$y2)) {
    d <- dat[keep, ]
    ratAll  <- sum(d$x1) / sum(d$y1)
    ratAll2 <- sum(d$x2, na.rm = TRUE) / sum(d$y2, na.rm = TRUE)
    rat <- d$x1 / d$y1
    ran <- rank(rat)
    rat2 <- d$x2 / d$y2
    rat2[d$y2 == 0] <- NA
    qr <- quantile(rat)
    ll <- qr[2] - pKL * (qr[3] - qr[2])
    ul <- qr[4] + pKU * (qr[4] - qr[3])
    outlier <- ifelse(rat < ll | rat > ul, 1, 0)
    if (is.null(dat$strataName)) {
      return(data.frame(id = d$id, x1 = d$x1, y1 = d$y1, x2 = d$x2, y2 = d$y2, ratio = rat, ratio2 = rat2,
                        ratioAll = ratAll, ratioAll2 = ratAll2, ratioStr = NA, ratioStr2 = NA, 
                        lowerLimit = as.numeric(ll), upperLimit = as.numeric(ul),
                        outlier = outlier, strata = "1", ranking = ran, stringsAsFactors = FALSE, row.names = NULL))
    } else {
      qres <- NULL
      for (i in 1:h) {
        d <- dat[keep & dat$strataName == s[i], ]
        rat <- d$x1 / d$y1
        outlier <- ifelse(rat < ll | rat > ul, 1, 0)
        rat2 <- d$x2 / d$y2
        rat2[d$y2 == 0] <- NA
        ratStr <- sum(d$x1) / sum(d$y1)
        ratStr2 <- sum(d$x2, na.rm = TRUE) / sum(d$y2, na.rm = TRUE)
        outStr <- data.frame(id = d$id, x1 = d$x1, y1 = d$y1, x2 = d$x2, y2 = d$y2, ratio = rat, ratio2 = rat2,
                             ratioAll = ratAll, ratioAll2 = ratAll2, ratioStr = ratStr, ratioStr2 = ratStr2,
                             lowerLimit = as.numeric(ll), upperLimit = as.numeric(ul),
                             outlier = outlier, strata = s[i], stringsAsFactors = FALSE, row.names = NULL)
        qres <- rbind(qres, outStr)
      }
      qres$ranking <- rank(qres$ratio)
      row.names(qres) <- NULL
      return (qres)
    }
  }
}

if (TestkjoringQuartile) {
  ## Testkjøringer
  ## t = 2015, t-1 = 2014, x = eier, y = leier
  qres1 <- Quartile(data = minedata, id = "Region", 
                    x1 = "areal_130_eier_2015", 
                    y1 = "areal_130_leier_2015")
  qres2 <- Quartile(data = minedata, id = "Region", 
                    x1 = "areal_130_eier_2015", 
                    y1 = "areal_130_leier_2015", 
                    strataName = "strata")
  qres3 <- Quartile(data = minedata, id = "Region", 
                    x1 = "areal_130_eier_2015", 
                    y1 = "areal_130_leier_2015",
                    x2 = "areal_130_eier_2014", 
                    y2 = "areal_130_leier_2014")
  qres4 <- Quartile(data = minedata, id = "Region", 
                    x1 = "areal_130_eier_2015", 
                    y1 = "areal_130_leier_2015",
                    x2 = "areal_130_eier_2014", 
                    y2 = "areal_130_leier_2014", 
                    strataName = "strata")
  qres5 <- Quartile(data = fysio_edit_lang, id = list("Region", Periode = 2015), 
                    x1 = list("Bruttodriftsutgifter", Periode = 2015),
                    y1 = list("Bruttoinvesteringsutgifter", Periode = 2015))
  qres6 <- Quartile(data = fysio_edit_lang, id = list("Region", Periode = 2015),
                    x1 = list("Bruttodriftsutgifter", Periode = 2015),
                    y1 = list("Bruttoinvesteringsutgifter", Periode = 2015),
                    x2 = list("Bruttodriftsutgifter", Periode = 2014), 
                    y2 = list("Bruttoinvesteringsutgifter", Periode = 2014))
  qres7 <- Quartile(data = fysio_edit_lang, id = list("Region", Periode = 2015),
                    x1 = list("Bruttodriftsutgifter", Periode = 2015),
                    y1 = list("Bruttoinvesteringsutgifter", Periode = 2015),
                    x2 = list("Bruttodriftsutgifter", Periode = 2014), 
                    y2 = list("Bruttoinvesteringsutgifter", Periode = 2014),
                    strataName = list("Kostragr", Periode = 2015))
} # END if(TestkjoringQuartile)
