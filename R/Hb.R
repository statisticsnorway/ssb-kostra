TestkjoringHb <- FALSE
if (TestkjoringHb) {
  setwd("S:/Organisasjon/A800/S820/Oppdrag og anbud/KOSTRA Modernisering")
  source("Data/KostraData.R")
  minedata <- KostraData("testdata")
  minedata$strata <- c(rep(1, 61), rep(2, 91), rep(3, 98), rep(4, 81), rep(5, 85))
  barnehage <- KostraData("barnehageData")
  fysio_edit_lang <- KostraData("fysioEditLang")
} # END if(TestkjoringHb)

#' Detection of outliers using the Hidiroglou-Berthelot (HB) method
#'
#' Detects possible outliers of a variable in period t
#' by comparing it with revised values from period t-1
#'
#' @encoding UTF8
#'
#' @param data Input of Hb is a data set of class data.frame.
#' @param id Name of an identification variable.
#' @param x1 Name of variable in period t.
#' @param x2 Name of variable in period t-1.
#' @param pU Parameter that adjusts for different level of the variables.Default value 0.5.
#' @param pA Parameter that adjusts for small differences between the median and the 1st or 3rd quartile. Default value 0.05.
#' @param pC Parameter that controls the length of the confidence interval.Default value 20.
#' @param strataName Name of stratification variable. Optional. If strataName is given, the HB method is performed within each stratum.
#' @param allowDiffOnly TRUE or FALSE. Optional. TRUE set as default. Can be set as FALSE to prevent the method from only considering different observations of x1 and x2
#'
#' @return Output of Hb is a data set of class data.frame. All units are returned, but the HB method is only performed on the data set
#' where units with both x1 and x2 not missing and greater than zero are included. In this data set, units with x1 = x2 are included in
#' the HB method only if they cover less than 50 per cent of the number of units in the stratum.
#' The output variables are:
#'    \item{id}{The input identification variable}
#'    \item{x1}{The input x1 variable}
#'    \item{x2}{The input x2 variable}
#'    \item{maxX}{The maximum of x1 and x2}
#'    \item{ratio}{The ratio between x1 and x2}
#'    \item{lowerLimit}{The lower limit of the ratio}
#'    \item{upperLimit}{The upper limit of the ratio}
#'    \item{medRatio}{The median ratio}
#'    \item{outlier}{A binary variable indicating whether the observation is an outlier (1) or not (0)}
#'    \item{strata}{Strata name or number}
#'    \item{allowDiffOnly}{TRUE/FALSE}
#'
#' @export
#' @importFrom stats median quantile
#' @importFrom graphics plot lines
#'
#' @author Magnar Lillegård
#'
#' @references
#'    Datarevisjon: Kontroll, granskning og retting av data. Anbefalt praksis,
#'    Statistisk sentralbyrås håndbøker, 2005/84.
#'
##' @examples
##'  minedata <- KostraData("testdata")
##'  minedata$strata <- as.character(c(rep(1, 61), rep(2, 91), rep(3, 98), rep(4, 81), rep(5, 85)))
##'  Hb(data = minedata, id = "Region", x1 = "areal_381_eier_2015", x2 = "areal_381_eier_2014")
##'  Hb(data = minedata, id = "Region", x1 = "areal_381_eier_2015", x2 = "areal_381_eier_2014",
##'     strataName = "strata")
##'  Hb(data = minedata, id = "Region", x1 = "areal_381_eier_2015", x2 = "areal_381_eier_2014",
##'     pA = 0.1, strataName = "strata")
##'
Hb <- function (data, id, x1, x2,  pU = 0.5, pA = 0.05, pC = 20, strataName = NULL) {
  CheckInput(id, type = "varName", data = data)
  CheckInput(x1, type = "varName", data = data)
  CheckInput(x2, type = "varName", data = data)
  CheckInput(strataName, type = "varName", data = data, okNULL = TRUE)
  CheckInput(pA, min = 0, max = 1, type = "numeric")
  CheckInput(pU, min = 0, max = 1, type = "numeric")
  CheckInput(pC, min = 0, max = Inf, type = "numeric")

  dat <- GetData(data, id = id, x1 = x1, x2 = x2, strataName = strataName)

  if (is.null(dat$strataName))
    h <- 1
  else {
    s <- as.character(sort(unique(dat$strataName)))
    h <- length(s)
  }

  if(length(pU) == 1)
    pU <- rep(pU, h)
  if(length(pA) == 1)
    pA <- rep(pA, h)
  if(length(pC) == 1)
    pC <- rep(pC, h)

  hbres <- NULL
  for (i in 1:h) {
    if (is.null(dat$strataName))
      d <- dat
    else d <- dat[dat$strataName == s[i], ]
    keep <- apply(cbind(d$x1, d$x2), 1, function (x) {sum(!is.na(x)) == 2 & sum(x > 0) == 2})
    dK <- d[keep, ]
    if (nrow(dK) == 0 && !is.null(dat$strataName)) {
        warning(paste0("Ingen enheter observert for begge perioder i stratum ", s[i], ". Hopper over."))
        next
    }    
    sumObs <- nrow(dK)
    sumEqObs <- sum(dK$x1 == dK$x2)
    if (sumEqObs == sumObs && !is.null(dat$strataName)) {
        warning(paste0("Alle enheter har lik verdi i de to periodene for stratum ", s[i], ". Hopper over."))
        next
    }
    if (sumEqObs / sumObs >= .5) {
      warning(paste0(round(sumEqObs*100 / sumObs)," % av enhetene har lik verdi i de to periodene. Dette er >=50 % og metoden kjører derfor kun på de
              enhetene som har to ulike verdier på x1 og x2."))
      dK <- dK[dK$x1 != dK$x2, ]
    }
    dN <- d[!is.element(d$id, dK$id), ]

    rat <- dK$x1 / dK$x2
    medRat <- median(rat)
    sRat <- ifelse(rat >= medRat, rat / medRat - 1, 1 - medRat / rat)
    maxX <- apply(cbind(dK$x1, dK$x2), 1, max)
    eRat <- sRat * maxX^pU[i]
    qe <- quantile(eRat)
    if (qe[3] != 0) {
      eLL <- qe[3] - pC[i] * max(qe[3] - qe[2], abs(qe[3] * pA[i]))
      eUL <- qe[3] + pC[i] * max(qe[4] - qe[3], abs(qe[3] * pA[i]))
    } else {
      eLL <- qe[3] - pC[i] * max(qe[3] - qe[2], pA[i])
      eUL <- qe[3] + pC[i] * max(qe[4] - qe[3], pA[i])
    }
    rLL <- medRat * maxX^pU[i] / (maxX^pU[i] - eLL)
    rUL <- medRat * (maxX^pU[i] + eUL) / maxX^pU[i]
    outlier <- ifelse(rat < rLL | rat > rUL, 1, 0)
    if (is.null(dat$strataName)) {
      outK <- data.frame(id = dK$id, x1 = dK$x1, x2 = dK$x2, maxX = maxX, ratio = rat, lowerLimit = rLL, upperLimit = rUL,
                         medRatio = medRat, outlier = outlier, strata = "1", stringsAsFactors = FALSE, row.names = NULL)
      if (nrow(dN) > 0) {
        outN <- data.frame(id = dN$id, x1 = dN$x1, x2 = dN$x2, maxX = NA, ratio = NA, lowerLimit = NA, upperLimit = NA,
                           medRatio = NA, outlier = NA, strata = "1", stringsAsFactors = FALSE, row.names = NULL)
        return(rbind(outK, outN))
      } else return(outK)
    } else {
      outStrK <- data.frame(id = dK$id, x1 = dK$x1, x2 = dK$x2, maxX = maxX, ratio = rat, lowerLimit = rLL, upperLimit = rUL,
                            medRatio = medRat, outlier = outlier, strata = s[i], stringsAsFactors = FALSE, row.names = NULL)
      if (nrow(dN) > 0) {
        outStrN <- data.frame(id = dN$id, x1 = dN$x1, x2 = dN$x2, maxX = NA, ratio = NA, lowerLimit = NA, upperLimit = NA,
                              medRatio = NA, outlier = NA, strata = s[i], stringsAsFactors = FALSE, row.names = NULL)
        hbres <- rbind(hbres, outStrK, outStrN)
      } else hbres <- rbind(hbres, outStrK)
    }
  }
  row.names(hbres) <- NULL
  return(hbres)
}

if (TestkjoringHb) {
  ## Testkjøringer
  hbres1 <- Hb(data = minedata, id = "Region",
               x1 = "areal_130_eier_2015",
               x2 = "areal_130_eier_2014")
  hbres2 <- Hb(data = minedata, id = "Region",
               x1 = "areal_130_eier_2015",
               x2 = "areal_130_eier_2014",
               strataName = "strata")
  hbres3 <- Hb(data = fysio_edit_lang, id = list("Region", Periode = 2015),
               x1 = list("Bruttodriftsutgifter", Periode = 2015),
               x2 = list("Bruttodriftsutgifter", Periode = 2014))
  hbres4 <- Hb(data = fysio_edit_lang, id = list("Region", Periode = 2015),
               x1 = list("Bruttodriftsutgifter", Periode = 2015),
               x2 = list("Bruttodriftsutgifter", Periode = 2014),
               strataName = list("Kostragr", Periode = 2015))
} # END if(TestkjoringHb)

#################################################################################
# Funksjon som plotter resultatene fra HB-metoden.
# plot.type = 1: rate x1/x2 mot max(x1, x2) med tilhørende nedre og øvre grense for outliere.
# plot.type = 2: rate x1/x2 mot x2.
# For begge plottene er outliere markert med røddt.
#################################################################################
hbPlot <- function (hbRes, plot.type = 1) {
  colour <- ifelse(hbRes$outlier == 1, "red", "black")
  if (plot.type == 1) {
    d <- data.frame(x = hbRes$maxX, y1 = hbRes$ratio, y2 = hbRes$rLL, y3 = hbRes$rUL, y4 = hbRes$medrat, c = colour)
    d <- d[order(d$x), ]
    plot(d$x, d$y1, ylim = c(min(d$y1, d$y2), max(d$y1, d$y3)), xlab = "Maximum value", ylab = "Ratio", col = d$c, pch = 19)
    lines(d$x, d$y2, col = "blue", lty = 2)
    lines(d$x, d$y3, col = "blue", lty = 2)
    lines(d$x, d$y4, col = "blue")
  } else if (plot.type == 2) {
    d <- data.frame(x = hbRes$x2, y = hbRes$ratio, c = colour)
    d <- d[order(d$x), ]
    plot(d$x, d$y, xlab = "Previous value", ylab = "Ratio", col = d$c, pch = 19)
  } else stop("The plot type has to be 1 or 2.")
}

