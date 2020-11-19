#' Difference between two numerical variables
#'
#' Calculating the difference between two numerical variables
#' Listing units with big difference, either the k units with the biggest absolute difference, or units with a absolute
#' difference greater than a threshold
#' Only units with value on both variables are used in the calculations
#'
#' @encoding UTF8
#'
#' @param data Input data set of class data.frame.
#' @param idVar Name of an identification variable.
#' @param xVar Name of the x variable to be compared.
#' @param yVar Name of the y variable to be compared.
#' @param strataVar Name of stratification variable. Optional. If strataVar is given, the calculation and listing is
#'        performed within each stratum.
#' @param antall Parameter specifying how many units with the biggest difference to be listed. Default 5.
#' @param grense Parameter specifying a threshold for the units to be listed. This parameter overrules antall. Optional.
#' @param zVar Name of the original y variable, before editing. Optional.
#' @param kommentarVar Name of a variable giving information about the editing. Optional.
#'

#' @return Output of Diff2NumVar is a data set of class data.frame. The variables in the data frame are:
#'    \item{strata}{The stratum (if strataVar is given, "1" otherwise)}
#'    \item{id}{The input identification variable}
#'    \item{x}{The input x variable}
#'    \item{y}{The input y variable}
#'    \item{Forh}{The ratio between x and y: y / x}
#'    \item{Diff}{The difference between x and y: y - x}
#'    \item{AbsDiff}{The absolute difference: |Diff|}
#'    \item{DiffProsAvx}{The difference in percent of x: (Diff / x) * 100}
#'    \item{DiffProsAvSumx}{The difference in percent of the stratum total for x: (Diff / stratum x) * 100}
#'    \item{DiffProsAvTotx}{The difference in percent of the total for x: (Diff / total x) * 100}
#'    \item{SumDiffProsAvSumx}{The stratum difference in percent of the stratum total for x: ((stratum y - stratum x) / stratum x) * 100}
#'    \item{SumDiffProsAvTotx}{The stratum difference in percent of the total for x: ((stratum y - stratum x) / total x) * 100}
#'    \item{z}{The input z variable}
#'    \item{EdEndring}{The difference between z and y: y - z}
#'    \item{Kommentar}{The input kommentar variable}
#'
#' @export
#'
#' @author Anna Mevik
#'
#' @examples
#' testdata <- KostraData("testdata")
#'
#' # lager en grupperingsvariabel
#' testdata$gr <- c(rep(3, 30), rep(5, 40), rep(1, 61), rep(2, 91), rep(3, 68), rep(4, 61),
#'                  rep(5, 45),  rep(4, 20))
#'
#' # lager en z-variabel
#' testdata$z <- testdata$areal_130_eier_2015
#' testdata$z[4*(1:104)] <- testdata$areal_130_eier_2014[2*(1:104)]
#' testdata$z[10*(1:40)] <- 1.2 * testdata$areal_130_eier_2015[10*(1:40)]
#' testdata$z[10*(1:40) - 5] <- 0.7 * testdata$areal_130_eier_2015[10*(1:40) - 5]
#'
#' # lager en kommentarvariabel
#' testdata$kommentar <- ifelse(testdata$areal_130_eier_2015 == testdata$z, "ikke kontrollert",
#'                             "godkjent")
#' testdata$kommentar[c(88)] <- "oppgavegiver kontaktet"
#'
#' # uten strata
#' Diff2NumVar(data = testdata, idVar = "Region", xVar = "areal_130_eier_2014", yVar = "areal_130_eier_2015",
#'             strataVar = NULL, antall = 5, grense = NULL, zVar = NULL, kommentarVar = NULL)
#'
#' # med strata
#' Diff2NumVar(data = testdata, idVar = "Region", xVar = "areal_130_eier_2014", yVar = "areal_130_eier_2015",
#'             strataVar = "gr", antall = 5, grense = NULL, zVar = NULL, kommentarVar = NULL)
#'
#' # med z og kommentar
#' Diff2NumVar(data = testdata, idVar = "Region", xVar = "areal_130_eier_2014", yVar = "areal_130_eier_2015",
#'             strataVar = "gr", antall = 5, grense = NULL, zVar = "z", kommentarVar = "kommentar")
#'
#' # med grense
#' Diff2NumVar(data = testdata, idVar = "Region", xVar = "areal_130_eier_2014", yVar = "areal_130_eier_2015",
#'             strataVar = "gr", antall = 5, grense = 5000, zVar = "z", kommentarVar = "kommentar")
#'
#'


Diff2NumVar <- function (data, idVar, xVar, yVar, strataVar = NULL, antall = 5, grense = NULL, zVar = NULL, kommentarVar = NULL) {

  CheckInput(idVar, type = "varName", data = data)
  CheckInput(xVar, type = "varName", data = data)
  CheckInput(yVar, type = "varName", data = data)
  CheckInput(strataVar, type = "varName", data = data, okNULL = TRUE)
  CheckInput(antall, type = "integer", min = 1)
  CheckInput(grense, type = "numeric", okNULL = TRUE)
  CheckInput(zVar, type = "varName", data = data, okNULL = TRUE)
  CheckInput(kommentarVar, type = "varName", data = data, okNULL = TRUE)

  d <- GetData(data = data, id = idVar, x = xVar, y = yVar, strata = strataVar, z = zVar, Kommentar = kommentarVar)

  if (is.null(strataVar))
    d$strata <- NA

  if (is.null(zVar))
    d$z <- NA

  if (is.null(kommentarVar))
    d$Kommentar <- NA

  # hvis en av x og y er NA, settes den andre også til NA
  drop <- is.na(d$x) | is.na(d$y)
  d$x[drop] <- NA
  d$y[drop] <- NA

  # teller opp ant. strata
  h <- unique(d$strata)
  h <- sort(h, na.last = TRUE)
  H <- length(h)

  res <- NULL

  for (i in 1:H) {
    if (i == H & sum(is.na(h)) == 1)
      di <- d[is.na(d$strata), ]
    else di <- d[!is.na(d$strata) & d$strata == h[i], ]

    di$Forh <- di$y / di$x
    di$Diff <- di$y - di$x
    di$AbsDiff <- abs(di$Diff)
    di$DiffProsAvx <- (di$Diff / di$x) * 100
    di$AbsDiffProsAvx <- abs(di$DiffProsAvx)   # brukes kun til sortering
    di$DiffProsAvSumx <- (di$Diff / Sum(di$x)) * 100
    di$DiffProsAvTotx <- (di$Diff / Sum(d$x)) * 100
    di$SumDiffProsAvSumx <- (Sum(di$Diff) / Sum(di$x)) * 100
    di$SumDiffProsAvTotx <- (Sum(di$Diff) / Sum(d$x)) * 100

    if (!is.null(zVar))
      di$EdEndring <- di$y - di$z
    else di$EdEndring <- NA

    di <- di[order(-di$AbsDiff, -di$AbsDiffProsAvx), ]
    di$rank <- rank(-di$AbsDiff, ties.method = "first")  # brukes kun får å plukke ut ønskede enheter
    di$rank[is.na(di$AbsDiff)] <- NA        # fjerner rangeringen for AbsDiff = NA

    if (is.null(grense))
      keep <- di$rank <= antall
    else
      keep <- di$AbsDiff > grense
    keep[is.na(keep)] <- FALSE
    di <- di[keep, ]

    res <- rbind(res, di)
  }

  if (is.null(strataVar))
    res$strata <- "1"

  rownames(res) <- NULL
  res <- res[, c("strata", "id", "x", "y", "Forh", "Diff", "AbsDiff", "DiffProsAvx", "DiffProsAvSumx", "DiffProsAvTotx", "SumDiffProsAvSumx", "SumDiffProsAvTotx", "z", "EdEndring", "Kommentar")]
  res

}




TestkjoringDiff2NumVar <- FALSE
if(TestkjoringDiff2NumVar){
  library(Kostra)
  #  Sum <- Kostra:::Sum    # Definerer igjen slipper check warning (:::)
  Sum <- function(x){
    if(!sum(!is.na(x))) return(NA)
    sum(x, na.rm = TRUE)
  }

  testdata <- KostraData("testdata")

  # lager en grupperingsvariabel
  testdata$gr <- c(rep(3, 30), rep(5, 40), rep(1, 61), rep(2, 91), rep(3, 68), rep(4, 61), rep(5, 45),  rep(4, 20))

  # lager en z-variabel
  testdata$z <- testdata$areal_130_eier_2015
  testdata$z[4*(1:104)] <- testdata$areal_130_eier_2014[2*(1:104)]
  testdata$z[10*(1:40)] <- 1.2 * testdata$areal_130_eier_2015[10*(1:40)]
  testdata$z[10*(1:40) - 5] <- 0.7 * testdata$areal_130_eier_2015[10*(1:40) - 5]

  # lager en kommentarvariabel
  testdata$kommentar <- ifelse(testdata$areal_130_eier_2015 == testdata$z, "ikke kontrollert", "godkjent")
  testdata$kommentar[c(88)] <- "oppgavegiver kontaktet"

  # uten strata
  Diff2NumVar(data = testdata, idVar = "Region", xVar = "areal_130_eier_2014", yVar = "areal_130_eier_2015", strataVar = NULL, antall = 5, grense = NULL, zVar = NULL, kommentarVar = NULL)

  # med strata
  Diff2NumVar(data = testdata, idVar = "Region", xVar = "areal_130_eier_2014", yVar = "areal_130_eier_2015", strataVar = "gr", antall = 5, grense = NULL, zVar = NULL, kommentarVar = NULL)

  # med z og kommentar
  Diff2NumVar(data = testdata, idVar = "Region", xVar = "areal_130_eier_2014", yVar = "areal_130_eier_2015", strataVar = "gr", antall = 5, grense = NULL, zVar = "z", kommentarVar = "kommentar")

  # med grense
  Diff2NumVar(data = testdata, idVar = "Region", xVar = "areal_130_eier_2014", yVar = "areal_130_eier_2015", strataVar = "gr", antall = 5, grense = 5000, zVar = "z", kommentarVar = "kommentar")

} # END if(TestkjoringDiff2NumVar)






