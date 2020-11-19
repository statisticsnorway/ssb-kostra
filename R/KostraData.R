

# stackoverflow questions 30357330
pkgEnvKostraData <- new.env(parent=emptyenv())


#' Return a data set
#'
#' @encoding UTF8
#'
#' @param dataset Name of data set within the Kostra package  (rateData, testdata)
#' @param path When non-NULL the data set is read from "path/dataset.RData"
#'
#' @return The data set
#' @export
#' @importFrom utils data
#'
#' @examples
#'  z  <- KostraData("balanseRegnskapDataPen")
#'  z  <- KostraData("balanseRegnskapData")
#'  z  <- KostraData("kostraRegnskapDataPen")
#'  z  <- KostraData("kostraRegnskapData")
#'  z  <- KostraData("rateData")
#'  b1 <- KostraData("testdata")
#'  tull <- KostraData("tulledata")
#'
KostraData <- function(dataset, path = NULL) {
  if (!is.null(path)) {
    filename <- paste(path, dataset, ".RData", sep = "")
    return(get(load(filename, envir = environment())))
  }
  if (!exists(dataset, pkgEnvKostraData))
    data(list = dataset, package = "Kostra", envir = pkgEnvKostraData)
  return(pkgEnvKostraData[[dataset]])
  return(NULL)
}





