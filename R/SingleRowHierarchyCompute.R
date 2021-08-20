#' SingleRowHierarchyComputations
#'
#' Showing computations behind the scenes in HierarchyCompute
#'
#' @param a a
#' @param valueName Parameter to Stack
#' @param indName Parameter to Stack
#'
#' @return Data frame where the "weights in weighted sum" is named as sign. Firs row (before stacking) contains the "results".
#' @export
#'
#' @examples
#' # Data and hierarchies used in the examples
#' x <- SSBtoolsData("sprt_emp")  # Employment in sport in thousand persons from Eurostat database
#' geoHier <- SSBtoolsData("sprt_emp_geoHier")
#' ageHier <- SSBtoolsData("sprt_emp_ageHier")
#'
#' # Create rowSelect since a single row only
#' rowSelect <- data.frame(age = "Y15-64", geo = "Europe", stringsAsFactors = FALSE)
#'
#' # Create input
#' a <- HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "colFactor"), "ths_per", rowSelect = rowSelect, output = "matrixComponents")
#'
#'
#' SingleRowHierarchyComputations(a)
#'
#' SingleRowHierarchyComputations(a, TRUE)
SingleRowHierarchyComputations <- function(a, doStack = FALSE, valueName = "values", indName = "ind") {
  if (NROW(a$toCrossCode) != 1)
    stop("Only single row in toCrossCode allowed (single row in rowSelect input to HierarchyCompute).")

  dataDummyHierarchy <- t(as.matrix(a$dataDummyHierarchy))

  valueMatrix <- as.matrix(a$valueMatrix)

  nonZero <- rowSums(abs(valueMatrix)) > 0 & abs(dataDummyHierarchy) > 0

  z <- cbind(a$fromCrossCode, sign = as.vector(dataDummyHierarchy), valueMatrix)

  #out1 <- cbind(a$toCrossCode, sign = 0, as.matrix(a$dataDummyHierarchy %*% a$valueMatrix), stringsAsFactors = FALSE)
  out1 <- cbind(a$toCrossCode, sign = 0, as.matrix(Mult(a$dataDummyHierarchy, a$valueMatrix)), stringsAsFactors = FALSE)

  x <- rbind(out1, z[nonZero, , drop = FALSE])

  rownames(x) <- NULL

  if (doStack) {
    blockN <- NCOL(a$toCrossCode) + 1
    blockVar <- seq_len(blockN)
    stackVar <- matlabColon(blockN + 1, NCOL(x))
    x <- Stack(x, stackVar = stackVar, blockVar = blockVar, valueName = valueName, indName = indName)
  }
  x
}
