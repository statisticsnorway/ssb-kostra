#' Small functions that return hard coded values
#'
#' @encoding UTF8
#'
#' @return A vector of values
#'
#' @details The functions are written using a parameter with a default value. This is done to make the return value visible in the documentation.
#'
#' @export
#'
Hardcode <- function(returnValue = "See documentation"){returnValue}


#' @rdname Hardcode
#' @export
TalliD <- function(returnValue =c("id","strata","Total")){returnValue} # Endring av "id" og "strata" er mer enn kosmetisk


#' @rdname Hardcode
#' @export
WideAddName <- function(returnValue = c("","Strata","Total")){returnValue}


#' @rdname Hardcode
#' @export
WideSep <- function(returnValue = "_"){returnValue}

#' @rdname Hardcode
#' @export
WideSep2 <- function(returnValue = "_"){returnValue} # Brukes i ImputeRegressionMulti

#' @rdname Hardcode
#' @export
ProtectKostraTotal <- function(returnValue = "Total"){returnValue} # Brukes i ProtectKostra

#' @rdname Hardcode
#' @export
ProtectKostraSdcStatus <- function(returnValue = "sdcStatus"){returnValue} # Brukes i ProtectKostra

#' @rdname Hardcode
#' @export
ProtectKostraSep <- function(returnValue = "_"){returnValue} # Brukes i ProtectKostra



