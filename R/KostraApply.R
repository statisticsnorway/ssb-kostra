
#RowGroupsOld <- function(x,returnGroups=FALSE, returnGroupsId=FALSE){
#  ix <- SortRows(x,index.return = TRUE)
#  dp <- duplicated(x)
#  cumsum(!dp[ix])
#  a <- rep(NaN,length(dp))
#  a[ix] <- cumsum(!dp[ix])
#  if(!(returnGroups|returnGroupsId)) return(a)
#
#  out <- NULL
#  out$idx <- a
#
#  idg <- ix[!dp[ix]]
#
#  if(returnGroups){
#    out$groups <- x[idg, ,drop=FALSE]
#    row.names(out$groups) = NULL
#  }
#
#  if(returnGroupsId)
#    out$idg <- idg
#
#  out
#}




# Old with bug
#RowGroups <- function(x,returnGroups=FALSE){
#  ix <- SortRows(x,index.return = TRUE)
#  dp <- duplicated(x)
#  cumsum(!dp[ix])
#  a <- rep(NaN,length(dp))
#  a[ix] <- cumsum(!dp[ix])
#  if(!returnGroups) return(a)
#  groups <- x[!dp, ,drop=FALSE]
#  row.names(groups) = NULL
#  list(idx=a, groups = groups)
#}

#' Apply a function with data frame output over data subsets
#'
#' @param data Data frame
#' @param by Vector of variable names (or variable numbers) defining the subsets or alternatively a named list or data frame (see examples).
#' @param Fun Function to be applied on data subsets
#' @param ... Arguments passed to Fun
#'
#' @return A data.frame
#' @export
#'
#' @note The output ordering of the example depends on getOption("stringsAsFactors")
#'
#' @examples
#' z1w <- KostraData("z1w")
#' zz  <- rbind(cbind(aar=2014,z1w),cbind(aar=2015,z1w))
#' zzz <- rbind(cbind(ab="b",zz),cbind(ab="a",zz))
#' zzz$annet[c(1,9,17,25)] =10*(1:4)
#'
#' # All subset in data by vector input
#' KostraApply(zzz, by = c("aar", "ab"), Fun = ProtectKostra, idVar = "region", freqVar = 4:7)
#'
#' # Selected subsets by using list input to specify each variable
#' KostraApply(zzz, by = list(aar = c(2014, 2015), ab = "a"), Fun = ProtectKostra, idVar = "region", freqVar = 4:7)
#'
#' # Or use NULL when all values in data
#' KostraApply(zzz, by = list(aar = NULL, ab = "a"), Fun = ProtectKostra, idVar = "region", freqVar = 4:7)
#'
#' # Selected subsets by using data frame input to specify variable combinations
#' KostraApply(zzz, by = data.frame(aar = c(2014, 2015), ab = c("b", "a")), Fun = ProtectKostra, idVar = "region", freqVar = 4:7)
#'
KostraApply <- function( data, by, Fun, ...){
  if(is.list(by)){
    byList <- by
    by <- names(by)
  }
  else
    byList <- NULL
  rg <- RowGroups(data[,by,drop=FALSE],TRUE)
  n  <- max(rg$idx)
  okGroup <- rep(TRUE,n)
  if(!is.null(byList)){
    if(is.data.frame(byList))
      okGroup = !is.na(Match(rg$groups,byList))
    else
      for(i in seq_len(length(by))){
        if(!is.null(byList[[i]])) okGroup[!(rg$groups[,by[i]] %in% byList[[i]])] = FALSE
      }
  }
  n  <- sum(okGroup)
  wok <- which(okGroup)
  a <- vector("list",n)
  for(i in seq_len(n))
    a[[i]] = cbind(rg$groups[wok[i],,drop=FALSE],Fun(data=data[rg$idx==wok[i], ,drop=FALSE],...),row.names = NULL)
  RbindAll(a)
}

#OldKostraApply <- function(data=z,by="aar",Fun=ProtectKostra,...){
#  databy <- data[,by]
#  uniqueby <- unique(databy)
#  a <- vector("list", length(uniqueby))
#  for(i in 1:length(uniqueby)) a[[i]] = cbind(data[databy==uniqueby[i],by,drop=FALSE][1,,drop=FALSE],Fun(data=data[databy==uniqueby[i], ,drop=FALSE],...))
#  RbindAll(a)
#}
