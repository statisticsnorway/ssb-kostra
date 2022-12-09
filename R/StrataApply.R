#' INTERNAL FUNCTION: Apply a function with list output over data subsets and structure output into various matrices.
#'
#' @param data Data frame
#' @param by Separate vector or Variable in data (name or number) defining the subsets (strata)
#' @param Fun Function to be applied on data subsets
#' @param copyVar Variables from input data to be directly copied to output
#' @param byName Name to be used in output of variable defining subsets
#' @param byNameDefault Name to be used when byName=NULL and when name cannot be taken from input variable
#' @param multiUnlist When TRUE output elements "multi*" are split as separate elements.
#' @param FunTotal A function to be applied on output element "aggregates" so that  output element "total" is produced
#' @param FunMultiTotal A function to be applied on output element "multiAggregates" so that  output element "multiTotal" is produced
#' @param ... Further arguments passed to Fun
#'
#' @return Output is a list with elements:
#'   \item{micro}{Matrix with as many rows as data - composed of vector output.}
#'   \item{aggregates}{Matrix with as many rows as the number of subsets - composed of scalar output.}
#'   \item{total}{Result of FunTotal (typically a matrix with a single row).}
#'   \item{multiMicro}{List of matrices with as many rows as data - composed of matrix output.}
#'   \item{multiAggregates}{List of matrices with as many rows as the number of subsets - composed of single row matrix output.}
#'   \item{multiTotal}{Result of FunMultiTotal (typically list of matrices with a single row).}
#'   \item{other}{List of output that did not fit into other elements.}
#'   Empty elements are removed from output. Elements may also be changed due to multiUnlist.
#' @export
#'
#' @examples
#'  FunFun = function(data,x){
#'  list(max=max(data[,x]),plus100=data[,x]+100,range=t(range(data[,x])),
#'       xsqrtx5=cbind(data[,x],sqrt(data[,x]),5),onefive=t(1:5))}
#'  z = data.frame(x=(1:10)^2,k=c(1,2,2,3,3,3,4,4,4,4),six=rep(6,10))
#'  FunFun(z,"x")
#'  StrataApply(z,"k",FunFun,copyVar="six",x="x")
#'  StrataApply(z,"k",FunFun,copyVar="six",x="x",multiUnlist=TRUE)
StrataApply <- function(data,by,Fun,copyVar=NULL,byName=NULL,byNameDefault="strata",
                        multiUnlist=FALSE, FunTotal=NULL,FunMultiTotal=NULL,
                        ...){
  stringsAsFactors <- FALSE
  default_stringsAsFactors <- getOption("stringsAsFactors", default = FALSE)  #default.stringsAsFactors()
  
  if (default_stringsAsFactors) {
    options(stringsAsFactors = stringsAsFactors)
  }
  
  noEmpty=TRUE  # Not input parameter since NULL will anyway be removed
  n = NROW(data)
  if(length(by)!=n){
    if(length(by)!=1) stop("by must be a vector of length NROW(data) or a variable name/number")
    if(is.null(byName)) byName = names(data[1,by,drop=FALSE])
    by = data[,by]
  }
  if(is.null(byName)) byName = byNameDefault

  uniqueBy = unique(by)
  nBy = length(uniqueBy)

  dupby = duplicated(by)

  #print(by)

  if(!any(dupby)){
    warning("Only unique elements in by")
    byi = uniqueBy[1]
  }
  else
    byi = (by[dupby])[1] # Ensure more that a single element

  indbyi = match(byi,uniqueBy)

  rbyi = by==byi
  a = Fun(data[rbyi, ],...)
  ###a$m = matrix(rnorm(16),4,4)   ###############################
  variables = names(a)
  nVar = length(variables)


  nCOL  <- sapply(a, NCOL)
  nROW  <- sapply(a, NROW)

  #empty  <- (nCOL*nROW)==0
  scalar <- (nCOL*nROW)==1                                    # x
  micro  <- (nCOL*nROW)==sum(rbyi) & pmin(nCOL,nROW)==1       # y

  multi      <-  nROW==1 & nCOL>1
  micro      <-  micro & !multi  # definition conflict ...
  multiMicro <-  nROW==sum(rbyi) & nCOL>1
  other  <- !(scalar|micro|multi|multiMicro)

  #return(list(empty=empty,scalar=scalar,micro=micro,other=other))

  #if(any(empty)) warning(paste("Empty output ignored:", names(a)[empty]))
  #if(any(other)) warning(paste("Other output ignored:", names(a)[empty])) # Change later

  ind = rep(1,n)
  ind[rbyi] = seq_len(sum(rbyi))

  #return(a)

  x = cbind(uniqueBy,data.frame(a[scalar])[rep(1,nBy), ,drop=FALSE])
  y = cbind(data[,copyVar],by,data.frame(a[micro])[ind, ,drop=FALSE])

  names(x)[1]=byName
  names(y)[seq_len(length(copyVar))] = names(data[1,copyVar,drop=FALSE])
  names(y)[length(copyVar) +1]=byName

  rownames(x) = NULL
  rownames(y) = NULL

  nMulti = sum(multi)
  nMultiMicro = sum(multiMicro)

  indMulti = seq_len(nVar)[multi]
  indMultiMicro = seq_len(nVar)[multiMicro]

  z =vector("list", nMulti)
  m =vector("list", nMultiMicro)

  names(z) = variables[multi]
  names(m) = variables[multiMicro]

  for(i in seq_len(nMulti)){
    z[[i]] =  a[[indMulti[i]]][rep(1,nBy), ,drop=FALSE]
    rownames(z[[i]]) = NULL
  }
  for(i in seq_len(nMultiMicro)){
    m[[i]] =  a[[indMultiMicro[i]]][ind, ,drop=FALSE]
    rownames(m[[i]]) = NULL
  }

  if(sum(other)>0){
    w =vector("list", nBy)
    w[[indbyi]] = a[other]
    names(w) = uniqueBy
  }
  else w = NULL


  for(byi in uniqueBy[-indbyi]){ #indbyi] already calculated
    rbyi = by==byi
    k = match(byi,uniqueBy)
    a = Fun(data[rbyi, ],...)
    ####a$m = matrix(rnorm(16),4,4)   ###############################
    x[k,variables[scalar]] =  a[scalar]
    y[rbyi,variables[micro]] =  a[micro]

    for(i in seq_len(nMulti)){
      z[[i]][k, ] =  a[[indMulti[i]]]
    }
    for(i in seq_len(nMultiMicro)){
      m[[i]][rbyi, ] =  a[[indMultiMicro[i]]]
    }
    if(!is.null(w)) w[[k]] = a[other]
  }

  if(!is.null(FunTotal)) total  = FunTotal(x)
  else total=NULL
  if(!is.null(FunMultiTotal)) multiTotal = FunMultiTotal(z)
  else multiTotal=NULL



  out=NULL

  if(!noEmpty | length(y)) out$micro=y
  if(!noEmpty | length(x)) out$aggregates=x
  if(!noEmpty | length(total)) out$total=total
  if(!multiUnlist & (!noEmpty | length(m))) out$multiMicro=m
  if(!multiUnlist & (!noEmpty | length(z))) out$multiAggregates=z
  if((!multiUnlist | !is.list(multiTotal))& (!noEmpty | length(multiTotal))) out$multiTotal=multiTotal
  if(!noEmpty | length(w)) out$other=w


  if(multiUnlist){
    if(!is.list(multiTotal)) multiTotal=NULL
    out=c(out,m,z,multiTotal)
  }
  
  if (default_stringsAsFactors) {
    options(stringsAsFactors = default_stringsAsFactors)
  }
  
  return(out)
  #tot=NULL
  #list(micro=y,aggregates=x,total=total,multiMicro=m,multiAggregates=z,multiTotal=multiTotal,other=w)

}


























