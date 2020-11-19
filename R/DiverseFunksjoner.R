#' Wrapper when name and value of parameter is the same
#'
#' @encoding UTF8
#'
#' @param fun Function to be called
#' @param dots vector of parameternames where value is name
#' @param ... other parameters
#'
#' @export
#'
#' @examples
#'
#' Funksjon1 <- function(a = 3, b = "Hei", ...) {
#'   cat("a = ", a, "\n")
#'   cat("b = ", b, "\n")
#'   cat("dots = \n")
#'   print(list(...))
#' }
#'
#' Funksjon2 <- function(aa = 33, bb = "Heia", ...) {
#'   sys.call()
#' }
#'
#' DotWrap("Funksjon1", c("p1", "p2"), a = 15, tull = "tullball")
#' DotWrap("Funksjon2", c("p1", "p2"), a = 15, tull = "tullball")
#'
#' DotWrap("Funksjon2", NULL, a = 9)  # Use NULL when dots is not in use
#'
#' DotWrap("Funksjon2", strsplit("par1 par2", split = " ")[[1]], ho = "hoho")
#'
#' DotWrap("Funksjon2", strsplit("parA;parB;parC", split = ";")[[1]], ho = "hoho", a = 88)
#' DotWrap("Funksjon1", strsplit("parA;parB;parC", split = ";")[[1]], ho = "hoho", a = 88)
#'
DotWrap <- function(fun, dots, ...) {  # default parameters cannot be included
  sysCall <- sys.call()
  sysCall[[1]] <- as.name(fun)
  dotsList <- as.list(dots)
  names(dotsList) <- dots
  sysCall[[2]] <- NULL
  sysCall[[2]] <- NULL  # Yes! Same line twice
  sysCall <- as.call(c(as.list(sysCall), dotsList))
  parentFrame <- parent.frame()
  return(eval(sysCall, envir = parentFrame))
}



#' Assign named elements of a list or data.frame to caller's environment
#'
#' @param x A list or data.frame
#'
#' @return The function returns only NULL, but breaks the rules for ordinary functions.
#'         Objects are written to caller's environment.
#' @export
#'
#' @keywords internal
#'
#' @examples
#' Separate(data.frame(aAa=1:3, bBb=100))
#' aAa*bBb
Separate <- function(x){
  list2env(x,envir=parent.frame())
  NULL
}


#' Extend an input parameter to GetData
#'
#' @param x Original input parameter
#' @param ... New elements to be added
#'
#' @return Extended version of x
#' @export
#'
#' @examples
#' GD("ola",aar=2014)
#' GD(list("kari",expression(aar>2014)),function(x) x^2)
GD = function(x,...){
  c(AsList(x),list(...))
}


colSumsSameType = function(x,...){
  if(!is.integer(x[[1]])) return(colSums(x,...))
  zSum <- colSums(x,...)
  z   <- is.finite(zSum)  # Names will be included
  z[] <- as.integer(zSum)
  if(!is.integer(z[1])) stop("Something went wrong when making integer")
  z
}


AsList = function(x){
  if(!is.list(x)) return(list(x))
  x
}


MinPos = function(x){
  z=min(c(x[x>0],Inf))
  if(!is.finite(z)) z=NA
  z
}

WhereFirst =  function(x){
  x = as.matrix(x)
  apply(col(x)* (is.finite(x)),1,MinPos)
}

WhereFirst2 =  function(x){
  x = as.matrix(x)
  x[,1] = NA
  apply(col(x)* (is.finite(x)),1,MinPos)
}

WhereLast =  function(x){
  x = as.matrix(x)
  a = 1+NCOL(x)
  a-apply( (a-col(x)) * (is.finite(x)),1,MinPos)
}

WhereLast2 =  function(x){
  x = as.matrix(x)
  x[,NCOL(x)] = NA
  a = 1+NCOL(x)
  a-apply( (a-col(x)) * (is.finite(x)),1,MinPos)
}

FirstFinite = function(x){
  unAsIs(x[cbind(1:dim(x)[1],WhereFirst(x))])
}

FirstFinite2 = function(x){
  unAsIs(x[cbind(1:dim(x)[1],WhereFirst2(x))])
}

LastFinite = function(x){
  unAsIs(x[cbind(1:dim(x)[1],WhereLast(x))])
}

LastFinite2 = function(x){
  unAsIs(x[cbind(1:dim(x)[1],WhereLast2(x))])
}


# Varianter som har med navn som forteller hvilken variabel som ble brukt
FirstFiniteNames <- function(x, WhereFun=WhereFirst){
  x = as.matrix(x)
  whereFunX <- WhereFun(x)
  colnamesX <- colnames(x)
  if(is.null(colnamesX)) colnamesX <- rep("MiSsInGnAme",NCOL(x))
  xNames <- colnamesX[whereFunX]
  z <- unAsIs(x[cbind(1:dim(x)[1],whereFunX)])
  names(z) <- xNames
  z
}

FirstFinite2Names <- function(x){
  FirstFiniteNames(x, WhereFirst2)
}

LastFiniteNames <- function(x){
  FirstFiniteNames(x, WhereLast)
}

LastFinite2Names <- function(x){
  FirstFiniteNames(x, WhereLast2)
}

FirstColNames = function(x){
  FirstFiniteNames(x, WhereFirstCol)
}

LastColNames = function(x){
  FirstFiniteNames(x, WhereLastCol)
}


WhereFirstCol =  function(x){
  rep(1,NROW(x))
}


WhereLastCol =  function(x){
  rep(NCOL(x),NROW(x))
}




FirstCol = function(x){
 x = as.matrix(x)
 unAsIs(x[,1])
}

LastCol = function(x){
  x = as.matrix(x)
  unAsIs(x[,NCOL(x)])
}



OneFromEqual = function(x){
  if(NCOL(x)<=1) return(x)
  v <- apply(x,1,unique)
  if (is.list(v)) stop("All not equal")
  if (NCOL(v) != 1) stop("All not equal")
  v
}


#' Getting rid of 'AsIs' class attribute
#'
#' @param X
#'
#' @return X without 'AsIs'
#' @export
#' @details Notice that I take care to preserve any other class attributes that the vector might have.
#'
#' @author Verena Haunschmid
#'
#' @note The function is taken from
#'       \url{http://stackoverflow.com/questions/12865218/getting-rid-of-asis-class-attribute}
#'
#' @keywords internal
#'
#' @examples
#' class(I(TRUE))
#' class(unAsIs(I(TRUE)))
#' class(data.frame(x=I(matrix(1:12,3,4)))$x)
#' class(unAsIs(data.frame(x=I(matrix(1:12,3,4)))$x))
unAsIs <- function(X) {
  if("AsIs" %in% class(X)) {
    class(X) <- class(X)[-match("AsIs", class(X))]
  }
  X
}


FixEmpty = function(x,imputeEmpty=0){
  if(is.null(x)) return(imputeEmpty)
  if(NCOL(x)==0) return(rep(imputeEmpty,NROW(x)))
  x
}


#' Change numeric variables in data frame to integer
#'
#' Only done when the integer values are exact the same as the original values
#'
#' @param x data frame
#' @param makeWarning When TRUE warning made when not all numeric variables could be changed
#' @param allNumeric When TRUE no change is made unless all could be changed
#'
#' @return data frame
#' @export
#'
IntegerDataFrame = function(x,makeWarning = FALSE, allNumeric =TRUE){
  #cat("\n [ IntegerDataFrame ...")
  #flush.console()

   toInteger = rep(FALSE,NCOL(x))
  notInteger = rep(FALSE,NCOL(x))

  for (i in seq_len(NCOL(x))){
   if(is.numeric(x[[i]]))
     if(!is.integer(x[[i]])){
       if(identical( as.numeric(as.vector(as.integer(x[[i]]))),as.numeric(as.vector(x[[i]]))))
         toInteger[i] = TRUE
       else
         notInteger[i] = TRUE
     }
  }

  #cat(paste(names(x)[toInteger],collapse="+"))
  #flush.console()


  if(allNumeric){
    if(any(notInteger)){
      if(makeWarning){
            warning(paste("Integer not forced for any variable since identical result not obtained for these variables:",
                          paste(names(x)[notInteger],collapse=", ")))
      }
    } else {
      if(any(toInteger))
        for(i in which(toInteger))
          x[[i]] = as.integer(x[[i]])
    }

  } else{
    if(any(notInteger)){
      if(makeWarning){
        warning(paste("Integer not forced for some variables since identical result not obtained:",
                      paste(names(x)[notInteger],collapse=", ")))
      }
    }
    if(any(toInteger))
      for(i in which(toInteger))
        x[[i]] = as.integer(x[[i]])
  }
  #cat("]\n")
  #flush.console()
  x
}



SelectAndRename = function(data, oldNames, newNames=NULL){
  z = data[, oldNames,drop=FALSE]
  names(z) = newNames
  z
}

#' CharacterReCode
#'
#' @param x  (character) vector
#' @param oldLevels oldLevels
#' @param newLevels newLevels
#'
#' @return character vector
#' @export
#'
#' @examples
#' CharacterReCode(rep(1:4,2),c("2","3"),c("B","C"))
#'
CharacterReCode = function(x,oldLevels, newLevels){
  x = as.factor(x)
  le = levels(x)
  ma = match(le,oldLevels)
  isMatch = !is.na(ma)
  le[isMatch] = newLevels[ma[isMatch]]
  levels(x) = le
  as.character(x)
}














