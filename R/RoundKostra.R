#' Rounding à la Heldal following the standards in the Kostra project.
#'
#'
#' @encoding UTF8
#'
#' @param data Input data set of class data.frame
#' @param idVar Id-variable (name or number)
#' @param strataVar Strata-variable(s) (name or number)
#' @param freqVar Variable(s) holding counts (name or number)
#' @param freqVarGroup  NULL (default) or integer representing groups of variables (see details)
#' @param roundBase Basis for rounding
#' @param method Algorithm for the rounding calculations. Currently "pls" or "singleRandom".
#' @param formula Model formula as a string defining cells to be published (additional to automation)
#' @param level  Interaction level or 0 (all levels) defining complexity of model component created from strata
#' @param allSmall When TRUE all small values will be rounded (when a single freqVar)
#' @param singleTotal When TRUE identical rowsums in all freqVarGroups needed.
#'          When FALSE totals for each freqVarGroup will be in output.
#' @param makeSums When TRUE totals vil be made similar to ProtectKostra (in fact this is done by calling ProtectKostra)
#' @param output One of "rounded" (default), "original", "difference" or "status".
#'               Unrounded results are returned by “original” (same as roundBase=0) and
#'               \code{"difference" = "rounded" -  “original”}.
#'               With output=”status” zero differences are set to “o” (original) and others are coded as “r” (rounded)
#' @param total String used to name totals.
#' @param split Parameter to \code{\link{AutoSplit}} - see varNames and rowData above.
#'    When NULL automatic splitting without needing a split string.
#' @param extraOutput When TRUE output is a list of several elements (makeSums ignored)
#' @param seed NULL or seed for random number generator  (\code{set.seed(seed)} will be run at the beginning of the function)
#' @param ... Variables for formula and additional variables that will be included in output (name or number).
#'
#' @details
#'
#'   \strong{A single freq variable and formula:}
#'   A formula defines all the publishable cells. Rounding is performed so that all the publishable cells are safe.
#'   When allSmall=TRUE all small cells are rounded. All possible totals are then safe, but totals not defined by
#'   the formula can be far from the original values.
#'
#'   \strong{A single freq variable and strata:}
#'   Instead of a formula it is assumed that the cells to be published are obtained by crossing all strata variables.
#'   The parameter "level" may be used.
#'
#'   \strong{Several freq variables without freqVarGroup:}
#'   The data is in a unstacked form and stack/unstack will be performed in the background similar to ProtectKostra.
#'   The original id-var will be considered as a strata-var when stacked and rounding is performed similar
#'   to "A single freq variable and strata".
#'
#'   \strong{With freqVarGroup without single-groups:}
#'   Each group can be stacked to form a separate data set, but a common data set is needed.
#'   An ad hoc data set is created to match all the single data sets and this data set will be used in the rounding process.
#'   This method will not work in complicated cases. Use with care.
#'   Try extraOutput=TRUE to see what is going on.
#'
#'   \strong{With freqVarGroup with single-groups:}
#'   The single-groups are assumed to be two-category groups (yes and no, but only yes is reported).
#'   The remaining category will be computed in order to create ad hoc data set.
#'
#' @note
#'
#' Even if freq-variables with \code{freqVarGroup<1} is not used they will be read by \code{\link{GetData}}  together with the other
#' the freq-variables variables into a matrix. Use a common numeric type for all these variables to prevent change of data type.
#'
#'
#' Parameter \code{namesAsInput} in ProtectTable() is not yet available in RoundKostra and therefore very advanced variable name
#' coding will give insufficient results. Example:
#'
#'
#' \code{RoundKostra(KostraData("z3wb") ,idVar="region",strataVar=c("fylke","kostragr"),freqVar=4:15)}
#'
#' But this will work:
#'
#' \code{RoundKostra(KostraData("z3wb") ,idVar="region",strataVar=c("fylke","kostragr"),freqVar=4:15,split=NULL)}
#'
#' See similar example in \code{\link{ProtectTable}}.
#'
#' \strong{NOTE:} Be sure to spell the input parameters correctly. Because of the "..."-input misspelled parameters
#' may give strange results instead of error.
#'
#' @seealso \code{\link{ProtectKostra}}, \code{\link{RoundViaDummy}}, \code{\link{makeroundtabs}},
#' \code{\link{Round2}}, \code{\link{FormulaSums}}, \code{\link{ModelMatrix}}
#'
#' @return A data.frame unless extraOutput = TRUE
#' @export
#' @importFrom SSBtools AutoSplit Stack Unstack MakeHierFormula FormulaSums
#' @importFrom SmallCountRounding RoundViaDummy
#' @importFrom stats aggregate
#'
#' @examples
#'
#'  # ========================================================
#'  #    Examples:  A single freq variable
#'  # =======================================================
#'
#'  z2w <- KostraData("z2w")
#'
#'  # ==== Without strataVar and  without formula ====
#'  RoundKostra(z2w ,idVar="region", freqVar="arbeid", roundBase=5)
#'  RoundKostra(z2w ,idVar="region", freqVar="arbeid", makeSums=FALSE) # Without total
#'
#'  # ==== With strataVar and  without formula ====
#'  RoundKostra(z2w ,idVar="region", freqVar="arbeid", strataVar=c("fylke","kostragr"), roundBase=5)
#'  RoundKostra(z2w ,idVar="region", freqVar="arbeid", strataVar=c("fylke","kostragr"), makeSums=FALSE)
#'  RoundKostra(z2w ,idVar="region", freqVar="arbeid", strataVar= "fylke", allSmall=FALSE) # Warning when makeSums=TRUE
#'  RoundKostra(z2w ,idVar="region", freqVar="arbeid", strataVar= "fylke", allSmall=FALSE, makeSums=FALSE)
#'  RoundKostra(z2w ,idVar="region", freqVar="arbeid", strataVar=c("fylke","kostragr"), extraOutput=TRUE)$formula
#'
#'  # ==== Without strataVar and  with formula ( makeSums ignored without warning)  ====
#'  RoundKostra(z2w ,idVar="region", freqVar="arbeid", formula="fylke", fylke="fylke")
#'  RoundKostra(z2w ,idVar="region", freqVar="arbeid", formula="fylke", fylke="fylke",allSmall = FALSE)
#'  RoundKostra(z2w ,idVar="region", freqVar="arbeid", formula="A+B", A="fylke",B="kostragr")
#'  RoundKostra(z2w ,idVar="region", freqVar="arbeid", formula="A*B+C", A="fylke",B="kostragr",C="annet")
#'
#'  # =============================================================================
#'  #   Examples:  Several freq variables without freqVarGroup (allSmall ignored)
#'  # =============================================================================
#'  RoundKostra(z2w ,idVar="region",strataVar=c("fylke","kostragr"),freqVar=4:7)
#'  RoundKostra(z2w ,idVar="region",strataVar=c("fylke","kostragr"),freqVar=4:7,makeSums=FALSE)
#'  RoundKostra(z2w ,idVar="region",strataVar=c("fylke","kostragr"),freqVar=4:7,extraOutput=TRUE)$input
#'  RoundKostra(z2w ,idVar="region",strataVar=c("fylke","kostragr"),freqVar=4:7,extraOutput=TRUE)$formula
#'
#'  # ===============================================================
#'  #   Examples: With freqVarGroup
#'  # ==========================================================================
#'
#'  # ==========  With no single-groups  ================
#'  ex1 = Kostra:::exData1()   #  hack endre seinere
#'  freqVarGroup <- c(1,1,1,1,1,1,1,1,2,2,2,2)
#'  RoundKostra(ex1, idVar="region",strataVar=c("fylke","kostragr"),freqVar=4:15,freqVarGroup=freqVarGroup)
#'  RoundKostra(ex1, idVar="region",strataVar=c("fylke","kostragr"),freqVar=4:15,freqVarGroup=freqVarGroup, makeSums=FALSE)
#'  a1 <- RoundKostra(ex1, idVar="region",strataVar=c("fylke","kostragr"),freqVar=4:15,freqVarGroup=freqVarGroup, extraOutput=TRUE)
#'  head(a1$input) # ad hoc created data
#'  a1$formula     # The formula used
#'
#'
#'  # ==========  With some single-groups  ================
#'  freqVarGroup <- c(1,1,1,1,1,1,1,1,2,2,2,2,3,4,-1,5)
#'  RoundKostra(ex1, idVar="region",strataVar=c("fylke","kostragr"),freqVar=4:19,freqVarGroup=freqVarGroup)
#'  RoundKostra(ex1, idVar="region",strataVar=c("fylke","kostragr"),freqVar=4:19,freqVarGroup=freqVarGroup, singleTotal = FALSE)
#'
#'  # ====== With incorrect totals
#'  ex1b <- ex1
#'  ex1b$s1[1]=2L
#'  ex1b$arb_A[2]=5L
#'  RoundKostra(ex1b, idVar="region",strataVar=c("fylke","kostragr"),freqVar=4:19,freqVarGroup=freqVarGroup, singleTotal = FALSE)
#'  a2 <- RoundKostra(ex1b, idVar="region",strataVar=c("fylke","kostragr"),freqVar=4:19,freqVarGroup=freqVarGroup, singleTotal = FALSE,extraOutput=TRUE)
#'  table(a2$input$s1_s2_s3_s4,  useNA ="always") # Missing values in ad hoc created data when incorrect totals
#'
#'  # ===============================================================
#'  #   Examples  With parameter output
#'  # ===============================================================
#'
#'  RoundKostra(z2w ,idVar="region", freqVar="arbeid",output="status")
#'  RoundKostra(z2w ,idVar="region", freqVar="arbeid",output="difference")
#'  RoundKostra(z2w ,idVar="region",strataVar=c("fylke","kostragr"),freqVar=4:7,output="status")
#'  RoundKostra(z2w ,idVar="region",strataVar=c("fylke","kostragr"),freqVar=4:7,output="difference")
#'
#'  # ===============================================================
#'  #   Micro data example  (":::" since functions not exported yet)
#'  # ===============================================================
#'
#'  microData <- Kostra:::microEx1()                 # A micro data set
#'  freqData  <- MakeFreq(microData,"freq") # Make cross-classified data
#'  freqData$id <- 1:NROW(freqData)                  # Add id-variable
#'
#'  #  Rounding with makeSums=FALSE
#'  freqRound <- RoundKostra(freqData, idVar="id", strataVar=c("region", "fylke", "kostragr", "hovedint"), freqVar="freq", makeSums=FALSE)
#'
#'  microRound  <- MakeMicro(freqRound,"freq")  # Create micro data set from output
#'  microRound  <- microRound[,-c(1,6)]                        # Remove some variables
#'
#'  # Alternative where only region sums and the cross-classifications fylke*hovedint and kostragr*hovedint are to be published
#'  freqRound2 <- RoundKostra(freqData, idVar="id", formula=("region +fylke*hovedint + kostragr*hovedint"), freqVar="freq",
#'                    makeSums=FALSE, allSmall=FALSE, region="region",fylke="fylke",kostragr="kostragr",hovedint="hovedint")
#'  microRound2  <- MakeMicro(freqRound2,"freq")
#'  microRound2  <- microRound2[,-c(1,2)]
#'
RoundKostra <- function(data,idVar, strataVar = NULL,
                        freqVar, freqVarGroup = NULL,
                        roundBase = 3,
                        method = "pls",
                        formula = NULL,
                        level = 0,
                        allSmall = TRUE,
                        singleTotal=TRUE,
                        makeSums = TRUE,
                        output = "rounded",
                        total = "Total",
                        split = "_",
                        extraOutput=FALSE,
                        seed = 12345,
                        ...){
  CheckInput(idVar, type = "varNrName", data = data, okSeveral = TRUE)
  CheckInput(strataVar, type = "varNrName", data = data, okNULL = TRUE, okSeveral=TRUE)
  CheckInput(freqVar, type = "varNrName", data = data, okSeveral=TRUE)
  CheckInput(freqVarGroup, type = "integer", okSeveral=TRUE, okNULL = TRUE)
  CheckInput(roundBase, type = "integer", min=0)
  CheckInput(method,type = "character", alt = c("pls","singleRandom"))
  CheckInput(formula, type = "character",okNULL = TRUE)
  CheckInput(level, type = "integer", min=0)
  CheckInput(allSmall, type = "logical")
  CheckInput(singleTotal, type = "logical")
  CheckInput(makeSums, type = "logical")
  CheckInput(output,type = "character", alt = c("rounded", "original", "difference", "status"))
  CheckInput(total, type = "character")
  CheckInput(split, type = "character",  okNULL = TRUE)
  CheckInput(extraOutput, type = "logical")

  if(!is.null(seed)){
    set.seed(seed)
  }

  default_stringsAsFactors = default.stringsAsFactors()
  options(stringsAsFactors = FALSE)

  if(output=="original")
    roundBase = 0

  checkGroupTotal = TRUE

  if(!is.null(freqVarGroup) & !is.null(formula))
    stop("Either freqVarGroup or formula need to be NULL")

  if(is.null(freqVarGroup) & length(freqVar)>1)
    freqVarGroup = rep(1,length(freqVar))


  if(length(freqVar)>1 & !is.null(formula))
    stop("formula need to be NULL when several freq variables")

  if(length(freqVar)>1 & !allSmall)
    warning("allSmall ignorded when several freq variables")

  if(!is.null(formula)){
    makeSums = FALSE
    if(!is.null(strataVar))
      stop("stratavar combined with formula is not implemented")
  }

  if(extraOutput & output=="status")
    warning("difference instead of status when extraOutput")

  #  # ==== Without strataVar and  with formula ( makeSums ignored without warning)  ====
  #  # Må gjøre endring her for å unngå "region:fylke"
  #  RoundKostra(z2w ,idVar="region", freqVar="arbeid", strataVar="kostragr", formula="fylke", fylke="fylke")
  #  RoundKostra(z2w ,idVar="region", freqVar="arbeid", strataVar="kostragr", formula="fylke", fylke="fylke",extraOutput=TRUE)$formula


  ## Littel "hack" is needed to combine possibility of using GetData()
  ## and at the same time keep original names
  ## In addition
  ##   freqVar is recreated since advanced GetData input allowed
  ##   dimVar is created from idVar and strataVar

  z <- GetData(data=data, id = GD(idVar,MatrixPaste), strata = strataVar,freq=freqVar, ...)

  # Get oriiginal names
  origCols <- attr(z,"origCols")[length(idVar):(length(idVar)+length(strataVar)+length(freqVar))]
  origCols[1] <- attr(z,"origVars")[1]


  idVar = 1
  dimVar = 1 + seq_len(Ncol(z$strata))
  # Get rid of matrix in data.frame
  z$strata=unAsIs(z$strata)
  z$freq=unAsIs(z$freq)

  standardVar = names(z) %in% c("id","strata","freq")
  extraVar = !standardVar
  if(any(extraVar)){
    extraVar[1] <- TRUE  # Include id for matching
    extraData = do.call(data.frame, z[extraVar])
    names(extraData)[1] <- origCols[1]
  } else
    extraData <- NULL
  # Finished creating extra data from ...


  z = do.call(data.frame, z[standardVar])
  freqVar= matlabColon(max(c(dimVar,1))+1,NCOL(z))
  names(z) <- origCols # Original names


  # Hack since reusing ProtectKostra
  if(!is.null(extraData)){
    z = cbind(z,extraData[,-1,drop=FALSE])
    extraNames = colnames(extraData)[-1]
  }
  else
    extraNames = NULL


  # Finished creating z from data + freqVar, idVar, strataVar

  if(!is.null(dimVar))
    if(length(dimVar)==0)
      dimVar=NULL


  idVarName=colnames(z)[idVar]

  formulaAll = NULL


  if(length(freqVar)==1){
    if(!is.null(freqVarGroup)){
      if(freqVarGroup[1]<1){
        warning("freqVarGroup ignored when a single freqVar")
      }
      freqVarGroup = NULL
    }
}



  if(anyNA(z[,freqVar])){
    z[,freqVar][is.na(z[,freqVar])] <- 0
    warning("Missing values set to 0")
  }


    if(makeSums)
      if(!allSmall){
        warning("Since allSmall=FALSE all output is not safe")
      }


    if(!is.null(dimVar)){
      if(allSmall)
        formulaAll = MakeHierFormula(z[,c(idVar,dimVar),drop=FALSE],n=level)
      else
        formulaAll = MakeHierFormula(z[,dimVar,drop=FALSE],n=level)
      if(!is.null(formula))
        formulaAll = update(as.formula(formulaAll),paste("~ . * ",formula))

    } else {
    if(!is.null(formula))
        formulaAll = as.formula(correctFormula(formula))
    if(allSmall){
      if(!is.null(formulaAll))
        formulaAll = update(as.formula(formulaAll),paste("~ . * ",idVarName))
      else
        formulaAll = as.formula(correctFormula(idVarName))
    }
    }


  if(is.null(formulaAll))
    stop("No information for the rounding method. Maybe try allSmall=TRUE.")


  if(is.null(freqVarGroup)){




    if(extraOutput){
      out = vector("list", 3)
      names(out) = c("output","input","formula")
      out$input = z
      out$formula = formulaAll
    }



    if(roundBase>1){
      methodOutput = RoundViaDummy(data=z, freqVar=freqVar, formula=formulaAll, roundBase = roundBase, singleRandom = FALSE)

      if(output=="rounded")
        z[,freqVar] =  methodOutput[[1]][,2]
      if(output=="difference" | output=="status")
        z[,freqVar] =  methodOutput[[1]][,2] - methodOutput[[1]][,1]
    } else
      methodOutput = NULL
      if(extraOutput){
        out$output = z
        options(stringsAsFactors = default_stringsAsFactors)
        return(c(out,methodOutput))
      }

    #return(z)
  }
  else{

  if(length(freqVar) != length(freqVarGroup))
    stop("freqVar and freqVarGroup must have same length")

  uniqueGroup = unique(freqVarGroup[freqVarGroup>0])
  uniqueGroup = sort(uniqueGroup)
  n = length(uniqueGroup)
  outputList = vector("list", n+1)
  freqVarNames = names(z[1,freqVar,drop=FALSE])   #  Allow both numbers and names here, but know that freqVAr is number (see above)
  # totalMatrix = NULL
  totalFrame = NULL
  groupTotal = NULL
  if(checkGroupTotal) # Check in separate loop to avoid spending time before error
    for(i in 1:n){
      gr = uniqueGroup[i]
      freqVarGroupgr = freqVarGroup==gr
      freqVari = freqVar[freqVarGroupgr]
      if(sum(freqVarGroupgr)>1 ){
        if(is.null(groupTotal))
          groupTotal         = RowSumsByApply(z[,freqVari])      # rowSums(z[,freqVari])
        else
          if(any(groupTotal != rowSums(z[,freqVari]))){
            if(singleTotal) stop("Not identical rowsums in all groups.")
            warning(paste("Not identical rowsums in all groups"))

            groupTotal = pmax(groupTotal,RowSumsByApply(z[,freqVari]))      #pmax(groupTotal,rowSums(z[,freqVari]))
          }
      }
    }

  tableFVG = table(freqVarGroup[freqVarGroup>0])
  if(any(tableFVG==1)){
    tableFVG = tableFVG[tableFVG==1]
    singleFreqVar = as.integer(names(tableFVG))

    freqVarGroup  %in% singleFreqVar

    zNot = as.vector(groupTotal) - z[,freqVar[ freqVarGroup  %in% singleFreqVar]]
    colnames(zNot) = paste("nOOt",colnames(zNot),sep="")

    freqVar = c(freqVar,dim(z)[2] + seq_len(dim(zNot)[2]))
    freqVarGroup = c(freqVarGroup,freqVarGroup[freqVarGroup  %in% singleFreqVar])

    z = cbind(z,zNot)


  }

  dimVarNames=colnames(z)[dimVar]


  z <- RoundManyTables(data=z,idVar = idVar, strataVar = dimVar,
                              freqVar = freqVar, freqVarGroup = freqVarGroup,
                              roundBase = roundBase,
                              method = method, split = split,
                  extraOutput=extraOutput, output=output)
  if(extraOutput) return(z)
  }
  if(!makeSums & output=="status"){
    statuso = z[,freqVar]==0
    z[,freqVar] = "r"
    z[,freqVar][statuso]="o"
  }

  options(stringsAsFactors = default_stringsAsFactors)
  if(!makeSums)
    return(z)
  if(is.null(freqVarGroup))
    freqVarGroup=1
  DotWrap("ProtectKostraForRound",extraNames,
            data=z,idVar = idVar, strataVar = dimVar,
                    freqVar = freqVar, freqVarGroup = freqVarGroup,
                  protectZeros = FALSE, maxN = 0,
                  method = "Simple", output = output,
          total = total,
                     split = split, singleTotal=singleTotal)
}


# This keeps integer but rowSums not
RowSumsByApply = function(x){
  apply(x,1,sum)
}


correctFormula <- function(x){
  if(class(x)=="formula") return(x)
  x <- trimws(as.character(x))
  if(substr(x,1,1)!="~")
    x = paste("~",x,sep="")
  x
}

# z3 = cbind(id=1:432, EasyData("z3"))
# mf = "region*mnd + hovedint*mnd + fylke*hovedint*mnd + kostragr*hovedint*mnd"
# RoundKostra(z3, idVar = "id", strataVar = NULL, freqVar = "ant",formula = mf)


#' RoundManyTables
#'
#' @param data data
#' @param idVar idVar
#' @param strataVar strataVar
#' @param freqVar freqVar
#' @param freqVarGroup freqVarGroup
#' @param roundBase roundBase
#' @param method method
#' @param extraOutput extraOutput
#'
#' @return Data frame som ligner på input og sammensatt med resultat fra mange avrundinger
#' @keywords internal
#' @export
#'
#' @examples
#' # Generate example data for this function
#' exData   <- KostraData("z3w")[,c(1:15,15,4:6)]
#' names(exData)[12:19]=c("s1","s2","s3","s4","A","B","C","D")
#' exData[,"s4"] <- rowSums(exData[,4:11]) - rowSums(exData[,12:14])
#' z  = exData[,1:15]
#' cz = colnames(z)
#' cz = str_replace(cz,"m10m12","B")
#' cz = str_replace(cz,"m01m05","A")
#' cz = str_replace(cz,"soshjelp","soshj")
#' cz = str_replace(cz,"arbeid","arb")
#' colnames(z) = cz
#' # Create input parameter
#' freqVarGroup <- c(1,1,1,1,1,1,1,1,2,2,2,2)
#' #aPrikk <- ProtectKostra(z ,idVar="region",strataVar=c("fylke","kostragr"),freqVar=4:15,freqVarGroup=freqVarGroup)
#' a0 <- RoundManyTables(z ,roundBase=0,idVar="region",strataVar=c("fylke","kostragr"),freqVar=4:15,freqVarGroup=freqVarGroup)
#' a3 <- RoundManyTables(z ,roundBase=3,idVar="region",strataVar=c("fylke","kostragr"),freqVar=4:15,freqVarGroup=freqVarGroup)
#' a5 <- RoundManyTables(z ,roundBase=5,idVar="region",strataVar=c("fylke","kostragr"),freqVar=4:15,freqVarGroup=freqVarGroup)
RoundManyTables <- function(data,idVar = 1, strataVar = NULL,
                        freqVar = 2, freqVarGroup = NULL,
                        roundBase = 3,
                        method = "pls",
                        split = NULL,
                        extraOutput=FALSE,
                        output = "rounded"){

  z = data

  colnames = colnames(z)
  if(!is.numeric(idVar)) idVar = match(idVar,colnames)
  if(!is.numeric(strataVar)) strataVar = match(strataVar,colnames)
  if(!is.numeric(freqVar)) freqVar = match(freqVar,colnames)

  dimVar = strataVar

  if(is.null(freqVarGroup))
    freqVarGroup = rep(1,length(freqVar))
  uniqueGroup = unique(freqVarGroup[freqVarGroup>0])
  uniqueGroup = sort(uniqueGroup)
  n = length(uniqueGroup)


  dimVarNames=colnames(z)[dimVar]

  outputList = vector("list", n+1)
  outputList[[1]] = z[,c(idVar,dimVar) ,drop=FALSE]


  allInteger=TRUE

  for(i in 1:n){
    gr = uniqueGroup[i]
    freqVarGroupgr = freqVarGroup==gr
    freqVari = freqVar[freqVarGroupgr]

    x=AutoStack(data=z,dimVarInd=c(idVar,dimVar),freqVarInd=freqVari,varNames=TRUE,freqName="f_Re_Q",split=split,border="_",rowData=NULL)

    outputList[[i+1]] = x

    formulax = MakeHierFormula(x[,-dim(x)[2],drop=FALSE])



    allInteger = allInteger & is.integer(x$f_Re_Q)


    if(i==1) {
      xMerge = x
      formulaAll = as.formula(formulax)
    }
    else {
      xMerge = FreqMerge(xMerge,x,"f_Re_Q","f_Re_Q","f_Re_Q")
      formulaAll = update(formulaAll,paste("~ . + ",formulax))
    }

  }

  if(allInteger) # Fix integer since not implemented in FreqMerge
    xMerge$f_Re_Q = as.integer(xMerge$f_Re_Q)


  singleRandom = (method=="singleRandom")


  #return(list(xMerge,formulaAll))
  if(extraOutput){
    out = vector("list", 3)
    names(out) = c("output","input","formula")
    out$input = xMerge
    out$formula = formulaAll
  }


  if(roundBase>1){
    methodOutput = RoundViaDummy(data = xMerge, freqVar = "f_Re_Q", formula = formulaAll, roundBase = roundBase, singleRandom = singleRandom)
    if(output=="rounded")
      xMerge$f_Re_Q = methodOutput[[1]][,2]
    if(output=="difference" | output=="status")
      xMerge$f_Re_Q = methodOutput[[1]][,2] - methodOutput[[1]][,1]
  } else
    methodOutput = NULL


  for(i in 1:n){
    au = AutoUnstack(RoundedValuesf_Re_Q(outputList[[i+1]] ,xMerge))$data
    coli = !(colnames(au) %in% dimVarNames)
    outputList[[i+1]] = au[,coli,drop=FALSE]
  }


  zNew = CbindIdMatch(outputList)


  mc = match(colnames(zNew),colnames(z))


  z[,mc] = zNew
  rownames(z)=NULL



  #return(list(z=z,zNew=zNew,xMerge=xMerge,formulaAll=formulaAll))
  if(extraOutput){
    out$output = z
    return(c(out,methodOutput))
  }

  z   # Input endret
}



RoundedValuesf_Re_Q = function(x,y){
  x$f_Re_Q = 0L
  xy=rbind(x,y[, match(colnames(x),colnames(y)),drop=FALSE])
  rg = RowGroups(xy[,-dim(xy)[2],drop=FALSE])
  ag = aggregate(xy$f_Re_Q,list(rg),sum)
  x$f_Re_Q = ag$x[rg[seq_len(NROW(x))]]
  x
}




AutoStack <- function(data,dimVarInd,freqVarInd,varNames=TRUE,freqName="values",split=NULL,border="_",rowData=NULL){


  autoVarNames=FALSE
  if(is.logical(varNames)){
    if(varNames){
      autoVarNames=TRUE
      varNames=paste("varAu",1:100,sep="")
    } else
      varNames=paste("var",1:100,sep="")
  }


  #paste(levels(as.factor(sort(as.character(x$var1)))),collapse="-")

  stackVar <- freqVarInd
  dataOrig <- data
  if (is.null(rowData))
    rowData <- AutoSplit(colnames(data)[freqVarInd], split = split, border = border,
                         varNames = varNames)
  else rownames(rowData) <- colnames(data)[freqVarInd]
  varNames <- colnames(rowData)
  data <- Stack(dataOrig, stackVar = freqVarInd, blockVar = dimVarInd, rowData = rowData,
                valueName = freqName, indName = NULL)
  colnamesOrig <- colnames(dataOrig)
  dimVarNamesOrig <- colnamesOrig[dimVarInd]
  dimVarNames <- c(dimVarNamesOrig, varNames)
  dimVarInd <- match(dimVarNames, colnames(data))  ######### New dimVarInd refer to stacked data
  dimVarOrigInd <- match(dimVarNamesOrig, colnames(data))
  dimVarNewInd <- match(varNames, colnames(data))

  freqVarInd <- match(freqName, colnames(data))  ######### New freqVarInd refer to stacked data

  if(autoVarNames){
    varNames = colnames(data)
    for(i in dimVarNewInd)
      varNames[i] = paste(levels(as.factor(sort(as.character(data[,i])))),collapse="_")
    colnames(data) = varNames
  }

  if(!is.null(split))
    sep=split
  else
    sep=border
  attr(data,"freqVarInd") <- freqVarInd
  attr(data,"dimVarOrigInd") <- dimVarOrigInd
  attr(data,"dimVarNewInd") <- dimVarNewInd
  attr(data,"sep") <- sep
  data
}


AutoUnstack <- function(data){
  Unstack(data,
          mainVar = attr(data,"freqVarInd"),
          stackVar =   attr(data,"dimVarNewInd"),
          blockVar = attr(data,"dimVarOrigInd"),sep= attr(data,"sep") )
}






AllCombinations <- function(x = c(3,1,2),with0=TRUE,m=matrix(0,1,0)){
  if(!length(x)) return(m)
  nm <- NROW(m)
  AllCombinations(x[-1],with0,cbind(m[rep(seq_len(nm),x[1]+with0),],sort(rep(seq_len(x[1]+with0),nm))-with0))
}


AllNCombinations <- function(x = c(3,1,2),n=0,returnSorted=TRUE,returnList=FALSE){
  m = AllCombinations(x)
  rS = rowSums(m>0)
  if(n) return(m[rS==n, ,drop=FALSE])
  if(returnList){
    a <- vector("list",max(rS))
    for(i in seq_len(max(rS))) a[[i]] = m[rS==i, ,drop=FALSE]
    return(a)
  }
  m = m[!rS==0, ,drop=FALSE]
  rS = rS[!rS==0]
  if(returnSorted) return(m[order(rS), ,drop=FALSE])
  m
}




#' Combine two frequency data frames
#'
#' The process is done by constructing fictive micro data in accordance with both input data sets.
#'
#' @param x data frame
#' @param y data frame
#' @param xFreqVar NULL or name/number of x-variable holding counts
#' @param yFreqVar NULL or name/number of y-variable holding counts
#' @param freqVar NULL or name of output-variable holding counts
#'
#' @details Using NULL as input for xFreqVar, yFreqVar, or freqVar means microdata instead of frequency data
#'
#' @return A data frame
#' @importFrom SSBtools MakeFreq MakeMicro
#' @export
#'
#' @examples
#'
#' # Create example data
#' z3 <- EasyData("z3")
#' zAmicro <- MakeMicro(z3[,c(1,4,7)],"ant")[-3]
#' zA = MakeFreq(zAmicro)
#' zBmicro = MakeMicro(z3[,c(2,4,5,7)],"ant")[,-4]
#' zB = MakeFreq(zBmicro)
#'
#' # Three way of constructing the same output
#' zAB1 = FreqMerge(zA,zB)
#' zAB2 = FreqMerge(zAmicro,zBmicro,NULL,NULL)
#' zAB3 = MakeFreq(FreqMerge(zAmicro,zBmicro,NULL,NULL,NULL))
FreqMerge <- function(x,y,xFreqVar="freq",yFreqVar="freq",freqVar="freq"){
  if(is.null(xFreqVar)){
    xFreqVar = "freq8462419"
    x <- MakeFreq(x,xFreqVar)
  }
  if(is.null(yFreqVar)){
    yFreqVar = "freq8462419"
    y <- MakeFreq(y,yFreqVar)
  }

  xM <- MakeMicro(x[order(x[,xFreqVar]),],xFreqVar)
  yM <- MakeMicro(y[order(y[,yFreqVar]),],yFreqVar)

  xNames <- colnames(x)
  xNames <- xNames[xNames != xFreqVar]
  yNames <- colnames(y)
  yNames <- yNames[yNames != yFreqVar]

  xyNames <-  xNames[xNames %in% yNames]

  #return(rbind(xM[,xyNames,drop=FALSE],yM[,xyNames,drop=FALSE]))

  rg <- RowGroups(rbind(xM[,xyNames,drop=FALSE],yM[,xyNames,drop=FALSE]))

  seqx <- seq_len(dim(xM)[1])
  seqy <- seq_len(dim(yM)[1])

  rgx <- rg[seqx]
  rgy <- rg[-seqx]
  n  <- max(rg)
  a <- vector("list",n)

  rgxs <- sort(rgx,index.return=TRUE)
  rgys <- sort(rgy,index.return=TRUE)

  nrx <-  1 + seqx - cummax(as.numeric(!duplicated(rgxs$x))*seqx)
  nry <-  1 + seqy - cummax(as.numeric(!duplicated(rgys$x))*seqy)

  nrx[rgxs$ix] <- nrx
  nry[rgys$ix] <- nry

  z <- merge(
    cbind(nrForMerge12345=nrx,xM),
    cbind(nrForMerge12345=nry,yM),
    by = c("nrForMerge12345",xyNames),
    all = TRUE)

  b <- z[,c(xyNames,xNames[!(xNames %in% yNames)],yNames[!(yNames %in% xNames)]),drop=FALSE]
  if(!is.null(freqVar)) b <- MakeFreq(b,freqName=freqVar)
  b
}




