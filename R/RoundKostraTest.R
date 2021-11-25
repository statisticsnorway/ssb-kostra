

RoundKostra1 <- function(data,idVar = 1, dimVar = 2:NCOL(data), freqVar = NULL, freqName="f_Re_Q",
                           protectZeros = TRUE, maxN = 3, method = "SIMPLEHEURISTIC", output, split=NULL, total = "Total", ...){


  if(length(freqVar)>1)
    x=AutoStack(data=data,dimVarInd=c(idVar,dimVar),freqVarInd=freqVar,varNames=TRUE,freqName=freqName,split=split,border="_",rowData=NULL)
  else{
    x = data[,c(idVar,dimVar,freqVar)]
    freqName = colnames(data)[freqVar]
  }


  formulax = MakeHierFormula(x[,-dim(x)[2],drop=FALSE])



  #MakeHierFormula(z[,c(idVar,dimVar),drop=FALSE],n=level)

  #a = try(ProtectTable(data=data,dimVar=c(idVar,dimVar),freqVar=freqVar,protectZeros=protectZeros,
  #                     maxN=maxN,method=method,  singleOutput = FALSE, ...),silent = TRUE)
  #x
  formulax = update(as.formula(formulax),paste(freqName," ~ ."))
  fs = FormulaSums(formula=formulax,data=x,crossTable=TRUE,total=total,printInc = FALSE)


  if(output=="status"){
    statuso = fs$allSums==0
    fs$allSums[] = "r"
    fs$allSums[statuso] = "o"
  }
  z = cbind(as.data.frame(fs$crossTable),as.data.frame(fs$allSums))

  if((length(freqVar)>1)){


  namesx = names(x)
  namesz = names(z)

  mainVar   =  which(namesz %in% namesx[attr(x,"freqVarInd")])
  stackVar  =  which(namesz %in% namesx[attr(x,"dimVarNewInd")])
  blockVar  =  which(namesz %in% namesx[attr(x,"dimVarOrigInd")])


  sep = attr(x,"sep")
  z = Unstack(data=z,
          mainVar = mainVar,
          stackVar =  stackVar,
          blockVar = blockVar,sep=sep,returnRowData = FALSE )

  colRT = !(names(z) %in% namesz[c(blockVar)])

  colnames(z)[colRT] = RemoveTotal(colnames(z)[colRT],total=total)


  }


  idCol    = match(colnames(data)[idVar], colnames(z))[1]
  origCol  = match(colnames(data), colnames(z))
  origCol  = origCol[!is.na(origCol)]

  totalCol = match(total, colnames(z))[1]
  totalCol = totalCol[!is.na(totalCol)]

  seqNcol  = seq_len(NCOL(z))
  otherCol =   seqNcol[!(seqNcol %in% c(origCol,totalCol))]
  colIdx   = c(origCol,otherCol,totalCol)


  idRow    = match(data[,idVar], z[,idCol])
  totalRow = match(total, z[,idCol])[1]
  seqNrow = seq_len(NROW(z))
  otherRow =   seqNrow[!(seqNrow %in% c(idRow,totalRow))]

  rowIdx = c(idRow,otherRow,totalRow)


  # Kontroll som kan fjernes
  if(length(rowIdx)!=NROW(z))
    stop("A")
  if(length(unique(rowIdx))!=NROW(z))
    stop("B")
  if(length(colIdx)!=NCOL(z))
    stop("C")
  if(length(unique(colIdx))!=NCOL(z))
    stop("D")

  z = z[rowIdx,colIdx,drop=FALSE]

  rownames(z) = NULL

  z
}





