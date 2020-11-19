#' MultiRowHierarchyComputations
#'
#' Showing computations behind the scenes in HierarchyCompute
#'
#' @param a a
#' @param drop0 see code
#' @param ind see code
#' @param doStack Stacked output when TRUE
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
#' # Create rowSelect
#' rowSelect = data.frame(geo = "Europe", age = c("Y15-64", "Y15-29"), year="2015")
#'
#' # Create input
#' a <- HierarchyCompute(x, list(age = ageHier, geo = geoHier, year = "rowFactor"), "ths_per", rowSelect = rowSelect, output = "matrixComponents")
#'
#' MultiRowHierarchyComputations(a)
#'
MultiRowHierarchyComputations <- function(a, drop0 = TRUE, valueName = "values", indName = "ind", ind = NULL) {
  if (NCOL(a$valueMatrix) != 1)
    stop("Only single col in valueMatrix allowed (no colFactor).")

  dataDummyHierarchy <- rbind(0,t(as.matrix(a$dataDummyHierarchy)))

  valueMatrix <-
  rbind(t(as.matrix(a$dataDummyHierarchy %*% a$valueMatrix)),
        matrix(a$valueMatrix,nrow=NROW(a$valueMatrix),ncol=NROW(a$dataDummyHierarchy)))

  rownames(valueMatrix ) = NULL

  if(is.null(ind))
    ind = MatrixPaste(a$toCrossCode,sep=".")

  #nonZero <- rowSums(abs(valueMatrix)) > 0 & abs(dataDummyHierarchy) > 0


  fromCrossCode = a$fromCrossCode[c(1,seq_len(NROW(a$fromCrossCode))), ]
  rownames(fromCrossCode) = NULL


  z <- cbind(fromCrossCode, sign = as.vector(dataDummyHierarchy),
             valueName7yw4h7k3 = as.vector(valueMatrix),
             indName7yw4h7k3   = rep(ind,each=NROW(valueMatrix)))

  toCode = rep(FALSE, NROW(valueMatrix))
  toCode[1] = TRUE
  toCode = rep(toCode, NCOL(valueMatrix))

  z[toCode, names(fromCrossCode)] = a$toCrossCode[ , names(fromCrossCode)]

  names(z)[names(z)=="valueName7yw4h7k3"] = valueName
  names(z)[names(z)=="indName7yw4h7k3"] = indName

  if(drop0)
    z = z[!(!toCode & z$sign==0) , ]

  z
}



if(FALSE){
  library(Kostra)
  a=KostraData("kostraRegnskapDataPen")
  arter = "AGD65"    #rep(c("AGD9","AGID1","AGI14","AGD32","AGD65", "800", "AG20"),2)
  kontoklasser= "D"    #c("D","D","D","D","D","D", "I","I","I","D","I","D","I","D")
  perioder =  "2015" #c(rep("2015",7),rep("2016",7) )
  regnskapsomfang = "A"   #c("A","A","A","B","A","B","B" ,"B","A","A","B","A","A","A")
  funksjoner= "FG2"   #c(rep("FG2",7),"120","800","FG4","FG2","FG2","800","FG2")
  regioner =  "0301"  #unique(a$data$region)
  zBI <-
    KostraRegnskapBeregningInput(a$data, a$funksjonshierarki, a$artshierarki, a$data_saer, a$artshierarki_nettinger,a$artshierarki_nettinger_kasse,
                                 formler = a$formler,
                                 perioder =  perioder,
                                 arter =  arter,
                                 funksjoner= funksjoner,
                                 kontoklasser= kontoklasser,
                                 regioner = regioner,
                                 regnskapsomfang = regnskapsomfang)
  zBH <-
    KostraRegnskapBeregningHierarki(a$data, a$funksjonshierarki, a$artshierarki, a$data_saer, a$artshierarki_nettinger,a$artshierarki_nettinger_kasse,
                                 formler = a$formler,
                                 perioder =  perioder,
                                 arter =  arter,
                                 funksjoner= funksjoner,
                                 kontoklasser= kontoklasser,
                                 regioner = regioner,
                                 regnskapsomfang = regnskapsomfang)

  zBHI <-
    KostraRegnskap(a$data, a$funksjonshierarki, a$artshierarki, a$data_saer, a$artshierarki_nettinger,a$artshierarki_nettinger_kasse,
                                    formler = a$formler,
                                    perioder =  perioder,
                                    arter =  arter,
                                    funksjoner= funksjoner,
                                    kontoklasser= kontoklasser,
                                     regioner = regioner,
                                    regnskapsomfang = regnskapsomfang,
                   output = "beregningHierarkiInput")


  beregnetHierarki = zBHI[[1]]

  krMC = zBHI[[2]]


  RowMC = function(a, row){
    a$toCrossCode = a$toCrossCode[row,  ,drop=FALSE]
    a$dataDummyHierarchy = a$dataDummyHierarchy[row,  ,drop=FALSE]
    a
  }
  LagBeregningInputRow = function(a1, a2, a3, a4, periode, row){
    LagBeregningInput(a1= RowMC(a1,row), a2= RowMC(a2,row), a3= RowMC(a3,row), a4= RowMC(a4,row), periode=periode)
  }

  LagBeregningInputRowkrMC = function(krMC, periode, row){
    LagBeregningInputRow(krMC$a1, krMC$a2, krMC$a3, krMC$a4, periode, row)
  }

  krBI =  LagBeregningInputRowkrMC(krMC, periode, 1)



  factorType = factor(beregnetHierarki_type)
  integerType <- as.integer(factorType)
  fromMatrix <- matrix("", dim(rgBH$groups)[1], max(integerType))
  colnames(fromMatrix) <- levels(factorType)
  fromMatrix[cbind(rgBH$idx, integerType)] <- beregnetHierarki$from

  return(LagBeregningInput(a1=a1,a2=a2,a3=a3,a4=a4, periode=periode))




  beregnetHierarki_type = CharacterReCode(beregnetHierarki$type_from, c("Input_Kasse", "Input_Saer"), c("Kasse", "Saer"))


  beregnetHierarki =
  SelectAndRename(beregnetHierarki,
                  oldNames= c("periode", "region", "kontoklasse_from", "funksjon_from", "art_from", "from", "sign", "type_from", "belop"),
                  newNames= c("periode", "region", "kontoklasse",      "funksjon",      "art",      "from", "sign", "type",    "belop"))


  rgBH =RowGroups(beregnetHierarki[,c("kontoklasse", "funksjon", "art")], returnGroups = TRUE, returnGroupsId = FALSE)

  krBH = KostraRegnskap(a$data, a$funksjonshierarki, a$artshierarki, a$data_saer,
                      a$artshierarki_nettinger, a$artshierarki_nettinger_kasse,
                      kombinasjoner = rgBH$groups,
                      regioner = unique(beregnetHierarki$region))
                      #output = outp,
                      #printData = FALSE, autoNetting = FALSE, lag0300 = FALSE, fixRegionkode = FALSE, fixArtkode = FALSE, fixFunksjonkode = FALSE)

}
















