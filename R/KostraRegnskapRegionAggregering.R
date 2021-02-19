




#'  Aggregering av regioner basert på output fra \code{\link{KostraRegnskap}}
#'
#'  Funksjonen generaliserer summering ved \code{\link{aggregate}} til
#'  å bruke hierarki istedenfor en variabel (kostragruppe) i datasettet
#'
#'  Parameteren \code{slettOutput0} har mening i tillegg til \code{slettInput0} dersom det er negative fortegn i hierarki slik at 0-er kan oppnås ved differanser.
#'
#' @param data          data frame med output fra KostraRegnskap
#' @param hierarki      aggregeringshierarki for region
#' @param slettInput0   Ved TRUE fjernes rader med belop=0 i input
#' @param slettOutput0  Ved TRUE fjernes rader med belop=0 i output
#' @param fixRegionkode Ved TRUE (default): Sørger for blanke i starten/slutten fjernes og at regionkoder (from) i hierarki får 4 eller 6 plasser og ledende nuller (gir warning ved endring av input)
#' @param region        Variabelnavn for region
#' @param belop         Variabelnavn for belop
#' @param dimvar        De andre klassifiseringsvariablene ("regnskapsomfang", "kontoklasse", "art", "funksjon")
#' @param drop          Ved TRUE begrenses output for hvert aggregat til  dimvar-kombinasjoner som har rader i inputdata som bidrar.
#'                      Ved FALSE genereres alle kombinasjoner av alle dimvar-variabler.
#'                      Ved NA vil output for alle aggregater ha alle dimvar-kombinasjoner som finnes i inputdata.
#' @param verbose       Ved TRUE printes informasjon om fremdrift
#' @param highspeed     Ved TRUE brukes en raskere algoritme (men ikke når drop=FALSE)
#'
#' @return   En data frame
#' @export
#' @importFrom SSBtools AutoHierarchies DummyHierarchy HierarchyCompute HierarchyCompute2 RowGroups
#' @importFrom utils flush.console
#'
#' @note Kjernefunksjonalitet i funksjonen kan på sikt ende opp som en ny funksjon i pakka SSBtools
#'
#' @examples
#' hierarki <- Work:::Get_litehierarki()
#' data22 <- Work:::Get_bev_basis_R_36()[1:22, ]
#' KostraRegnskapRegionAggregering(data22, hierarki)
#' KostraRegnskapRegionAggregering(data22, hierarki, drop = NA)
#' KostraRegnskapRegionAggregering(data22, hierarki, drop = FALSE)
#' KostraRegnskapRegionAggregering(data22, hierarki, slettInput0 = TRUE, drop = NA)
KostraRegnskapRegionAggregering <- function(data, hierarki, slettInput0=FALSE, slettOutput0=FALSE, fixRegionkode = TRUE,
                                           region= "region", belop = "belop",
                                           dimvar = c("regnskapsomfang", "kontoklasse", "art", "funksjon"),
                                           drop = TRUE,
                                           verbose = TRUE, highspeed = TRUE){ # True defaut nå. Må erstatte duplicated med noe raskere
  if(is.null(drop))
    drop=NA


  if(!is.na(drop) & !drop){
    highspeed <- FALSE
  }

  if(highspeed){
    cat(" [ Forberede highspeed ...")
    flush.console()
    rg <- RowGroups(data[, dimvar, drop = FALSE], returnGroupsId = TRUE)
    cat("] ")
    flush.console()

    z <- KostraRegnskapRegionAggregering(cbind(data[, c(region, belop)], diMvAr = rg$idx), hierarki=hierarki, slettInput0=slettInput0,
                                         slettOutput0=slettOutput0, fixRegionkode =fixRegionkode, region=region, belop=belop,
                                         dimvar = "diMvAr", drop=drop, verbose=verbose, highspeed = FALSE)
    z <- cbind(z[, region, drop=FALSE], data[rg$idg[as.integer(z$diMvAr)], dimvar, drop=FALSE], z[, belop, drop=FALSE])
    rownames(z) <- NULL
    return(z)
  }



  # Enkel hack som erstatter funksjonene  duplicated og unique med noe raskere
  DuplicatedHere <- DuplicatedByMatch
  UniqueHere     <- UniqueByMatch

  if(fixRegionkode)
    hierarki$from = Kostra::FixRegionkode(hierarki$from,"Regionskoder i hierarki endret")

  hi <- SSBtools::AutoHierarchies(list(hi=hierarki) ,  hierarchyVarNames = c(mapsFrom = "from", mapsTo = "to", sign = "sign"))[[1]]
  dummyHierarchy <- SSBtools::DummyHierarchy( hi$mapsFrom, hi$mapsTo, hi$sign, hi$level)


  hierarchies <- c(list(hierarki), as.list(rep("rowFactor", length(dimvar))))
  names(hierarchies) <- c(region, dimvar)

  cat(" [ Generering av input til HierarchyCompute ...")
  flush.console()


  if(slettOutput0){
    slettInput0 <- TRUE
    drop <- TRUE
  }

  if(slettInput0)
    data <- data[data[[belop]]!=0, ,drop=FALSE]


  if(!is.na(drop) & !drop){
    rowSelect <- NULL
  }


  if(is.na(drop)){
    cat(".\n")
    flush.console()
    rowSelect = UniqueHere(data[, dimvar, drop = FALSE])
    drop <- FALSE
  }

  if(!drop){
    cat("]\n")
    flush.console()

    return(SSBtools::HierarchyCompute(data=data, valueVar = belop,
                                     hierarchies=hierarchies,
                                     hierarchyVarNames = c(mapsFrom = "from", mapsTo = "to", sign = "sign"),
                                     colVar = region, rowSelect = rowSelect,  verbose =  verbose))

  }




  cat(".")
  flush.console()

  k <- UniqueRegnskapRowsAll(rownames(dummyHierarchy),  dummyHierarchy, data, dimvar = dimvar)


  select <- data[k$rows, c(dimvar, region)]
  select[[region]] <- k$to

  cat(".")
  flush.console()

  if(FALSE) # Kode som kan redusere data. Men i praksis har dette liten gevinst.
    if(!slettInput0){ # Etter select definert kan rader slettes

      # data <- data[data[[belop]]!=0, ,drop=FALSE] # Denne ikke nok pga situasjon med warning
      # ReductionCrossDataDummyHierarchies(dataDummyHierarchies[hierarchyInd],
      # Not all rowSelect possible. Row removed.
      # Mulig å forbedre HierarchyCompute?
      # Men legger inn noen ekstra rader slik at det definert av select finnes

      rows0 <- which(data[[belop]]!=0)
      rowsExtra <- c(which(!DuplicatedHere(data[, dimvar,drop=FALSE])),which(!DuplicatedHere(data[, region,drop=FALSE])))
      rowsExtra <- rowsExtra[!(rowsExtra %in% rows0)]

      cat(".")
      flush.console()

      # Data med ikke-0 samt ekstra rader som sikrer nødvendige kombinasjoner
      data <- data[c(rows0, rowsExtra), ,drop=FALSE]

      cat(".")
      flush.console()

      # Noen av ekstraradene er overflødige
      keep <- unique(c(seq_len(length(rows0)), which(!DuplicatedHere(data[, dimvar,drop=FALSE])), which(!DuplicatedHere(data[, region,drop=FALSE]))))

      # Endelige redusert data som er input til HierarchyCompute
      data <- data[ keep, ,drop=FALSE]

      cat(".")
      flush.console()
    }



  cat("]\n")
  flush.console()

  data <- SSBtools::HierarchyCompute(data=data, valueVar = belop,
                   hierarchies=hierarchies,
                   hierarchyVarNames = c(mapsFrom = "from", mapsTo = "to", sign = "sign"),
                   colVar = region, select = select,  verbose =  verbose)

  if(slettOutput0)
    data <- data[data[[belop]]!=0, ,drop=FALSE]

  data
}



# Sletter rader med 0-er i output fra KostraRegnskap
KostraRegnskapSlett0 = function(data, belop = "belop"){
  data[data[[belop]]!=0, ,drop=FALSE]
}




# Eksempeldata: Funksjonen returnerer litehierarki
Get_litehierarki <- function(){
  data.frame( to = c("EKA50", "EKA50", "EKG03", "EKG14", "EAK", "EAK", "EAKUO", "EAKUO", "EAFK", "EAFKUO", "EAFK04"),
              sign = "+",
              from = c(5001L, 5014L, 5014L, 5001L, 5001L, 5014L, 5001L, 5014L, 5000L, 5000L, 5000L))
}

# Eksempeldata: Funksjonen returnerer36 rader fra bev_basis_R
Get_bev_basis_R_36  <- function(){
data.frame(periode = "2019",
           regnskapsomfang = c("A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A",
                               "A", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B"),
           kontoklasse = c("D", "I", "D", "I",  "D", "I", "D", "I", "D", "I", "D", "I", "D", "I", "D", "I", "D", "I", "D", "I",
                           "D", "I", "D", "I", "D", "I", "D", "I", "D", "I",  "D", "I", "D", "I", "D", "I"),
           art = "AG35",
           region = c("0200", "0200", "5000", "5000", "0101", "0101", "0216", "0216", "0219", "0219", "0220", "0220", "0301",
                      "0301", "5001", "5001", "5014", "5014", "0200", "0200", "5000", "5000", "0101", "0101", "0216", "0216", "0219",
                      "0219", "0220", "0220", "0301", "0301", "5001", "5001", "5014", "5014"),
           funksjon = c("FGF1", "FGF1", "FGF1", "FGF1", "FG1", "FG1",
                        "FG1", "FG1", "FG1", "FG1", "FG1", "FG1", "FG1", "FG1", "FG1", "FG1", "FG1", "FG1", "FGF1", "FGF1", "FGF1", "FGF1", "FG1", "FG1",
                        "FG1", "FG1", "FG1", "FG1", "FG1", "FG1", "FG1", "FG1", "FG1", "FG1", "FG1", "FG1"),
           belop = c(38029L, 3697L, 37907L, -43L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 38029L, 0L,
                     37907L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L))}

# En hjelpefunksjon
UniqueRegnskapRows <- function(to, dummyHierarchy, data, region= "region" , dimvar = c("regnskapsomfang", "kontoklasse") ){

  DuplicatedHere <- DuplicatedByMatch

  if(!(to %in% rownames(dummyHierarchy))){
    warning(paste("to-code not in hierarchy:",to))
    return(integer(0))
  }

  from <- colnames(dummyHierarchy)[dummyHierarchy[to,]!=0]

  rows <- which(data[[region]] %in% from)

  rows[!DuplicatedHere(data[rows, dimvar, drop=FALSE])]

}



UniqueRegnskapRowsAll <- function(to, ...) {
  rows <- integer(0)
  toOut <- character(0)
  for (i in seq_along(to)) {
    cat("-")
    flush.console()
    rowsi <- UniqueRegnskapRows(to[i], ...)
    rows <- c(rows, rowsi)
    toOut <- c(toOut, rep(to[i], length(rowsi)))
  }
  data.frame(to=toOut, rows = rows)
}


DuplicatedByMatch <- function(x){
  a <- rep(TRUE, nrow(x))
  a[unique(SSBtools::Match(x,x))] <- FALSE
  a
}

UniqueByMatch <- function(x){
  x[unique(SSBtools::Match(x,x)), , drop=FALSE]
}



if(FALSE){ # testkode
  hierarki <- Work:::Get_litehierarki()
  data22  <- Work:::Get_bev_basis_R_36()[1:22, ]
  KostraRegnskapRegionAggregering(data22, hierarki)
  KostraRegnskapRegionAggregering(data22, hierarki, drop = NA)
  KostraRegnskapRegionAggregering(data22, hierarki, drop = FALSE)
  KostraRegnskapRegionAggregering(data22, hierarki, slettInput0=TRUE, drop =NA)

  hierarki <- Work:::Get_litehierarki()
  data22  <- Work:::Get_bev_basis_R_36()[1:22, ]
  a1 = KostraRegnskapRegionAggregering(data22, hierarki, highspeed = TRUE)
  a2 = KostraRegnskapRegionAggregering(data22, hierarki, drop = NA, highspeed = TRUE)
  a3 = KostraRegnskapRegionAggregering(data22, hierarki, drop = FALSE, highspeed = TRUE)
  a4 = KostraRegnskapRegionAggregering(data22, hierarki, slettInput0=TRUE, drop =NA, highspeed = TRUE)
  b1 = KostraRegnskapRegionAggregering(data22, hierarki, highspeed = FALSE)
  b2 = KostraRegnskapRegionAggregering(data22, hierarki, drop = NA, highspeed = FALSE)
  b3 = KostraRegnskapRegionAggregering(data22, hierarki, drop = FALSE, highspeed = FALSE)
  b4 = KostraRegnskapRegionAggregering(data22, hierarki, slettInput0=TRUE, drop =NA, highspeed = FALSE)


  hi <- AutoHierarchies(list(hi=litehierarki) ,  hierarchyVarNames = c(mapsFrom = "from", mapsTo = "to", sign = "sign"))[[1]]
  dummyHierarchy <- DummyHierarchy( hi$mapsFrom, hi$mapsTo, hi$sign, hi$level)
  bev_basis_R_36[ UniqueRegnskapRows("EKG03",  dummyHierarchy, bev_basis_R_36) , ]
  UniqueRegnskapRowsAll(rownames(dummyHierarchy),  dummyHierarchy, bev_basis_R_36)
  UniqueRegnskapRowsAll(rownames(dummyHierarchy),  dummyHierarchy, bev_basis_R_36, dimvar = c("regnskapsomfang") )



  z <- HierarchyCompute(data=bev_basis_R_36, valueVar = "belop",
                        hierarchies=list(region = litehierarki, regnskapsomfang = "rowFactor", kontoklasse = "rowFactor",  art = "rowFactor", funksjon = "rowFactor"),
                        hierarchyVarNames = c(mapsFrom = "from", mapsTo = "to", sign = "sign"),
                        colVar = "region", rowSelect = unique(bev_basis_R_36[, c("regnskapsomfang", "kontoklasse", "art", "funksjon")]),
                        inputInOutput = FALSE)
  bev_basis_R_36$en = 1

  zen <- HierarchyCompute(data=bev_basis_R_36, valueVar = "en",
                        hierarchies=list(region = litehierarki, regnskapsomfang = "rowFactor", kontoklasse = "rowFactor",  art = "rowFactor", funksjon = "rowFactor"),
                        hierarchyVarNames = c(mapsFrom = "from", mapsTo = "to", sign = "sign"),
                        colVar = "region", rowSelect = unique(bev_basis_R_36[, c("regnskapsomfang", "kontoklasse", "art", "funksjon")]),
                        inputInOutput = FALSE)

  k <- UniqueRegnskapRowsAll(rownames(dummyHierarchy),  dummyHierarchy, bev_basis_R_36, dimvar = c("regnskapsomfang", "kontoklasse", "art", "funksjon") )

  select <- bev_basis_R_36[k$rows, c("regnskapsomfang", "kontoklasse", "art", "funksjon", "region")]

  select[["region"]] <- k$to

  zselect <- HierarchyCompute(data=bev_basis_R_36, valueVar = "belop",
                          hierarchies=list(region = litehierarki, regnskapsomfang = "rowFactor", kontoklasse = "rowFactor",  art = "rowFactor", funksjon = "rowFactor"),
                          hierarchyVarNames = c(mapsFrom = "from", mapsTo = "to", sign = "sign"),
                          colVar = "region", select = select)

  zselectNy <- KostraRegnskapRegionAggregering(bev_basis_R_36, litehierarki)

}


# Kode postet på Slack
# Den kjøres ikke siden den ligger i if(FALSE)
if(FALSE){
  # Det er også problem at stjernekombinasjoner er kodet med missing (NA). Funksjonen forventer en tom streng ("").
  # Prøv denne omkodingen for å løse begge problemene.
  regnskap$kontoklasse <- CharacterReCode(NaToZero(regnskap$kontoklasse), c("0", "1"), c("I", "D"))
  saerbedrift$kontoklasse <- CharacterReCode(NaToZero(saerbedrift$kontoklasse), c("3", "4", "0"), c("I", "D", "I"))
  kombinasjoner[is.na(kombinasjoner)] <- ""


  # Reduserer hierarkiet slik at regioner som ikke er i testdata ikke er med
  litehierarki <- aggregeringshierarki[aggregeringshierarki$from %in% bev_basis_R$region, ]
  # Kjører aggregering
  z <- HierarchyCompute(data=bev_basis_R, valueVar = "belop",
                        hierarchies=list(region = litehierarki, regnskapsomfang = "rowFactor", kontoklasse = "rowFactor",  art = "rowFactor", funksjon = "rowFactor"),
                        hierarchyVarNames = c(mapsFrom = "from", mapsTo = "to", sign = "sign"),
                        colVar = "region", rowSelect = unique(bev_basis_R[, c("regnskapsomfang", "kontoklasse", "art", "funksjon")]),
                        inputInOutput = TRUE)
  # Dersom man bruker inputInOutput = FALSE, så blir bare de nye aggregatene med i output.
  # rowSelect gjør at output begrenses til kodekombinasjoner som fins i input.


  # Her er kode som først gjør et forsøk på å lage en variabel eka med aggregeringskode i datasettet.
  # Deretter bruker klassisk aggregate i R med data.frame-input.
  aggregeringshierarki$from <- FixRegionkode(aggregeringshierarki$from)
  eka01to20 <- WildcardGlobbingVector(unique(aggregeringshierarki$to), c("EKA0*", "EKA1*", "EKA2*", "EKA5*"))
  hierarki01to20 <- aggregeringshierarki[aggregeringshierarki$to %in% eka01to20, ]
  bev_basis_R$eka <- CharacterReCode(bev_basis_R$region, hierarki01to20$from, hierarki01to20$to)
  aggdata <- aggregate(bev_basis_R[, "belop",drop=FALSE], bev_basis_R[,  c("regnskapsomfang", "kontoklasse",  "art", "funksjon", "eka") ], sum)
}












