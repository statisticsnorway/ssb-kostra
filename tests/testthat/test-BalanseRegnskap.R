context("BalanseRegnskap")




test_that("BalanseRegnskap - Vanlig beregning og sjekk av kodefix og at eval(as.call(... fungerer", {
  a = KostraData("balanseRegnskapDataPen")
  b = KostraData("balanseRegnskapData")


  co <- capture.output({  # unngår ustkrift
    zVanlig <-  BalanseRegnskap(a$data, a$kapittelhierarki)
    })

  co <- capture.output({
    zEval <- suppressWarnings(  # unngår warning
      eval(as.call(c(as.name("BalanseRegnskap"),b))))
  })

  expect_equal(zEval, zVanlig)
  expect_equal(sum(zEval$belop),10408658676)
  expect_equivalent(dim(zEval), c(1992,5))
  expect_equivalent(names(zEval), c("periode", "region", "regnskapsomfang", "kapittel", "belop"))
  expect_equivalent(sapply(zEval,class),  c(rep("character",4),"integer"))


  b$data$periode = as.character(2010 + as.integer(factor(b$data$fylkesregion)))

  b$kapittelhierarki = rbind(b[[2]],b[[2]],b[[2]],b[[2]],b[[2]],b[[2]])
  b$kapittelhierarki$periode = as.character(rep(2011:2016, each=NROW(a[[2]])))

  co <- capture.output({
    z6 <- suppressWarnings(  # unngår warning
      eval(as.call(c(as.name("BalanseRegnskap"),b))))
  })

  expect_equal(anyNA(Match(z6[,-1],zVanlig[,-1])), FALSE)

  co <- capture.output({
    z1 <- BalanseRegnskap(a$data, a$kapittelhierarki, regioner = "0301", omfang = "A", kapitler = "KG3")
    z2 <- BalanseRegnskap(a$data, a$kapittelhierarki, regioner = c("0100", "0301", "2021"), omfang = c("B", "A" ), kapitler = c("KG3", "KG2"))
    z3 <- suppressWarnings(BalanseRegnskap(b$data, b$kapittelhierarki, perioder = c("2011", "2016", "2013"),
                                           omfang = c("B", "A" ), kapitler = c("KG3", "KG2")))
    })


  expect_equal(anyNA(Match(z1, z2)), FALSE)
  expect_equal(anyNA(Match(z2, zVanlig)), FALSE)
  expect_equal(z2[, -1],z3[ , -1])



})




test_that("BalanseRegnskap - beregningstester", {
  a=KostraData("balanseRegnskapDataPen")
  a$data$periode[a$data$region == "2021"] = "2021"
  ak2 = a$kapittelhierarki
  ak2$periode = "2021"
  a$kapittelhierarki = rbind(a$kapittelhierarki,ak2)


  co <- capture.output({
    z <- suppressWarnings(  # unngår warning
      eval(as.call(c(as.name("BalanseRegnskap"),a,
                     list(kapitler =  c("KG3", "KG27","NTO_RENTEEKSP_KOMM_KONSERN", "31", "32"))))))})


  #kapitler = c("KG3", "KG27","NTO_RENTEEKSP_KOMM_KONSERN", "32", "31", "KG3", "32")   ## enkeltarter 31/32 kan ikke brukes i BalanseRegnskapBeregningHierarki
  kapitler = c("KG3", "KG27","NTO_RENTEEKSP_KOMM_KONSERN", "KG3")   ## enkeltarter 31/32 kan ikke brukes i BalanseRegnskapBeregningHierarki
  omfang   = c("A","B","A","A")
  perioder =  c(rep("2016",3), "2021")
  regioner = c("0100", "0301", "0300", "2021")

  zBI = vector("list",4)
  zBH = vector("list",4)
  zBIr = vector("list",4)
  zBHI = vector("list",4)
  frameBI = NULL
  frameBH = NULL
  for(i in 1:4){
    co <- capture.output({
      zBI[[i]]  <- suppressWarnings(  # unngår warning
      BalanseRegnskapBeregningInput(a$data,  a$kapittelhierarki, a$artshierarki,
                                 perioder =  perioder[i],
                                 kapitler =  kapitler[i],
                                 omfang = omfang[i]))
      zBIr[[i]]  <- suppressWarnings(  # unngår warning
        BalanseRegnskapBeregningInput(a$data,  a$kapittelhierarki, a$artshierarki,
                                      regioner = regioner[i],
                                      perioder =  perioder[i],
                                      kapitler =  kapitler[i],
                                      omfang = omfang[i]))
      zBH[[i]]  <- suppressWarnings(  # unngår warning
        BalanseRegnskapBeregningHierarki(a$data,  a$kapittelhierarki, a$artshierarki,
                                      perioder =  perioder[i],
                                      kapitler =  kapitler[i],
                                      omfang = omfang[i]))
      zBHI[[i]]  <- suppressWarnings(  # unngår warning
        BalanseRegnskapBeregningHierarkiInput(a$data,  a$kapittelhierarki, a$artshierarki,
                                      regioner = regioner[i],
                                      perioder =  perioder[i],
                                      kapitler =  kapitler[i],
                                      omfang = omfang[i]))


      k = sample(which(zBHI[[i]]$sign==0 & zBHI[[i]]$belop!=0), 1)


      omf = zBHI[[i]][k, "regnskapsomfang"]
      if(omf=="konsern") omf = "A"
      if(omf=="kasse") omf = "B"
      zBIrk  <- suppressWarnings(  # unngår warning
        BalanseRegnskapBeregningInput(a$data,  a$kapittelhierarki, a$artshierarki,
                                      regioner = regioner[i],
                                      perioder =  perioder[i],
                                      kapitler =  zBHI[[i]][k, "kapittel"],
                                      omfang = omf))
      expect_false(anyNA(Match(zBHI[[i]][which(zBHI[[i]][,"from"] == zBHI[[i]][k,"from"]),-7], zBIrk)))
      })



    for(reg in  unique(zBI[[i]]$region)){
      zBI_i_reg = zBI[[i]][zBI[[i]]$region== reg,]
      frameBI = rbind(frameBI,
                      cbind(zBI_i_reg[1, ,drop=FALSE],
                            regnskapsomfang = omfang[i], stringsAsFactors = FALSE,
                      belop2 = sum(zBI_i_reg$sign*zBI_i_reg$belop)))
      frameBH = rbind(frameBH,
                      cbind(tail(zBH[[i]][zBH[[i]]$region== reg,],1), regnskapsomfang = omfang[i], stringsAsFactors = FALSE))
    }
  }

  expect_equal(anyNA(Match(frameBI[, names(z)],Kostra:::CharacterReCodeRegnskapsomfang(z))),FALSE)
  expect_equivalent(frameBI[, names(z)],frameBH[, names(z)])
  expect_equal(frameBI$belop,frameBI$belop2)

  i=3
  co <- capture.output({
    zBI_3_0101 = BalanseRegnskapBeregningInput(a$data,  a$kapittelhierarki, a$artshierarki,
                                  regioner = "0101",
                                  perioder =  perioder[i],
                                  kapitler =  kapitler[i],
                                  omfang = omfang[i])})

  expect_equal(anyNA(Match(zBI_3_0101,zBI[[i]])),FALSE)
  expect_equal(sum(zBI_3_0101$belop==0), 0L)


  for(i in 2:3) for(reg in c("0101", "0300")){
    x=zBH[[i]][zBH[[i]]$region== "0101",]
    xInput = x[!is.na(suppressWarnings(as.integer(x$kapittel))), ]
    xInput = xInput[!duplicated(xInput$from), ]
    hierarchyVarNames = c(mapsFrom = "from", mapsTo = "to", sign = "sign", level = "level")
    co <- capture.output({
      a=HierarchyCompute(xInput, list(from=x),"belop",hierarchyVarNames =hierarchyVarNames, inputInOutput=TRUE)
    })
    expect_equal(sum(is.na(Match(a,x[, names(a)]))), 1L)
    expect_equal(dim(a)[1] - dim(unique(x[, names(a)]))[1], 1L)
    expect_equal(sum(duplicated(x[, c("to", "from")])), 0L)}

})









