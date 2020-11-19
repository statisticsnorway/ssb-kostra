context("KostraRegnskap")

#options(stringsAsFactors = FALSE)

test_that("KostraRegnskap - beregningstester", {
  a=KostraData("kostraRegnskapDataPen")

  co <- capture.output({
    z <- suppressWarnings(  # unngår warning
      eval(as.call(c(as.name("KostraRegnskap"),a[-7],
                     list(arter =  c("AGD9","AGID1","AGI14","AGD32","AGD65","800","AG20","AG11")),
                     list(funksjoner=c("FG2","FG4","120","800"))))))})


  arter =  rep(c("AGD9","AGID1","AGI14","AGD32","AGD65", "800", "AG20", "AG11"),2)
  kontoklasser= c("D","D","D","D","D","D", "I", "D","I","I","D","I","D","I","D", "I")
  perioder =  c(rep("2015",8),rep("2016",8) )
  perioder[13] = "2015"
  regnskapsomfang = c("A","A","A","A","A","B","B", "A" ,"B","A","A","B","B","A","A", "A")
  funksjoner= c(rep("FG2",8),"120","800","FG4","FG2","FG2","800","FG2", "FG2")
  regioner = rep_len(unique(a$data$region),16)
  regioner[c(4,5,13)] = "0301"

  zBI = vector("list",16)
  zBH = vector("list",16)
  zBIr = vector("list",16)
  zBHI = vector("list",16)
  frameBI = NULL
  frameBH = NULL
  for(i in 1:16){
    co <- capture.output({
      flush.console()
      zBI[[i]]  <- suppressWarnings(  # unngår warning
      KostraRegnskapBeregningInput(a$data, a$funksjonshierarki, a$artshierarki, a$data_saer, a$artshierarki_nettinger,a$artshierarki_nettinger_kasse,
                                 formler = a$formler,
                                 perioder =  perioder[i],
                                 arter =  arter[i],
                                 funksjoner= funksjoner[i],
                                 kontoklasser= kontoklasser[i],
                                 regnskapsomfang = regnskapsomfang[i]))
      zBH[[i]]  <- suppressWarnings(  # unngår warning
        KostraRegnskapBeregningHierarki(a$data, a$funksjonshierarki, a$artshierarki, a$data_saer, a$artshierarki_nettinger,a$artshierarki_nettinger_kasse,
                                     formler = a$formler,
                                     perioder =  perioder[i],
                                     arter =  arter[i],
                                     funksjoner= funksjoner[i],
                                     kontoklasser= kontoklasser[i],
                                     regnskapsomfang = regnskapsomfang[i]))
      zBIr[[i]]  <- suppressWarnings(  # unngår warning
        KostraRegnskapBeregningInput(a$data, a$funksjonshierarki, a$artshierarki, a$data_saer, a$artshierarki_nettinger,a$artshierarki_nettinger_kasse,
                                     regioner = regioner[i],
                                     formler = a$formler,
                                     perioder =  perioder[i],
                                     arter =  arter[i],
                                     funksjoner= funksjoner[i],
                                     kontoklasser= kontoklasser[i],
                                     regnskapsomfang = regnskapsomfang[i]))
      zBHI[[i]]  <- suppressWarnings(  # unngår warning   rep_len(regioner,14)
        KostraRegnskapBeregningHierarkiInput(a$data, a$funksjonshierarki, a$artshierarki, a$data_saer, a$artshierarki_nettinger,a$artshierarki_nettinger_kasse,
                                        regioner = regioner[i],
                                        formler = a$formler,
                                        perioder =  perioder[i],
                                        arter =  arter[i],
                                        funksjoner= funksjoner[i],
                                        kontoklasser= kontoklasser[i],
                                        regnskapsomfang = regnskapsomfang[i]))

      kCand =  which(zBHI[[i]]$regnskapsomfang %in% c("OUTPUT_kasse", "OUTPUT_nett_kasse", "OUTPUT_nett_sbedr", "OUTPUT_sbedr")
            & zBHI[[i]]$belop>0
            & !grepl(":",zBHI[[i]]$from))

      okK = length(kCand>0)
      if(okK)
        k = sample(kCand,1)

      if(okK)
      zBIrk  <- suppressWarnings(  # unngår warning
        KostraRegnskapBeregningInput(a$data, a$funksjonshierarki, a$artshierarki, a$data_saer, a$artshierarki_nettinger,a$artshierarki_nettinger_kasse,
                                     regioner = regioner[i],
                                     perioder =  perioder[i],
                                     arter =  zBHI[[i]][k, "art"],
                                     funksjoner= zBHI[[i]][k, "funksjon"],
                                     kontoklasser= zBHI[[i]][k, "kontoklasse"],
                                     regnskapsomfang = regnskapsomfang[i]))

      })
    expect_false(anyNA(Match(zBIr[[i]]  ,zBHI[[i]][,-9])))
    if(okK) expect_false(anyNA(Match(zBHI[[i]][which(zBHI[[i]][,"from"] == zBHI[[i]][k,"from"]),-9], zBIrk)))
    for(reg in unique(regioner)){
      zBI_i_reg = zBI[[i]][zBI[[i]]$region== reg,]
      frameBI = rbind(frameBI,
                      cbind(zBI_i_reg[1, ,drop=FALSE],
                            regnskapsomfang_ =regnskapsomfang[i], stringsAsFactors = FALSE,
                      belop2 = sum(zBI_i_reg$sign*zBI_i_reg$belop)))
      frameBH = rbind(frameBH,
                      cbind(tail(zBH[[i]][zBH[[i]]$region== reg,],1), regnskapsomfang_ =regnskapsomfang[i], stringsAsFactors = FALSE))
    }
  }

  names(z)[names(z)=="regnskapsomfang"] = "regnskapsomfang_"
  expect_equal(anyNA(Match(frameBI[, names(z)],z)),FALSE)
  expect_equivalent(frameBI[, names(z)],frameBH[, names(z)])
  expect_equal(frameBI$belop,frameBI$belop2)
  expect_false(anyNA(Match(zBHI[[5]],zBHI[[4]])))
  expect_false(anyNA(Match(zBHI[[13]],zBHI[[5]])))
  expect_equal(sum(is.na(Match(zBH[[13]],zBH[[5]]))), 38)

  i=3
  co <- capture.output({
    zBI_3_0101 = KostraRegnskapBeregningInput(a$data, a$funksjonshierarki, a$artshierarki, a$data_saer, a$artshierarki_nettinger,a$artshierarki_nettinger_kasse,
                               regioner = "0101",
                               formler = a$formler,
                               perioder =  perioder[i],
                               arter =  arter[i],
                               funksjoner= funksjoner[i],
                               kontoklasser= kontoklasser[i],
                               regnskapsomfang = regnskapsomfang[i])})

  expect_false(anyNA(Match(zBI_3_0101,zBI[[i]])))
  expect_equal(sum(zBI_3_0101$belop==0), 0L)


  for(i in 2:3) for(reg in c("0101", "0300")){
    x=zBH[[i]][zBH[[i]]$region== "0101",]
    xInput = x[x$regnskapsomfang  %in% c("Input_Kasse", "Input_Saer", "input_kasse", "input_sbedr"), ]
    xInput = xInput[!duplicated(xInput$from), ]
    hierarchyVarNames = c(mapsFrom = "from", mapsTo = "to", sign = "sign", level = "level")
    co <- capture.output({
      a=HierarchyCompute(xInput, list(from=x),"belop",hierarchyVarNames =hierarchyVarNames, inputInOutput=TRUE)
    })
    expect_equal(sum(is.na(Match(a,x[, names(a)]))), 1L)
    expect_equal(dim(a)[1] - dim(unique(x[, names(a)]))[1], 1L)
    expect_equal(sum(duplicated(x[, c("to", "from")])), 0L)}

})


test_that("KostraRegnskap - Vanlig beregning og sjekk av kodefix og at eval(as.call(... fungerer", {
  a=KostraData("kostraRegnskapDataPen")
  b=KostraData("kostraRegnskapData")

  co <- capture.output({  # unngår ustkrift
  zVanlig <-  KostraRegnskap(a$data, a$funksjonshierarki, a$artshierarki, a$data_saer, a$artshierarki_nettinger,a$artshierarki_nettinger_kasse,
                               stjernetabell = a$stjernetabell, formler = a$formler,
                               arter =  c("AGD9","AGID1","AGI14","AGD32","AGD65"),
                               funksjoner=c("FG2","FG1"))})

  co <- capture.output({
  zEval <- suppressWarnings(  # unngår warning
                 eval(as.call(c(as.name("KostraRegnskap"),b,
                 list(arter =  c("AGD9","AGID1","AGI14","AGD32","AGD65")),
                 list(funksjoner=c("FG2","FG1"))))))})

  expect_equal(zEval, zVanlig)
  expect_equal(sum(zEval$belop),1270849893L)
  expect_equivalent(dim(zEval), c(192,7))
  expect_equivalent(names(zEval), c("periode", "regnskapsomfang", "region", "funksjon", "kontoklasse", "art", "belop"))
  expect_equivalent(sapply(zEval,class),  c(rep("character",6),"integer"))


  co <- capture.output({
    z <- suppressWarnings(  # unngår warning
      eval(as.call(c(as.name("KostraRegnskap"),a,
                     list(perioder="2015")))))})

  expect_equal(sum(z$belop),9419229115)
  expect_equivalent(dim(z), c(357112,7))
  expect_equivalent(names(z), c("periode", "regnskapsomfang", "region", "funksjon", "kontoklasse", "art", "belop"))
  expect_equivalent(sapply(z,class),  c(rep("character",6),"integer"))
  rr <- Match(zEval,z)
  expect_equivalent(z[rr[!is.na(rr)],],zEval[zEval$periode=="2015",])



  co <- capture.output({  # unngår ustkrift
    zVanlig2015 <-  KostraRegnskap(a$data[a$data$periode=="2015"& a$data$region=="0301", ], a$funksjonshierarki, a$artshierarki,
                                   a$data_saer[a$data_saer$periode=="2015" & a$data_saer$region=="0301", ], a$artshierarki_nettinger,a$artshierarki_nettinger_kasse,
                                   stjernetabell = a$stjernetabell, formler = a$formler,
                                   arter =  c("AGD9","AGID1","AGI14","AGD32","AGD65"),
                                   funksjoner=c("FG2","FG1"))})

  expect_equivalent(zVanlig2015, zEval[zEval$periode=="2015" & zEval$region=="0301" ,])

})


test_that("KostraRegnskap - gammel rutine og test av ulike HierarchyCompute varianter", {
  a=KostraData("kostraRegnskapDataPen")
  b=KostraData("kostraRegnskapData")

  co1 <- capture.output({
    z1 <-  suppressWarnings(KostraRegnskap(a$data, a$funksjonshierarki, a$artshierarki, a$data_saer,
                                           a$artshierarki_nettinger,perioder="2015",regnskapsomfang="B"))
  })

  co2 <- capture.output({
    z2 <-  suppressWarnings(KostraRegnskap(b$data, b$funksjonshierarki, b$artshierarki, perioder="2015"))
  })

  co3 <- capture.output({
    z3 <-  suppressWarnings(KostraRegnskap(a$data, a$funksjonshierarki, a$artshierarki,
                                           a$data_saer,a$artshierarki_nettinger,perioder="2015",
                          regnskapsomfang="B", stjernetabell = a$stjernetabell))
  })


  # Samme svar med "gammel og ny" metode (ReductionKhatriRao = KhatriRao)
  expect_equal(anyNA(Match(z1[,names(z2)],z2)), FALSE)

  # Alle tre varianter er i bruk
  #  Tar bort siden verbose satt til FALSE som default  i HierarchyCompute
  # expect_equal(length(grep("ReductionKhatriRao", co1)),1)
  # expect_equal(length(grep(" KhatriRao", co2)),1)
  # expect_equal(length(grep("SelectionByMultiplication", co3)),1)

  # Sammes svar. Dvs, det som er felles på koder er også felles på beløp (ReductionKhatriRao = SelectionByMultiplication)
  expect_equal( Match(z3[,names(z2)],z2), Match(z3[,names(z2)][,-7],z2[,-7]))


})










