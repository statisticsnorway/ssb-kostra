context("RoundKostra")

#options(stringsAsFactors = FALSE)

test_that("RoundKostra", {

  z2w <- IntegerDataFrame(KostraData("z2w"))

  expect_false(any(RoundKostra(z2w ,idVar="region", freqVar="arbeid", roundBase=5)$arbeid  %in% 1:4))

  set.seed(123)
  expect_equal(
    RoundKostra(z2w ,idVar="region", freqVar="arbeid", formula="A*B+C", A="fylke",B="kostragr",C="annet")$arbeid,
    c(11, 0, 8, 3, 14, 9, 4, 3, 0, 0, 3))

  # ==========  With no single-groups  ================
  ex1 = Kostra:::exData1()   #  hack endre seinere
  freqVarGroup <- c(1,1,1,1,1,1,1,1,2,2,2,2)

  suppressWarnings(RNGversion("3.5.0"))
  set.seed(123)
  a = RoundKostra(ex1, idVar="region",strataVar=c("fylke","kostragr"),freqVar=4:15,freqVarGroup=freqVarGroup, makeSums=FALSE)
  b = table(as.vector(as.matrix(a[,-(1:3)])))
  expect_false(any(names(b) %in% c("1", "2")))
  expect_equal(names(b[b>4]), c("0", "3", "4", "5", "6", "7", "8", "9", "11", "12", "22")) # Endring pga easyCheck

  # ==========  With some single-groups  ================
  #freqVarGroup <- c(1,1,1,1,1,1,1,1,2,2,2,2,3,4,-1,5)
  #set.seed(123)
  #a = RoundKostra(ex1, idVar="region",strataVar=c("fylke","kostragr"),freqVar=4:19,freqVarGroup=freqVarGroup)

  z3 <- IntegerDataFrame(KostraData("z3"))
  mf <- ~region*mnd + hovedint*mnd + fylke*hovedint*mnd + kostragr*hovedint*mnd
  set.seed(123)
  a <- RoundViaDummy(SmallCountData('z3'), 'ant', mf, 5, easyCheck = FALSE)
  b = table(a$yPublish[,2])
  expect_false(any(names(b) %in% c("1", "2", "3", "4")))
  expect_equal(names(b[b>4]), c("0", "5", "7", "8", "10", "11", "12", "15", "18", "20", "44"))

})
