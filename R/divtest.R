

exData1old = function(){
exData   <- KostraData("z3w")[,c(1:15,15,4:6)]
names(exData)[12:19]=c("s1","s2","s3","s4","A","B","C","D")
exData[,"s4"] <- rowSums(exData[,4:11]) - rowSums(exData[,12:14])

z  = exData[,1:15]
cz = colnames(z)
cz = str_replace(cz,"m10m12","B")
cz = str_replace(cz,"m01m05","A")
cz = str_replace(cz,"soshjelp","soshj")
cz = str_replace(cz,"arbeid","arb")
colnames(z) = cz
z
}

exData1 = function(){
  exData   <- KostraData("z3w")[,c(1:15,15,4:6)]
  names(exData)[12:19]=c("s1","s2","s3","s4","W","X","Y","Z")
  exData[,"s4"] <- rowSums(exData[,4:11]) - rowSums(exData[,12:14])
  z  = exData
  cz = colnames(z)
  cz = str_replace(cz,"m10m12","B")
  cz = str_replace(cz,"m01m05","A")
  cz = str_replace(cz,"soshjelp","soshj")
  cz = str_replace(cz,"arbeid","arb")
  colnames(z) = cz
  for(i in 4:19)
    z[,i] = as.integer(z[,i])
  z
}

microEx1 = function(){
  z2 <- KostraData("z2")
  z2micro <- MakeMicro(z2,"ant")[,-5]
  z2micro
}



