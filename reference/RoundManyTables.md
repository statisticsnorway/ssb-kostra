# RoundManyTables

RoundManyTables

## Usage

``` r
RoundManyTables(
  data,
  idVar = 1,
  strataVar = NULL,
  freqVar = 2,
  freqVarGroup = NULL,
  roundBase = 3,
  method = "pls",
  split = NULL,
  extraOutput = FALSE,
  output = "rounded"
)
```

## Arguments

- data:

  data

- idVar:

  idVar

- strataVar:

  strataVar

- freqVar:

  freqVar

- freqVarGroup:

  freqVarGroup

- roundBase:

  roundBase

- method:

  method

- extraOutput:

  extraOutput

## Value

Data frame som ligner på input og sammensatt med resultat fra mange
avrundinger

## Examples

``` r
# Generate example data for this function
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
# Create input parameter
freqVarGroup <- c(1,1,1,1,1,1,1,1,2,2,2,2)
#aPrikk <- ProtectKostra(z ,idVar="region",strataVar=c("fylke","kostragr"),freqVar=4:15,freqVarGroup=freqVarGroup)
a0 <- RoundManyTables(z ,roundBase=0,idVar="region",strataVar=c("fylke","kostragr"),freqVar=4:15,freqVarGroup=freqVarGroup)
a3 <- RoundManyTables(z ,roundBase=3,idVar="region",strataVar=c("fylke","kostragr"),freqVar=4:15,freqVarGroup=freqVarGroup)
#> [-**.........=-**.:=(Check:28)-**..............:=]
a5 <- RoundManyTables(z ,roundBase=5,idVar="region",strataVar=c("fylke","kostragr"),freqVar=4:15,freqVarGroup=freqVarGroup)
#> [-**..........=-(Check:12)-**.....::=]
```
