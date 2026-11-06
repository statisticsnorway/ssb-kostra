
# Updated code in LmImpute.pdf to ensure compatibility with the latest package versions.

library(CalibrateSSB) 
library(Kostra)     

# Copy from updated ImputeRegression example code
rateData <- KostraData("rateData")               # Real Kostra data set
w <- rateData$data[, c(17,19,16,5)]              # Data with id, strata, x and y
w <- w[is.finite(w[,"Ny.kostragruppe"]), ]       # Remove Longyearbyen
ImputeRegression(w, strataName = names(w)[2])    # Works without combining strata
w[w[,"Ny.kostragruppe"]>13,"Ny.kostragruppe"]=13 # Combine small strata

a <- ImputeRegression(w, strataName = names(w)[2],limitModel = 50, limitIterate = 50)

# Exact as code in LmImpute.pdf
z <- as.data.frame(a$micro)
z$R <- as.numeric(is.finite(z$y))
N <- dim(z)[1]
b <- CalibrateSSB(
  z,
  calmodel = "~ x",
  partition = "strata",
  y = "y",
  leverageOutput = TRUE
)
bWide <- WideFromCalibrate(b, rep(1, N), 1:N, subSet = z$strata)
p <- PanelEstimation(bWide, "y", leveragePower = 1)

# SortRows() for easy comparison
SortRows(a$aggregates)

# Exact as code in LmImpute.pdf
for (i in 1:12) {
  cat(
    names(p)[i], p[[i]]$Estimates,
    sqrt(p[[i]]$varEstimates), "\n"
  )
}