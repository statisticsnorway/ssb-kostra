# INTERNAL FUNCTION: Repeated regression imputation.

LmImpute is run several times using different versions of input
parameters. Output from each run of LmImpute is in output of this
function. In each run it is possible to update the interest variable (y)
with available imputed values. Thus the function can be used to impute
using a primary x-variable and a secondary x-variable for cases where
the primary is missing.

## Usage

``` r
LmImputeMulti(
  data,
  yName = "y",
  xModel = c("x1", "x2"),
  yModel = yName,
  weights = NULL,
  limitModel = 2.5,
  limitIterate = 4.5,
  limitImpute = 50,
  maxiter = 10,
  returnIter = TRUE,
  returnYHat = FALSE,
  returnFirst = FALSE,
  returnLast = TRUE,
  returnFinal = FALSE,
  MultiFuction = function(x) {
     max(abs(x))
 },
  estimationGroup = TRUE,
  unfoldCoef = FALSE,
  BackTransform = list(NULL),
  warningEstimate = vector("list", n),
  removeEmpty = FALSE,
  replaceByImputed = TRUE,
  imputedInModel = FALSE,
  category123FromFirst = FALSE,
  cvPercent = TRUE,
  returnSameType = FALSE
)

LmImpute2(
  data,
  warningEstimate = list(NULL, "estimate: Missing yImputed replaced by zero"),
  replaceByImputed = TRUE,
  cvPercent = TRUE,
  ...
)

LmImputeOne2Many(
  data,
  yName = "y",
  xModel = "x",
  yModel = c(yName, "yMany"),
  category123FromFirst = TRUE,
  keepSinge = NULL,
  keepMulti = NULL,
  ...
)
```

## Arguments

- data:

  Input data set (data.frame, data.table or list)

- yName:

  Name of interest variable in data set

- xModel:

  Vector of strings with the right part model formula.

- yModel:

  String with left part model formula (vector possible)

- weights:

  NULL or string with weight expression (vector possible)

- limitModel:

  Studentized residuals limit. Above limit -\> category 2.(vector
  possible)

- limitIterate:

  Studentized residuals limit for iterative calculation of studentized
  residuals.(vector possible)

- limitImpute:

  Studentized residuals limit. Above limit -\> category 3. No imputation
  when 0.(vector possible)

- maxiter:

  Maximum number of iterations.(vector possible)

- returnIter:

  When TRUE, iteration when observation was thrown outin output.(vector
  possible)

- returnYHat:

  When TRUE, fitted values and corresponding estimates in output.
  (vector possible)

- returnFirst:

  When TRUE, studentized residuals from first iteration in output.
  (vector possible)

- returnLast:

  When TRUE, some results from last iteration in output.(vector
  possible)

- returnFinal:

  When TRUE, extra results from final model in output. (vector possible)

- MultiFuction:

  Transforming rStud for several responses into a single positive value.

- estimationGroup:

  Total estimates will be computed within each group. Default (and TRUE)
  is a single group (estimationGroup \<- rep(1, N) ).

- unfoldCoef:

  When TRUE several elements of coef will be spilt as several ouput
  elements.

- BackTransform:

  When model contains transformation of y (e.g: "log(y)~x") a function
  (e.g: exp) can be supplied to transform back to original scale before
  calculation of leaveOutResid, yHat, yImputed, estimate, estimateYHat,
  estimateOrig and seRobust. (list possible)

- warningEstimate:

  Warning text when missing values. Use NULL to avoid warning. (list
  possible)

- removeEmpty:

  When TRUE empty elements will be removed from output.

- replaceByImputed:

  When TRUE missing values of the interest variable (y) is replaced by
  imputed values in each round.

- imputedInModel:

  When FALSE above imputed values are omitted from subsequent models
  (category 2 forced).

- category123FromFirst:

  Whene TRUE category123 from first run is input to subsequent LmImpute
  calls.

- cvPercent:

  When TRUE (default) cv output is in percent

- returnSameType:

  When TRUE and when the type of input y variable(s) is integer, the
  output type of yImputed and estimate is also integer. Estimates/sums
  are then calculated from rounded imputed values.

- keepSinge:

  When non-NULL only output elements with names in keepSinge are kept
  from first LmImputeOne2Many run.

- keepMulti:

  When non-NULL only output elements with names in keepMulti are kept
  from second LmImputeOne2Many run.

## Value

Output of LmImputeMulti is a list where each element is the output of
[`LmImpute`](https://statisticsnorway.github.io/ssb-kostra/reference/LmImpute.md).
Output of LmImpute2 is not such a list. Instead the names are changed
using "A" and "B". Output of LmImputeOne2Many is not such a list.
Instead the names from second round are changed using "M".

## Details

LmImputeMulti performs several calls to LmImpute and the number of calls
is the length of xModel. Other parameters can also change between calls
by specifying them as vectors or lists.

LmImpute2 is a specialized variant for two LmImpute runs only and
combined estimates of seRobust, seEStimate and cv are calculated when
replaceByImputed = TRUE.

LmImputeOne2Many is another specialized variant meant for two runs using
the same model except that the first run has a single y and the next run
several y's. Category123 from the first run is used in the second.

## Examples

``` r

# -----  LmImpute2 and LmImputeMulti -----
set.seed(123)  # same results each time
z = data.frame(  # Small test data set
x1 = c(NA,2:19,NA),
x2 = rep(1:20),
y = runif(20)+c(rep(0,15),NA,1E3,1E5,NA,NA))
LmImpute2(z)
#> $Acategory123
#>  [1] 0 1 1 1 1 2 1 1 1 1 1 1 1 1 1 3 3 3 3 0
#> 
#> $N
#> [1] 20
#> 
#> $AnImputed
#> [1] 4
#> 
#> $AestimateOrig
#> [1] 101008.8
#> 
#> $Acoef
#> (Intercept)          x1 
#>  0.74619158 -0.01839066 
#> 
#> $AnModel
#> [1] 14
#> 
#> $ArStud
#>  [1]            NA  3.028524e-01 -1.097380e+00  7.770173e-01  1.058450e+00
#>  [6] -2.605458e+00 -3.084761e-01  1.052993e+00 -1.000843e-01 -3.654455e-01
#> [11]  1.589354e+00 -2.549938e-01  6.236896e-01  3.113201e-01 -1.550398e+00
#> [16]            NA  2.919388e+03  2.840154e+05            NA            NA
#> 
#> $Adffits
#>  [1]         NaN  0.17818274 -0.55621711  0.33967255  0.40055866 -0.86317413
#>  [7] -0.09177708  0.29445999 -0.02798766 -0.10872649  0.52654429 -0.09649960
#> [13]  0.27264544  0.15779543 -0.91217413         NaN         NaN         NaN
#> [19]         NaN         NaN
#> 
#> $Ahii
#>  [1]        NaN 0.25714286 0.20439560 0.16043956 0.12527473 0.09890110
#>  [7] 0.08131868 0.07252747 0.07252747 0.08131868 0.09890110 0.12527473
#> [13] 0.16043956 0.20439560 0.25714286        NaN        NaN        NaN
#> [19]        NaN        NaN
#> 
#> $AleaveOutResid
#>  [1]            NA  1.062046e-01 -3.545012e-01  2.505936e-01  3.272216e-01
#>  [6] -6.550792e-01 -9.726058e-02  3.162926e-01 -3.152723e-02 -1.150238e-01
#> [11]  4.582616e-01 -8.250536e-02  2.030320e-01  1.054683e-01 -4.945864e-01
#> [16]            NA  9.998125e+02  9.999963e+04            NA            NA
#> 
#> $AsigmaHat
#> [1] 0.2905855
#> 
#> $AseEstimate
#> [1] 0.9567087
#> 
#> $AseRobust
#> [1] 1.011323
#> 
#> $Aiter
#>  [1] 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 3 2 1 1
#> 
#> $Bcategory123
#>  [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 3
#> 
#> $yImputed
#>         1         2         3         4         5         6         7         8 
#> 0.2875775 0.7883051 0.4089769 0.8830174 0.9404673 0.0455565 0.5281055 0.8924190 
#>         9        10        11        12        13        14        15        16 
#> 0.5514350 0.4566147 0.9568333 0.4533342 0.6775706 0.5726334 0.1029247 0.4447424 
#>        17        18        19        20 
#> 0.4191530 0.3935637 0.3679744 0.4810972 
#> 
#> $BnImputed
#> [1] 1
#> 
#> $estimate
#> [1] 10.6523
#> 
#> $BestimateOrig
#> [1] 10.1712
#> 
#> $Bcoef
#>  (Intercept)           x2 
#>  0.628798670 -0.007385073 
#> 
#> $BnModel
#> [1] 15
#> 
#> $BrStud
#>  [1] -1.31925666  0.63518863 -0.70616697  1.01598050  1.25619058 -2.11948316
#>  [7] -0.16360244  1.12993545 -0.03634973 -0.33137954  1.51296885 -0.29948296
#> [13]  0.51231079  0.16950439 -1.70948215 -0.19380991 -0.24175089 -0.28698726
#> [19] -0.32948813          NA
#> 
#> $Bdffits
#>  [1] -0.744745002  0.312860902 -0.303542798  0.381911850  0.415955279
#>  [6] -0.629036399 -0.044966623  0.301987952 -0.009990832 -0.098349351
#> [11]  0.500980814 -0.112577056  0.220214561  0.083489051 -0.965034572
#> [16]          NaN          NaN          NaN          NaN          NaN
#> 
#> $Bhii
#>  [1] 0.24166667 0.19523810 0.15595238 0.12380952 0.09880952 0.08095238
#>  [7] 0.07023810 0.06666667 0.07023810 0.08095238 0.09880952 0.12380952
#> [13] 0.15595238 0.19523810 0.24166667        NaN        NaN        NaN
#> [19]        NaN        NaN
#> 
#> $BleaveOutResid
#>  [1] -0.44022340  0.21655674 -0.23418884  0.32385541  0.38681498 -0.58640240
#>  [7] -0.05269916  0.34575103 -0.01172128 -0.10699468  0.45414426 -0.09911502
#> [13]  0.17152814  0.05868289 -0.54738183 -0.06589513 -0.08409938 -0.10230364
#> [19] -0.12050790          NA
#> 
#> $BsigmaHat
#> [1] 0.2987463
#> 
#> $BseEstimate
#> [1] 0.3756314
#> 
#> $BseRobust
#> [1] 0.4064078
#> 
#> $Biter
#>  [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1
#> 
#> $nImputed
#> [1] 5
#> 
#> $seRobust
#> [1] 1.089928
#> 
#> $seEstimate
#> [1] 1.027809
#> 
#> $cv
#> [1] 9.6487
#> 
LmImputeMulti(z)
#> [[1]]
#> [[1]]$category123
#>  [1] 0 1 1 1 1 2 1 1 1 1 1 1 1 1 1 3 3 3 3 0
#> 
#> [[1]]$N
#> [1] 20
#> 
#> [[1]]$yImputed
#>         1         2         3         4         5         6         7         8 
#> 0.2875775 0.7883051 0.4089769 0.8830174 0.9404673 0.0455565 0.5281055 0.8924190 
#>         9        10        11        12        13        14        15        16 
#> 0.5514350 0.4566147 0.9568333 0.4533342 0.6775706 0.5726334 0.1029247 0.4447424 
#>        17        18        19        20 
#> 0.4191530 0.3935637 0.3679744        NA 
#> 
#> [[1]]$nImputed
#> [1] 4
#> 
#> [[1]]$estimate
#> [1] 10.1712
#> 
#> [[1]]$cv
#> [1] 9.406051
#> 
#> [[1]]$estimateOrig
#> [1] 101008.8
#> 
#> [[1]]$coef
#> (Intercept)          x1 
#>  0.74619158 -0.01839066 
#> 
#> [[1]]$nModel
#> [1] 14
#> 
#> [[1]]$rStud
#>  [1]            NA  3.028524e-01 -1.097380e+00  7.770173e-01  1.058450e+00
#>  [6] -2.605458e+00 -3.084761e-01  1.052993e+00 -1.000843e-01 -3.654455e-01
#> [11]  1.589354e+00 -2.549938e-01  6.236896e-01  3.113201e-01 -1.550398e+00
#> [16]            NA  2.919388e+03  2.840154e+05            NA            NA
#> 
#> [[1]]$dffits
#>  [1]         NaN  0.17818274 -0.55621711  0.33967255  0.40055866 -0.86317413
#>  [7] -0.09177708  0.29445999 -0.02798766 -0.10872649  0.52654429 -0.09649960
#> [13]  0.27264544  0.15779543 -0.91217413         NaN         NaN         NaN
#> [19]         NaN         NaN
#> 
#> [[1]]$hii
#>  [1]        NaN 0.25714286 0.20439560 0.16043956 0.12527473 0.09890110
#>  [7] 0.08131868 0.07252747 0.07252747 0.08131868 0.09890110 0.12527473
#> [13] 0.16043956 0.20439560 0.25714286        NaN        NaN        NaN
#> [19]        NaN        NaN
#> 
#> [[1]]$leaveOutResid
#>  [1]            NA  1.062046e-01 -3.545012e-01  2.505936e-01  3.272216e-01
#>  [6] -6.550792e-01 -9.726058e-02  3.162926e-01 -3.152723e-02 -1.150238e-01
#> [11]  4.582616e-01 -8.250536e-02  2.030320e-01  1.054683e-01 -4.945864e-01
#> [16]            NA  9.998125e+02  9.999963e+04            NA            NA
#> 
#> [[1]]$sigmaHat
#> [1] 0.2905855
#> 
#> [[1]]$seEstimate
#> [1] 0.9567087
#> 
#> [[1]]$seRobust
#> [1] 1.011323
#> 
#> [[1]]$iter
#>  [1] 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 3 2 1 1
#> 
#> 
#> [[2]]
#> [[2]]$category123
#>  [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 3
#> 
#> [[2]]$N
#> [1] 20
#> 
#> [[2]]$yImputed
#>         1         2         3         4         5         6         7         8 
#> 0.2875775 0.7883051 0.4089769 0.8830174 0.9404673 0.0455565 0.5281055 0.8924190 
#>         9        10        11        12        13        14        15        16 
#> 0.5514350 0.4566147 0.9568333 0.4533342 0.6775706 0.5726334 0.1029247 0.4447424 
#>        17        18        19        20 
#> 0.4191530 0.3935637 0.3679744 0.4810972 
#> 
#> [[2]]$nImputed
#> [1] 1
#> 
#> [[2]]$estimate
#> [1] 10.6523
#> 
#> [[2]]$cv
#> [1] 3.526293
#> 
#> [[2]]$estimateOrig
#> [1] 10.1712
#> 
#> [[2]]$coef
#>  (Intercept)           x2 
#>  0.628798670 -0.007385073 
#> 
#> [[2]]$nModel
#> [1] 15
#> 
#> [[2]]$rStud
#>  [1] -1.31925666  0.63518863 -0.70616697  1.01598050  1.25619058 -2.11948316
#>  [7] -0.16360244  1.12993545 -0.03634973 -0.33137954  1.51296885 -0.29948296
#> [13]  0.51231079  0.16950439 -1.70948215 -0.19380991 -0.24175089 -0.28698726
#> [19] -0.32948813          NA
#> 
#> [[2]]$dffits
#>  [1] -0.744745002  0.312860902 -0.303542798  0.381911850  0.415955279
#>  [6] -0.629036399 -0.044966623  0.301987952 -0.009990832 -0.098349351
#> [11]  0.500980814 -0.112577056  0.220214561  0.083489051 -0.965034572
#> [16]          NaN          NaN          NaN          NaN          NaN
#> 
#> [[2]]$hii
#>  [1] 0.24166667 0.19523810 0.15595238 0.12380952 0.09880952 0.08095238
#>  [7] 0.07023810 0.06666667 0.07023810 0.08095238 0.09880952 0.12380952
#> [13] 0.15595238 0.19523810 0.24166667        NaN        NaN        NaN
#> [19]        NaN        NaN
#> 
#> [[2]]$leaveOutResid
#>  [1] -0.44022340  0.21655674 -0.23418884  0.32385541  0.38681498 -0.58640240
#>  [7] -0.05269916  0.34575103 -0.01172128 -0.10699468  0.45414426 -0.09911502
#> [13]  0.17152814  0.05868289 -0.54738183 -0.06589513 -0.08409938 -0.10230364
#> [19] -0.12050790          NA
#> 
#> [[2]]$sigmaHat
#> [1] 0.2987463
#> 
#> [[2]]$seEstimate
#> [1] 0.3756314
#> 
#> [[2]]$seRobust
#> [1] 0.4064078
#> 
#> [[2]]$iter
#>  [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1
#> 
#> 
LmImputeMulti(z,xModel=c("x1","x2","x2"),limitImpute=c(50,50,3), replaceByImputed=FALSE)
#> [[1]]
#> [[1]]$category123
#>  [1] 0 1 1 1 1 2 1 1 1 1 1 1 1 1 1 3 3 3 3 0
#> 
#> [[1]]$N
#> [1] 20
#> 
#> [[1]]$yImputed
#>         1         2         3         4         5         6         7         8 
#> 0.2875775 0.7883051 0.4089769 0.8830174 0.9404673 0.0455565 0.5281055 0.8924190 
#>         9        10        11        12        13        14        15        16 
#> 0.5514350 0.4566147 0.9568333 0.4533342 0.6775706 0.5726334 0.1029247 0.4447424 
#>        17        18        19        20 
#> 0.4191530 0.3935637 0.3679744        NA 
#> 
#> [[1]]$nImputed
#> [1] 4
#> 
#> [[1]]$estimate
#> [1] 10.1712
#> 
#> [[1]]$cv
#> [1] 9.406051
#> 
#> [[1]]$estimateOrig
#> [1] 101008.8
#> 
#> [[1]]$coef
#> (Intercept)          x1 
#>  0.74619158 -0.01839066 
#> 
#> [[1]]$nModel
#> [1] 14
#> 
#> [[1]]$rStud
#>  [1]            NA  3.028524e-01 -1.097380e+00  7.770173e-01  1.058450e+00
#>  [6] -2.605458e+00 -3.084761e-01  1.052993e+00 -1.000843e-01 -3.654455e-01
#> [11]  1.589354e+00 -2.549938e-01  6.236896e-01  3.113201e-01 -1.550398e+00
#> [16]            NA  2.919388e+03  2.840154e+05            NA            NA
#> 
#> [[1]]$dffits
#>  [1]         NaN  0.17818274 -0.55621711  0.33967255  0.40055866 -0.86317413
#>  [7] -0.09177708  0.29445999 -0.02798766 -0.10872649  0.52654429 -0.09649960
#> [13]  0.27264544  0.15779543 -0.91217413         NaN         NaN         NaN
#> [19]         NaN         NaN
#> 
#> [[1]]$hii
#>  [1]        NaN 0.25714286 0.20439560 0.16043956 0.12527473 0.09890110
#>  [7] 0.08131868 0.07252747 0.07252747 0.08131868 0.09890110 0.12527473
#> [13] 0.16043956 0.20439560 0.25714286        NaN        NaN        NaN
#> [19]        NaN        NaN
#> 
#> [[1]]$leaveOutResid
#>  [1]            NA  1.062046e-01 -3.545012e-01  2.505936e-01  3.272216e-01
#>  [6] -6.550792e-01 -9.726058e-02  3.162926e-01 -3.152723e-02 -1.150238e-01
#> [11]  4.582616e-01 -8.250536e-02  2.030320e-01  1.054683e-01 -4.945864e-01
#> [16]            NA  9.998125e+02  9.999963e+04            NA            NA
#> 
#> [[1]]$sigmaHat
#> [1] 0.2905855
#> 
#> [[1]]$seEstimate
#> [1] 0.9567087
#> 
#> [[1]]$seRobust
#> [1] 1.011323
#> 
#> [[1]]$iter
#>  [1] 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 3 2 1 1
#> 
#> 
#> [[2]]
#> [[2]]$category123
#>  [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 3
#> 
#> [[2]]$N
#> [1] 20
#> 
#> [[2]]$yImputed
#>         1         2         3         4         5         6         7         8 
#> 0.2875775 0.7883051 0.4089769 0.8830174 0.9404673 0.0455565 0.5281055 0.8924190 
#>         9        10        11        12        13        14        15        16 
#> 0.5514350 0.4566147 0.9568333 0.4533342 0.6775706 0.5726334 0.1029247 0.5106375 
#>        17        18        19        20 
#> 0.5032524 0.4958674 0.4884823 0.4810972 
#> 
#> [[2]]$nImputed
#> [1] 5
#> 
#> [[2]]$estimate
#> [1] 11.02511
#> 
#> [[2]]$cv
#> [1] 10.70079
#> 
#> [[2]]$estimateOrig
#> [1] 101008.8
#> 
#> [[2]]$coef
#>  (Intercept)           x2 
#>  0.628798670 -0.007385073 
#> 
#> [[2]]$nModel
#> [1] 15
#> 
#> [[2]]$rStud
#>  [1] -1.319257e+00  6.351886e-01 -7.061670e-01  1.015980e+00  1.256191e+00
#>  [6] -2.119483e+00 -1.636024e-01  1.129935e+00 -3.634973e-02 -3.313795e-01
#> [11]  1.512969e+00 -2.994830e-01  5.123108e-01  1.695044e-01 -1.709482e+00
#> [16]            NA  2.873846e+03  2.805237e+05            NA            NA
#> 
#> [[2]]$dffits
#>  [1] -0.744745002  0.312860902 -0.303542798  0.381911850  0.415955279
#>  [6] -0.629036399 -0.044966623  0.301987952 -0.009990832 -0.098349351
#> [11]  0.500980814 -0.112577056  0.220214561  0.083489051 -0.965034572
#> [16]          NaN          NaN          NaN          NaN          NaN
#> 
#> [[2]]$hii
#>  [1] 0.24166667 0.19523810 0.15595238 0.12380952 0.09880952 0.08095238
#>  [7] 0.07023810 0.06666667 0.07023810 0.08095238 0.09880952 0.12380952
#> [13] 0.15595238 0.19523810 0.24166667        NaN        NaN        NaN
#> [19]        NaN        NaN
#> 
#> [[2]]$leaveOutResid
#>  [1] -4.402234e-01  2.165567e-01 -2.341888e-01  3.238554e-01  3.868150e-01
#>  [6] -5.864024e-01 -5.269916e-02  3.457510e-01 -1.172128e-02 -1.069947e-01
#> [11]  4.541443e-01 -9.911502e-02  1.715281e-01  5.868289e-02 -5.473818e-01
#> [16]            NA  9.997428e+02  9.999955e+04            NA            NA
#> 
#> [[2]]$sigmaHat
#> [1] 0.2987463
#> 
#> [[2]]$seEstimate
#> [1] 1.179773
#> 
#> [[2]]$seRobust
#> [1] 1.30235
#> 
#> [[2]]$iter
#>  [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 3 2 1 1
#> 
#> 
#> [[3]]
#> [[3]]$category123
#>  [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 3
#> 
#> [[3]]$N
#> [1] 20
#> 
#> [[3]]$yImputed
#>         1         2         3         4         5         6         7         8 
#> 0.2875775 0.7883051 0.4089769 0.8830174 0.9404673 0.0455565 0.5281055 0.8924190 
#>         9        10        11        12        13        14        15        16 
#> 0.5514350 0.4566147 0.9568333 0.4533342 0.6775706 0.5726334 0.1029247 0.5106375 
#>        17        18        19        20 
#> 0.5032524 0.4958674 0.4884823 0.4810972 
#> 
#> [[3]]$nImputed
#> [1] 5
#> 
#> [[3]]$estimate
#> [1] 11.02511
#> 
#> [[3]]$cv
#> [1] 10.70079
#> 
#> [[3]]$estimateOrig
#> [1] 101008.8
#> 
#> [[3]]$coef
#>  (Intercept)           x2 
#>  0.628798670 -0.007385073 
#> 
#> [[3]]$nModel
#> [1] 15
#> 
#> [[3]]$rStud
#>  [1] -1.319257e+00  6.351886e-01 -7.061670e-01  1.015980e+00  1.256191e+00
#>  [6] -2.119483e+00 -1.636024e-01  1.129935e+00 -3.634973e-02 -3.313795e-01
#> [11]  1.512969e+00 -2.994830e-01  5.123108e-01  1.695044e-01 -1.709482e+00
#> [16]            NA  2.873846e+03  2.805237e+05            NA            NA
#> 
#> [[3]]$dffits
#>  [1] -0.744745002  0.312860902 -0.303542798  0.381911850  0.415955279
#>  [6] -0.629036399 -0.044966623  0.301987952 -0.009990832 -0.098349351
#> [11]  0.500980814 -0.112577056  0.220214561  0.083489051 -0.965034572
#> [16]          NaN          NaN          NaN          NaN          NaN
#> 
#> [[3]]$hii
#>  [1] 0.24166667 0.19523810 0.15595238 0.12380952 0.09880952 0.08095238
#>  [7] 0.07023810 0.06666667 0.07023810 0.08095238 0.09880952 0.12380952
#> [13] 0.15595238 0.19523810 0.24166667        NaN        NaN        NaN
#> [19]        NaN        NaN
#> 
#> [[3]]$leaveOutResid
#>  [1] -4.402234e-01  2.165567e-01 -2.341888e-01  3.238554e-01  3.868150e-01
#>  [6] -5.864024e-01 -5.269916e-02  3.457510e-01 -1.172128e-02 -1.069947e-01
#> [11]  4.541443e-01 -9.911502e-02  1.715281e-01  5.868289e-02 -5.473818e-01
#> [16]            NA  9.997428e+02  9.999955e+04            NA            NA
#> 
#> [[3]]$sigmaHat
#> [1] 0.2987463
#> 
#> [[3]]$seEstimate
#> [1] 1.179773
#> 
#> [[3]]$seRobust
#> [1] 1.30235
#> 
#> [[3]]$iter
#>  [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 3 2 1 1
#> 
#> 


 # -----  LmImputeOne2Many -----
 # Create data fram z with matrix embedded in one variable
 # The single y is also the first variable in the matrix yMany
 set.seed(123)  # same results each time
 y = runif(20)+c(rep(0,15),NA,1E3,1E5,NA,NA)
 z = data.frame(  # Small test data set
   x = c(1,1,1:10,3,3,3,4,4,5,5,5),
   y = y,
   yMany = I(cbind(y,matrix(1:60,20,3,dimnames=list(NULL,c("A","B","C"))))))
 a=LmImputeOne2Many(z,limitModel=1)
 print(a)
#> $category123
#>  [1] 1 1 1 2 2 2 1 2 1 1 2 1 1 1 2 3 3 3 3 3
#> 
#> $N
#> [1] 20
#> 
#> $yImputed
#>         1         2         3         4         5         6         7         8 
#> 0.2875775 0.7883051 0.4089769 0.8830174 0.9404673 0.0455565 0.5281055 0.8924190 
#>         9        10        11        12        13        14        15        16 
#> 0.5514350 0.4566147 0.9568333 0.4533342 0.6775706 0.5726334 0.1029247 0.5269902 
#>        17        18        19        20 
#> 0.5269902 0.5208706 0.5208706 0.5208706 
#> 
#> $nImputed
#> [1] 5
#> 
#> $estimate
#> [1] 11.16236
#> 
#> $cv
#> [1] 6.915114
#> 
#> $estimateOrig
#> [1] 101008.8
#> 
#> $coef
#> (Intercept)           x 
#>  0.52111299  0.01104661 
#> 
#> $nModel
#> [1] 15
#> 
#> $rStud
#>  [1] -8.858747e-01  9.307077e-01 -4.356614e-01  1.231600e+00  1.398263e+00
#>  [6] -1.998724e+00 -1.609987e-01  1.075081e+00 -1.614298e-01 -5.467762e-01
#> [11]  1.326493e+00 -7.097364e-01  4.169977e-01  6.171800e-02 -1.684959e+00
#> [16]            NA  3.237769e+03  3.236335e+05            NA            NA
#> 
#> $dffits
#>  [1] -0.38690666  0.40648755 -0.19027556  0.44017817  0.41935559 -0.53971725
#>  [7] -0.04402737  0.33265848 -0.05999492 -0.24878693  0.74105963 -0.48730970
#> [13]  0.12506254  0.01850996 -0.50533899         NaN         NaN         NaN
#> [19]         NaN         NaN
#> 
#> $hii
#>  [1] 0.16019417 0.16019417 0.16019417 0.11326861 0.08252427 0.06796117
#>  [7] 0.06957929 0.08737864 0.12135922 0.17152104 0.23786408 0.32038835
#> [13] 0.08252427 0.08252427 0.08252427        NaN        NaN        NaN
#> [19]        NaN        NaN
#> 
#> $leaveOutResid
#>  [1] -2.912365e-01  3.050057e-01 -1.466800e-01  3.832177e-01  4.209533e-01
#>  [6] -5.576409e-01 -5.184812e-02  3.342310e-01 -5.349656e-02 -1.845203e-01
#> [11]  4.412610e-01 -2.622747e-01  1.344099e-01  2.003385e-02 -4.919238e-01
#> [16]            NA  9.996808e+02  9.999947e+04            NA            NA
#> 
#> $sigmaHat
#> [1] 0.2987705
#> 
#> $seEstimate
#> [1] 0.7718902
#> 
#> $seRobust
#> [1] 0.8181103
#> 
#> $iter
#>  [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 3 2 1 1
#> 
#> $Mcategory123
#>  [1] 1 1 1 2 2 2 1 2 1 1 2 1 1 1 2 3 3 3 3 3
#> 
#> $MN
#> [1] 20
#> 
#> $MyImputed
#>            y         A        B        C
#> 1  0.2875775  1.000000 21.00000 41.00000
#> 2  0.7883051  2.000000 22.00000 42.00000
#> 3  0.4089769  3.000000 23.00000 43.00000
#> 4  0.8830174  4.000000 24.00000 44.00000
#> 5  0.9404673  5.000000 25.00000 45.00000
#> 6  0.0455565  6.000000 26.00000 46.00000
#> 7  0.5281055  7.000000 27.00000 47.00000
#> 8  0.8924190  8.000000 28.00000 48.00000
#> 9  0.5514350  9.000000 29.00000 49.00000
#> 10 0.4566147 10.000000 30.00000 50.00000
#> 11 0.9568333 11.000000 31.00000 51.00000
#> 12 0.4533342 12.000000 32.00000 52.00000
#> 13 0.6775706 13.000000 33.00000 53.00000
#> 14 0.5726334 14.000000 34.00000 54.00000
#> 15 0.1029247 15.000000 35.00000 55.00000
#> 16 0.5269902  7.602469 27.60247 47.60247
#> 17 0.5269902  7.602469 27.60247 47.60247
#> 18 0.5208706  8.461728 28.46173 48.46173
#> 19 0.5208706  8.461728 28.46173 48.46173
#> 20 0.5208706  8.461728 28.46173 48.46173
#> 
#> $MnImputed
#> [1] 5
#> 
#> $Mestimate
#>          y        A        B        C
#> 1 11.16236 160.5901 560.5901 960.5901
#> 
#> $Mcv
#>          y       A        B        C
#> 1 3.919745 7.39013 2.117023 1.235472
#> 
#> $MestimateOrig
#>          y   A   B    C
#> 1 101008.8 210 610 1010
#> 
#> $Mcoef
#>                        y         A          B          C
#> (Intercept)  0.551468378 4.1654321 24.1654321 44.1654321
#> x           -0.006119548 0.8592593  0.8592593  0.8592593
#> 
#> $MnModel
#> [1] 9
#> 
#> $MrStud
#>                   y          A          B          C
#>  [1,] -2.474696e+00 -1.0982147 -1.0982147 -1.0982147
#>  [2,]  2.209948e+00 -0.7914529 -0.7914529 -0.7914529
#>  [3,] -9.939421e-01 -0.5151551 -0.5151551 -0.5151551
#>  [4,]  2.027052e+00 -0.4095308 -0.4095308 -0.4095308
#>  [5,]  2.444753e+00 -0.3857022 -0.3857022 -0.3857022
#>  [6,] -2.913273e+00 -0.3575011 -0.3575011 -0.3575011
#>  [7,]  4.547594e-02 -0.3419624 -0.3419624 -0.3419624
#>  [8,]  2.255518e+00 -0.2908560 -0.2908560 -0.2908560
#>  [9,]  2.829034e-01 -0.2876550 -0.2876550 -0.2876550
#> [10,] -3.180068e-01 -0.2648589 -0.2648589 -0.2648589
#> [11,]  2.526246e+00 -0.1817989 -0.1817989 -0.1817989
#> [12,] -3.014682e-01 -0.2273443 -0.2273443 -0.2273443
#> [13,]  9.871350e-01  1.8221212  1.8221212  1.8221212
#> [14,]  2.518184e-01  2.3496347  2.3496347  2.3496347
#> [15,] -2.581753e+00  1.8268952  1.8268952  1.8268952
#> [16,]            NA  1.8734381  1.8734381  1.8734381
#> [17,]  6.049544e+03  2.0965320  2.0965320  2.0965320
#> [18,]  6.041166e+05  2.1243966  2.1243966  2.1243966
#> [19,]            NA  2.3471201  2.3471201  2.3471201
#> [20,]            NA  2.5698435  2.5698435  2.5698435
#> 
#> $Mdffits
#>                 y          A          B          C
#>  [1,] -1.36994408 -0.6079505 -0.6079505 -0.6079505
#>  [2,]  1.22338446 -0.4381331 -0.4381331 -0.4381331
#>  [3,] -0.55022717 -0.2851799 -0.2851799 -0.2851799
#>  [4,]         NaN        NaN        NaN        NaN
#>  [5,]         NaN        NaN        NaN        NaN
#>  [6,]         NaN        NaN        NaN        NaN
#>  [7,]  0.01647741 -0.1239041 -0.1239041 -0.1239041
#>  [8,]         NaN        NaN        NaN        NaN
#>  [9,]  0.13707132 -0.1393735 -0.1393735 -0.1393735
#> [10,] -0.18874011 -0.1571963 -0.1571963 -0.1571963
#> [11,]         NaN        NaN        NaN        NaN
#> [12,] -0.28269786 -0.2131891 -0.2131891 -0.2131891
#> [13,]  0.38303914  0.7070398  0.7070398  0.7070398
#> [14,]  0.09771340  0.9117315  0.9117315  0.9117315
#> [15,]         NaN        NaN        NaN        NaN
#> [16,]         NaN        NaN        NaN        NaN
#> [17,]         NaN        NaN        NaN        NaN
#> [18,]         NaN        NaN        NaN        NaN
#> [19,]         NaN        NaN        NaN        NaN
#> [20,]         NaN        NaN        NaN        NaN
#> 
#> $Mhii
#>  [1] 0.2345679 0.2345679 0.2345679       NaN       NaN       NaN 0.1160494
#>  [8]       NaN 0.1901235 0.2604938       NaN 0.4679012 0.1308642 0.1308642
#> [15]       NaN       NaN       NaN       NaN       NaN       NaN
#> 
#> $MleaveOutResid
#>                   y          A          B          C
#>  [1,] -3.367657e-01 -5.2580645 -5.2580645 -5.2580645
#>  [2,]  3.174107e-01 -3.9516129 -3.9516129 -3.9516129
#>  [3,] -1.781633e-01 -2.6451613 -2.6451613 -2.6451613
#>  [4,]  3.437881e-01 -1.8839506 -1.8839506 -1.8839506
#>  [5,]  4.073576e-01 -1.7432099 -1.7432099 -1.7432099
#>  [6,] -4.814337e-01 -1.6024691 -1.6024691 -1.6024691
#>  [7,]  8.184680e-03 -1.6536313 -1.6536313 -1.6536313
#>  [8,]  3.776680e-01 -1.3209877 -1.3209877 -1.3209877
#>  [9,]  5.285185e-02 -1.4573171 -1.4573171 -1.4573171
#> [10,] -6.206473e-02 -1.4056761 -1.4056761 -1.4056761
#> [11,]  4.604409e-01 -0.8987654 -0.8987654 -0.8987654
#> [12,] -6.942083e-02 -1.4245940 -1.4245940 -1.4245940
#> [13,]  1.662121e-01  7.1988636  7.1988636  7.1988636
#> [14,]  4.547468e-02  8.3494318  8.3494318  8.3494318
#> [15,] -4.301850e-01  8.2567901  8.2567901  8.2567901
#> [16,]            NA  8.3975309  8.3975309  8.3975309
#> [17,]  9.997191e+02  9.3975309  9.3975309  9.3975309
#> [18,]  9.999952e+04  9.5382716  9.5382716  9.5382716
#> [19,]            NA 10.5382716 10.5382716 10.5382716
#> [20,]            NA 11.5382716 11.5382716 11.5382716
#> 
#> $MsigmaHat
#>              y        A        B        C
#> [1,] 0.1566879 4.250034 4.250034 4.250034
#> 
#> $MseEstimate
#>           y        A        B        C
#> 1 0.4375362 11.86782 11.86782 11.86782
#> 
#> $MseRobust
#>           [,1]     [,2]     [,3]     [,4]
#> [1,] 0.4727081 12.11742 12.11742 12.11742
#> 
 a$MrStud[, 1] -  a$rStud # Not equal since no iteration and no "last" when multiple y
#>  [1] -1.588821e+00  1.279240e+00 -5.582807e-01  7.954514e-01  1.046490e+00
#>  [6] -9.145482e-01  2.064747e-01  1.180437e+00  4.443332e-01  2.287694e-01
#> [11]  1.199753e+00  4.082682e-01  5.701373e-01  1.901004e-01 -8.967939e-01
#> [16]            NA  2.811775e+03  2.804831e+05            NA            NA
 a=LmImputeOne2Many(z,limitModel=1,returnFinal=TRUE)
 a$MrStud[, 1] -  a$rStud # Now equal
#>  [1]  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0 NA  0  0 NA NA


```
