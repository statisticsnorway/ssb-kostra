# INTERNAL FUNCTION: Extract a single component from multivariate data

Rows with any missing elements give corresponding missing values in
output.

## Usage

``` r
OneComp(
  data,
  variables = 1:dim(data)[2],
  method = c("mean", "pca", "pcaMedian", "pcaStd")
)
```

## Arguments

- data:

  Data frame or matrix containing the variables

- variables:

  Indices of variables to extract component from

- method:

  Method coded as a string: "mean" (default),"pca","pcaMedian" or
  "pcaStd" where: "mean" means rowMeans. "pcaMedian" means pca/svd on
  data scaled by median of absolute values. "pcaStd" means pca/svd on
  data centered and scaled by st.dev.

## Value

A single component is retuned as a vector. This component is scaled to
be a weighted row-sum of input so that sum(abs(weights))=1.

## Examples

``` r
 OneComp(matrix(rnorm(15),5,3),method="pca")
#> [1]  1.5981417  0.4423841 -1.1754409  0.5725333 -0.3940014
```
