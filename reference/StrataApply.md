# INTERNAL FUNCTION: Apply a function with list output over data subsets and structure output into various matrices.

INTERNAL FUNCTION: Apply a function with list output over data subsets
and structure output into various matrices.

## Usage

``` r
StrataApply(
  data,
  by,
  Fun,
  copyVar = NULL,
  byName = NULL,
  byNameDefault = "strata",
  multiUnlist = FALSE,
  FunTotal = NULL,
  FunMultiTotal = NULL,
  ...
)
```

## Arguments

- data:

  Data frame

- by:

  Separate vector or Variable in data (name or number) defining the
  subsets (strata)

- Fun:

  Function to be applied on data subsets

- copyVar:

  Variables from input data to be directly copied to output

- byName:

  Name to be used in output of variable defining subsets

- byNameDefault:

  Name to be used when byName=NULL and when name cannot be taken from
  input variable

- multiUnlist:

  When TRUE output elements "multi\*" are split as separate elements.

- FunTotal:

  A function to be applied on output element "aggregates" so that output
  element "total" is produced

- FunMultiTotal:

  A function to be applied on output element "multiAggregates" so that
  output element "multiTotal" is produced

- ...:

  Further arguments passed to Fun

## Value

Output is a list with elements:

- micro:

  Matrix with as many rows as data - composed of vector output.

- aggregates:

  Matrix with as many rows as the number of subsets - composed of scalar
  output.

- total:

  Result of FunTotal (typically a matrix with a single row).

- multiMicro:

  List of matrices with as many rows as data - composed of matrix
  output.

- multiAggregates:

  List of matrices with as many rows as the number of subsets - composed
  of single row matrix output.

- multiTotal:

  Result of FunMultiTotal (typically list of matrices with a single
  row).

- other:

  List of output that did not fit into other elements.

Empty elements are removed from output. Elements may also be changed due
to multiUnlist.

## Examples

``` r
 FunFun = function(data,x){
 list(max=max(data[,x]),plus100=data[,x]+100,range=t(range(data[,x])),
      xsqrtx5=cbind(data[,x],sqrt(data[,x]),5),onefive=t(1:5))}
 z = data.frame(x=(1:10)^2,k=c(1,2,2,3,3,3,4,4,4,4),six=rep(6,10))
 FunFun(z,"x")
#> $max
#> [1] 100
#> 
#> $plus100
#>  [1] 101 104 109 116 125 136 149 164 181 200
#> 
#> $range
#>      [,1] [,2]
#> [1,]    1  100
#> 
#> $xsqrtx5
#>       [,1] [,2] [,3]
#>  [1,]    1    1    5
#>  [2,]    4    2    5
#>  [3,]    9    3    5
#>  [4,]   16    4    5
#>  [5,]   25    5    5
#>  [6,]   36    6    5
#>  [7,]   49    7    5
#>  [8,]   64    8    5
#>  [9,]   81    9    5
#> [10,]  100   10    5
#> 
#> $onefive
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]    1    2    3    4    5
#> 
 StrataApply(z,"k",FunFun,copyVar="six",x="x")
#> $micro
#>    six k plus100
#> 1    6 1     101
#> 2    6 2     104
#> 3    6 2     109
#> 4    6 3     116
#> 5    6 3     125
#> 6    6 3     136
#> 7    6 4     149
#> 8    6 4     164
#> 9    6 4     181
#> 10   6 4     200
#> 
#> $aggregates
#>   k max
#> 1 1   1
#> 2 2   9
#> 3 3  36
#> 4 4 100
#> 
#> $multiMicro
#> $multiMicro$xsqrtx5
#>       [,1] [,2] [,3]
#>  [1,]    1    1    5
#>  [2,]    4    2    5
#>  [3,]    9    3    5
#>  [4,]   16    4    5
#>  [5,]   25    5    5
#>  [6,]   36    6    5
#>  [7,]   49    7    5
#>  [8,]   64    8    5
#>  [9,]   81    9    5
#> [10,]  100   10    5
#> 
#> 
#> $multiAggregates
#> $multiAggregates$range
#>      [,1] [,2]
#> [1,]    1    1
#> [2,]    4    9
#> [3,]   16   36
#> [4,]   49  100
#> 
#> $multiAggregates$onefive
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]    1    2    3    4    5
#> [2,]    1    2    3    4    5
#> [3,]    1    2    3    4    5
#> [4,]    1    2    3    4    5
#> 
#> 
 StrataApply(z,"k",FunFun,copyVar="six",x="x",multiUnlist=TRUE)
#> $micro
#>    six k plus100
#> 1    6 1     101
#> 2    6 2     104
#> 3    6 2     109
#> 4    6 3     116
#> 5    6 3     125
#> 6    6 3     136
#> 7    6 4     149
#> 8    6 4     164
#> 9    6 4     181
#> 10   6 4     200
#> 
#> $aggregates
#>   k max
#> 1 1   1
#> 2 2   9
#> 3 3  36
#> 4 4 100
#> 
#> $xsqrtx5
#>       [,1] [,2] [,3]
#>  [1,]    1    1    5
#>  [2,]    4    2    5
#>  [3,]    9    3    5
#>  [4,]   16    4    5
#>  [5,]   25    5    5
#>  [6,]   36    6    5
#>  [7,]   49    7    5
#>  [8,]   64    8    5
#>  [9,]   81    9    5
#> [10,]  100   10    5
#> 
#> $range
#>      [,1] [,2]
#> [1,]    1    1
#> [2,]    4    9
#> [3,]   16   36
#> [4,]   49  100
#> 
#> $onefive
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]    1    2    3    4    5
#> [2,]    1    2    3    4    5
#> [3,]    1    2    3    4    5
#> [4,]    1    2    3    4    5
#> 
```
