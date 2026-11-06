# Only-Gauss replacement function for easySdcTable::ProtectTable1

protectTable or protect_linked_tables is run with a data set at the only
required input.

## Usage

``` r
ProtectTable1(
  data,
  dimVarInd = 1:NCOL(data),
  freqVarInd = NULL,
  protectZeros = TRUE,
  maxN = 3,
  method = "SIMPLEHEURISTIC",
  findLinked = TRUE,
  total = "Total",
  addName = FALSE,
  sep = ".",
  removeZeros = FALSE,
  dimList = NULL,
  groupVarInd = NULL,
  ind1 = NULL,
  ind2 = NULL,
  dimDataReturn = FALSE,
  IncProgress = IncDefault,
  verbose = FALSE,
  ...
)
```

## Arguments

- data:

  Matrix or data frame

- dimVarInd:

  Column-indices of the main dimensional variables and additional
  aggregating variables.

- freqVarInd:

  Column-indices of a variable holding counts or NULL in the case of
  micro data.

- protectZeros:

  When TRUE empty cells (count=0) is considered sensitive (i.e. same as
  allowZeros in primarySuppression).

- maxN:

  All cells having counts \<= maxN are set as primary suppressed.

- method:

  Parameter "method" in protectTable or protect_linked_tables.
  Alternatively a list defining parameters for running tau-argus (see
  `{ProtectTable}`).

- findLinked:

  When TRUE, the function may find two linked tables and run
  protect_linked_tables.

- total:

  String used to name totals.

- addName:

  When TRUE the variable name is added to the level names, except for
  variables with most levels.

- sep:

  A character string to separate when addName apply.

- removeZeros:

  When TRUE, rows with zero count will be removed from the data.

- dimList:

  See `{ProtectTable}`.

- groupVarInd:

  Possible manual specification if list defining the hierarchical
  variable groups

- ind1:

  Coding of table 1 as indices referring to elements of groupVarInd

- ind2:

  Coding of table 2 as indices referring to elements of groupVarInd

- dimDataReturn:

  When TRUE a data frame containing the dimVarInd variables is retuned

- IncProgress:

  A function to report progress (incProgress in Shiny).

- verbose:

  Parameter sent to protectTable, protect_linked_tables or
  runArgusBatchFile.

- ...:

  Further parameters sent to protectTable, protect_linked_tables or
  createArgusInput.

## Value

Output is a list of three elements.

**table1** consists of the following elements:

- secondary:

  Output from `{protectTable}` or first element of output from
  `{protect_linked_tables}` or output from `{runArgusBatchFile}`.

- primary:

  Output from `{primarySuppression}`.

- problem:

  Output from `{makeProblem}`.

- dimList:

  Generated input to makeProblem.

- ind:

  Indices referring to elements of groupVarInd in the output element
  common.

**table2** consists of elements of the same type as table1 in cases of
two linked tables. Otherwise table2 is NULL.

**common** consists of the following elements:

- commonCells:

  Input to protect_linked_tables.

- groupVarInd:

  List defining the hierarchical variable groups

- info:

  A table summarizing the tables using variable names

- nLevels:

  The number of levels of each variable (only when groupVarInd input is
  NULL)

- dimData:

  Data frame containing the dimVarInd variables when dimDataReturn=TRUE.
  Otherwise NULL.

## Details

One or two tables are identified automatically and subjected to cell
suppression methods in package sdcTable. The tables can alternatively be
specified manually by groupVarInd, ind1 and ind2 (see
`{FindTableGroup}`).

## See also

`{ProtectTable}`, `{HierarchicalGroups}`, `{FactorLevCorr}`,
`{FindDimLists}`, `{FindCommonCells}`

## Examples

``` r
if (FALSE) { # \dontrun{
z2 <- EasyData("z2")
a <- ProtectTable1(z2, c(1, 3, 4), 5)
head(as.data.frame(getInfo(a[[1]][[1]], type = "finalData"))) # The table (not linked)

z3 <- EasyData("z3")
b <- ProtectTable1(z3, 1:6, 7)
head(as.data.frame(getInfo(b[[1]][[1]], type = "finalData"))) # First table
head(as.data.frame(getInfo(b[[2]][[1]], type = "finalData"))) # Second table
} # }
```
