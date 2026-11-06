# Combine two frequency data frames

The process is done by constructing fictive micro data in accordance
with both input data sets.

## Usage

``` r
FreqMerge(x, y, xFreqVar = "freq", yFreqVar = "freq", freqVar = "freq")
```

## Arguments

- x:

  data frame

- y:

  data frame

- xFreqVar:

  NULL or name/number of x-variable holding counts

- yFreqVar:

  NULL or name/number of y-variable holding counts

- freqVar:

  NULL or name of output-variable holding counts

## Value

A data frame

## Details

Using NULL as input for xFreqVar, yFreqVar, or freqVar means microdata
instead of frequency data

## Examples

``` r

# Create example data
z3 <- EasyData("z3")
zAmicro <- MakeMicro(z3[,c(1,4,7)],"ant")[-3]
zA = MakeFreq(zAmicro)
zBmicro = MakeMicro(z3[,c(2,4,5,7)],"ant")[,-4]
zB = MakeFreq(zBmicro)

# Three way of constructing the same output
zAB1 = FreqMerge(zA,zB)
zAB2 = FreqMerge(zAmicro,zBmicro,NULL,NULL)
zAB3 = MakeFreq(FreqMerge(zAmicro,zBmicro,NULL,NULL,NULL))
```
