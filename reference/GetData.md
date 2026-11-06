# Extracting a data.frame from a data.frame

Extracting a data.frame from a data.frame

## Usage

``` r
GetData(data, ..., removeNULL = TRUE, returnAsDataFrame = TRUE)

GetData1(...)

GetData2me(...)
```

## Arguments

- data:

  A data frame

- ...:

  Input specifying how to extract data (see examples).

- removeNULL:

  When TRUE (default) variables specified as NULL are completely
  removed. Otherwise zero column matrices will be embedded. It is
  possible to specify removeNULL as a vector - one element for each
  variable.

- returnAsDataFrame:

  When TRUE (default) a data.frame is returned. Otherwise a list is
  retuned.

## Value

GetData returns a data frame by default (see details).

## Details

`GetData` returns a data frame with extra attributes (see examples).

To create data according to id matching: The id variable must be the
first variable. This variable must be specified using the list type
input. As opposed to other variable the first element of this list must
be named an this name must be `"id"`. See the examples. (I nyere
versjoner trenger det ikke være første variabel??)

`GetData1` is a single variable variant which returns the variable
instead of a data.frame.

`GetData2me` returns only NULL, but breaks the rules for ordinary
functions. That is, each variable is written to caller's environment (no
data.frame).

## Examples

``` r
### Example data
z <- data.frame(aar = c(2014, 2015, 2016),
                ola = c(4.4, 6.6, 2.2, 3.2, 8.8, 9.9),
                kari = 10 * (1:6),
                tull = c("A", "A", "B", "B", "C", "C"))
print(z)
#>    aar ola kari tull
#> 1 2014 4.4   10    A
#> 2 2015 6.6   20    A
#> 3 2016 2.2   30    B
#> 4 2014 3.2   40    B
#> 5 2015 8.8   50    C
#> 6 2016 9.9   60    C

### Ordinary use: names or numbers
GetData(z, x = "kari", y = "ola")
#>    x   y
#> 1 10 4.4
#> 2 20 6.6
#> 3 30 2.2
#> 4 40 3.2
#> 5 50 8.8
#> 6 60 9.9
GetData(z, A = 3, B  = 2, C = 1)
#>    A   B    C
#> 1 10 4.4 2014
#> 2 20 6.6 2015
#> 3 30 2.2 2016
#> 4 40 3.2 2014
#> 5 50 8.8 2015
#> 6 60 9.9 2016

### With matrix embedded in one variable
a = GetData(z, x = c("kari","ola"), y = "aar")
print(a)
#>   x.kari x.ola    y
#> 1   10.0   4.4 2014
#> 2   20.0   6.6 2015
#> 3   30.0   2.2 2016
#> 4   40.0   3.2 2014
#> 5   50.0   8.8 2015
#> 6   60.0   9.9 2016
print(as.list(a)[-99]) # 99 tric to avoid printing of attributes
#> $x
#>   kari ola
#> 1   10 4.4
#> 2   20 6.6
#> 3   30 2.2
#> 4   40 3.2
#> 5   50 8.8
#> 6   60 9.9
#> 
#> $y
#> [1] 2014 2015 2016 2014 2015 2016
#> 

### Looking at attributes stored in output
attr(a,"origVars") # Original names corresponding to variables
#>          x          y 
#> "kari_ola"      "aar" 
attr(a,"origCols") # Original names corresponding to columns
#>     x1     x2      y 
#> "kari"  "ola"  "aar" 

### Using a named list to specify equality
GetData(z, x = list("kari",aar=2014), y = list("ola", aar=2015))
#>    x   y
#> 1 10 6.6
#> 2 40 8.8
GetData(z, x = list("kari",aar=2016, tull="B"),  y = list("kari",aar=2014, tull="B"))
#>    x  y
#> 1 30 40

### With matrix input to obtain matrix embedded in output
a = GetData(z, x = list("kari",aar=t(c(2014,2015))), y = list("ola", aar=t(2015:2016)))
print(a);
#>   x.2014 x.2015 y.2015 y.2016
#> 1     10     20    6.6    2.2
#> 2     40     50    8.8    9.9
print(as.list(a)[-99])
#> $x
#>      2014 2015
#> [1,]   10   20
#> [2,]   40   50
#> 
#> $y
#>      2015 2016
#> [1,]  6.6  2.2
#> [2,]  8.8  9.9
#> 
GetData(z, x = list("kari",aar=t(1:3000))) # Impossible values ignored, warning produced
#> Warning: Empty dropped
#>   x.2014 x.2015 x.2016
#> 1     10     20     30
#> 2     40     50     60

### Effect of removeNULL
a = GetData(z, x = NULL, y = "ola")
print(a);
#>     y
#> 1 4.4
#> 2 6.6
#> 3 2.2
#> 4 3.2
#> 5 8.8
#> 6 9.9
print(as.list(a)[-99])
#> $y
#> [1] 4.4 6.6 2.2 3.2 8.8 9.9
#> 
a = GetData(z, x = NULL, y = "ola", removeNULL = FALSE)
print(a);
#>     y
#> 1 4.4
#> 2 6.6
#> 3 2.2
#> 4 3.2
#> 5 8.8
#> 6 9.9
print(as.list(a)[-99])  # x is a 6x0 matrix
#> $x
#>  
#> 1
#> 2
#> 3
#> 4
#> 5
#> 6
#> 
#> $y
#> [1] 4.4 6.6 2.2 3.2 8.8 9.9
#> 

### Using "expression"
GetData(z, x = list("kari",expression(aar>2014)), y = list("kari", expression(tull != "B")))
#>    x  y
#> 1 20 10
#> 2 30 20
#> 3 50 50
#> 4 60 60
GetData(z, x = list("kari",expression(aar>2014 & tull=="B" | tull=="C" )))
#>    x
#> 1 30
#> 2 50
#> 3 60
GetData(z, x = list("kari",expression(aar==min(aar))), y = list("kari", expression(aar==max(aar)-1)))
#>    x  y
#> 1 10 20
#> 2 40 50

### Using names as list elements instead of named list
GetData(z, x = list("kari","aar", "2014"), y = list("ola", "aar", "2015"))
#>    x   y
#> 1 10 6.6
#> 2 40 8.8
GetData(z, x = list("kari","aar", "2016", "tull", "B"))
#>    x
#> 1 30

### Using function to be run on each variable
GetData(z, x = list("kari",aar=2014:2015,function(x)(x+1))) # One function
#>    x
#> 1 11
#> 2 21
#> 3 41
#> 4 51
GetData(z, x = list("kari",aar=2014:2015,function(x)(x+1),function(x)(x*10))) # Tow functions
#>     x
#> 1 110
#> 2 210
#> 3 410
#> 4 510
GetData(z, x = list(c("kari","ola"),function(x)apply(x,1,paste,collapse="-")), y = "aar")
#>        x    y
#> 1 10-4.4 2014
#> 2 20-6.6 2015
#> 3 30-2.2 2016
#> 4 40-3.2 2014
#> 5 50-8.8 2015
#> 6 60-9.9 2016

### Advanced examples
GetData(z, x = list(c("kari","ola"),aar=t(2014:2015)), y = list("ola", aar=2015))
#>   x.kari_2014 x.kari_2015 x.ola_2014 x.ola_2015   y
#> 1        10.0        20.0        4.4        6.6 6.6
#> 2        40.0        50.0        3.2        8.8 8.8
GetData(z, x = list("kari",expression(aar==max(aar)),tull=t(c("B","C"))))
#>   x.B x.C
#> 1  30  60
GetData(z, x = list("kari",expression(eval(as.symbol("aar"))>2014 & eval(as.symbol("tull"))=="B")))
#>    x
#> 1 30
GetData(z, x = list("ola",aar=cbind(2014:2015,2015:2016)))
#>   x.2014_2015 x.2015_2016
#> 1         4.4         6.6
#> 2         6.6         2.2
#> 3         3.2         8.8
#> 4         8.8         9.9
GetData(z, x = list("kari",aar=2014:2015,function(x)(cbind(a=x,b=1000))))
#>   x.a  x.b
#> 1  10 1000
#> 2  20 1000
#> 3  40 1000
#> 4  50 1000
GetData(z, x = list("kari",aar=2014:2015,function(x)(cbind(x=x,tid=date()))))
#>   x.x                    x.tid
#> 1  10 Fri Nov  6 13:46:23 2026
#> 2  20 Fri Nov  6 13:46:23 2026
#> 3  40 Fri Nov  6 13:46:23 2026
#> 4  50 Fri Nov  6 13:46:23 2026

### GetData1
aAa <- GetData1(z, "kari")
bBb <- GetData1(z, x = c("kari","ola"))

### GetData2me
GetData2me(z, cCc = "kari", dDd = "ola")
#> NULL
cCc + dDd
#> [1] 14.4 26.6 32.2 43.2 58.8 69.9
GetData2me(z, eEe = list(c("kari","ola"),aar=t(2014:2015)))
#> NULL
print(eEe)
#>      kari_2014 kari_2015 ola_2014 ola_2015
#> [1,]        10        20      4.4      6.6
#> [2,]        40        50      3.2      8.8

######  Using id  #######

#### Make new example data
z2 <- rbind(z,z)
z2$ola <- c(z$ola,2*z$ola)
z2 <- SortRows(z2)[1:11,]
rownames(z2) <- NULL
z2$ID=c(1:3,4,1:3,5,1:2,6)
print(z2)
#>     aar  ola kari tull ID
#> 1  2014  3.2   40    B  1
#> 2  2014  4.4   10    A  2
#> 3  2014  6.4   40    B  3
#> 4  2014  8.8   10    A  4
#> 5  2015  6.6   20    A  1
#> 6  2015  8.8   50    C  2
#> 7  2015 13.2   20    A  3
#> 8  2015 17.6   50    C  5
#> 9  2016  2.2   30    B  1
#> 10 2016  4.4   30    B  2
#> 11 2016  9.9   60    C  6

# All possible ID-values in data
GetData(z2, iD = list(id="ID"), x = list("kari",aar=2014), y = list("ola", aar=2015))
#>   iD  x    y
#> 1  1 40  6.6
#> 2  2 10  8.8
#> 3  3 40 13.2
#> 4  4 10   NA
#> 5  5 NA 17.6
#> 6  6 NA   NA

# ID-values in union of 2014 and 2015
GetData(z2, iD = list(id="ID",aar=c(2014,2015)), x = list("kari",aar=2014), y = list("ola", aar=2015))
#>   iD  x    y
#> 1  1 40  6.6
#> 2  2 10  8.8
#> 3  3 40 13.2
#> 4  4 10   NA
#> 5  5 NA 17.6

# ID-values in intersection of 2014 and 2015
# (matrix input similar to above but no matrix in output, instead intersection "of columns" created)
GetData(z2, iD = list(id="ID",aar=t(c(2014,2015))), x = list("kari",aar=2014), y = list("ola", aar=2015))
#>   iD  x    y
#> 1  1 40  6.6
#> 2  2 10  8.8
#> 3  3 40 13.2

# Only ID-values in 2016
GetData(z2, iD = list(id="ID",aar=2016), x = list("kari",aar=2014), y = list("ola", aar=2015))
#>   iD  x   y
#> 1  1 40 6.6
#> 2  2 10 8.8
#> 3  6 NA  NA

# ID-values in 2016 +intersection of 2014 and 2015
GetData(z2, iD = list(id="ID",aar=cbind(c(2014,2016),c(2015,2016))), x = list("kari",aar=2014), y = list("ola", aar=2015))
#>   iD  x    y
#> 1  1 40  6.6
#> 2  2 10  8.8
#> 3  3 40 13.2
#> 4  6 NA   NA

# Only first value used hven multiple id
GetData(z2, iD = list(id="ID"), x = "kari", y = "ola")
#>   iD  x    y
#> 1  1 40  3.2
#> 2  2 10  4.4
#> 3  3 40  6.4
#> 4  4 10  8.8
#> 5  5 50 17.6
#> 6  6 60  9.9

# Construct a single id from two variables
GetData(z2, iD = list(id=c("tull", "ID")), x = "kari", y = "ola")
#>     iD  x    y
#> 1  B_1 40  3.2
#> 2  A_2 10  4.4
#> 3  B_3 40  6.4
#> 4  A_4 10  8.8
#> 5  A_1 20  6.6
#> 6  C_2 50  8.8
#> 7  A_3 20 13.2
#> 8  C_5 50 17.6
#> 9  B_2 30  4.4
#> 10 C_6 60  9.9

```
