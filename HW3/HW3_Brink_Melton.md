HW3, Rebecca Brink and Chad Melton
================

    <style type="text/css">
        ol { list-style-type: upper-alpha; }
    </style>

``` r
library(epiR)
```

    ## Loading required package: survival

    ## Package epiR 1.0-15 is loaded

    ## Type help(epi.about) for summary information

    ## Type browseVignettes(package = 'epiR') to learn how to use epiR for applied epidemiological analyses

    ## 

``` r
library(ggplot2)
library(vcdExtra)
```

    ## Loading required package: vcd

    ## Loading required package: grid

    ## Loading required package: gnm

``` r
library(vcd)
library(MASS)
library(lmtest)
```

    ## Loading required package: zoo

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

## 3.13

1)  
<!-- end list -->

``` r
crab = read.csv("C:\\Users\\chadm\\OneDrive\\Documents\\StatsData\\crab.csv",header=TRUE)

crab.model = glm(satell~weight,data=crab,family = poisson(link ="log"))
summary(crab.model)
```

    ## 
    ## Call:
    ## glm(formula = satell ~ weight, family = poisson(link = "log"), 
    ##     data = crab)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.9307  -1.9981  -0.5627   0.9298   4.9992  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -0.42841    0.17893  -2.394   0.0167 *  
    ## weight       0.58930    0.06502   9.064   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for poisson family taken to be 1)
    ## 
    ##     Null deviance: 632.79  on 172  degrees of freedom
    ## Residual deviance: 560.87  on 171  degrees of freedom
    ## AIC: 920.16
    ## 
    ## Number of Fisher Scoring iterations: 5

The equation is: log(muHat) = -0.42841 + 0.58930x

``` r
x=2.44

muHat = exp(-0.42841 + 0.58930*x)
```

The mean Y for female crabs of average weight 2.44 kg is 2.744179.

## Including Code

You can include R code in the document as follows:

``` r
summary(cars)
```

    ##      speed           dist       
    ##  Min.   : 4.0   Min.   :  2.00  
    ##  1st Qu.:12.0   1st Qu.: 26.00  
    ##  Median :15.0   Median : 36.00  
    ##  Mean   :15.4   Mean   : 42.98  
    ##  3rd Qu.:19.0   3rd Qu.: 56.00  
    ##  Max.   :25.0   Max.   :120.00

## Including Plots

You can also embed plots, for example:

![](HW3_Brink_Melton_files/figure-gfm/pressure-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
