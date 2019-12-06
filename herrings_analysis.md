---
title: "Analysis of Herrings Dataset"
output: 
  html_document:
    toc: true
    toc_float: true
    self_contained: false
    keep_md: true
  md_document:
    toc: true
    variant: markdown_github
author: "Piotr Kaszuba & Mateusz Norel"
date: "06 grudnia 2019"
params:
  dataset_name: "sledzie.csv"
  dataset_url: "http://www.cs.put.poznan.pl/alabijak/emd/projekt/sledzie.csv"
  na_chars: "?"
---



## Summary

Following work is an analysis of herrings dataset. Our assignment was to find what causes herrings to became shorter and shorter over time. Analysis contains a survey of attributes, dealing with missing values, inspecting correlations between values, value transformation and normalization. Everything finished with regression and checking for the most important attribute, which turned out to be a `sea surface temperature` attribute.

## Used Packages

Loading libaries:

```r
library(RCurl)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(knitr)
library(kableExtra)
library(mice)
library(VIM)
library(e1071)
library(ggplot2)
library(corrplot)
library(plotly)
library(caret)
library(doMC)
library(xgboost)
```

Listing of libraries loaded during code execution can be found in the cell below:

```
## [1] "minqa, class, rio, rstudioapi, prodlim, lubridate, ranger, xml2, codetools, splines, robustbase, zeallot, jsonlite, nloptr, broom, compiler, httr, backports, assertthat, Matrix, lazyeval, htmltools, tools, gtable, glue, reshape2, Rcpp, carData, cellranger, vctrs, nlme, lmtest, timeDate, xfun, gower, laeken, openxlsx, lme4, rvest, lifecycle, pan, DEoptimR, MASS, zoo, scales, ipred, hms, yaml, curl, rpart, stringi, boot, zip, lava, rlang, pkgconfig, evaluate, purrr, recipes, htmlwidgets, tidyselect, plyr, magrittr, R6, generics, mitml, pillar, haven, foreign, withr, survival, abind, sp, nnet, tibble, crayon, car, jomo, rmarkdown, readxl, forcats, ModelMetrics, vcd, digest, webshot, stats4, munsell, viridisLite"
```
## Loading Data

Data is loded accoring to the metadata provided in the `params` object, where a location of a dataset (both local and url) is provided. Mentioned object contains also information about expected form of missing data. In this case 

```r
if(file.exists(params$dataset_name)) {
  dataset <- read.csv(params$dataset_name, na.strings = params$na_chars)
} else if(url.exists(params$dataset_url)){
  download.file(params$dataset_url, params$dataset_name)
  dataset <- read.csv(params$dataset_name, na.strings = params$na_chars)
} else {
  stop("There is no file nor url resource to work with!")
}
```

## Dataset

To see dimensionality we've used `dim` function:

```r
dim(dataset)
```

```
## [1] 52582    16
```
There are 52582 row and 16 columns.

Here are names of all columns available in a raw dataset:

```r
dataset %>%
  colnames %>%
  print
```

```
##  [1] "X"      "length" "cfin1"  "cfin2"  "chel1"  "chel2"  "lcop1" 
##  [8] "lcop2"  "fbar"   "recr"   "cumf"   "totaln" "sst"    "sal"   
## [15] "xmonth" "nao"
```

First sight at some rows from dataset will help us get better view on the data.

```r
head(dataset) %>%
  kable("html") %>%
  kable_styling(bootstrap_options = c('striped', 'hover')) %>%
  scroll_box(width = '100%')
```

<div style="border: 1px solid #ddd; padding: 5px; overflow-x: scroll; width:100%; "><table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:right;"> X </th>
   <th style="text-align:right;"> length </th>
   <th style="text-align:right;"> cfin1 </th>
   <th style="text-align:right;"> cfin2 </th>
   <th style="text-align:right;"> chel1 </th>
   <th style="text-align:right;"> chel2 </th>
   <th style="text-align:right;"> lcop1 </th>
   <th style="text-align:right;"> lcop2 </th>
   <th style="text-align:right;"> fbar </th>
   <th style="text-align:right;"> recr </th>
   <th style="text-align:right;"> cumf </th>
   <th style="text-align:right;"> totaln </th>
   <th style="text-align:right;"> sst </th>
   <th style="text-align:right;"> sal </th>
   <th style="text-align:right;"> xmonth </th>
   <th style="text-align:right;"> nao </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 23.0 </td>
   <td style="text-align:right;"> 0.02778 </td>
   <td style="text-align:right;"> 0.27785 </td>
   <td style="text-align:right;"> 2.46875 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 2.54787 </td>
   <td style="text-align:right;"> 26.35881 </td>
   <td style="text-align:right;"> 0.356 </td>
   <td style="text-align:right;"> 482831 </td>
   <td style="text-align:right;"> 0.3059879 </td>
   <td style="text-align:right;"> 267380.8 </td>
   <td style="text-align:right;"> 14.30693 </td>
   <td style="text-align:right;"> 35.51234 </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 2.8 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 22.5 </td>
   <td style="text-align:right;"> 0.02778 </td>
   <td style="text-align:right;"> 0.27785 </td>
   <td style="text-align:right;"> 2.46875 </td>
   <td style="text-align:right;"> 21.43548 </td>
   <td style="text-align:right;"> 2.54787 </td>
   <td style="text-align:right;"> 26.35881 </td>
   <td style="text-align:right;"> 0.356 </td>
   <td style="text-align:right;"> 482831 </td>
   <td style="text-align:right;"> 0.3059879 </td>
   <td style="text-align:right;"> 267380.8 </td>
   <td style="text-align:right;"> 14.30693 </td>
   <td style="text-align:right;"> 35.51234 </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 2.8 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 25.0 </td>
   <td style="text-align:right;"> 0.02778 </td>
   <td style="text-align:right;"> 0.27785 </td>
   <td style="text-align:right;"> 2.46875 </td>
   <td style="text-align:right;"> 21.43548 </td>
   <td style="text-align:right;"> 2.54787 </td>
   <td style="text-align:right;"> 26.35881 </td>
   <td style="text-align:right;"> 0.356 </td>
   <td style="text-align:right;"> 482831 </td>
   <td style="text-align:right;"> 0.3059879 </td>
   <td style="text-align:right;"> 267380.8 </td>
   <td style="text-align:right;"> 14.30693 </td>
   <td style="text-align:right;"> 35.51234 </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 2.8 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 25.5 </td>
   <td style="text-align:right;"> 0.02778 </td>
   <td style="text-align:right;"> 0.27785 </td>
   <td style="text-align:right;"> 2.46875 </td>
   <td style="text-align:right;"> 21.43548 </td>
   <td style="text-align:right;"> 2.54787 </td>
   <td style="text-align:right;"> 26.35881 </td>
   <td style="text-align:right;"> 0.356 </td>
   <td style="text-align:right;"> 482831 </td>
   <td style="text-align:right;"> 0.3059879 </td>
   <td style="text-align:right;"> 267380.8 </td>
   <td style="text-align:right;"> 14.30693 </td>
   <td style="text-align:right;"> 35.51234 </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 2.8 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 24.0 </td>
   <td style="text-align:right;"> 0.02778 </td>
   <td style="text-align:right;"> 0.27785 </td>
   <td style="text-align:right;"> 2.46875 </td>
   <td style="text-align:right;"> 21.43548 </td>
   <td style="text-align:right;"> 2.54787 </td>
   <td style="text-align:right;"> 26.35881 </td>
   <td style="text-align:right;"> 0.356 </td>
   <td style="text-align:right;"> 482831 </td>
   <td style="text-align:right;"> 0.3059879 </td>
   <td style="text-align:right;"> 267380.8 </td>
   <td style="text-align:right;"> 14.30693 </td>
   <td style="text-align:right;"> 35.51234 </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 2.8 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 22.0 </td>
   <td style="text-align:right;"> 0.02778 </td>
   <td style="text-align:right;"> 0.27785 </td>
   <td style="text-align:right;"> 2.46875 </td>
   <td style="text-align:right;"> 21.43548 </td>
   <td style="text-align:right;"> 2.54787 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 0.356 </td>
   <td style="text-align:right;"> 482831 </td>
   <td style="text-align:right;"> 0.3059879 </td>
   <td style="text-align:right;"> 267380.8 </td>
   <td style="text-align:right;"> 14.30693 </td>
   <td style="text-align:right;"> 35.51234 </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 2.8 </td>
  </tr>
</tbody>
</table></div>
We can see that all available data is numerical. There are some `NA` values. Column `xmonth` should be changed to dummy values before regression.

Names of some columns may be ambiguous. This is their brief explanation:

```
## Dataset contains following attributes:
## - length - length of the fished herring [cm]
## - cfin1 - density of Calanus finmarchicus kat. 1 plankton
## - cfin2 - density of Calanus finmarchicus kat. 2 plankton
## - chel1 - density of Calanus helgolandicus kat. 1 plankton
## - chel2 - density of Calanus helgolandicus kat. 2 plankton
## - lcop1 - density of copepods kat. 1
## - lcop2 - density of copepods kat. 2
## - fbar - intensity of fishing in region [remaining part of herring fry]
## - recr - annual number of herrings
## - cumf - intensity of annual fishing in region [remaining part of herring fry]
## - totaln - number of fished herrings
## - sst - sea surface temperature [Celsius]
## - sal - salinity [Kundsen ppt]
## - xmonth - number of fishing month
## - nao - north atlantic oscillation [mean sea level pressure]
```

After that step, dataset could be transferred to dyplr's `DataFrame` object.
By selecting just columns with `numeric` type and then checking if all column names match those from the raw set, we were able to make sure that all columns are ready for further work.

```r
herrings <- tbl_df(dataset)
herrings %>% 
  select_if(is.numeric) %>%
  colnames
```

```
##  [1] "X"      "length" "cfin1"  "cfin2"  "chel1"  "chel2"  "lcop1" 
##  [8] "lcop2"  "fbar"   "recr"   "cumf"   "totaln" "sst"    "sal"   
## [15] "xmonth" "nao"
```

It was also required to transform `xmonth` column to cathegorical, as its values are numbers of months.

```r
herrings <- herrings %>%
  mutate(xmonth = as.factor(xmonth))
```

## Missing Values
All columns were extracted correctly and do not contain anything different to numeric variables. But there are still missing values (`NA`s) in the dataframe. Before deciding what to do with them it is crucial to know how many rows have `NA` value.

We started with showing the list of columns that have at least one `NA` value.

```r
cols_with_na = c()
for(col in colnames(herrings)){
  if(any(is.na(herrings[col]))){
    cols_with_na <- cols_with_na %>% append(col)
  }
}
cols_with_na
```

```
## [1] "cfin1" "cfin2" "chel1" "chel2" "lcop1" "lcop2" "sst"
```
There are 7 such columns in the herrings dataset. 

Before making any decision about missing data it is important to visualize it. It is recommended to not reconstruct variable if more than 5% of the data is not available. Also it is worth knowing missing data pattern. If it's random it is good idea to try some imputation methods. But if it seems that there is some reason for data being not available it's better to consider other methods (profound analysis of data gathering process is a reasonable way to start).

Following plots contains interesting information about missing data. Their were done using VIM's `aggr` function on dataset with just those columns which contain at least one `NA` value. Left figure, "Histogram of missing data" informs us how many values of each column are missing. We can see that it is around 3% of all values for each column are missing. It is less than mentioned 5% so data reconstruction is worth considering. More than that, as all columns have around 3% missing data it is a first hint that not availability might be caused by some random process. The right-side chart, "Missing data pattern", shows what is the pattern of missing data. All combinations of missing data were captured from the dataset and presented. Red color symbolizes `NA`, while blue indicates that correct value is available. Most common combinations are at the very bottom. Moving to the top there are less and less frequent combinations. 

It seems that there are no strong relations between missing values. Next step is an imputation.

```r
just_with_na <- herrings %>% select(cols_with_na)
aggr_plot <- aggr(just_with_na, col=c('navyblue','red'), numbers=FALSE, bars=TRUE, sortVars=TRUE, labels=names(just_with_na), cex.axis=.8, gap=3, ylab=c("Histogram of missing data","Missing data pattern"))
```

![](herrings_analysis_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```
## 
##  Variables sorted by number of missings: 
##  Variable      Count
##     lcop1 0.03143661
##     lcop2 0.03025750
##       sst 0.03012438
##     cfin1 0.03006732
##     chel2 0.02959188
##     chel1 0.02957286
##     cfin2 0.02921152
```

## Imputing Missing Data

In order to reconstruct missing data we've used a `mice` library (Multivariate Imputation By Chained Equations). Seed for a random generator was set to 17. MICE tries to imput values in places of missing data based on the context from other attributes. One of attributes, `sal`, had to be excluded from this step as its participation resulted in error. MICE supports many methods from which it is might be a good idea to choose by first trying its reconstruction results on smaller subsequence of data and compare that to ground truth. We omitted that step and went straight to fairly good `pmm` imputation method. Its name stands for "Predictive Mean Matching.

```r
set.seed(17)
imputed <- mice(herrings,predictorMatrix = quickpred(herrings, exclude = c('sal')), meth='pmm', maxit=1)
```

```
## 
##  iter imp variable
##   1   1  cfin1  cfin2  chel1  chel2  lcop1  lcop2  sst
##   1   2  cfin1  cfin2  chel1  chel2  lcop1  lcop2  sst
##   1   3  cfin1  cfin2  chel1  chel2  lcop1  lcop2  sst
##   1   4  cfin1  cfin2  chel1  chel2  lcop1  lcop2  sst
##   1   5  cfin1  cfin2  chel1  chel2  lcop1  lcop2  sst
```

```r
tmp_data <- complete(imputed,1)
imputed_data <- mutate(tmp_data, sal = herrings$sal)
```

## Attribute-wise Analysis

We started studying attributes by showing their summary which contains basic statistical metrics. There are no missing values as we've got rid of them earlier.

```r
summary(imputed_data) %>%
  kable("html") %>%
  kable_styling(bootstrap_options = c('striped', 'hover')) %>%
  scroll_box(width = '100%')
```

<div style="border: 1px solid #ddd; padding: 5px; overflow-x: scroll; width:100%; "><table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:left;">       X </th>
   <th style="text-align:left;">     length </th>
   <th style="text-align:left;">     cfin1 </th>
   <th style="text-align:left;">     cfin2 </th>
   <th style="text-align:left;">     chel1 </th>
   <th style="text-align:left;">     chel2 </th>
   <th style="text-align:left;">     lcop1 </th>
   <th style="text-align:left;">     lcop2 </th>
   <th style="text-align:left;">      fbar </th>
   <th style="text-align:left;">      recr </th>
   <th style="text-align:left;">      cumf </th>
   <th style="text-align:left;">     totaln </th>
   <th style="text-align:left;">      sst </th>
   <th style="text-align:left;">      sal </th>
   <th style="text-align:left;">     xmonth </th>
   <th style="text-align:left;">      nao </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Min.   :    0 </td>
   <td style="text-align:left;"> Min.   :19.0 </td>
   <td style="text-align:left;"> Min.   : 0.0000 </td>
   <td style="text-align:left;"> Min.   : 0.0000 </td>
   <td style="text-align:left;"> Min.   : 0.000 </td>
   <td style="text-align:left;"> Min.   : 5.238 </td>
   <td style="text-align:left;"> Min.   :  0.3074 </td>
   <td style="text-align:left;"> Min.   : 7.849 </td>
   <td style="text-align:left;"> Min.   :0.0680 </td>
   <td style="text-align:left;"> Min.   : 140515 </td>
   <td style="text-align:left;"> Min.   :0.06833 </td>
   <td style="text-align:left;"> Min.   : 144137 </td>
   <td style="text-align:left;"> Min.   :12.77 </td>
   <td style="text-align:left;"> Min.   :35.40 </td>
   <td style="text-align:left;"> 8      : 9920 </td>
   <td style="text-align:left;"> Min.   :-4.89000 </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 1st Qu.:13145 </td>
   <td style="text-align:left;"> 1st Qu.:24.0 </td>
   <td style="text-align:left;"> 1st Qu.: 0.0000 </td>
   <td style="text-align:left;"> 1st Qu.: 0.2778 </td>
   <td style="text-align:left;"> 1st Qu.: 2.469 </td>
   <td style="text-align:left;"> 1st Qu.:13.427 </td>
   <td style="text-align:left;"> 1st Qu.:  2.5479 </td>
   <td style="text-align:left;"> 1st Qu.:17.808 </td>
   <td style="text-align:left;"> 1st Qu.:0.2270 </td>
   <td style="text-align:left;"> 1st Qu.: 360061 </td>
   <td style="text-align:left;"> 1st Qu.:0.14809 </td>
   <td style="text-align:left;"> 1st Qu.: 306068 </td>
   <td style="text-align:left;"> 1st Qu.:13.60 </td>
   <td style="text-align:left;"> 1st Qu.:35.51 </td>
   <td style="text-align:left;"> 10     : 7972 </td>
   <td style="text-align:left;"> 1st Qu.:-1.89000 </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Median :26290 </td>
   <td style="text-align:left;"> Median :25.5 </td>
   <td style="text-align:left;"> Median : 0.1111 </td>
   <td style="text-align:left;"> Median : 0.7012 </td>
   <td style="text-align:left;"> Median : 5.750 </td>
   <td style="text-align:left;"> Median :21.435 </td>
   <td style="text-align:left;"> Median :  7.0000 </td>
   <td style="text-align:left;"> Median :24.859 </td>
   <td style="text-align:left;"> Median :0.3320 </td>
   <td style="text-align:left;"> Median : 421391 </td>
   <td style="text-align:left;"> Median :0.23191 </td>
   <td style="text-align:left;"> Median : 539558 </td>
   <td style="text-align:left;"> Median :13.86 </td>
   <td style="text-align:left;"> Median :35.51 </td>
   <td style="text-align:left;"> 7      : 6922 </td>
   <td style="text-align:left;"> Median : 0.20000 </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Mean   :26290 </td>
   <td style="text-align:left;"> Mean   :25.3 </td>
   <td style="text-align:left;"> Mean   : 0.4457 </td>
   <td style="text-align:left;"> Mean   : 2.0228 </td>
   <td style="text-align:left;"> Mean   :10.003 </td>
   <td style="text-align:left;"> Mean   :21.204 </td>
   <td style="text-align:left;"> Mean   : 12.8051 </td>
   <td style="text-align:left;"> Mean   :28.422 </td>
   <td style="text-align:left;"> Mean   :0.3304 </td>
   <td style="text-align:left;"> Mean   : 520366 </td>
   <td style="text-align:left;"> Mean   :0.22981 </td>
   <td style="text-align:left;"> Mean   : 514973 </td>
   <td style="text-align:left;"> Mean   :13.87 </td>
   <td style="text-align:left;"> Mean   :35.51 </td>
   <td style="text-align:left;"> 9      : 5714 </td>
   <td style="text-align:left;"> Mean   :-0.09236 </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> 3rd Qu.:39436 </td>
   <td style="text-align:left;"> 3rd Qu.:26.5 </td>
   <td style="text-align:left;"> 3rd Qu.: 0.3333 </td>
   <td style="text-align:left;"> 3rd Qu.: 1.7936 </td>
   <td style="text-align:left;"> 3rd Qu.:11.500 </td>
   <td style="text-align:left;"> 3rd Qu.:27.193 </td>
   <td style="text-align:left;"> 3rd Qu.: 21.2315 </td>
   <td style="text-align:left;"> 3rd Qu.:37.232 </td>
   <td style="text-align:left;"> 3rd Qu.:0.4560 </td>
   <td style="text-align:left;"> 3rd Qu.: 724151 </td>
   <td style="text-align:left;"> 3rd Qu.:0.29803 </td>
   <td style="text-align:left;"> 3rd Qu.: 730351 </td>
   <td style="text-align:left;"> 3rd Qu.:14.16 </td>
   <td style="text-align:left;"> 3rd Qu.:35.52 </td>
   <td style="text-align:left;"> 6      : 4218 </td>
   <td style="text-align:left;"> 3rd Qu.: 1.63000 </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Max.   :52581 </td>
   <td style="text-align:left;"> Max.   :32.5 </td>
   <td style="text-align:left;"> Max.   :37.6667 </td>
   <td style="text-align:left;"> Max.   :19.3958 </td>
   <td style="text-align:left;"> Max.   :75.000 </td>
   <td style="text-align:left;"> Max.   :57.706 </td>
   <td style="text-align:left;"> Max.   :115.5833 </td>
   <td style="text-align:left;"> Max.   :68.736 </td>
   <td style="text-align:left;"> Max.   :0.8490 </td>
   <td style="text-align:left;"> Max.   :1565890 </td>
   <td style="text-align:left;"> Max.   :0.39801 </td>
   <td style="text-align:left;"> Max.   :1015595 </td>
   <td style="text-align:left;"> Max.   :14.73 </td>
   <td style="text-align:left;"> Max.   :35.61 </td>
   <td style="text-align:left;"> 5      : 3736 </td>
   <td style="text-align:left;"> Max.   : 5.08000 </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> (Other):14100 </td>
   <td style="text-align:left;"> NA </td>
  </tr>
</tbody>
</table></div>

Figure below presents a correlation matrix (or rather its upper part). Attribute `length` has fairly strong negative correlation with sea surface temperature, but besides that it is fairly weakly connected with other attributes. See surface temperature is connected to North Atlantic oscillation. When NAO phase is positive when the difference between air pressure in Azores and Iceland is higher than usual. This difference causes westerlies (steady winds blowing from the west) to change their path towards north-east. Because of that temperature of air and water around Icelandic Low starts to rise.

```r
cor_data <- select(imputed_data, -c('xmonth', 'X'))
corrplot(cor(cor_data), type='upper', order='hclust', tl.col='black', tl.srt=55, title="Correlation matrix")
```

![](herrings_analysis_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

The chart below presents how a leanth of herrings was changing over time. Blue line is an effect of smooth function. It allows us to clearly see trend present in data. There is noticeable breakpoint. It is worth noting that `X` variable which occupies x axis in following figure is not equivalent to time. Values of `X` are the indices of hronologically sorted events, but time between measuruments was not specified and therefore we cannot preceive that as time series. This can be seen as additional noise in data.
<!--html_preserve--><div id="htmlwidget-eede0cf33cd151f8ed8b" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-eede0cf33cd151f8ed8b">{"x":{"data":[{"x":[0,50,100,150,200,250,300,350,400,450,500,550,600,650,700,750,800,850,900,950,1000,1050,1100,1150,1200,1250,1300,1350,1400,1450,1500,1550,1600,1650,1700,1750,1800,1850,1900,1950,2000,2050,2100,2150,2200,2250,2300,2350,2400,2450,2500,2550,2600,2650,2700,2750,2800,2850,2900,2950,3000,3050,3100,3150,3200,3250,3300,3350,3400,3450,3500,3550,3600,3650,3700,3750,3800,3850,3900,3950,4000,4050,4100,4150,4200,4250,4300,4350,4400,4450,4500,4550,4600,4650,4700,4750,4800,4850,4900,4950,5000,5050,5100,5150,5200,5250,5300,5350,5400,5450,5500,5550,5600,5650,5700,5750,5800,5850,5900,5950,6000,6050,6100,6150,6200,6250,6300,6350,6400,6450,6500,6550,6600,6650,6700,6750,6800,6850,6900,6950,7000,7050,7100,7150,7200,7250,7300,7350,7400,7450,7500,7550,7600,7650,7700,7750,7800,7850,7900,7950,8000,8050,8100,8150,8200,8250,8300,8350,8400,8450,8500,8550,8600,8650,8700,8750,8800,8850,8900,8950,9000,9050,9100,9150,9200,9250,9300,9350,9400,9450,9500,9550,9600,9650,9700,9750,9800,9850,9900,9950,10000,10050,10100,10150,10200,10250,10300,10350,10400,10450,10500,10550,10600,10650,10700,10750,10800,10850,10900,10950,11000,11050,11100,11150,11200,11250,11300,11350,11400,11450,11500,11550,11600,11650,11700,11750,11800,11850,11900,11950,12000,12050,12100,12150,12200,12250,12300,12350,12400,12450,12500,12550,12600,12650,12700,12750,12800,12850,12900,12950,13000,13050,13100,13150,13200,13250,13300,13350,13400,13450,13500,13550,13600,13650,13700,13750,13800,13850,13900,13950,14000,14050,14100,14150,14200,14250,14300,14350,14400,14450,14500,14550,14600,14650,14700,14750,14800,14850,14900,14950,15000,15050,15100,15150,15200,15250,15300,15350,15400,15450,15500,15550,15600,15650,15700,15750,15800,15850,15900,15950,16000,16050,16100,16150,16200,16250,16300,16350,16400,16450,16500,16550,16600,16650,16700,16750,16800,16850,16900,16950,17000,17050,17100,17150,17200,17250,17300,17350,17400,17450,17500,17550,17600,17650,17700,17750,17800,17850,17900,17950,18000,18050,18100,18150,18200,18250,18300,18350,18400,18450,18500,18550,18600,18650,18700,18750,18800,18850,18900,18950,19000,19050,19100,19150,19200,19250,19300,19350,19400,19450,19500,19550,19600,19650,19700,19750,19800,19850,19900,19950,20000,20050,20100,20150,20200,20250,20300,20350,20400,20450,20500,20550,20600,20650,20700,20750,20800,20850,20900,20950,21000,21050,21100,21150,21200,21250,21300,21350,21400,21450,21500,21550,21600,21650,21700,21750,21800,21850,21900,21950,22000,22050,22100,22150,22200,22250,22300,22350,22400,22450,22500,22550,22600,22650,22700,22750,22800,22850,22900,22950,23000,23050,23100,23150,23200,23250,23300,23350,23400,23450,23500,23550,23600,23650,23700,23750,23800,23850,23900,23950,24000,24050,24100,24150,24200,24250,24300,24350,24400,24450,24500,24550,24600,24650,24700,24750,24800,24850,24900,24950,25000,25050,25100,25150,25200,25250,25300,25350,25400,25450,25500,25550,25600,25650,25700,25750,25800,25850,25900,25950,26000,26050,26100,26150,26200,26250,26300,26350,26400,26450,26500,26550,26600,26650,26700,26750,26800,26850,26900,26950,27000,27050,27100,27150,27200,27250,27300,27350,27400,27450,27500,27550,27600,27650,27700,27750,27800,27850,27900,27950,28000,28050,28100,28150,28200,28250,28300,28350,28400,28450,28500,28550,28600,28650,28700,28750,28800,28850,28900,28950,29000,29050,29100,29150,29200,29250,29300,29350,29400,29450,29500,29550,29600,29650,29700,29750,29800,29850,29900,29950,30000,30050,30100,30150,30200,30250,30300,30350,30400,30450,30500,30550,30600,30650,30700,30750,30800,30850,30900,30950,31000,31050,31100,31150,31200,31250,31300,31350,31400,31450,31500,31550,31600,31650,31700,31750,31800,31850,31900,31950,32000,32050,32100,32150,32200,32250,32300,32350,32400,32450,32500,32550,32600,32650,32700,32750,32800,32850,32900,32950,33000,33050,33100,33150,33200,33250,33300,33350,33400,33450,33500,33550,33600,33650,33700,33750,33800,33850,33900,33950,34000,34050,34100,34150,34200,34250,34300,34350,34400,34450,34500,34550,34600,34650,34700,34750,34800,34850,34900,34950,35000,35050,35100,35150,35200,35250,35300,35350,35400,35450,35500,35550,35600,35650,35700,35750,35800,35850,35900,35950,36000,36050,36100,36150,36200,36250,36300,36350,36400,36450,36500,36550,36600,36650,36700,36750,36800,36850,36900,36950,37000,37050,37100,37150,37200,37250,37300,37350,37400,37450,37500,37550,37600,37650,37700,37750,37800,37850,37900,37950,38000,38050,38100,38150,38200,38250,38300,38350,38400,38450,38500,38550,38600,38650,38700,38750,38800,38850,38900,38950,39000,39050,39100,39150,39200,39250,39300,39350,39400,39450,39500,39550,39600,39650,39700,39750,39800,39850,39900,39950,40000,40050,40100,40150,40200,40250,40300,40350,40400,40450,40500,40550,40600,40650,40700,40750,40800,40850,40900,40950,41000,41050,41100,41150,41200,41250,41300,41350,41400,41450,41500,41550,41600,41650,41700,41750,41800,41850,41900,41950,42000,42050,42100,42150,42200,42250,42300,42350,42400,42450,42500,42550,42600,42650,42700,42750,42800,42850,42900,42950,43000,43050,43100,43150,43200,43250,43300,43350,43400,43450,43500,43550,43600,43650,43700,43750,43800,43850,43900,43950,44000,44050,44100,44150,44200,44250,44300,44350,44400,44450,44500,44550,44600,44650,44700,44750,44800,44850,44900,44950,45000,45050,45100,45150,45200,45250,45300,45350,45400,45450,45500,45550,45600,45650,45700,45750,45800,45850,45900,45950,46000,46050,46100,46150,46200,46250,46300,46350,46400,46450,46500,46550,46600,46650,46700,46750,46800,46850,46900,46950,47000,47050,47100,47150,47200,47250,47300,47350,47400,47450,47500,47550,47600,47650,47700,47750,47800,47850,47900,47950,48000,48050,48100,48150,48200,48250,48300,48350,48400,48450,48500,48550,48600,48650,48700,48750,48800,48850,48900,48950,49000,49050,49100,49150,49200,49250,49300,49350,49400,49450,49500,49550,49600,49650,49700,49750,49800,49850,49900,49950,50000,50050,50100,50150,50200,50250,50300,50350,50400,50450,50500,50550,50600,50650,50700,50750,50800,50850,50900,50950,51000,51050,51100,51150,51200,51250,51300,51350,51400,51450,51500,51550,51600,51650,51700,51750,51800,51850,51900,51950,52000,52050,52100,52150,52200,52250,52300,52350,52400,52450,52500,52550],"y":[23,21.5,22.5,22.5,21,21,23,24,26,22,26.5,22.5,23.5,21.5,24,25.5,25,25,24.5,27.5,26.5,26.5,25.5,26.5,26,22.5,25,27,26.5,25.5,25,27,25.5,25,25,25,25,24.5,26,24,26,21.5,26.5,25.5,25.5,25.5,25.5,23,22,27.5,26,25.5,25,26,27,23,25.5,25,24.5,24.5,24.5,23,25,22.5,20,21,23.5,25.5,26.5,21.5,24.5,25,24.5,27,21,23,22.5,25.5,25.5,26,25,28,25.5,26,24.5,26,24.5,26.5,25,26,23.5,23,25,25.5,24.5,26,26,26.5,27,26.5,27.5,26.5,24,26,26.5,22.5,25.5,24.5,26,25.5,24,26.5,25,24,23,26.5,26,27,23.5,25,24.5,24.5,25,24.5,26,27,25,27,27.5,24.5,26,27,21.5,24,26.5,26.5,25.5,25.5,24,26,23.5,26.5,25.5,24,25.5,27,25.5,24.5,25,26,25,25,28,26,27,25,22.5,26.5,26.5,26.5,26.5,26,25.5,26.5,23,25.5,24.5,27.5,26.5,24,25,26,26.5,28,25.5,26.5,27,26.5,26.5,21.5,24.5,25,27.5,27,26,24,27,25.5,24,23,24,25.5,23,26.5,27,26.5,27,25.5,27.5,25.5,24,25,28,25,27,25,25.5,26.5,26,25,27,25.5,25,25.5,26,26.5,25.5,27,24.5,26.5,26.5,29.5,25,26.5,27,26.5,25,27.5,28,26,28,27,27,26,27.5,27.5,25,27,26.5,25,26,25,27,27.5,25,27.5,28,24.5,23.5,27,28,25.5,26.5,27.5,25,24.5,25.5,28.5,26,26.5,25.5,24,28,25.5,26,26.5,26.5,26,25,25,26,26.5,25.5,26,26,25,26,26,25.5,26.5,27.5,28.5,25.5,28,26,27.5,27,27,26.5,28,26.5,27.5,24.5,25.5,28,26.5,27.5,26.5,27.5,27,27.5,25.5,26.5,26,24.5,26,25.5,26,26,27,26.5,26.5,26.5,28,26,24.5,27,26.5,21.5,26.5,28,26.5,26.5,27.5,27,26.5,27,27.5,29,29.5,26.5,28.5,26,27,28.5,27,26.5,27.5,28.5,28,27,26.5,25.5,26,27,28,28,26,27.5,29,26.5,27.5,27,28,29,27,26.5,28.5,24.5,26.5,25,26,29.5,26.5,26.5,27.5,28,25.5,26,25.5,26.5,27.5,26,26,27.5,27,27.5,29.8,26,28,28,28.5,29,28,26.5,27.5,27,27,27,27,26,25.5,26,27.5,26,25,25.5,25.5,24.5,24.5,26,26,28,28.5,26,25.5,24,26.5,26,26,26.5,25,27,26.5,25,24.5,26.5,25.5,26,27,25.5,25.5,26.5,27,24,25,28,29,24.5,25,25.5,25.5,30.5,23.5,26,26.5,28.5,24,25,23,23,24.5,24.5,27,25.5,23,23,24.5,26,25,24.5,23.5,22,24.5,24,24,27.5,21.5,26.5,25.5,27,28.5,27.5,25,27.5,26.5,26.5,25.5,27,26.5,27,26.5,27,26.5,27.5,27,25.5,28.5,27,27,28,23.5,27,25,26.5,24.5,25,25.5,26,27.5,25.5,25.5,28.5,23.5,25,25.5,24.5,23,25,23.5,28,26.5,27.5,25,25,23,25,29,26.5,26.5,25,25.5,27,27,23.5,28.5,25.5,26.5,27.5,30.5,27,27.5,26.5,26.5,26.5,28.5,27,24.5,28,22.5,23.5,26.5,25.5,21,24.5,25,26,25,25,22,23,22.5,24,23.5,27,28.5,23,22,22,23.5,27,26,23.5,24.5,26.5,28,28,25,25,28,28.5,27,22.5,25,22.5,23,25.5,26.5,27,27,25,24.5,27.5,26.5,25.5,25.5,26,25,26,27,27.5,26.5,28,25,26.5,26,25,27,25.5,25.5,26.5,27,26,23.5,27,22.5,26.5,28,28,25,26.5,26,25.5,26,25.5,26,25,26.5,26,23.5,26,25.5,28,26,24.5,24,23.5,26,26,24.5,26,25,25.5,24,27,24,26.5,25.5,24.5,27,26.5,26,28,25,25.5,25,26.5,26,23.5,23,25,25.5,27,25,24,24.5,28,28,23.5,28.5,26.5,27,22.5,25.5,26,25.5,25.5,25,25,27.5,25,24,25,24.5,25,26,24.5,26,25,25,23.5,23,23,27,23,24,26.5,24,24.5,25.5,26,28,28.5,25,27.5,26,26,24.5,22.5,22.5,26.5,24.5,22,25.5,24.5,25,24.5,25,25,24,24,27,24.5,26.5,25,26,25.5,25,25,26.5,24.5,24.5,24,26.5,24,23.5,24.5,25,24,23,25.5,24.5,25.5,24.5,26.5,25.5,24.5,25.5,23,25.5,26.5,26,26,26.5,24.5,24.5,26.5,25.5,24,24,25,26,25,26,25,25,25,24.5,27.5,26.5,25.5,25.5,24.5,27.5,24,28.5,24.5,28,26,25.5,26.5,25,26,26.5,22,26,24,25.5,25,24.5,26.5,23.5,23,24,22.5,23,23.5,23.5,24.5,24.5,24,25.5,26.5,24,25.5,22.5,25.5,25,25.5,24,26.5,24.5,24.5,25.5,26,24,23,22.5,22.5,25,23.5,24.5,23.5,24,25,24.5,26,23.5,25.5,27,24.5,25,25,24.5,25.5,25,26,25.5,27,25,25.5,21,24,24,26,24,24,25,22.5,25,25.5,25.5,26,23,23.5,25,24,24.5,24,24.5,25,27.5,24.5,23,24.5,24.5,24.5,25,25,25,23,25.5,24.5,22,22,26,24,24,24,25.5,24.5,24.5,25.5,24,25.5,25.5,23.5,26.5,25.5,24.5,23.5,23.5,24.5,26,24,23,25,26,23.5,26,27,22.5,23.5,22.5,24.5,21.5,22,24,23.5,24.5,26,22.5,25.5,24,23,23,23.5,24,27,25.5,26,26,25.5,25,25,23.5,21.5,24.5,22.5,24,26.5,23,23,25.5,24.5,26.5,24.5,24,23,24.5,24,21,22.5,24.5,21.5,24,26,24.5,24.5,23.5,25,23.5,23,24,25,23,23,25,21.5,24.5,26,23,27.5,23,23.5,22.5,25,21,22,25,24.5,24,25,24,23.5,23,23,24.5,23.5,25.5,25.5,21.5,23,23,24.5,21,23.5,25.5,24.5,22,23.5,27,24,24,25.5,23.5,23.5,24.5,26,25.5,24.5,27,22,25.5,26,23,25.5,24.5,23.5,24,23,22.5,23,25.5,25,25,25.5,23.5,25.5,24.5,24.5,23.5,23.5,24,24,24,23,22,23.5,23,25,23,22.5,23.5,22.5,23,25,24.5,22.5,24,22.5,22,23.5,21,23,25,22.5,23,22,24,25,24,25,22,23,24,27,22.5,25.5,24.5,23,24,23.5,23,26,24.5,26,26,24.5,27,25.5,22],"text":["X:     0<br />length: 23.0","X:    50<br />length: 21.5","X:   100<br />length: 22.5","X:   150<br />length: 22.5","X:   200<br />length: 21.0","X:   250<br />length: 21.0","X:   300<br />length: 23.0","X:   350<br />length: 24.0","X:   400<br />length: 26.0","X:   450<br />length: 22.0","X:   500<br />length: 26.5","X:   550<br />length: 22.5","X:   600<br />length: 23.5","X:   650<br />length: 21.5","X:   700<br />length: 24.0","X:   750<br />length: 25.5","X:   800<br />length: 25.0","X:   850<br />length: 25.0","X:   900<br />length: 24.5","X:   950<br />length: 27.5","X:  1000<br />length: 26.5","X:  1050<br />length: 26.5","X:  1100<br />length: 25.5","X:  1150<br />length: 26.5","X:  1200<br />length: 26.0","X:  1250<br />length: 22.5","X:  1300<br />length: 25.0","X:  1350<br />length: 27.0","X:  1400<br />length: 26.5","X:  1450<br />length: 25.5","X:  1500<br />length: 25.0","X:  1550<br />length: 27.0","X:  1600<br />length: 25.5","X:  1650<br />length: 25.0","X:  1700<br />length: 25.0","X:  1750<br />length: 25.0","X:  1800<br />length: 25.0","X:  1850<br />length: 24.5","X:  1900<br />length: 26.0","X:  1950<br />length: 24.0","X:  2000<br />length: 26.0","X:  2050<br />length: 21.5","X:  2100<br />length: 26.5","X:  2150<br />length: 25.5","X:  2200<br />length: 25.5","X:  2250<br />length: 25.5","X:  2300<br />length: 25.5","X:  2350<br />length: 23.0","X:  2400<br />length: 22.0","X:  2450<br />length: 27.5","X:  2500<br />length: 26.0","X:  2550<br />length: 25.5","X:  2600<br />length: 25.0","X:  2650<br />length: 26.0","X:  2700<br />length: 27.0","X:  2750<br />length: 23.0","X:  2800<br />length: 25.5","X:  2850<br />length: 25.0","X:  2900<br />length: 24.5","X:  2950<br />length: 24.5","X:  3000<br />length: 24.5","X:  3050<br />length: 23.0","X:  3100<br />length: 25.0","X:  3150<br />length: 22.5","X:  3200<br />length: 20.0","X:  3250<br />length: 21.0","X:  3300<br />length: 23.5","X:  3350<br />length: 25.5","X:  3400<br />length: 26.5","X:  3450<br />length: 21.5","X:  3500<br />length: 24.5","X:  3550<br />length: 25.0","X:  3600<br />length: 24.5","X:  3650<br />length: 27.0","X:  3700<br />length: 21.0","X:  3750<br />length: 23.0","X:  3800<br />length: 22.5","X:  3850<br />length: 25.5","X:  3900<br />length: 25.5","X:  3950<br />length: 26.0","X:  4000<br />length: 25.0","X:  4050<br />length: 28.0","X:  4100<br />length: 25.5","X:  4150<br />length: 26.0","X:  4200<br />length: 24.5","X:  4250<br />length: 26.0","X:  4300<br />length: 24.5","X:  4350<br />length: 26.5","X:  4400<br />length: 25.0","X:  4450<br />length: 26.0","X:  4500<br />length: 23.5","X:  4550<br />length: 23.0","X:  4600<br />length: 25.0","X:  4650<br />length: 25.5","X:  4700<br />length: 24.5","X:  4750<br />length: 26.0","X:  4800<br />length: 26.0","X:  4850<br />length: 26.5","X:  4900<br />length: 27.0","X:  4950<br />length: 26.5","X:  5000<br />length: 27.5","X:  5050<br />length: 26.5","X:  5100<br />length: 24.0","X:  5150<br />length: 26.0","X:  5200<br />length: 26.5","X:  5250<br />length: 22.5","X:  5300<br />length: 25.5","X:  5350<br />length: 24.5","X:  5400<br />length: 26.0","X:  5450<br />length: 25.5","X:  5500<br />length: 24.0","X:  5550<br />length: 26.5","X:  5600<br />length: 25.0","X:  5650<br />length: 24.0","X:  5700<br />length: 23.0","X:  5750<br />length: 26.5","X:  5800<br />length: 26.0","X:  5850<br />length: 27.0","X:  5900<br />length: 23.5","X:  5950<br />length: 25.0","X:  6000<br />length: 24.5","X:  6050<br />length: 24.5","X:  6100<br />length: 25.0","X:  6150<br />length: 24.5","X:  6200<br />length: 26.0","X:  6250<br />length: 27.0","X:  6300<br />length: 25.0","X:  6350<br />length: 27.0","X:  6400<br />length: 27.5","X:  6450<br />length: 24.5","X:  6500<br />length: 26.0","X:  6550<br />length: 27.0","X:  6600<br />length: 21.5","X:  6650<br />length: 24.0","X:  6700<br />length: 26.5","X:  6750<br />length: 26.5","X:  6800<br />length: 25.5","X:  6850<br />length: 25.5","X:  6900<br />length: 24.0","X:  6950<br />length: 26.0","X:  7000<br />length: 23.5","X:  7050<br />length: 26.5","X:  7100<br />length: 25.5","X:  7150<br />length: 24.0","X:  7200<br />length: 25.5","X:  7250<br />length: 27.0","X:  7300<br />length: 25.5","X:  7350<br />length: 24.5","X:  7400<br />length: 25.0","X:  7450<br />length: 26.0","X:  7500<br />length: 25.0","X:  7550<br />length: 25.0","X:  7600<br />length: 28.0","X:  7650<br />length: 26.0","X:  7700<br />length: 27.0","X:  7750<br />length: 25.0","X:  7800<br />length: 22.5","X:  7850<br />length: 26.5","X:  7900<br />length: 26.5","X:  7950<br />length: 26.5","X:  8000<br />length: 26.5","X:  8050<br />length: 26.0","X:  8100<br />length: 25.5","X:  8150<br />length: 26.5","X:  8200<br />length: 23.0","X:  8250<br />length: 25.5","X:  8300<br />length: 24.5","X:  8350<br />length: 27.5","X:  8400<br />length: 26.5","X:  8450<br />length: 24.0","X:  8500<br />length: 25.0","X:  8550<br />length: 26.0","X:  8600<br />length: 26.5","X:  8650<br />length: 28.0","X:  8700<br />length: 25.5","X:  8750<br />length: 26.5","X:  8800<br />length: 27.0","X:  8850<br />length: 26.5","X:  8900<br />length: 26.5","X:  8950<br />length: 21.5","X:  9000<br />length: 24.5","X:  9050<br />length: 25.0","X:  9100<br />length: 27.5","X:  9150<br />length: 27.0","X:  9200<br />length: 26.0","X:  9250<br />length: 24.0","X:  9300<br />length: 27.0","X:  9350<br />length: 25.5","X:  9400<br />length: 24.0","X:  9450<br />length: 23.0","X:  9500<br />length: 24.0","X:  9550<br />length: 25.5","X:  9600<br />length: 23.0","X:  9650<br />length: 26.5","X:  9700<br />length: 27.0","X:  9750<br />length: 26.5","X:  9800<br />length: 27.0","X:  9850<br />length: 25.5","X:  9900<br />length: 27.5","X:  9950<br />length: 25.5","X: 10000<br />length: 24.0","X: 10050<br />length: 25.0","X: 10100<br />length: 28.0","X: 10150<br />length: 25.0","X: 10200<br />length: 27.0","X: 10250<br />length: 25.0","X: 10300<br />length: 25.5","X: 10350<br />length: 26.5","X: 10400<br />length: 26.0","X: 10450<br />length: 25.0","X: 10500<br />length: 27.0","X: 10550<br />length: 25.5","X: 10600<br />length: 25.0","X: 10650<br />length: 25.5","X: 10700<br />length: 26.0","X: 10750<br />length: 26.5","X: 10800<br />length: 25.5","X: 10850<br />length: 27.0","X: 10900<br />length: 24.5","X: 10950<br />length: 26.5","X: 11000<br />length: 26.5","X: 11050<br />length: 29.5","X: 11100<br />length: 25.0","X: 11150<br />length: 26.5","X: 11200<br />length: 27.0","X: 11250<br />length: 26.5","X: 11300<br />length: 25.0","X: 11350<br />length: 27.5","X: 11400<br />length: 28.0","X: 11450<br />length: 26.0","X: 11500<br />length: 28.0","X: 11550<br />length: 27.0","X: 11600<br />length: 27.0","X: 11650<br />length: 26.0","X: 11700<br />length: 27.5","X: 11750<br />length: 27.5","X: 11800<br />length: 25.0","X: 11850<br />length: 27.0","X: 11900<br />length: 26.5","X: 11950<br />length: 25.0","X: 12000<br />length: 26.0","X: 12050<br />length: 25.0","X: 12100<br />length: 27.0","X: 12150<br />length: 27.5","X: 12200<br />length: 25.0","X: 12250<br />length: 27.5","X: 12300<br />length: 28.0","X: 12350<br />length: 24.5","X: 12400<br />length: 23.5","X: 12450<br />length: 27.0","X: 12500<br />length: 28.0","X: 12550<br />length: 25.5","X: 12600<br />length: 26.5","X: 12650<br />length: 27.5","X: 12700<br />length: 25.0","X: 12750<br />length: 24.5","X: 12800<br />length: 25.5","X: 12850<br />length: 28.5","X: 12900<br />length: 26.0","X: 12950<br />length: 26.5","X: 13000<br />length: 25.5","X: 13050<br />length: 24.0","X: 13100<br />length: 28.0","X: 13150<br />length: 25.5","X: 13200<br />length: 26.0","X: 13250<br />length: 26.5","X: 13300<br />length: 26.5","X: 13350<br />length: 26.0","X: 13400<br />length: 25.0","X: 13450<br />length: 25.0","X: 13500<br />length: 26.0","X: 13550<br />length: 26.5","X: 13600<br />length: 25.5","X: 13650<br />length: 26.0","X: 13700<br />length: 26.0","X: 13750<br />length: 25.0","X: 13800<br />length: 26.0","X: 13850<br />length: 26.0","X: 13900<br />length: 25.5","X: 13950<br />length: 26.5","X: 14000<br />length: 27.5","X: 14050<br />length: 28.5","X: 14100<br />length: 25.5","X: 14150<br />length: 28.0","X: 14200<br />length: 26.0","X: 14250<br />length: 27.5","X: 14300<br />length: 27.0","X: 14350<br />length: 27.0","X: 14400<br />length: 26.5","X: 14450<br />length: 28.0","X: 14500<br />length: 26.5","X: 14550<br />length: 27.5","X: 14600<br />length: 24.5","X: 14650<br />length: 25.5","X: 14700<br />length: 28.0","X: 14750<br />length: 26.5","X: 14800<br />length: 27.5","X: 14850<br />length: 26.5","X: 14900<br />length: 27.5","X: 14950<br />length: 27.0","X: 15000<br />length: 27.5","X: 15050<br />length: 25.5","X: 15100<br />length: 26.5","X: 15150<br />length: 26.0","X: 15200<br />length: 24.5","X: 15250<br />length: 26.0","X: 15300<br />length: 25.5","X: 15350<br />length: 26.0","X: 15400<br />length: 26.0","X: 15450<br />length: 27.0","X: 15500<br />length: 26.5","X: 15550<br />length: 26.5","X: 15600<br />length: 26.5","X: 15650<br />length: 28.0","X: 15700<br />length: 26.0","X: 15750<br />length: 24.5","X: 15800<br />length: 27.0","X: 15850<br />length: 26.5","X: 15900<br />length: 21.5","X: 15950<br />length: 26.5","X: 16000<br />length: 28.0","X: 16050<br />length: 26.5","X: 16100<br />length: 26.5","X: 16150<br />length: 27.5","X: 16200<br />length: 27.0","X: 16250<br />length: 26.5","X: 16300<br />length: 27.0","X: 16350<br />length: 27.5","X: 16400<br />length: 29.0","X: 16450<br />length: 29.5","X: 16500<br />length: 26.5","X: 16550<br />length: 28.5","X: 16600<br />length: 26.0","X: 16650<br />length: 27.0","X: 16700<br />length: 28.5","X: 16750<br />length: 27.0","X: 16800<br />length: 26.5","X: 16850<br />length: 27.5","X: 16900<br />length: 28.5","X: 16950<br />length: 28.0","X: 17000<br />length: 27.0","X: 17050<br />length: 26.5","X: 17100<br />length: 25.5","X: 17150<br />length: 26.0","X: 17200<br />length: 27.0","X: 17250<br />length: 28.0","X: 17300<br />length: 28.0","X: 17350<br />length: 26.0","X: 17400<br />length: 27.5","X: 17450<br />length: 29.0","X: 17500<br />length: 26.5","X: 17550<br />length: 27.5","X: 17600<br />length: 27.0","X: 17650<br />length: 28.0","X: 17700<br />length: 29.0","X: 17750<br />length: 27.0","X: 17800<br />length: 26.5","X: 17850<br />length: 28.5","X: 17900<br />length: 24.5","X: 17950<br />length: 26.5","X: 18000<br />length: 25.0","X: 18050<br />length: 26.0","X: 18100<br />length: 29.5","X: 18150<br />length: 26.5","X: 18200<br />length: 26.5","X: 18250<br />length: 27.5","X: 18300<br />length: 28.0","X: 18350<br />length: 25.5","X: 18400<br />length: 26.0","X: 18450<br />length: 25.5","X: 18500<br />length: 26.5","X: 18550<br />length: 27.5","X: 18600<br />length: 26.0","X: 18650<br />length: 26.0","X: 18700<br />length: 27.5","X: 18750<br />length: 27.0","X: 18800<br />length: 27.5","X: 18850<br />length: 29.8","X: 18900<br />length: 26.0","X: 18950<br />length: 28.0","X: 19000<br />length: 28.0","X: 19050<br />length: 28.5","X: 19100<br />length: 29.0","X: 19150<br />length: 28.0","X: 19200<br />length: 26.5","X: 19250<br />length: 27.5","X: 19300<br />length: 27.0","X: 19350<br />length: 27.0","X: 19400<br />length: 27.0","X: 19450<br />length: 27.0","X: 19500<br />length: 26.0","X: 19550<br />length: 25.5","X: 19600<br />length: 26.0","X: 19650<br />length: 27.5","X: 19700<br />length: 26.0","X: 19750<br />length: 25.0","X: 19800<br />length: 25.5","X: 19850<br />length: 25.5","X: 19900<br />length: 24.5","X: 19950<br />length: 24.5","X: 20000<br />length: 26.0","X: 20050<br />length: 26.0","X: 20100<br />length: 28.0","X: 20150<br />length: 28.5","X: 20200<br />length: 26.0","X: 20250<br />length: 25.5","X: 20300<br />length: 24.0","X: 20350<br />length: 26.5","X: 20400<br />length: 26.0","X: 20450<br />length: 26.0","X: 20500<br />length: 26.5","X: 20550<br />length: 25.0","X: 20600<br />length: 27.0","X: 20650<br />length: 26.5","X: 20700<br />length: 25.0","X: 20750<br />length: 24.5","X: 20800<br />length: 26.5","X: 20850<br />length: 25.5","X: 20900<br />length: 26.0","X: 20950<br />length: 27.0","X: 21000<br />length: 25.5","X: 21050<br />length: 25.5","X: 21100<br />length: 26.5","X: 21150<br />length: 27.0","X: 21200<br />length: 24.0","X: 21250<br />length: 25.0","X: 21300<br />length: 28.0","X: 21350<br />length: 29.0","X: 21400<br />length: 24.5","X: 21450<br />length: 25.0","X: 21500<br />length: 25.5","X: 21550<br />length: 25.5","X: 21600<br />length: 30.5","X: 21650<br />length: 23.5","X: 21700<br />length: 26.0","X: 21750<br />length: 26.5","X: 21800<br />length: 28.5","X: 21850<br />length: 24.0","X: 21900<br />length: 25.0","X: 21950<br />length: 23.0","X: 22000<br />length: 23.0","X: 22050<br />length: 24.5","X: 22100<br />length: 24.5","X: 22150<br />length: 27.0","X: 22200<br />length: 25.5","X: 22250<br />length: 23.0","X: 22300<br />length: 23.0","X: 22350<br />length: 24.5","X: 22400<br />length: 26.0","X: 22450<br />length: 25.0","X: 22500<br />length: 24.5","X: 22550<br />length: 23.5","X: 22600<br />length: 22.0","X: 22650<br />length: 24.5","X: 22700<br />length: 24.0","X: 22750<br />length: 24.0","X: 22800<br />length: 27.5","X: 22850<br />length: 21.5","X: 22900<br />length: 26.5","X: 22950<br />length: 25.5","X: 23000<br />length: 27.0","X: 23050<br />length: 28.5","X: 23100<br />length: 27.5","X: 23150<br />length: 25.0","X: 23200<br />length: 27.5","X: 23250<br />length: 26.5","X: 23300<br />length: 26.5","X: 23350<br />length: 25.5","X: 23400<br />length: 27.0","X: 23450<br />length: 26.5","X: 23500<br />length: 27.0","X: 23550<br />length: 26.5","X: 23600<br />length: 27.0","X: 23650<br />length: 26.5","X: 23700<br />length: 27.5","X: 23750<br />length: 27.0","X: 23800<br />length: 25.5","X: 23850<br />length: 28.5","X: 23900<br />length: 27.0","X: 23950<br />length: 27.0","X: 24000<br />length: 28.0","X: 24050<br />length: 23.5","X: 24100<br />length: 27.0","X: 24150<br />length: 25.0","X: 24200<br />length: 26.5","X: 24250<br />length: 24.5","X: 24300<br />length: 25.0","X: 24350<br />length: 25.5","X: 24400<br />length: 26.0","X: 24450<br />length: 27.5","X: 24500<br />length: 25.5","X: 24550<br />length: 25.5","X: 24600<br />length: 28.5","X: 24650<br />length: 23.5","X: 24700<br />length: 25.0","X: 24750<br />length: 25.5","X: 24800<br />length: 24.5","X: 24850<br />length: 23.0","X: 24900<br />length: 25.0","X: 24950<br />length: 23.5","X: 25000<br />length: 28.0","X: 25050<br />length: 26.5","X: 25100<br />length: 27.5","X: 25150<br />length: 25.0","X: 25200<br />length: 25.0","X: 25250<br />length: 23.0","X: 25300<br />length: 25.0","X: 25350<br />length: 29.0","X: 25400<br />length: 26.5","X: 25450<br />length: 26.5","X: 25500<br />length: 25.0","X: 25550<br />length: 25.5","X: 25600<br />length: 27.0","X: 25650<br />length: 27.0","X: 25700<br />length: 23.5","X: 25750<br />length: 28.5","X: 25800<br />length: 25.5","X: 25850<br />length: 26.5","X: 25900<br />length: 27.5","X: 25950<br />length: 30.5","X: 26000<br />length: 27.0","X: 26050<br />length: 27.5","X: 26100<br />length: 26.5","X: 26150<br />length: 26.5","X: 26200<br />length: 26.5","X: 26250<br />length: 28.5","X: 26300<br />length: 27.0","X: 26350<br />length: 24.5","X: 26400<br />length: 28.0","X: 26450<br />length: 22.5","X: 26500<br />length: 23.5","X: 26550<br />length: 26.5","X: 26600<br />length: 25.5","X: 26650<br />length: 21.0","X: 26700<br />length: 24.5","X: 26750<br />length: 25.0","X: 26800<br />length: 26.0","X: 26850<br />length: 25.0","X: 26900<br />length: 25.0","X: 26950<br />length: 22.0","X: 27000<br />length: 23.0","X: 27050<br />length: 22.5","X: 27100<br />length: 24.0","X: 27150<br />length: 23.5","X: 27200<br />length: 27.0","X: 27250<br />length: 28.5","X: 27300<br />length: 23.0","X: 27350<br />length: 22.0","X: 27400<br />length: 22.0","X: 27450<br />length: 23.5","X: 27500<br />length: 27.0","X: 27550<br />length: 26.0","X: 27600<br />length: 23.5","X: 27650<br />length: 24.5","X: 27700<br />length: 26.5","X: 27750<br />length: 28.0","X: 27800<br />length: 28.0","X: 27850<br />length: 25.0","X: 27900<br />length: 25.0","X: 27950<br />length: 28.0","X: 28000<br />length: 28.5","X: 28050<br />length: 27.0","X: 28100<br />length: 22.5","X: 28150<br />length: 25.0","X: 28200<br />length: 22.5","X: 28250<br />length: 23.0","X: 28300<br />length: 25.5","X: 28350<br />length: 26.5","X: 28400<br />length: 27.0","X: 28450<br />length: 27.0","X: 28500<br />length: 25.0","X: 28550<br />length: 24.5","X: 28600<br />length: 27.5","X: 28650<br />length: 26.5","X: 28700<br />length: 25.5","X: 28750<br />length: 25.5","X: 28800<br />length: 26.0","X: 28850<br />length: 25.0","X: 28900<br />length: 26.0","X: 28950<br />length: 27.0","X: 29000<br />length: 27.5","X: 29050<br />length: 26.5","X: 29100<br />length: 28.0","X: 29150<br />length: 25.0","X: 29200<br />length: 26.5","X: 29250<br />length: 26.0","X: 29300<br />length: 25.0","X: 29350<br />length: 27.0","X: 29400<br />length: 25.5","X: 29450<br />length: 25.5","X: 29500<br />length: 26.5","X: 29550<br />length: 27.0","X: 29600<br />length: 26.0","X: 29650<br />length: 23.5","X: 29700<br />length: 27.0","X: 29750<br />length: 22.5","X: 29800<br />length: 26.5","X: 29850<br />length: 28.0","X: 29900<br />length: 28.0","X: 29950<br />length: 25.0","X: 30000<br />length: 26.5","X: 30050<br />length: 26.0","X: 30100<br />length: 25.5","X: 30150<br />length: 26.0","X: 30200<br />length: 25.5","X: 30250<br />length: 26.0","X: 30300<br />length: 25.0","X: 30350<br />length: 26.5","X: 30400<br />length: 26.0","X: 30450<br />length: 23.5","X: 30500<br />length: 26.0","X: 30550<br />length: 25.5","X: 30600<br />length: 28.0","X: 30650<br />length: 26.0","X: 30700<br />length: 24.5","X: 30750<br />length: 24.0","X: 30800<br />length: 23.5","X: 30850<br />length: 26.0","X: 30900<br />length: 26.0","X: 30950<br />length: 24.5","X: 31000<br />length: 26.0","X: 31050<br />length: 25.0","X: 31100<br />length: 25.5","X: 31150<br />length: 24.0","X: 31200<br />length: 27.0","X: 31250<br />length: 24.0","X: 31300<br />length: 26.5","X: 31350<br />length: 25.5","X: 31400<br />length: 24.5","X: 31450<br />length: 27.0","X: 31500<br />length: 26.5","X: 31550<br />length: 26.0","X: 31600<br />length: 28.0","X: 31650<br />length: 25.0","X: 31700<br />length: 25.5","X: 31750<br />length: 25.0","X: 31800<br />length: 26.5","X: 31850<br />length: 26.0","X: 31900<br />length: 23.5","X: 31950<br />length: 23.0","X: 32000<br />length: 25.0","X: 32050<br />length: 25.5","X: 32100<br />length: 27.0","X: 32150<br />length: 25.0","X: 32200<br />length: 24.0","X: 32250<br />length: 24.5","X: 32300<br />length: 28.0","X: 32350<br />length: 28.0","X: 32400<br />length: 23.5","X: 32450<br />length: 28.5","X: 32500<br />length: 26.5","X: 32550<br />length: 27.0","X: 32600<br />length: 22.5","X: 32650<br />length: 25.5","X: 32700<br />length: 26.0","X: 32750<br />length: 25.5","X: 32800<br />length: 25.5","X: 32850<br />length: 25.0","X: 32900<br />length: 25.0","X: 32950<br />length: 27.5","X: 33000<br />length: 25.0","X: 33050<br />length: 24.0","X: 33100<br />length: 25.0","X: 33150<br />length: 24.5","X: 33200<br />length: 25.0","X: 33250<br />length: 26.0","X: 33300<br />length: 24.5","X: 33350<br />length: 26.0","X: 33400<br />length: 25.0","X: 33450<br />length: 25.0","X: 33500<br />length: 23.5","X: 33550<br />length: 23.0","X: 33600<br />length: 23.0","X: 33650<br />length: 27.0","X: 33700<br />length: 23.0","X: 33750<br />length: 24.0","X: 33800<br />length: 26.5","X: 33850<br />length: 24.0","X: 33900<br />length: 24.5","X: 33950<br />length: 25.5","X: 34000<br />length: 26.0","X: 34050<br />length: 28.0","X: 34100<br />length: 28.5","X: 34150<br />length: 25.0","X: 34200<br />length: 27.5","X: 34250<br />length: 26.0","X: 34300<br />length: 26.0","X: 34350<br />length: 24.5","X: 34400<br />length: 22.5","X: 34450<br />length: 22.5","X: 34500<br />length: 26.5","X: 34550<br />length: 24.5","X: 34600<br />length: 22.0","X: 34650<br />length: 25.5","X: 34700<br />length: 24.5","X: 34750<br />length: 25.0","X: 34800<br />length: 24.5","X: 34850<br />length: 25.0","X: 34900<br />length: 25.0","X: 34950<br />length: 24.0","X: 35000<br />length: 24.0","X: 35050<br />length: 27.0","X: 35100<br />length: 24.5","X: 35150<br />length: 26.5","X: 35200<br />length: 25.0","X: 35250<br />length: 26.0","X: 35300<br />length: 25.5","X: 35350<br />length: 25.0","X: 35400<br />length: 25.0","X: 35450<br />length: 26.5","X: 35500<br />length: 24.5","X: 35550<br />length: 24.5","X: 35600<br />length: 24.0","X: 35650<br />length: 26.5","X: 35700<br />length: 24.0","X: 35750<br />length: 23.5","X: 35800<br />length: 24.5","X: 35850<br />length: 25.0","X: 35900<br />length: 24.0","X: 35950<br />length: 23.0","X: 36000<br />length: 25.5","X: 36050<br />length: 24.5","X: 36100<br />length: 25.5","X: 36150<br />length: 24.5","X: 36200<br />length: 26.5","X: 36250<br />length: 25.5","X: 36300<br />length: 24.5","X: 36350<br />length: 25.5","X: 36400<br />length: 23.0","X: 36450<br />length: 25.5","X: 36500<br />length: 26.5","X: 36550<br />length: 26.0","X: 36600<br />length: 26.0","X: 36650<br />length: 26.5","X: 36700<br />length: 24.5","X: 36750<br />length: 24.5","X: 36800<br />length: 26.5","X: 36850<br />length: 25.5","X: 36900<br />length: 24.0","X: 36950<br />length: 24.0","X: 37000<br />length: 25.0","X: 37050<br />length: 26.0","X: 37100<br />length: 25.0","X: 37150<br />length: 26.0","X: 37200<br />length: 25.0","X: 37250<br />length: 25.0","X: 37300<br />length: 25.0","X: 37350<br />length: 24.5","X: 37400<br />length: 27.5","X: 37450<br />length: 26.5","X: 37500<br />length: 25.5","X: 37550<br />length: 25.5","X: 37600<br />length: 24.5","X: 37650<br />length: 27.5","X: 37700<br />length: 24.0","X: 37750<br />length: 28.5","X: 37800<br />length: 24.5","X: 37850<br />length: 28.0","X: 37900<br />length: 26.0","X: 37950<br />length: 25.5","X: 38000<br />length: 26.5","X: 38050<br />length: 25.0","X: 38100<br />length: 26.0","X: 38150<br />length: 26.5","X: 38200<br />length: 22.0","X: 38250<br />length: 26.0","X: 38300<br />length: 24.0","X: 38350<br />length: 25.5","X: 38400<br />length: 25.0","X: 38450<br />length: 24.5","X: 38500<br />length: 26.5","X: 38550<br />length: 23.5","X: 38600<br />length: 23.0","X: 38650<br />length: 24.0","X: 38700<br />length: 22.5","X: 38750<br />length: 23.0","X: 38800<br />length: 23.5","X: 38850<br />length: 23.5","X: 38900<br />length: 24.5","X: 38950<br />length: 24.5","X: 39000<br />length: 24.0","X: 39050<br />length: 25.5","X: 39100<br />length: 26.5","X: 39150<br />length: 24.0","X: 39200<br />length: 25.5","X: 39250<br />length: 22.5","X: 39300<br />length: 25.5","X: 39350<br />length: 25.0","X: 39400<br />length: 25.5","X: 39450<br />length: 24.0","X: 39500<br />length: 26.5","X: 39550<br />length: 24.5","X: 39600<br />length: 24.5","X: 39650<br />length: 25.5","X: 39700<br />length: 26.0","X: 39750<br />length: 24.0","X: 39800<br />length: 23.0","X: 39850<br />length: 22.5","X: 39900<br />length: 22.5","X: 39950<br />length: 25.0","X: 40000<br />length: 23.5","X: 40050<br />length: 24.5","X: 40100<br />length: 23.5","X: 40150<br />length: 24.0","X: 40200<br />length: 25.0","X: 40250<br />length: 24.5","X: 40300<br />length: 26.0","X: 40350<br />length: 23.5","X: 40400<br />length: 25.5","X: 40450<br />length: 27.0","X: 40500<br />length: 24.5","X: 40550<br />length: 25.0","X: 40600<br />length: 25.0","X: 40650<br />length: 24.5","X: 40700<br />length: 25.5","X: 40750<br />length: 25.0","X: 40800<br />length: 26.0","X: 40850<br />length: 25.5","X: 40900<br />length: 27.0","X: 40950<br />length: 25.0","X: 41000<br />length: 25.5","X: 41050<br />length: 21.0","X: 41100<br />length: 24.0","X: 41150<br />length: 24.0","X: 41200<br />length: 26.0","X: 41250<br />length: 24.0","X: 41300<br />length: 24.0","X: 41350<br />length: 25.0","X: 41400<br />length: 22.5","X: 41450<br />length: 25.0","X: 41500<br />length: 25.5","X: 41550<br />length: 25.5","X: 41600<br />length: 26.0","X: 41650<br />length: 23.0","X: 41700<br />length: 23.5","X: 41750<br />length: 25.0","X: 41800<br />length: 24.0","X: 41850<br />length: 24.5","X: 41900<br />length: 24.0","X: 41950<br />length: 24.5","X: 42000<br />length: 25.0","X: 42050<br />length: 27.5","X: 42100<br />length: 24.5","X: 42150<br />length: 23.0","X: 42200<br />length: 24.5","X: 42250<br />length: 24.5","X: 42300<br />length: 24.5","X: 42350<br />length: 25.0","X: 42400<br />length: 25.0","X: 42450<br />length: 25.0","X: 42500<br />length: 23.0","X: 42550<br />length: 25.5","X: 42600<br />length: 24.5","X: 42650<br />length: 22.0","X: 42700<br />length: 22.0","X: 42750<br />length: 26.0","X: 42800<br />length: 24.0","X: 42850<br />length: 24.0","X: 42900<br />length: 24.0","X: 42950<br />length: 25.5","X: 43000<br />length: 24.5","X: 43050<br />length: 24.5","X: 43100<br />length: 25.5","X: 43150<br />length: 24.0","X: 43200<br />length: 25.5","X: 43250<br />length: 25.5","X: 43300<br />length: 23.5","X: 43350<br />length: 26.5","X: 43400<br />length: 25.5","X: 43450<br />length: 24.5","X: 43500<br />length: 23.5","X: 43550<br />length: 23.5","X: 43600<br />length: 24.5","X: 43650<br />length: 26.0","X: 43700<br />length: 24.0","X: 43750<br />length: 23.0","X: 43800<br />length: 25.0","X: 43850<br />length: 26.0","X: 43900<br />length: 23.5","X: 43950<br />length: 26.0","X: 44000<br />length: 27.0","X: 44050<br />length: 22.5","X: 44100<br />length: 23.5","X: 44150<br />length: 22.5","X: 44200<br />length: 24.5","X: 44250<br />length: 21.5","X: 44300<br />length: 22.0","X: 44350<br />length: 24.0","X: 44400<br />length: 23.5","X: 44450<br />length: 24.5","X: 44500<br />length: 26.0","X: 44550<br />length: 22.5","X: 44600<br />length: 25.5","X: 44650<br />length: 24.0","X: 44700<br />length: 23.0","X: 44750<br />length: 23.0","X: 44800<br />length: 23.5","X: 44850<br />length: 24.0","X: 44900<br />length: 27.0","X: 44950<br />length: 25.5","X: 45000<br />length: 26.0","X: 45050<br />length: 26.0","X: 45100<br />length: 25.5","X: 45150<br />length: 25.0","X: 45200<br />length: 25.0","X: 45250<br />length: 23.5","X: 45300<br />length: 21.5","X: 45350<br />length: 24.5","X: 45400<br />length: 22.5","X: 45450<br />length: 24.0","X: 45500<br />length: 26.5","X: 45550<br />length: 23.0","X: 45600<br />length: 23.0","X: 45650<br />length: 25.5","X: 45700<br />length: 24.5","X: 45750<br />length: 26.5","X: 45800<br />length: 24.5","X: 45850<br />length: 24.0","X: 45900<br />length: 23.0","X: 45950<br />length: 24.5","X: 46000<br />length: 24.0","X: 46050<br />length: 21.0","X: 46100<br />length: 22.5","X: 46150<br />length: 24.5","X: 46200<br />length: 21.5","X: 46250<br />length: 24.0","X: 46300<br />length: 26.0","X: 46350<br />length: 24.5","X: 46400<br />length: 24.5","X: 46450<br />length: 23.5","X: 46500<br />length: 25.0","X: 46550<br />length: 23.5","X: 46600<br />length: 23.0","X: 46650<br />length: 24.0","X: 46700<br />length: 25.0","X: 46750<br />length: 23.0","X: 46800<br />length: 23.0","X: 46850<br />length: 25.0","X: 46900<br />length: 21.5","X: 46950<br />length: 24.5","X: 47000<br />length: 26.0","X: 47050<br />length: 23.0","X: 47100<br />length: 27.5","X: 47150<br />length: 23.0","X: 47200<br />length: 23.5","X: 47250<br />length: 22.5","X: 47300<br />length: 25.0","X: 47350<br />length: 21.0","X: 47400<br />length: 22.0","X: 47450<br />length: 25.0","X: 47500<br />length: 24.5","X: 47550<br />length: 24.0","X: 47600<br />length: 25.0","X: 47650<br />length: 24.0","X: 47700<br />length: 23.5","X: 47750<br />length: 23.0","X: 47800<br />length: 23.0","X: 47850<br />length: 24.5","X: 47900<br />length: 23.5","X: 47950<br />length: 25.5","X: 48000<br />length: 25.5","X: 48050<br />length: 21.5","X: 48100<br />length: 23.0","X: 48150<br />length: 23.0","X: 48200<br />length: 24.5","X: 48250<br />length: 21.0","X: 48300<br />length: 23.5","X: 48350<br />length: 25.5","X: 48400<br />length: 24.5","X: 48450<br />length: 22.0","X: 48500<br />length: 23.5","X: 48550<br />length: 27.0","X: 48600<br />length: 24.0","X: 48650<br />length: 24.0","X: 48700<br />length: 25.5","X: 48750<br />length: 23.5","X: 48800<br />length: 23.5","X: 48850<br />length: 24.5","X: 48900<br />length: 26.0","X: 48950<br />length: 25.5","X: 49000<br />length: 24.5","X: 49050<br />length: 27.0","X: 49100<br />length: 22.0","X: 49150<br />length: 25.5","X: 49200<br />length: 26.0","X: 49250<br />length: 23.0","X: 49300<br />length: 25.5","X: 49350<br />length: 24.5","X: 49400<br />length: 23.5","X: 49450<br />length: 24.0","X: 49500<br />length: 23.0","X: 49550<br />length: 22.5","X: 49600<br />length: 23.0","X: 49650<br />length: 25.5","X: 49700<br />length: 25.0","X: 49750<br />length: 25.0","X: 49800<br />length: 25.5","X: 49850<br />length: 23.5","X: 49900<br />length: 25.5","X: 49950<br />length: 24.5","X: 50000<br />length: 24.5","X: 50050<br />length: 23.5","X: 50100<br />length: 23.5","X: 50150<br />length: 24.0","X: 50200<br />length: 24.0","X: 50250<br />length: 24.0","X: 50300<br />length: 23.0","X: 50350<br />length: 22.0","X: 50400<br />length: 23.5","X: 50450<br />length: 23.0","X: 50500<br />length: 25.0","X: 50550<br />length: 23.0","X: 50600<br />length: 22.5","X: 50650<br />length: 23.5","X: 50700<br />length: 22.5","X: 50750<br />length: 23.0","X: 50800<br />length: 25.0","X: 50850<br />length: 24.5","X: 50900<br />length: 22.5","X: 50950<br />length: 24.0","X: 51000<br />length: 22.5","X: 51050<br />length: 22.0","X: 51100<br />length: 23.5","X: 51150<br />length: 21.0","X: 51200<br />length: 23.0","X: 51250<br />length: 25.0","X: 51300<br />length: 22.5","X: 51350<br />length: 23.0","X: 51400<br />length: 22.0","X: 51450<br />length: 24.0","X: 51500<br />length: 25.0","X: 51550<br />length: 24.0","X: 51600<br />length: 25.0","X: 51650<br />length: 22.0","X: 51700<br />length: 23.0","X: 51750<br />length: 24.0","X: 51800<br />length: 27.0","X: 51850<br />length: 22.5","X: 51900<br />length: 25.5","X: 51950<br />length: 24.5","X: 52000<br />length: 23.0","X: 52050<br />length: 24.0","X: 52100<br />length: 23.5","X: 52150<br />length: 23.0","X: 52200<br />length: 26.0","X: 52250<br />length: 24.5","X: 52300<br />length: 26.0","X: 52350<br />length: 26.0","X: 52400<br />length: 24.5","X: 52450<br />length: 27.0","X: 52500<br />length: 25.5","X: 52550<br />length: 22.0"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,0,0,1)","opacity":1,"size":5.66929133858268,"symbol":"circle-open","line":{"width":1.88976377952756,"color":"rgba(0,0,0,1)"}},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[0,665.189873417722,1330.37974683544,1995.56962025316,2660.75949367089,3325.94936708861,3991.13924050633,4656.32911392405,5321.51898734177,5986.70886075949,6651.89873417722,7317.08860759494,7982.27848101266,8647.46835443038,9312.6582278481,9977.84810126582,10643.0379746835,11308.2278481013,11973.417721519,12638.6075949367,13303.7974683544,13968.9873417722,14634.1772151899,15299.3670886076,15964.5569620253,16629.746835443,17294.9367088608,17960.1265822785,18625.3164556962,19290.5063291139,19955.6962025316,20620.8860759494,21286.0759493671,21951.2658227848,22616.4556962025,23281.6455696203,23946.835443038,24612.0253164557,25277.2151898734,25942.4050632911,26607.5949367089,27272.7848101266,27937.9746835443,28603.164556962,29268.3544303797,29933.5443037975,30598.7341772152,31263.9240506329,31929.1139240506,32594.3037974684,33259.4936708861,33924.6835443038,34589.8734177215,35255.0632911392,35920.253164557,36585.4430379747,37250.6329113924,37915.8227848101,38581.0126582278,39246.2025316456,39911.3924050633,40576.582278481,41241.7721518987,41906.9620253165,42572.1518987342,43237.3417721519,43902.5316455696,44567.7215189873,45232.9113924051,45898.1012658228,46563.2911392405,47228.4810126582,47893.6708860759,48558.8607594937,49224.0506329114,49889.2405063291,50554.4303797468,51219.6202531646,51884.8101265823,52550],"y":[24.1398259640317,24.2893422490761,24.4377359947443,24.5838846616601,24.7266657104474,24.86495660173,24.9976347961317,25.1235777542762,25.2416629367876,25.3507739877705,25.4507985940635,25.5437299177982,25.6318262378561,25.7173458331184,25.8025469824665,25.8896879647818,25.9810270589454,26.0788225438388,26.1852101193429,26.2986078940958,26.4131360505376,26.5226753589989,26.6211065898099,26.702310513301,26.7601678998027,26.7885595196453,26.7813661431592,26.7332111523408,26.6473507898058,26.5326108856658,26.3979100964905,26.25216707885,26.104300489314,25.9632289844526,25.8378712208355,25.7371458550327,25.6682160602974,25.6284218464029,25.6116882594829,25.6119369169931,25.6230894363888,25.6390674351256,25.6537925306592,25.6611863404448,25.6551725508833,25.6317355176877,25.5927871242623,25.5412985538988,25.480240989889,25.4125856155248,25.3413036140979,25.2693661689002,25.1997444632233,25.1353799244829,25.0774286275181,25.024279350675,24.9740828252893,24.9249897826969,24.8751509542334,24.8227170712348,24.7658388650368,24.7026670669751,24.6314830875645,24.5529142899374,24.4696172235147,24.3843153454568,24.2997321129243,24.2185909830776,24.1436154130771,24.0775288600834,24.0230547812569,23.9825695863888,23.9556935423132,23.9407325877806,23.9359845671419,23.9397473247482,23.9503187049504,23.9659965520996,23.9850787105467,24.0058630246429],"text":["X:     0.0000<br />length: 24.13983","X:   665.1899<br />length: 24.28934","X:  1330.3797<br />length: 24.43774","X:  1995.5696<br />length: 24.58388","X:  2660.7595<br />length: 24.72667","X:  3325.9494<br />length: 24.86496","X:  3991.1392<br />length: 24.99763","X:  4656.3291<br />length: 25.12358","X:  5321.5190<br />length: 25.24166","X:  5986.7089<br />length: 25.35077","X:  6651.8987<br />length: 25.45080","X:  7317.0886<br />length: 25.54373","X:  7982.2785<br />length: 25.63183","X:  8647.4684<br />length: 25.71735","X:  9312.6582<br />length: 25.80255","X:  9977.8481<br />length: 25.88969","X: 10643.0380<br />length: 25.98103","X: 11308.2278<br />length: 26.07882","X: 11973.4177<br />length: 26.18521","X: 12638.6076<br />length: 26.29861","X: 13303.7975<br />length: 26.41314","X: 13968.9873<br />length: 26.52268","X: 14634.1772<br />length: 26.62111","X: 15299.3671<br />length: 26.70231","X: 15964.5570<br />length: 26.76017","X: 16629.7468<br />length: 26.78856","X: 17294.9367<br />length: 26.78137","X: 17960.1266<br />length: 26.73321","X: 18625.3165<br />length: 26.64735","X: 19290.5063<br />length: 26.53261","X: 19955.6962<br />length: 26.39791","X: 20620.8861<br />length: 26.25217","X: 21286.0759<br />length: 26.10430","X: 21951.2658<br />length: 25.96323","X: 22616.4557<br />length: 25.83787","X: 23281.6456<br />length: 25.73715","X: 23946.8354<br />length: 25.66822","X: 24612.0253<br />length: 25.62842","X: 25277.2152<br />length: 25.61169","X: 25942.4051<br />length: 25.61194","X: 26607.5949<br />length: 25.62309","X: 27272.7848<br />length: 25.63907","X: 27937.9747<br />length: 25.65379","X: 28603.1646<br />length: 25.66119","X: 29268.3544<br />length: 25.65517","X: 29933.5443<br />length: 25.63174","X: 30598.7342<br />length: 25.59279","X: 31263.9241<br />length: 25.54130","X: 31929.1139<br />length: 25.48024","X: 32594.3038<br />length: 25.41259","X: 33259.4937<br />length: 25.34130","X: 33924.6835<br />length: 25.26937","X: 34589.8734<br />length: 25.19974","X: 35255.0633<br />length: 25.13538","X: 35920.2532<br />length: 25.07743","X: 36585.4430<br />length: 25.02428","X: 37250.6329<br />length: 24.97408","X: 37915.8228<br />length: 24.92499","X: 38581.0127<br />length: 24.87515","X: 39246.2025<br />length: 24.82272","X: 39911.3924<br />length: 24.76584","X: 40576.5823<br />length: 24.70267","X: 41241.7722<br />length: 24.63148","X: 41906.9620<br />length: 24.55291","X: 42572.1519<br />length: 24.46962","X: 43237.3418<br />length: 24.38432","X: 43902.5316<br />length: 24.29973","X: 44567.7215<br />length: 24.21859","X: 45232.9114<br />length: 24.14362","X: 45898.1013<br />length: 24.07753","X: 46563.2911<br />length: 24.02305","X: 47228.4810<br />length: 23.98257","X: 47893.6709<br />length: 23.95569","X: 48558.8608<br />length: 23.94073","X: 49224.0506<br />length: 23.93598","X: 49889.2405<br />length: 23.93975","X: 50554.4304<br />length: 23.95032","X: 51219.6203<br />length: 23.96600","X: 51884.8101<br />length: 23.98508","X: 52550.0000<br />length: 24.00586"],"type":"scatter","mode":"lines","name":"fitted values","line":{"width":3.77952755905512,"color":"rgba(51,102,255,1)","dash":"solid"},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[0,665.189873417722,1330.37974683544,1995.56962025316,2660.75949367089,3325.94936708861,3991.13924050633,4656.32911392405,5321.51898734177,5986.70886075949,6651.89873417722,7317.08860759494,7982.27848101266,8647.46835443038,9312.6582278481,9977.84810126582,10643.0379746835,11308.2278481013,11973.417721519,12638.6075949367,13303.7974683544,13968.9873417722,14634.1772151899,15299.3670886076,15964.5569620253,16629.746835443,17294.9367088608,17960.1265822785,18625.3164556962,19290.5063291139,19955.6962025316,20620.8860759494,21286.0759493671,21951.2658227848,22616.4556962025,23281.6455696203,23946.835443038,24612.0253164557,25277.2151898734,25942.4050632911,26607.5949367089,27272.7848101266,27937.9746835443,28603.164556962,29268.3544303797,29933.5443037975,30598.7341772152,31263.9240506329,31929.1139240506,32594.3037974684,33259.4936708861,33924.6835443038,34589.8734177215,35255.0632911392,35920.253164557,36585.4430379747,37250.6329113924,37915.8227848101,38581.0126582278,39246.2025316456,39911.3924050633,40576.582278481,41241.7721518987,41906.9620253165,42572.1518987342,43237.3417721519,43902.5316455696,44567.7215189873,45232.9113924051,45898.1012658228,46563.2911392405,47228.4810126582,47893.6708860759,48558.8607594937,49224.0506329114,49889.2405063291,50554.4303797468,51219.6202531646,51884.8101265823,52550,52550,52550,51884.8101265823,51219.6202531646,50554.4303797468,49889.2405063291,49224.0506329114,48558.8607594937,47893.6708860759,47228.4810126582,46563.2911392405,45898.1012658228,45232.9113924051,44567.7215189873,43902.5316455696,43237.3417721519,42572.1518987342,41906.9620253165,41241.7721518987,40576.582278481,39911.3924050633,39246.2025316456,38581.0126582278,37915.8227848101,37250.6329113924,36585.4430379747,35920.253164557,35255.0632911392,34589.8734177215,33924.6835443038,33259.4936708861,32594.3037974684,31929.1139240506,31263.9240506329,30598.7341772152,29933.5443037975,29268.3544303797,28603.164556962,27937.9746835443,27272.7848101266,26607.5949367089,25942.4050632911,25277.2151898734,24612.0253164557,23946.835443038,23281.6455696203,22616.4556962025,21951.2658227848,21286.0759493671,20620.8860759494,19955.6962025316,19290.5063291139,18625.3164556962,17960.1265822785,17294.9367088608,16629.746835443,15964.5569620253,15299.3670886076,14634.1772151899,13968.9873417722,13303.7974683544,12638.6075949367,11973.417721519,11308.2278481013,10643.0379746835,9977.84810126582,9312.6582278481,8647.46835443038,7982.27848101266,7317.08860759494,6651.89873417722,5986.70886075949,5321.51898734177,4656.32911392405,3991.13924050633,3325.94936708861,2660.75949367089,1995.56962025316,1330.37974683544,665.189873417722,0,0],"y":[23.6341473928121,23.8682686764162,24.0910044653855,24.2950293229188,24.473132597273,24.6225530409456,24.7486147744727,24.8611797786492,24.9687465630037,25.0763573177366,25.1851403433208,25.2924805698554,25.3942303535319,25.4865747787892,25.5686801138775,25.6445651201605,25.7220184924083,25.8098757628995,25.9157494590097,26.0384444849722,26.1668626620671,26.2883501427635,26.3910842951881,26.4667898205044,26.5120704732825,26.5267563731064,26.5112115275499,26.4649162146593,26.3898918789717,26.2892172171607,26.1652376807678,26.0216983513905,25.8664111589793,25.7119524753852,25.5735303353688,25.466397093875,25.4016826762475,25.3739884479702,25.371144581388,25.3805799814788,25.3917324998177,25.3985237537285,25.3993591262949,25.394652947234,25.3844237765838,25.367394614347,25.3415105921666,25.3034091956461,25.2497722308196,25.1799131662287,25.0979099112174,25.011907222897,24.9314494890291,24.8652252713326,24.8156254458724,24.7761818994443,24.7385621291822,24.6949675149785,24.6408257945575,24.5764437569887,24.5056755288419,24.4332064569847,24.3625363133588,24.2939056737276,24.2244942752344,24.1504483377337,24.0689609127117,23.9809949687108,23.8923659529464,23.8118704978366,23.7486379769066,23.7096530453028,23.6932954014545,23.69171251704,23.6935812831964,23.6862150527458,23.6614648969322,23.6192672299943,23.5640079608332,23.5001878380738,23.5001878380738,24.5115382112119,24.4061494602603,24.3127258742049,24.2391725129687,24.1932795967506,24.1783878510874,24.1897526585213,24.218091683172,24.2554861274748,24.2974715856073,24.3431872223303,24.3948648732079,24.4561869974444,24.5305033131369,24.6181823531799,24.714740171795,24.8119229061473,24.9004298617703,24.9721276769656,25.0260022012316,25.0689903854809,25.1094761139094,25.1550120504152,25.2096035213965,25.2723768019057,25.3392318091639,25.4055345776332,25.4680394374175,25.5268251149033,25.5846973169784,25.645258064821,25.7107097489585,25.7791879121515,25.844063656358,25.8960764210285,25.9259213251828,25.9277197336557,25.9082259350235,25.8796111165228,25.8544463729599,25.8432938525073,25.8522319375779,25.8828552448356,25.9347494443474,26.0078946161903,26.1022121063022,26.2145054935199,26.3421898196488,26.4826358063095,26.6305825122132,26.7760045541708,26.9048097006399,27.0015060900224,27.0515207587684,27.0503626661842,27.0082653263229,26.9378312060977,26.8511288844317,26.7570005752343,26.6594094390082,26.5587713032193,26.4546707796762,26.347769324778,26.2400356254825,26.134810809403,26.0364138510556,25.9481168874476,25.8694221221802,25.794979265741,25.7164568448061,25.6251906578044,25.5145793105714,25.3859757299033,25.2466548177906,25.1073601625143,24.9801988236218,24.8727400004014,24.7844675241031,24.7104158217359,24.6455045352513,23.6341473928121],"text":["X:     0.0000<br />length: 24.13983","X:   665.1899<br />length: 24.28934","X:  1330.3797<br />length: 24.43774","X:  1995.5696<br />length: 24.58388","X:  2660.7595<br />length: 24.72667","X:  3325.9494<br />length: 24.86496","X:  3991.1392<br />length: 24.99763","X:  4656.3291<br />length: 25.12358","X:  5321.5190<br />length: 25.24166","X:  5986.7089<br />length: 25.35077","X:  6651.8987<br />length: 25.45080","X:  7317.0886<br />length: 25.54373","X:  7982.2785<br />length: 25.63183","X:  8647.4684<br />length: 25.71735","X:  9312.6582<br />length: 25.80255","X:  9977.8481<br />length: 25.88969","X: 10643.0380<br />length: 25.98103","X: 11308.2278<br />length: 26.07882","X: 11973.4177<br />length: 26.18521","X: 12638.6076<br />length: 26.29861","X: 13303.7975<br />length: 26.41314","X: 13968.9873<br />length: 26.52268","X: 14634.1772<br />length: 26.62111","X: 15299.3671<br />length: 26.70231","X: 15964.5570<br />length: 26.76017","X: 16629.7468<br />length: 26.78856","X: 17294.9367<br />length: 26.78137","X: 17960.1266<br />length: 26.73321","X: 18625.3165<br />length: 26.64735","X: 19290.5063<br />length: 26.53261","X: 19955.6962<br />length: 26.39791","X: 20620.8861<br />length: 26.25217","X: 21286.0759<br />length: 26.10430","X: 21951.2658<br />length: 25.96323","X: 22616.4557<br />length: 25.83787","X: 23281.6456<br />length: 25.73715","X: 23946.8354<br />length: 25.66822","X: 24612.0253<br />length: 25.62842","X: 25277.2152<br />length: 25.61169","X: 25942.4051<br />length: 25.61194","X: 26607.5949<br />length: 25.62309","X: 27272.7848<br />length: 25.63907","X: 27937.9747<br />length: 25.65379","X: 28603.1646<br />length: 25.66119","X: 29268.3544<br />length: 25.65517","X: 29933.5443<br />length: 25.63174","X: 30598.7342<br />length: 25.59279","X: 31263.9241<br />length: 25.54130","X: 31929.1139<br />length: 25.48024","X: 32594.3038<br />length: 25.41259","X: 33259.4937<br />length: 25.34130","X: 33924.6835<br />length: 25.26937","X: 34589.8734<br />length: 25.19974","X: 35255.0633<br />length: 25.13538","X: 35920.2532<br />length: 25.07743","X: 36585.4430<br />length: 25.02428","X: 37250.6329<br />length: 24.97408","X: 37915.8228<br />length: 24.92499","X: 38581.0127<br />length: 24.87515","X: 39246.2025<br />length: 24.82272","X: 39911.3924<br />length: 24.76584","X: 40576.5823<br />length: 24.70267","X: 41241.7722<br />length: 24.63148","X: 41906.9620<br />length: 24.55291","X: 42572.1519<br />length: 24.46962","X: 43237.3418<br />length: 24.38432","X: 43902.5316<br />length: 24.29973","X: 44567.7215<br />length: 24.21859","X: 45232.9114<br />length: 24.14362","X: 45898.1013<br />length: 24.07753","X: 46563.2911<br />length: 24.02305","X: 47228.4810<br />length: 23.98257","X: 47893.6709<br />length: 23.95569","X: 48558.8608<br />length: 23.94073","X: 49224.0506<br />length: 23.93598","X: 49889.2405<br />length: 23.93975","X: 50554.4304<br />length: 23.95032","X: 51219.6203<br />length: 23.96600","X: 51884.8101<br />length: 23.98508","X: 52550.0000<br />length: 24.00586","X: 52550.0000<br />length: 24.00586","X: 52550.0000<br />length: 24.00586","X: 51884.8101<br />length: 23.98508","X: 51219.6203<br />length: 23.96600","X: 50554.4304<br />length: 23.95032","X: 49889.2405<br />length: 23.93975","X: 49224.0506<br />length: 23.93598","X: 48558.8608<br />length: 23.94073","X: 47893.6709<br />length: 23.95569","X: 47228.4810<br />length: 23.98257","X: 46563.2911<br />length: 24.02305","X: 45898.1013<br />length: 24.07753","X: 45232.9114<br />length: 24.14362","X: 44567.7215<br />length: 24.21859","X: 43902.5316<br />length: 24.29973","X: 43237.3418<br />length: 24.38432","X: 42572.1519<br />length: 24.46962","X: 41906.9620<br />length: 24.55291","X: 41241.7722<br />length: 24.63148","X: 40576.5823<br />length: 24.70267","X: 39911.3924<br />length: 24.76584","X: 39246.2025<br />length: 24.82272","X: 38581.0127<br />length: 24.87515","X: 37915.8228<br />length: 24.92499","X: 37250.6329<br />length: 24.97408","X: 36585.4430<br />length: 25.02428","X: 35920.2532<br />length: 25.07743","X: 35255.0633<br />length: 25.13538","X: 34589.8734<br />length: 25.19974","X: 33924.6835<br />length: 25.26937","X: 33259.4937<br />length: 25.34130","X: 32594.3038<br />length: 25.41259","X: 31929.1139<br />length: 25.48024","X: 31263.9241<br />length: 25.54130","X: 30598.7342<br />length: 25.59279","X: 29933.5443<br />length: 25.63174","X: 29268.3544<br />length: 25.65517","X: 28603.1646<br />length: 25.66119","X: 27937.9747<br />length: 25.65379","X: 27272.7848<br />length: 25.63907","X: 26607.5949<br />length: 25.62309","X: 25942.4051<br />length: 25.61194","X: 25277.2152<br />length: 25.61169","X: 24612.0253<br />length: 25.62842","X: 23946.8354<br />length: 25.66822","X: 23281.6456<br />length: 25.73715","X: 22616.4557<br />length: 25.83787","X: 21951.2658<br />length: 25.96323","X: 21286.0759<br />length: 26.10430","X: 20620.8861<br />length: 26.25217","X: 19955.6962<br />length: 26.39791","X: 19290.5063<br />length: 26.53261","X: 18625.3165<br />length: 26.64735","X: 17960.1266<br />length: 26.73321","X: 17294.9367<br />length: 26.78137","X: 16629.7468<br />length: 26.78856","X: 15964.5570<br />length: 26.76017","X: 15299.3671<br />length: 26.70231","X: 14634.1772<br />length: 26.62111","X: 13968.9873<br />length: 26.52268","X: 13303.7975<br />length: 26.41314","X: 12638.6076<br />length: 26.29861","X: 11973.4177<br />length: 26.18521","X: 11308.2278<br />length: 26.07882","X: 10643.0380<br />length: 25.98103","X:  9977.8481<br />length: 25.88969","X:  9312.6582<br />length: 25.80255","X:  8647.4684<br />length: 25.71735","X:  7982.2785<br />length: 25.63183","X:  7317.0886<br />length: 25.54373","X:  6651.8987<br />length: 25.45080","X:  5986.7089<br />length: 25.35077","X:  5321.5190<br />length: 25.24166","X:  4656.3291<br />length: 25.12358","X:  3991.1392<br />length: 24.99763","X:  3325.9494<br />length: 24.86496","X:  2660.7595<br />length: 24.72667","X:  1995.5696<br />length: 24.58388","X:  1330.3797<br />length: 24.43774","X:   665.1899<br />length: 24.28934","X:     0.0000<br />length: 24.13983","X:     0.0000<br />length: 24.13983"],"type":"scatter","mode":"lines","line":{"width":3.77952755905512,"color":"transparent","dash":"solid"},"fill":"toself","fillcolor":"rgba(153,153,153,0.4)","hoveron":"points","hoverinfo":"x+y","showlegend":false,"xaxis":"x","yaxis":"y","frame":null}],"layout":{"margin":{"t":26.2283105022831,"r":7.30593607305936,"b":40.1826484018265,"l":48.9497716894977},"plot_bgcolor":"rgba(235,235,235,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-2627.5,55177.5],"tickmode":"array","ticktext":["0","10000","20000","30000","40000","50000"],"tickvals":[0,10000,20000,30000,40000,50000],"categoryorder":"array","categoryarray":["0","10000","20000","30000","40000","50000"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":{"text":"X","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[19.475,31.025],"tickmode":"array","ticktext":["20.0","22.5","25.0","27.5","30.0"],"tickvals":[20,22.5,25,27.5,30],"categoryorder":"array","categoryarray":["20.0","22.5","25.0","27.5","30.0"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":{"text":"length","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":false,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","showSendToCloud":false},"source":"A","attrs":{"44c136408013":{"x":{},"y":{},"type":"scatter"},"44c163e4b4cf":{"x":{},"y":{}}},"cur_data":"44c136408013","visdat":{"44c136408013":["function (y) ","x"],"44c163e4b4cf":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

Below are plots presenting histograms for all attributes. Looking at distribution of different values helps us get a better understanding theirs pattern.
It is a standard practice to check distributions of data plotting histograms and measure the distribution skewness, therefore  there is a skewness value printed after each chart.
![](herrings_analysis_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

```
## [1] "Skewness: -0.0994780516138581"
```

![](herrings_analysis_files/figure-html/unnamed-chunk-13-2.png)<!-- -->

```
## [1] "Skewness: 9.02930729716598"
```

![](herrings_analysis_files/figure-html/unnamed-chunk-13-3.png)<!-- -->

```
## [1] "Skewness: 3.42259706724389"
```

![](herrings_analysis_files/figure-html/unnamed-chunk-13-4.png)<!-- -->

```
## [1] "Skewness: 3.23185249965351"
```

![](herrings_analysis_files/figure-html/unnamed-chunk-13-5.png)<!-- -->

```
## [1] "Skewness: 0.472953320955949"
```

![](herrings_analysis_files/figure-html/unnamed-chunk-13-6.png)<!-- -->

```
## [1] "Skewness: 2.4505819779758"
```

![](herrings_analysis_files/figure-html/unnamed-chunk-13-7.png)<!-- -->

```
## [1] "Skewness: 0.869697656065464"
```

![](herrings_analysis_files/figure-html/unnamed-chunk-13-8.png)<!-- -->

```
## [1] "Skewness: 0.444786484569745"
```

![](herrings_analysis_files/figure-html/unnamed-chunk-13-9.png)<!-- -->

```
## [1] "Skewness: 1.04307102789055"
```

![](herrings_analysis_files/figure-html/unnamed-chunk-13-10.png)<!-- -->

```
## [1] "Skewness: -0.0113586261620935"
```

![](herrings_analysis_files/figure-html/unnamed-chunk-13-11.png)<!-- -->

```
## [1] "Skewness: 0.0624454992824206"
```

![](herrings_analysis_files/figure-html/unnamed-chunk-13-12.png)<!-- -->

```
## [1] "Skewness: -0.0488869050329582"
```

![](herrings_analysis_files/figure-html/unnamed-chunk-13-13.png)<!-- -->

```
## [1] "Skewness: -0.537466666995026"
```

```
## Warning in mean.default(x): argument is not numeric or logical: returning
## NA
```

```
## Warning in Ops.factor(x, mean(x)): '-' not meaningful for factors
```

![](herrings_analysis_files/figure-html/unnamed-chunk-13-14.png)<!-- -->

```
## [1] "Skewness: NA"
```

![](herrings_analysis_files/figure-html/unnamed-chunk-13-15.png)<!-- -->

```
## [1] "Skewness: -0.0217822557792181"
```
There are some variables that require closer look. For those that skewness was > 2 a log transform will be used. There could be more analysis done to all of the variables but authors decided that it is enough to further investigate only some of them. Depending on the number of zeros there will be a constant (before a log transform) added or dummy variable introduced that takes value of 1 if the column was zero and 0 otherwise. This way a more suitable distribution can be obtained and important information regarding zeros in the data kept.


```r
number_of_zeros = NROW(imputed_data$cfin1[imputed_data$cfin1== 0])
```

For the variable `cfin1` - log transform was used and a dummy variable introduced. There were  0.2804192% zero values in this column.


```r
  print(paste("Number of zeros: ", number_of_zeros))
```

```
## [1] "Number of zeros:  14745"
```

```r
  imputed_data$cfin1Changed[imputed_data$cfin1 > 0] <- log(imputed_data$cfin1[imputed_data$cfin1 > 0])
  imputed_data$cfin1Changed[imputed_data$cfin1 <= 0] <- 0
  plot <- qplot(imputed_data$cfin1Changed[imputed_data$cfin1 > 0], xlab="cfin1", binwidth=0.05)
  print(plot)
```

![](herrings_analysis_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

```r
  imputed_data$cfin1Zero <- as.numeric(imputed_data$cfin1 == 0)
  print(qplot(imputed_data$cfin1Zero,xlab="cfin1Zero"))
```

![](herrings_analysis_files/figure-html/unnamed-chunk-15-2.png)<!-- -->

```r
number_of_zeros = NROW(imputed_data$cfin2[imputed_data$cfin2== 0])
```

Same technique was used for `cfin2` variable as there were still 0.0746453% records with zero values.

```r
  print(paste("Number of zeros: ", number_of_zeros))
```

```
## [1] "Number of zeros:  3925"
```

```r
  imputed_data$cfin2Changed[imputed_data$cfin2 > 0] <- log(imputed_data$cfin2[imputed_data$cfin2 > 0])
  imputed_data$cfin2Changed[imputed_data$cfin2 <= 0] <- 0
  plot <- qplot(imputed_data$cfin2Changed[imputed_data$cfin2 > 0], xlab="cfin2", binwidth=0.05)
  print(plot)
```

![](herrings_analysis_files/figure-html/unnamed-chunk-17-1.png)<!-- -->

```r
  imputed_data$cfin2Zero <- as.numeric(imputed_data$cfin2 == 0)
  print(qplot(imputed_data$cfin2Zero,xlab="cfin2Zero"))
```

![](herrings_analysis_files/figure-html/unnamed-chunk-17-2.png)<!-- -->

```r
number_of_zeros = NROW(imputed_data$chel1[imputed_data$chel1== 0])
```

For `chel1` there were 0.0365905% zero values (<5%) that is why log transform with an added constant was preferred. The 5% value is not a rule but rather a threshold authors decided on. More appropriate solution could be to test all possible variable setups.

```r
  print(paste("Number of zeros: ", number_of_zeros))
```

```
## [1] "Number of zeros:  1924"
```

```r
  imputed_data$chel1Changed <- log(imputed_data$chel1 +1)
  
  plot <- qplot(imputed_data$chel1Changed, xlab="chel1", binwidth=0.05)
  print(plot)
```

![](herrings_analysis_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

```r
number_of_zeros = NROW(imputed_data$lcop1[imputed_data$lcop1== 0])
```

Attribute `lcop1` was only log transformed with an added constant. There were 0% zero values!


```r
  print(paste("Number of zeros: ", number_of_zeros))
```

```
## [1] "Number of zeros:  0"
```

```r
  imputed_data$lcop1Changed <- log(imputed_data$lcop1 +1)
  plot <- qplot(imputed_data$lcop1Changed, xlab="lcop1", binwidth=0.05)
  print(plot)
```

![](herrings_analysis_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

## Regression
First we got rid of index `X` column as it should not participate in regression.

```r
without_X <- select(imputed_data, -c(X))
```

We had to turn `xmonth` into a dummy variable. Now there are new columns for each month. Zero indicates that sample was not taken in this month and one if it was taken in month which is specified by the name of the column.

```r
dummies <- dummyVars('~.', data=without_X)
reg_data <- predict(dummies, newdata=without_X)
head(reg_data) %>%
  kable("html") %>%
  kable_styling(bootstrap_options = c('striped', 'hover')) %>%
  scroll_box(width = '100%')
```

<div style="border: 1px solid #ddd; padding: 5px; overflow-x: scroll; width:100%; "><table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:right;"> length </th>
   <th style="text-align:right;"> cfin1 </th>
   <th style="text-align:right;"> cfin2 </th>
   <th style="text-align:right;"> chel1 </th>
   <th style="text-align:right;"> chel2 </th>
   <th style="text-align:right;"> lcop1 </th>
   <th style="text-align:right;"> lcop2 </th>
   <th style="text-align:right;"> fbar </th>
   <th style="text-align:right;"> recr </th>
   <th style="text-align:right;"> cumf </th>
   <th style="text-align:right;"> totaln </th>
   <th style="text-align:right;"> sst </th>
   <th style="text-align:right;"> sal </th>
   <th style="text-align:right;"> xmonth.1 </th>
   <th style="text-align:right;"> xmonth.2 </th>
   <th style="text-align:right;"> xmonth.3 </th>
   <th style="text-align:right;"> xmonth.4 </th>
   <th style="text-align:right;"> xmonth.5 </th>
   <th style="text-align:right;"> xmonth.6 </th>
   <th style="text-align:right;"> xmonth.7 </th>
   <th style="text-align:right;"> xmonth.8 </th>
   <th style="text-align:right;"> xmonth.9 </th>
   <th style="text-align:right;"> xmonth.10 </th>
   <th style="text-align:right;"> xmonth.11 </th>
   <th style="text-align:right;"> xmonth.12 </th>
   <th style="text-align:right;"> nao </th>
   <th style="text-align:right;"> cfin1Changed </th>
   <th style="text-align:right;"> cfin1Zero </th>
   <th style="text-align:right;"> cfin2Changed </th>
   <th style="text-align:right;"> cfin2Zero </th>
   <th style="text-align:right;"> chel1Changed </th>
   <th style="text-align:right;"> lcop1Changed </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 23.0 </td>
   <td style="text-align:right;"> 0.02778 </td>
   <td style="text-align:right;"> 0.27785 </td>
   <td style="text-align:right;"> 2.46875 </td>
   <td style="text-align:right;"> 21.43548 </td>
   <td style="text-align:right;"> 2.54787 </td>
   <td style="text-align:right;"> 26.35881 </td>
   <td style="text-align:right;"> 0.356 </td>
   <td style="text-align:right;"> 482831 </td>
   <td style="text-align:right;"> 0.3059879 </td>
   <td style="text-align:right;"> 267380.8 </td>
   <td style="text-align:right;"> 14.30693 </td>
   <td style="text-align:right;"> 35.51234 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 2.8 </td>
   <td style="text-align:right;"> -3.583439 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> -1.280674 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1.243794 </td>
   <td style="text-align:right;"> 1.266347 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 22.5 </td>
   <td style="text-align:right;"> 0.02778 </td>
   <td style="text-align:right;"> 0.27785 </td>
   <td style="text-align:right;"> 2.46875 </td>
   <td style="text-align:right;"> 21.43548 </td>
   <td style="text-align:right;"> 2.54787 </td>
   <td style="text-align:right;"> 26.35881 </td>
   <td style="text-align:right;"> 0.356 </td>
   <td style="text-align:right;"> 482831 </td>
   <td style="text-align:right;"> 0.3059879 </td>
   <td style="text-align:right;"> 267380.8 </td>
   <td style="text-align:right;"> 14.30693 </td>
   <td style="text-align:right;"> 35.51234 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 2.8 </td>
   <td style="text-align:right;"> -3.583439 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> -1.280674 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1.243794 </td>
   <td style="text-align:right;"> 1.266347 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 25.0 </td>
   <td style="text-align:right;"> 0.02778 </td>
   <td style="text-align:right;"> 0.27785 </td>
   <td style="text-align:right;"> 2.46875 </td>
   <td style="text-align:right;"> 21.43548 </td>
   <td style="text-align:right;"> 2.54787 </td>
   <td style="text-align:right;"> 26.35881 </td>
   <td style="text-align:right;"> 0.356 </td>
   <td style="text-align:right;"> 482831 </td>
   <td style="text-align:right;"> 0.3059879 </td>
   <td style="text-align:right;"> 267380.8 </td>
   <td style="text-align:right;"> 14.30693 </td>
   <td style="text-align:right;"> 35.51234 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 2.8 </td>
   <td style="text-align:right;"> -3.583439 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> -1.280674 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1.243794 </td>
   <td style="text-align:right;"> 1.266347 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 25.5 </td>
   <td style="text-align:right;"> 0.02778 </td>
   <td style="text-align:right;"> 0.27785 </td>
   <td style="text-align:right;"> 2.46875 </td>
   <td style="text-align:right;"> 21.43548 </td>
   <td style="text-align:right;"> 2.54787 </td>
   <td style="text-align:right;"> 26.35881 </td>
   <td style="text-align:right;"> 0.356 </td>
   <td style="text-align:right;"> 482831 </td>
   <td style="text-align:right;"> 0.3059879 </td>
   <td style="text-align:right;"> 267380.8 </td>
   <td style="text-align:right;"> 14.30693 </td>
   <td style="text-align:right;"> 35.51234 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 2.8 </td>
   <td style="text-align:right;"> -3.583439 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> -1.280674 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1.243794 </td>
   <td style="text-align:right;"> 1.266347 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 24.0 </td>
   <td style="text-align:right;"> 0.02778 </td>
   <td style="text-align:right;"> 0.27785 </td>
   <td style="text-align:right;"> 2.46875 </td>
   <td style="text-align:right;"> 21.43548 </td>
   <td style="text-align:right;"> 2.54787 </td>
   <td style="text-align:right;"> 26.35881 </td>
   <td style="text-align:right;"> 0.356 </td>
   <td style="text-align:right;"> 482831 </td>
   <td style="text-align:right;"> 0.3059879 </td>
   <td style="text-align:right;"> 267380.8 </td>
   <td style="text-align:right;"> 14.30693 </td>
   <td style="text-align:right;"> 35.51234 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 2.8 </td>
   <td style="text-align:right;"> -3.583439 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> -1.280674 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1.243794 </td>
   <td style="text-align:right;"> 1.266347 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 22.0 </td>
   <td style="text-align:right;"> 0.02778 </td>
   <td style="text-align:right;"> 0.27785 </td>
   <td style="text-align:right;"> 2.46875 </td>
   <td style="text-align:right;"> 21.43548 </td>
   <td style="text-align:right;"> 2.54787 </td>
   <td style="text-align:right;"> 26.35881 </td>
   <td style="text-align:right;"> 0.356 </td>
   <td style="text-align:right;"> 482831 </td>
   <td style="text-align:right;"> 0.3059879 </td>
   <td style="text-align:right;"> 267380.8 </td>
   <td style="text-align:right;"> 14.30693 </td>
   <td style="text-align:right;"> 35.51234 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 2.8 </td>
   <td style="text-align:right;"> -3.583439 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> -1.280674 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1.243794 </td>
   <td style="text-align:right;"> 1.266347 </td>
  </tr>
</tbody>
</table></div>

Some attributes in this dataset have values measured in tens of thousands while others never exceeds a value of one. If our data will stay in that form some attributes will contribute greatly to the final result while others will be ignored. To make sure that all data equaly participates in learning process it has to be normalized in one way or another. We decided to use simple scale and centering methods from `caret` library. But before that data had to be divided into test and training set. Fitting normalization model can be done just on training dataset, as it would introduce data leak if conducted on the the whole dataset.

Linear model was our first choice. Model was evaluated in 8-fold cross-validation. Parameters for preprocessing were set to centering and scaling to make sure that each training set will receive its own normalization process.

```r
ctrl <- trainControl(method = "cv", number=8, allowParallel = TRUE)
lm <- train(length~., data = reg_data, method='lm', trControl=ctrl, preProcess=c('center', 'scale'))
```

```
## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient
## fit may be misleading

## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient
## fit may be misleading

## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient
## fit may be misleading

## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient
## fit may be misleading

## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient
## fit may be misleading

## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient
## fit may be misleading

## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient
## fit may be misleading

## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient
## fit may be misleading
```

```r
lm
```

```
## Linear Regression 
## 
## 52582 samples
##    31 predictor
## 
## Pre-processing: centered (31), scaled (31) 
## Resampling: Cross-Validated (8 fold) 
## Summary of sample sizes: 46010, 46009, 46009, 46010, 46009, 46008, ... 
## Resampling results:
## 
##   RMSE      Rsquared  MAE     
##   1.322786  0.359529  1.053329
## 
## Tuning parameter 'intercept' was held constant at a value of TRUE
```

Second model we've used was `eXtreme Gradient Boosting Tree`. Evaluating was done with 8-fold cross-validation. For this model we prepared grid search object which contained multiple values for `max_depth`, `eta`, `gamma`, `min_child_weight`. Normalization was set to centering and scaling, separately for each coross-validation subprocess.

```r
registerDoMC(cores=4)
ctrl <- trainControl(method = "cv", number=8, allowParallel = TRUE)
grid <- expand.grid(max_depth=c(3,4,5), 
                    subsample=c(1.0), 
                    nrounds=c(200),
                    eta=c(0.4, 0.3),
                    gamma=c(0.001,0.01),
                    colsample_bytree=c(0.7),
                    min_child_weight=c(1.0, 0.5))
xgb_tree <- train(length~., data = reg_data, method='xgbTree', trControl=ctrl, tuneGrid=grid, preProcess=c('center', 'scale'))
xgb_tree
```

```
## eXtreme Gradient Boosting 
## 
## 52582 samples
##    31 predictor
## 
## Pre-processing: centered (31), scaled (31) 
## Resampling: Cross-Validated (8 fold) 
## Summary of sample sizes: 46010, 46010, 46010, 46009, 46009, 46008, ... 
## Resampling results across tuning parameters:
## 
##   eta  max_depth  gamma  min_child_weight  RMSE      Rsquared   MAE      
##   0.3  3          0.001  0.5               1.142606  0.5222043  0.9000105
##   0.3  3          0.001  1.0               1.143801  0.5211980  0.9006797
##   0.3  3          0.010  0.5               1.143538  0.5214250  0.9007151
##   0.3  3          0.010  1.0               1.143493  0.5214626  0.9008763
##   0.3  4          0.001  0.5               1.138078  0.5259746  0.8954577
##   0.3  4          0.001  1.0               1.138733  0.5254293  0.8959060
##   0.3  4          0.010  0.5               1.139218  0.5250301  0.8963995
##   0.3  4          0.010  1.0               1.138488  0.5256332  0.8959008
##   0.3  5          0.001  0.5               1.138685  0.5255263  0.8951759
##   0.3  5          0.001  1.0               1.139362  0.5249709  0.8957114
##   0.3  5          0.010  0.5               1.139476  0.5248716  0.8960762
##   0.3  5          0.010  1.0               1.138440  0.5257401  0.8950917
##   0.4  3          0.001  0.5               1.141872  0.5227997  0.8988409
##   0.4  3          0.001  1.0               1.141638  0.5229918  0.8989751
##   0.4  3          0.010  0.5               1.142353  0.5223955  0.8995959
##   0.4  3          0.010  1.0               1.141506  0.5231008  0.8987390
##   0.4  4          0.001  0.5               1.140190  0.5242519  0.8965994
##   0.4  4          0.001  1.0               1.139358  0.5249498  0.8959610
##   0.4  4          0.010  0.5               1.138816  0.5254000  0.8960323
##   0.4  4          0.010  1.0               1.139443  0.5248751  0.8964658
##   0.4  5          0.001  0.5               1.141056  0.5236717  0.8966840
##   0.4  5          0.001  1.0               1.141288  0.5234624  0.8968843
##   0.4  5          0.010  0.5               1.140992  0.5237016  0.8965652
##   0.4  5          0.010  1.0               1.141019  0.5236809  0.8970745
## 
## Tuning parameter 'nrounds' was held constant at a value of 200
## 
## Tuning parameter 'colsample_bytree' was held constant at a value of
##  0.7
## Tuning parameter 'subsample' was held constant at a value of 1
## RMSE was used to select the optimal model using the smallest value.
## The final values used for the model were nrounds = 200, max_depth = 4,
##  eta = 0.3, gamma = 0.001, colsample_bytree = 0.7, min_child_weight =
##  0.5 and subsample = 1.
```

Expectedly, `eXtreme Gradient Boosting Tree` turned out to be superior model. And as such was used for next step, that is, checking for feature importance in regressing model. Following chart presents importance of each attribute. 

```r
imp <- varImp(xgb_tree, scale=FALSE)
plot(imp)
```

![](herrings_analysis_files/figure-html/unnamed-chunk-26-1.png)<!-- -->

The most important attributes can be found at the top. As expected after analysis of correlation matrix sea surface temperature was the most prominent factor contributing to this regression. 
