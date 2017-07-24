# Mastering Software Development in R Capstone - NOAA Earthquake Visualizations
Yaakov Miller  
7/20/17  

[![Travis-CI Build Status](https://travis-ci.org/ykv001/ykvdpcap.svg?branch=master)](https://travis-ci.org/ykv001/ykvdpcap)


[![AppVeyor Build status](https://ci.appveyor.com/api/projects/status/4e2o517rh8ymnt9k?svg=true)](https://ci.appveyor.com/project/ykv001/ykvdpcap)



## Installation

Use the devtools package:


```r
library(devtools)
install_github("ykv001/ykvdpcap")
library(ykvdpcap)
```

## Functions

This package includes the following functions

* eq_clean_data
* eq_create_label
* eq_data_read
* eq_location_clean
* eq_map
* eq_time
* geom_timeline
* geom_timeline_label

## Usage

### Data loading and cleaning


```r
library(ggplot2)
library(leaflet)
library(lubridate)
library(dplyr)
library(readr)
library(stringr)

library(ykvdpcap)
data<-system.file('extdata','data.gz', package = 'ykvdpcap')

eq_data <- eq_data_read(data)
head(eq_data[,1:7])
```

```
## # A tibble: 6 x 7
##     I_D FLAG_TSUNAMI  YEAR MONTH   DAY  HOUR MINUTE
##   <int>        <chr> <int> <int> <int> <int>  <int>
## 1     1         <NA> -2150    NA    NA    NA     NA
## 2     3         <NA> -2000    NA    NA    NA     NA
## 3     2          Tsu -2000    NA    NA    NA     NA
## 4  5877          Tsu -1610    NA    NA    NA     NA
## 5     8         <NA> -1566    NA    NA    NA     NA
## 6    11         <NA> -1450    NA    NA    NA     NA
```

```r
eq_clean <- eq_data %>% eq_clean_data()
head(eq_clean[,1:7])
```

```
## # A tibble: 6 x 7
##     I_D FLAG_TSUNAMI       DATE  HOUR MINUTE SECOND FOCAL_DEPTH
##   <int>        <chr>     <date> <int>  <int>  <chr>       <int>
## 1    38         <NA> 0010-01-01    NA     NA                 18
## 2    39         <NA> 0011-01-01    NA     NA                 NA
## 3    40         <NA> 0017-01-01    NA     NA   <NA>          NA
## 4    41         <NA> 0023-01-01    NA     NA                 NA
## 5    42         <NA> 0025-01-01    NA     NA                 NA
## 6    43         <NA> 0027-01-01    NA     NA                 NA
```

### Time Line


```r
eq_clean %>%
dplyr::filter(COUNTRY == "USA" & lubridate::year(DATE) >= 2000) %>%
eq_time(size="EQ_PRIMARY",color="DEATHS")
```

![](README_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

### With Location Names

```r
eq_clean %>%
dplyr::filter(COUNTRY == "USA" & lubridate::year(DATE) >= 2000) %>%
eq_time(size="EQ_PRIMARY",color="DEATHS",alpha=0.5,timeline_label=TRUE)
```

![](README_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

### 2 Countries


```r
eq_clean %>%
dplyr::filter((COUNTRY=="USA" | COUNTRY=="MEXICO") & lubridate::year(DATE) >= 2000) %>%
eq_time(y="COUNTRY",color="DEATHS",alpha=0.5)
```

![](README_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

### 2 Countries With Location Names


```r
eq_clean %>%
dplyr::filter((COUNTRY=="USA" | COUNTRY=="CHILE") & lubridate::year(DATE) >= 2000) %>%
eq_time(y="COUNTRY",color="DEATHS",alpha=0.5,timeline_label=TRUE)
```

![](README_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

### Interactive Map


```r
eq_clean %>%
dplyr::filter(COUNTRY == "PERU" & lubridate::year(DATE) >= 2000) %>%
eq_map(annot_col = "DATE")
```

![](README_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

### Interactive Map with automated popup text


```r
eq_clean %>%
dplyr::filter(COUNTRY == "ITALY" & lubridate::year(DATE) >= 2000) %>%
dplyr::mutate(popup_text = eq_create_label(.)) %>%
eq_map(annot_col = "popup_text")
```

![](README_files/figure-html/unnamed-chunk-9-1.png)<!-- -->
