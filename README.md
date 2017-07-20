# MSDR Capstone - NOAA Earthquake Visualizations
Yaakov Miller  
`r Sys.Date()`  

[![Travis-CI Build Status](https://travis-ci.org/ykv001/ykvdpcap.svg?branch=master)](https://travis-ci.org/ykv001/ykvdpcap)


[![AppVeyor Build status](https://ci.appveyor.com/api/projects/status/4e2o517rh8ymnt9k?svg=true)](https://ci.appveyor.com/project/ykv001/ykvdpcap)

For now docs and testing more to follow...

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
knitr::kable(head(eq_data[,1:7]))
```



  I_D  FLAG_TSUNAMI     YEAR   MONTH   DAY   HOUR   MINUTE
-----  -------------  ------  ------  ----  -----  -------
    1  NA              -2150      NA    NA     NA       NA
    3  NA              -2000      NA    NA     NA       NA
    2  Tsu             -2000      NA    NA     NA       NA
 5877  Tsu             -1610      NA    NA     NA       NA
    8  NA              -1566      NA    NA     NA       NA
   11  NA              -1450      NA    NA     NA       NA

```r
eq_clean <- eq_data %>% eq_clean_data()
knitr::kable(head(eq_clean[,1:7]))
```



 I_D  FLAG_TSUNAMI   DATE          HOUR   MINUTE  SECOND    FOCAL_DEPTH
----  -------------  -----------  -----  -------  -------  ------------
  38  NA             0010-01-01      NA       NA                     18
  39  NA             0011-01-01      NA       NA                     NA
  40  NA             0017-01-01      NA       NA  NA                 NA
  41  NA             0023-01-01      NA       NA                     NA
  42  NA             0025-01-01      NA       NA                     NA
  43  NA             0027-01-01      NA       NA                     NA

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

<!--html_preserve--><div id="htmlwidget-d35d08251a1f135ca068" style="width:672px;height:480px;" class="leaflet html-widget"></div>
<script type="application/json" data-for="htmlwidget-d35d08251a1f135ca068">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addTiles","args":["//{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"maxNativeZoom":null,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"continuousWorld":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":null,"unloadInvisibleTiles":null,"updateWhenIdle":null,"detectRetina":false,"reuseTiles":false,"attribution":"&copy; <a href=\"http://openstreetmap.org\">OpenStreetMap<\/a> contributors, <a href=\"http://creativecommons.org/licenses/by-sa/2.0/\">CC-BY-SA<\/a>"}]},{"method":"addCircleMarkers","args":[[-16.265,-16.086,-17.543,-14.258,-5.678,-16.635,-5.852,-13.457,-13.386,-12.178,-10.368,-15.986,-11.779,-18.058,-14.438,-14.168,-15.748,-15.851,-13.814,-10.047,-10.548,-15.613,-15.294],[-73.641,-73.987,-72.077,-72.683,-76.398,-70.794,-78.704,-76.677,-76.603,-77.164,-75.512,-71.748,-75.626,-70.547,-75.966,-75.635,-71.425,-74.562,-71.744,-71.023,-70.904,-71.671,-70.823],[8.4,6.6,7.6,5.8,7.5,5.3,5.4,6.7,8,5.3,5.5,6.2,4.6,6.2,6.9,6.4,5.3,7,4.9,7.6,7.6,5.4,6.2],null,null,{"lineCap":null,"lineJoin":null,"clickable":true,"pointerEvents":null,"className":"","stroke":true,"color":"#03F","weight":1,"opacity":0.5,"fill":true,"fillColor":"#03F","fillOpacity":0.2,"dashArray":null},null,null,["2001-06-23","2001-07-05","2001-07-07","2001-08-09","2005-09-26","2005-10-01","2005-10-31","2006-10-20","2007-08-15","2008-03-29","2008-07-01","2008-07-08","2009-01-21","2010-05-06","2011-10-28","2012-01-30","2013-02-22","2013-09-25","2014-09-27","2015-11-24","2015-11-24","2016-08-15","2016-12-01"],null,null,null,null]}],"limits":{"lat":[-18.058,-5.678],"lng":[-78.704,-70.547]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->


### Interactive Map with automated popup text


```r
eq_clean %>%
dplyr::filter(COUNTRY == "ITALY" & lubridate::year(DATE) >= 2000) %>%
dplyr::mutate(popup_text = eq_create_label(.)) %>%
eq_map(annot_col = "popup_text")
```

<!--html_preserve--><div id="htmlwidget-cd9e3f4adc960e25dae0" style="width:672px;height:480px;" class="leaflet html-widget"></div>
<script type="application/json" data-for="htmlwidget-cd9e3f4adc960e25dae0">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addTiles","args":["//{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"maxNativeZoom":null,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"continuousWorld":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":null,"unloadInvisibleTiles":null,"updateWhenIdle":null,"detectRetina":false,"reuseTiles":false,"attribution":"&copy; <a href=\"http://openstreetmap.org\">OpenStreetMap<\/a> contributors, <a href=\"http://creativecommons.org/licenses/by-sa/2.0/\">CC-BY-SA<\/a>"}]},{"method":"addCircleMarkers","args":[[38.381,37.67,41.789,43.883,44.329,45.626,42.334,42.275,44.89,44.831,44.851,42.714,42.934,42.855,42.601],[13.701,15.267,14.872,11.96,11.45,10.559,13.334,13.464,11.23,11.49,11.086,13.172,13.043,13.088,13.227],[6,4.3,5.7,4.7,5.3,5.1,6.3,5.5,6.1,5.1,5.9,6.2,6.6,6.6,5.7],null,null,{"lineCap":null,"lineJoin":null,"clickable":true,"pointerEvents":null,"className":"","stroke":true,"color":"#03F","weight":1,"opacity":0.5,"fill":true,"fillColor":"#03F","fillOpacity":0.2,"dashArray":null},null,null,["<b>Location:<\/b> Palermo<br /><b>Magnitude:<\/b> 6<br /><b>Total Deaths:<\/b> 2","<b>Location:<\/b> Santa Venerina<br /><b>Magnitude:<\/b> 4.3<br />","<b>Location:<\/b> San Guiliano Di Puglia, Campobasso<br /><b>Magnitude:<\/b> 5.7<br /><b>Total Deaths:<\/b> 29","<b>Location:<\/b> Spinello, Santa Sofia, Bagno Di Romagna<br /><b>Magnitude:<\/b> 4.7<br />","<b>Location:<\/b> Loiano, Monghidoro, Monzuno<br /><b>Magnitude:<\/b> 5.3<br />","<b>Location:<\/b> Brescia<br /><b>Magnitude:<\/b> 5.1<br />","<b>Location:<\/b> L'aquila<br /><b>Magnitude:<\/b> 6.3<br /><b>Total Deaths:<\/b> 309","<b>Location:<\/b> L'aquila<br /><b>Magnitude:<\/b> 5.5<br /><b>Total Deaths:<\/b> 1","<b>Location:<\/b> Mirandola<br /><b>Magnitude:<\/b> 6.1<br /><b>Total Deaths:<\/b> 7","<b>Location:<\/b> Sant'agostino<br /><b>Magnitude:<\/b> 5.1<br />","<b>Location:<\/b> Medolla, Mirandola, Cavezzo<br /><b>Magnitude:<\/b> 5.9<br /><b>Total Deaths:<\/b> 17","<b>Location:<\/b> Accumoli, Arquata, Amatrice<br /><b>Magnitude:<\/b> 6.2<br /><b>Total Deaths:<\/b> 299","<b>Location:<\/b> Visso, Ussita<br /><b>Magnitude:<\/b> 6.6<br /><b>Total Deaths:<\/b> 1","<b>Location:<\/b> Norcia<br /><b>Magnitude:<\/b> 6.6<br />","<b>Location:<\/b> Farindola<br /><b>Magnitude:<\/b> 5.7<br /><b>Total Deaths:<\/b> 29"],null,null,null,null]}],"limits":{"lat":[37.67,45.626],"lng":[10.559,15.267]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->
