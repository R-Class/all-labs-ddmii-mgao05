Lab05
================
Mengran Gao

``` r
knitr::opts_chunk$set( message = F, warning = F)

library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(RCurl)
```

    ## Warning: package 'RCurl' was built under R version 3.3.2

    ## Loading required package: bitops

``` r
library(ggmap)
```

    ## Warning: package 'ggmap' was built under R version 3.3.2

    ## Loading required package: ggplot2

    ## Warning: package 'ggplot2' was built under R version 3.3.2

``` r
library(jsonlite)
```

    ## Warning: package 'jsonlite' was built under R version 3.3.2

``` r
library(censusapi)
```

    ## 
    ## Attaching package: 'censusapi'

    ## The following object is masked from 'package:methods':
    ## 
    ##     getFunction

``` r
library(sp)
```

    ## Warning: package 'sp' was built under R version 3.3.2

``` r
library(maps)
```

    ## Warning: package 'maps' was built under R version 3.3.2

``` r
library(maptools)
```

    ## Warning: package 'maptools' was built under R version 3.3.2

    ## Checking rgeos availability: FALSE
    ##      Note: when rgeos is not available, polygon geometry     computations in maptools depend on gpclib,
    ##      which has a restricted licence. It is disabled by default;
    ##      to enable gpclib, type gpclibPermit()

``` r
library(memisc)
```

    ## Warning: package 'memisc' was built under R version 3.3.2

    ## Loading required package: lattice

    ## Loading required package: MASS

    ## 
    ## Attaching package: 'MASS'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

    ## 
    ## Attaching package: 'memisc'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     collect, query, recode, rename

    ## The following objects are masked from 'package:stats':
    ## 
    ##     contr.sum, contr.treatment, contrasts

    ## The following object is masked from 'package:base':
    ## 
    ##     as.array

load and create a dataset of home prices and assets collected from Zillow
=========================================================================

``` r
#read in housing data from Zillow


zillow.url <- "https://raw.githubusercontent.com/lecy/hedonic-prices/master/Data/Housing%20Price%20In-Class%20Exercise%20(Responses).csv"
zillow.dat <- read.csv(zillow.url, stringsAsFactors=FALSE )

#subset for Street Addresses and Zip Codes
str.zip <- zillow.dat[ , c("Street.Address.of.House","Zip.Code") ]

#clean data, get rid of symbols that are not used in matching dataset
str.zip$Street.Address.of.House <- gsub( ",", "", str.zip$Street.Address.of.House )
str.zip$Street.Address.of.House <- gsub( "\\.", "", str.zip$Street.Address.of.House )

#create address adding Syracuse, NY 
address <- paste( str.zip$Street.Address.of.House, "Syracuse, NY", str.zip$zip, sep=", " )
head(address)
```

    ## [1] "504 Winkworth Pkwy, Syracuse, NY, "
    ## [2] "136 Austin Ave, Syracuse, NY, "    
    ## [3] "701 Velasko Rd, Syracuse, NY, "    
    ## [4] "518 Wolcott Ave, Syracuse, NY, "   
    ## [5] "112 Wolcott Ave, Syracuse, NY, "   
    ## [6] "212 Roberts Ave, Syracuse, NY, "

``` r
#Geocode addresses

setwd("C:/Users/mgao05/Desktop/PAI INdenpendent study/lab5")

#create file with lat.lon using geocode function
#lat.long <- geocode( address )
#write.csv(lat.long, file = "lat.lon.csv")

lat.long <- read.csv("lat.lon.csv")
#reorder lon.lat
lat.long <- lat.long[ c("lon","lat")]
#use cbind to bind str.zip and lat.long
houses <- cbind(str.zip, lat.long)

head(houses)
```

    ##   Street.Address.of.House Zip.Code       lon      lat
    ## 1      504 Winkworth Pkwy    13219 -76.19918 43.02561
    ## 2          136 Austin Ave    13207 -76.18848 43.02894
    ## 3          701 Velasko Rd    13207 -76.18540 43.02957
    ## 4         518 Wolcott Ave    13207 -76.18301 43.02757
    ## 5         112 Wolcott Ave    13207 -76.18324 43.03210
    ## 6         212 Roberts Ave    13207 -76.17196 43.03111

Add census tracts FIPS ID to house and spatial join to census tract data
========================================================================

``` r
#read in onondoga shapefiles

setwd( "C:/Users/mgao05/Desktop/PAI INdenpendent study/lab5/shapefiles")

onondoga <- readShapePoly( fn="tl_2010_36067_tract10", proj4string=CRS("+proj=longlat +datum=WGS84") )

#turn lat.long data into spatial points and use over function to match it with onongoda
lat.long <- SpatialPoints(lat.long, proj4string=CRS("+proj=longlat +datum=WGS84") )
poly.match.point <- over(lat.long, onondoga )

#use cbind to combine dataframes
houses.dat <- cbind( houses, poly.match.point )

head(houses.dat)
```

    ##   Street.Address.of.House Zip.Code       lon      lat STATEFP10 COUNTYFP10
    ## 1      504 Winkworth Pkwy    13219 -76.19918 43.02561        36        067
    ## 2          136 Austin Ave    13207 -76.18848 43.02894        36        067
    ## 3          701 Velasko Rd    13207 -76.18540 43.02957        36        067
    ## 4         518 Wolcott Ave    13207 -76.18301 43.02757        36        067
    ## 5         112 Wolcott Ave    13207 -76.18324 43.03210        36        067
    ## 6         212 Roberts Ave    13207 -76.17196 43.03111        36        067
    ##   TRACTCE10     GEOID10 NAME10      NAMELSAD10 MTFCC10 FUNCSTAT10 ALAND10
    ## 1    004800 36067004800     48 Census Tract 48   G5020          S 1206079
    ## 2    004800 36067004800     48 Census Tract 48   G5020          S 1206079
    ## 3    004800 36067004800     48 Census Tract 48   G5020          S 1206079
    ## 4    004800 36067004800     48 Census Tract 48   G5020          S 1206079
    ## 5    004800 36067004800     48 Census Tract 48   G5020          S 1206079
    ## 6    005000 36067005000     50 Census Tract 50   G5020          S 1149255
    ##   AWATER10  INTPTLAT10   INTPTLON10
    ## 1        0 +43.0279451 -076.1920648
    ## 2        0 +43.0279451 -076.1920648
    ## 3        0 +43.0279451 -076.1920648
    ## 4        0 +43.0279451 -076.1920648
    ## 5        0 +43.0279451 -076.1920648
    ## 6    45948 +43.0272963 -076.1689815

add census data to each home
============================

``` r
censuskey <- "4a9f4388ad997978054658eeafcb1febfb33c535"
### median age: B01002A_001E, acs Total Population:B01001_001E, acs, Single Mothers Below Poverty Level B17010_017E acs, 

#choose single mothers below poverty level and total population as variables
myvars <- c("NAME", "B01001_001E", "B17010_017E")

#download census data for selected variables
acs5_2015 <- getCensus(name="acs5", vintage=2015, key=censuskey
                       ,vars=myvars
                       , region="tract:*"
                       , regionin = "state: 36 + county: 067")

#create a new variable of percentage single mother below poverty level in population

acs5_2015 <- plyr::rename(acs5_2015, c("B17010_017E"="SingleMom.below.poverty", "B01001_001E"="Total.Population"))
acs5_2015 <- mutate(acs5_2015, single.below.percent = SingleMom.below.poverty/Total.Population)


#merge housing dataframe from above with census data by CENSUS TRACT
housing.census.dat <- merge(x=houses.dat, y=acs5_2015, by.x="TRACTCE10", by.y="tract",all.x=T)

head(housing.census.dat)
```

    ##   TRACTCE10 Street.Address.of.House Zip.Code       lon      lat STATEFP10
    ## 1    000100          139 Pulaski St    13204 -76.17168 43.05846        36
    ## 2    000200             709 Wolf St    13208 -76.16095 43.07638        36
    ## 3    000200        1006 Wolf Street    13208 -76.15762 43.07961        36
    ## 4    000200        619 2nd N Street    13208 -76.16063 43.07537        36
    ## 5    000200            609 2nd N St    13208 -76.16025 43.07522        36
    ## 6    000300         308 Cadillac St    13208 -76.14986 43.07923        36
    ##   COUNTYFP10     GEOID10 NAME10     NAMELSAD10 MTFCC10 FUNCSTAT10 ALAND10
    ## 1        067 36067000100      1 Census Tract 1   G5020          S 4842958
    ## 2        067 36067000200      2 Census Tract 2   G5020          S 1095299
    ## 3        067 36067000200      2 Census Tract 2   G5020          S 1095299
    ## 4        067 36067000200      2 Census Tract 2   G5020          S 1095299
    ## 5        067 36067000200      2 Census Tract 2   G5020          S 1095299
    ## 6        067 36067000300      3 Census Tract 3   G5020          S  574912
    ##   AWATER10  INTPTLAT10   INTPTLON10
    ## 1  1284980 +43.0691355 -076.1730170
    ## 2        0 +43.0747759 -076.1583997
    ## 3        0 +43.0747759 -076.1583997
    ## 4        0 +43.0747759 -076.1583997
    ## 5        0 +43.0747759 -076.1583997
    ## 6        0 +43.0812047 -076.1483599
    ##                                        NAME state county Total.Population
    ## 1 Census Tract 1, Onondaga County, New York    36    067              644
    ## 2 Census Tract 2, Onondaga County, New York    36    067             3042
    ## 3 Census Tract 2, Onondaga County, New York    36    067             3042
    ## 4 Census Tract 2, Onondaga County, New York    36    067             3042
    ## 5 Census Tract 2, Onondaga County, New York    36    067             3042
    ## 6 Census Tract 3, Onondaga County, New York    36    067             1635
    ##   SingleMom.below.poverty single.below.percent
    ## 1                       0           0.00000000
    ## 2                     194           0.06377383
    ## 3                     194           0.06377383
    ## 4                     194           0.06377383
    ## 5                     194           0.06377383
    ## 6                      24           0.01467890

spatial join for crime data
===========================

``` r
#load crime data
crime.dat <- read.csv("https://raw.githubusercontent.com/lecy/hedonic-prices/master/Data/crime.lat.lon.csv", stringsAsFactors=FALSE )

#subset long.lat data
lat.long.coords <- crime.dat[ ,c("lon","lat")]  

#overlay lat.long crime data to projection of shape files and add to onondoga map
crime.map <- SpatialPoints( lat.long.coords, proj4string=CRS("+proj=longlat +datum=WGS84") )

# identicalCRS( onondoga, crime.map ) check and this should be true 
#use over function to overlay crimemap and onondaga
crime.onondoga <- over( crime.map, onondoga ) 

#turn crime in onondoga into data frame
crime.onondoga  <- as.data.frame(crime.onondoga)

#count number of crimes in census tract
crime.onondoga  <-  as.data.frame(table(crime.onondoga$TRACTCE10))
#rename variables
crime.onondoga <- plyr::rename(crime.onondoga, c("Var1"="tract", "Freq"="crime number"))

#merge housing dataframe with number of crimes by census tract 
census.crime.dat <- merge(x=housing.census.dat, y=crime.onondoga, by.x="TRACTCE10", by.y="tract",all.x=T)

head(census.crime.dat) 
```

    ##   TRACTCE10 Street.Address.of.House Zip.Code       lon      lat STATEFP10
    ## 1    000100          139 Pulaski St    13204 -76.17168 43.05846        36
    ## 2    000200        1006 Wolf Street    13208 -76.15762 43.07961        36
    ## 3    000200        619 2nd N Street    13208 -76.16063 43.07537        36
    ## 4    000200            609 2nd N St    13208 -76.16025 43.07522        36
    ## 5    000200             709 Wolf St    13208 -76.16095 43.07638        36
    ## 6    000300          133 Harford Rd    13208 -76.14412 43.08204        36
    ##   COUNTYFP10     GEOID10 NAME10     NAMELSAD10 MTFCC10 FUNCSTAT10 ALAND10
    ## 1        067 36067000100      1 Census Tract 1   G5020          S 4842958
    ## 2        067 36067000200      2 Census Tract 2   G5020          S 1095299
    ## 3        067 36067000200      2 Census Tract 2   G5020          S 1095299
    ## 4        067 36067000200      2 Census Tract 2   G5020          S 1095299
    ## 5        067 36067000200      2 Census Tract 2   G5020          S 1095299
    ## 6        067 36067000300      3 Census Tract 3   G5020          S  574912
    ##   AWATER10  INTPTLAT10   INTPTLON10
    ## 1  1284980 +43.0691355 -076.1730170
    ## 2        0 +43.0747759 -076.1583997
    ## 3        0 +43.0747759 -076.1583997
    ## 4        0 +43.0747759 -076.1583997
    ## 5        0 +43.0747759 -076.1583997
    ## 6        0 +43.0812047 -076.1483599
    ##                                        NAME state county Total.Population
    ## 1 Census Tract 1, Onondaga County, New York    36    067              644
    ## 2 Census Tract 2, Onondaga County, New York    36    067             3042
    ## 3 Census Tract 2, Onondaga County, New York    36    067             3042
    ## 4 Census Tract 2, Onondaga County, New York    36    067             3042
    ## 5 Census Tract 2, Onondaga County, New York    36    067             3042
    ## 6 Census Tract 3, Onondaga County, New York    36    067             1635
    ##   SingleMom.below.poverty single.below.percent crime number
    ## 1                       0           0.00000000           45
    ## 2                     194           0.06377383           11
    ## 3                     194           0.06377383           11
    ## 4                     194           0.06377383           11
    ## 5                     194           0.06377383           11
    ## 6                      24           0.01467890            2
