lab1\_mengran
================
mgao05
February 9, 2017

1.Follow the steps in the Dates tutoial to read in the code violation data and drop all variables except violation dates, violation types, and their coordinates (lat,lon).
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

``` r
#import data
data.url <- "https://raw.githubusercontent.com/lecy/code-orange/master/data/code%20violations.csv"

dat <- read.csv( data.url, stringsAsFactors=F)

#subset dataframe
mydat <- dat[ , c("Complaint.Type","Violation.Date","lat","lon") ]
```

2.Convert the Violation.Date from a character into a date class.
================================================================

3.Create new variables for days of the week (Mon, Tues, Wed, etc.), weeks of the year (1-52), months, and years.
================================================================================================================

``` r
#transform dates

class(mydat$Violation.Date)
```

    ## [1] "character"

``` r
mydat$Violation.Date <- as.Date(mydat$Violation.Date, format = "%m/%d/%Y")

#format as years and make a table based on years
as.year <- format(mydat$Violation.Date, format = "%Y")

table(as.year) %>% pander
```

<table style="width:97%;">
<colgroup>
<col width="9%" />
<col width="9%" />
<col width="9%" />
<col width="9%" />
<col width="9%" />
<col width="9%" />
<col width="9%" />
<col width="9%" />
<col width="9%" />
<col width="9%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">2007</th>
<th align="center">2008</th>
<th align="center">2009</th>
<th align="center">2010</th>
<th align="center">2011</th>
<th align="center">2012</th>
<th align="center">2013</th>
<th align="center">2014</th>
<th align="center">2015</th>
<th align="center">2016</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">2</td>
<td align="center">16</td>
<td align="center">28</td>
<td align="center">30</td>
<td align="center">59</td>
<td align="center">11832</td>
<td align="center">11754</td>
<td align="center">13903</td>
<td align="center">11859</td>
<td align="center">1944</td>
</tr>
</tbody>
</table>

``` r
#format as months, and maka table based on different months

as.months <- format(mydat$Violation.Date, format = "%b")
table(as.months) %>% pander
```

<table>
<colgroup>
<col width="8%" />
<col width="8%" />
<col width="8%" />
<col width="8%" />
<col width="8%" />
<col width="8%" />
<col width="8%" />
<col width="8%" />
<col width="8%" />
<col width="8%" />
<col width="8%" />
<col width="8%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">Apr</th>
<th align="center">Aug</th>
<th align="center">Dec</th>
<th align="center">Feb</th>
<th align="center">Jan</th>
<th align="center">Jul</th>
<th align="center">Jun</th>
<th align="center">Mar</th>
<th align="center">May</th>
<th align="center">Nov</th>
<th align="center">Oct</th>
<th align="center">Sep</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">3592</td>
<td align="center">4953</td>
<td align="center">3672</td>
<td align="center">4423</td>
<td align="center">4812</td>
<td align="center">4608</td>
<td align="center">5021</td>
<td align="center">3940</td>
<td align="center">4842</td>
<td align="center">3590</td>
<td align="center">3967</td>
<td align="center">4007</td>
</tr>
</tbody>
</table>

``` r
#format as days of the week and make a table based on days of the week

as.days <- format(mydat$Violation.Date, format = "%A")
table(as.days) %>% pander
```

<table style="width:97%;">
<colgroup>
<col width="12%" />
<col width="12%" />
<col width="15%" />
<col width="12%" />
<col width="15%" />
<col width="13%" />
<col width="15%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">Friday</th>
<th align="center">Monday</th>
<th align="center">Saturday</th>
<th align="center">Sunday</th>
<th align="center">Thursday</th>
<th align="center">Tuesday</th>
<th align="center">Wednesday</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">9480</td>
<td align="center">8769</td>
<td align="center">35</td>
<td align="center">5</td>
<td align="center">10521</td>
<td align="center">11219</td>
<td align="center">11398</td>
</tr>
</tbody>
</table>

``` r
#format as weeks of the year
as.weeks <- format(mydat$Violation.Date, format = "%V")
```

4.Select a category of code violations other than bed bugs and create a new dataset with all other data dropped.
----------------------------------------------------------------------------------------------------------------

``` r
syr <- get_map(location = "syracuse", zoom=13, maptype = "toner-lite")
ggmap(syr)
```

![](lab1_mengran_gao_files/figure-markdown_github/Infestation%20violations-1.png)

``` r
syr.min.lat <- 42.96
syr.max.lat <- 43.12
syr.min.lon <- -76.25
syr.max.lon <- -76.05
mydat <- mydat [mydat$lat > syr.min.lat & mydat$lat < syr.max.lat, ]
mydat <- mydat [mydat$lon > syr.min.lon & mydat$lon < syr.max.lon, ]

#check complaint numbers for each type
sort(table(mydat$Complaint.Type), decreasing = T)
```

    ## 
    ##  Property Maintenance-Int  Property Maintenance-Ext 
    ##                     23472                      8226 
    ## Trash/Debris-Private, Occ  Overgrowth: Private, Occ 
    ##                      2886                      2563 
    ## Complaint Reqst - General     Illegal Trash Set Out 
    ##                      2272                      1736 
    ##                  Bed Bugs               Infestation 
    ##                       739                       661 
    ##               Fire Safety       Building W/O Permit 
    ##                       605                       386 
    ##  Cert of Use - Restaurant  Cert of Use - Food Store 
    ##                       280                       232 
    ##         Unsafe Conditions         Cert of Use - Bar 
    ##                       167                       127 
    ##                Demolition                Fire Alarm 
    ##                       115                       115 
    ##         Zoning Violations               Other (FPB) 
    ##                        86                        73 
    ##          Sprinkler System     Vacant Lot: Overgrown 
    ##                        65                        49 
    ##          Faulty Equipment      Blocked/Locked Exits 
    ##                        43                        38 
    ##                   Heating Suppression-not sprinkler 
    ##                        35                        26 
    ##         Structural Issues Graffiti: PrivateProperty 
    ##                        23                        20 
    ## Water Shop:All Complaints    Tenant Safety Concerns 
    ##                        18                        17 
    ##             Sewer Back Up  Over Capacity (Assembly) 
    ##                        15                        10 
    ## Vacant House Open toEntry OverCapacity/too many ppl 
    ##                        10                         8 
    ##  Sanitation/Special Reqst         Electrical Hazard 
    ##                         8                         5 
    ## Outdoor / Illegal Burning    Overgrown Veg - Public 
    ##                         4                         4 
    ## Vacant: Illegal Occupancy Const/Demo Debris: Req PU 
    ##                         3                         2 
    ##          Improper storage                  Lock Box 
    ##                         2                         2 
    ## Medical Waste-P/U Refused Preventative Code Enforce 
    ##                         2                         2 
    ##  Vacant Lot: Trash/Debris   Vacant Structure Hazard 
    ##                         2                         2 
    ##                Yard Waste  Blue Bin: request new BB 
    ##                         2                         1 
    ##      Bulk Household Items Corners Need Snow Removal 
    ##                         1                         1 
    ## Graffiti: Public Property  Tree Inspect/Problem Req 
    ##                         1                         1 
    ##   Tree/Limb/Stump Removal 
    ##                         1

``` r
# filter by count of violation number of types,only the top 10 frequency
top10viola <- names (sort(table(mydat$Complaint.Type), decreasing = T) [1:10])
top10viola <- mydat[mydat$Complaint.Type %in% top10viola , ]
class(top10viola$Complaint.Type)
```

    ## [1] "character"

``` r
top10viola$Complaint.Type <- as.factor(top10viola$Complaint.Type)
levels(top10viola$Complaint.Type)
```

    ##  [1] "Bed Bugs"                  "Building W/O Permit"      
    ##  [3] "Complaint Reqst - General" "Fire Safety"              
    ##  [5] "Illegal Trash Set Out"     "Infestation"              
    ##  [7] "Overgrowth: Private, Occ"  "Property Maintenance-Ext" 
    ##  [9] "Property Maintenance-Int"  "Trash/Debris-Private, Occ"

``` r
#add legend to violation types 
new.labels <- c("Bed Bugs", "No Permit", "General", "Fire Hazard", "Trash", "Infestation", "Overgrown", "Exterior Maintenance", "Interior Maintenance", "Trash")
levels(top10viola$Complaint.Type) <- new.labels
qmplot(lon, lat, data = top10viola, maptype = "toner-lite", color=Complaint.Type) + facet_wrap(~ Complaint.Type)
```

![](lab1_mengran_gao_files/figure-markdown_github/Infestation%20violations-2.png)

``` r
#add month and year 
levels(top10viola$Complaint.Type)
```

    ## [1] "Bed Bugs"             "No Permit"            "General"             
    ## [4] "Fire Hazard"          "Trash"                "Infestation"         
    ## [7] "Overgrown"            "Exterior Maintenance" "Interior Maintenance"

``` r
as.month <- format( mydat$Violation.Date, "%b" )

as.year <- format(mydat$Violation.Date, "%Y")

mydat$Month <- as.month

mydat$Year <- as.year

qmplot(lon, lat, data=mydat, maptype = "toner-lite", color=I("red"), alpha = 0.3) + theme(legend.position = "none")
```

![](lab1_mengran_gao_files/figure-markdown_github/Infestation%20violations-3.png)

``` r
qmplot(lon, lat, data=mydat, maptype = "toner-lite", color=I("red"), geom = "density2d")
```

![](lab1_mengran_gao_files/figure-markdown_github/Infestation%20violations-4.png)

``` r
infestation <- mydat[mydat$Complaint.Type == "Infestation",]
qmplot(lon,lat, data = infestation, maptype = "toner-lite", color =I("red"), alpha= 0.3)
```

![](lab1_mengran_gao_files/figure-markdown_github/Infestation%20violations-5.png)

6.Select one year of data. Using the qmplot() function in the ggmap package, create a plot with one map for each month.
=======================================================================================================================

``` r
# Select one year of data. Using the qmplot() function in the ggmap package, create a plot with one map for each month.
as.month <- format( mydat$Violation.Date, "%b" )

as.year <- format(mydat$Violation.Date, "%Y")

mydat$Month <- as.month

mydat$Year <- as.year

year2015 <- mydat[mydat$Year == "2015", ]
theme_set(theme_bw())

#remove nas in year2015 dataframe
year2015 <- na.omit(year2015)

# how to do it in ggplot : 
#ggplot( data=year2015, aes( x=lon, y=lat ) ) + geom_point() + facet_wrap( ~ Month )

#show months in qmplot funtion
qmplot(lon, lat, data = year2015, maptype = "toner-lite", 
      color = I("orange")) +  facet_wrap( ~ Month )
```

![](lab1_mengran_gao_files/figure-markdown_github/each%20month-1.png)
