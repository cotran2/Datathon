---
title: "Uber Pickups in New York City"
date: "November 14, 2016"
output: html_document
---

```{r setup, include=FALSE}

knitr::opts_chunk$set(echo = FALSE, warning = FALSE, fig.width = 9, fig.height = 6)

library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)
library(tidyr)
library(DT)
library(ggthemes)

uber_jan_feb_2015 <- read.csv("../input/Uber-Jan-Feb-FOIL.csv")

mycolors <-  c("#CC0000", "#666666", "#009E73", "#CCCCCC", "#F0E442", "#0072B2", "#CC79A7")

```

## Exploring Uber trip data for 2014 (April - September)

```{r}

uber_apr14 <- read.csv("../input/uber-raw-data-apr14.csv")
uber_may14 <- read.csv("../input/uber-raw-data-may14.csv")
uber_jun14 <- read.csv("../input/uber-raw-data-jun14.csv")
uber_jul14 <- read.csv("../input/uber-raw-data-jul14.csv")
uber_aug14 <- read.csv("../input/uber-raw-data-aug14.csv")
uber_sep14 <- read.csv("../input/uber-raw-data-sep14.csv")


uber_2014 <- rbind(uber_apr14,uber_may14, uber_jun14, uber_jul14, uber_aug14, uber_sep14)


uber_2014$Date.Time <- as.POSIXct(uber_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S")

uber_2014$Time <- format(as.POSIXct(uber_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S"), format="%H:%M:%S")

uber_2014$Date.Time <- ymd_hms(uber_2014$Date.Time)

uber_2014$day <- factor(day(uber_2014$Date.Time))
uber_2014$month <- factor(month(uber_2014$Date.Time, label = TRUE))
uber_2014$year <- factor(year(uber_2014$Date.Time))
uber_2014$dayofweek <- factor(wday(uber_2014$Date.Time, label = TRUE))

uber_2014$hour <- factor(hour(hms(uber_2014$Time)))
uber_2014$minute <- factor(minute(hms(uber_2014$Time)))
uber_2014$second <- factor(second(hms(uber_2014$Time)))

```


### Number of Trips by Hour

```{r}

by_hour <- uber_2014 %>%
                  group_by(hour) %>%
                        dplyr::summarize(Total = n()) 
datatable(by_hour)

ggplot(by_hour, aes(hour, Total)) + 
            geom_bar( stat = "identity", fill = "darkgreen") +
                  ggtitle("Trips Every Hour") +
                    theme(legend.position = "none") +
                    scale_y_continuous(labels = comma)

by_month_hour <- uber_2014 %>%
                  group_by(month, hour) %>%
                        dplyr::summarize(Total = n()) 

ggplot(by_month_hour, aes(hour, Total, fill = month)) + 
            geom_bar( stat = "identity") +
                  ggtitle("Trips by Hour and Month") +
                    scale_y_continuous(labels = comma) +
                    scale_fill_manual(values = mycolors)
                    
```


### Number of Trips by Day

```{r}

by_day <- uber_2014 %>%
                  group_by(day) %>%
                        dplyr::summarize(Total = n()) 
datatable(by_day)

ggplot(by_day, aes(day, Total)) + 
            geom_bar( stat = "identity", fill = "darkred") +
                  ggtitle("Trips Every Day") +
                    theme(legend.position = "none") +
                    scale_y_continuous(labels = comma)


by_month_day <- uber_2014 %>%
                  group_by(month, day) %>%
                        dplyr::summarize(Total = n()) 

ggplot(by_month_day, aes(day, Total, fill = month)) + 
            geom_bar( stat = "identity") +
                  ggtitle("Trips by Day and Month") +
                    scale_y_continuous(labels = comma) +
                    scale_fill_manual(values = mycolors)
                    
```



### Number of Trips by Month

```{r}

by_month <- uber_2014 %>%
                  group_by(month) %>%
                        dplyr::summarize(Total = n()) 
datatable(by_month)

ggplot(by_month, aes(month, Total, fill = month)) + 
            geom_bar( stat = "identity") +
                  ggtitle("Trips by Month") +
                    theme(legend.position = "none") +
                    scale_y_continuous(labels = comma) +
                    scale_fill_manual(values = mycolors)

by_month_weekday <- uber_2014 %>%
                  group_by(month, dayofweek) %>%
                        dplyr::summarize(Total = n()) 

ggplot(by_month_weekday, aes(month, Total, fill = dayofweek)) + 
            geom_bar( stat = "identity", position = "dodge") +
                  ggtitle("Trips by Day and Month") +
                    scale_y_continuous(labels = comma) +
                    scale_fill_manual(values = mycolors)

```

### Number of Trips by bases


```{r}


ggplot(uber_2014, aes(Base)) + 
  geom_bar(fill = "darkblue") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases")

ggplot(uber_2014, aes(Base, fill = month)) + 
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases and Month") +
  scale_fill_manual(values = mycolors)

ggplot(uber_2014, aes(Base, fill = dayofweek)) + 
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases and DayofWeek") +
  scale_fill_manual(values = mycolors)

```



### Heat Map of Hour, Day and Month

```{r}

by_hour_day <- uber_2014 %>%
                  group_by(day, hour) %>%
                        dplyr::summarize(Total = n()) 

datatable(by_hour_day)

ggplot(by_hour_day, aes(day, hour, fill = Total)) +
            geom_tile(color = "white") +
              ggtitle("Heat Map by Hour and Day")

ggplot(by_month_day, aes(day, month, fill = Total)) +
            geom_tile(color = "white") +
              ggtitle("Heat Map by Month and Day")

ggplot(by_month_weekday, aes(dayofweek, month, fill = Total)) +
            geom_tile(color = "white") +
              ggtitle("Heat Map by Month and Day of Week")
              

by_bases_month <-  uber_2014 %>%
                    group_by(Base, month) %>%
                        dplyr::summarize(Total = n()) 
                        
by_bases_dayofweek <-  uber_2014 %>%
                    group_by(Base, dayofweek) %>%
                        dplyr::summarize(Total = n()) 
                        
ggplot(by_bases_month, aes(Base, month, fill = Total)) +
            geom_tile(color = "white") +
              ggtitle("Heat Map by Month and Bases")
              
ggplot(by_bases_dayofweek, aes(Base, dayofweek, fill = Total)) +
            geom_tile(color = "white") +
              ggtitle("Heat Map by Bases and Day of Week")              
              
```


* Number of trips increases as the day progress
* It peaks during evening time around the time when people are logging off from work 
* September is the best month.
* There is gradual increase in the number of trips from April to Sep.
* There is almost double the number of trips in September 2014 compared with April 2014
* Each month(April - Sep) had different day of week with most trips.
* B02617 base had the highest number of trips
* B02512 and B02764 are not the best bases from business perspective.
* September was the best month for B02617
* May was the best month for B02598
* Apr was the best month for B02682
* Thursday was the best day for B02598, B02617 and B02682



### Exploring Uber trip statistics in January and February 2015

```{r}

uber_jan_feb_2015$date <- as.Date(uber_jan_feb_2015$date, "%m/%d/%Y")

uber_jan_feb_2015$day <- factor(day(uber_jan_feb_2015$date))
uber_jan_feb_2015$month <- factor(month(uber_jan_feb_2015$date, label = TRUE))
uber_jan_feb_2015$year <- factor(year(uber_jan_feb_2015$date))
uber_jan_feb_2015$dayofweek <- factor(wday(uber_jan_feb_2015$date, label = TRUE))


uber_jan_feb_2015$TripsperVehicle  <- uber_jan_feb_2015$trips/uber_jan_feb_2015$active_vehicles

```

### Number of Trips by Day of the month

```{r}

datatable(uber_jan_feb_2015 %>% 
              group_by(day) %>% 
                dplyr::summarize(Total = sum(trips)))

ggplot(uber_jan_feb_2015, aes(day, trips)) + 
            geom_bar(fill = "darkred", stat = "identity") +
                  ggtitle("Uber trips by Day of the Month") +
                    scale_fill_manual(values = mycolors)

ggplot(uber_jan_feb_2015, aes(day, trips, fill = month)) + 
            geom_bar(stat = "identity") +
                  ggtitle("Uber trips by Day and Month") +
                    scale_fill_manual(values = mycolors)

```


* Huge number of trips
* 15th day of the month had the highest number of trips

### Number of Active Vehicles by Day of the month

```{r}

datatable(uber_jan_feb_2015 %>% 
              group_by(day) %>% 
                dplyr::summarize(Total = sum(active_vehicles)))

ggplot(uber_jan_feb_2015, aes(day, active_vehicles)) + 
            geom_bar(fill = "#0072B2", stat = "identity") +
                  ggtitle("Active Vehicles by Day of the Month") +
                    scale_fill_manual(values = mycolors)

ggplot(uber_jan_feb_2015, aes(day, active_vehicles, fill = month)) + 
            geom_bar(stat = "identity") +
                  ggtitle("Active Vehicles by Day and Month") +
                    scale_fill_manual(values = mycolors)

```


### Number of Trips by Weekday 

```{r}

datatable(uber_jan_feb_2015 %>% 
              group_by(dayofweek) %>% 
                dplyr::summarize(Total = sum(trips)))

ggplot(uber_jan_feb_2015, aes(dayofweek, trips, fill = dayofweek)) + 
            geom_bar( stat = "identity") +
                  ggtitle("Uber trips by Weekday of the Month") +
                    theme(legend.position = "none") +
                    scale_y_continuous(labels = comma) +
                    scale_fill_manual(values = mycolors)

ggplot(uber_jan_feb_2015, aes(dayofweek, trips, fill = month)) + 
            geom_bar(stat = "identity", position = "dodge") +
                  ggtitle("Uber trips by Weekday and Month") +
                    scale_y_continuous(labels = comma) +
                    scale_fill_manual(values = mycolors)

```

* Friday and Saturday had the highest number of trips.


### Number of Active Vehicles By Weekday

```{r}

datatable(uber_jan_feb_2015 %>% 
              group_by(dayofweek) %>% 
                dplyr::summarize(Total = sum(active_vehicles)))

ggplot(uber_jan_feb_2015, aes(dayofweek, active_vehicles, fill = dayofweek)) + 
            geom_bar( stat = "identity") +
                  ggtitle("Active Vehicles By Weekday") +
                    theme(legend.position = "none") +
                    scale_y_continuous(labels = comma) +
                    scale_fill_manual(values = mycolors)

ggplot(uber_jan_feb_2015, aes(dayofweek, active_vehicles, fill = month)) + 
            geom_bar(stat = "identity", position = "dodge") +
                  ggtitle("Active Vehicles By Weekday and Month") +
                    scale_y_continuous(labels = comma) +
                    scale_fill_manual(values = mycolors)


ggplot(uber_jan_feb_2015, aes(dayofweek, active_vehicles, fill = dayofweek)) + 
  geom_boxplot() +
  scale_y_log10() +
  ggtitle("Distribution of Active Vehicles By Weekday") +
  theme(legend.position = "top") +
  coord_flip()
  
```


* Thursday, Friday and Saturday had more number of active Vehicles


### Number of Trips Per Vehicle


```{r}


ggplot(uber_jan_feb_2015, aes(TripsperVehicle)) + 
  geom_histogram(bins = 30, fill  = "#0072B2") + 
      ggtitle("Distribution of Trips Per Vehicle") +
                    scale_fill_manual(values = mycolors)

ggplot(uber_jan_feb_2015, aes(TripsperVehicle, fill = dayofweek)) +
    geom_histogram(bins = 30) + 
      ggtitle("Distribution of Trips Per Vehicle by Weekday") +
                    scale_fill_manual(values = mycolors)


ggplot(uber_jan_feb_2015, aes(dayofweek, TripsperVehicle, fill = dayofweek)) + 
  geom_boxplot() +
  scale_y_log10() +
  ggtitle("Distribution of Trips Per Vehicle by Weekday") +
  theme(legend.position = "top") +
  coord_flip()
  
```

* Around 9 trips per vehicle is the most common 
* There are vehicles which have run even more than 10 trips per day


### Distribution of Number of Trips


```{r}


ggplot(uber_jan_feb_2015, aes(trips)) + 
  geom_histogram(bins = 50, fill  = "#0072B2") + 
      ggtitle("Distribution of Number of Trips") +
                    scale_fill_manual(values = mycolors)

ggplot(uber_jan_feb_2015, aes(trips, fill = dayofweek)) +
    geom_histogram(bins = 30) + 
      ggtitle("Distribution of Number of Trips by Weekday") +
                    scale_fill_manual(values = mycolors)


ggplot(uber_jan_feb_2015, aes(dayofweek, trips, fill = dayofweek)) + 
  geom_boxplot() +
  scale_y_log10() +
  ggtitle("Distribution of Trips by Weekday") +
  theme(legend.position = "top") +
  coord_flip()
  
```

### Mapping number of rides


```{r}

## Thanks to http://minimaxir.com/2015/11/nyc-ggplot2-howto/ for helping learn how to do it.

min_lat <- 40.5774
max_lat <- 40.9176
min_long <- -74.15
max_long <- -73.7004

ggplot(uber_2014, aes(x=Lon, y=Lat)) +
     geom_point(size=1, color = "blue") +
            scale_x_continuous(limits=c(min_long, max_long)) +
               scale_y_continuous(limits=c(min_lat, max_lat)) +
                   theme_map() +
                        ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014 (APR-SEP)")

ggplot(uber_2014, aes(x=Lon, y=Lat, color = Base)) +
     geom_point(size=1) +
            scale_x_continuous(limits=c(min_long, max_long)) +
               scale_y_continuous(limits=c(min_lat, max_lat)) +
                  theme_map() +
                       ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014 (APR-SEP) by BASE")

```


### To continue
