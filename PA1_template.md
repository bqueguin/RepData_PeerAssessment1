# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

Here is the code to check if the data are available and for download it if necessary.  


```r
setwd("C:/Users/QUEGUINER/Desktop/Cours/Autre/Coursera/Reproducible Research/week2/RepData_PeerAssessment1")

if (!dir.exists("data")){
    dir.create("data")
}

if (!file.exists("data/activity.csv")){
    if(!file.exists("activity.zip")){
        download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip","activity.zip")
    }
    unzip("activity.zip", exdir = "data")
    file.remove("activity.zip")
}

activity <- read.csv("data/activity.csv")
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
There are 3 variables in the data:  

* **steps:** *integer* number of steps for a five-minute interval
* **date:** *factor* the 61 dates in format YYYY-MM-DD. That could be useful to convert in a date format.


```r
activity$date <- as.Date(activity$date)
```
* **interval** *integer* the 5 minute intrval of the day from 0 to 2355.  


## What is mean total number of steps taken per day?


```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.3.2
```

```r
steps_by_day <- with(activity, tapply(steps, date, sum, na.rm = T))

qplot(steps_by_day, geom = "histogram", bins = 20,
      fill = I("blue"), col = I("red"), alpha = I(0.5),
      main = "Histogram of steps by day", xlab = "steps")
```

![](PA1_template_files/figure-html/per_day-1.png)<!-- -->

```r
mean <- mean(steps_by_day)
median <- median(steps_by_day)
```
That histogram shows that the person often makes more than 10 steps per day. There are 10 days with no steps, this quite strange and probably due to missing values.  
The mean total number of steps taken per day is 9354 and the median is 10395.


## What is the average daily activity pattern?


```r
mean_by_interval <- with(activity, tapply(steps, interval, mean, na.rm = T))

qplot(x = as.integer(names(mean_by_interval)), y = mean_by_interval,
      geom = "line", col = I("red"), xlab = "interval", ylab = "steps",
      main = "Number of steps for 5 minute-intervals per day in average")
```

![](PA1_template_files/figure-html/per_interval-1.png)<!-- -->

We can notice that the person doesn't walk at all from 11 p.m. to 5 a.m. There is a peak of steps up to 200 steps in a period of 5 minutes in the morning around 8 and 9 a.m. Then, the average of steps during the day from 10 a.m. to 6 p.m s about 50 steps by interval of five minutes. An it begins to decrease at 7 p.m.


## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
