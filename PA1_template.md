Reproducible Research: Peer Assessment 1
========================================


## Loading and preprocessing the data


```r
library(ggplot2)
steps = read.csv(header=T, file="activity.csv")
uniqueDaysList = unique(steps$date)
uniqueIntervalList = unique(steps$interval)
```

## What is mean total number of steps taken per day?

Histogram on amount of steps daily:

```r
sumPerDay = lapply(uniqueDaysList, function(x) { sum(na.rm = T, steps[ steps$date == x, "steps" ] ) } )
hist(as.integer(sumPerDay))
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

Mean of steps daily:

```r
mean(as.integer(sumPerDay), na.rm = T)
```

```
## [1] 9354.23
```
Meadian of steps daily:

```r
median(as.integer(sumPerDay), na.rm = T)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

Amount of steps in average in different intervals:

```r
avgPerInterval = lapply(uniqueIntervalList, function(x) { mean(na.rm = T, steps[ steps$interval == x, "steps" ] ) } )
avgPerIntervalDF = data.frame("interval"=uniqueIntervalList, "avgSteps"=as.double(avgPerInterval))
plot( x=uniqueIntervalList, y=avgPerInterval, type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 
Interval with maximum average steps:

```r
avgPerIntervalDF[which.max(avgPerIntervalDF$avgSteps),"interval"]
```

```
## [1] 835
```

## Imputing missing values

Amount of missign values:

```r
amountOfNAs = length( steps[ is.na(steps$steps) | is.na(steps$date) | is.na(steps$interval), 1])
```
Filling empty values with interval average:

```r
stepsWithNAsReplaced = mapply(function(x,y) {
    x[is.na(x)] <- avgPerIntervalDF[avgPerIntervalDF$interval == y, "avgSteps"]
    return(x)
    }, steps$steps, steps$interval)
steps = cbind(steps, stepsWithNAsReplaced)
```
Plot histogram on amount of steps daily with NAs as avarage:

```r
sumPerDayWithoutNAs = lapply(uniqueDaysList, function(x) { sum(steps[ steps$date == x, "stepsWithNAsReplaced" ] ) } )
hist(as.integer(sumPerDayWithoutNAs))
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

Mean of steps daily with NAs as avarage:

```r
mean(as.integer(sumPerDayWithoutNAs))
```

```
## [1] 10766.16
```
Meadian of steps daily with NAs as avarage:

```r
median(as.integer(sumPerDayWithoutNAs))
```

```
## [1] 10766
```

There is bigger impact on mean than the median. Also histogram has altered shape more closely to Gauss curve.


## Are there differences in activity patterns between weekdays and weekends?
Creating data frame with weekdays information.

```r
weekdaysChar = mapply(function(x) {
    weekday = weekdays(as.POSIXct(x))
    if( weekday == "Sunday" || weekday == "Saturday")
        return("weekend")
    else
        return("weekday")
}, steps$date)
weekdaysFac = as.factor(weekdaysChar)
stepsWithWeekdays = cbind(steps, weekdaysFac)
```
Creating plots for weekend and weekday cases.

```r
qplot(x=interval, y=stepsWithNAsReplaced, data=stepsWithWeekdays, xlab="Interval", facets=weekdaysFac~., geom=c("line"))
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png) 
