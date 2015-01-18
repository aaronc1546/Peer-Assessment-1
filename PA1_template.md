---
title: "Peer Assessment 1"
author: "Aaron Christensen"
output: html_document
---

###Loading and preprocessing the data


```r
activity <- read.csv("/Users/marissatomko/activity.csv", colClasses = c("numeric", "character", "numeric"))
activity$date <- as.Date(activity$date, "%Y-%m-%d")
library(lattice)
```

###What is the mean total number of steps taken each day?


```r
DailySteps <- aggregate(steps ~ date, data = activity, sum, na.rm = TRUE)
hist(DailySteps$steps, col = "blue", 
     main = "Daily Steps Taken", xlab = "Steps", 
     ylab = "Frequency")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

```r
  mean(DailySteps$steps)
```

```
## [1] 10766.19
```

```r
  median(DailySteps$steps)
```

```
## [1] 10765
```

###What is the average daily activity pattern?

```r
IntSteps <- aggregate (steps ~ interval, data = activity, mean, na.rm = TRUE)
plot(steps ~ interval, data = IntSteps, type = "l", main = "Steps Taken by Interval", xlab = "Interval", ylab = "Steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

Which five minute interval contains the maximum number of steps? 


```r
IntSteps[which.max(IntSteps[,2]),]
```

```
##     interval    steps
## 104      835 206.1698
```


###Imputing missing values

Calculate the numbe of missing values:

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

Inpute missing values

```r
StepsAverage <- aggregate(steps ~ interval, data = activity, FUN = mean)
fillNA <- numeric()
for (i in 1:nrow(activity)) {
  obs <- activity[i, ]
  if (is.na(obs$steps)) {
    steps <- subset(StepsAverage, interval == obs$interval)$steps
  } else {
    steps <- obs$steps
  }
  fillNA <- c(fillNA, steps)
}

new_activity <- activity
new_activity$steps <- fillNA
```

Create a histogram with new values filled in

```r
StepsTotal2 <- aggregate(steps ~ date, data = new_activity, sum, na.rm = TRUE)
hist(StepsTotal2$steps, main = "Total steps by day", xlab = "day", col = "red")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

###Are there any differences in activity patters between weekdays and weekends?


```r
day <- weekdays(activity$date)
daylevel <- vector()
for (i in 1:nrow(activity)) {
  if (day[i] == "Saturday") {
    daylevel[i] <- "Weekend"
  } else if (day[i] == "Sunday") {
    daylevel[i] <- "Weekend"
  } else {
    daylevel[i] <- "Weekday"
  }
}
activity$daylevel <- daylevel
activity$daylevel <- factor(activity$daylevel)

stepsByDay <- aggregate(steps ~ interval + daylevel, data = activity, mean)
names(stepsByDay) <- c("interval", "daylevel", "steps")

'Panel plot comparing weekdays to weekends'
```

```
## [1] "Panel plot comparing weekdays to weekends"
```

```r
xyplot(steps ~ interval | daylevel, stepsByDay, type = "l", layout = c(1, 2), 
       xlab = "Interval", ylab = "Number of steps")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 

