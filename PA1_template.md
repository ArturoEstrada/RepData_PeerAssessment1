Reproducible Research Week 2 Project
==========================================
This document presents a 5 parts analysis. "This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day".

###PART 1: Loading and preprocessing the data

Defining document relevant path and generating/processing data frame 

```r
file <- "C:/Users/Arturo/Documents/Data Science Specialization/Reproducible Research/data/activity.csv"
data.activity <- read.csv(file, header = TRUE)
data.activity$date <- as.Date(data.activity$date, "%Y-%m-%d")
head(data.activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

###PART 2: What is the mean total number of steps taken per day?

Calculation of total steps taken per day and generation of Histogram of such result

```r
StepsTotalDay <- aggregate(steps ~ date, data.activity, sum, na.rm = TRUE)
StepsTotalDay$steps <- as.numeric(StepsTotalDay$steps)
hist(StepsTotalDay$steps, xlab = "Total Steps per Day", main = "Histogram of Total number of Steps taken per day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

###Calculation of mean and median for number of steps taken by day

```r
StepsMean <- mean(StepsTotalDay$steps)
StepsMedian <- median(StepsTotalDay$steps)
print(c(StepsMean, StepsMedian))
```

```
## [1] 10766.19 10765.00
```

###PART 3: What is the average daily activity pattern?

Definition of average steps taken in each 5 min interval of time each day and generation of time series of such result

```r
StepsIntMean <- aggregate(steps ~ interval, data.activity, mean, rm.na = TRUE)
head(StepsIntMean)
```

```
##   interval     steps
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```

```r
with(StepsIntMean, plot(interval, steps, xlab = "Interval", ylab = "Average steps per day", main = "Average steps per day time series", type = "l"))
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

Defining interval with maximum average steps per day 

```r
Max <- StepsIntMean$interval[which.max(StepsIntMean$steps)]
print(Max)
```

```
## [1] 835
```

###PART 4: Imputing missing values

Calculation of total number of missing values (i.e. NAs)

```r
TotalNAs <- sum(is.na(data.activity$steps))
print(TotalNAs)
```

```
## [1] 2304
```

Creating new data frame changing NAs for average steps per day in such interval

```r
data.activity2 <- data.activity
i <- 1
for (i in 1:nrow(data.activity2)){
  if(is.na(data.activity2$steps[i]) == 1){
    data.activity2$steps[i] <- StepsIntMean$steps[((grep(data.activity2$interval[i],StepsIntMean$interval))[1])]
  }
  i <- i + 1
}
head(data.activity2)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

Calculation of total steps taken per day and generation of Histogram of such result

```r
StepsTotalDay2 <- aggregate(steps ~ date, data.activity2, sum)
StepsTotalDay2$steps <- as.numeric(StepsTotalDay2$steps)
hist(StepsTotalDay2$steps, xlab = "Total Steps per Day", main = "Histogram of Total number of Steps taken per day - no NAs")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)

Calculation of mean and median for number of steps taken by day

```r
StepsMean2 <- mean(StepsTotalDay2$steps)
StepsMedian2 <- median(StepsTotalDay2$steps)
print(c(StepsMean2, StepsMedian2))
```

```
## [1] 10766.19 10766.19
```

###PART 5: Are there differences in activity patterns between weekdays and weekends?

Defining which days correspond to a weekday or to the weekend 

```r
j <- 1
DayType <- c(rep(0,nrow(data.activity2)))
data.activity2 <- cbind(data.activity2, DayType)
for (j in 1:nrow(data.activity2)) {
  if((weekdays(data.activity2$date[j]) == "sábado") | (weekdays(data.activity2$date[j]) == "domingo" )){
    data.activity2$DayType[j] <- "Weekend"
  } else {data.activity2$DayType[j] <- "Weekday"}
  j <- j + 1
}
data.activity2 <- data.activity2[ ,1:4]
data.activity2$DayType <- as.factor(data.activity2$DayType)
head(data.activity2)
```

```
##       steps       date interval DayType
## 1 1.7169811 2012-10-01        0 Weekday
## 2 0.3396226 2012-10-01        5 Weekday
## 3 0.1320755 2012-10-01       10 Weekday
## 4 0.1509434 2012-10-01       15 Weekday
## 5 0.0754717 2012-10-01       20 Weekday
## 6 2.0943396 2012-10-01       25 Weekday
```

Creating panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)

```r
data.activity2_sub1 <- subset(data.activity2, data.activity2$DayType == "Weekday")
data.activity2_sub2 <- subset(data.activity2, data.activity2$DayType == "Weekend")
StepsIntMean_sub1 <- aggregate(steps ~ interval, data.activity2_sub1, mean)
StepsIntMean_sub2 <- aggregate(steps ~ interval, data.activity2_sub2, mean)
par(mfrow = c(2,1))
with(StepsIntMean_sub1, plot(interval, steps, xlab = "Interval", ylab = "Average steps per day", main = "Average steps per day time series - Weekdays", type = "l"))
with(StepsIntMean_sub2, plot(interval, steps, xlab = "Interval", ylab = "Average steps per day", main = "Average steps per day time series - Weekends", type = "l"))
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png)

