# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data
1. Load the data


```r
set.seed(0)
activity <- read.csv("activity.csv")
```

2. Add datetime field to the data


```r
activity$datetime <- strptime(activity$date, format = "%Y-%m-%d")
summary(activity)
```

```
##      steps               date          interval       datetime         
##  Min.   :  0.0   2012-10-01:  288   Min.   :   0   Min.   :2012-10-01  
##  1st Qu.:  0.0   2012-10-02:  288   1st Qu.: 589   1st Qu.:2012-10-16  
##  Median :  0.0   2012-10-03:  288   Median :1178   Median :2012-10-31  
##  Mean   : 37.4   2012-10-04:  288   Mean   :1178   Mean   :2012-10-31  
##  3rd Qu.: 12.0   2012-10-05:  288   3rd Qu.:1766   3rd Qu.:2012-11-15  
##  Max.   :806.0   2012-10-06:  288   Max.   :2355   Max.   :2012-11-30  
##  NA's   :2304    (Other)   :15840
```

## What is mean total number of steps taken per day?
1. Make a histogram of the total number of steps taken each day


```r
total_steps <- tapply(activity$steps, activity$date, sum, na.rm = T)
hist(total_steps, 
     xlab = "Steps",
     ylab = "Number of days",
     main = "Total number of steps per day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

2. Calculate and report the **mean** and **median** total number of steps taken per day


```r
mean(total_steps)
```

```
## [1] 9354
```

```r
median(total_steps)
```

```
## [1] 10395
```


## What is the average daily activity pattern?
1. Make a time series plot of the 5-minute interval and the average number of steps taken, averaged across all days


```r
interval_avg <- tapply(activity$steps, activity$interval, mean, na.rm = T)
plot(interval_avg,
     main="Average daily activity",
     xlab="5-minute interval",
     ylab="Average number of steps",
     type="l")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
interval_avg[interval_avg == max(interval_avg)]
```

```
##   835 
## 206.2
```

## Imputing missing values
1. Calculate and report the total number of missing values in the dataset


```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

2. Strategy for filling in all of the missing values:

Fill NA with average of interval (which is calculated in previous part)

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
activity_imputed <- activity
missing <- is.na(activity$steps)

activity_imputed$steps[missing] <- 
  interval_avg[as.character(activity$interval[missing])]
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day


```r
total_steps <- tapply(activity_imputed$steps, activity_imputed$date, sum)
hist(total_steps, 
     xlab = "steps",
     ylab = "number of days",
     main = "Total number of steps per day")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 

```r
mean(total_steps)
```

```
## [1] 10766
```

```r
median(total_steps)
```

```
## [1] 10766
```

Filling NA values do have impact on mean and median.

## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend”


```r
activity_imputed$weekday <- weekdays(activity_imputed$datetime)
activity_imputed$weekday <- ifelse(
  (activity_imputed$weekday == "Saturday") 
  | (activity_imputed$weekday == "Sunday"),
  "weekend",
  "weekday")
activity_imputed$weekday <- factor(activity_imputed$weekday)
```

2. Make a panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days.


```r
library(lattice)
activity_weekday = activity_imputed[activity_imputed$weekday == "weekday",]
activity_weekend = activity_imputed[activity_imputed$weekday == "weekend",]
interval_avg_weekday <- tapply(activity_weekday$steps,
       activity_weekday$interval,
       mean)
interval_avg_weekend <- tapply(activity_weekend$steps,
       activity_weekend$interval,
       mean)

avg <- data.frame(
  c(interval_avg_weekday, interval_avg_weekend),
  c(unique(activity_weekday$interval),
        unique(activity_weekend$interval)),
  c(rep("weekday", length(interval_avg_weekday)),
        rep("weekend", length(interval_avg_weekend))),
  )
```

```
## Error: argument is missing, with no default
```

```r
colnames(avg) <- c("steps", "interval", "weekday")
```

```
## Error: object 'avg' not found
```

```r
xyplot(steps ~ interval | weekday,
       data = avg,
       layout=c(1,2),
       xlab="Interval",
       ylab="Number of steps",
       type="l")
```

```
## Error: object 'avg' not found
```
On the weekday, the number of steps have a clear peek, while on the weekend, the steps are spread throughout the day.
