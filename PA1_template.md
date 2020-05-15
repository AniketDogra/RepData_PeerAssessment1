---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

Reading data from csv


```r
df <- read.csv("activity.csv")
head(df)
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

```r
print(colnames(df))
```

```
## [1] "steps"    "date"     "interval"
```

```r
options(scipen = 100)
```



## What is mean total number of steps taken per day?


```r
total.steps <- tapply(df$steps, df$date, sum, na.rm = T)
hist(total.steps, 
	main = "Histogram of total steps per day", 
	xlab = "Total steps per day", 
	ylab = "Frequency", 
	breaks = 20,
	col = "green"
)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
steps.mean <- mean(total.steps, na.rm = T)
steps.median <- median(total.steps, na.rm = T)
```

The mean number of steps taken each day is 9354, the median number is 10395. 


## What is the average daily activity pattern?


```r
steps.interval <- aggregate(df$steps, list(df$interval), mean, na.rm = T)
colnames(steps.interval) <- c("interval", "steps")
plot(steps.interval, type = "l",
	main = "Average steps by 5-minute interval",
	xlab = "Interval",
	ylab = "Average number of steps",
)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

5-minute interval, on average across all the days in the dataset that contains the maximum number of steps


```r
max <- steps.interval[which.max(steps.interval$steps),]
```

Maximum average is in the interval 835

## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
