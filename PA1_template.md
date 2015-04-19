## Reproducible Research - Peer Assessment 1

Title:  PA1_Template
Author: Vitor Lopes
Date:   April 18th, 2014

### Basic settings

```r
echo = TRUE  # Always make code visible
options(scipen = 10)  # Turn off scientific notations for numbers
```

### Loading and preprocessing the data

```r
data <- read.csv("activity.csv", colClasses = c("integer", "Date", "factor")) #Data file on the same path
data$month <- as.numeric(format(data$date, "%m"))
summary(data)
```

```
##      steps             date               interval         month      
##  Min.   :  0.00   Min.   :2012-10-01   0      :   61   Min.   :10.00  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   10     :   61   1st Qu.:10.00  
##  Median :  0.00   Median :2012-10-31   100    :   61   Median :10.00  
##  Mean   : 37.38   Mean   :2012-10-31   1000   :   61   Mean   :10.49  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   1005   :   61   3rd Qu.:11.00  
##  Max.   :806.00   Max.   :2012-11-30   1010   :   61   Max.   :11.00  
##  NA's   :2304                          (Other):17202
```

```r
data_nona <- na.omit(data)
summary(data_nona)
```

```
##      steps             date               interval         month      
##  Min.   :  0.00   Min.   :2012-10-02   0      :   53   Min.   :10.00  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   10     :   53   1st Qu.:10.00  
##  Median :  0.00   Median :2012-10-29   100    :   53   Median :10.00  
##  Mean   : 37.38   Mean   :2012-10-30   1000   :   53   Mean   :10.45  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-16   1005   :   53   3rd Qu.:11.00  
##  Max.   :806.00   Max.   :2012-11-29   1010   :   53   Max.   :11.00  
##                                        (Other):14946
```

```r
rownames(data_nona) <- 1:nrow(data_nona)
head(data_nona)
```

```
##   steps       date interval month
## 1     0 2012-10-02        0    10
## 2     0 2012-10-02        5    10
## 3     0 2012-10-02       10    10
## 4     0 2012-10-02       15    10
## 5     0 2012-10-02       20    10
## 6     0 2012-10-02       25    10
```

```r
dim(data_nona)
```

```
## [1] 15264     4
```

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.1.3
```


### What is mean total number of steps taken per day?
- Total number of steps taken per day

```r
steps <- aggregate(list(totalSteps = data_nona$steps), list(Date = data_nona$date), FUN = "sum")
head(steps)  #First dates to show the required number of steps.
```

```
##         Date totalSteps
## 1 2012-10-02        126
## 2 2012-10-03      11352
## 3 2012-10-04      12116
## 4 2012-10-05      13294
## 5 2012-10-06      15420
## 6 2012-10-07      11015
```

```r
totalSteps <- steps$totalSteps
```

- Make a histogram of the total number of steps taken each day

```r
c <- ggplot(data_nona, aes(date, steps)) 
c <- c + geom_histogram(stat = "identity", colour = "blue", fill = "blue", width = 0.8) 
c <- c + labs(title = "Histogram - Total Number of steps taken each day", x = "Date", y = "Total number of steps")
c
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

- Calculate and report the mean and median total number of steps taken per day

* Mean total number of steps taken per day:

```r
mean(totalSteps)
```

```
## [1] 10766.19
```
* Median total number of steps taken per day:

```r
median(totalSteps)
```

```
## [1] 10765
```

### What is the average daily activity pattern?
- Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
avg_Steps <- aggregate(list(mean_of_steps = data_nona$steps), list(interval = as.numeric(as.character(data_nona$interval))), FUN = "mean")

ggplot(avg_Steps, aes(interval, mean_of_steps)) + geom_line(color = "blue", size = 0.8) + labs(title = "Time Series Plot of the 5-minute Interval", x = "5-minute intervals", y = "Average Number of Steps Taken")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

- Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps? Interval 835

```r
avg_Steps[avg_Steps$mean_of_steps == max(avg_Steps$mean_of_steps), ]  #Interval 835 has the maximum number of steps
```

```
##     interval mean_of_steps
## 104      835      206.1698
```

### Imputing missing values
- The total number of rows with NAs: 2304 rows.


```r
sum(is.na(data)) #The total number is 2304
```

```
## [1] 2304
```

- Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

I chose the mean for that 5-minute interval to fill each NA value in the steps column as my strategy.

- Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
newData <- data 
for (i in 1:nrow(newData)) {
    if (is.na(newData$steps[i])) {
        newData$steps[i] <- avg_Steps[which(avg_Steps$interval == newData$interval[i]), ]$mean_of_steps
    }
}

head(newData)
```

```
##       steps       date interval month
## 1 1.7169811 2012-10-01        0    10
## 2 0.3396226 2012-10-01        5    10
## 3 0.1320755 2012-10-01       10    10
## 4 0.1509434 2012-10-01       15    10
## 5 0.0754717 2012-10-01       20    10
## 6 2.0943396 2012-10-01       25    10
```

```r
sum(is.na(newData))
```

```
## [1] 0
```

- Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 


```r
ggplot(newData, aes(date, steps)) + geom_histogram(stat = "identity",
                                             colour = "blue",
                                             fill = "blue",
                                             width = 0.8) + labs(title = "Histogram - Total Number of steps taken each day (no missing data)", x = "Date", y = "Total number of steps")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 

* Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

Mean total number of steps taken per day:

```r
newTotalSteps <- aggregate(newData$steps, 
                           list(Date = newData$date), 
                           FUN = "sum")$x
newMean <- mean(newTotalSteps)
newMean
```

```
## [1] 10766.19
```
Median total number of steps taken per day:

```r
newMedian <- median(newTotalSteps)
newMedian
```

```
## [1] 10766.19
```
Compare them with the two before imputing missing data:

```r
oldMean <- mean(totalSteps)
oldMedian <- median(totalSteps)
newMean - oldMean
```

```
## [1] 0
```

```r
newMedian - oldMedian
```

```
## [1] 1.188679
```
As we can see, after imputing the missing data, the new mean of total steps taken per day is the same as that of the old mean; the new median of total steps taken per day is greater than that of the old median.

### Are there differences in activity patterns between weekdays and weekends?

- Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
Sys.setlocale("LC_TIME", "en_US") #Change session options to English
```

```
## [1] "en_US"
```

```r
head(newData)
```

```
##       steps       date interval month
## 1 1.7169811 2012-10-01        0    10
## 2 0.3396226 2012-10-01        5    10
## 3 0.1320755 2012-10-01       10    10
## 4 0.1509434 2012-10-01       15    10
## 5 0.0754717 2012-10-01       20    10
## 6 2.0943396 2012-10-01       25    10
```

```r
newData$weekdays <- factor(format(newData$date, "%A"))
levels(newData$weekdays)
```

```
## [1] "Friday"    "Monday"    "Saturday"  "Sunday"    "Thursday"  "Tuesday"  
## [7] "Wednesday"
```

```r
levels(newData$weekdays) <- list(weekday = c("Monday", "Tuesday",
                                             "Wednesday", 
                                             "Thursday", "Friday"),
                                 weekend = c("Saturday", "Sunday"))
levels(newData$weekdays)
```

```
## [1] "weekday" "weekend"
```

```r
table(newData$weekdays)
```

```
## 
## weekday weekend 
##   12960    4608
```

- Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
avg_Steps <- aggregate(list(mean_of_steps = newData$steps), 
                      list(interval = as.numeric(as.character(newData$interval)), 
                           weekdays = newData$weekdays),
                      FUN = "mean")
library(lattice)
```

```
## Warning: package 'lattice' was built under R version 3.1.2
```

```r
xyplot(avg_Steps$mean_of_steps ~ avg_Steps$interval | avg_Steps$weekdays, 
       layout = c(1, 2), type = "l", 
       xlab = "Interval", ylab = "Number of steps")
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16-1.png) 
