# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
  The exercise in reproducible research is based on a dataset containing counts of steps taken during five minute intervals over a two month period in 2012 by a single test subject. Steps were measured and recorded by the subject's cell phone.
  
  We begin the exercise by reading in the "activity" data set using the "read.csv" command - after unzipping the compressed file:


```r
activity <- read.csv(unzip("activity.zip"),header = TRUE)
```
The variables included in this dataset are:

    steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)

    date: The date on which the measurement was taken in YYYY-MM-DD format

    interval: Identifier for the 5-minute interval in which measurement was taken

There are a total of 17568 observations: equal to 288 5-minute intervals on 61 days.


## What is mean total number of steps taken per day?

In order to investigate daily patterns in activity (steps taken), we reduce the activity dataset to daily summaries:
Since the "dplyr" package facilitates data manipulation, we load it and use its "group_by","summarize" and "%>%"
commands to create the daily totals in a new data frame called "daily."


```r
library(dplyr)
daily <- activity %>% group_by(date) %>% summarize(TotalSteps = sum(steps))
```

We visualize the daily effort levels using a histogram, then get the summary statistics through the "summary" function.


```r
hist(daily$TotalSteps,xlab = "Number of Steps per Day", main = "CHART 1: Histogram of Steps per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
summary(daily$TotalSteps,na.rm = TRUE, digits=6)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    41.0  8841.0 10765.0 10766.2 13294.0 21194.0       8
```

Here, the mean (10766.19) and median (10765) are nearly the same which indicates a reasonably symmetric distribution of days around those measures of central location. Also shown are some ideas of the spread in the distribution of days with quartiles and extrema.  In particular, the interquartile range is the  difference between the 3rd and 1st quartiles (4453)
 

## What is the average daily activity pattern?

This question can be answered by averaging across days for the 5-minute intervals (288 = 12 per hour times 24 hours).


```r
intervals <- activity %>% group_by(interval) %>% summarise(meansteps = mean(steps,na.rm=TRUE))
plot(intervals,type = "l",ylab="Mean Number of Steps per 5 Minutes",main="CHART 2: Daily Activity Pattern",xlab="Five Minute Intervals",xaxt = "n")
axis(1,at=seq(0,2400,600),labels = c("0","6am","Noon","6pm","Mid"))
library(Hmisc)
minor.tick(nx=5, tick.ratio=0.5)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

## Imputing missing values

First, in order to get a handle on the magnitude of the missing values problem, we compute the number of cases with NAs in them (2304), and the proportion of the total that represents (0.1311475).

Next, we replace the NAs with "predicted" values which are computed as the average for that time period. We create a new variable "insteps" to hold the new, complete set of values.  "Insteps" starts as a duplicate of the "steps" variables, then the NAs are replaced:


```r
activity$insteps <- activity$steps
xx <- intervals$meansteps
activity$insteps[is.na(activity$steps)] <- xx[findInterval(activity$interval[is.na(activity$steps)],intervals$interval)]
```

Now we look at what has been distorted by the estimation of NAs. First, a histogram of total (in)steps per day:


```r
indaily <- activity %>% group_by(date) %>% summarise(total = sum(insteps))
hist(as.integer(indaily[,2][[1]]),xlab = "Estimated Number of Steps per Day", main = "CHART 3: Histogram of Steps per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

The differences are subtle, but the modal column is over 35 for the estimated steps, but under 30 for actual reported steps (see chart 1 above).

For the central tendency we use "summary" which give us two measures - mean and median - as well as some indication of spread.


```r
summary(as.integer(indaily[,2][[1]]),na.rm = TRUE, digits=6)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    41.0  9819.0 10766.0 10766.2 12811.0 21194.0
```

The mean (10766.16) did not change - at all - and the median (10766) moved only a single step. 

Nor, as expected, did the extrema.  The main effect was to reduce - dramatically - the spread. The interquartile range (difference between the 3rd and 1st quartiles = 2992) dropped by nearly 1500 steps (from 4453).

## Are there differences in activity patterns between weekdays and weekends?

First, we need to identify which observations are on weekdays. which on weekends. We use the "isWeekday" function from the timeDate package, which gives us a logical variable "weekday", which is then changed to a factor in the activity dataframe.



```r
library(timeDate)
activity$weekday <- factor(isWeekday(as.timeDate(activity$date)),levels=c("TRUE","FALSE"),labels=c("Weekdays","Weekends"))
actweeks <- activity %>% group_by(weekday,interval) %>% summarise(meansteps = mean(insteps))
```

Now we can reproduce the time series plot (Chart 2 above) for weekdays compared to weekends using the Lattice package:


```r
library(lattice)
xyplot(meansteps ~ interval | weekday,actweeks,type="l",layout=c(1,2),main = "CHART 4: Comparison of Weekday v Weekend Activity Levels")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

The evenness of the weekend activity pattern is apparent when compared to the activity spike in the mornings during the week.
