---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---




This file descripts my course project. reproducible research assignment.

##0. Install and Load Packages

```r
library(lattice)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

##1. Loading and preprocessing the data 

1. Load the data (i.e. read.csv())
2. Process/transform the data (if necessary) into a format suitable for your analysis

###1.1 Downloading Data

we download original data from this website: 

Dataset:[Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)[52K]

also, we can download in Rstudio console by R command:

```r
## download dataset to R current work directory,filename "activity.zip"
##  download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip","./acticity.zip")

## decompressed dataset to current work directory
##  unzip("activity.zip",exdir=".")


## sometimes, I can't download dataset using URL, so I annotated them
## I assume that you have downloaded and decompressed the data into the current directory.

## loading dataset to R console,stored at variable "activity""
  activity <- read.csv("activity.csv")
```

###1.2 Check Data

Now, we can simply check dataset:

```r
dim(activity)
```

```
## [1] 17568     3
```

```r
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
head(activity)
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
## change data format
activity$date <- as.Date(activity$date,"%Y-%m-%d")
## calculate NA's number in column one
sum(is.na(activity$steps))/nrow(activity)
```

```
## [1] 0.1311475
```

##2. What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day

2. Calculate and report the mean and median of the total number of steps taken per day

###2.1 Total number of steps

Fist,we can calculate total number of steps taken each day:

```r
fixed_data <- na.omit(activity)
step_day <- tapply(fixed_data$steps,fixed_data$date,sum)
day_names <-names(step_day)
step_day <- data.frame(step = step_day,day=day_names)
hist(step_day$step,main="Total number of steps taken each day",xlab="steps",ylab="Frequency",col="purple")

###Next, we calculate **mean** and **median**, add each lines to histgram.
## mean calculate
abline(v=mean(step_day$step),col="red")
## median calculate
abline(v=median(step_day$step),col="blue",lty=3)
## add legend descript lines
legend("topleft",legend=c("mean","median"),col=c("red","blue"),lty=c(1,3),bty="n",text.col = c("red","blue"),yjust=3)
```

![](PA1_template_files/figure-html/step/day-1.png)<!-- -->

```r
step_mean <- mean(step_day$step)
step_median <- median(step_day$step)
step_mean
```

```
## [1] 10766.19
```

```r
step_median
```

```
## [1] 10765
```

###2.2 Calculate mean and median value

the mean of the total number of steps taken per day is 1.0766189\times 10^{4}.

the median of the total number of steps taken per day is 10765.

##3. What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

###3.1 Times series analysis

This section we analysis time series problem
First, we need calculate average steps for each day each interval

```r
## each day records number
interval <- tapply(activity$steps,activity$interval,sum,na.rm=TRUE)
interval <- interval/61 ##normalize data,total 61 days
plot(names(interval),interval,type="l",main="average steps for each day/5 min",col="blue",lwd=2.0,xlab="One day time series/5 min",ylab="steps")
```

![](PA1_template_files/figure-html/ava_steps_each_day-1.png)<!-- -->

###3.2 Calculate specific interval

So, we can easily find which 5-minute interval contains the maximun number of steps

```r
max_step<- which(interval == max(interval))
max_step
```

```
## 835 
## 104
```
The data tells us,these two month,every day's morning 8:35 our research object had maximum number of steps:104 

##4. Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
2. Create a new dataset that is equal to the original dataset but with the missing data filled in.
3. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

###4.1 The total number of row with NA

NA records all comes from steps avariable. but we can use `complete.cases()` function to calculate total rows contain NA value.


```r
sum(!complete.cases(activity))
```

```
## [1] 2304
```

```r
## Above number of day/interval contains NAs records.
```

###4.2 Refilling NA obeservations

I used that day's step mean value to refill NA oberservation

```r
## calculte every day total interval records number
24*60/5
```

```
## [1] 288
```

```r
## every day we will record 288 times step
step_day2 <- tapply(activity$steps,activity$date,sum,na.rm=TRUE)
day_names2 <-names(step_day2)
step_day2 <- data.frame(step = step_day2,day=day_names2)
step_day2$average <- step_day2$step/288
## calculate mean of steps  each day,stored at variable: ave_step
ave_step <- rep(step_day2$average,each=288)

refill_data <- activity
for (i in 1:nrow(refill_data)){
  if (is.na(refill_data$steps[i])){
    refill_data$steps[i] <- ave_step[i]
  }
}
head(refill_data);tail(refill_data)
```

```
##   steps       date interval
## 1     0 2012-10-01        0
## 2     0 2012-10-01        5
## 3     0 2012-10-01       10
## 4     0 2012-10-01       15
## 5     0 2012-10-01       20
## 6     0 2012-10-01       25
```

```
##       steps       date interval
## 17563     0 2012-11-30     2330
## 17564     0 2012-11-30     2335
## 17565     0 2012-11-30     2340
## 17566     0 2012-11-30     2345
## 17567     0 2012-11-30     2350
## 17568     0 2012-11-30     2355
```

```r
sum(!complete.cases(refill_data))
```

```
## [1] 0
```

###4.3 Recalculte mean and median


```r
step_day2 <- tapply(refill_data$steps,refill_data$date,sum)
step_day2 <- data.frame(step = step_day2,day=names(step_day2))
## plotting refilled data
hist(step_day2$step,main="Total number of steps taken each day(refilled NAs)",xlab="steps",ylab="Frequency",col="purple")
```

![](PA1_template_files/figure-html/refill_data-1.png)<!-- -->

```r
## calculate mean and median
step_mean2 <- mean(step_day2$step)
step_median2 <- median(step_day2$step)
```
we can see refilled data mean=9354.2295082,median=1.0395\times 10^{4}.
The Result shows the difference between mean value, because we delete row contains NA before,so second mean larger than first mean.

##5. Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

###5.1 Determine weekdays or weekends


```r
## created new variable contains "weekend" and"weekday" sequence
for( i in 1:nrow(refill_data)){
  if (weekdays(refill_data$date[i])=="Saturday"|weekdays(refill_data$date[i])=="Sunday"){
    refill_data$week[i] <- "Weekend"
  }else{
    refill_data$week[i] <-"Weekday"
  }
}
```

###5.2 Plotting the difference

we can sovle this part problem like before.

```r
## select specific subset data
weekend <- filter(refill_data,refill_data$week =="Weekend")
weekday <- filter(refill_data,refill_data$week =="Weekday")
wd_step <- tapply(weekend$steps,weekend$interval,sum)
wy_step <- tapply(weekday$steps,weekday$interval,sum)
## creating normalized interval steps data
wd_step <- wd_step/16
wy_step <- wy_step/45
## create data.frame format dataset
wd_step <- data.frame(steps=wd_step,interval=names(wd_step),week=rep("weekend",288))
wy_step <- data.frame(steps=wy_step,interval=names(wy_step),week=rep("weekday",288))
week_step <- rbind(wd_step,wy_step)

## change interval variable to numeric
week_step$interval <- as.character(week_step$interval)
week_step$interval <- as.numeric(week_step$interval)


## plotting by lattice package function `xyplot`
xyplot(steps~interval|week,data=week_step,type="l",layout=c(1,2),stack=TRUE,lwd=2)
```

![](PA1_template_files/figure-html/ploting-1.png)<!-- -->

we can find that the person's weekend is similar to the work day.

Now, we finished the course project












  
  
  
  


