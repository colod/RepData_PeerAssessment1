# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
activity<- read.csv("activity.csv", sep=",", header=TRUE, na.strings="NA")
activity$date<-as.Date(activity$date, format="%Y-%m-%d")
activity$days<-weekdays(activity$date)
activity$days<-factor(activity$days)
activity$date<-factor(activity$date)
activity$hour<-factor(floor(activity$interval/100))
activity$minute<-factor(activity$interval %% 100)
```
## What is mean total number of steps taken per day?
This calculates the total number of steps per day by taking the sum over each date of the steps, counting NAs as 0.

```r
dailytotals<-data.frame(levels(factor(activity$date)), tapply(activity$steps, activity$date, sum, na.rm=T))
names(dailytotals)<-c("Date", "Total Steps")
dailytotals
```

```
##                  Date Total Steps
## 2012-10-01 2012-10-01           0
## 2012-10-02 2012-10-02         126
## 2012-10-03 2012-10-03       11352
## 2012-10-04 2012-10-04       12116
## 2012-10-05 2012-10-05       13294
## 2012-10-06 2012-10-06       15420
## 2012-10-07 2012-10-07       11015
## 2012-10-08 2012-10-08           0
## 2012-10-09 2012-10-09       12811
## 2012-10-10 2012-10-10        9900
## 2012-10-11 2012-10-11       10304
## 2012-10-12 2012-10-12       17382
## 2012-10-13 2012-10-13       12426
## 2012-10-14 2012-10-14       15098
## 2012-10-15 2012-10-15       10139
## 2012-10-16 2012-10-16       15084
## 2012-10-17 2012-10-17       13452
## 2012-10-18 2012-10-18       10056
## 2012-10-19 2012-10-19       11829
## 2012-10-20 2012-10-20       10395
## 2012-10-21 2012-10-21        8821
## 2012-10-22 2012-10-22       13460
## 2012-10-23 2012-10-23        8918
## 2012-10-24 2012-10-24        8355
## 2012-10-25 2012-10-25        2492
## 2012-10-26 2012-10-26        6778
## 2012-10-27 2012-10-27       10119
## 2012-10-28 2012-10-28       11458
## 2012-10-29 2012-10-29        5018
## 2012-10-30 2012-10-30        9819
## 2012-10-31 2012-10-31       15414
## 2012-11-01 2012-11-01           0
## 2012-11-02 2012-11-02       10600
## 2012-11-03 2012-11-03       10571
## 2012-11-04 2012-11-04           0
## 2012-11-05 2012-11-05       10439
## 2012-11-06 2012-11-06        8334
## 2012-11-07 2012-11-07       12883
## 2012-11-08 2012-11-08        3219
## 2012-11-09 2012-11-09           0
## 2012-11-10 2012-11-10           0
## 2012-11-11 2012-11-11       12608
## 2012-11-12 2012-11-12       10765
## 2012-11-13 2012-11-13        7336
## 2012-11-14 2012-11-14           0
## 2012-11-15 2012-11-15          41
## 2012-11-16 2012-11-16        5441
## 2012-11-17 2012-11-17       14339
## 2012-11-18 2012-11-18       15110
## 2012-11-19 2012-11-19        8841
## 2012-11-20 2012-11-20        4472
## 2012-11-21 2012-11-21       12787
## 2012-11-22 2012-11-22       20427
## 2012-11-23 2012-11-23       21194
## 2012-11-24 2012-11-24       14478
## 2012-11-25 2012-11-25       11834
## 2012-11-26 2012-11-26       11162
## 2012-11-27 2012-11-27       13646
## 2012-11-28 2012-11-28       10183
## 2012-11-29 2012-11-29        7047
## 2012-11-30 2012-11-30           0
```
This shows a histogram of the total steps per day in the dataset.

```r
hist(dailytotals$"Total Steps", main="Histogram of Total Steps per Day", xlab="Total Steps per Day")
```

![](PA1_template_files/figure-html/histogram-1.png) 

This shows the mean total steps per day in the dataset.

```r
meansteps<-summary(dailytotals$"Total Steps")[4]
meansteps
```

```
## Mean 
## 9354
```

This shows the median total steps per day in the dataset.

```r
summary(dailytotals$"Total Steps")[3]
```

```
## Median 
##  10400
```
## What is the average daily activity pattern?
Here is a graph of the average number of steps taken per five minute interval.

```r
timeaverages<-data.frame(as.numeric(levels(factor(activity$interval))), tapply(activity$steps, activity$interval, mean, na.rm=T))
names(timeaverages)<-c("Interval", "Average Steps")
par(lab=c(23,5,7))
plot(timeaverages, type="l")
```

![](PA1_template_files/figure-html/line_graph_interval-1.png) 

The maximum activity interval is shown below.

```r
timeaverages[which(timeaverages$"Average Steps"==max(timeaverages$"Average Steps")),]
```

```
##     Interval Average Steps
## 835      835      206.1698
```
## Imputing missing values
Total number of missing values for steps by date and interval.

```r
length(which(is.na(activity$steps)))
```

```
## [1] 2304
```
Total number of missing days.

```r
dailytotals2<-data.frame(levels(factor(activity$date)), tapply(activity$steps, activity$date, sum))
names(dailytotals2)<-c("Date", "Total Steps")
length(which(is.na(dailytotals2$"Total Steps")))
```

```
## [1] 8
```
Imputing the mean number of steps per day as a replacement for the missing data points in the average daily activity pattern data.

```r
dailytotals2[which(is.na(dailytotals2$"Total Steps")),2]<-meansteps
dailytotals2
```

```
##                  Date Total Steps
## 2012-10-01 2012-10-01        9354
## 2012-10-02 2012-10-02         126
## 2012-10-03 2012-10-03       11352
## 2012-10-04 2012-10-04       12116
## 2012-10-05 2012-10-05       13294
## 2012-10-06 2012-10-06       15420
## 2012-10-07 2012-10-07       11015
## 2012-10-08 2012-10-08        9354
## 2012-10-09 2012-10-09       12811
## 2012-10-10 2012-10-10        9900
## 2012-10-11 2012-10-11       10304
## 2012-10-12 2012-10-12       17382
## 2012-10-13 2012-10-13       12426
## 2012-10-14 2012-10-14       15098
## 2012-10-15 2012-10-15       10139
## 2012-10-16 2012-10-16       15084
## 2012-10-17 2012-10-17       13452
## 2012-10-18 2012-10-18       10056
## 2012-10-19 2012-10-19       11829
## 2012-10-20 2012-10-20       10395
## 2012-10-21 2012-10-21        8821
## 2012-10-22 2012-10-22       13460
## 2012-10-23 2012-10-23        8918
## 2012-10-24 2012-10-24        8355
## 2012-10-25 2012-10-25        2492
## 2012-10-26 2012-10-26        6778
## 2012-10-27 2012-10-27       10119
## 2012-10-28 2012-10-28       11458
## 2012-10-29 2012-10-29        5018
## 2012-10-30 2012-10-30        9819
## 2012-10-31 2012-10-31       15414
## 2012-11-01 2012-11-01        9354
## 2012-11-02 2012-11-02       10600
## 2012-11-03 2012-11-03       10571
## 2012-11-04 2012-11-04        9354
## 2012-11-05 2012-11-05       10439
## 2012-11-06 2012-11-06        8334
## 2012-11-07 2012-11-07       12883
## 2012-11-08 2012-11-08        3219
## 2012-11-09 2012-11-09        9354
## 2012-11-10 2012-11-10        9354
## 2012-11-11 2012-11-11       12608
## 2012-11-12 2012-11-12       10765
## 2012-11-13 2012-11-13        7336
## 2012-11-14 2012-11-14        9354
## 2012-11-15 2012-11-15          41
## 2012-11-16 2012-11-16        5441
## 2012-11-17 2012-11-17       14339
## 2012-11-18 2012-11-18       15110
## 2012-11-19 2012-11-19        8841
## 2012-11-20 2012-11-20        4472
## 2012-11-21 2012-11-21       12787
## 2012-11-22 2012-11-22       20427
## 2012-11-23 2012-11-23       21194
## 2012-11-24 2012-11-24       14478
## 2012-11-25 2012-11-25       11834
## 2012-11-26 2012-11-26       11162
## 2012-11-27 2012-11-27       13646
## 2012-11-28 2012-11-28       10183
## 2012-11-29 2012-11-29        7047
## 2012-11-30 2012-11-30        9354
```
This shows a histogram of the total steps per day in the imputed dataset. As you can see, there is a bump in the 5000-10000 bin because we added 8 dates with the average.

```r
hist(dailytotals2$"Total Steps", main="Histogram of Total Steps per Day", xlab="Total Steps per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 


This shows the mean total steps per day in the imputed dataset.

```r
summary(dailytotals2$"Total Steps")[4]
```

```
##  Mean 
## 10580
```

This shows the median total steps per day in the imputed dataset.

```r
summary(dailytotals2$"Total Steps")[3]
```

```
## Median 
##  10400
```
The original number of total steps:

```r
sum(dailytotals$"Total Steps", na.rm=TRUE)
```

```
## [1] 570608
```
The number of total steps in the imputed dataset:

```r
sum(dailytotals2$"Total Steps")
```

```
## [1] 645440
```
The increase is simply 8 times the mean, as expected.

```r
sum(dailytotals2$"Total Steps")-sum(dailytotals$"Total Steps", na.rm=TRUE)
```

```
## [1] 74832
```

## Are there differences in activity patterns between weekdays and weekends?
We create a factor variable called weekend on the imputed data to differentiate between the two.

```r
dailytotals2$Date<-as.Date(dailytotals2$Date)
dailytotals2$day<-weekdays(dailytotals2$Date)
weekends<-which(dailytotals2$day=="Saturday" | dailytotals2$day=="Sunday")
dailytotals2$weekend<-"weekday"
dailytotals2[weekends,4]<-"weekend"
dailytotals2
```

```
##                  Date Total Steps       day weekend
## 2012-10-01 2012-10-01        9354    Monday weekday
## 2012-10-02 2012-10-02         126   Tuesday weekday
## 2012-10-03 2012-10-03       11352 Wednesday weekday
## 2012-10-04 2012-10-04       12116  Thursday weekday
## 2012-10-05 2012-10-05       13294    Friday weekday
## 2012-10-06 2012-10-06       15420  Saturday weekend
## 2012-10-07 2012-10-07       11015    Sunday weekend
## 2012-10-08 2012-10-08        9354    Monday weekday
## 2012-10-09 2012-10-09       12811   Tuesday weekday
## 2012-10-10 2012-10-10        9900 Wednesday weekday
## 2012-10-11 2012-10-11       10304  Thursday weekday
## 2012-10-12 2012-10-12       17382    Friday weekday
## 2012-10-13 2012-10-13       12426  Saturday weekend
## 2012-10-14 2012-10-14       15098    Sunday weekend
## 2012-10-15 2012-10-15       10139    Monday weekday
## 2012-10-16 2012-10-16       15084   Tuesday weekday
## 2012-10-17 2012-10-17       13452 Wednesday weekday
## 2012-10-18 2012-10-18       10056  Thursday weekday
## 2012-10-19 2012-10-19       11829    Friday weekday
## 2012-10-20 2012-10-20       10395  Saturday weekend
## 2012-10-21 2012-10-21        8821    Sunday weekend
## 2012-10-22 2012-10-22       13460    Monday weekday
## 2012-10-23 2012-10-23        8918   Tuesday weekday
## 2012-10-24 2012-10-24        8355 Wednesday weekday
## 2012-10-25 2012-10-25        2492  Thursday weekday
## 2012-10-26 2012-10-26        6778    Friday weekday
## 2012-10-27 2012-10-27       10119  Saturday weekend
## 2012-10-28 2012-10-28       11458    Sunday weekend
## 2012-10-29 2012-10-29        5018    Monday weekday
## 2012-10-30 2012-10-30        9819   Tuesday weekday
## 2012-10-31 2012-10-31       15414 Wednesday weekday
## 2012-11-01 2012-11-01        9354  Thursday weekday
## 2012-11-02 2012-11-02       10600    Friday weekday
## 2012-11-03 2012-11-03       10571  Saturday weekend
## 2012-11-04 2012-11-04        9354    Sunday weekend
## 2012-11-05 2012-11-05       10439    Monday weekday
## 2012-11-06 2012-11-06        8334   Tuesday weekday
## 2012-11-07 2012-11-07       12883 Wednesday weekday
## 2012-11-08 2012-11-08        3219  Thursday weekday
## 2012-11-09 2012-11-09        9354    Friday weekday
## 2012-11-10 2012-11-10        9354  Saturday weekend
## 2012-11-11 2012-11-11       12608    Sunday weekend
## 2012-11-12 2012-11-12       10765    Monday weekday
## 2012-11-13 2012-11-13        7336   Tuesday weekday
## 2012-11-14 2012-11-14        9354 Wednesday weekday
## 2012-11-15 2012-11-15          41  Thursday weekday
## 2012-11-16 2012-11-16        5441    Friday weekday
## 2012-11-17 2012-11-17       14339  Saturday weekend
## 2012-11-18 2012-11-18       15110    Sunday weekend
## 2012-11-19 2012-11-19        8841    Monday weekday
## 2012-11-20 2012-11-20        4472   Tuesday weekday
## 2012-11-21 2012-11-21       12787 Wednesday weekday
## 2012-11-22 2012-11-22       20427  Thursday weekday
## 2012-11-23 2012-11-23       21194    Friday weekday
## 2012-11-24 2012-11-24       14478  Saturday weekend
## 2012-11-25 2012-11-25       11834    Sunday weekend
## 2012-11-26 2012-11-26       11162    Monday weekday
## 2012-11-27 2012-11-27       13646   Tuesday weekday
## 2012-11-28 2012-11-28       10183 Wednesday weekday
## 2012-11-29 2012-11-29        7047  Thursday weekday
## 2012-11-30 2012-11-30        9354    Friday weekday
```
