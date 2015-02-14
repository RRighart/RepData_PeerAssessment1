---
title: "course-project1"
author: "Ruthger Righart"
date: "13.02.2015"
output: html_document
---

#Activity monitoring data

### Loading and preprocessing the data


```r
dat<-read.csv("/home/righart/Documents/prs/Coursera-Reproducible/course-project1/activity.csv")
head(dat)
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
### What is mean total number of steps taken per day?

The total number of steps taken per day


```r
library(plyr)
day<-ddply(dat, .(date), summarize, "total" = sum(steps, na.rm=FALSE)) 
```

A histogram of the total number of steps taken each day.


```r
hist(day$total, breaks=100, xlab="Average steps per day", ylab="frequency", main="Histogram", col="blue")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

The mean and median of the total number of steps taken per day


```r
mean(day$total, na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(day$total, na.rm=TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern

Time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

The 5 minute interval that contains the max number of steps across all the days


```r
library(plyr)
newdat<-ddply(dat, .(interval), summarize, avgstepsint = mean(steps, na.rm=TRUE)) 
newdat[which(newdat$avgstepsint == max(newdat$avgstepsint)), 1]
```

```
## [1] 835
```

## Imputing missing values

Number of missing values in the dataset (i.e., total number of rows with NAs).

As some rows could have double NA's, the command which(is.na(dat)) would give an incorrect value. For this reason I have searched in every column for NA's; after that I have taken the unique values and calculated the number of NA's by taking length of the vector temp.


```r
temp1<-(which(is.na(dat[,1])))
temp2<-(which(is.na(dat[,2])))
temp3<-(which(is.na(dat[,3])))

temp<-sort(unique(c(temp1, temp2, temp3)), decreasing=FALSE)
length(temp)
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

The strategy I used was to fill-in NA's based on the average of 5min interval


```r
newdat<-ddply(dat, .(interval), summarize, avgstepsint = mean(steps, na.rm=TRUE)) #contains avg interval values

temp1<-(which(is.na(dat[,1])))
temp2<-(which(is.na(dat[,2])))
temp3<-(which(is.na(dat[,3])))

temp<-sort(unique(c(temp1, temp2, temp3)), decreasing=FALSE)

fil <- cbind(dat, newdat)
fil[temp, 1] <- fil[temp, 5]
```
Create new dataset that is equal to the original dataset but with the missing values filled-in 


```r
dat2<-fil[,1:3]
```

A histogram of the total number of steps taken each day


```r
library(plyr)
day2<-ddply(dat2, .(date), summarize, "total" = sum(steps, na.rm=FALSE)) 
hist(day2$total, breaks=100, xlab="steps", ylab="frequency", col="green", main="Histogram after imputing missing values")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

The mean and median of the total number of steps taken per day.

Mean and median are similar to the previous measurement. However, the distribution of the histogram changes.


```r
mean(day2$total, na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(day2$total, na.rm=TRUE)
```

```
## [1] 10766.19
```
## Are there differences in activity patterns between weekdays and weekends?

A new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day


```r
w<-as.Date(dat2$date)
wk<-as.factor(weekdays(w))
levels(wk) <- c(levels(wk), "Week", "Weekend")

wk <- replace(wk, wk=="Samstag", "Weekend")
wk <- replace(wk, wk=="Sonntag", "Weekend")
wk <- replace(wk, wk=="Montag", "Week")
wk <- replace(wk, wk=="Dienstag", "Week")
wk <- replace(wk, wk=="Mittwoch", "Week")
wk <- replace(wk, wk=="Donnerstag", "Week")
wk <- replace(wk, wk=="Freitag", "Week")
dat2$di<-wk
levels(dat2$di)
```

```
## [1] "Dienstag"   "Donnerstag" "Freitag"    "Mittwoch"   "Montag"    
## [6] "Samstag"    "Sonntag"    "Week"       "Weekend"
```

```r
table(dat2$di)
```

```
## 
##   Dienstag Donnerstag    Freitag   Mittwoch     Montag    Samstag 
##          0          0          0          0          0          0 
##    Sonntag       Week    Weekend 
##          0      12960       4608
```

A panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
library(plyr)

weekend<-subset(dat2, wk=="Weekend")
week<-subset(dat2, wk=="Week")

nweekend<-ddply(weekend, .(interval), summarize, avgstepsint = mean(steps, na.rm=TRUE)) 
nweek<-ddply(week, .(interval), summarize, avgstepsint = mean(steps, na.rm=TRUE)) 

plot(nweekend$avgstepsint ~ nweekend$interval, type="l", xlab="intervals", ylab="Number of steps averaged across days", main="Activity levels", col="green", lwd=2, ylim=c(0,250), xlim=c(0,2300))
lines(nweek$avgstepsint ~ nweek$interval, type="l", col="red", lwd=2)
legend(x="topleft", legend=c("Week","Weekend"), col=c("red","green"), lwd=2, lty=c(1,1), cex=0.75)
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png) 





