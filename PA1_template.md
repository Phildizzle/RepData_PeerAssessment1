# Reproducible Research: Peer Assessment 1
Philipp Knoepfle  
# Introduction
This markdown file contains the solutions for the Reproducible Research Assignment which is part of the Coursera Data Science Specialization. This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day. We will only perform explanatory analysis to highlight features of the data set. Numeration refers to the respective question the instructions asked for.

## Loading and preprocessing the data
First we need to load and process the data.


```r
# First we load the packages for our analysis:
library(dplyr)
library(ggplot2)
library(gridExtra)

# Read in the data
setwd("C:\\Users\\user1\\Desktop\\Data Science\\5. Reproducible Research\\RepData_PeerAssessment1")
unzip("activity.zip")
df <- read.csv("activity.csv", header=T, colClasses = c("numeric", "character", "integer"))
```

1. Let's tidy up the data set and take a look at it

```r
# The date variable was coded as a character although it is a DATE variable
df$date <- as.Date(df$date)
str(df)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
dim(df)
```

```
## [1] 17568     3
```
Looks like we have lots of missing "steps" values, in fact we can calculate how many percent are missing:

```r
sum(is.na(df$steps))/length(df$steps)
```

```
## [1] 0.1311475
```
13.12% is quite a lot. Hence, we need to clean the data set:


```r
df <- df[which(!is.na(df$steps)),]
str(df)
```

```
## 'data.frame':	15264 obs. of  3 variables:
##  $ steps   : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ date    : Date, format: "2012-10-02" "2012-10-02" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
Looks better! Let us start by performing the assignments.

## What is mean total number of steps taken per day?

To get the sum per day, we can use tapply to sum up the number of steps each 5 minute interval per day. The following code accomplishes this:

```r
per_day <- tapply(df$steps, df$date, sum)
per_day <- as.data.frame(per_day)
```

2. We can plot this as a histogram with ggplot:

```r
ggplot(per_day, aes(x = per_day)) +
  geom_histogram(fill = "pink", binwidth = 1000) +
  labs(title = "Histogram of Steps per day", x = "Steps per day", y = " Absolute Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

3. The mean and median number of steps taken each day are respectively:

```r
mean(per_day$per_day)
```

```
## [1] 10766.19
```

```r
median(per_day$per_day)
```

```
## [1] 10765
```

## What is the average daily activity pattern?
4. First, let us plot the average number of steps taken per day

```r
mean_daily_activity<-tapply(df$steps, df$interval, mean)
plot(y = mean_daily_activity, x = names(mean_daily_activity), type = "l", xlab = "5 Minute Interval", main = "Daily Activity Pattern", ylab = "Average number of steps per 5 minute Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

5. Now we want to find out the peak of our chart, i.e. the maximum of the mean:

```r
which.max(mean_daily_activity);max(mean_daily_activity)
```

```
## 835 
## 104
```

```
## [1] 206.1698
```
The interval 835 has on average the highest steps, that is 206.17.

## Imputing missing values
First, we need to re-load the data set since `df` contains no NA values. This is pretty much a no-brainer as we need to impute exactly those values.


```r
dfNA <- read.csv("activity.csv", header=T, colClasses = c("numeric", "character", "integer"))

sum(is.na(dfNA$steps))
```

```
## [1] 2304
```

```r
sum(is.na(dfNA))
```

```
## [1] 2304
```
The data set contains 2304 NA values, all of which are in the `steps` column (as shown by the second sum() call. Now we create a new data set and employ tappls to fill in the missing values with the average number of steps per 5-minute intervall. After that we make sure that there are no missing values.


```r
NAS <- is.na(dfNA$steps)
avg_interval <- tapply(dfNA$steps, dfNA$interval, mean, na.rm=TRUE, simplify=TRUE)
dfNA$steps[NAS] <- avg_interval[as.character(dfNA$interval[NAS])]

sum(is.na(dfNA$steps))
```

```
## [1] 0
```
Looks like we did a good job! Let's plot our results again with ggplot. For this we first need to calculate the means for the NA imputed data set.


```r
# Let us calculate the means for the NA-imputed data set
per_day2 <- tapply(dfNA$steps, dfNA$date, sum)
per_day2 <- as.data.frame(per_day2)

# We rerun the code of histogram 1 and save it into plot1 so we can compare it to plot2 which is the histogram of the imputed data set
plot1 <- ggplot(per_day, aes(x = per_day)) + geom_histogram(fill = "pink", binwidth = 1000) + labs(title = "Histogram of Steps per day", x = "Steps per day", y = " Absolute Frequency")
plot2 <- ggplot(per_day2, aes(x = per_day2)) + geom_histogram(fill = "purple", binwidth = 1000) + labs(title = "Histogram of Steps per day", x = "Steps per day", y = " Absolute Frequency")

grid.arrange(plot1,plot2)
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

It doesn't look like anything has changed at all. Nice colors though, right? Let's see if the mean and median are still the same:


```r
mean(per_day2$per_day2)-mean(per_day$per_day)
```

```
## [1] 0
```

```r
median(per_day2$per_day2)-median(per_day$per_day)
```

```
## [1] 1.188679
```

```r
median(per_day2$per_day2)
```

```
## [1] 10766.19
```
Hereby we have shown that the mean is the same but the median changes marginally to 10766.19 from 10765.

## Are there differences in activity patterns between weekdays and weekends?
Last but not least we are asked to check whether there are differencesbetween weekdays and weekends in the activity pattern. 
We are going to do this by creating a new factor variable in the data set with two levels "weekday" and "weekend". For this we will use the `weekdays()` function in `R`.


```r
df_week <- read.csv("activity.csv", header=T, colClasses = c("numeric", "character", "integer"))
df_week$date <- as.Date(df_week$date)
df_week <- df_week[which(!is.na(df_week$steps)),]

df_week <- mutate(df_week, weektype = ifelse(weekdays(df_week$date) == "Samstag" | weekdays(df_week$date) == "Sonntag", "Wochenende", "Wochentag"))

df_week$weektype <- as.factor(df_week$weektype)
str(df_week)
```

```
## 'data.frame':	15264 obs. of  4 variables:
##  $ steps   : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ date    : Date, format: "2012-10-02" "2012-10-02" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ weektype: Factor w/ 2 levels "Wochenende","Wochentag": 2 2 2 2 2 2 2 2 2 2 ...
```

Now that we have our factor variable we may simply plot the results and compare them:

```r
interval_full <- df_week %>% group_by(interval, weektype) %>% summarise(steps=mean(steps))

ggplot(interval_full, aes(x=interval, y=steps, color = weektype)) +
  geom_line() +
  facet_wrap(~weektype, ncol = 1, nrow=2)
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

This is our final plot. Since my OS (and consequently R) are set up in German the `Weekdays()` function delivers German output. 'Wochentag' refers to weekdays while 'Wochenende' refers to weekends. From the plot we can see that people like to sleep in on the weekend. That is, the activity in the early hours is higher on work days than on the weekend. Second, we could possibly infer that most people remain relatively inactive during weekdays as the activity during the day appears quite low. This could imply that they are having desk jobs or are couch potatoes. Consequently, the activity on weekends is higher. People apparently like to get stuff done or maybe go for a walk or to the gym. The end! Thanks for reading!


