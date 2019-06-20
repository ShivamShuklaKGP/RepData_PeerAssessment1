# Course Project 1

### Loading necessary packages

```r
library(dplyr)
library(lubridate)
```

### Downloading file from the internet link and extracting it


```r
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip","activity.zip")

unzip("activity.zip")
```


### Reading the file into a dataset and converting it into a tibble 


```r
activity<-read.csv("activity.csv")
activity<-tbl_df(activity)
activity
```

```
## # A tibble: 17,568 x 3
##    steps date       interval
##    <int> <fct>         <int>
##  1    NA 2012-10-01        0
##  2    NA 2012-10-01        5
##  3    NA 2012-10-01       10
##  4    NA 2012-10-01       15
##  5    NA 2012-10-01       20
##  6    NA 2012-10-01       25
##  7    NA 2012-10-01       30
##  8    NA 2012-10-01       35
##  9    NA 2012-10-01       40
## 10    NA 2012-10-01       45
## # ... with 17,558 more rows
```

Converting the date variable in the activity dataset to date variable using lubridate function ymd.


```r
activity<-mutate(activity,date=ymd(date))
```

## What is the mean total number of steps taken per day? 

### Step1: Summing up steps for each day
For that we have to first group our dataset by each day, and then sum the steps for each day by either ignoring the steps given as NA or taking them as zero. For this we employ group_by,summarise and sum function, along with na.rm=TRUE.


```r
sum_steps<-group_by(activity,date)%>%
  summarise(sum_day=sum(steps,na.rm=TRUE))
sum_steps
```

```
## # A tibble: 61 x 2
##    date       sum_day
##    <date>       <int>
##  1 2012-10-01       0
##  2 2012-10-02     126
##  3 2012-10-03   11352
##  4 2012-10-04   12116
##  5 2012-10-05   13294
##  6 2012-10-06   15420
##  7 2012-10-07   11015
##  8 2012-10-08       0
##  9 2012-10-09   12811
## 10 2012-10-10    9900
## # ... with 51 more rows
```

### Step 2: Calculating mean of steps taken per day

We use the tibble sum_steps and its colum sum_day


```r
mean_steps_day<-mean(sum_steps$sum_day)
print(mean_steps_day)
```

```
## [1] 9354.23
```

```r
median_steps_day<-median(sum_steps$sum_day)
print(median_steps_day)
```

```
## [1] 10395
```


## What is the average daily activity pattern?

In this case we have to group the activity by interval as we want mean of each activity over all the days.Thus, we use group_by function and summarise function along with mean to obtain the required dataset of values.

Also, we can develop time series the plot by simply using the plot function with appropriate attributes.


```r
mean_interval<-group_by(activity,interval)%>%
  summarise(mean_steps=mean(steps,na.rm=TRUE))

##Plotting the time series plot
plot(mean_interval,type="l",ylab="Mean number of steps taken",xlab="Interval",col="blue")
```

![plot of chunk Time series plot and mean of steps by activity, ](figure/Time series plot and mean of steps by activity, -1.png)

## Imputing missing values

Step 1: Number of missing values in Dataset.


```r
missing_values<-sum(is.na(activity$steps))
missing_values
```

```
## [1] 2304
```

Step 2: Imputing the missing values-

Here first we find the index of values which are missing:

```r
vector_na<-is.na(activity$steps)
index_na<-which(is.na(activity$steps))
```

Next we find the intervals of all the missing values.


```r
intervals_na<-activity$interval[vector_na]
```

Now since we have the intervals and also mean of values of intervals by day. Thus we can use them to impute mean values in the respective missing values.

```r
i<-1
for( x in intervals_na){
  activity$steps[index_na[i]]<-mean_interval$mean_steps[which(mean_interval$interval==x)]
  i<-i+1
}

## Now checking if there are any missing values
sum(is.na(activity$steps))
```

```
## [1] 0
```

## Are there differences in activity patterns between weekdays and weekends?

Step 1: Categorising data into weekday and weekend


```r
weekend<-c("Saturday", "Sunday")
activity<-mutate(activity,weekend=weekdays(date) %in% weekend)
activity$weekend<-as.numeric(activity$weekend)
table(activity$weekend)
```

```
## 
##     0     1 
## 12960  4608
```

Step 2: Plotting the time series plot

```r
mean_interval<-group_by(activity,interval,weekend)%>%
  summarise(mean_steps=mean(steps,na.rm=TRUE))
library(ggplot2)
```


```r
##Plotting the time series plot

ggplot(mean_interval,aes(x=interval,y=mean_steps))+geom_line(col="blue",aes(alpha=0.3))+facet_wrap(.~weekend)+theme_bw()
```

![plot of chunk Plotting](figure/Plotting-1.png)

Here the 0 labeled panel gives us the plot of Weekdays whereas 1 labeled panel of the plot provides us insights into the activity on Weekends. 
We can easly infer from these plots that the activity varies significantly based on whether it's a weekend or a weekday.
