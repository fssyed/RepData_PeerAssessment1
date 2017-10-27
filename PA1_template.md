
knitr::opts_chunk$set(echo = TRUE)


# Coursera Reporducible Research
##PA1 - Faraz S.

 
```{r}
setwd("~/R/Working Dir")
library(ggplot2)
library(scales)
library(Hmisc)
```

First loading the data

```{r}

if(!file.exists('activity.csv')){
    unzip('activity.zip')}
activityData <- read.csv('activity.csv')

```


## What is mean total number of steps taken per day?

Storing the total steps by date
```{r}

stepsByDay <- tapply(activityData$steps, activityData$date, sum, na.rm=TRUE)
```

Histogram of the total number of steps taken each day
```{r}
qplot(stepsByDay, xlab='Total steps per day', ylab='Frequency using binwith 500', binwidth=500)

```

What is the mean and median total number of steps taken per day
```{r}
stepsByDayMean <- mean(stepsByDay)
stepsByDayMedian <- median(stepsByDay)
```

Mean: `r stepsByDayMean` steps

Median: `r stepsByDayMedian` steps

## What is the average daily activity pattern?

```{r}
daysum <- tapply(activityData$steps,activityData$date,sum)
averagesday <- tapply(activityData$steps,activityData$date,mean)
plot(averagesday,type="l", xlab="Day", ylab="Avg Steps per interval")
```



2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r}
intervalmax <- aggregate(steps ~ interval, activityData, mean)
stepsmax <- which.max(intervalmax$steps)
MostSteps <- intervalmax[stepsmax,]
```


Most Steps on average are found at interval ID: 

`r MostSteps` steps


## Inputing Missing Values

Calculate and report the total number of missing values in the dataset
```{r}
numMissingValues <- length(which(is.na(activityData$steps)))
```

Number of missing values is `r numMissingValues`


2. Devise a strategy for filling in all of the missing values in the dataset.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in. Using 0 to replace the NA
```{r}
activityData2 <- activityData
activityData2$steps <- impute(activityData$steps, 0)
```

Make a histogram of the total number of steps taken each day, and recalculate the mean and median
```{r}
stepsByDayImputed <- tapply(activityData2$steps, activityData2$date, sum)
qplot(stepsByDayImputed, xlab='Total steps per day (Imputed)', ylab='Frequency using binwith 500', binwidth=500)
```

The mean and median total number of steps taken per day
```{r}
stepsByDayMeanImputed <- mean(stepsByDayImputed)
stepsByDayMedianImputed <- median(stepsByDayImputed)
```

1. Mean (Imputed): `r stepsByDayMeanImputed` steps
2. Median (Imputed): `r stepsByDayMedianImputed` steps

1. Mean(Original): `r stepsByDayMean` steps
2. Median(Original): `r stepsByDayMedian` steps

When replacing the NA with '0' there are no differnces in the calculated mean and median.

##Activity Patterns between Weekdays and Weekends

Create a new factor variable in the dataset with two levels ??? ???weekday??? and ???weekend??? indicating whether a given date is a weekday or weekend day.
```{r}
activityData2$dateType <-  ifelse(as.POSIXlt(activityData2$date)$wday %in% c(0,6), 'weekend', 'weekday')
```

Make a panel plot containing a time series plot
```{r}
averagedActivityDataImputed <- aggregate(steps ~ interval + dateType, data=activityData2, mean)
ggplot(averagedActivityDataImputed, aes(interval, steps)) + 
    geom_line() + 
    facet_grid(dateType ~ .) +
    xlab("5-minute interval") + 
    ylab("avarage number of steps")
```

