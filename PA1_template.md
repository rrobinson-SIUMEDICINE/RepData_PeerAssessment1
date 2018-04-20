Reproducible Research: Peer Assessment 1
=====================================================

Set up RStudio to work with this assignment.
```{r}
#load libraries
library(knitr)
library(dplyr)
library(ggplot2)
#set echo to be = TRUE
opts_chunk$set(echo = TRUE)
```

## Loading and preprocessing the data
## Be sure to unzip the activity.zip file!

```{r}
## Load data from CSV
researchData <- read.csv('activity.csv')

## How many NAs are there?
sum(is.na(researchData))

##Remove the NAs
researchDataNoNA <- researchData[ with (researchData, { !(is.na(steps)) } ), ]

## Any NAs in new dataset?
sum(is.na(researchDataNoNA))
```


## What is mean total number of steps taken per day?
```{r}
dailysteps <- group_by(researchDataNoNA, date)
stepsPerDay <- summarise(dailysteps, total = sum(steps))
summary(stepsPerDay)

##Histogram of steps per day
hist(stepsPerDay$total, main="Distribution of total steps per day", 
     xlab="Total steps per day")

```


## What is the average daily activity pattern?
```{r}

intervalSteps <- aggregate(steps ~ interval, researchDataNoNA, mean)

# create a time series plot 
plot(intervalSteps$interval, intervalSteps$steps, type='l', 
     main="Mean steps per time interval", xlab="Interval", 
     ylab="Mean number of steps")

## Draw line for mean
abline(h=mean(intervalSteps$steps), col="red", lwd=2)

## Find the interval with the maximum number of steps
max_steps <- which.max(intervalSteps$steps)
intervalSteps[max_steps, ]

```

## Imputing missing values
```{r}
#How many NAs in origional dataset?
sum(is.na(researchData))

## Summary of data in origional dataset
summary(stepsPerDay)

##Impute missing values to be equal to mean of each interval
imputedData <- researchData
for (i in intervalSteps$interval) {
    imputedData[imputedData$interval == i & is.na(imputedData$steps), ]$steps <- 
        intervalSteps$steps[intervalSteps$interval == i]
}

##How many NAs now?
sum(is.na(imputedData))


IMPUTEDdailysteps <- group_by(imputedData, date)
IMPUTEDstepsPerDay <- summarise(IMPUTEDdailysteps, total = sum(steps))

##Histogram of steps per day
hist(IMPUTEDstepsPerDay$total, main="Distribution of total steps per day (Imputed)", 
     xlab="Total steps per day")

#how does the imputedData differ from researchData?
summary(IMPUTEDstepsPerDay)
```


## Are there differences in activity patterns between weekdays and weekends?
```{r}

imputedData['type_of_day'] <- weekdays(as.Date(imputedData$date))
imputedData$type_of_day[imputedData$type_of_day %in% c('Saturday','Sunday') ] <- "Weekend"
imputedData$type_of_day[imputedData$type_of_day != "Weekend"] <- "Weekday"

# convert type_of_day from character to factor
imputedData$type_of_day <- as.factor(imputedData$type_of_day)

# calculate average steps by interval across all days
imputedByInterval <- aggregate(steps ~ interval + type_of_day, imputedData, mean)

# creat a plot
qplot(interval, 
      steps, 
      data = imputedByInterval, 
      type = 'l', 
      geom=c("line"),
      xlab = "Interval", 
      ylab = "Number of steps (Imputed)", 
      main = "") +
  facet_wrap(~ type_of_day, ncol = 1)
```