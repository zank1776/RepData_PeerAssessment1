---
title: "Analyzing Monitoring Activity Data"
output: html_document
---
## Step 1: Load and preprocess data
```{r 1, echo=TRUE}
data <- read.csv("activity.csv")
```
## Step 2: What is the mean total number of steps taken per day?
#### Calculate the total number of steps taken per day.
```{r 2, echo=TRUE}
total.steps <- aggregate(steps ~ date, data, sum)
```
#### Create histogram of steps taken each day.
```{r 3, echo=TRUE}
library(ggplot2)
hist(total.steps$steps, 
     main = "Total Steps by Day",
     xlab = "Number of Steps",
     col = "red")
```


#### Calculate and report the mean and median of the total number of steps taken each day. 
```{r 4, echo=TRUE}
step.mean <- mean(total.steps$steps) 
step.mean #Average of 10766.19 steps per day. 
step.median <- median(total.steps$steps) 
step.median #Median is 10765 steps per day. 
```
## Step 3: What is the average daily activity pattern?
This step requries the creation of a time series plot of five minute intervals and average number of steps taken, averaged across all days. 
```{r 5, echo=TRUE}
step.interval <- aggregate(steps ~ interval, data, mean)
plot(step.interval$interval, 
     step.interval$steps, type = "l",
     main = "Average Steps Taken per Day at Five Minute Interval",
     xlab = "Five Minute Interval",
     ylab = "Average Number of Steps")
```

#### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r 6, echo=TRUE}
max.interval <- step.interval[which.max(step.interval$steps),1] 
max.interval #Interval 835 
```
## Step 4: Imputing missing values
This step requires calculating and reporting the nubmer of NAs in the dataset. There are 2304 NAs in the dataset. 
```{r 7, echo=TRUE}
NAs <- sum(!complete.cases(data)) 
NAs #2304 NAs in the dataset. 
```
#### Devise a strategy for filling in all of the missing values in the dataset.
Interval mean from all days was substituted for all step data that was NA. 
#### Create a new dataset that is equal to the original dataset but with the missing data filled in. 
```{r 8, echo=TRUE}
newData <- data
imputed.data <- transform(newData,
                          steps = ifelse(is.na(newData$steps), 
                        step.interval$steps[match(newData$interval,
                        step.interval$interval)], 
                        newData$steps))
```
#### Make a histogram of the total number of steps taken each day. 
```{r 9,fig.show="hide"}
New.total.step <- aggregate(steps ~ date, imputed.data, sum)
p1 <- hist(New.total.step$steps)
p2 <- hist(total.steps$steps)
```
```{r 10}
plot(p1, col = "blue",
     main = "Histogram of Changes in Data from Replacing NAs",
     xlab = "Total Steps")
plot(p2, col = "red", add = TRUE)
legend("topright", c("Imputed", "Non-imputed"), col=c("blue", "red"), 
       lwd=5, cex = .75)
```

#### Report mean and median and the difference between the first part of the assigment.
The new mean is equal to what it was in the first dataset: 10766.19. The new median shifted by 1.189 after the change in the data. 
```{r 11, echo=TRUE}
new.step.mean <- mean(New.total.step$steps) #Average of 10766.19 steps per day. 

new.step.median <- median(New.total.step$steps) #Median is 10766.19 steps per day. 

diff.mean = new.step.mean - step.mean
diff.mean #difference of 0. 
diff.median = new.step.median - step.median
diff.median #difference of 1.188679
```
## Step 5: What is the difference in activity partterns between weekdays and weekends?
The graphs show that the activity level on weekdays is higher in the mornings and lower through the rest of the day. The weekends have a more consistent, higher activity level than weekdays after interval 1000. 
```{r 12, echo=TRUE}
weekday <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
imputed.data$Day = as.factor(ifelse(is.element(weekdays(as.Date(imputed.data$date)),weekday), "Weekday", "Weekend"))
steps.interval.day <- aggregate(steps~interval+Day, imputed.data, mean)
library(lattice)

xyplot(steps.interval.day$steps ~ steps.interval.day$interval|steps.interval.day$Day,
       main = "Average Steps per Day by Interval",
       xlab = "Interval",
       ylab = "Steps",
       layout = c(1,2),
       type ="l")
```

