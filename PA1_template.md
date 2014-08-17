# Reproducible Research: Peer Assessment 1
## Loading and preprocessing the data

First of all call for the required libraries for the researh:
```{r}
library(plyr)
library(lattice)
```

Load the data and assign data to the variable x:
```{r}
x <- read.csv("activity.csv", sep = ",")
str(x)
```

Then transform the data into suitable formats.
So, date will be in "Date" format, "steps" and "interval" will be numeric
```{r}
x[, 1] <- as.numeric(x[, 1])
x[, 2] <- as.Date(x[, 2])
x[, 3] <- as.numeric(x[, 3])
```

Clear the data by creating "xc" object for "x clear" that shows complete cases in "x"
```{r}
xc <- x[complete.cases(x), ]
```


## What is mean total number of steps taken per day?

Create data frame named "spd" for steps per date, where the first column 
corresponds to unique dates and the second column corresponds steps in this day
```{r}
spd <- ddply(xc, c("date"), function(x) apply(x[1], 2, sum)) 
```

Make a histogram of the total number of steps taken each day
```{r}
hist(spd[, 2], xlab = "Steps per Day", 
     main = "Histogram of Steps per Day", col = "blue")
```

Calculate mean and median total number of steps taken per day
```{r}
mean(spd[, 2])
```
[1] 10766

```{r}
median(spd[, 2])
```
[1] 10765


## What is the average daily activity pattern?

Create data frame named "spi" for steps per interval, where the first column 
corresponds to unique intervals and the second column corresponds the mean steps in this day
```{r}
spi <- ddply(xc, c("interval"), function(x) apply(x[1], 2, mean))
```

Make a time series plot of the 5-minute interval and the average number of steps taken, averaged across all days
```{r}
plot(spi[, 2] ~ spi[, 1], type = "l", ylab = "Steps", xlab = "Interval", 
     main = "Mean Steps per Interval", col = "blue")
```

Find out, which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps
```{r}
spi[spi[, 2] == max(spi[, 2]), ]
```
```{r}
interval    steps
104      835 206.1698
```

## Imputing missing values

Calculate and report the total number of missing values in the dataset
```{r}
sum(is.na(x[, 1]))
```
[1] 2304

Create vector "ifmv" with length of number of missing values in x
```{r}
ifmv <- rep(NA, times = sum(is.na(x[, 1])))
```

Fill the vector with the mean number of steps corresponding the interval of the missing values
```{r}
for(i in 1:sum(is.na(x[, 1]))){
    ifmv[i] <- spi[,2][(which(x[, 3][is.na(x[,1])][i] == spi[,1]))]
}
```

Create a new dataset that is equal to the original dataset but with the missing data filled in
```{r}
xinc <- x[!complete.cases(x), ]
xinc[, 1] <- ifmv
xcomplete <- rbind(xc, xinc)
xcomplete <- xcomplete[with(xcomplete, order(date, interval)),]
```

Create data frame named "spdn" for steps per date new, where the first column 
corresponds to unique dates and the second column corresponds steps in this day
```{r}
spdn <- ddply(xcomplete, c("date"), function(x) apply(x[1], 2, sum))
```

Make a histogram of the total number of steps taken each day
```{r}
hist(spdn[, 2], xlab = "Steps per Day", 
     main = "New Histogram of Steps per Day", col = "red")
```

Calculate mean and median total number of steps taken per day
```{r}
mean(spdn[, 2])
```
[1] 10766

```{r}
median(spdn[, 2])
```
[1] 10766

## Are there differences in activity patterns between weekdays and weekends?
Create a new factor variable "WD" in the dataset with two levels
```{r}
xcomplete$WD <- weekdays(as.Date(xcomplete[, 2]))
xcomplete$WD[xcomplete$WD == "Friday"] <- "Weekday"
xcomplete$WD[xcomplete$WD == "Monday"] <- "Weekday"
xcomplete$WD[xcomplete$WD == "Thursday"] <- "Weekday"
xcomplete$WD[xcomplete$WD == "Tuesday"] <- "Weekday"
xcomplete$WD[xcomplete$WD == "Wednesday"] <- "Weekday"
xcomplete$WD[xcomplete$WD == "Saturday"] <- "Weekend"
xcomplete$WD[xcomplete$WD == "Sunday"] <- "Weekend"
```
Create data frame named "spin" for steps per interval new, where the first column corresponds to unique intervals, the second column shows whether the day is weekday or weekend and the third column corresponds the mean steps in this day
```{r}
spin <- ddply(xcomplete, c("interval", "WD"), function(x) apply(x[1], 2, mean))
```
Make a panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days
```{r}
xyplot(spin$steps ~ spin$interval | spin$WD, type = "l", ylab = "Steps", 
       xlab = "Interval", main = "Mean Steps per Interval", layout = c(1, 2))
```