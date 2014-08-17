library(plyr)
library(lattice)
x <- read.csv("activity.csv", sep = ",")
x[, 1] <- as.numeric(x[, 1])
x[, 2] <- as.Date(x[, 2])
x[, 3] <- as.numeric(x[, 3])
xc <- x[complete.cases(x), ]
spd <- ddply(xc, c("date"), function(x) apply(x[1], 2, sum)) 
hist(spd[, 2], xlab = "Steps per Day", 
     main = "Histogram of Steps per Day", col = "blue")
mean(spd[, 2])
median(spd[, 2])
spi <- ddply(xc, c("interval"), function(x) apply(x[1], 2, mean)) 
plot(spi[, 2] ~ spi[, 1], type = "l", ylab = "Steps", xlab = "Interval", 
     main = "Mean Steps per Interval", col = "blue")
spi[spi[, 2] == max(spi[, 2]), ]
sum(is.na(x[, 1]))
ifmv <- rep(NA, times = sum(is.na(x[, 1])))
for(i in 1:sum(is.na(x[, 1]))){
    ifmv[i] <- spi[,2][(which(x[, 3][is.na(x[,1])][i] == spi[,1]))]
}
xinc <- x[!complete.cases(x), ]
xinc[, 1] <- ifmv
xcomplete <- rbind(xc, xinc)
xcomplete <- xcomplete[with(xcomplete, order(date, interval)),]
spdn <- ddply(xcomplete, c("date"), function(x) apply(x[1], 2, sum)) 
hist(spdn[, 2], xlab = "Steps per Day", 
     main = "New Histogram of Steps per Day", col = "red")
mean(spdn[, 2])
median(spdn[, 2])
xcomplete$WD <- weekdays(as.Date(xcomplete[, 2]))
xcomplete$WD[xcomplete$WD == "Friday"] <- "Weekday"
xcomplete$WD[xcomplete$WD == "Monday"] <- "Weekday"
xcomplete$WD[xcomplete$WD == "Thursday"] <- "Weekday"
xcomplete$WD[xcomplete$WD == "Tuesday"] <- "Weekday"
xcomplete$WD[xcomplete$WD == "Wednesday"] <- "Weekday"
xcomplete$WD[xcomplete$WD == "Saturday"] <- "Weekend"
xcomplete$WD[xcomplete$WD == "Sunday"] <- "Weekend"
spin <- ddply(xcomplete, c("interval", "WD"), function(x) apply(x[1], 2, mean))
xyplot(spin$steps ~ spin$interval | spin$WD, type = "l", ylab = "Steps", 
       xlab = "Interval", main = "Mean Steps per Interval", layout = c(1, 2))