# Set Global Echo = On

# Load data
if (!file.exists("activity.csv") )
{
  dlurl <- 'http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'  
  download.file(dlurl,destfile='repdata%2Fdata%2Factivity.zip',mode='wb')  
  unzip('repdata%2Fdata%2Factivity.zip')
}

# Read data
data <- read.csv("activity.csv")  

##3a What is mean total number of steps taken per day?
png("Rplot.png")
steps_by_day <- aggregate(steps ~ date, data, sum)
hist(steps_by_day$steps, main = paste("Total Steps Each Day"), col="green",xlab="Number of Steps")
dev.off()

rmean <- mean(steps_by_day$steps)
rmean

rmedian <- median(steps_by_day$steps)
rmedian

## 3b What is the average daily activity pattern?
png("Rplot1.png")
steps_by_interval <- aggregate(steps ~ interval, data, mean)
plot(steps_by_interval$interval,steps_by_interval$steps, type="l", xlab="Interval", ylab="Number of Steps",main="Average Number of Steps per Day by Interval")
dev.off()

max_interval <- steps_by_interval[which.max(steps_by_interval$steps),1]
max_interval

## 3c Imputing missing values
## 1.Calculate and report the total number of missing values in the dataset
NATotal <- sum(!complete.cases(data))
NATotal

## 2. Using Mean for the day compute missing values
StepsAverage <- aggregate(steps ~ interval, data = data, FUN = mean)
fillNA <- numeric()
for (i in 1:nrow(data)) {
  obs <- data[i, ]
  if (is.na(obs$steps)) {
    steps <- subset(StepsAverage, interval == obs$interval)$steps
  } else {
    steps <- obs$steps
  }
  fillNA <- c(fillNA, steps)
}

## 3.Create a new dataset including the imputed missing values
new_activity <- data
new_activity$steps <- fillNA

##4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.
png("Rplot2.png")
StepsTotalUnion <- aggregate(steps ~ date, data = new_activity, sum, na.rm = TRUE)
hist(StepsTotalUnion$steps, main = paste("Total Steps Each Day"), col="blue", xlab="Number of Steps")

#Create Histogram to show difference. 
hist(steps_by_day$steps, main = paste("Total Steps Each Day"), col="green", xlab="Number of Steps", add=T)
legend("topright", c("Imputed", "Non-imputed"), col=c("blue", "green"), lwd=10)
dev.off()

rmeantotal <- mean(StepsTotalUnion$steps)
rmeantotal

rmediantotal <- median(StepsTotalUnion$steps)
rmediantotal

rmediandiff <- rmediantotal - rmedian
rmediandiff

rmeandiff <- rmeantotal - rmean
rmeandiff

##3d.Are there differences in activity patterns between weekdays and weekends?
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday","Friday")
new_activity$dow = as.factor(ifelse(is.element(weekdays(as.Date(new_activity$date)),weekdays), "Weekday", "Weekend"))
StepsTotalUnion <- aggregate(steps ~ interval + dow, new_activity, mean)
library(lattice)
png("Rplot3.png")
xyplot(StepsTotalUnion$steps ~ StepsTotalUnion$interval|StepsTotalUnion$dow, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")
dev.off()

