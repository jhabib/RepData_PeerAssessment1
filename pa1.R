require(dplyr)
require(ggplot2)
require(data.table)

##Loading and preprocessing the data
pa <- read.csv("activity.csv", header = TRUE)

##What is mean total number of steps taken per day?
##Remove NA values
pa_complete <- pa[complete.cases(pa),]
rownames(pa_complete) <- NULL

##Calculate the total number of steps taken per day
steps_per_day <- aggregate(steps~date, data=pa_complete, FUN=sum)

##Create a histogram of number of steps per day
ghist <- ggplot(data=steps_per_day, aes(steps)) + 
  geom_histogram(col="white", 
                 aes(fill=..count..), binwidth=2500) +
  scale_fill_gradient("count", low = "lightblue", high = "darkblue")
plot(ghist)

##Calculate the mean and median of steps taken per day
mean_steps_per_day <- mean(steps_per_day$steps)
print(mean_steps_per_day)

median_steps_per_day <- median(steps_per_day$steps)
print(median_steps_per_day)

##What is Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
##and the average number of steps taken, averaged across all days (y-axis)the average daily activity pattern

##calculate average number of steps per interval across all days
mean_steps_per_interval <- aggregate(steps~interval, data=pa_complete, FUN=mean)

##Create a plot of mean steps per interval (y_axis) vs. interval (x-axis)
gplot <- ggplot(data=mean_steps_per_interval, aes(x=interval, y=steps)) + 
  geom_line(aes(color=steps))
plot(gplot)

##Get the interval with the highest average number of steps across all days
max_average_steps_interval <- mean_steps_per_interval[which.max(mean_steps_per_interval$steps), "interval"]
print(max_average_steps_interval)

##Impute missing values
##Calculate and report the total number of missing values in the dataset 
pa_incomplete <- pa[!complete.cases(pa),]
missing_values <- nrow(pa_incomplete)
print(missing_values)

##Devise a strategy for filling in all of the missing values in the dataset
##Use interval mean to replace NA values
md <- merge(pa_incomplete, mean_steps_per_interval, by.x="interval", by.y="interval", all=TRUE)
md <- md[, c("steps.y", "date", "interval")]
colnames(md) <- c("steps", "date", "interval")
md <- data.table(md, key=c("date", "interval"))
pa_complete <- data.table(pa.complete, key=c("date", "interval"))

##combine md and pa_complete
l <- list(pa_complete, md)
pa_full <- rbindlist(l)
##sort the rows by date and interval to make the data look like original
pa_full <- pa_full[order(date, interval),]

##Make a histogram of the total number of steps taken each day
##Calculate the total number of steps taken per day
steps_per_day_full <- aggregate(steps~date, data=pa_full, FUN=sum)

##Create a histogram of number of steps per day
ghist_full <- ggplot(data=steps_per_day_full, aes(steps)) + 
  geom_histogram(col="white", 
                 aes(fill=..count..), binwidth=2500) +
  scale_fill_gradient("count", low = "lightblue", high = "darkblue")
plot(ghist_full)

##Calculate the mean and median of steps taken per day with the imputed data
mean_steps_per_day_full <- mean(steps_per_day_full$steps)
print(mean_steps_per_day_full)

median_steps_per_day_full <- median(steps_per_day_full$steps)
print(median_steps_per_day_full)
