require(dplyr)
require(ggplot2)

##Loading and preprocessing the data
pa <- read.csv("activity.csv", header = TRUE)

##What is mean total number of steps taken per day?
##Remove NA values
pa_complete <- pa[complete.cases(pa),]

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
