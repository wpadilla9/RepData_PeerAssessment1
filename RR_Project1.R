## Load data

library(ggplot2)
activity<-read.csv("activity.csv")

## Preprocess data

activity$date <- as.Date(activity$date)
activity_rev <- subset(activity, !is.na(activity$steps))

## Calculate mean and median total steps taken per day 
## Plot Histogram

daily_steps <- tapply(activity_rev$steps, activity_rev$date, sum, 
                      na.rm=TRUE,simplify=T)
daily_steps <-daily_steps[!is.na(daily_steps)]

png("Plot_Hist_DailySteps.png")
hist(x=daily_steps, col="red", breaks=20, xlab="Daily Total Steps", 
     ylab="Frequency", main="Distribution of Daily Total Steps")
dev.off()

mean(daily_steps)
median(daily_steps)

## Calculate average daily activity pattern
## Plot time series

Average_Interval <- tapply(activity_rev$steps, activity_rev$interval, 
                           mean, na.rm=TRUE, simplify=T)
activity_interval_avg <- data.frame(interval=as.integer(names(Average_Interval)),
                                    avg=Average_Interval)

png("Plot_TimeSeries.png")
with(activity_interval_avg, plot(interval, avg, type="l", xlab="5-Minute Intervals",
                                 ylab="Average Number of Steps Taken, Averaged 
                                 Across All Days"))
dev.off()

## Calculate maximum steps on average across all days 

maximum_steps <- max(activity_interval_avg$avg)
activity_interval_avg[activity_interval_avg$avg == maximum_steps, ]

## Imputing Missing Values
## Determine rows in original data set with missing data
## Then create new data frame with missing data filled in
## Using mean in place of missing values

sum(is.na(activity$steps))
activity_impute <- activity
nd <- is.na(activity_impute$steps)
Average_Interval <- tapply(activity_rev$steps, activity_rev$interval, 
                           mean, na.rm=TRUE, simplify=T)
activity_impute$steps[nd] <- Average_Interval[as.character(activity_impute$interval[nd])]

## Plot Histogram and Calculate new mean and median of daily steps

new_daily_steps <- tapply(activity_impute$steps, activity_impute$date, sum, 
                          na.rm=TRUE, simplify=T)
png("Plot_Hist_NewDailySteps.png")
hist(new_daily_steps, col="blue", breaks=20, xlab="Daily Steps", ylab="Frequency",
     main="Daily Total Steps with Missing Values Imputed")
dev.off()

mean(new_daily_steps)
median(new_daily_steps)

## Are there differences in activity pattern between weekdays and weekends?
## Create new factor variables - weekday and weekend day

is_weekday <- function(d){wd <- weekdays(d)
      ifelse(wd == "Saturday" | wd == "Sunday", "weekend", "weekday")
}

wx <- sapply(activity_impute$date, is_weekday)
activity_impute$wk <- as.factor(wx)
head(activity_impute)

wk_activity <- aggregate(steps ~wk+interval, data=activity_impute, FUN=mean)

library(lattice)

png("Plot_Compare.png")
xyplot(steps ~ interval | factor(wk), 
       layout=c(1,2), 
       xlab="Interval",
       ylab="Number of Steps", 
       type="l", 
       lty=1, 
       data=wk_activity)
dev.off()





