##Analysis code raw

################ What is mean total number of steps taken per day?

#### Histogram of total number of steps taken each day

library(dplyr)

##grouping steps and date and then summarise it with dplyr "summarise" and plot histogram with base system

df <- select(df_full, steps,date)
df <- group_by(df, date)

df_sum <- summarise(df, steps_sum=sum(steps,na.rm=TRUE))

with(df_sum, hist(steps_sum, main="total number of steps", xlab="Total steps", ylab="Days"))

## Mean and Median number of steps taken each day

mean <- summarise(df_sum, mean=mean(steps_sum,na.rm=TRUE))
median <- summarise(df_sum, median=median(steps_sum,na.rm=TRUE))

mean
median

#Mean total steps per day is 9354 and the median total steps per day is 10395


################ What is the average daily activity pattern?

temp1 <- group_by(df_full, interval)
avg_steps <- summarise(temp1, avg_steps=mean(steps,na.rm = TRUE))
plot(avg_steps$interval, avg_steps$avg_steps, xlab = "Intervall", ylab= "Average Steps", type="l")

interval <- arrange(avg_steps, desc(avg_steps))$interval[1] 
interval
        
###The 5-minute interval that, on average , contains the maximum number of steps is 835

################ Imputing missing values

totalna = sum(is.na(df_full))
totalna

##There are a total of 2304  missing values

###imputation of na with the mean of all steps (=37)

df_nona <- data.frame(df_full)

for(i in 1:ncol(df_nona)){
        df_nona[is.na(df_nona[,i]), i] <- mean(df_nona[,i], na.rm = TRUE)
}
head(df_nona)

##make histogram

df_nona <- group_by(df_full, date)

df_nona_sum <- summarise(df_nona, steps_sum=sum(steps,na.rm=TRUE))

with(df_nona_sum, hist(steps_sum, main="total number of steps", xlab="Total steps", ylab="Days"))

mean2 <- summarise(df_nona_sum, mean=mean(steps_sum,na.rm=TRUE))
median2 <- summarise(df_nona_sum, median=median(steps_sum,na.rm=TRUE))

mean2
median2


################ Are there differences in activity patterns between weekdays and weekends?
install.packages("timeDate")
library(timeDate)
df_nona2 <- df_nona
df_nona2$date <- as.Date(df_nona2$date)
df_nona2$weekend <- ifelse(isWeekday(df_nona2$date, wday=1:5), "Weekdays", "Weekend")


df_nona2 <- group_by(df_nona2, weekend, interval)
df_sum2 <- summarise(df_nona2, steps=mean(steps,na.rm=TRUE))

library(ggplot2)
ggplot(df_sum2, aes(x = interval, y = steps,colour=weekend)) + 
geom_line(aes(colour=weekend))+ xlab("Interval")+ 
ylab("Number of steps ")+facet_wrap(~weekend)+
theme(strip.background = element_rect(fill="orange"))
