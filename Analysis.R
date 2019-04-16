# Set project path
setwd('/home/mohit/data_science/RepData_PeerAssessment1')

# Read data
DF = read.csv(unzip('activity.zip', 'activity.csv'))


# 2
df <- tapply(DF$steps, DF$date, sum, na.rm=TRUE)
library(ggplot2)
qplot(df, xlab='Total steps per day', ylab='Frequency using binwith 500', binwidth=500)


# 3
Mean <- mean(df)
Median <- median(df)


# 4
avgNumberStepByTime <- aggregate(x=list(meanSteps=DF$steps), by = list(interval=DF$interval), FUN=mean, na.rm=TRUE)
ggplot(data=avgNumberStepByTime, aes(x=interval, y=meanSteps)) +
  geom_line() +
  xlab("5-minute interval") +
  ylab("average number of steps taken") 

# 5
mostSteps <- which.max(avgNumberStepByTime$meanSteps)
timeMostStep <- gsub("([0-9]{1,2})([0-9]{2})", "\\1:\\2", avgNumberStepByTime[mostSteps, 'interval'])


# 6
missingValues <- length(which(is.na(DF$steps)))
library(Hmisc)
imputedData <- DF
imputedData$steps <- impute(DF$steps, fun=mean)

stepsByDayImputed <- tapply(imputedData$steps, imputedData$date, sum)
qplot(stepsByDayImputed, xlab = 'Total steps per day (Imputed)', ylab = 'Frequency using bandwidth 500', bandwidth=500)

stepsByDayImputedMean <- mean(stepsByDayImputed)
stepsByDayImputedMedian <- median(stepsByDayImputed)


# 7
imputedData$dateType <- ifelse(as.POSIXlt(imputedData$date)$wday %in% c(0, 6), 'weekend', 'weekday')
avgimputedData <- aggregate(steps ~ interval + dateType, data = imputedData, mean)
ggplot(avgimputedData, aes(interval, steps)) +
  geom_line() +
  facet_grid(dateType ~ .) +
  xlab('5-minute interval') + 
  ylab('Avg number of steps')

