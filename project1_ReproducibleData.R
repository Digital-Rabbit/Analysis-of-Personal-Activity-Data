project1_ReproducibleData <- function(){
    ## The variables in the original dataset are:
    ## steps: Number of steps taking in a 5-minute interval
    ##      (missing values are coded as NA ) 
    ## date: The date on which the measurement was taken in YYYY-MM-DD format 
    ## interval: Identifier for the 5-minute interval in which measurement 
    ##      was taken
    ## The values in the original dataset are stored in comma delimited format
    
    ## Use echo = TRUE
    setwd("~/Desktop/Reproducible Research/Project 1")
   ## read.csv("activity.csv",header=TRUE,sep=",", colClasses = "character")
   
   ## Download and load the data into R.
   activity <- read.csv("activity.csv",header=TRUE,sep=",",colClasses = "character")
   
   ## Process/transform the data into a format suitable for analysis
   ## 1.Remove NA 
   cleanAct <- na.omit(activity)
   ## 2. Change interval data to numeric
   cleanAct$interval <- as.numeric(cleanAct$interval)
   ## 3. Change step data to numeric
   cleanAct$steps <- as.numeric(cleanAct$steps)
   ## 4. Change date data to Date class
   cleanAct$date <- as.Date(cleanAct$date,format='%Y-%m-%d')
   ## Extract weekdays and bind as factor column
   dayOfWeek <- factor(weekdays(cleanAct$date), c("Monday","Tuesday",
                  "Wednesday","Thursday","Friday","Saturday","Sunday"))
   cleanAct <- cbind(cleanAct,dayOfWeek)
   
   ## What is mean total number of steps taken per day?
   ## Make sure ggplot2 is loaded 
   ## 1. Make a histogram of the total number of steps taken each day
   qplot(dayOfWeek,data=cleanAct, geom="bar", weight=steps, 
         ylab = "Steps",xlab="Day of Week")
  
   ## 2. Calculate and report the mean and median total number of steps 
   ## taken per day. To do this, load the plyr package and use ddply
   ddply(cleanAct, "dayOfWeek", summarize, Mean_Steps = mean(steps))
   ddply(cleanAct, "dayOfWeek", summarize, Median_Steps = median(steps))
   
   ## What is the average daily activity pattern?
   ## 1. Make a time series plot of the 5-minute interval 
   ## (x-axis) and the average number of steps taken, averaged 
   ## across all days (y-axis)
   ggplot(cleanAct,aes(x=interval,y=steps)) + geom_line()
   
   ## 2. Which 5-minute interval, on average across all the days in the dataset,
   ## contains the maximum number of steps?
   ## First calculate the max for each interval
   maxStepsByInterval <- ddply(cleanAct, "interval", summarize, Max_Steps = max(steps))
   ## Then find the maximum of the Max_Steps Column
   theMax <- max(maxStepsByInterval$Max_Steps)
   ## Retrieve the associated interval by getting the index, then getting
   ## the time interval assodiated wiht that index
   theIndex <- match(theMax,maxStepsByInterval$Max_Steps)
   theInterval <- maxStepsByInterval[theIndex,"interval"]
   
   ## Impute missing values
   ## 1. Calculate and report the total number of missing values in the dataset
   ## (i.e. the total number of rows with NA s)
   numNA <- nrow(activity) - nrow(cleanAct)
   
   ## 2. Fill in missing values (my strategy is to use the mean)
   meanSteps <- mean(as.numeric(activity$steps),na.rm=TRUE)
   filledAct <- activity
   filledAct$steps[is.na(filledAct$steps)]<-meanSteps
   
   ## 3. Create a new dataset with the filled in data in it
   ## Same steps as previously, but on the filled in data frame
   filledAct$interval <- as.numeric(filledAct$interval)
   filledAct$steps <- as.numeric(filledAct$steps)
   filledAct$date <- as.Date(filledAct$date,format='%Y-%m-%d')
   dayOfWeek <- factor(weekdays(filledAct$date), c("Monday","Tuesday",
                                                 "Wednesday","Thursday","Friday","Saturday","Sunday"))
   filledAct <- cbind(filledAct,dayOfWeek)

   ## 4. Make a histogram of the total number of steps taken each day and 
   qplot(dayOfWeek,data=filledAct, geom="bar", weight=steps, 
         ylab = "Steps",xlab="Day of Week")
   
   ## Calculate and report the mean and median total number of 
   ## steps taken per day.
   ddply(filledAct, "dayOfWeek", summarize, Mean_Steps = mean(steps))
   ddply(filledAct, "dayOfWeek", summarize, Median_Steps = median(steps))
   
   ## Are there differences in activity patterns between weekdays and weekends?
   ## 1. Create a new factor variable in the dataset with two levels – “weekday” 
   ## and “weekend” indicating whether a given date is a weekday or weekend day.
   ## ******* Need to transform days to weekday and weekend
   ## If dayOfWeek matches c("Monday","Tuesday", "Wednesday","Thursday","Friday")
   ## If dayOfWeek matchs c("Saturday","Sunday"))
   weekendFactor = c()
   for (i in 1:nrow(filledAct)){
       if ((filledAct[i,"dayOfWeek"] == "Saturday") || (filledAct[i,"dayOfWeek"] == "Sunday")){
           weekendFactor[i] <- "Weekend"
       } else {
           weekendFactor[i] <- "Weekday"
       }
   }
   weekEndOrDay <- factor(weekendFactor)
   filledAct <- cbind(filledAct,weekEndOrDay)
   ## 2. Make a panel plot containing a time series plot (i.e. type = "l" ) 
   ## of the 5-minute interval (x-axis) and the average number of steps taken, 
   ## averaged across all weekday days or weekend days (y-axis). 
   qplot(interval,steps,data=filledAct, geom="line") + facet_grid(weekEndOrDay~.)
  
}