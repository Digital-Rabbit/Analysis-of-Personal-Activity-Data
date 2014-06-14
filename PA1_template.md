# Reproducible Research: Peer Assessment 1

Fitbit, Nike Fuelband, Jawbone Up, and other devices are popular with people who want to monitor their personal activity level for one reason or another. 
Although lots of data are collected, these data are not used much due to a lack
of statistical methods and software for processing and interpreting the data. 

## About the data
This assignment analyzes data from the personal activity monitoring  of an 
anonymous individual. The data represents step activitly collected at 5 minute intervals throughout the day during October and November, 2012.

## Data variables
The variables in the dataset are:
* steps: Number of steps taken in a 5-minute interval (missing values are 
coded as NA)
* date: The date on which the measurement was taken, in YYYY-MM-DD format
* interval: The identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file, activity.csv. There
are 17,568 observations in the file.

## Loading and preprocessing the data
Make sure you set the working directory correctly. After downloading or unzipping
the data, read it into R. 

```r
activity <- read.csv("activity.csv",header=TRUE,sep=",", colClasses = "character")
```

Process and transform the data into a format suitable for analysis. For this 
part, remove the NA values. Then, change the interval and step data to numeric,
and the date data to the Date class.

```r
cleanAct <- na.omit(activity)
cleanAct$interval <- as.numeric(cleanAct$interval)
cleanAct$steps <- as.numeric(cleanAct$steps)
cleanAct$date <- as.Date(cleanAct$date,format='%Y-%m-%d')
```

Extract weekdays and bind them as a factor column to the cleaned dataset.

```r
dayOfWeek <- factor(weekdays(cleanAct$date), c("Monday","Tuesday",
                  "Wednesday","Thursday","Friday","Saturday","Sunday"))
cleanAct <- cbind(cleanAct,dayOfWeek)
```

Now the data is ready for plotting.

## What is mean and total number of steps taken per day?
To answer this question, you can use ggplot2. Make sure the package is loaded.
Create a histogram of the total number of steps taken each day.

```r
library("ggplot2", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")

qplot(dayOfWeek,data=cleanAct, geom="bar", weight=steps, 
      ylab = "Total Steps",xlab="Day of Week")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

To calculate the mean and median of total number of steps taken per day, load the 
plyr package and use ddply.

```r
library("plyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")

ddply(cleanAct, "dayOfWeek", summarize, Mean_Steps = mean(steps))
```

```
##   dayOfWeek Mean_Steps
## 1    Monday      34.63
## 2   Tuesday      31.07
## 3 Wednesday      40.94
## 4  Thursday      28.52
## 5    Friday      42.92
## 6  Saturday      43.53
## 7    Sunday      42.63
```

```r
ddply(cleanAct, "dayOfWeek", summarize, Median_Steps = median(steps))
```

```
##   dayOfWeek Median_Steps
## 1    Monday            0
## 2   Tuesday            0
## 3 Wednesday            0
## 4  Thursday            0
## 5    Friday            0
## 6  Saturday            0
## 7    Sunday            0
```

 
## What is the average daily activity pattern?
Make a time series plot of the 5-minute interval (x-axis) and the average number 
of steps taken, averaged across all days  (y-axis). First you need to average 
the steps taken across all days, for each interval. Then use ggplot to plot the 
mean number of steps by interval.

```r
meanStepsByInterval <- ddply(cleanAct, "interval", summarize, Mean_Steps = mean(steps))
ggplot(meanStepsByInterval,aes(x=interval,y=Mean_Steps)) + geom_line() + 
    xlab("Interval") + ylab("Mean Steps Across Days")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 

To find out which 5-minute interval, on average across all the days in the 
dataset, contains the maximum number of steps, first calculate the max 
for each interval.

```r
 maxStepsByInterval <- ddply(cleanAct, "interval", summarize, Max_Steps = max(steps))
```

Then, find the maximum of the Max_Steps Column and retrieve the associated 
interval by getting the index, then getting the time interval associated with 
that index.

```r
theMax <- max(maxStepsByInterval$Max_Steps)
theIndex <- match(theMax,maxStepsByInterval$Max_Steps)
theInterval <- maxStepsByInterval[theIndex,"interval"]
```

The maximum number of steps is 806 and the associated time interval 
is 615.

## Imputing missing values
Thus far the plots used cleaned up version of the activity data; one that had the 
missing values removed. Let's see how many missing values there are in the 
original data set. That is, the total number of rows that have NA values. 
Subtract the cleanAct dataset that has the NA rows removed from the original,
uncleaned dataset.

```r
numNA <- nrow(activity) - nrow(cleanAct)
```

There are 2304 missing values.

Next, fill in missing values. There are a variety of strategies available for
this. For example, you can substitute the value from the closest row (either 
preceding or trailing), or you can subsitute the mean. Here,use the mean as the 
substitution value.

```r
numNA <- nrow(activity) - nrow(cleanAct)
meanSteps <- mean(as.numeric(activity$steps),na.rm=TRUE)
filledAct <- activity
filledAct$steps[is.na(filledAct$steps)]<-meanSteps
```

Now that the missing values are filled in, use all the same steps used to answer
previous questions, but using the filledAct dataset. First, get the data into 
the appropriate format and append a factor column for the day of the week.

```r
filledAct$interval <- as.numeric(filledAct$interval)
filledAct$steps <- as.numeric(filledAct$steps)
filledAct$date <- as.Date(filledAct$date,format='%Y-%m-%d')
dayOfWeek <- factor(weekdays(filledAct$date), c("Monday","Tuesday",
                                                 "Wednesday","Thursday","Friday","Saturday","Sunday"))
filledAct <- cbind(filledAct,dayOfWeek)
```

Make a histogram of the total number of steps taken each day.

```r
qplot(dayOfWeek,data=filledAct, geom="bar", weight=steps, 
         ylab = "Total Steps",xlab="Day of Week")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 

Calculate and report the mean and median for the total number of steps taken 
per day.

```r
ddply(filledAct, "dayOfWeek", summarize, Mean_Steps = mean(steps))
```

```
##   dayOfWeek Mean_Steps
## 1    Monday      35.25
## 2   Tuesday      31.07
## 3 Wednesday      40.54
## 4  Thursday      29.50
## 5    Friday      41.69
## 6  Saturday      42.76
## 7    Sunday      41.97
```

```r
ddply(filledAct, "dayOfWeek", summarize, Median_Steps = median(steps))
```

```
##   dayOfWeek Median_Steps
## 1    Monday            0
## 2   Tuesday            0
## 3 Wednesday            0
## 4  Thursday            0
## 5    Friday            0
## 6  Saturday            0
## 7    Sunday            0
```

## Comparing datasets: Missing values removed vs. missing values replaced
Compare the previous plot, means, and medians for the cleaned dataset (the one
with the missing values removed) with these results. The most obvious difference
is that the total number of steps went up, as one would expect. Comparing the
plots of the totals, you'll also see that the pattern changed. For example, 
the total steps on Tuesday changed relative to Monday's totals. However, if you
compare the means for each day between the two data sets, you'll see there 
is no change in the relative ranking of the means. 


```r
cleanMeans <- ddply(cleanAct, "dayOfWeek", summarize, Clean_Mean = mean(steps))
filled <- ddply(filledAct, "dayOfWeek", summarize, Mean = mean(steps))
cbind(cleanMeans,filled$Mean)
```

```
##   dayOfWeek Clean_Mean filled$Mean
## 1    Monday      34.63       35.25
## 2   Tuesday      31.07       31.07
## 3 Wednesday      40.94       40.54
## 4  Thursday      28.52       29.50
## 5    Friday      42.92       41.69
## 6  Saturday      43.53       42.76
## 7    Sunday      42.63       41.97
```

## Are there differences in activity patterns between weekdays and weekends?
To answer this question, you need to create a new factor variable in the dataset 
with two levels—“weekday” and “weekend”—indicating whether a given date is a weekday 
or weekend day. To create the factor, use a conditional in for loop to iterate
over the the dayOfWeek factor. After creating the factor,  bind it to the 
filledAct dataset.

```r
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
```

Make a panel plot that contains two time series plot of the 5-minute interval 
(x-axis) and the average number of steps taken. One plot shows the steps averaged
across all weekday days  and the other shows the average over weekend days. 
For this, use qplot and facets.

First, subset the data by weekend or weekday.

```r
wkDay <- subset(filledAct,select=c(steps,interval,weekEndOrDay),subset=(weekEndOrDay=="Weekday"))
wkEnd <- subset(filledAct,select=c(steps,interval,weekEndOrDay),subset=(weekEndOrDay=="Weekend"))
```

Calculate the meant steps by interval over all days in each data subset.

```r
wkDayMeanStepsByInterval <- ddply(wkDay, "interval", summarize, Mean_Steps = mean(steps))
wkEndMeanStepsByInterval <- ddply(wkEnd, "interval", summarize, Mean_Steps = mean(steps))
```

Add the weekend or weekday factor

```r
wkDayMeans <- cbind(wkDayMeanStepsByInterval, wkDay$weekEndOrDay)
wkEndMeans <- cbind(wkEndMeanStepsByInterval, wkEnd$weekEndOrDay)
```

Clean up the column names.

```r
colnames(wkDayMeans) <- c("Interval","Mean_Steps","Day_Type")
colnames(wkEndMeans) <- c("Interval","Mean_Steps","Day_Type")   
```

Combine the weekend and weekday means.

```r
combinedSet <- rbind(wkDayMeans, wkEndMeans)
```

Plot the average number of steps for each interval in a panel plot that 
separates weekend from weekday data.

```r
qplot(Interval,Mean_Steps,data = combinedSet, geom="line", xlab = "Interval",
      ylab = "Average number of steps") + facet_grid(Day_Type~.)
```

![plot of chunk unnamed-chunk-21](figure/unnamed-chunk-21.png) 

For this individual, activity begins earlier during the week than on the
weekend. There also appears to be a burst of activity early on, with steps
generally remaining under 100 thereafter. The weekend activity has more 
intervals that exceed 100, but none that exceed 200 as shown in the plot of
the weekday data.
