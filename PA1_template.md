# Reproducible Research: Peer Assessment 1

## Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Apple Watch, Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## Basic setup

Before we dive into the analysis, some few basics parameters need to be set. So, we first set the working directory, set echo to be true in order to make visible all the code presented in every chunk.


```r
setwd("/Users/home/r-test/github/RepData_PeerAssessment1/RepData_PeerAssessment1")
echo=TRUE
```

To begin this study, we will need to install and load a few packages. If you have not installed them yet, you will need to install ggplot, gridExtra and dplyr using:

1. install.packages("ggplots")
2. install.packages("gridExtra")
3. install.packages("dplyr"). 

These installs must be done manually. 


```r
# load the libraries we will need
library(ggplot2)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(gridExtra)
```

## Loading and preprocessing the data

The activity data is provided in the github repo and can be downloaded from the following URL:

https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip

The following R code "Chunk" provides a method for obtaining the data that accounts for both scenarios where the file needs to be downloaded, unzipped, both or is otherwise present in the root directory where the .Rmd file is located. 


```r
if(file.exists("./activity.csv")){
                print("Dataset already downloaded and unzipped")
                print("Loading Data...")
}else if(file.exists("./repdata-data-activity.zip")){
                print("Dataset downloaded. Now Unzipping...")
                unzip("./repdata-data-activity.zip")
                print("Completed.")
                print("Loading Data...")
}else if(!file.exists("./repdata-data-activity.zip")){
                print("Downloading and unzipping dataset...")
                download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip","./repdata-data-activity.zip")
                unzip("./repdata-data-activity.zip")
                print("Completed.")
                print("Loading Data...")
}
```

```
## [1] "Dataset already downloaded and unzipped"
## [1] "Loading Data..."
```

```r
##read file using read.csv
activityData<-read.csv("./activity.csv", header=TRUE, sep=",", )
##convert date column from factor to Date class
activityData$date<-as.Date(activityData$date)
##provide a summary of the activityData table
summary(activityData)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```

```r
## variables for documenting the observations and variables for the activity dataset
obs<-nrow(activityData)
vars<-ncol(activityData)
```

The file, "activity.csv", consists of 17568 observation of 3 variables.

As you can see in the summary, this data set has a number of N/A observations, or observations where data is missing. We first want to create a dataset that has those NAs removed (We will impute this missing data later). In this dataset we will also make the date a factor which will help me when we subset the data using dplyr.


```r
activityDataComplete <- na.omit(activityData)
activityDataComplete$date <- as.factor(activityDataComplete$date)
##provide a summary of the activityDataComplete table
summary(activityDataComplete)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-02:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-03:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-04:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-05:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-06:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-07:  288   Max.   :2355.0  
##                   (Other)   :13536
```

```r
## variables for documenting the observations and variables for the activity dataset
obs2<-nrow(activityDataComplete)
vars2<-ncol(activityDataComplete)
```

The activityDataComplete table consists of only 15264 observation of 3 variables.



## What is mean total number of steps taken per day?

To subset the data, calculate the sum, median and mean of the daily step count, we create a third data set based on the activityDataCleaned dataset. We use Hadley Wickham's dplyr package, as it is fast, elegant ans easy to read. 


```r
calcActivity <- activityDataComplete %>% 
        group_by(date) %>% 
        summarise (step.sum = sum(steps), step.mean = round(mean(steps)), step.median=median(steps)) 
calcActivity
```

```
## Source: local data frame [53 x 4]
## 
##          date step.sum step.mean step.median
## 1  2012-10-02      126         0           0
## 2  2012-10-03    11352        39           0
## 3  2012-10-04    12116        42           0
## 4  2012-10-05    13294        46           0
## 5  2012-10-06    15420        54           0
## 6  2012-10-07    11015        38           0
## 7  2012-10-09    12811        44           0
## 8  2012-10-10     9900        34           0
## 9  2012-10-11    10304        36           0
## 10 2012-10-12    17382        60           0
## ..        ...      ...       ...         ...
```

### 1. Histogram of the total number of steps taken per day

Now that we have all the data subset and calculated it's possible to show the distribution of steps across the two month recording window in a histogram with the daily step count mean and median provided in the legend.


```r
dMean <- round(mean(calcActivity$step.sum))
dMedian <- median(calcActivity$step.sum)

p <- ggplot(calcActivity, aes(x=step.sum)) + geom_histogram(binwidth=750, colour="black", fill="#bbaaff")
p <- p+geom_vline(aes(xintercept=dMean), color="#ff1100", size = 10, linetype = "solid", alpha=0.33)
p <- p+geom_vline(aes(xintercept=dMedian), color="#0000ff", linetype = "dashed", size=1, alpha=1)
p <-  p+ggtitle("A Histogram of the Total Step Counts per Day from October ~ November")+ theme(plot.title = element_text(lineheight=.8, face="bold"))
p<-p+xlab("Step Count Daily Totals") + ylab("Frequency of Step Count Totals")
p <- p+geom_text(aes(dMean,0,label = "mean =", hjust=-1, vjust = -24))
p <- p+geom_text(aes(dMean,0,label = dMean, hjust=-2.5, vjust = -24))
p <- p+geom_text(aes(dMedian,0,label = dMedian, hjust=-2.5, vjust = -22))
p <- p+geom_text(aes(dMedian,0,label = "median = ", hjust=-0.6, vjust = -22))
p
```

![](PA1_template_files/figure-html/overallmeanhisto-1.png) 

### 2. Mean and median of steps taken per day

The mean step count (October~November) is 1.0766\times 10^{4}.

The median step count (October~November) is 10765.


## What is the average daily activity pattern?

To observe the pedometer wearer's daily activity we can look at the number of steps in each interval across a given day. This would give us a general trend line for the users activity in that day. If we take the mean of each intervals across every day in the two month recording window, we might be able to observe the users overall activity patterns during the sample period.


```r
##make a new complete and clean data set
activityDataComplete2 <- na.omit(activityData)
## convert the interval col to a factor
activityDataComplete2$interval <- as.factor(activityDataComplete2$interval)
##group by interval factor and average
calcActivity2 <- activityDataComplete2 %>% 
        group_by(interval) %>% 
        summarise (step.mean = mean(steps))
calcActivity2
```

```
## Source: local data frame [288 x 2]
## 
##    interval step.mean
## 1         0 1.7169811
## 2         5 0.3396226
## 3        10 0.1320755
## 4        15 0.1509434
## 5        20 0.0754717
## 6        25 2.0943396
## 7        30 0.5283019
## 8        35 0.8679245
## 9        40 0.0000000
## 10       45 1.4716981
## ..      ...       ...
```


```r
l <- ggplot(data=calcActivity2, aes(x=as.numeric(levels(interval))[interval]/100, y=step.mean, group=1)) + geom_line(colour="#000099", linetype="solid", size=1.4) + geom_point(colour="#bbaaff", size=1.1, shape=21, fill="#EB92F7")
l <- l + scale_x_continuous(breaks=c(0, 4, 8, 12, 16, 20, 24))
l <- l + ggtitle("Average Number of Steps per Interval from October ~ November")+ theme(plot.title = element_text(lineheight=.8, face="bold"))
l <- l + xlab("Step Count Recording Interval (hour)") + ylab("Average Step Count")
l
```

![](PA1_template_files/figure-html/dailylineplot-1.png) 

In order to find the most active time of the day, on average, across the two month recording window (the most active interval on average), we need to execute the following code:


```r
calcActivity2[which.max(calcActivity2$step.mean),]
```

```
## Source: local data frame [1 x 2]
## 
##   interval step.mean
## 1      835  206.1698
```

The most active interval for this user is ~206 steps at the 8:35am interval.


## Imputing missing values

Missing values are a problem with any activity tracking dataset. The user occasionaly forgets to wear their pedometer or the battery dies and data will be missing. There are packages like impute and others designed to fill in this missing data with logical equivalents. In this assignment we will use a more simple method and use our list of average steps per interval to fill in the missing data.

First we can make an array with the missing values:


```r
missingValues<-activityData[which(is.na(activityData)),]
```

We can determine how many missing values there are using either summary orthe following snippet:


```r
nrow(missingValues)
```

```
## [1] 2304
```

Now we can loop through our original dataActivity dataset and select the intervals where the step count is equal to N/A and replace those "steps" with the step.mean of the same index from our calcactivity2 dataset (which was a the daily averages for each interval minus the N/As).

We will copy the orginal dataset into a new tmp dataset (ad) in order to preserve the original data with N/As:


```r
ad<- activityData
for(i in 1:nrow(ad)){
        if(is.na(ad[i,]$steps)){
                tmp<-ad[i,]$interval  
                ad[i,]$steps <- calcActivity2[which(calcActivity2$interval==tmp),]$step.mean
        }        
}
summary(ad)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 27.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0
```

Now we can perform our original calculations to compute a new histogram. This will be the same calculations as before except the NAs that we removed before have now been imputed with daily averages and we can see the overall effect of the imputation on the mean, median and daily averages.


```r
calcActivity3 <- ad %>% 
        group_by(date) %>% 
        summarise (step.sum = sum(steps), step.mean = round(mean(steps)), step.median=median(steps)) 
calcActivity3
```

```
## Source: local data frame [61 x 4]
## 
##          date step.sum step.mean step.median
## 1  2012-10-01 10766.19        37    34.11321
## 2  2012-10-02   126.00         0     0.00000
## 3  2012-10-03 11352.00        39     0.00000
## 4  2012-10-04 12116.00        42     0.00000
## 5  2012-10-05 13294.00        46     0.00000
## 6  2012-10-06 15420.00        54     0.00000
## 7  2012-10-07 11015.00        38     0.00000
## 8  2012-10-08 10766.19        37    34.11321
## 9  2012-10-09 12811.00        44     0.00000
## 10 2012-10-10  9900.00        34     0.00000
## ..        ...      ...       ...         ...
```

### 1. Histogram of the total number of steps taken per day


```r
dMean3 <- round(mean(calcActivity3$step.sum))
dMedian3 <- round(median(calcActivity3$step.sum))

p <- ggplot(calcActivity3, aes(x=step.sum)) + geom_histogram(binwidth=750, colour="black", fill="#bbaaff")
p <- p+geom_vline(aes(xintercept=dMean), color="#ff1100", size = 10, linetype = "solid", alpha=0.33)
p <- p+geom_vline(aes(xintercept=dMedian), color="#0000ff", linetype = "dashed", size=1, alpha=1)
p <-  p+ggtitle("A Histogram of the Total Step Counts per Day from October ~ November")+ theme(plot.title = element_text(lineheight=.8, face="bold"))
p<-p+xlab("Step Count Daily Totals") + ylab("Frequency of Step Count Totals")
p <- p+geom_text(aes(dMean,0,label = "mean =", hjust=-1, vjust = -24))
p <- p+geom_text(aes(dMean,0,label = dMean, hjust=-2.5, vjust = -24))
p <- p+geom_text(aes(dMedian,0,label = dMedian, hjust=-2.5, vjust = -22))
p <- p+geom_text(aes(dMedian,0,label = "median = ", hjust=-0.6, vjust = -22))
p
```

![](PA1_template_files/figure-html/imputedmeanhisto2-1.png) 

### 2. Mean and median of steps taken per day

The new calculations are nearly identical to the first histogram with the N/As removed. The imputation had only a small effect on the overall median. 

The mean step count (October~November) is 1.0766\times 10^{4}.

The median step count (October~November) is 10765.


## Are there differences in activity patterns between weekdays and weekends?

The last plot will explore the differences in activity patterns on weekends vs. weekdays. We will make a plot similar to the line plot above, where the steps per interval are averaged across every day during the two month sample period. We will seperate the weekends from the weekdays, by a new factor column, to look at the two time periods seperately. 

First we need to create a new column in our data frame to be able to select for weekends or weekdays. First, we will need to convert our date to a Date class.


```r
ad$date <- as.Date(ad$date)
class(ad$date)
```

```
## [1] "Date"
```

```r
tmp2<-ad
```

Then we can create the new column using dplyr and the weekday() function. I will also convert this character to a factor to prepare for grouping and summary operations.


```r
##mutate a new col using weekdays (POSIX weekday Sunday=0, Saturday=6), an inline ifelse and dplyr
tmp3 <- mutate(tmp2, day = ifelse(as.POSIXlt(ad$date)$wday==0 | as.POSIXlt(ad$date)$wday==6, "weekend", "weekday"))

## convert to a factor
tmp3$day<-as.factor(tmp3$day)
##provide a summary to confirm
summary(tmp3)
```

```
##      steps             date               interval           day       
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0   weekday:12960  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8   weekend: 4608  
##  Median :  0.00   Median :2012-10-31   Median :1177.5                  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5                  
##  3rd Qu.: 27.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2                  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0
```

Using the dplyr package again, we group and summarise the data by the weekend and weekday factor:


```r
##group by interval factor and average
tmp3$interval <- as.factor(tmp3$interval)

calcActivity5 <- tmp3 %>% 
        filter(day=="weekend") %>%
        group_by(interval) %>% 
        summarise (step.mean = mean(steps))
calcActivity5
```

```
## Source: local data frame [288 x 2]
## 
##    interval   step.mean
## 1         0 0.214622642
## 2         5 0.042452830
## 3        10 0.016509434
## 4        15 0.018867925
## 5        20 0.009433962
## 6        25 3.511792453
## 7        30 0.066037736
## 8        35 0.108490566
## 9        40 0.000000000
## 10       45 0.558962264
## ..      ...         ...
```

```r
calcActivity4 <- tmp3 %>% 
        filter(day=="weekday") %>%
        group_by(interval) %>% 
        summarise (step.mean = mean(steps))
calcActivity4
```

```
## Source: local data frame [288 x 2]
## 
##    interval  step.mean
## 1         0 2.25115304
## 2         5 0.44528302
## 3        10 0.17316562
## 4        15 0.19790356
## 5        20 0.09895178
## 6        25 1.59035639
## 7        30 0.69266247
## 8        35 1.13794549
## 9        40 0.00000000
## 10       45 1.79622642
## ..      ...        ...
```

We then create one line plot for each of the average weekend and weekday activities, by sample interval, across the two month sample period.


```r
l <- ggplot(data=calcActivity5, aes(x=as.numeric(levels(interval))[interval]/100, y=step.mean, group=1)) + geom_line(colour="#000099", linetype="solid", size=1.4) + geom_point(colour="#bbaaff", size=1.1, shape=21, fill="#EB92F7")
l <- l + scale_x_continuous(breaks=c(0, 4, 8, 12, 16, 20, 24))
l <- l + ggtitle("Average Number of Steps per Interval on Weekends")+ theme(plot.title = element_text(lineheight=.8, face="bold"))
l <- l + xlab("Step Count Recording Interval (hour)") + ylab("Average Step Count")
```


```r
r <- ggplot(data=calcActivity4, aes(x=as.numeric(levels(interval))[interval]/100, y=step.mean, group=1)) + geom_line(colour="#000099", linetype="solid", size=1.4) + geom_point(colour="#bbaaff", size=1.1, shape=21, fill="#EB92F7")
r <- r + scale_x_continuous(breaks=c(0, 4, 8, 12, 16, 20, 24))
r <- r + ggtitle("Average Number of Steps per Interval on Weekdays")+ theme(plot.title = element_text(lineheight=.8, face="bold"))
r <- r + xlab("Step Count Recording Interval (hour)") + ylab("Average Step Count")
```

Eventually we can use the gridExtra package to arrange these two plots in the same panel.


```r
grid.arrange(l, r, ncol=1, top="Average Weekend vs. Weekday activity patterns")
```

![](PA1_template_files/figure-html/gridplot-1.png) 

We can see that the anon user had a higher activity level on average throughout the day on the weekend but a lower peak activity level between 5AM ~ 10AM. This could be for a number of reasons including sedentary office work and a morning commute on foot during the weekdays vs more distributed (across the day) and sustained activity on the weekends.
