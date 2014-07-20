root <- "C:/Users/albert.QBIDS/Coursera/Johns Hopkins/The Data Science Track/5 Reproducible Research/Project1"
setwd(root)

#setup libraries
#install.packages("ggplot2")
#library(ggplot2)


#
# Raw Data      - Activity monitoring data
# https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip
#
#
# REQUIREMENTS: 
# tested on windows 8
# @author Albert van Dok, https://github.com/kjoebie
#


## input dataset
archivedataset <- 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'


## download and unzip to temp location
localarchive    <- "repdata_data_activity.zip"
tempdir <- root
download.file(archivedataset, localarchive)
unzip(localarchive, exdir=tempdir)
file = "activity.csv" #the name of the file in the archive
data <- read.csv(file , header=T)

#Preprocessing data
data$date <- as.Date(data$date, format = "%Y-%m-%d")

#What is mean total number of steps taken per day?
#note: For this part of the assignment, you can ignore the missing values in the dataset.

#calculate the numbers of steps for each day, removing NA's
stepscount <- aggregate(steps ~ date, data, FUN=sum, na.rm=T)

## What is mean total number of steps taken per day?

# The purpose of the histogram is to give an idea of distribution is of the values 
# of the total number of steps that are taken each day. 
hist(stepscount$steps,col="blue",xlab="Number of steps", main="Distribution of the total number of steps taken each day")

#to plot the total steps per day use the following:
#ggplot(stepscount, aes(date,steps)) + geom_histogram(binwidth = .5, stat="identity", colour="white")

#calculate the mean
mean(stepscount$steps)

#calculate the median
median(stepscount$steps)

## What is the average daily activity pattern?

# 1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
# and the average number of steps taken, averaged across all days (y-axis)
meanperinterval <- aggregate(steps ~ interval, data, FUN=mean, na.rm=T)
ggplot(meanperinterval, aes(interval, steps)) + geom_line() + xlab("Interval") + ylab("Avg steps")

# 2.Which 5-minute interval, on average across all the days in the dataset, contains 
# the maximum number of steps?
maxmeanperinterval <- aggregate(steps ~ interval, meanperinterval, FUN=max, na.rm=T)

#return the row with the highest average number of steps
maxmeanperinterval[maxmeanperinterval$steps == max(maxmeanperinterval$steps),]

## Imputing missing values

# 1.Calculate and report the total number of missing values 
# in the dataset (i.e. the total number of rows with NAs)
numberofmissingvalues <- sum(is.na(data$steps))

# 2. Devise a strategy for filling in all of the missing values in the dataset. 

#My strategy:

# generates average values for each time period: see dataframe meanperinterval
# Merge them back into the data into a new variable 
# (so that each entry has the average available)
# data$interval_mean <- meanperinterval[data$interval, 2]
# however, this is causing me an error which I could not solve quickly. 

# Replace the NAs with the average data$thingtoreplace[is.na(data$thingtoreplace)] <- data$newthing

