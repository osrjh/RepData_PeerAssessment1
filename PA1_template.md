---
title: "Reproducible Research: Peer Assessment 1"
author : "Jack Huang"
date: "2022-12-20"
output: 
  html_document:
    keep_md: true
---


```r
r = getOption("repos")
r["CRAN"] = "https://cran.curtin.edu.au/"
options(repos = r)

knitr::opts_chunk$set(echo = TRUE,
                      fig.align = "center",
                      fig.path = 'figure/')

#installing packages
packages <- c("knitr", "dplyr", "ggplot2")

installed_packages <- packages %in% rownames(installed.packages())

if (any(installed_packages == FALSE)) {
  print("Please install the required packages")
} else {
  print("All packages installed")
}
```

```
## [1] "All packages installed"
```

```r
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages], type = "source")
}

update.packages(packages)

#loading packages
invisible(lapply(packages,
                 library,
                 character.only = TRUE))
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

## Loading and preprocessing the data

1. Load the data (i.e. read.csv())


```r
dataset_folder_name <- "activity"
dataset_file_name <- "activity.zip"

if(!file.exists(dataset_folder_name)) {
  dir.create(dataset_folder_name)
  unzip(zipfile = dataset_file_name, exdir = dataset_folder_name)
}
```


```r
fname <- "activity.csv"

# loading the data
data <- read.csv(paste0(dataset_folder_name,"/",fname), 
                 header = TRUE)
```

2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
# convert the date column type into POSIX 
data$date <- as.POSIXct(data$date)

# viewing the data summary
summary(data) %>%
  kable
```



|   |    steps      |     date          |   interval    |
|:--|:--------------|:------------------|:--------------|
|   |Min.   :  0.00 |Min.   :2012-10-01 |Min.   :   0.0 |
|   |1st Qu.:  0.00 |1st Qu.:2012-10-16 |1st Qu.: 588.8 |
|   |Median :  0.00 |Median :2012-10-31 |Median :1177.5 |
|   |Mean   : 37.38 |Mean   :2012-10-31 |Mean   :1177.5 |
|   |3rd Qu.: 12.00 |3rd Qu.:2012-11-15 |3rd Qu.:1766.2 |
|   |Max.   :806.00 |Max.   :2012-11-30 |Max.   :2355.0 |
|   |NA's   :2304   |NA                 |NA             |

```r
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : POSIXct, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

## What is mean total number of steps taken per day? 

*ignoring the missing values in the dataset*

1. Calculate the total number of steps taken per day


```r
# Calculating the total number of steps taken per day
data_totals <- as.data.frame(tapply(data$steps, data$date, sum, na.rm = TRUE))

data_totals$date <- rownames(data_totals)
names(data_totals) <- c("total steps", "date")
rownames(data_totals) <- NULL
```

2. Make a histogram of the total number of steps taken each day


```r
# Histogram of the total number of steps taken each day
data_totals %>%
  ggplot(aes(x = .[,1])) +
  geom_histogram(col = "#ff964f", 
                 position = "dodge",
                 fill = "#4f5374",
                 breaks = seq(0,25000,2500),
                 bins = 10,
                 boundary = 0) +
  labs(x = "Total Steps Taken",
       y = "Number of Days",
       title = "Total Steps Taken Each Day") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(vjust = -0.5)) +
  scale_x_continuous(breaks = seq(0,25000, 2500)) 
```

<img src="figure/2.1 Histogram of total number of steps taken per day-1.png" style="display: block; margin: auto;" />

3. Calculate and report the mean and median of the total number of steps taken per day


```r
# Calculating the mean number of total steps taken per day
mean(data_totals$`total steps`)
```

```
## [1] 9354.23
```

```r
# Calculating the median number of total steps taken per day
median(data_totals$`total steps`)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. **type = "l"**) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
# Time series plot of the average number of steps taken averaged across all days
data_int_avg_step <- aggregate(steps ~ interval,
                               FUN = mean,
                               data = data,
                               na.rm = TRUE)

names(data_int_avg_step) <- c("Interval", "Average Number of Steps Taken")

data_int_avg_step %>%
  ggplot(aes(x = Interval,
             y = `Average Number of Steps Taken`)) +
  geom_line(col = "#4f5374") +
  labs(title = "Average Number of Steps Per Interval Averaged Across All Days") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(vjust = -0.5)) +
  scale_x_continuous(breaks = seq(0,2400, 200)) 
```

<img src="figure/3.0 Time series plot of the average number of steps taken averaged across all days-1.png" style="display: block; margin: auto;" />

2. Which 5-minute interval, on average across all days in the dataset, contains the maximum number of steps?


```r
# The 5-minute interval that, on average, contains the maximum number of steps
data_int_avg_step[which(data_int_avg_step$`Average Number of Steps Taken` == max(data_int_avg_step$`Average Number of Steps Taken`)),1]
```

```
## [1] 835
```

## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
# Code to describe missing data
sapply(data, function(x) sum(is.na(x)))
```

```
##    steps     date interval 
##     2304        0        0
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

*To fill in the missing values in the dataset, I will use the mean for that 5-minute interval averaged over all days*

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
# Code to show a strategy for imputing missing data
data %>% 
  left_join(rename(data_int_avg_step, interval = Interval), by = "interval") %>%
  mutate(steps = ifelse(is.na(steps), `Average Number of Steps Taken`, steps)) %>%
  select(-`Average Number of Steps Taken`) -> data_imp

head(data_imp)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

```r
# Code to verify no missing values
sapply(data_imp, function(x) sum(is.na(x)))
```

```
##    steps     date interval 
##        0        0        0
```

4.1 Make a histogram of the total number of steps taken each day. 


```r
# Calculating the total number of steps taken per day
data_imp_totals <- as.data.frame(tapply(data_imp$steps, data_imp$date, sum, na.rm = TRUE))

data_imp_totals$date <- rownames(data_imp_totals)
names(data_imp_totals) <- c("total steps", "date")
rownames(data_imp_totals) <- NULL

# Histogram of the total number of steps taken each day
data_imp_totals %>%
  ggplot(aes(x = .[,1])) +
  geom_histogram(col = "#ff964f", 
                 position = "dodge",
                 fill = "#4f5374",
                 breaks = seq(0,25000,2500),
                 bins = 10,
                 boundary = 0) +
  labs(x = "Total Steps Taken",
       y = "Number of Days",
       title = "Total Steps Taken Each Day (without NAs)") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(vjust = -0.5)) +
  scale_x_continuous(breaks = seq(0,25000, 2500)) 
```

<img src="figure/4.2 Histogram of total number of steps taken per day (without missing values)-1.png" style="display: block; margin: auto;" />

4.2 Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
old_mean <- mean(data_totals$`total steps`)
old_median <- median(data_totals$`total steps`)

# Calculating the mean number of total steps taken per day
new_mean <- mean(data_imp_totals$`total steps`)
new_mean
```

```
## [1] 10766.19
```

```r
# Percentage change from old mean to new mean
noquote(paste0(round(100*(new_mean - old_mean)/old_mean,2),"%"))
```

```
## [1] 15.09%
```

```r
# Calculating the median number of total steps taken per day
new_median <- median(data_imp_totals$`total steps`)
new_median
```

```
## [1] 10766.19
```

```r
# Percentage change from old mean to new mean
noquote(paste0(round(100*(new_median - old_median)/old_median,2),"%"))
```

```
## [1] 3.57%
```

*The new mean and median are the same. The new mean has increased from the old mean by 15.09% while the new median has increased from the old median by 3.57%

## Are there differences in activity patterns between weekdays and weekends?

For this part the \color{red}{\verb|weekdays()|}weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
data_imp %>%
  mutate(weekday = ifelse(weekdays(date) %in% c("Saturday", "Sunday"), "weekend", "weekday"))  %>%
  mutate(weekday = factor(weekday, levels = c("weekday", "weekend"))) -> data_imp_wkday
```

2. Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
# Time series plot of the average number of steps taken averaged across all days
data_int_avg_step_wkday <- aggregate(steps ~ interval + weekday,
                            FUN = mean,
                            data = data_imp_wkday,
                            na.rm = TRUE)

names(data_int_avg_step_wkday) <- c("Interval", "Weekday", "Average Number of Steps Taken")

data_int_avg_step_wkday %>%
  ggplot(aes(x = Interval,
             y = `Average Number of Steps Taken`,
             col = Weekday)) +
  scale_fill_manual(values = c("#4f5374","#ff964f")) + 
  geom_line(col = "#4f5374") +
  labs(title = "Average Number of Steps Per Interval Averaged Across All Days") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(vjust = -0.5)) +
  facet_wrap(~ Weekday, 2, 1) + 
  scale_x_continuous(breaks = seq(0,2400, 200)) 
```

<img src="figure/5.1 Time series plot of the average number of steps taken averaged across all days by weekday/weekend-1.png" style="display: block; margin: auto;" />
