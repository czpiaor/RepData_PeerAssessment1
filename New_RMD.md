**Loading and preprocessing the data**
--------------------------------------

    getwd()

    ## [1] "C:/Users/aliu/Documents/Data_Science_Coursera/RepData_PeerAssessment1"

    setwd("/Users/aliu/Documents/Data_Science_Coursera/RepData_PeerAssessment1")
    ## ----loaddata------------------------------------------------------------
    if(!file.exists('activity.csv')){
      unzip('activity.zip')
    }
    data <- read.csv("activity.csv")

**What is mean total number of steps taken per day?**
-----------------------------------------------------

    library(ggplot2)

    ## Warning: package 'ggplot2' was built under R version 3.2.3

    total.steps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
    mean(total.steps, na.rm=TRUE)

    ## [1] 9354.23

    median(total.steps, na.rm=TRUE)

    ## [1] 10395

![](New_RMD_files/figure-markdown_strict/unnamed-chunk-3-1.png)<!-- -->

**What is the average daily activity pattern?**
-----------------------------------------------

    averages <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                          FUN=mean, na.rm=TRUE)

![](New_RMD_files/figure-markdown_strict/unnamed-chunk-5-1.png)<!-- -->

    averages[which.max(averages$steps),]

    ##     interval    steps
    ## 104      835 206.1698

**Imputing missing values**
---------------------------

    missing <- is.na(data$steps)
    table(missing)

    ## missing
    ## FALSE  TRUE 
    ## 15264  2304

**Are there differences in activity patterns between weekdays and weekends?**
-----------------------------------------------------------------------------

    fill.value <- function(steps, interval) {
      filled <- NA
      if (!is.na(steps))
        filled <- c(steps)
      else
        filled <- (averages[averages$interval==interval, "steps"])
      return(filled)
    }
    filled.data <- data
    filled.data$steps <- mapply(fill.value, filled.data$steps, filled.data$interval)

    weekday.or.weekend <- function(date) {
      day <- weekdays(date)
      if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        return("weekday")
      else if (day %in% c("Saturday", "Sunday"))
        return("weekend")
      else
        stop("invalid date")
    }
    filled.data$date <- as.Date(filled.data$date)
    filled.data$day <- sapply(filled.data$date, FUN=weekday.or.weekend)

    averages <- aggregate(steps ~ interval + day, data=filled.data, mean)

![](New_RMD_files/figure-markdown_strict/unnamed-chunk-11-1.png)<!-- -->
