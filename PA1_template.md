Loading and preprocessing the data
----------------------------------

    url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    download.file(url=url,destfile = "ActivityMonitoring.zip")
    unzip("ActivityMonitoring.zip")
    Activity <- read.csv("activity.csv")
    Activity$date <- as.Date(Activity$date, format = "%Y-%m-%d")
    summary(Activity)

    ##      steps             date               interval     
    ##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
    ##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
    ##  Median :  0.00   Median :2012-10-31   Median :1177.5  
    ##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
    ##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
    ##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
    ##  NA's   :2304

What is mean total number of steps taken per day?
-------------------------------------------------

    DailyActivity <- aggregate(. ~ date, data = Activity,FUN=sum,na.rm = TRUE)

    hist(DailyActivity$steps,main = "histogram of the total number of steps taken each day", xlab = "steps per day")

![](PA1_template_files/figure-markdown_strict/mean-1.png)

    mymean <- mean(DailyActivity$steps)
    mymedian <- median(DailyActivity$steps)

-   mean steps per day : 10766
-   median steps per day : 10765

What is the average daily activity pattern?
-------------------------------------------

    AvgDailyPattern <- aggregate(. ~ interval, data = Activity,FUN=mean,na.rm = TRUE)

    ## Adding a column to represent interval as decimal hours (i.e. 30 minutes is 0.50 hours)

    AvgDailyPattern$interval.Hours <- floor(AvgDailyPattern$interval/100) + (AvgDailyPattern$interval/100-floor(AvgDailyPattern$interval/100))/0.6

    plot(steps ~ interval.Hours, type = "l", data=AvgDailyPattern, 
         main = "average daily activity pattern",
         xlab = "hour of the day", xaxt = "n")

    axis(1, at = seq(0, 24, by = 4))

![](PA1_template_files/figure-markdown_strict/avgdaily-1.png)

    ## using dplyr on the following line to extract interval with max steps

     MaxSteps <-  AvgDailyPattern %>% filter(steps == max(steps)) %>% select(interval,steps)

**The moment of the day (HH.mm) with on average the most steps was :
8.35**

Imputing missing values
-----------------------

    sum(is.na(Activity$steps))

    ## [1] 2304

**The total number (rows) of missing values in the dataset is 2304 or 13
%.**

    ## Calculate the mean number of steps per interval in order to attribute that value to the NA's in the dataset

    IntervalAvgActivity <- aggregate(. ~ interval, data = Activity,FUN=mean,na.rm = TRUE)

    ## match on interval to attribute avg steps to missing steps in newly created Activity dataset
    ## avg is rounded to integer as steps are not fractioned

    Activity_Avg4NA <- Activity

    Activity_Avg4NA$steps[is.na(Activity_Avg4NA$steps)] <- IntervalAvgActivity[match(Activity_Avg4NA$interval,IntervalAvgActivity$interval),2]

    ## Warning in Activity_Avg4NA$steps[is.na(Activity_Avg4NA$steps)] <-
    ## IntervalAvgActivity[match(Activity_Avg4NA$interval, : number of items to
    ## replace is not a multiple of replacement length

    Activity_Avg4NA$steps <- round(Activity_Avg4NA$steps,0)

    DailyActivity_Avg4NA <- aggregate(. ~ date, data = Activity_Avg4NA,FUN=sum,na.rm = TRUE)

    hist(DailyActivity_Avg4NA$steps ,main = "histogram of the total number of steps taken each day", xlab = "steps per day")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-2-1.png)

    mean(DailyActivity_Avg4NA$steps)

    ## [1] 10765.64

    median(DailyActivity_Avg4NA$steps)

    ## [1] 10762

Imputing the missing data on the estimates of the total daily number of
steps clearly has an impact. When using the average number of steps per
interval as a strategy to fil missing values, mean values remained
equal, whereas the median value more or less joined the mean value,
clearly an effect of adding a lot of days with average values (as the
NA's where mostly for entire days). However, when rounding the average
steps to integer values (in order to properly represent granular data)
things changed : rounding a sum is different from summing rounded
values, so bias was introduced.

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

    ## please use your words for saturday and sunday if you would be running the line of code below

    Activity_Avg4NA$WeekendFlag <- factor((weekdays(Activity_Avg4NA$date) %in% c("zaterdag","zondag")), levels=c(TRUE, FALSE), labels=c('weekend', 'weekday'))

    WeekendActivity <- aggregate(. ~ WeekendFlag+interval, data = Activity_Avg4NA,FUN=mean,na.rm = TRUE)

    WeekendActivity$interval.Hour <- floor(WeekendActivity$interval/100) + (WeekendActivity$interval/100-floor(WeekendActivity$interval/100))/0.6

    library(lattice)

    xyplot(steps~interval.Hour|WeekendFlag, 
        type = "l",
        data = WeekendActivity,
        xlab="interval start time (hours)", 
        ylab = "number of steps",
        xlim=c(0,24),
        scales = list(x=list(at=seq(0,24,4))),
        layout=c(1,2))

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-3-1.png)
