Untitled
================

# Data Cleaning and Pre-Processing

## Loading Data and Basic Packages

### Load packages

First we need to load the basic packages for the manipulation of data.
Other packages will be loaded later.

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.4.0      ✔ purrr   0.3.4 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.1      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.3      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(dplyr)
library(stringr)
library(arrow)
```

    ## 
    ## Attaching package: 'arrow'
    ## 
    ## The following object is masked from 'package:utils':
    ## 
    ##     timestamp

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'
    ## 
    ## The following object is masked from 'package:arrow':
    ## 
    ##     duration
    ## 
    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(tsibble)
```

    ## 
    ## Attaching package: 'tsibble'
    ## 
    ## The following object is masked from 'package:lubridate':
    ## 
    ##     interval
    ## 
    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, union

``` r
library(ggplot2)
```

### Load data

Now we load in the data. The app_data is the primary data we will use
for now.

``` r
# Personal level data from correction
person_level_data= read.csv('person_level_data.csv')
# Data of art unit changes
aus=read.csv("examiner_aus.csv")
# Get id crosswalk table
ids=read.csv("examiner_ids.csv")
# Get parquet data that details the transactionsfor each examiner, their gender and most likely ethnicity
App_data=read_parquet('apps_gender_rate.parquet')
```

## Clean Data

We have one Dataset that contains everything we might need. All the
patent transactions, patent examiner ID, generd and ethnicity. We will
need to remove the application status dates that are missing as we are
relying on this field to predict the future.

``` r
# Remove Nas
App_data <- App_data %>% 
  filter(!is.na(appl_status_date))

# Clean Date format
#get the date format cleaned
App_data$Date_time=as.POSIXct(App_data$appl_status_date, format="%d%b%Y %H:%M:%S")
#get the quarter number
App_data$Quarter_Year=as.character(yearquarter(App_data$Date_time))
#get the week number
App_data$Week_Year=as.character(yearweek(App_data$Date_time))
```

## Pre-process

Next we need to maks a few transformations to make sure that the data is
in the format we need.First we need to filter the status update to a
decison since we are trying to determine the production (number of
application decisions by art unit in a week). Therefore we will remove
all applications with the status “PEND”. Since we know the only status
tags are:

``` r
unique(App_data$disposal_type)
```

    ## [1] "ISS"  "ABN"  "PEND"

``` r
#create new data fame with all manipulations. App_Data held as clean data
T_Data=App_data

#remove all values before 2018
T_Data <- T_Data %>% 
  filter(Date_time< as.Date("2018-01-01"))



#Remove all the data we will not need based on application status
exclude_list=c("PEND")
T_Data <- T_Data %>%
  filter(!disposal_type %in% exclude_list)

#Data Remain
nrow(T_Data)/nrow(App_data)*100
```

    ## [1] 83.83612

We therefore have 83.8% of our data remaining. This is an acceptable
amout for out analysis.

Now we will need to group our data based on the processed applications.
We will make a gender neutral and 2 gender inclusive(Male,Female) Data
sets

``` r
#rename columns for prophet pacakge
#requires 'ds' and 'y'
#'ds' will be first day of week
#'y' will be the production value

GN_Data <- T_Data %>% 
  group_by(Week_Year) %>% 
  summarise(
    ds = min(Date_time),
    y=n(),
    )


G_Data <- T_Data %>% 
  group_by(Week_Year,gender) %>% 
  summarise(
    ds = min(Date_time),
    y=n(),
    )
```

    ## `summarise()` has grouped output by 'Week_Year'. You can override using the
    ## `.groups` argument.

``` r
M_Data <- T_Data %>% 
  group_by(Week_Year,gender) %>% 
  summarise(
    ds = min(Date_time),
    y=n(),
    ) %>%
  filter(gender=="male")
```

    ## `summarise()` has grouped output by 'Week_Year'. You can override using the
    ## `.groups` argument.

``` r
F_Data <- T_Data %>% 
  group_by(Week_Year,gender) %>% 
  summarise(
    ds = min(Date_time),
    y=n(),
    ) %>%
  filter(gender=="female")
```

    ## `summarise()` has grouped output by 'Week_Year'. You can override using the
    ## `.groups` argument.

``` r
#remove(T_Data,App_data)
```

Now we need to separate the gender neutral dataframe to keep out the
last year

``` r
fdate="2017-01-01"
GN_Data_Train <- GN_Data %>%
  filter(ds< as.Date(fdate))
GN_Data_Test <- GN_Data %>%
  filter(ds>= as.Date(fdate))
```

# Use ‘Prophet to predict’

Now that we have our datasets we can use the prohet package to predict
the rates and future state. Future is creating an empty dataframe for us
for 365 days in the future.

``` r
#install.packages('prophet')
library(prophet)
```

    ## Loading required package: Rcpp

    ## Loading required package: rlang

    ## 
    ## Attaching package: 'rlang'

    ## The following object is masked from 'package:arrow':
    ## 
    ##     string

    ## The following objects are masked from 'package:purrr':
    ## 
    ##     %@%, as_function, flatten, flatten_chr, flatten_dbl, flatten_int,
    ##     flatten_lgl, flatten_raw, invoke, splice

``` r
m<-prophet(GN_Data_Train)
```

    ## Disabling daily seasonality. Run prophet with daily.seasonality=TRUE to override this.

``` r
future <- make_future_dataframe(m, periods = 365)
tail(future)
```

    ##              ds
    ## 1225 2017-12-21
    ## 1226 2017-12-22
    ## 1227 2017-12-23
    ## 1228 2017-12-24
    ## 1229 2017-12-25
    ## 1230 2017-12-26

Now we can make some predictions

``` r
forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
```

    ##              ds     yhat yhat_lower yhat_upper
    ## 1225 2017-12-21 4170.108   3854.280   4499.686
    ## 1226 2017-12-22 4216.971   3927.194   4531.210
    ## 1227 2017-12-23 4193.634   3893.858   4500.360
    ## 1228 2017-12-24 4173.281   3868.267   4505.710
    ## 1229 2017-12-25 4099.485   3784.820   4412.369
    ## 1230 2017-12-26 4067.710   3743.887   4397.794

It looks like we are predicting between 3,600 and 4,300 applications per
week in end of 2017. This is not accounting for hollidays, which we will
need to fix.

``` r
plot(m, forecast)
```

![](Assignment-5_files/figure-gfm/prediction%203-1.png)<!-- -->

``` r
prophet_plot_components(m, forecast)
```

![](Assignment-5_files/figure-gfm/prediction%204-1.png)<!-- -->

Let’s account for US publich Hollidays based on:
<https://www.commerce.gov/hr/employees/leave/holidays>
<https://www.kaggle.com/datasets/donnetew/us-holiday-dates-2004-2021>

``` r
Stat_Holliday <- data_frame(
  holiday = 'Stat_Holliday',
  ds = as.Date(c('2004-07-04','2005-07-04','2006-07-04','2007-07-04','2008-07-04','2009-07-04','2010-07-04','2011-07-04','2012-07-04','2013-07-04','2014-07-04','2015-07-04','2016-07-04','2017-07-04','2018-07-04','2019-07-04','2020-07-04','2021-07-04','2004-12-25','2005-12-25','2006-12-25','2007-12-25','2008-12-25','2009-12-25','2010-12-25','2011-12-25','2012-12-25','2013-12-25','2014-12-25','2015-12-25','2016-12-25','2017-12-25','2018-12-25','2019-12-25','2020-12-25','2021-12-25','2004-12-24','2005-12-24','2006-12-24','2007-12-24','2008-12-24','2009-12-24','2010-12-24','2011-12-24','2012-12-24','2013-12-24','2014-12-24','2015-12-24','2016-12-24','2017-12-24','2018-12-24','2019-12-24','2020-12-24','2021-12-24','2007-10-08','2012-10-08','2018-10-08','2006-10-09','2017-10-09','2005-10-10','2011-10-10','2016-10-10','2004-10-11','2010-10-11','2021-10-11','2009-10-12','2015-10-12','2020-10-12','2008-10-13','2014-10-13','2013-10-14','2019-10-14','2010-04-04','2007-04-08','2018-04-08','2004-04-11','2015-04-12','2012-04-15','2017-04-16','2020-04-19','2009-04-19','2014-04-20','2006-04-23','2011-04-24','2008-04-27','2019-04-28','2021-05-02','2005-05-01','2016-05-01','2013-05-05','2020-06-19','2021-06-19','2004-06-19','2005-06-19','2006-06-19','2007-06-19','2008-06-19','2009-06-19','2010-06-19','2011-06-19','2012-06-19','2013-06-19','2014-06-19','2015-06-19','2016-06-19','2017-06-19','2018-06-19','2019-06-19','2008-09-01','2014-09-01','2013-09-02','2019-09-02','2007-09-03','2012-09-03','2018-09-03','2006-09-04','2017-09-04','2005-09-05','2011-09-05','2016-09-05','2004-09-06','2010-09-06','2021-09-06','2009-09-07','2015-09-07','2020-09-07','2008-08-30','2014-08-30','2008-08-31','2014-08-31','2013-08-31','2019-08-31','2013-09-01','2019-09-01','2007-09-01','2012-09-01','2018-09-01','2007-09-02','2012-09-02','2018-09-02','2006-09-02','2017-09-02','2006-09-03','2017-09-03','2005-09-03','2011-09-03','2016-09-03','2005-09-04','2011-09-04','2016-09-04','2004-09-04','2010-09-04','2021-09-04','2004-09-05','2010-09-05','2021-09-05','2009-09-05','2015-09-05','2020-09-05','2009-09-06','2015-09-06','2020-09-06','2007-01-15','2018-01-15','2006-01-16','2012-01-16','2017-01-16','2005-01-17','2011-01-17','2010-01-18','2016-01-18','2021-01-18','2004-01-19','2009-01-19','2015-01-19','2014-01-20','2020-01-20','2008-01-21','2013-01-21','2019-01-21','2009-05-25','2015-05-25','2020-05-25','2008-05-26','2014-05-26','2013-05-27','2019-05-27','2007-05-28','2012-05-28','2018-05-28','2006-05-29','2017-05-29','2005-05-30','2011-05-30','2016-05-30','2004-05-31','2010-05-31','2021-05-31','2012-01-01','2013-01-01','2014-01-01','2015-01-01','2016-01-01','2017-01-01','2018-01-01','2019-01-01','2020-01-01','2011-01-01','2004-01-01','2005-01-01','2006-01-01','2007-01-01','2008-01-01','2009-01-01','2010-01-01','2021-01-01','2004-12-31','2005-12-31','2006-12-31','2007-12-31','2008-12-31','2009-12-31','2010-12-31','2011-12-31','2012-12-31','2013-12-31','2014-12-31','2015-12-31','2016-12-31','2017-12-31','2018-12-31','2019-12-31','2020-12-31','2021-12-31','2007-11-22','2012-11-22','2018-11-22','2006-11-23','2017-11-23','2005-11-24','2011-11-24','2016-11-24','2004-11-25','2010-11-25','2021-11-25','2009-11-26','2015-11-26','2020-11-26','2008-11-27','2014-11-27','2013-11-28','2019-11-28','2007-11-21','2012-11-21','2018-11-21','2006-11-22','2017-11-22','2005-11-23','2011-11-23','2016-11-23','2004-11-24','2010-11-24','2021-11-24','2009-11-25','2015-11-25','2020-11-25','2008-11-26','2014-11-26','2013-11-27','2019-11-27','2004-02-14','2005-02-14','2006-02-14','2007-02-14','2008-02-14','2009-02-14','2010-02-14','2011-02-14','2012-02-14','2013-02-14','2014-02-14','2015-02-14','2016-02-14','2017-02-14','2018-02-14','2019-02-14','2020-02-14','2021-02-14','2004-11-11','2005-11-11','2006-11-11','2007-11-11','2008-11-11','2009-11-11','2010-11-11','2011-11-11','2012-11-11','2013-11-11','2014-11-11','2015-11-11','2016-11-11','2017-11-11','2018-11-11','2019-11-11','2020-11-11','2021-11-11','2016-02-15','2015-02-16','2014-02-17','2020-02-17','2013-02-18','2019-02-18','2018-02-19','2012-02-20','2017-02-20','2011-02-21','2010-02-15','2021-02-15','2004-02-16','2009-02-16','2008-02-18','2007-02-19','2006-02-20','2005-02-21','2008-03-23','2005-03-27','2016-03-27','2013-03-31','2018-04-01','2010-04-04','2021-04-04','2015-04-05','2007-04-08','2012-04-08','2004-04-11','2009-04-12','2020-04-12','2006-04-16','2017-04-16','2014-04-20','2019-04-21','2011-04-24')
),
  lower_window = 0,
  upper_window = 1
)
```

    ## Warning: `data_frame()` was deprecated in tibble 1.1.0.
    ## ℹ Please use `tibble()` instead.

``` r
Other_events <- data_frame(
  holiday = 'Other_events',
  ds = as.Date(c('2010-02-07', '2014-02-02', '2016-02-07')),
  lower_window = 0,
  upper_window = 1
)
holidays <- bind_rows(Stat_Holliday, Other_events)
```

``` r
m<-prophet(GN_Data_Train,holidays = holidays)
```

    ## Disabling daily seasonality. Run prophet with daily.seasonality=TRUE to override this.

``` r
future <- make_future_dataframe(m, periods = 365)
tail(future)
```

    ##              ds
    ## 1225 2017-12-21
    ## 1226 2017-12-22
    ## 1227 2017-12-23
    ## 1228 2017-12-24
    ## 1229 2017-12-25
    ## 1230 2017-12-26

``` r
plot(m, forecast)
```

![](Assignment-5_files/figure-gfm/prediction%207-1.png)<!-- -->

``` r
prophet_plot_components(m, forecast)
```

![](Assignment-5_files/figure-gfm/prediction%208-1.png)<!-- -->

``` r
plot(sin, -pi, 2*pi) # see ?plot.function
```

![](Assignment-5_files/figure-gfm/prediction%208-2.png)<!-- -->
