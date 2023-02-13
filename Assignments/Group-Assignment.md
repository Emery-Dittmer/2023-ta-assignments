Group Assignment
================

# Background

This notebook contains all code and answers to the talent analytics
assignment. Specifically we will be tackling the following question:

What are the organizational and social factors associated with the
length of patent application prosecution?

# Data Cleaning and Pre-Processing

In order to analyze the data, we first need to pre-process it in a way
that is usable for analysis.

## Loading Data and Basic Packages

### Load packages

First we need to load the basic packages for the manipulation of data.
Other packages will be loaded as needed.

``` r
#library(tidyverse)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(stringr)
library(arrow)
```

    ## 
    ## Attaching package: 'arrow'

    ## The following object is masked from 'package:utils':
    ## 
    ##     timestamp

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:arrow':
    ## 
    ##     duration

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
#library(tsibble)
library(ggplot2)
```

### Load data

Now we load in the data. The app_gender_rate data is the primary data we
will use for now. This data contains the transaction data for all
applications, the examiner who processed them and their associated
traits such as gender and ethnicity.

``` r
App_data=read_parquet('apps_gender_rate.parquet')
```

## Clean Data

Now that we have the data, we can clean and pre-process it. We will
remove all the fields with NAs so that we can get meaningful insights.

``` r
# Remove Nas from status date
App_data <- App_data %>% 
  filter(!is.na(appl_status_date))

# Remove Nas from gender
App_data <- App_data %>% 
  filter(!is.na(gender))
  
# Clean Date format
#get the date format cleaned
App_data$Date_time=as.Date(App_data$appl_status_date, format="%d%b%Y")

#get the date format for the filing date cleaned
App_data$filing_date=as.Date(App_data$filing_date, format="%d%b%Y")
```

## Pre-process

Next we need to maks a few transformations to make sure that the data is
in the format we need.First we need to filter the status update to a
decison since we are trying to determine the amount of time between
application date and a descision. Next, we will need to remove all data
before 2017 decision previous examples have shown that this data is full
of outliers and other incomplte data. Next we will have to convert some
data fields to a nyew format. Last, we will need to add a field that
calculates the amount of time between each application and each
desscion.

What type of applications are there?

``` r
unique(App_data$disposal_type)
```

    ## [1] "ISS"  "ABN"  "PEND"

We will remove all “PEND” type applications. Then make a new field to
compute time.

``` r
#create new data fame with all manipulations. App_Data held as clean data
T_Data=App_data

#Remove all the data we will not need based on application status
exclude_list=c("PEND")
T_Data <- T_Data %>%
  filter(!disposal_type %in% exclude_list)
```

Now we can remove the data from before 2017 since it has high levels of
outliers and bad data.

``` r
#remove all values before 2017
T_Data <- T_Data %>% 
  filter(Date_time<= as.Date("2017-01-01"))

#Data Remain
nrow(T_Data)/nrow(App_data)*100
```

    ## [1] 79.78629

We therefore have 79.8% of our data remaining. This is an acceptable
amount for out analysis.

Now we can remove some un-needed datacolumns. These are saved so we can
merge these later if we need to

``` r
#list of data to keep)
keep=c("filing_date","disposal_type","tc","gender","race","tenure_days","Date_time") #examiner_art_unit not kept as produces too man variables for packages
T_Data = subset(T_Data, select = keep)
```

Pen-ultimately we will change the data type on a few columns for
analysis ease

``` r
#Setting Gender as factor
T_Data$gender = as.factor(T_Data$gender)

#Setting ethnicity as factor
T_Data$race = as.factor(T_Data$race)

# #Art unit as a factor in case
# T_Data$examiner_art_unit = as.factor(T_Data$examiner_art_unit)

#setting the technology center as a factor
T_Data$tc = as.factor(T_Data$tc)
```

## Feature Engineering

Now the last step is to add a column that computes the time between
application date and decision date. This column is called the
application time and is the time in days between application filing and

``` r
#this is the amount of time in days that the applications take
T_Data$Application_time <- T_Data$Date_time - T_Data$filing_date
T_Data$Application_time <- as.numeric(T_Data$Application_time)

#adding the year of filling and the year of approval to see time's effect
T_Data$filing_year= as.numeric(year(T_Data$filing_date))
T_Data$descision_year=as.numeric(year(T_Data$Date_time))
```

garbage collection (optional)

``` r
rm(App_data,exclude_list,keep)
```

# Analysis

Now that the data is in a clean and useable format let’s examine the
data more closely. First lets look at the summary stats for all the data

``` r
# 
# library(vtable)
# sumtab=sumtable(T_Data)
# sumtab
```

Now lets look at the correlation plots

``` r
# library(ggplot2)
# library(GGally)
# d=ggpairs(T_Data)
# d
```

``` r
require(corrplot)
```

    ## Loading required package: corrplot

    ## corrplot 0.92 loaded

``` r
num_cols <- unlist(lapply(T_Data, is.numeric))       
quanvars <- T_Data[ , num_cols]           
corr_matrix <- cor(quanvars)
corrplot(corr_matrix)
```

![](Group-Assignment_files/figure-gfm/pair%20plots-1.png)<!-- -->

``` r
library(ggfortify)
pca=prcomp(quanvars, scale=TRUE)
autoplot(pca, data = quanvars, loadings = TRUE, loadings.label = TRUE )
```

![](Group-Assignment_files/figure-gfm/pca%20analysis-1.png)<!-- -->

``` r
library(tree)
library(rpart)
library(rpart.plot)


myoverfittedtree=rpart(Application_time~disposal_type+tc+gender+race+tenure_days+descision_year+filing_year,data = T_Data, control=rpart.control(cp=0.001))
#this will generate a plot of the decision tree
rpart.plot(myoverfittedtree)
```

![](Group-Assignment_files/figure-gfm/tree%20model-1.png)<!-- --> Let’s
fit the tree using the bast control paramter

``` r
plotcp(myoverfittedtree)
```

![](Group-Assignment_files/figure-gfm/plot%20the%20cp%20control%20paramter-1.png)<!-- -->

``` r
#This returns the optimal cp value
opt_cp=myoverfittedtree$cptable[which.min(myoverfittedtree$cptable[,"xerror"]),"CP"]
```

Now we can plot the optimal tree

``` r
optimal_tree=rpart(Application_time~disposal_type+tc+gender+race+tenure_days+descision_year+filing_year,data = T_Data,control=rpart.control(cp=opt_cp))

rpart.plot(optimal_tree)
```

![](Group-Assignment_files/figure-gfm/tree%202-1.png)<!-- -->

``` r
summary(optimal_tree)
```

    ## Call:
    ## rpart(formula = Application_time ~ disposal_type + tc + gender + 
    ##     race + tenure_days + descision_year + filing_year, data = T_Data, 
    ##     control = rpart.control(cp = opt_cp))
    ##   n= 1364979 
    ## 
    ##             CP nsplit  rel error     xerror         xstd
    ## 1  0.251009738      0 1.00000000 1.00000125 1.991457e-03
    ## 2  0.097685510      2 0.49798052 0.49798321 8.251991e-04
    ## 3  0.064289319      3 0.40029501 0.40029796 7.098179e-04
    ## 4  0.020633202      5 0.27171638 0.27171989 3.678904e-04
    ## 5  0.019174124      7 0.23044997 0.23045371 3.234953e-04
    ## 6  0.016287059      8 0.21127585 0.21127975 3.040737e-04
    ## 7  0.009860970     10 0.17870173 0.17870572 2.366037e-04
    ## 8  0.008921354     11 0.16884076 0.16884471 2.204072e-04
    ## 9  0.008804216     12 0.15991940 0.15992344 2.037169e-04
    ## 10 0.008183935     13 0.15111519 0.15111948 1.812018e-04
    ## 11 0.006438717     14 0.14293125 0.14293562 1.737064e-04
    ## 12 0.005437419     16 0.13005382 0.13005820 1.544272e-04
    ## 13 0.004962542     17 0.12461640 0.12462097 1.471329e-04
    ## 14 0.004769820     19 0.11469131 0.11469607 1.426699e-04
    ## 15 0.004530210     20 0.10992150 0.10992633 1.367532e-04
    ## 16 0.004368713     22 0.10086108 0.10086587 1.289911e-04
    ## 17 0.003843402     23 0.09649236 0.09649719 1.250465e-04
    ## 18 0.003381739     24 0.09264896 0.09265385 1.210854e-04
    ## 19 0.003127629     25 0.08926722 0.08927216 1.181760e-04
    ## 20 0.002642142     26 0.08613959 0.08614389 1.124235e-04
    ## 21 0.002248247     27 0.08349745 0.08350200 1.035672e-04
    ## 22 0.001980702     28 0.08124920 0.08125407 9.627814e-05
    ## 23 0.001911216     29 0.07926850 0.07927338 9.182941e-05
    ## 24 0.001706026     30 0.07735729 0.07736222 8.979895e-05
    ## 25 0.001664475     31 0.07565126 0.07565620 8.904157e-05
    ## 26 0.001594757     32 0.07398678 0.07399175 8.767283e-05
    ## 27 0.001507368     33 0.07239203 0.07239700 8.477050e-05
    ## 28 0.001463639     34 0.07088466 0.07088976 8.332612e-05
    ## 29 0.001334036     35 0.06942102 0.06942614 8.209240e-05
    ## 30 0.001301243     36 0.06808698 0.06809209 8.077708e-05
    ## 31 0.001296615     37 0.06678574 0.06707508 7.944035e-05
    ## 32 0.001196235     38 0.06548913 0.06549417 7.793170e-05
    ## 33 0.001180343     39 0.06429289 0.06407656 7.749098e-05
    ## 34 0.001098802     41 0.06193220 0.06193743 7.448329e-05
    ## 35 0.001080205     42 0.06083340 0.06083863 7.426208e-05
    ## 36 0.001080037     43 0.05975320 0.05978712 7.298836e-05
    ## 37 0.001068144     44 0.05867316 0.05890744 7.234332e-05
    ## 38 0.001045054     45 0.05760502 0.05761017 7.053824e-05
    ## 39 0.001029465     46 0.05655996 0.05637856 6.951607e-05
    ## 40 0.001000000     47 0.05553050 0.05553548 6.603512e-05
    ## 
    ## Variable importance
    ## descision_year    filing_year    tenure_days             tc 
    ##             52             41              5              1 
    ## 
    ## Node number 1: 1364979 observations,    complexity param=0.2510097
    ##   mean=1488.732, MSE=974498 
    ##   left son=2 (566415 obs) right son=3 (798564 obs)
    ##   Primary splits:
    ##       filing_year    < 2008.5 to the right, improve=0.168978100, (0 missing)
    ##       descision_year < 2005.5 to the left,  improve=0.034913650, (0 missing)
    ##       disposal_type  splits as  LR,         improve=0.021632840, (0 missing)
    ##       tenure_days    < 5334.5 to the left,  improve=0.013210860, (0 missing)
    ##       tc             splits as  LLRL,       improve=0.001692823, (0 missing)
    ##   Surrogate splits:
    ##       descision_year < 2012.5 to the right, agree=0.829, adj=0.588, (0 split)
    ##       tenure_days    < 5329.5 to the left,  agree=0.666, adj=0.195, (0 split)
    ##       tc             splits as  RRRL,       agree=0.609, adj=0.058, (0 split)
    ## 
    ## Node number 2: 566415 observations,    complexity param=0.0206332
    ##   mean=1006.903, MSE=214760.4 
    ##   left son=4 (273004 obs) right son=5 (293411 obs)
    ##   Primary splits:
    ##       filing_year    < 2011.5 to the right, improve=0.189017700, (0 missing)
    ##       descision_year < 2013.5 to the left,  improve=0.039705930, (0 missing)
    ##       tenure_days    < 6252.5 to the right, improve=0.012227210, (0 missing)
    ##       tc             splits as  LRRR,       improve=0.005589913, (0 missing)
    ##       disposal_type  splits as  RL,         improve=0.005577725, (0 missing)
    ##   Surrogate splits:
    ##       descision_year < 2014.5 to the right, agree=0.773, adj=0.529, (0 split)
    ##       tenure_days    < 3971   to the left,  agree=0.548, adj=0.062, (0 split)
    ##       disposal_type  splits as  RL,         agree=0.530, adj=0.024, (0 split)
    ##       tc             splits as  RRRL,       agree=0.526, adj=0.016, (0 split)
    ##       race           splits as  RLLRR,      agree=0.519, adj=0.003, (0 split)
    ## 
    ## Node number 3: 798564 observations,    complexity param=0.2510097
    ##   mean=1830.49, MSE=1231907 
    ##   left son=6 (651916 obs) right son=7 (146648 obs)
    ##   Primary splits:
    ##       descision_year < 2012.5 to the left,  improve=0.450316200, (0 missing)
    ##       disposal_type  splits as  LR,         improve=0.078890740, (0 missing)
    ##       filing_year    < 2003.5 to the right, improve=0.036863720, (0 missing)
    ##       tc             splits as  LLRR,       improve=0.004286930, (0 missing)
    ##       tenure_days    < 3340.5 to the left,  improve=0.001024113, (0 missing)
    ## 
    ## Node number 4: 273004 observations,    complexity param=0.004962542
    ##   mean=798.0299, MSE=101793.4 
    ##   left son=8 (79709 obs) right son=9 (193295 obs)
    ##   Primary splits:
    ##       filing_year    < 2013.5 to the right, improve=0.17755530, (0 missing)
    ##       descision_year < 2014.5 to the left,  improve=0.14172510, (0 missing)
    ##       tenure_days    < 3760.5 to the right, improve=0.02864476, (0 missing)
    ##       disposal_type  splits as  RL,         improve=0.01858935, (0 missing)
    ##       tc             splits as  LRRR,       improve=0.01632819, (0 missing)
    ##   Surrogate splits:
    ##       descision_year < 2015.5 to the right, agree=0.720, adj=0.041, (0 split)
    ##       tenure_days    < 1337.5 to the left,  agree=0.708, adj=0.000, (0 split)
    ## 
    ## Node number 5: 293411 observations,    complexity param=0.0206332
    ##   mean=1201.248, MSE=241506.8 
    ##   left son=10 (231247 obs) right son=11 (62164 obs)
    ##   Primary splits:
    ##       descision_year < 2014.5 to the left,  improve=0.4501578000, (0 missing)
    ##       filing_year    < 2009.5 to the right, improve=0.0445831700, (0 missing)
    ##       tenure_days    < 6242.5 to the right, improve=0.0229152400, (0 missing)
    ##       tc             splits as  LLRR,       improve=0.0078053930, (0 missing)
    ##       race           splits as  RLRRL,      improve=0.0003362282, (0 missing)
    ## 
    ## Node number 6: 651916 observations,    complexity param=0.06428932
    ##   mean=1477.233, MSE=543623 
    ##   left son=12 (180275 obs) right son=13 (471641 obs)
    ##   Primary splits:
    ##       descision_year < 2006.5 to the left,  improve=0.147370600, (0 missing)
    ##       filing_year    < 2005.5 to the right, improve=0.051999670, (0 missing)
    ##       disposal_type  splits as  LR,         improve=0.031339500, (0 missing)
    ##       tc             splits as  LLRR,       improve=0.014572390, (0 missing)
    ##       tenure_days    < 6331.5 to the right, improve=0.002552482, (0 missing)
    ##   Surrogate splits:
    ##       filing_year < 2002.5 to the left,  agree=0.830, adj=0.385, (0 split)
    ##       tenure_days < 1002   to the left,  agree=0.724, adj=0.000, (0 split)
    ## 
    ## Node number 7: 146648 observations,    complexity param=0.09768551
    ##   mean=3400.872, MSE=1270787 
    ##   left son=14 (89646 obs) right son=15 (57002 obs)
    ##   Primary splits:
    ##       filing_year    < 2004.5 to the right, improve=0.69724950, (0 missing)
    ##       tenure_days    < 6096.5 to the left,  improve=0.20792750, (0 missing)
    ##       descision_year < 2014.5 to the left,  improve=0.18220160, (0 missing)
    ##       disposal_type  splits as  LR,         improve=0.10742370, (0 missing)
    ##       tc             splits as  RRRL,       improve=0.05824533, (0 missing)
    ##   Surrogate splits:
    ##       tenure_days < 6218.5 to the left,  agree=0.707, adj=0.245, (0 split)
    ## 
    ## Node number 8: 79709 observations
    ##   mean=588.6749, MSE=48338.89 
    ## 
    ## Node number 9: 193295 observations,    complexity param=0.004962542
    ##   mean=884.3615, MSE=98309.29 
    ##   left son=18 (65107 obs) right son=19 (128188 obs)
    ##   Primary splits:
    ##       descision_year < 2014.5 to the left,  improve=0.43508470, (0 missing)
    ##       tenure_days    < 3750.5 to the right, improve=0.03047452, (0 missing)
    ##       filing_year    < 2012.5 to the right, improve=0.01952294, (0 missing)
    ##       tc             splits as  LRRR,       improve=0.01878861, (0 missing)
    ##       disposal_type  splits as  RL,         improve=0.01242101, (0 missing)
    ##   Surrogate splits:
    ##       tenure_days < 791    to the left,  agree=0.663, adj=0, (0 split)
    ## 
    ## Node number 10: 231247 observations,    complexity param=0.005437419
    ##   mean=1030.295, MSE=125913.9 
    ##   left son=20 (85686 obs) right son=21 (145561 obs)
    ##   Primary splits:
    ##       descision_year < 2012.5 to the left,  improve=0.2483990000, (0 missing)
    ##       filing_year    < 2010.5 to the right, improve=0.0955895200, (0 missing)
    ##       tenure_days    < 6251.5 to the right, improve=0.0275855700, (0 missing)
    ##       tc             splits as  LLRR,       improve=0.0143732600, (0 missing)
    ##       race           splits as  RLRRL,      improve=0.0006910081, (0 missing)
    ##   Surrogate splits:
    ##       filing_year < 2009.5 to the left,  agree=0.712, adj=0.222, (0 split)
    ##       tenure_days < 684    to the left,  agree=0.629, adj=0.000, (0 split)
    ## 
    ## Node number 11: 62164 observations,    complexity param=0.003843402
    ##   mean=1837.188, MSE=158371.7 
    ##   left son=22 (29672 obs) right son=23 (32492 obs)
    ##   Primary splits:
    ##       filing_year    < 2010.5 to the right, improve=0.519285400, (0 missing)
    ##       descision_year < 2015.5 to the left,  improve=0.285551000, (0 missing)
    ##       tenure_days    < 3310   to the left,  improve=0.024681790, (0 missing)
    ##       tc             splits as  RLLL,       improve=0.004594591, (0 missing)
    ##       disposal_type  splits as  LR,         improve=0.004135412, (0 missing)
    ##   Surrogate splits:
    ##       tenure_days    < 3757.5 to the left,  agree=0.568, adj=0.094, (0 split)
    ##       descision_year < 2015.5 to the left,  agree=0.536, adj=0.027, (0 split)
    ##       tc             splits as  RRRL,       agree=0.524, adj=0.003, (0 split)
    ##       race           splits as  RLLRR,      agree=0.524, adj=0.003, (0 split)
    ##       disposal_type  splits as  LR,         agree=0.524, adj=0.002, (0 split)
    ## 
    ## Node number 12: 180275 observations,    complexity param=0.006438717
    ##   mean=1019.416, MSE=185796.9 
    ##   left son=24 (84009 obs) right son=25 (96266 obs)
    ##   Primary splits:
    ##       descision_year < 2004.5 to the left,  improve=0.226135100, (0 missing)
    ##       tc             splits as  LLRR,       improve=0.102930100, (0 missing)
    ##       filing_year    < 2003.5 to the right, improve=0.063457220, (0 missing)
    ##       race           splits as  RLRRL,      improve=0.008654410, (0 missing)
    ##       tenure_days    < 6336.5 to the right, improve=0.007349834, (0 missing)
    ##   Surrogate splits:
    ##       filing_year < 2001.5 to the left,  agree=0.743, adj=0.449, (0 split)
    ##       tc          splits as  RLRR,       agree=0.565, adj=0.067, (0 split)
    ##       tenure_days < 976.5  to the left,  agree=0.534, adj=0.001, (0 split)
    ## 
    ## Node number 13: 471641 observations,    complexity param=0.06428932
    ##   mean=1652.225, MSE=569658.6 
    ##   left son=26 (406508 obs) right son=27 (65133 obs)
    ##   Primary splits:
    ##       filing_year    < 2002.5 to the right, improve=0.442184800, (0 missing)
    ##       descision_year < 2010.5 to the left,  improve=0.073956910, (0 missing)
    ##       disposal_type  splits as  LR,         improve=0.059949740, (0 missing)
    ##       tenure_days    < 5565.5 to the left,  improve=0.017885400, (0 missing)
    ##       tc             splits as  LLLR,       improve=0.002730492, (0 missing)
    ## 
    ## Node number 14: 89646 observations,    complexity param=0.008804216
    ##   mean=2650.269, MSE=337563.9 
    ##   left son=28 (59503 obs) right son=29 (30143 obs)
    ##   Primary splits:
    ##       filing_year    < 2006.5 to the right, improve=0.386999700, (0 missing)
    ##       descision_year < 2014.5 to the left,  improve=0.357409800, (0 missing)
    ##       disposal_type  splits as  LR,         improve=0.022295820, (0 missing)
    ##       tenure_days    < 6249.5 to the left,  improve=0.020029520, (0 missing)
    ##       tc             splits as  RRRL,       improve=0.005471597, (0 missing)
    ##   Surrogate splits:
    ##       tenure_days < 6454.5 to the left,  agree=0.664, adj=0, (0 split)
    ## 
    ## Node number 15: 57002 observations,    complexity param=0.00986097
    ##   mean=4581.331, MSE=458910.5 
    ##   left son=30 (22827 obs) right son=31 (34175 obs)
    ##   Primary splits:
    ##       filing_year    < 2002.5 to the right, improve=0.50142780, (0 missing)
    ##       descision_year < 2014.5 to the left,  improve=0.27791270, (0 missing)
    ##       tenure_days    < 6096.5 to the left,  improve=0.05988059, (0 missing)
    ##       disposal_type  splits as  LR,         improve=0.05216695, (0 missing)
    ##       tc             splits as  RRRL,       improve=0.04739637, (0 missing)
    ##   Surrogate splits:
    ##       tenure_days   < 6095   to the left,  agree=0.658, adj=0.145, (0 split)
    ##       disposal_type splits as  LR,         agree=0.627, adj=0.068, (0 split)
    ##       tc            splits as  RRRL,       agree=0.625, adj=0.063, (0 split)
    ##       race          splits as  RRRLR,      agree=0.600, adj=0.000, (0 split)
    ## 
    ## Node number 18: 65107 observations
    ##   mean=594.1638, MSE=44490.05 
    ## 
    ## Node number 19: 128188 observations,    complexity param=0.001664475
    ##   mean=1031.754, MSE=61146.91 
    ##   left son=38 (70752 obs) right son=39 (57436 obs)
    ##   Primary splits:
    ##       descision_year < 2015.5 to the left,  improve=0.282463500, (0 missing)
    ##       filing_year    < 2012.5 to the right, improve=0.271956400, (0 missing)
    ##       tenure_days    < 5164   to the right, improve=0.015848310, (0 missing)
    ##       tc             splits as  LRRR,       improve=0.008310307, (0 missing)
    ##       disposal_type  splits as  RL,         improve=0.005748181, (0 missing)
    ##   Surrogate splits:
    ##       filing_year < 2012.5 to the left,  agree=0.574, adj=0.049, (0 split)
    ##       tenure_days < 3784.5 to the right, agree=0.565, adj=0.029, (0 split)
    ## 
    ## Node number 20: 85686 observations,    complexity param=0.001706026
    ##   mean=799.7901, MSE=84086.14 
    ##   left son=40 (36876 obs) right son=41 (48810 obs)
    ##   Primary splits:
    ##       filing_year    < 2009.5 to the right, improve=0.314962100, (0 missing)
    ##       descision_year < 2011.5 to the left,  improve=0.178688300, (0 missing)
    ##       disposal_type  splits as  RL,         improve=0.016810160, (0 missing)
    ##       tenure_days    < 6251.5 to the right, improve=0.015692540, (0 missing)
    ##       tc             splits as  LLRR,       improve=0.007708624, (0 missing)
    ##   Surrogate splits:
    ##       tenure_days < 2906.5 to the left,  agree=0.57, adj=0, (0 split)
    ## 
    ## Node number 21: 145561 observations,    complexity param=0.004368713
    ##   mean=1165.983, MSE=100847.9 
    ##   left son=42 (115811 obs) right son=43 (29750 obs)
    ##   Primary splits:
    ##       filing_year    < 2009.5 to the right, improve=0.3958663000, (0 missing)
    ##       descision_year < 2013.5 to the left,  improve=0.1627888000, (0 missing)
    ##       tenure_days    < 6253.5 to the right, improve=0.0112008100, (0 missing)
    ##       tc             splits as  LRRR,       improve=0.0063528850, (0 missing)
    ##       gender         splits as  LR,         improve=0.0005147008, (0 missing)
    ## 
    ## Node number 22: 29672 observations
    ##   mean=1537.094, MSE=52879.91 
    ## 
    ## Node number 23: 32492 observations
    ##   mean=2111.236, MSE=97365.24 
    ## 
    ## Node number 24: 84009 observations,    complexity param=0.001180343
    ##   mean=799.9955, MSE=87535.45 
    ##   left son=48 (22830 obs) right son=49 (61179 obs)
    ##   Primary splits:
    ##       descision_year < 2002.5 to the left,  improve=0.191075800, (0 missing)
    ##       tc             splits as  LLRL,       improve=0.103647000, (0 missing)
    ##       filing_year    < 2001.5 to the right, improve=0.094873610, (0 missing)
    ##       race           splits as  RLRRL,      improve=0.010758370, (0 missing)
    ##       tenure_days    < 5770   to the left,  improve=0.007942693, (0 missing)
    ##   Surrogate splits:
    ##       filing_year < 2000.5 to the left,  agree=0.753, adj=0.091, (0 split)
    ##       tenure_days < 5770   to the left,  agree=0.740, adj=0.044, (0 split)
    ## 
    ## Node number 25: 96266 observations,    complexity param=0.006438717
    ##   mean=1210.899, MSE=192866.4 
    ##   left son=50 (70672 obs) right son=51 (25594 obs)
    ##   Primary splits:
    ##       filing_year    < 2001.5 to the right, improve=0.514630400, (0 missing)
    ##       tc             splits as  LLRR,       improve=0.072861180, (0 missing)
    ##       descision_year < 2005.5 to the left,  improve=0.026385720, (0 missing)
    ##       disposal_type  splits as  LR,         improve=0.012971650, (0 missing)
    ##       tenure_days    < 6336.5 to the right, improve=0.008089881, (0 missing)
    ## 
    ## Node number 26: 406508 observations,    complexity param=0.01628706
    ##   mean=1451.327, MSE=276664.5 
    ##   left son=52 (297794 obs) right son=53 (108714 obs)
    ##   Primary splits:
    ##       filing_year    < 2004.5 to the right, improve=0.18969750, (0 missing)
    ##       descision_year < 2010.5 to the left,  improve=0.10885400, (0 missing)
    ##       tc             splits as  LLLR,       improve=0.02306729, (0 missing)
    ##       disposal_type  splits as  LR,         improve=0.01585409, (0 missing)
    ##       tenure_days    < 6315.5 to the right, improve=0.00718641, (0 missing)
    ##   Surrogate splits:
    ##       descision_year < 2007.5 to the right, agree=0.781, adj=0.18, (0 split)
    ## 
    ## Node number 27: 65133 observations,    complexity param=0.01917412
    ##   mean=2906.068, MSE=574275.5 
    ##   left son=54 (35724 obs) right son=55 (29409 obs)
    ##   Primary splits:
    ##       descision_year < 2009.5 to the left,  improve=0.68186920, (0 missing)
    ##       filing_year    < 2001.5 to the right, improve=0.20186260, (0 missing)
    ##       disposal_type  splits as  LR,         improve=0.13604490, (0 missing)
    ##       tc             splits as  RRLR,       improve=0.05502064, (0 missing)
    ##       tenure_days    < 6252.5 to the left,  improve=0.01559727, (0 missing)
    ##   Surrogate splits:
    ##       disposal_type splits as  LR,         agree=0.591, adj=0.094, (0 split)
    ##       filing_year   < 2000.5 to the right, agree=0.587, adj=0.086, (0 split)
    ##       tc            splits as  LRLR,       agree=0.581, adj=0.071, (0 split)
    ##       tenure_days   < 6348.5 to the left,  agree=0.549, adj=0.001, (0 split)
    ## 
    ## Node number 28: 59503 observations,    complexity param=0.00476982
    ##   mean=2393.018, MSE=195448.3 
    ##   left son=56 (37656 obs) right son=57 (21847 obs)
    ##   Primary splits:
    ##       descision_year < 2014.5 to the left,  improve=0.545554400, (0 missing)
    ##       filing_year    < 2007.5 to the right, improve=0.159986200, (0 missing)
    ##       disposal_type  splits as  LR,         improve=0.010426020, (0 missing)
    ##       tenure_days    < 5099.5 to the left,  improve=0.004291697, (0 missing)
    ##       tc             splits as  RLRL,       improve=0.004162374, (0 missing)
    ## 
    ## Node number 29: 30143 observations,    complexity param=0.003127629
    ##   mean=3158.089, MSE=229585.5 
    ##   left son=58 (18594 obs) right son=59 (11549 obs)
    ##   Primary splits:
    ##       descision_year < 2014.5 to the left,  improve=0.60116150, (0 missing)
    ##       filing_year    < 2005.5 to the right, improve=0.20947620, (0 missing)
    ##       tenure_days    < 6228   to the left,  improve=0.02946248, (0 missing)
    ##       disposal_type  splits as  LR,         improve=0.02756404, (0 missing)
    ##       tc             splits as  RRRL,       improve=0.01288053, (0 missing)
    ##   Surrogate splits:
    ##       tenure_days < 6370.5 to the left,  agree=0.617, adj=0, (0 split)
    ## 
    ## Node number 30: 22827 observations,    complexity param=0.001980702
    ##   mean=3994.385, MSE=205891.2 
    ##   left son=60 (11891 obs) right son=61 (10936 obs)
    ##   Primary splits:
    ##       descision_year < 2014.5 to the left,  improve=0.56058230, (0 missing)
    ##       filing_year    < 2003.5 to the right, improve=0.13713270, (0 missing)
    ##       disposal_type  splits as  LR,         improve=0.03575391, (0 missing)
    ##       tc             splits as  RRRL,       improve=0.03058545, (0 missing)
    ##       tenure_days    < 5991   to the left,  improve=0.01943364, (0 missing)
    ##   Surrogate splits:
    ##       tc            splits as  LLRL,       agree=0.543, adj=0.047, (0 split)
    ##       tenure_days   < 6338.5 to the left,  agree=0.530, adj=0.018, (0 split)
    ##       filing_year   < 2003.5 to the left,  agree=0.525, adj=0.009, (0 split)
    ##       disposal_type splits as  LR,         agree=0.522, adj=0.002, (0 split)
    ##       race          splits as  LLRRL,      agree=0.521, adj=0.001, (0 split)
    ## 
    ## Node number 31: 34175 observations,    complexity param=0.002642142
    ##   mean=4973.378, MSE=244101.8 
    ##   left son=62 (15665 obs) right son=63 (18510 obs)
    ##   Primary splits:
    ##       descision_year < 2014.5 to the left,  improve=0.421292300, (0 missing)
    ##       filing_year    < 2001.5 to the right, improve=0.269785600, (0 missing)
    ##       tc             splits as  RRRL,       improve=0.018237310, (0 missing)
    ##       disposal_type  splits as  LR,         improve=0.014639020, (0 missing)
    ##       tenure_days    < 6114.5 to the left,  improve=0.005300257, (0 missing)
    ##   Surrogate splits:
    ##       filing_year   < 2000.5 to the left,  agree=0.575, adj=0.072, (0 split)
    ##       tc            splits as  LRRL,       agree=0.569, adj=0.059, (0 split)
    ##       tenure_days   < 5823   to the left,  agree=0.555, adj=0.030, (0 split)
    ##       disposal_type splits as  LR,         agree=0.548, adj=0.014, (0 split)
    ##       race          splits as  RRRLR,      agree=0.542, adj=0.000, (0 split)
    ## 
    ## Node number 38: 70752 observations,    complexity param=0.001196235
    ##   mean=913.3429, MSE=42259.76 
    ##   left son=76 (36202 obs) right son=77 (34550 obs)
    ##   Primary splits:
    ##       filing_year   < 2012.5 to the right, improve=0.5321791000, (0 missing)
    ##       tenure_days   < 3757.5 to the right, improve=0.0155208200, (0 missing)
    ##       tc            splits as  LRRR,       improve=0.0127580200, (0 missing)
    ##       disposal_type splits as  RL,         improve=0.0045950770, (0 missing)
    ##       race          splits as  RLRRL,      improve=0.0001926617, (0 missing)
    ##   Surrogate splits:
    ##       tenure_days   < 4589.5 to the right, agree=0.547, adj=0.073, (0 split)
    ##       tc            splits as  LRRR,       agree=0.534, adj=0.047, (0 split)
    ##       disposal_type splits as  RL,         agree=0.527, adj=0.032, (0 split)
    ##       race          splits as  LLRLL,      agree=0.513, adj=0.003, (0 split)
    ## 
    ## Node number 39: 57436 observations,    complexity param=0.001080205
    ##   mean=1177.617, MSE=45865.02 
    ##   left son=78 (39015 obs) right son=79 (18421 obs)
    ##   Primary splits:
    ##       filing_year   < 2012.5 to the right, improve=0.5454405000, (0 missing)
    ##       tenure_days   < 5180.5 to the right, improve=0.0057669540, (0 missing)
    ##       disposal_type splits as  RL,         improve=0.0020131920, (0 missing)
    ##       tc            splits as  LRLL,       improve=0.0012181900, (0 missing)
    ##       race          splits as  LLRRR,      improve=0.0007268107, (0 missing)
    ## 
    ## Node number 40: 36876 observations
    ##   mean=612.5608, MSE=53317.51 
    ## 
    ## Node number 41: 48810 observations,    complexity param=0.001296615
    ##   mean=941.242, MSE=60839.27 
    ##   left son=82 (19532 obs) right son=83 (29278 obs)
    ##   Primary splits:
    ##       descision_year < 2011.5 to the left,  improve=0.5807979000, (0 missing)
    ##       tenure_days    < 6150.5 to the right, improve=0.0199732100, (0 missing)
    ##       tc             splits as  LLRR,       improve=0.0150589500, (0 missing)
    ##       disposal_type  splits as  RL,         improve=0.0020863620, (0 missing)
    ##       race           splits as  RLRLL,      improve=0.0007868645, (0 missing)
    ##   Surrogate splits:
    ##       tenure_days < 2159.5 to the left,  agree=0.6, adj=0.001, (0 split)
    ##       race        splits as  RRRLR,      agree=0.6, adj=0.000, (0 split)
    ## 
    ## Node number 42: 115811 observations,    complexity param=0.001507368
    ##   mean=1064.715, MSE=61471.52 
    ##   left son=84 (61367 obs) right son=85 (54444 obs)
    ##   Primary splits:
    ##       filing_year    < 2010.5 to the right, improve=0.2816453000, (0 missing)
    ##       descision_year < 2013.5 to the left,  improve=0.2729112000, (0 missing)
    ##       tenure_days    < 5786.5 to the right, improve=0.0107598200, (0 missing)
    ##       tc             splits as  LRRR,       improve=0.0078040610, (0 missing)
    ##       gender         splits as  LR,         improve=0.0008070502, (0 missing)
    ##   Surrogate splits:
    ##       descision_year < 2013.5 to the right, agree=0.59, adj=0.129, (0 split)
    ##       tenure_days    < 6349.5 to the left,  agree=0.53, adj=0.000, (0 split)
    ## 
    ## Node number 43: 29750 observations
    ##   mean=1560.204, MSE=58800.49 
    ## 
    ## Node number 48: 22830 observations
    ##   mean=588.2847, MSE=36489.04 
    ## 
    ## Node number 49: 61179 observations,    complexity param=0.001180343
    ##   mean=878.9991, MSE=83616.82 
    ##   left son=98 (46839 obs) right son=99 (14340 obs)
    ##   Primary splits:
    ##       filing_year    < 2000.5 to the right, improve=0.339156200, (0 missing)
    ##       tc             splits as  LLRL,       improve=0.116177800, (0 missing)
    ##       descision_year < 2003.5 to the left,  improve=0.063270350, (0 missing)
    ##       race           splits as  RLRRL,      improve=0.011526900, (0 missing)
    ##       tenure_days    < 6336.5 to the right, improve=0.003614993, (0 missing)
    ## 
    ## Node number 50: 70672 observations,    complexity param=0.001911216
    ##   mean=1021.306, MSE=94564.11 
    ##   left son=100 (42281 obs) right son=101 (28391 obs)
    ##   Primary splits:
    ##       filing_year    < 2002.5 to the right, improve=0.380402200, (0 missing)
    ##       descision_year < 2005.5 to the left,  improve=0.076075340, (0 missing)
    ##       tc             splits as  LLRR,       improve=0.045839170, (0 missing)
    ##       tenure_days    < 6256   to the right, improve=0.012189420, (0 missing)
    ##       race           splits as  RRLRL,      improve=0.003781309, (0 missing)
    ##   Surrogate splits:
    ##       descision_year < 2005.5 to the right, agree=0.629, adj=0.077, (0 split)
    ##       tc             splits as  LLRL,       agree=0.618, adj=0.049, (0 split)
    ##       tenure_days    < 1817   to the right, agree=0.598, adj=0.000, (0 split)
    ## 
    ## Node number 51: 25594 observations
    ##   mean=1734.415, MSE=90981.1 
    ## 
    ## Node number 52: 297794 observations,    complexity param=0.008921354
    ##   mean=1312.909, MSE=185338.7 
    ##   left son=104 (180404 obs) right son=105 (117390 obs)
    ##   Primary splits:
    ##       descision_year < 2010.5 to the left,  improve=0.215008500, (0 missing)
    ##       filing_year    < 2006.5 to the right, improve=0.087132020, (0 missing)
    ##       tc             splits as  LLLR,       improve=0.032134220, (0 missing)
    ##       tenure_days    < 6231   to the right, improve=0.026768410, (0 missing)
    ##       disposal_type  splits as  LR,         improve=0.008047171, (0 missing)
    ##   Surrogate splits:
    ##       filing_year < 2007.5 to the left,  agree=0.713, adj=0.273, (0 split)
    ##       tc          splits as  LLLR,       agree=0.622, adj=0.042, (0 split)
    ##       tenure_days < 4404.5 to the right, agree=0.615, adj=0.024, (0 split)
    ##       race        splits as  LLLRL,      agree=0.606, adj=0.000, (0 split)
    ## 
    ## Node number 53: 108714 observations,    complexity param=0.01628706
    ##   mean=1830.487, MSE=330583.1 
    ##   left son=106 (78215 obs) right son=107 (30499 obs)
    ##   Primary splits:
    ##       descision_year < 2009.5 to the left,  improve=0.611995300, (0 missing)
    ##       filing_year    < 2003.5 to the right, improve=0.067098590, (0 missing)
    ##       tc             splits as  LLLR,       improve=0.056380720, (0 missing)
    ##       disposal_type  splits as  LR,         improve=0.048267160, (0 missing)
    ##       tenure_days    < 6244.5 to the right, improve=0.008233179, (0 missing)
    ##   Surrogate splits:
    ##       tc splits as  LLLR, agree=0.722, adj=0.01, (0 split)
    ## 
    ## Node number 54: 35724 observations,    complexity param=0.002248247
    ##   mean=2338.3, MSE=206747.5 
    ##   left son=108 (26425 obs) right son=109 (9299 obs)
    ##   Primary splits:
    ##       descision_year < 2008.5 to the left,  improve=0.404902800, (0 missing)
    ##       filing_year    < 2001.5 to the right, improve=0.314819400, (0 missing)
    ##       disposal_type  splits as  LR,         improve=0.054929460, (0 missing)
    ##       tc             splits as  LLLR,       improve=0.038049450, (0 missing)
    ##       tenure_days    < 6052   to the left,  improve=0.008622444, (0 missing)
    ##   Surrogate splits:
    ##       tc splits as  LLLR, agree=0.785, adj=0.172, (0 split)
    ## 
    ## Node number 55: 29409 observations,    complexity param=0.001029465
    ##   mean=3595.752, MSE=153477.1 
    ##   left son=110 (10075 obs) right son=111 (19334 obs)
    ##   Primary splits:
    ##       filing_year    < 2001.5 to the right, improve=0.30338540, (0 missing)
    ##       descision_year < 2010.5 to the left,  improve=0.29084190, (0 missing)
    ##       disposal_type  splits as  LR,         improve=0.05607427, (0 missing)
    ##       tc             splits as  RRRL,       improve=0.03896540, (0 missing)
    ##       tenure_days    < 6194.5 to the left,  improve=0.03281145, (0 missing)
    ##   Surrogate splits:
    ##       disposal_type splits as  LR,         agree=0.672, adj=0.043, (0 split)
    ##       tc            splits as  RRRL,       agree=0.666, adj=0.026, (0 split)
    ##       tenure_days   < 5751   to the left,  agree=0.659, adj=0.003, (0 split)
    ## 
    ## Node number 56: 37656 observations,    complexity param=0.001068144
    ##   mean=2144.297, MSE=92493.67 
    ##   left son=112 (19176 obs) right son=113 (18480 obs)
    ##   Primary splits:
    ##       descision_year < 2013.5 to the left,  improve=0.407934500, (0 missing)
    ##       filing_year    < 2007.5 to the right, improve=0.390531600, (0 missing)
    ##       tenure_days    < 3987.5 to the left,  improve=0.005457572, (0 missing)
    ##       tc             splits as  RLLL,       improve=0.005154546, (0 missing)
    ##       disposal_type  splits as  LR,         improve=0.001708401, (0 missing)
    ##   Surrogate splits:
    ##       tc          splits as  RLLL,       agree=0.535, adj=0.052, (0 split)
    ##       filing_year < 2007.5 to the right, agree=0.524, adj=0.031, (0 split)
    ##       tenure_days < 5005.5 to the left,  agree=0.513, adj=0.008, (0 split)
    ##       race        splits as  LLRLL,      agree=0.510, adj=0.001, (0 split)
    ## 
    ## Node number 57: 21847 observations
    ##   mean=2821.721, MSE=82489.73 
    ## 
    ## Node number 58: 18594 observations
    ##   mean=2865.301, MSE=83254.53 
    ## 
    ## Node number 59: 11549 observations
    ##   mean=3629.481, MSE=104951.6 
    ## 
    ## Node number 60: 11891 observations
    ##   mean=3668.579, MSE=87550.9 
    ## 
    ## Node number 61: 10936 observations
    ##   mean=4348.642, MSE=93648.67 
    ## 
    ## Node number 62: 15665 observations,    complexity param=0.001301243
    ##   mean=4624.788, MSE=188503.5 
    ##   left son=124 (5217 obs) right son=125 (10448 obs)
    ##   Primary splits:
    ##       filing_year    < 2001.5 to the right, improve=0.586159000, (0 missing)
    ##       descision_year < 2013.5 to the left,  improve=0.323125900, (0 missing)
    ##       disposal_type  splits as  LR,         improve=0.016388890, (0 missing)
    ##       tc             splits as  RRRL,       improve=0.015946930, (0 missing)
    ##       tenure_days    < 5590.5 to the right, improve=0.003424889, (0 missing)
    ##   Surrogate splits:
    ##       disposal_type splits as  LR,         agree=0.675, adj=0.023, (0 split)
    ##       tc            splits as  RRRL,       agree=0.671, adj=0.013, (0 split)
    ##       tenure_days   < 4845.5 to the left,  agree=0.668, adj=0.003, (0 split)
    ##       race          splits as  RRRLR,      agree=0.667, adj=0.000, (0 split)
    ## 
    ## Node number 63: 18510 observations
    ##   mean=5268.39, MSE=101284.5 
    ## 
    ## Node number 76: 36202 observations
    ##   mean=766.8387, MSE=19524.66 
    ## 
    ## Node number 77: 34550 observations
    ##   mean=1066.852, MSE=20027.07 
    ## 
    ## Node number 78: 39015 observations
    ##   mean=1068.935, MSE=20268.83 
    ## 
    ## Node number 79: 18421 observations
    ##   mean=1407.8, MSE=22075.85 
    ## 
    ## Node number 82: 19532 observations
    ##   mean=711.0969, MSE=37914.63 
    ## 
    ## Node number 83: 29278 observations
    ##   mean=1094.777, MSE=17224.52 
    ## 
    ## Node number 84: 61367 observations,    complexity param=0.001045054
    ##   mean=940.779, MSE=42360.76 
    ##   left son=168 (27377 obs) right son=169 (33990 obs)
    ##   Primary splits:
    ##       descision_year < 2013.5 to the left,  improve=0.5347452000, (0 missing)
    ##       tenure_days    < 4136.5 to the right, improve=0.0159443800, (0 missing)
    ##       tc             splits as  LRRR,       improve=0.0143316900, (0 missing)
    ##       gender         splits as  LR,         improve=0.0010938210, (0 missing)
    ##       race           splits as  LLRRL,      improve=0.0005025661, (0 missing)
    ##   Surrogate splits:
    ##       tc          splits as  LRRR,       agree=0.573, adj=0.043, (0 split)
    ##       tenure_days < 6253.5 to the right, agree=0.560, adj=0.015, (0 split)
    ## 
    ## Node number 85: 54444 observations,    complexity param=0.001098802
    ##   mean=1204.409, MSE=46184.54 
    ##   left son=170 (34387 obs) right son=171 (20057 obs)
    ##   Primary splits:
    ##       descision_year < 2013.5 to the left,  improve=0.5812724000, (0 missing)
    ##       tenure_days    < 5770.5 to the right, improve=0.0108625200, (0 missing)
    ##       tc             splits as  LRRR,       improve=0.0036260320, (0 missing)
    ##       disposal_type  splits as  LR,         improve=0.0034221090, (0 missing)
    ##       gender         splits as  LR,         improve=0.0004153284, (0 missing)
    ## 
    ## Node number 98: 46839 observations
    ##   mean=785.8202, MSE=56800.43 
    ## 
    ## Node number 99: 14340 observations
    ##   mean=1183.351, MSE=50218.48 
    ## 
    ## Node number 100: 42281 observations
    ##   mean=865.8876, MSE=65309.15 
    ## 
    ## Node number 101: 28391 observations
    ##   mean=1252.761, MSE=48587.85 
    ## 
    ## Node number 104: 180404 observations,    complexity param=0.00453021
    ##   mean=1151.88, MSE=131763.1 
    ##   left son=208 (58524 obs) right son=209 (121880 obs)
    ##   Primary splits:
    ##       filing_year    < 2006.5 to the right, improve=0.227960100, (0 missing)
    ##       descision_year < 2008.5 to the left,  improve=0.154029500, (0 missing)
    ##       tenure_days    < 6231   to the right, improve=0.028158110, (0 missing)
    ##       tc             splits as  LLLR,       improve=0.025590160, (0 missing)
    ##       race           splits as  RRLRL,      improve=0.002629824, (0 missing)
    ##   Surrogate splits:
    ##       descision_year < 2009.5 to the right, agree=0.677, adj=0.005, (0 split)
    ## 
    ## Node number 105: 117390 observations,    complexity param=0.008183935
    ##   mean=1560.376, MSE=166583.5 
    ##   left son=210 (85236 obs) right son=211 (32154 obs)
    ##   Primary splits:
    ##       filing_year    < 2006.5 to the right, improve=0.556680300, (0 missing)
    ##       descision_year < 2011.5 to the left,  improve=0.114487800, (0 missing)
    ##       disposal_type  splits as  LR,         improve=0.011819460, (0 missing)
    ##       tc             splits as  LLLR,       improve=0.010719150, (0 missing)
    ##       tenure_days    < 6296.5 to the right, improve=0.004774847, (0 missing)
    ## 
    ## Node number 106: 78215 observations,    complexity param=0.003381739
    ##   mean=1549.613, MSE=122854.2 
    ##   left son=212 (55931 obs) right son=213 (22284 obs)
    ##   Primary splits:
    ##       descision_year < 2008.5 to the left,  improve=0.468130600, (0 missing)
    ##       filing_year    < 2003.5 to the right, improve=0.155343100, (0 missing)
    ##       tc             splits as  LLLR,       improve=0.086628690, (0 missing)
    ##       tenure_days    < 6244.5 to the right, improve=0.014732340, (0 missing)
    ##       race           splits as  RRRRL,      improve=0.003594723, (0 missing)
    ##   Surrogate splits:
    ##       tc splits as  LLLR, agree=0.772, adj=0.2, (0 split)
    ## 
    ## Node number 107: 30499 observations,    complexity param=0.001594757
    ##   mean=2550.792, MSE=142151.1 
    ##   left son=214 (15744 obs) right son=215 (14755 obs)
    ##   Primary splits:
    ##       descision_year < 2010.5 to the left,  improve=0.489289200, (0 missing)
    ##       filing_year    < 2003.5 to the right, improve=0.258237900, (0 missing)
    ##       disposal_type  splits as  LR,         improve=0.022890010, (0 missing)
    ##       tenure_days    < 5334.5 to the left,  improve=0.018212530, (0 missing)
    ##       tc             splits as  RRLL,       improve=0.004646542, (0 missing)
    ##   Surrogate splits:
    ##       disposal_type splits as  LR,         agree=0.537, adj=0.044, (0 split)
    ##       tenure_days   < 6343.5 to the left,  agree=0.523, adj=0.014, (0 split)
    ##       tc            splits as  RRLL,       agree=0.519, adj=0.007, (0 split)
    ##       filing_year   < 2003.5 to the right, agree=0.518, adj=0.003, (0 split)
    ## 
    ## Node number 108: 26425 observations,    complexity param=0.001080037
    ##   mean=2166.665, MSE=120063.4 
    ##   left son=216 (13733 obs) right son=217 (12692 obs)
    ##   Primary splits:
    ##       filing_year    < 2001.5 to the right, improve=0.45281410, (0 missing)
    ##       descision_year < 2007.5 to the left,  improve=0.23331120, (0 missing)
    ##       disposal_type  splits as  LR,         improve=0.06047228, (0 missing)
    ##       tenure_days    < 6114.5 to the left,  improve=0.02311931, (0 missing)
    ##       tc             splits as  LLLR,       improve=0.00612111, (0 missing)
    ##   Surrogate splits:
    ##       disposal_type splits as  LR,         agree=0.587, adj=0.140, (0 split)
    ##       tenure_days   < 6231   to the left,  agree=0.548, adj=0.059, (0 split)
    ##       tc            splits as  LRLL,       agree=0.527, adj=0.016, (0 split)
    ##       race          splits as  LLRLL,      agree=0.520, adj=0.001, (0 split)
    ## 
    ## Node number 109: 9299 observations
    ##   mean=2826.036, MSE=131478.9 
    ## 
    ## Node number 110: 10075 observations
    ##   mean=3296.831, MSE=130357.2 
    ## 
    ## Node number 111: 19334 observations
    ##   mean=3751.521, MSE=94698.26 
    ## 
    ## Node number 112: 19176 observations
    ##   mean=1953.609, MSE=56103.2 
    ## 
    ## Node number 113: 18480 observations
    ##   mean=2342.166, MSE=53370.92 
    ## 
    ## Node number 124: 5217 observations
    ##   mean=4154.381, MSE=55374 
    ## 
    ## Node number 125: 10448 observations
    ##   mean=4859.676, MSE=89313.57 
    ## 
    ## Node number 168: 27377 observations
    ##   mean=773.0771, MSE=20006.9 
    ## 
    ## Node number 169: 33990 observations
    ##   mean=1075.853, MSE=19468.24 
    ## 
    ## Node number 170: 34387 observations
    ##   mean=1079.276, MSE=18089.33 
    ## 
    ## Node number 171: 20057 observations
    ##   mean=1418.947, MSE=21480.81 
    ## 
    ## Node number 208: 58524 observations
    ##   mean=901.7733, MSE=64203.02 
    ## 
    ## Node number 209: 121880 observations,    complexity param=0.00453021
    ##   mean=1271.976, MSE=119744.2 
    ##   left son=418 (35540 obs) right son=419 (86340 obs)
    ##   Primary splits:
    ##       descision_year < 2008.5 to the left,  improve=0.454498600, (0 missing)
    ##       filing_year    < 2005.5 to the right, improve=0.072244800, (0 missing)
    ##       tc             splits as  LLLR,       improve=0.047474780, (0 missing)
    ##       tenure_days    < 6231   to the right, improve=0.039679500, (0 missing)
    ##       disposal_type  splits as  LR,         improve=0.002986323, (0 missing)
    ##   Surrogate splits:
    ##       tenure_days < 1251.5 to the left,  agree=0.709, adj=0.002, (0 split)
    ## 
    ## Node number 210: 85236 observations,    complexity param=0.001334036
    ##   mean=1373.341, MSE=67031.32 
    ##   left son=420 (49691 obs) right son=421 (35545 obs)
    ##   Primary splits:
    ##       filing_year    < 2007.5 to the right, improve=0.310580100, (0 missing)
    ##       descision_year < 2011.5 to the left,  improve=0.297870000, (0 missing)
    ##       tc             splits as  LLRR,       improve=0.017782850, (0 missing)
    ##       tenure_days    < 6295.5 to the right, improve=0.008488275, (0 missing)
    ##       disposal_type  splits as  LR,         improve=0.005250454, (0 missing)
    ##   Surrogate splits:
    ##       race splits as  LLLRL, agree=0.583, adj=0, (0 split)
    ## 
    ## Node number 211: 32154 observations
    ##   mean=2056.184, MSE=91924.56 
    ## 
    ## Node number 212: 55931 observations
    ##   mean=1398.239, MSE=70705.39 
    ## 
    ## Node number 213: 22284 observations
    ##   mean=1929.547, MSE=51881.64 
    ## 
    ## Node number 214: 15744 observations
    ##   mean=2295.481, MSE=53565.02 
    ## 
    ## Node number 215: 14755 observations
    ##   mean=2823.217, MSE=92906.9 
    ## 
    ## Node number 216: 13733 observations
    ##   mean=1942.51, MSE=57023 
    ## 
    ## Node number 217: 12692 observations
    ##   mean=2409.205, MSE=75082.41 
    ## 
    ## Node number 418: 35540 observations
    ##   mean=908.3615, MSE=60434.69 
    ## 
    ## Node number 419: 86340 observations,    complexity param=0.001463639
    ##   mean=1421.65, MSE=67331.83 
    ##   left son=838 (47510 obs) right son=839 (38830 obs)
    ##   Primary splits:
    ##       filing_year    < 2005.5 to the right, improve=0.334894800, (0 missing)
    ##       descision_year < 2009.5 to the left,  improve=0.284032200, (0 missing)
    ##       tc             splits as  LLLR,       improve=0.014148340, (0 missing)
    ##       disposal_type  splits as  LR,         improve=0.012795110, (0 missing)
    ##       tenure_days    < 6314.5 to the right, improve=0.008891304, (0 missing)
    ##   Surrogate splits:
    ##       tc             splits as  LLLR,       agree=0.561, adj=0.025, (0 split)
    ##       descision_year < 2009.5 to the right, agree=0.554, adj=0.009, (0 split)
    ##       race           splits as  LLLRL,      agree=0.550, adj=0.000, (0 split)
    ##       tenure_days    < 6454.5 to the left,  agree=0.550, adj=0.000, (0 split)
    ## 
    ## Node number 420: 49691 observations
    ##   mean=1251.308, MSE=43836.5 
    ## 
    ## Node number 421: 35545 observations
    ##   mean=1543.939, MSE=49534.64 
    ## 
    ## Node number 838: 47510 observations
    ##   mean=1285.895, MSE=43166.64 
    ## 
    ## Node number 839: 38830 observations
    ##   mean=1587.751, MSE=46760.12

``` r
# # Personal level data from correction
# person_level_data= read.csv('person_level_data.csv')
# # Data of art unit changes
# aus=read.csv("examiner_aus.csv")
# # Get id crosswalk table
# ids=read.csv("examiner_ids.csv")
# Get parquet data that details the transactions for each examiner, their gender and most likely ethnicity


# Code not needed
# #get the quarter number
# App_data$Quarter_Year=as.character(yearquarter(App_data$Date_time))
# #get the week number
# App_data$Week_Year=as.character(yearweek(App_data$Date_time))
```
