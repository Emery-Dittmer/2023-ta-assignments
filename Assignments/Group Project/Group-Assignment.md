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


# Remove Nas from race
App_data <- App_data %>% 
  filter(!is.na(race))
  
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

keep=c("filing_date","disposal_type","tc","gender","race","tenure_days","Date_time","examiner_id") #examiner_art_unit not kept as produces too man variables for packages
T_Data = subset(T_Data, select = keep)
```

Pen-ultimately we will change the data type on a few columns for
analysis ease

``` r
#Setting Gender as factor
T_Data$gender = as.factor(T_Data$gender)

#Setting ethnicity as factor
T_Data$race = as.factor(T_Data$race)

#Setting ethnicity as factor
T_Data$disposal_type = as.factor(T_Data$disposal_type)

#setting the technology center as a factor
T_Data$tc = as.factor(T_Data$tc)

# #Art unit as a factor in case
# T_Data$examiner_art_unit = as.factor(T_Data$examiner_art_unit)

T_Data_ggally=T_Data[,1:7]
T_Data_ggally=T_Data[,c(1:5,7)]
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

#we will assume that tenure days is roughly equal to the tenure days since the last patent descision
Temp_data <- T_Data %>% 
  group_by(examiner_id) %>% 
  summarise(
    start_data = min(Date_time)
    )

T_Data <- merge(T_Data, Temp_data, by = 'examiner_id',all=T)
T_Data$Approx_Tenue_Days=as.numeric(T_Data$Date_time-T_Data$start_data)

#get median
med_app_time=median(T_Data$Application_time)

#T_data=T_Data[,2:12]
```

garbage collection (optional)

``` r
rm(App_data,exclude_list,keep,Temp_data)
```

# Descriptive Analysis

## Correlation Data Investigation

Now that the data is in a clean and useable format let’s examine the
data more closely. First lets look at the summary stats for all the data

``` r
# 
library(vtable)
```

    ## Loading required package: kableExtra

    ## 
    ## Attaching package: 'kableExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     group_rows

``` r
# print(sumtable(T_Data))
Temp_data <- T_Data %>% 
  group_by(gender,race) %>% 
  summarise(
    count = n()
    )
```

    ## `summarise()` has grouped output by 'gender'. You can override using the
    ## `.groups` argument.

``` r
Temp_data
```

    ## # A tibble: 9 × 3
    ## # Groups:   gender [2]
    ##   gender race      count
    ##   <fct>  <fct>     <int>
    ## 1 female Asian    112599
    ## 2 female black     22450
    ## 3 female Hispanic  12772
    ## 4 female white    305044
    ## 5 male   Asian    212813
    ## 6 male   black     25423
    ## 7 male   Hispanic  25993
    ## 8 male   other       993
    ## 9 male   white    646892

``` r
summary(T_Data)
```

    ##   examiner_id     filing_date         disposal_type    tc        
    ##  Min.   :59012   Min.   :2000-01-02   ABN:498337    1600:368721  
    ##  1st Qu.:66582   1st Qu.:2004-04-02   ISS:866642    1700:529276  
    ##  Median :75345   Median :2007-11-13                 2100:270837  
    ##  Mean   :78847   Mean   :2007-10-08                 2400:196145  
    ##  3rd Qu.:93839   3rd Qu.:2011-05-04                              
    ##  Max.   :99988   Max.   :2016-11-14                              
    ##     gender             race         tenure_days     Date_time         
    ##  female:452865   Asian   :325412   Min.   : 216   Min.   :2000-05-24  
    ##  male  :912114   black   : 47873   1st Qu.:5180   1st Qu.:2009-06-29  
    ##                  Hispanic: 38765   Median :6209   Median :2012-07-13  
    ##                  other   :   993   Mean   :5670   Mean   :2011-11-05  
    ##                  white   :951936   3rd Qu.:6338   3rd Qu.:2014-12-10  
    ##                                    Max.   :6518   Max.   :2017-01-01  
    ##  Application_time  filing_year   descision_year   start_data        
    ##  Min.   :  11     Min.   :2000   Min.   :2000   Min.   :2000-05-24  
    ##  1st Qu.: 858     1st Qu.:2004   1st Qu.:2009   1st Qu.:2001-07-19  
    ##  Median :1213     Median :2007   Median :2012   Median :2004-09-29  
    ##  Mean   :1489     Mean   :2007   Mean   :2011   Mean   :2005-04-04  
    ##  3rd Qu.:1786     3rd Qu.:2011   3rd Qu.:2014   3rd Qu.:2008-08-13  
    ##  Max.   :6187     Max.   :2016   Max.   :2017   Max.   :2016-11-29  
    ##  Approx_Tenue_Days
    ##  Min.   :   0     
    ##  1st Qu.:1186     
    ##  Median :2216     
    ##  Mean   :2405     
    ##  3rd Qu.:3473     
    ##  Max.   :6064

Now lets look at the correlation plots

``` r
library(GGally)
```

    ## Registered S3 method overwritten by 'GGally':
    ##   method from   
    ##   +.gg   ggplot2

``` r
d=ggpairs(T_Data_ggally)
d
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Group-Assignment_files/figure-gfm/g%20gally-1.png)<!-- -->

``` r
rm(T_Data_ggally,Temp_data)
```

``` r
require(corrplot)
```

    ## Loading required package: corrplot

    ## corrplot 0.92 loaded

``` r
num_cols <- unlist(lapply(T_Data, is.numeric))       
quanvars <- T_Data[ , num_cols] 
drop <- c("tenure_days","examiner_id")
quanvars = quanvars[,!(names(quanvars) %in% drop)]
corr_matrix <- cor(quanvars)
corrplot(corr_matrix)
```

![](Group-Assignment_files/figure-gfm/pair%20plots-1.png)<!-- --> \##
Distribution of Data

``` r
hists=ggplot(T_Data, aes(x=Application_time))+geom_histogram(bins = 30)+
  geom_vline(aes(xintercept = med_app_time), color = "red")+
    ggtitle("Histogram of Application Length in Days. Median in Red")
hists
```

![](Group-Assignment_files/figure-gfm/histogram%20distribtion-1.png)<!-- -->

lets examine the distribtuon of the number of aplications by the number
of days it usually takes for each tc to complete

``` r
hists=ggplot(T_Data, aes(x=Application_time))+geom_histogram(bins = 30)+
  facet_grid(T_Data$tc)+
  geom_vline(aes(xintercept = med_app_time), color = "red")+
  ggtitle("Histogram of Application Length in Days Brokenout by TC. Median in Red")
hists
```

![](Group-Assignment_files/figure-gfm/histogram%20distribtion%20ethnicity-1.png)<!-- -->
Lets look at it by gender and tc now

``` r
hists=ggplot(T_Data, aes(x=Application_time))+geom_histogram(bins = 30)+
  facet_grid(T_Data$tc~T_Data$gender)+
  geom_vline(aes(xintercept = med_app_time), color = "red")+
  ggtitle("Histogram of Application Length in Days Brokenout by TC. Median in Red")
hists
```

![](Group-Assignment_files/figure-gfm/histogram%20distribtion%20ethnicity%201-1.png)<!-- -->

Now let’s take a look at the spead of application years to see if there
are more applications in one year than another

``` r
hists=ggplot(T_Data, aes(x=filing_year))+
  geom_histogram(bins = 30)+
  facet_grid(T_Data$tc)+
  ggtitle("Histogram of Application Length in Days.")
hists
```

![](Group-Assignment_files/figure-gfm/histogram%20distribtion%20ethnicity%202-1.png)<!-- -->

Similarly let’s look at the distribution of approval years

``` r
hists=ggplot(T_Data, aes(x=descision_year))+geom_histogram(bins = 30)+facet_grid(T_Data$tc)
hists
```

![](Group-Assignment_files/figure-gfm/histogram%20distribtion%20ethnicity%203-1.png)<!-- -->
So is the time to process documents increasing, decreasing or
potentially neither?

``` r
ggplot()+
  geom_point(data=T_Data, aes(filing_date,Application_time), size = 2,alpha=.005)+
  facet_grid(T_Data$tc~T_Data$disposal_type)
```

![](Group-Assignment_files/figure-gfm/histogram%20distribtion%20ethnicity%204-1.png)<!-- -->

``` r
#T_Data$tc,
```

``` r
ggplot()+
  geom_histogram(data=T_Data, aes(Application_time))+
  facet_grid(T_Data$gender~T_Data$race)+
  xlab("Length of Application")+
  ylab("Count of Application Length")+
  ggtitle("Histogram of Application Length in Days Brokenout by TC. Median in Red")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Group-Assignment_files/figure-gfm/histogram%20distribtion%20gender%20ethnicity%205-1.png)<!-- -->

``` r
#T_Data$tc,
```

Now lets look at this as a percentage of the totals. Looking at the
counts is not very meaningful

``` r
ggplot()+
  geom_density(data=T_Data, aes(Application_time))+
  facet_grid(T_Data$gender~T_Data$race)
```

![](Group-Assignment_files/figure-gfm/density%20plotting-1.png)<!-- -->

``` r
#T_Data$tc,
```

Let’s compare side by side

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ tibble  3.1.8     ✔ purrr   0.3.4
    ## ✔ tidyr   1.2.1     ✔ forcats 0.5.2
    ## ✔ readr   2.1.3     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ lubridate::as.difftime() masks base::as.difftime()
    ## ✖ lubridate::date()        masks base::date()
    ## ✖ lubridate::duration()    masks arrow::duration()
    ## ✖ dplyr::filter()          masks stats::filter()
    ## ✖ kableExtra::group_rows() masks dplyr::group_rows()
    ## ✖ lubridate::intersect()   masks base::intersect()
    ## ✖ dplyr::lag()             masks stats::lag()
    ## ✖ lubridate::setdiff()     masks base::setdiff()
    ## ✖ lubridate::union()       masks base::union()

``` r
T_Data$race_gender=paste(T_Data$race,T_Data$gender)
T_Data$race_gender=as.factor(T_Data$race_gender)

T_Data %>% 
  mutate(factors = fct_reorder(race_gender,Application_time )) %>% 
  ggplot(aes(factors, Application_time)) +
  geom_hline(aes(yintercept = med_app_time), color = "red") +
  geom_violin() +
  theme(axis.text.x = element_text(angle = -45, hjust = 0))
```

![](Group-Assignment_files/figure-gfm/Violin%20plots-1.png)<!-- -->

Based on the graphs we can see a similar pattern for application time,
where all tcs have a steadliy decreasing application wait time. However,
this is likely a right censoring problem as the applications that are
taking longer are not being counted and therefore are filtered out.
These applications do not have a status of issued or no. It is
interesting to see there is no real discernable pattern between any tcs
on the amount of time ofr a patent applcation.

``` r
drop <- c("race_gender")
T_Data = T_Data[,!(names(T_Data) %in% drop)]
```

## PCA Analaysis

``` r
library(ggfortify)
pca=prcomp(quanvars, scale=TRUE)
autoplot(pca, data = quanvars, loadings = TRUE, loadings.label = TRUE )
```

![](Group-Assignment_files/figure-gfm/pca%20analysis-1.png)<!-- --> \#
Preictive Analysis

We will now attempt to use various models to predict the application
time based on the features of the data

## Prophet Prediction on the future

Let’s use the prophet package to predict the future of the filing number
of applications

``` r
#install.packages('prophet')
library(prophet)
```

    ## Loading required package: Rcpp

    ## Loading required package: rlang

    ## 
    ## Attaching package: 'rlang'

    ## The following objects are masked from 'package:purrr':
    ## 
    ##     %@%, as_function, flatten, flatten_chr, flatten_dbl, flatten_int,
    ##     flatten_lgl, flatten_raw, invoke, splice

    ## The following object is masked from 'package:arrow':
    ## 
    ##     string

``` r
Pred_Data <- T_Data %>% 
  group_by(Date_time) %>% 
  summarise(
    ds = as.POSIXct(min(Date_time)), # I have repeatedly attempted to change this assumption but have been unable to
    y=n(),
    )

m<-prophet(Pred_Data)
future <- make_future_dataframe(m, periods = 365)
tail(future)
```

    ##                       ds
    ## 5821 2017-12-26 19:00:00
    ## 5822 2017-12-27 19:00:00
    ## 5823 2017-12-28 19:00:00
    ## 5824 2017-12-29 19:00:00
    ## 5825 2017-12-30 19:00:00
    ## 5826 2017-12-31 19:00:00

Now we can make some predictions

``` r
forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
```

    ##                       ds      yhat yhat_lower yhat_upper
    ## 5821 2017-12-26 19:00:00 1138.8410  791.65822  1493.4931
    ## 5822 2017-12-27 19:00:00  445.2975   71.81251   790.8908
    ## 5823 2017-12-28 19:00:00  457.0060  117.46159   815.5938
    ## 5824 2017-12-29 19:00:00  276.0807  -57.33098   628.1289
    ## 5825 2017-12-30 19:00:00  226.9363 -115.43376   551.1176
    ## 5826 2017-12-31 19:00:00  534.7482  171.74237   899.0063

``` r
plot(m, forecast)
```

![](Group-Assignment_files/figure-gfm/prophet%20prediction%203-1.png)<!-- -->
We can look at the key feautres effecting the model

``` r
prophet_plot_components(m, forecast)
```

![](Group-Assignment_files/figure-gfm/prophet%20prediction%204-1.png)<!-- -->
and then visualize the predictions!

``` r
# Plot the prophet forecast with the test data points
ggplot(forecast, aes(ds, yhat)) +
  geom_ribbon(data = forecast,aes(ymin=yhat_lower,ymax=yhat_upper), alpha=0.1, color = "grey71")+
  geom_point(data = Pred_Data, aes(ds, y, color = "train data"), size = 2,alpha=0.2) +
  geom_line(aes(color = "prophet forecast",alpha=0.001)) +
  scale_color_manual(values = c("blue", "red", "black"), labels = c("Forecast", "Test data"))+
  xlab("Date") +
  ylab("y") +
  ggtitle("Forecast of number of descisions")
```

![](Group-Assignment_files/figure-gfm/prophet%20prediction%20plot-1.png)<!-- -->

## Tree model for predictive

``` r
library(tree)
library(rpart)
library(rpart.plot)


myoverfittedtree=rpart(Application_time~disposal_type+tc+gender+race+tenure_days+descision_year+filing_year,data = T_Data, control=rpart.control(cp=0.0001))
#this will generate a plot of the decision tree
rpart.plot(myoverfittedtree)
```

    ## Warning: labs do not fit even at cex 0.15, there may be some overplotting

![](Group-Assignment_files/figure-gfm/tree%20model-1.png)<!-- -->

Let’s fit the tree using the bast control paramter

``` r
plotcp(myoverfittedtree)
```

![](Group-Assignment_files/figure-gfm/plot%20the%20cp%20control%20paramter-1.png)<!-- -->

``` r
#This returns the optimal cp value
opt_cp=myoverfittedtree$cptable[which.min(myoverfittedtree$cptable[,"xerror"]),"CP"]
```

We can plot an optimal tree based on the lowest cv, However this becomes
in comprehnisble. Therefore we will switch to the elbow method.

``` r
opt_cp=0.01
optimal_tree=rpart(Application_time~disposal_type+tc+gender+race+tenure_days+descision_year+filing_year,data = T_Data,control=rpart.control(cp=opt_cp))

rpart.plot(optimal_tree)
```

![](Group-Assignment_files/figure-gfm/tree%202-1.png)<!-- -->

Lets look specifically at the people traits like gender and ethnicity.
We will use

``` r
opt_cp=0.00001
optimal_tree=rpart(Application_time~gender+race,data = T_Data,control=rpart.control(cp=opt_cp))

rpart.plot(optimal_tree)
```

![](Group-Assignment_files/figure-gfm/tree%203-1.png)<!-- -->

While his may look meaningful, the cp is set extremely high (0.00001).
Therefore the decision tree is extremely sensitive. A random forest
model might contain more meaningful data but at this level the ethnicity
and gender effects on application times are minimal.

## Survival finctions

Survival data are time-to-event data that consist of a distinct start
time and end time. These might be helpful.

``` r
library(survival)
library(lubridate)
library(ggsurvfit)
library(gtsummary)
library(tidycmprsk)
```

    ## 
    ## Attaching package: 'tidycmprsk'

    ## The following object is masked from 'package:gtsummary':
    ## 
    ##     trial

``` r
#library(condsurv)

T_Data <- T_Data %>% 
  mutate(
    status = recode(disposal_type, `ABN` = 0, `ISS` = 1)
  )

survfit(Surv(Application_time) ~ 1, data = T_Data) %>% 
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  ) + 
  add_confidence_interval()+
  add_risktable()
```

![](Group-Assignment_files/figure-gfm/survival%201-1.png)<!-- -->

Looking at the gender effect

``` r
survfit(Surv(Application_time) ~ gender, data = T_Data) %>% 
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  ) + 
  add_confidence_interval()
```

![](Group-Assignment_files/figure-gfm/survival%202-1.png)<!-- -->

looking at the ethnicity effects

``` r
survfit(Surv(Application_time) ~ race, data = T_Data) %>% 
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  ) + 
  add_confidence_interval()
```

![](Group-Assignment_files/figure-gfm/survival%203-1.png)<!-- -->

``` r
survfit(Surv(Application_time, status) ~ gender+race, data = T_Data) %>% 
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  ) + 
  add_confidence_interval()
```

![](Group-Assignment_files/figure-gfm/survival%204-1.png)<!-- -->

## Mixture models

## Linear Discrimanat Analysis

We should be able to look at how a specific factor like gender affects
the application rate. This method is based on prior probabilities and
Bayesian statistics. Since we have some indication that the data is
normally distributed we can use this method to estimate the effect of
various factors and use that.

``` r
library(MASS)
```

    ## 
    ## Attaching package: 'MASS'

    ## The following object is masked from 'package:gtsummary':
    ## 
    ##     select

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

``` r
library(klaR)

mylda=lda(Application_time~disposal_type+tc+gender+race+tenure_days+descision_year+filing_year,data = T_Data)
mylda[4]
```

    ## $scaling
    ##                            LD1           LD2           LD3           LD4
    ## disposal_typeISS -9.146867e-02  2.0872935997  0.7289554780 -5.964167e-01
    ## tc1700            1.122643e-02 -0.0637429672  0.0816867328 -1.003531e-01
    ## tc2100            1.359685e-02 -0.3914804799  1.4545366477  1.244677e+00
    ## tc2400            2.728631e-02 -0.3494620744  1.6408441743  8.161812e-01
    ## gendermale        4.227479e-03  0.0212075629 -0.1346891185 -1.733898e-02
    ## raceblack         3.079500e-03  0.0231509469 -0.1591104258 -1.788255e-01
    ## raceHispanic     -7.772099e-04  0.0744485951 -0.1983349556 -1.961163e-01
    ## raceother        -3.501465e-02 -0.3926538622  1.1830165376  2.001270e-01
    ## racewhite        -4.664602e-03  0.0740105213 -0.1196450678 -7.051124e-02
    ## tenure_days      -1.360098e-06  0.0002060137 -0.0005098554  2.002177e-05
    ## descision_year   -2.473120e+00  0.0124715925 -0.1430291886  1.724648e-01
    ## filing_year       2.473589e+00  0.0886887201 -0.0468115945  4.678465e-02
    ##                            LD5           LD6           LD7           LD8
    ## disposal_typeISS  0.0164046960 -0.0828798036 -0.2661727062  0.1159391219
    ## tc1700            1.6409085884  1.5627490695  0.3585502388 -0.5833847720
    ## tc2100            1.6183246990 -0.2752849907  0.8829496119 -1.1946491458
    ## tc2400           -0.8301895639  2.1919027392  1.0467098212 -1.0732213508
    ## gendermale        0.1841681327 -0.1413933817  1.3081680701  1.3618474569
    ## raceblack        -0.1054328247 -0.4680224450  0.4566965603  0.6128900826
    ## raceHispanic     -0.0733805963  0.0004726852  1.1177012156 -3.2422176586
    ## raceother        -2.0324757289 -0.0282474164  3.0959798893 13.7247461458
    ## racewhite        -0.0253907110  0.1953192183  1.2069886956 -0.7866615410
    ## tenure_days      -0.0002548702 -0.0001670467  0.0005808717 -0.0004152335
    ## descision_year    0.0163439644  0.0294037633 -0.0145220701  0.0088754792
    ## filing_year       0.0047759963 -0.0127649514  0.0267134820 -0.0237711821
    ##                           LD9          LD10          LD11         LD12
    ## disposal_typeISS -0.056457854  0.0766471320 -0.0555398623  0.238117016
    ## tc1700            0.172973349 -0.0636703420  0.4401333015 -0.841951855
    ## tc2100           -0.009951305  0.4885325711  0.3443880462 -0.925700391
    ## tc2400            0.058067722  0.1458910591  0.3905576605 -1.054023959
    ## gendermale       -0.017314610 -0.9069877320 -0.4424454092  0.399951929
    ## raceblack        -3.986820247 -0.8230861267  3.9190936845 -0.094225873
    ## raceHispanic      1.193334693 -2.4829176887  2.1986235353  3.908102303
    ## raceother        20.058730045 16.2428371120 22.3726251925  3.228394862
    ## racewhite        -1.016869511  1.2944531429  0.3274169230  1.026532185
    ## tenure_days       0.000237686 -0.0001051973  0.0001221482 -0.000649573
    ## descision_year   -0.004607788 -0.0021334411  0.0040284423  0.011718486
    ## filing_year       0.012004546  0.0015392278  0.0082654863 -0.030397681

``` r
mylda=lda(Application_time~gender+race,data = T_Data)
mylda[4]
```

    ## $scaling
    ##                     LD1        LD2         LD3         LD4          LD5
    ## gendermale   -0.9647934 -1.8890264 -0.04912675 -0.16832227  -0.06776579
    ## raceblack     0.8602127 -0.8759632 -3.36280484  3.63117398  -2.60382441
    ## raceHispanic  0.6087805  0.2268299 -0.59551907 -3.27588631  -5.28205290
    ## raceother    -4.6117744  1.4170005 28.35446719 17.71117271 -15.44707064
    ## racewhite     2.0594032 -0.9739733  0.12618170  0.07396294  -0.65978162

``` r
drop <- c("Application_time","descision_year","filing_year","Date_time","tenure_days","tc","filing_date","disposal_type")
pred_df = T_Data[0:1000,!(names(T_Data) %in% drop)]
library(caret)
```

    ## Loading required package: lattice

    ## 
    ## Attaching package: 'caret'

    ## The following object is masked from 'package:survival':
    ## 
    ##     cluster

    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

``` r
predictions=as.numeric(predict(mylda,newdata=pred_df)$class)
Application_time=T_Data$Application_time[0:1000]
test=data.frame(predictions,Application_time)
test$abs_diff<-abs(test$predictions-test$Application_time)
test$sq_error<-test$abs_diff**2
MAE=mean(test$abs_diff)
MSE=mean(test$sq_error)
MAE
```

    ## [1] 562.345

``` r
MSE
```

    ## [1] 896196.3

``` r
#partimat(race~Application_time+filing_date, method="lda",data=T_Data)
```

# Appendix

## Tree model Summary

``` r
summary(optimal_tree)
```

    ## Call:
    ## rpart(formula = Application_time ~ gender + race, data = T_Data, 
    ##     control = rpart.control(cp = opt_cp))
    ##   n= 1364979 
    ## 
    ##             CP nsplit rel error    xerror        xstd
    ## 1 2.517560e-05      0 1.0000000 1.0000019 0.001991459
    ## 2 1.796204e-05      4 0.9998993 0.9999296 0.001990961
    ## 3 1.165379e-05      5 0.9998813 0.9998978 0.001990966
    ## 4 1.000000e-05      6 0.9998697 0.9998886 0.001990930
    ## 
    ## Variable importance
    ##   race gender 
    ##     74     26 
    ## 
    ## Node number 1: 1364979 observations,    complexity param=2.51756e-05
    ##   mean=1488.732, MSE=974498 
    ##   left son=2 (1363986 obs) right son=3 (993 obs)
    ##   Primary splits:
    ##       race   splits as  LLLRL, improve=2.042661e-05, (0 missing)
    ##       gender splits as  LR,    improve=2.287552e-06, (0 missing)
    ## 
    ## Node number 2: 1363986 observations,    complexity param=2.51756e-05
    ##   mean=1488.612, MSE=974624.6 
    ##   left son=4 (412050 obs) right son=5 (951936 obs)
    ##   Primary splits:
    ##       race   splits as  LLL-R, improve=1.869641e-05, (0 missing)
    ##       gender splits as  LR,    improve=2.036967e-06, (0 missing)
    ## 
    ## Node number 3: 993 observations
    ##   mean=1654.088, MSE=773212.3 
    ## 
    ## Node number 4: 412050 observations,    complexity param=2.51756e-05
    ##   mean=1482.123, MSE=884607.7 
    ##   left son=8 (264229 obs) right son=9 (147821 obs)
    ##   Primary splits:
    ##       gender splits as  RL,    improve=5.858999e-05, (0 missing)
    ##       race   splits as  RLR--, improve=1.806609e-05, (0 missing)
    ## 
    ## Node number 5: 951936 observations,    complexity param=1.796204e-05
    ##   mean=1491.42, MSE=1013563 
    ##   left son=10 (305044 obs) right son=11 (646892 obs)
    ##   Primary splits:
    ##       gender splits as  LR, improve=2.476305e-05, (0 missing)
    ## 
    ## Node number 8: 264229 observations,    complexity param=1.165379e-05
    ##   mean=1476.739, MSE=859190.5 
    ##   left son=16 (238236 obs) right son=17 (25993 obs)
    ##   Primary splits:
    ##       race splits as  LLR--, improve=6.828168e-05, (0 missing)
    ## 
    ## Node number 9: 147821 observations,    complexity param=2.51756e-05
    ##   mean=1491.749, MSE=929896.2 
    ##   left son=18 (35222 obs) right son=19 (112599 obs)
    ##   Primary splits:
    ##       race splits as  RLL--, improve=0.0004406405, (0 missing)
    ## 
    ## Node number 10: 305044 observations
    ##   mean=1484.124, MSE=1011452 
    ## 
    ## Node number 11: 646892 observations
    ##   mean=1494.86, MSE=1014521 
    ## 
    ## Node number 16: 238236 observations
    ##   mean=1474.209, MSE=853949.1 
    ## 
    ## Node number 17: 25993 observations
    ##   mean=1499.927, MSE=906633.4 
    ## 
    ## Node number 18: 35222 observations
    ##   mean=1455.556, MSE=962994.9 
    ## 
    ## Node number 19: 112599 observations
    ##   mean=1503.07, MSE=919004.7

## LDA Summary

``` r
#mylda
```

## Clustering that did not work

## Unused Code kept for reference.

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


# library(dcldata)
```

## Clustering

Lastly, we are going to look at clustering to group the applications
together. This may reveal some hidden characteristics and trains not
seen before.

``` r
# km.2=kmeans(T_Data[,c(8,6)], 2) #2 clusters
# km.3=kmeans(T_Data[,c(8,6)], 3) #3 clusters
# km.5=kmeans(T_Data[,c(8,6)], 5)#5 clusters
```

# 

``` r
# T_Data$cluster=as.factor(km.2$cluster)
# ggplot(T_Data,aes(y=Application_time, x=tenure_days))+
#   geom_point(aes(colour=cluster))
```

# 

``` r
# T_Data$cluster=as.factor(km.3$cluster)
# ggplot(T_Data,aes(y=Application_time, x=tenure_days))+
#   geom_point(aes(colour=cluster))
```

# 

``` r
# T_Data$cluster=as.factor(km.5$cluster)
# ggplot(T_Data,aes(y=Application_time, x=tenure_days))+
#   geom_point(aes(colour=cluster))
```
