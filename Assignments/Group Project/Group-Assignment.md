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
library(ggplot2)
  
#for first time use
#install.packages('compareGroups')
#install.packages('webr')
#install.packages('prophet')
#install.packages('survival')
#install.packages('ggord')

#archive
#library(tsibble)
#library(tidyverse)
```

### Load data

Now we load in the data. The app_gender_rate data is the primary data we
will use for now. This data contains the transaction data for all
applications, the examiner who processed them and their associated
traits such as gender and ethnicity.

``` r
App_data=read_parquet('apps_gender_rate.parquet')
total_rows=nrow(App_data)
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

nrow(App_data)/total_rows*100
```

    ## [1] 84.75668

``` r
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


Aside <- T_Data %>% 
  filter(Date_time>= as.Date("2017-01-01"))

#Remove all the data we will not need based on application status
exclude_list=c("PEND")
T_Data <- T_Data %>%
  filter(!disposal_type %in% exclude_list)
#Data Remain
nrow(T_Data)/nrow(App_data)*100
```

    ## [1] 83.85241

Now we can remove the data from after 2017 since it has high levels of
outliers and bad data.

``` r
#remove all values after 2017
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

#=The other gender has been giving issues will be maintained in future in original dataset
T_Data_OG=T_Data
T_Data <- T_Data %>% 
  filter(race != "other")
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
T_Temp=distinct(T_Data_OG, examiner_id, .keep_all = TRUE)
Temp_data <- T_Temp %>% 
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
    ##   gender race     count
    ##   <chr>  <chr>    <int>
    ## 1 female Asian      358
    ## 2 female black       67
    ## 3 female Hispanic    62
    ## 4 female white      967
    ## 5 male   Asian      802
    ## 6 male   black       97
    ## 7 male   Hispanic   137
    ## 8 male   other        2
    ## 9 male   white     2254

``` r
summary(Aside)
```

    ##  application_number  filing_date         examiner_name_last examiner_name_first
    ##  Length:255633      Min.   :2000-01-06   Length:255633      Length:255633      
    ##  Class :character   1st Qu.:2013-11-01   Class :character   Class :character   
    ##  Mode  :character   Median :2014-12-10   Mode  :character   Mode  :character   
    ##                     Mean   :2014-05-15                                         
    ##                     3rd Qu.:2015-10-23                                         
    ##                     Max.   :2017-05-25                                         
    ##                                                                                
    ##  examiner_name_middle  examiner_id    examiner_art_unit  uspc_class       
    ##  Length:255633        Min.   :59025   Min.   :1600      Length:255633     
    ##  Class :character     1st Qu.:66620   1st Qu.:1711      Class :character  
    ##  Mode  :character     Median :75606   Median :1777      Mode  :character  
    ##                       Mean   :78936   Mean   :1955                        
    ##                       3rd Qu.:93750   3rd Qu.:2184                        
    ##                       Max.   :99988   Max.   :2498                        
    ##                                                                           
    ##  uspc_subclass      patent_number      patent_issue_date   
    ##  Length:255633      Length:255633      Min.   :2003-03-11  
    ##  Class :character   Class :character   1st Qu.:2017-01-31  
    ##  Mode  :character   Mode  :character   Median :2017-03-21  
    ##                                        Mean   :2015-09-05  
    ##                                        3rd Qu.:2017-05-02  
    ##                                        Max.   :2017-06-20  
    ##                                        NA's   :204389      
    ##   abandon_date        disposal_type      appl_status_code appl_status_date  
    ##  Min.   :2006-05-23   Length:255633      Min.   : 17.00   Length:255633     
    ##  1st Qu.:2016-10-13   Class :character   1st Qu.: 41.00   Class :character  
    ##  Median :2016-11-28   Mode  :character   Median : 71.00   Mode  :character  
    ##  Mean   :2016-11-27                      Mean   : 86.48                     
    ##  3rd Qu.:2017-01-10                      3rd Qu.:150.00                     
    ##  Max.   :2017-06-05                      Max.   :865.00                     
    ##  NA's   :237306                                                             
    ##        tc          gender              race           earliest_date       
    ##  Min.   :1600   Length:255633      Length:255633      Min.   :2000-01-02  
    ##  1st Qu.:1700   Class :character   Class :character   1st Qu.:2000-02-26  
    ##  Median :1700   Mode  :character   Mode  :character   Median :2002-12-09  
    ##  Mean   :1903                                         Mean   :2003-10-28  
    ##  3rd Qu.:2100                                         3rd Qu.:2005-12-09  
    ##  Max.   :2400                                         Max.   :2016-03-03  
    ##                                                                           
    ##   latest_date          tenure_days     Date_time         
    ##  Min.   :2017-01-04   Min.   : 373   Min.   :2017-01-01  
    ##  1st Qu.:2017-05-19   1st Qu.:4180   1st Qu.:2017-02-24  
    ##  Median :2017-05-22   Median :5274   Median :2017-03-29  
    ##  Mean   :2017-05-20   Mean   :4953   Mean   :2017-04-16  
    ##  3rd Qu.:2017-05-23   3rd Qu.:6292   3rd Qu.:2017-04-28  
    ##  Max.   :2017-12-06   Max.   :6518   Max.   :9468-10-16  
    ## 

``` r
summary(T_Data)
```

    ##   examiner_id     filing_date         disposal_type    tc        
    ##  Min.   :59012   Min.   :2000-01-02   ABN:498002    1600:368721  
    ##  1st Qu.:66582   1st Qu.:2004-04-02   ISS:865984    1700:528977  
    ##  Median :75341   Median :2007-11-13                 2100:270143  
    ##  Mean   :78842   Mean   :2007-10-08                 2400:196145  
    ##  3rd Qu.:93845   3rd Qu.:2011-05-04                              
    ##  Max.   :99988   Max.   :2016-11-14                              
    ##     gender             race         tenure_days     Date_time         
    ##  female:452865   Asian   :325412   Min.   : 216   Min.   :2000-05-24  
    ##  male  :911121   black   : 47873   1st Qu.:5180   1st Qu.:2009-06-29  
    ##                  Hispanic: 38765   Median :6209   Median :2012-07-13  
    ##                  white   :951936   Mean   :5669   Mean   :2011-11-05  
    ##                                    3rd Qu.:6338   3rd Qu.:2014-12-10  
    ##                                    Max.   :6518   Max.   :2017-01-01  
    ##  Application_time  filing_year   descision_year   start_data        
    ##  Min.   :  11     Min.   :2000   Min.   :2000   Min.   :2000-05-24  
    ##  1st Qu.: 858     1st Qu.:2004   1st Qu.:2009   1st Qu.:2001-07-19  
    ##  Median :1213     Median :2007   Median :2012   Median :2004-09-30  
    ##  Mean   :1489     Mean   :2007   Mean   :2011   Mean   :2005-04-05  
    ##  3rd Qu.:1786     3rd Qu.:2011   3rd Qu.:2014   3rd Qu.:2008-08-13  
    ##  Max.   :6187     Max.   :2016   Max.   :2017   Max.   :2016-11-29  
    ##  Approx_Tenue_Days
    ##  Min.   :   0     
    ##  1st Qu.:1186     
    ##  Median :2215     
    ##  Mean   :2405     
    ##  3rd Qu.:3472     
    ##  Max.   :6064

let’s make a quick pie chart to vislize the data

``` r
PD = T_Data %>%
  group_by(gender, race) %>%
  summarise(n = n())
```

    ## `summarise()` has grouped output by 'gender'. You can override using the
    ## `.groups` argument.

``` r
library(webr)
PieDonut(PD, aes(gender,race, count=n), title = "Patent Examiners by Race & Gender",r0 = 0.45, r1 = 0.9, addDonutLabel= TRUE )
```

    ## Warning: The `<scale>` argument of `guides()` cannot be `FALSE`. Use "none" instead as
    ## of ggplot2 3.3.4.
    ## ℹ The deprecated feature was likely used in the webr package.
    ##   Please report the issue at <]8;;https://github.com/cardiomoon/webr/issueshttps://github.com/cardiomoon/webr/issues]8;;>.

![](Group-Assignment_files/figure-gfm/pie%20chart-1.png)<!-- -->

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
  ggtitle("Histogram of Application filing year.")
hists
```

![](Group-Assignment_files/figure-gfm/histogram%20distribtion%20ethnicity%202-1-1.png)<!-- -->

``` r
hists=ggplot(T_Data, aes(x=filing_year))+
  geom_histogram(bins = 30)+
  facet_grid(T_Data$tc)+
  ggtitle("Histogram of Application filing year.")
hists
```

![](Group-Assignment_files/figure-gfm/histogram%20distribtion%20ethnicity%202-2-1.png)<!-- -->

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
library(ggpubr)

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

``` r
ggboxplot(T_Data, x= 'race', y='Application_time',color='race',xlab=FALSE,ylab="Application Time (in Days)")+
  rotate_x_text(45)
```

![](Group-Assignment_files/figure-gfm/box%20plots2-1.png)<!-- -->

``` r
ggboxplot(T_Data, x= 'gender', y='Application_time',color='gender',xlab=FALSE,ylab="Application Time (in Days)")+
  rotate_x_text(45)
```

![](Group-Assignment_files/figure-gfm/box%20plots3-1.png)<!-- -->

``` r
ggboxplot(T_Data, x= 'race_gender', y='Application_time',color='race_gender',xlab=FALSE,ylab="Application Time (in Days)")+
  rotate_x_text(45)
```

![](Group-Assignment_files/figure-gfm/box%20plots-1.png)<!-- -->

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

## Average Comparisons

Let’s compare the averages between groups. First we will get some
averages

``` r
aggregate(T_Data$Application_time, list(T_Data$gender), FUN=mean) 
```

    ##   Group.1        x
    ## 1  female 1486.613
    ## 2    male 1489.605

``` r
aggregate(T_Data$Application_time, list(T_Data$race), FUN=mean) 
```

    ##    Group.1        x
    ## 1    Asian 1483.886
    ## 2    black 1471.097
    ## 3 Hispanic 1480.946
    ## 4    white 1491.420

``` r
aggregate(T_Data$Application_time, list(T_Data$gender,T_Data$race), FUN=mean) 
```

    ##   Group.1  Group.2        x
    ## 1  female    Asian 1503.070
    ## 2    male    Asian 1473.735
    ## 3  female    black 1463.089
    ## 4    male    black 1478.169
    ## 5  female Hispanic 1442.315
    ## 6    male Hispanic 1499.927
    ## 7  female    white 1484.124
    ## 8    male    white 1494.860

Next, we will run some statistical testing on them

``` r
#similarlity between men and women
t.test(Application_time ~ gender, data = T_Data, var.equal = FALSE)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  Application_time by gender
    ## t = -1.6625, df = 897656, p-value = 0.09642
    ## alternative hypothesis: true difference in means between group female and group male is not equal to 0
    ## 95 percent confidence interval:
    ##  -6.5192064  0.5353911
    ## sample estimates:
    ## mean in group female   mean in group male 
    ##             1486.613             1489.605

``` r
#similarlity between all means
t=aov(Application_time ~ race, data = T_Data)
summary(t)
```

    ##                  Df    Sum Sq  Mean Sq F value   Pr(>F)    
    ## race              3 3.174e+07 10579703   10.86 3.98e-07 ***
    ## Residuals   1363982 1.329e+12   974604                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
t=aov(Application_time ~ race+gender, data = T_Data)
summary(t)
```

    ##                  Df    Sum Sq  Mean Sq F value   Pr(>F)    
    ## race              3 3.174e+07 10579703  10.855 3.98e-07 ***
    ## gender            1 1.810e+06  1809621   1.857    0.173    
    ## Residuals   1363981 1.329e+12   974604                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

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
    y=mean(Application_time),
    )
#Check for duplicates. If true then there are no duplicates
length(unique(Pred_Data$ds)) == nrow(Pred_Data)
```

    ## [1] TRUE

``` r
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

    ##                       ds     yhat yhat_lower yhat_upper
    ## 5821 2017-12-26 19:00:00 1537.969   1039.376   2006.257
    ## 5822 2017-12-27 19:00:00 1575.114   1039.623   2054.390
    ## 5823 2017-12-28 19:00:00 1991.977   1498.965   2482.493
    ## 5824 2017-12-29 19:00:00 1572.250   1068.335   2118.777
    ## 5825 2017-12-30 19:00:00 1574.136   1089.128   2086.413
    ## 5826 2017-12-31 19:00:00 1888.422   1383.422   2366.724

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
  ggtitle("Forecast of length of application time")
```

![](Group-Assignment_files/figure-gfm/prophet%20prediction%20plot-1.png)<!-- -->
Let’s try breaking these out a bit into weekdays.

``` r
Pred_Data$DOW=wday(Pred_Data$ds,week_start = 1, label=TRUE)

# Plot the prophet forecast with the test data points
ggplot(forecast, aes(ds, yhat)) +
  geom_point(data = Pred_Data, aes(ds, y, color = DOW), size = 1.5,alpha=0.4) +
  scale_color_brewer(palette = "Set1")+
  xlab("Date") +
  ylab("y") +
  ggtitle("Forecast of length of application time")
```

![](Group-Assignment_files/figure-gfm/prophet%20prediction%20plot2-1.png)<!-- -->
This is shows that as time goes on the average patent application length
is longer if the status update is on a Sunday or Thursday but farily
consistent otherwise

Let’s try breaking these out a bit, first by gender since there seems to
be 3 different trends

``` r
Pred_Data_M <- T_Data %>% 
  group_by(Date_time,gender) %>% 
  summarise(
    ds = as.POSIXct(min(Date_time)),
    y=mean(Application_time)
    )%>% 
  filter(gender=="male")
```

    ## `summarise()` has grouped output by 'Date_time'. You can override using the
    ## `.groups` argument.

``` r
Pred_Data_F <- T_Data %>% 
  group_by(Date_time,gender) %>% 
  summarise(
    ds = as.POSIXct(min(Date_time)),
    y=mean(Application_time)
    )%>% 
  filter(gender=="female")
```

    ## `summarise()` has grouped output by 'Date_time'. You can override using the
    ## `.groups` argument.

``` r
m_M<-prophet(Pred_Data_M)
future <- make_future_dataframe(m_M, periods = 720)
forecast_M <- predict(m_M, future)

m_F<-prophet(Pred_Data_F)
future <- make_future_dataframe(m_F, periods = 720)
forecast_F <- predict(m_F, future)
```

``` r
ggplot(forecast, aes(ds, yhat)) +
  geom_ribbon(data = forecast_F,aes(ymin=yhat_lower,ymax=yhat_upper), alpha=0.4, color = "lightpink")+
  geom_ribbon(data = forecast_M,aes(ymin=yhat_lower,ymax=yhat_upper), alpha=0.1, color = "lightblue")+
  geom_point(data = Pred_Data_F, aes(ds, y, color = "Female"), size = 2,alpha=.2) +
  geom_point(data = Pred_Data_M, aes(ds, y, color = "Male"), size = 2,alpha=.2) +
  xlab("Date") +
  ylab("y") +
  ggtitle("Forecast of Male vs. Femal Average Application Days")
```

![](Group-Assignment_files/figure-gfm/prophet%20prediction%20plot%205-1.png)<!-- -->

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
    ## disposal_typeISS -9.139768e-02  2.0875076332  0.7281614269 -5.964029e-01
    ## tc1700            1.122212e-02 -0.0638495316  0.0827894553 -9.934287e-02
    ## tc2100            1.359300e-02 -0.3909096491  1.4564658416  1.247454e+00
    ## tc2400            2.729405e-02 -0.3490901480  1.6432091224  8.167722e-01
    ## gendermale        4.230642e-03  0.0211669457 -0.1349822114 -1.777988e-02
    ## raceblack         3.079051e-03  0.0231003935 -0.1591787861 -1.778914e-01
    ## raceHispanic     -8.098515e-04  0.0744722060 -0.1984511755 -1.949934e-01
    ## racewhite        -4.666667e-03  0.0740425011 -0.1197757916 -6.980783e-02
    ## tenure_days      -1.372743e-06  0.0002059935 -0.0005105429  2.022426e-05
    ## descision_year   -2.473098e+00  0.0123957530 -0.1429608093  1.724485e-01
    ## filing_year       2.473558e+00  0.0886939228 -0.0470010632  4.675906e-02
    ##                            LD5           LD6           LD7           LD8
    ## disposal_typeISS  0.0166802781 -0.0847100128 -0.2677139310  0.1163360606
    ## tc1700            1.6399378495  1.5664540137  0.3600985358 -0.6376455539
    ## tc2100            1.6137567265 -0.2743873104  0.9147247002 -1.1783024245
    ## tc2400           -0.8360715397  2.1904703642  1.0713398600 -1.0471257949
    ## gendermale        0.1858349620 -0.1378222838  1.2782306137  1.4527003198
    ## raceblack        -0.1060142592 -0.4731181636  0.5069198999  1.4936127179
    ## raceHispanic     -0.0746913103  0.0001177869  1.1852026560 -3.4168286664
    ## racewhite        -0.0260327009  0.1947275806  1.2429535381 -0.5694920004
    ## tenure_days      -0.0002541026 -0.0001646978  0.0005866077 -0.0004506496
    ## descision_year    0.0163044779  0.0295401553 -0.0149234454  0.0090882057
    ## filing_year       0.0048847187 -0.0125359761  0.0268711815 -0.0266276464
    ##                            LD9          LD10          LD11
    ## disposal_typeISS  0.0513445914  0.0860847509  0.2390713885
    ## tc1700            0.0833429706 -0.3343439233 -0.8591706784
    ## tc2100            0.5459221324  0.0761605144 -0.9450020752
    ## tc2400            0.3126736608 -0.1842126029 -1.0672799583
    ## gendermale       -0.8528072939 -0.2918254744  0.4298659079
    ## raceblack         3.9500089672 -3.7910604297 -0.1831562880
    ## raceHispanic     -0.8224579479 -3.2999461388  3.8449115087
    ## racewhite         1.6723418472  0.5168258404  1.0046629987
    ## tenure_days      -0.0001412374 -0.0001327943 -0.0006553529
    ## descision_year    0.0030147682 -0.0047208564  0.0116806758
    ## filing_year      -0.0021589524 -0.0033643447 -0.0309467681

``` r
mylda=lda(Application_time~gender+race,data = T_Data)
mylda[4]
```

    ## $scaling
    ##                     LD1        LD2         LD3        LD4
    ## gendermale   -0.9833421 -1.8806432  0.09791943 -0.1338755
    ## raceblack     0.8510301 -0.8640677 -5.20001934 -2.0606157
    ## raceHispanic  0.6032587  0.2397385  1.64089376 -6.0220964
    ## racewhite     2.0858843 -0.9914619 -0.08071296 -0.5494954

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

    ## [1] 551.344

``` r
MSE
```

    ## [1] 885801.8

``` r
#partimat(race~Application_time+filing_date, method="lda",data=T_Data)
```

# Appendix

## Descriptive Stats

``` r
library(compareGroups)
res<-compareGroups(gender~Application_time, data=T_Data)
res
```

    ## 
    ## 
    ## -------- Summary of results by groups of 'gender'---------
    ## 
    ## 
    ##   var              N       p.value method            selection
    ## 1 Application_time 1363986 0.096*  continuous normal ALL      
    ## -----
    ## Signif. codes:  0 '**' 0.05 '*' 0.1 ' ' 1

``` r
res<-compareGroups(race~Application_time, data=T_Data)
res
```

    ## 
    ## 
    ## -------- Summary of results by groups of 'race'---------
    ## 
    ## 
    ##   var              N       p.value  method            selection
    ## 1 Application_time 1363986 <0.001** continuous normal ALL      
    ## -----
    ## Signif. codes:  0 '**' 0.05 '*' 0.1 ' ' 1

## predictions

We will make predictions based on productivity.

``` r
library(prophet)

Pred_Data <- T_Data %>% 
  group_by(Date_time) %>% 
  summarise(
    ds = as.POSIXct(min(Date_time)), # I have repeatedly attempted to change this assumption but have been unable to
    y=n(),
    )
#Check for duplicates. If true then there are no duplicates
length(unique(Pred_Data$ds)) == nrow(Pred_Data)
```

    ## [1] TRUE

``` r
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
    ## 5821 2017-12-26 19:00:00 1136.5869  790.59088  1491.7980
    ## 5822 2017-12-27 19:00:00  443.6840   93.74479   817.7561
    ## 5823 2017-12-28 19:00:00  455.3918   91.36617   801.7958
    ## 5824 2017-12-29 19:00:00  274.3422  -82.07572   616.6203
    ## 5825 2017-12-30 19:00:00  225.2376 -139.41310   568.4151
    ## 5826 2017-12-31 19:00:00  533.0541  172.11100   907.9425

``` r
plot(m, forecast)
```

![](Group-Assignment_files/figure-gfm/prophet%20prediction-update%203-1.png)<!-- -->
We can look at the key feautres effecting the model

``` r
prophet_plot_components(m, forecast)
```

![](Group-Assignment_files/figure-gfm/prophet%20prediction-update%204-1.png)<!-- -->
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
  ggtitle("Forecast of length of application time")
```

![](Group-Assignment_files/figure-gfm/prophet%20prediction-update%20plot-1.png)<!-- -->
Let’s try breaking these out a bit, since there seems to be 3 different
trends

``` r
Pred_Data$DOW=wday(Pred_Data$ds,week_start = 1, label=TRUE)

# Plot the prophet forecast with the test data points
ggplot(forecast, aes(ds, yhat)) +
  geom_point(data = Pred_Data, aes(ds, y, color = DOW), size = 1,alpha=0.4) +
  scale_color_brewer(palette = "Set1")+
  xlab("Date") +
  ylab("y") +
  ggtitle("Forecast of length of application time")
```

![](Group-Assignment_files/figure-gfm/prophet%20prediction-update%20plot2-1.png)<!-- -->

Let’s try breaking these out a bit, since there seems to be 3 different
trends

``` r
Pred_Data_low=App_data <- Pred_Data %>% 
  filter(y<365)
Pred_Data_med=App_data <- Pred_Data %>% 
  filter(y>365 & y<800)

Pred_Data_high=App_data <- Pred_Data %>% 
  filter(y>800)

# Plot the prophet forecast with the test data points
ggplot(forecast, aes(ds, yhat)) +
  geom_point(data = Pred_Data_low, aes(ds, y, color = "low"), size = 2,alpha=0.2) +
  geom_point(data = Pred_Data_med, aes(ds, y, color = "med"), size = 2,alpha=0.2) +
  geom_point(data = Pred_Data_high, aes(ds, y, color = "high"), size = 2,alpha=0.2) +
  scale_color_manual(values = c("blue", "red", "black"), labels = c("high", "low","med"))+
  xlab("Date") +
  ylab("y") +
  ggtitle("Forecast of application status updates")
```

![](Group-Assignment_files/figure-gfm/prophet%20prediction-update%20plot3-1.png)<!-- -->

## Tree model Summary

``` r
summary(optimal_tree)
```

    ## Call:
    ## rpart(formula = Application_time ~ gender + race, data = T_Data, 
    ##     control = rpart.control(cp = opt_cp))
    ##   n= 1363986 
    ## 
    ##             CP nsplit rel error    xerror        xstd
    ## 1 2.677460e-05      0 1.0000000 1.0000017 0.001992322
    ## 2 1.797278e-05      3 0.9999197 0.9999265 0.001991758
    ## 3 1.166076e-05      4 0.9999017 0.9999236 0.001991778
    ## 4 1.000000e-05      5 0.9998900 0.9999097 0.001991743
    ## 
    ## Variable importance
    ##   race gender 
    ##     69     31 
    ## 
    ## Node number 1: 1363986 observations,    complexity param=2.67746e-05
    ##   mean=1488.612, MSE=974624.6 
    ##   left son=2 (412050 obs) right son=3 (951936 obs)
    ##   Primary splits:
    ##       race   splits as  LLLR, improve=1.869641e-05, (0 missing)
    ##       gender splits as  LR,   improve=2.036967e-06, (0 missing)
    ## 
    ## Node number 2: 412050 observations,    complexity param=2.67746e-05
    ##   mean=1482.123, MSE=884607.7 
    ##   left son=4 (264229 obs) right son=5 (147821 obs)
    ##   Primary splits:
    ##       gender splits as  RL,   improve=5.858999e-05, (0 missing)
    ##       race   splits as  RLR-, improve=1.806609e-05, (0 missing)
    ## 
    ## Node number 3: 951936 observations,    complexity param=1.797278e-05
    ##   mean=1491.42, MSE=1013563 
    ##   left son=6 (305044 obs) right son=7 (646892 obs)
    ##   Primary splits:
    ##       gender splits as  LR, improve=2.476305e-05, (0 missing)
    ## 
    ## Node number 4: 264229 observations,    complexity param=1.166076e-05
    ##   mean=1476.739, MSE=859190.5 
    ##   left son=8 (238236 obs) right son=9 (25993 obs)
    ##   Primary splits:
    ##       race splits as  LLR-, improve=6.828168e-05, (0 missing)
    ## 
    ## Node number 5: 147821 observations,    complexity param=2.67746e-05
    ##   mean=1491.749, MSE=929896.2 
    ##   left son=10 (35222 obs) right son=11 (112599 obs)
    ##   Primary splits:
    ##       race splits as  RLL-, improve=0.0004406405, (0 missing)
    ## 
    ## Node number 6: 305044 observations
    ##   mean=1484.124, MSE=1011452 
    ## 
    ## Node number 7: 646892 observations
    ##   mean=1494.86, MSE=1014521 
    ## 
    ## Node number 8: 238236 observations
    ##   mean=1474.209, MSE=853949.1 
    ## 
    ## Node number 9: 25993 observations
    ##   mean=1499.927, MSE=906633.4 
    ## 
    ## Node number 10: 35222 observations
    ##   mean=1455.556, MSE=962994.9 
    ## 
    ## Node number 11: 112599 observations
    ##   mean=1503.07, MSE=919004.7

## Survival

``` r
library(survival)
library(lubridate)
library(ggsurvfit)
library(gtsummary)
library(tidycmprsk)

T_Data_OG<- T_Data_OG %>% 
  mutate(
    status = recode(disposal_type, `ABN` = 0, `ISS` = 1)
  )

T_Data_OG$Application_time <- T_Data_OG$Date_time - T_Data_OG$filing_date
T_Data_OG$Application_time <- as.numeric(T_Data_OG$Application_time)

survfit(Surv(Application_time) ~ 1, data = T_Data_OG) %>% 
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  ) + 
  add_confidence_interval()+
  add_risktable()
```

![](Group-Assignment_files/figure-gfm/survival%20appendix%201-1.png)<!-- -->
Looking at the gender effect

``` r
survfit(Surv(Application_time) ~ gender, data = T_Data_OG) %>% 
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  ) + 
  add_confidence_interval()
```

![](Group-Assignment_files/figure-gfm/survival%20appendix%202-1.png)<!-- -->

looking at the ethnicity effects

``` r
survfit(Surv(Application_time) ~ race, data = T_Data_OG) %>% 
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  ) + 
  add_confidence_interval()
```

![](Group-Assignment_files/figure-gfm/survival%20appendix%203-1.png)<!-- -->

``` r
survfit(Surv(Application_time, status) ~ gender+race, data = T_Data_OG) %>% 
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  ) + 
  add_confidence_interval()
```

![](Group-Assignment_files/figure-gfm/survival%20appendix%204-1.png)<!-- -->

## LDA Summary

``` r
#mylda
```

## Survival

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
