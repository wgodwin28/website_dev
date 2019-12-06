#Exploratory Analysis for google data

# clear memory
rm(list=ls())
library(data.table); library(ggplot2); library(glmnet); library(magrittr); library(tidyverse); 
library(scales); library(lubridate)
detach(package:plyr) #dplyr must be loaded after plyr for summarize to work correctly

#Downloaded from here: https://transparencyreport.google.com/political-ads/region/US?hl=en

##############################################################
#ECDF Tangent
##############################################################
#empirical cumulative distribution function-feed it a vector and produces ecdf
ecdf_custom <- function(x){
  x <- sort(x)
  y <- seq(1, length(x))/length(x)
  df <- data.frame(x=x, y=y, type="empirical")
}
#theoretical cdf
tcdf <- function(n, lambdaa){
  x <- rpois(n, lambda = lambdaa)
  x <- sort(x)
  y <- seq(1, length(x))/length(x)
  df <- data.frame(x=x, y=y, type="theoretical")
}

#use the functions and plot the distributions
df <- ecdf_custom(dt$Num_of_Days)
df.t <- tcdf(10000, mean(dt$Num_of_Days))
df <- rbind(df, df.t)
ggplot(df.t, aes(x,y)) +
  geom_point()

##############################################################
#Creative ads stats
##############################################################
#read in data
dt <- fread("~/Desktop/website_dev/google_ads/data/google-political-ads-transparency-bundle/google-political-ads-creative-stats.csv")
#glimpse(dt) #use this on other data frames

#relevant variables
vars_keep <- c("Ad_Type", "Regions", "Advertiser_ID", "Date_Range_Start", "Date_Range_End", "Num_of_Days", 
               "Impressions", "Spend_Range_Min_USD", "Spend_Range_Max_USD")

#subset to variables of interest and create useful variables
dt <- dt %>%
  select(vars_keep) %>% #keep relevant variables
  mutate(startYear = substr(Date_Range_Start,1,4), #variable indicating year ad started
         regions = ifelse(grepl("EU", Regions), "EU", "US"),
         month_year = format(as.Date(Date_Range_Start), "%m-%Y"),
         week_start = floor_date(as.Date(Date_Range_Start), unit = "week")) %>% #create cleaner regions variable
  filter(regions=="US") #only include US data

#Histogram continuous variables
ggplot(dt, aes(Num_of_Days)) +
  geom_histogram()
#log-transform due to long tail
ggplot(dt %>%
         filter(Num_of_Days<250), aes(log(Num_of_Days))) +
  geom_histogram()

#Visualize the category variables in tables
#Table 1
dt %>%
  count(Spend_Range_Min_USD, Spend_Range_Max_USD) %>%
  mutate(percent_ads=percent(n/sum(n)))#perfectly correlated to show lower/upper bounds of spending
#table 2
dt %>%
  count(Ad_Type) %>%
  mutate(percent_type=percent(n/sum(n)))#healthy distribution of all ad types
dt %>%
  count(month_year)
dt %>%
  count(Impressions)

##Conditional summaries and plots
dt %>% 
  group_by(month_year) %>%
  summarize(mean_max=mean(Spend_Range_Max_USD, na.rm=T),
            mean_min=mean(Spend_Range_Min_USD),
            total=n())###Majority of is spent on video ads over text/image

dt %>% 
  mutate(days_cat = cut(Num_of_Days, c(0,10,20,50,420))) %>%
  group_by(Ad_Type, days_cat) %>%
  summarize(mean_max=mean(Spend_Range_Max_USD, na.rm=T),
            mean_min=mean(Spend_Range_Min_USD),
            total=n())###Majority of money that's spent on video ads runs for longer than 20 days

ggplot(data=dt %>%
         mutate(days_cat = cut(Num_of_Days, c(0,10,20,50,420))), aes(Spend_Range_Max_USD)) +
  geom_histogram()

ggplot(data=dt, aes(Num_of_day)) +
  geom_histogram()

#time series of number of ads
dt.plot <- dt %>%
  group_by(date=as.Date(week_start)) %>%
  summarize(weekly_ad_count=n())

ggplot(dt.plot, aes(date, weekly_ad_count)) +
  geom_point() +
  xlab("Date") +
  ylab("Number of Ads") +
  scale_y_continuous(breaks= pretty_breaks()) +
  scale_x_date(labels = date_format("%m/%Y"), breaks = date_breaks("2 month")) +
  theme_bw()
##############################################################
#Ads in time series
##############################################################
#Look at spending over time
dt.spend <-read.csv("~/Desktop/website_dev/google_ads/data/google-political-ads-transparency-bundle/google-political-ads-advertiser-weekly-spend.csv")
dt.plot <- dt.spend %>%
  group_by(date=as.Date(Week_Start_Date)) %>%
  summarize(spend_weekly=sum(Spend_USD))

ggplot(dt.plot, aes(date, spend_weekly)) +
  geom_point() +
  xlab("Date") +
  ylab("Spending (in USD)") +
  scale_y_continuous(breaks= pretty_breaks()) +
  scale_x_date(labels = date_format("%m/%Y"), breaks = date_breaks("2 month")) +
  theme_bw()
#As expected, spending increased exponentially the weeks up to the election and then precipitously dropped to levels of 5 months previous


#Time series by region (US or EU)             
dt.adverts <-fread("/Desktop/website_dev/google_ads/data/google-political-ads-transparency-bundle/google-political-ads-advertiser-stats.csv")
dt.spend <- merge(dt.spend, dt.adverts[,.(Advertiser_ID, Regions, Total_Creatives)], by="Advertiser_ID", all.x = T)
dt.spend <- dt.spend %>%
  mutate(regions2=if_else(Regions=="US", "US", "EU"))

dt.plot <- dt.spend %>%
  group_by(date = as.Date(Week_Start_Date), region=regions2) %>%
  summarise(spend_weekly=sum(Spend_USD))

ggplot(dt.plot, aes(date, spend_weekly, color=region)) +
  geom_point() +
  xlab("Date") +
  ylab("Spending (in USD)") +
  scale_y_continuous(breaks= pretty_breaks()) +
  scale_x_date(labels = date_format("%m/%Y"), breaks = date_breaks("2 month")) +
  theme_bw()

##############################################################
#Targeting info
##############################################################
dt.target <-fread("/Desktop/website_dev/google_ads/data/google-political-ads-transparency-bundle/google-political-ads-campaign-targeting.csv")
dt.geo <-fread("/Desktop/website_dev/google_ads/data/google-political-ads-transparency-bundle/google-political-ads-geo-spend.csv")
dt.keywords <-fread("/Desktop/website_dev/google_ads/data/google-political-ads-transparency-bundle/google-political-ads-top-keywords-history.csv")

