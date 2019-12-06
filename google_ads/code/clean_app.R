#clean and process data for Shiny App
library(tidyverse); library(data.table); library(lubridate)

#find date when data was downloaded last
data_dir <- "../data/"
#if(interactive()) data_dir <- "google_ads/data/"
dwnld_date <- file.info(paste0(data_dir, "google-political-ads-transparency-bundle/google-political-ads-advertiser-weekly-spend.csv"))$mtime %>% as.Date()
dwnld_month <- lubridate::month(dwnld_date, label = T, abbr=F)

#if not downloaded in past 7 days, re-download
if(dwnld_date < Sys.Date() - 7){
  #set url and file destination
  url <- "https://storage.googleapis.com/transparencyreport/google-political-ads-transparency-bundle.zip"
  dest <- paste0(data_dir, "google_ads.zip")
  unzip_dir <- paste0(data_dir)
  
  #download ads data and save to central location
  download.file(url=url, destfile = dest, method = "curl")
  unzip(dest, exdir = unzip_dir)
}

#read in data
dt <- fread(paste0(data_dir, "google-political-ads-transparency-bundle/google-political-ads-creative-stats.csv"))

#relevant variables
vars_keep <- c("Ad_Type", "Regions", "Advertiser_ID", "Advertiser_Name", "Date_Range_Start", "Date_Range_End", "Num_of_Days", 
               "Impressions", "Spend_Range_Min_USD", "Spend_Range_Max_USD")

#subset to variables of interest and create useful variables
dt <- dt %>%
  select(vars_keep) %>% #keep relevant variables
  mutate(startYear = substr(Date_Range_Start,1,4), #variable indicating year ad started
         regions = ifelse(grepl("EU", Regions), "EU", "US"), #create cleaner regions variable
         month_year = format(as.Date(Date_Range_Start), "%m-%Y"),
         week_start = floor_date(as.Date(Date_Range_Start), unit = "week"),
         pricing_cat = paste0(Spend_Range_Min_USD, "-", Spend_Range_Max_USD)) %>%
  filter(regions=="US") #only include US data

#Create cleaner labels
dt <- dt %>%
  mutate(pricing_cat=factor(pricing_cat, 
                            levels = c("0-100", "100-1000", "1000-50000", "50000-100000", "100000-NA"),
                            labels = c("0-100", "100-1k", "1k-50k", "50k-100k", "100k +")))
dt <- dt %>%
  mutate(Impressions = factor(Impressions, levels = c("≤ 10k", "10k-100k", "100k-1M", "1M-10M", "> 10M"),
                              labels = c("Under 10k", "10k-100k", "100k-1M", "1M-10M", "10M+")))

dt <- dt %>% 
  mutate(Days = cut(Num_of_Days, breaks=c(0,10,20,50,420), 
                    labels = c("1-10", "11-20", "21-50", "50+")))
#save
write.csv(dt, "shiny/shiny_data.csv", row.names = F)
