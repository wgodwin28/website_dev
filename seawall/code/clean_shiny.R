#clean and process data for Shiny App
library(tidyverse); library(lubridate); library(openxlsx); library(data.table)

#filepaths and filenames
dir <- here::here()
cost_datapath   <- paste0(dir,"/seawall/data/Climate_Costs_2040_Data/Con Dist Cost Summary.xlsx")
demo_filepath   <- paste0(dir,"/seawall/data/demographics/ACS_17_5YR_DP03_with_ann.csv")
pop_filepath    <- paste0(dir,"/seawall/data/population/ACS_17_5YR_DP05_with_ann.csv")
pop_filepath    <- paste0(dir,"/seawall/data/population/ACS_17_5YR_DP05_with_ann.csv")

# variables to include in app
# HC03_VC161 = Percent; PERCENTAGE OF FAMILIES AND PEOPLE WHOSE INCOME IN THE PAST 12 MONTHS IS BELOW THE POVERTY LEVEL - All families
# HC03_VC07 = Percent; EMPLOYMENT STATUS - Population 16 years and over - In labor force - Civilian labor force - Unemployed
# HC03_VC09 = Percent; EMPLOYMENT STATUS - Population 16 years and over - Not in labor force
# HC03_VC45 = Percent; OCCUPATION - Civilian employed population 16 years and over - Production, transportation, and material moving occupations
# HC03_VC44 = Percent; OCCUPATION - Civilian employed population 16 years and over - Natural resources, construction, and maintenance occupations
# HC03_VC50 = Percent; INDUSTRY - Civilian employed population 16 years and over - Agriculture, forestry, fishing and hunting, and mining
# HC03_VC51 = Percent; INDUSTRY - Civilian employed population 16 years and over - Construction
# HC03_VC52 = Percent; INDUSTRY - Civilian employed population 16 years and over - Manufacturing
# HC01_VC85 = Estimate; INCOME AND BENEFITS (IN 2017 INFLATION-ADJUSTED DOLLARS) - Total households - Median household income (dollars)
# HC03_VC131= Percent; HEALTH INSURANCE COVERAGE - Civilian noninstitutionalized population - With health insurance coverage
# HC03_VC132= Percent; HEALTH INSURANCE COVERAGE - Civilian noninstitutionalized population - With health insurance coverage - With private health insurance
codes <- c("HC03_VC161", "HC03_VC07", "HC03_VC09", "HC03_VC45", "HC03_VC44", "HC03_VC50", "HC03_VC51", "HC03_VC52", "HC01_VC85", "HC03_VC131", "HC03_VC132")
titles <- c("Poverty", "Unemployed_LF", "Not_in_LF", "Transportation", "Construction", "Agriculture_fishing", "Construction_all", "Manufacturing_all", "Median_HH_income", "Health_insurance_cov", "Health_insurance_priv")

#####################################################
#data prep###########################################
#####################################################
##########
# Wrangle the cost data
# find sheets that hold data of interest
state_vec <- getSheetNames(cost_datapath)
state_vec <- state_vec[nchar(state_vec)==2] #where the number of characters is 2

# read in all sheets
df <- bind_rows(
  lapply(state_vec, function(x){
    # read in data
    read.xlsx(cost_datapath, sheet = x) %>% 
      mutate(state=x) %>% #generate state variable
      filter(X1=="RCP 4.5, 2040, 50th%, 1 year return")
  })
)

# reshape the data to long format
df <- pivot_longer(df, cols= contains("district"), names_to = "district", values_drop_na = T) %>%
  mutate(district=substr(district, 24, 26)) %>%
  select(state, district, seawall_cost=value)
######

# state postal codes for merging data onto shapefile
states <- cbind(state_abr=state.abb, STATENAME=state.name, region=as.character(state.region)) %>% as.data.frame()

######
# variables to keep from 2017 demographic and health survey 
keep_vars <- c("HC03_VC161", "HC03_VC07", "HC03_VC09", 
               "HC03_VC45", "HC03_VC44", "HC03_VC50", 
               "HC03_VC51", "HC03_VC52", "HC01_VC85", 
               "HC03_VC131", "HC03_VC132")

# demographic/economic data: https://factfinder.census.gov/faces/tableservices/
df_pov <- read_csv(demo_filepath) %>%
  select(contains("GEO"), keep_vars, geo_main="GEO.display-label") %>%
  mutate(district=as.character(substr(geo_main, 24, 25)),
         district=str_trim(district, side = "right"),
         STATENAME=sub('.*,\\s*','', geo_main)) %>%
  mutate_at(vars(contains("HC")), as.numeric, as.character) %>%
  left_join(states, by=c("STATENAME")) %>%
  filter(!is.na(district)) %>%
  select(district, Region=region, state=state_abr, keep_vars)
#####

# combine with seawall data
df_app <- left_join(df, df_pov, by=c("district", "state"))

#####
# read in population to generate per capita cost
df_pop <- read_csv(pop_filepath) %>%
  select(contains("GEO"), pop=HC01_VC03, geo_main="GEO.display-label") %>%
  mutate(district=as.character(substr(geo_main, 24, 25)),
         district=str_trim(district, side = "right"),
         STATENAME=sub('.*,\\s*','', geo_main)) %>%
  left_join(states, by=c("STATENAME")) %>%
  filter(!is.na(state_abr)) %>%
  select(pop, district, state=state_abr)

# combine with seawall data and standardize by population
df_app <- left_join(df_app, df_pop, by=c("district", "state")) %>%
  mutate(pop=as.numeric(as.character(pop)),
         seawall_cost_percap=seawall_cost/pop,
         seawall_cost_percap_cat=cut(seawall_cost_percap, breaks = c(0,350,1500,4000,10000,40000), 
                              labels = c("0-350", "350-1,500", "1,500-4,000", "4,000-10,000", "10,000+"))) %>%
  filter(!is.na(seawall_cost_percap))
#####

#not sure how to change column names as vectors
df_app <- setnames(as.data.table(df_app), codes, titles)

# save for app use
write_csv(df_app, paste0(dir, "/seawall/code/shiny/app_data.csv"))
