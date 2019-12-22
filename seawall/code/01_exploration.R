##########################################################################################################
#setup####################################################################################################
##########################################################################################################
#data downloaded here: https://www.climatecosts2040.org/data-sets
library(tidyverse); library(openxlsx); library(sf); library(biscale); library(cowplot); library(gridGraphics)

#filepaths and filenames
cost_datapath <- paste0(here::here(),"/seawall/data/Climate_Costs_2040_Data/Con Dist Cost Summary.xlsx")
shape_filepath <- paste0(here::here(),"/google_ads/data/districtShapes/districts114.shp")
demo_filepath <- paste0(here::here(),"/seawall/data/demographics/ACS_17_5YR_DP03_with_ann.csv")
pop_filepath <- paste0(here::here(),"/seawall/data/population/ACS_17_5YR_DP05_with_ann.csv")
out_dir <- paste0(here::here(),"/seawall/output/")

#find sheets that hold data of interest
state_vec <- getSheetNames(cost_datapath)
state_vec <- state_vec[nchar(state_vec)==2] #where the number of characters is 2

#read in all sheets
df <- bind_rows(
  lapply(state_vec, function(x){
    #read in data
    read.xlsx(cost_datapath, sheet = x) %>% 
      mutate(state=x) %>% #generate state variable
      filter(X1=="RCP 4.5, 2040, 50th%, 1 year return")
  })
)

#reshape the data to long format
df <- pivot_longer(df, cols= contains("district"), names_to = "district", values_drop_na = T) %>%
  mutate(district=substr(district, 24, 26)) %>%
  select(state, district, value)
  
#state postal codes for merging data onto shapefile
states <- cbind(state_abr=state.abb, STATENAME=state.name) %>% as.data.frame()

#read in shapefile of congressional districts
shape <- read_sf(shape_filepath)
  
#subset and merge with data
shape_data <- shape %>%
  left_join(states, by=c("STATENAME")) %>% 
  filter(state_abr %in% state_vec) %>%
  select(district=DISTRICT, state=state_abr) %>%
  left_join(df, by=c("district", "state"))

##########################################################################################################
#cost maps################################################################################################
##########################################################################################################
#subset and plot
shape_data %>%
  filter(state=="MA") %>%
  mutate(value=value/10^6) %>%
  ggplot() + 
  geom_sf(aes(fill=value), color="gray30") +
  scale_fill_viridis_c(name="USD in millions") +
  ggtitle("MA")

#loop through each state and plot


#plot for US
pdf(paste0(out_dir, "us_cost_2040_wsurge.pdf"))
shape_data %>%
  mutate(value=value/10^6) %>%
  ggplot() + 
    geom_sf(aes(fill=value), color="gray30") +
    scale_fill_viridis_c(name="USD in millions") +
    ggtitle("US Seawall Cost-2040")
dev.off()


##########################################################################################################
#poverty maps#############################################################################################
##########################################################################################################
#poverty data: https://factfinder.census.gov/faces/tableservices/
df_pov <- read.csv(demo_filepath) %>%
  select(contains("GEO"), poverty=HC03_VC161) %>% #percent of families/people whose income below poverty in past 12 months-2017
  mutate(district=as.character(substr(GEO.display.label, 24, 25)),
         district=str_trim(district, side = "right"),
         STATENAME=sub('.*,\\s*','', GEO.display.label),
         poverty=as.numeric(as.character(poverty))) %>%
  left_join(states, by=c("STATENAME")) %>%
  filter(!is.na(district)) %>%
  select(poverty, district, state=state_abr)

#combine with seawall data
shape_data <- left_join(shape_data, df_pov, by=c("district", "state"))

#merge in population to generate per capita cost
df_pop <- read.csv(pop_filepath) %>%
  select(contains("GEO"), pop=HC01_VC03) %>%
  mutate(district=as.character(substr(GEO.display.label, 24, 25)),
         district=str_trim(district, side = "right"),
         STATENAME=sub('.*,\\s*','', GEO.display.label)) %>%
  left_join(states, by=c("STATENAME")) %>%
  filter(!is.na(state_abr)) %>%
  select(pop, district, state=state_abr)

#combine with seawall data and standardize by population
shape_data <- left_join(shape_data, df_pop, by=c("district", "state")) %>%
  mutate(pop=as.numeric(as.character(pop)),
         value_percap=value/pop) %>%
  filter(!is.na(value_percap) & !is.na(poverty)) #some values are missing for some reason-messes up the bivariate classes

#create classes
shape_data <- bi_class(shape_data, x = value_percap, y = poverty, style = "quantile", dim = 3)

#bivariate plot
pdf(paste0(out_dir, "us_cost_2040_bivariate.pdf"))
map <- ggplot() +
  geom_sf(data = shape_data, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = F) +
  bi_scale_fill(pal = "DkBlue", dim = 3) +
  labs(title = "Seawall cost and Poverty in U.S.") +
  bi_theme()

legend <- bi_legend(pal = "DkBlue",
          dim = 3,
          xlab = "Higher seawall costs ",
          ylab = "Higher poverty ",
          size = 8) 

ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, x=0.35, y=.45, 0.2, 0.2)
dev.off()