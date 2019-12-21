#https://www.kaggle.com/rtatman/188-million-us-wildfires/downloads/188-million-us-wildfires.zip/1
library(RSQLite)
library(dplyr)
db <- dbConnect(SQLite(), dbname=paste0(here::here(),"/fires/data/FPA_FOD_20170508.sqlite"))
dbListTables(db)
fires <- dbGetQuery(db, "SELECT * FROM Fires")

#Have wildfires become more or less frequent over time?
#What counties are the most and least fire-prone?
#Given the size, location and date, can you predict the cause of a fire wildfire?

inner_join(
  burned_1990 <- fires %>%
    group_by(STATE) %>%
    summarise(fire_1990=log(sum(FIRE_SIZE))),
  burned_2011 <- fires %>%
    filter(FIRE_YEAR>2010) %>%
    group_by(STATE) %>%
    summarise(fire_2011=log(sum(FIRE_SIZE))),
  by="STATE"
) %>% write.csv(file = paste0(here::here(),"/fires/data/log_burn_state.csv"), row.names = F)