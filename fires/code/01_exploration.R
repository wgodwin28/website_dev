#https://www.kaggle.com/rtatman/188-million-us-wildfires/downloads/188-million-us-wildfires.zip/1
library(RSQLite)
db <- dbConnect(SQLite(), dbname="~/Desktop/fires/data/FPA_FOD_20170508.sqlite")
dbListTables(db)
fires <- dbGetQuery(db, "SELECT * FROM Fires")

#Have wildfires become more or less frequent over time?
#What counties are the most and least fire-prone?
#Given the size, location and date, can you predict the cause of a fire wildfire?