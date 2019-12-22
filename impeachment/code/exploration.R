# Impeachment contribution effect exploration
library(data.table); library(tidyverse); library(rgdal); library(sf); library(viridis)

#set directories
in_dir <- paste0("~/Desktop/impeachment/data/")
out_dir <- paste0("~/Desktop/impeachment/output/")

# read in contribution data
#main presidential pac-https://www.fec.gov/data/
df <- fread(paste0(in_dir, "schedule_a-2019-11-09T14_41_20.csv"))
df2 <- fread(paste0(in_dir, "schedule_a-2019-11-11T10_07_39.csv"))
df <- bind_rows(df, df2)
df <- df %>% as_tibble(.name_repair = "unique")

# contribution distribution
df_zip <- df %>%
  filter(contribution_receipt_amount>0) %>%
  mutate(contributor_zip=as.character(str_pad(contributor_zip, 5, pad = "0"))) %>%
  group_by(contributor_zip) %>%
  summarise(contri_amount=sum(contribution_receipt_amount)) %>%
  mutate(contri_amount_log = log(contri_amount))

# Import US zipcode data
#https://www.arcgis.com/home/item.html?id=8d2012a2016e484dafaac0451f9aea24
us_zips <- readOGR(dsn = "~/Desktop/impeachment/data/v10/zip_poly.gdb")
us_zips <- st_as_sf(us_zips)
states <- c("GA", "AL", "MS", "LA", "TX")

#######################################################################################
####map of a few states
#######################################################################################
pdf(paste0(out_dir, "state_plots_log.pdf"))
us_zips %>%
  filter(STATE %in% states) %>%
  mutate(ZIP_CODE=as.character(ZIP_CODE)) %>%
  left_join(df_zip, by=c("ZIP_CODE"="contributor_zip")) %>%
  ggplot() +
    geom_sf(aes(fill=contri_amount_log)) +
    theme_bw()

dev.off()

#######################################################################################
######country plot
#######################################################################################
states <- unique(us_zips$STATE)
pdf(paste0(out_dir, "country_plot_log.pdf"))
us_zips %>%
  filter(STATE %in% setdiff(states, c("AK", "HI", "GU", "PR", "VI"))) %>%
  mutate(ZIP_CODE=as.character(ZIP_CODE)) %>%
  left_join(df_zip, by=c("ZIP_CODE"="contributor_zip")) %>%
  ggplot() +
  geom_sf(aes(fill=contri_amount_log, color=contri_amount_log)) +
  theme_bw()

dev.off()
#######################################################################################
######country plot absolute change
#######################################################################################
#increase since impeachment inquiry began appears to be robust-remains when using different weeks as the
#baseline contribution.

#dates percent change
startDate1 <- as.Date("2019-09-01")
endDate1 <- as.Date("2019-09-14")
startDate2 <- as.Date("2019-09-15")
endDate2 <- as.Date("2019-09-30")

#tabulate total contributions from pre-impeachment
df_base <- df %>%
  mutate(date = as.Date(contribution_receipt_date)) %>%
  filter(contribution_receipt_amount>0 & date >= startDate1 & date <= endDate1) %>%
  mutate(contributor_zip=as.character(str_pad(contributor_zip, 5, pad = "0"))) %>%
  group_by(contributor_zip) %>%
  summarise(contri_amount1=sum(contribution_receipt_amount))

#tabulate total contributions post-impeachment and calculate absolute change
df_abs_change <- df %>%
  mutate(date = as.Date(contribution_receipt_date)) %>%
  filter(contribution_receipt_amount>0 & date >= startDate2 & date <= endDate2) %>% 
  mutate(contributor_zip=as.character(str_pad(contributor_zip, 5, pad = "0"))) %>%
  group_by(contributor_zip) %>%
  summarise(contri_amount2=sum(contribution_receipt_amount)) %>%
  left_join(df_base, by="contributor_zip") %>%
  mutate(contri_amount1=if_else(is.na(contri_amount1), 0, contri_amount1),
         change=contri_amount2-contri_amount1)

#map the abs change
labels <- c("$1000+ increase", "$500-1000 increase", "$100-500 increase", "$0-100 increase", 
            "$0-100 decrease", "$100-500 decrease", "$500-1000 decrease", "$1000+ decrease")
options(scipen = 999)
colors <- c("#d73027", "#f46d43", "#fdae61", "#fee090", "#e0f3f8", "#abd9e9", "#74add1", "#4575b4")
pdf(paste0(out_dir, "country_plot_abs_change_colors_battleground_changePercap.pdf"))
states_battleground <- c("MI", "PA", "WI", "FL", "NC", "AZ")
for(state in states_battleground) {
  g <- us_zips %>%
    filter(STATE %in% state) %>%
    mutate(ZIP_CODE=as.character(ZIP_CODE)) %>%
    left_join(df_abs_change, by=c("ZIP_CODE"="contributor_zip")) %>%
    mutate(
      change_cat = case_when(
        change > 1000 ~ "$1000+ increase",
        change >= 500 ~ "$500-1000 increase",
        change >= 100 ~ "$100-500 increase",
        change >= 0 ~ "$0-100 increase",
        change >= -100 ~ "$0-100 decrease",
        change >= -500 ~ "$100-500 decrease",
        change >= -1000 ~ "$500-1000 decrease",
        change < -1000 ~ "$1000+ decrease"
      ),
      Change=factor(change_cat, levels = labels),
      change_percap=(change/POPULATION)*1000
    ) %>%
    ggplot() +
    geom_sf(aes(fill=change_percap, color=change_percap)) +
    coord_sf(datum = NA) +
    ggtitle(state) +
    scale_fill_viridis(option="plasma") +
    scale_color_viridis(option="plasma") +
    #scale_fill_manual(values=colors, na.value="#dfdfdf") +
    #scale_color_manual(values=colors, na.value="#dfdfdf") +
    theme_bw()
  print(state)
  
  print(g)
}
dev.off()
####

#######################################################################################
######counts of zip codes by absolute change
#######################################################################################
df_abs_change %>%
  mutate(
    change_cat = case_when(
      change > 1000 ~ "$1000+ increase",
      change >= 500 ~ "$500-1000 increase",
      change >= 100 ~ "$100-500 increase",
      change >= 0 ~ "$0-100 increase",
      change >= -100 ~ "$0-100 decrease",
      change >= -500 ~ "$100-500 decrease",
      change >= -1000 ~ "$500-1000 decrease",
      TRUE~ "$1000+ decrease"
    ),
    change_cat=factor(change_cat, levels = labels)
  ) %>%
  count(change_cat)


dol1 <- 2500
dol2 <- 3600
pop1 <- 22
(dol2/pop1)-(dol1/pop1)
(dol2-dol1)/pop1