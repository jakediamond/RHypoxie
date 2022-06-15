# 
# Purpose: To load and clean the meteo data 
# Author: Jake Diamond
# Date: 01 February 2022
# 

# Load libraries
library(lubridate)
library(readxl)
library(tidyverse)

# Load data
# Read in meteo data for yzeron (pressure data is local at 803 m)
meteo_yz <- read_xlsx(file.path("Data", "05_meteo", "raw", "bdoh", "yzeron_meteo.xlsx"), 
                      skip = 1,
                      col_names = c("datetime", "rain_mm", "p_hpa", "rad_wm2"),
                      col_types = c("date", rep("numeric", 3))) %>%
  mutate(mins = floor_date(datetime, "15 minutes")) %>% # aggregate to every 15 minutes
  group_by(mins) %>%
  summarize(rain_mm = sum(rain_mm, na.rm = T),
            p_mbar = mean(p_hpa, na.rm = T),
            rad_wm2 = mean(rad_wm2, na.rm = T)) %>%
  ungroup() %>%
  mutate(p_mbar = imputeTS::na_kalman(p_mbar), #interpolate missing values with kalman smoothing
         rad_wm2 = if_else((is.na(rad_wm2) | hour(mins) == 0), 0, rad_wm2),
         mins = with_tz(mins, "Europe/Berlin"),
         watershed = "yzeron") %>%
  rename(datetime = mins)

# Read Yzeron air temperature and use regression with yzeron amont miloniere to fill gaps
air_yz <- read_xlsx(file.path("Data", "05_meteo", "raw", "bdoh", "yzeron_tair.xlsx"), 
                    skip = 2) %>%
  mutate(datetime = floor_date(DateHeure, "15 minutes")) %>%
  group_by(datetime) %>%
  summarize(tair = mean(Valeur, na.rm = T)) %>%
  right_join(distinct(meteo_yz, datetime)) %>%
  right_join(filter(readRDS(
    file.path("Data", "02_sensor_data", "all_sensor_data_clean_wide.RDS")), 
    site == "yzeron amont miloniere") %>% 
      select(datetime, tw = DO_temp)) %>%
  mutate(tair = if_else(is.na(tair), tw *1.902 -11.323, tair),
         datetime = with_tz(datetime, "Europe/Berlin"),
         watershed = "yzeron")

# Read in bdoh precipitation data for ardieres (tipping bucket data)
ppt_ar <- read_xlsx(file.path("Data", "05_meteo", "raw", "bdoh", "ardieres_precip.xlsx"),
                    skip = 3,
                    col_names = c("datetime", "rain_mm", "quality")) %>%
  mutate(datetime = floor_date(datetime, "1 minutes")) %>%
  complete(datetime = seq.POSIXt(min(datetime), max(datetime), by="min")) %>%
  mutate(mins = floor_date(datetime, "1 hour")) %>% # aggregate to every 1 hour
  group_by(mins) %>%
  summarize(rain_mm = sum(rain_mm, na.rm = T)) %>%
  ungroup() %>%
  mutate(watershed = "ardieres",
         mins = with_tz(mins, "Europe/Berlin")) %>%
  rename(datetime = mins) %>%
  semi_join(select(meteo_yz, datetime))

# Read in hourly MeteoFrance data for ardieres (pressure data is sea level)
meteo_ar <- read_xlsx(file.path("Data", "05_meteo", "raw", "MeteoFrance", "ardieres_meteo.xlsx"), 
                      skip = 1,
                      col_names = c("datetime", "p_mbar", "rad_wm2", "rain_mm", "tair"),
                      col_types = c("text", rep("numeric", 4))) %>%
  mutate(datetime = ymd_h(datetime),
         datetime = with_tz(datetime, "Europe/Berlin"),
         watershed = "ardieres")

# Actually just use the rainfall data (from Villefranche) for the Vauxonne catchment
meteo_vaux <- mutate(meteo_ar, watershed = "vauxonne")
meteo_ar <- select(meteo_ar, -rain_mm) %>%
  left_join(ppt_ar)

# Get data together
df <- meteo_yz %>%
  left_join(select(air_yz, datetime, tair)) %>%
  bind_rows(meteo_ar) %>%
  bind_rows(meteo_vaux) %>%
  mutate(elev_ref = case_when(watershed == "yzeron" ~ 803, #elevation of 803 m asl for yzeron meteo site
                              watershed == "ardieres" ~ 0, #sea level for ardieres and vauxonne
                              watershed == "vauxonne" ~ 0,
                              TRUE ~ NA_real_)) 

# Save data
saveRDS(df, "Data/05_meteo/meteo_data.RDS")

# Save as xlsx with sheets
# Split one data frame per site
df %>%
  group_split(watershed) -> list_of_dfs

# name of each sheet will be the site
list_of_dfs %>%
  map(~pull(., watershed)) %>%
  map(~unique(.)) -> names(list_of_dfs)

list_of_dfs %>%
  writexl::write_xlsx(path = "Data/05_meteo/meteo_data.xlsx")
