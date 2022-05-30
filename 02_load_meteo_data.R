# 
# Purpose: To load and clean the meteo data 
# Author: Jake Diamond
# Date: 01 February 2022
# 

# Set working directory
setwd("Z:/RHypoxie")
# setwd("C:/Users/jake.diamond/Dropbox/Projects/RHypoxie")

# Load libraries
library(lubridate)
library(readxl)
library(tidyverse)

# Load data
# Read in meteo data for yzeron (pressure data is sea level pressure)
meteo_yz <- read_xlsx("Data/05_meteo/raw/bdoh/yzeron_meteo.xlsx", skip = 1,
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

# Read in precipitation data for ardieres
ppt_ar <- read_xlsx("Data/05_meteo/raw/bdoh/ardieres_precip.xlsx", skip = 3,
                    col_names = c("datetime", "rain_mm", "quality")) %>%
  mutate(datetime = floor_date(datetime, "1 minutes")) %>%
  complete(datetime = seq.POSIXt(min(datetime), max(datetime), by="min")) %>%
  mutate(mins = floor_date(datetime, "15 minutes")) %>% # aggregate to every 15 minutes
  group_by(mins) %>%
  summarize(rain_mm = sum(rain_mm, na.rm = T)) %>%
  ungroup() %>%
  mutate(watershed = "ardieres",
         mins = with_tz(mins, "Europe/Berlin")) %>%
  rename(datetime = mins) %>%
  semi_join(select(meteo_yz, datetime))

# Read air temperature and use regression with yzeron amont miloniere to fill gaps
air_yz <- read_xlsx("Data/05_meteo/raw/bdoh/yzeron_tair.xlsx", skip = 2) %>%
  mutate(datetime = floor_date(DateHeure, "15 minutes")) %>%
  group_by(datetime) %>%
  summarize(tair = mean(Valeur, na.rm = T)) %>%
  right_join(distinct(meteo_yz, datetime)) %>%
  right_join(filter(readRDS("Data/02_sensor_data/all_sensor_data_clean_wide.RDS"), 
                    site == "yzeron amont miloniere") %>% 
               select(datetime, tw = DO_temp)) %>%
  mutate(tair = if_else(is.na(tair), tw *1.902 -11.323, tair),
         datetime = with_tz(datetime, "Europe/Berlin"),
         watershed = "yzeron")

# Get data together
df <- meteo_yz %>%
  left_join(select(air_yz, datetime, tair)) %>%
  bind_rows(ppt_ar) %>%
  mutate(elev_ref = if_else(watershed == "yzeron", 803, NA_real_)) #elevation of 803 m asl for yzeron meteo site

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