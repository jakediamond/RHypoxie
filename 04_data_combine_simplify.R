# 
# Purpose: To do a first analysis at all the headwaters data
# Author: Jake Diamond
# Date: 18 January 2022
# 

# Set working directory
setwd("Z:/RHypoxie")
# setwd("C:/Users/diamo/Dropbox/Projects/RHypoxie")

# Load libraries
library(lubridate)
library(readxl)
library(scales)
library(tidytext)
library(tidyverse)

# Load DO/temp/conductivity data
df <- readRDS("Data/02_sensor_data/all_sensor_data_clean_wide.RDS")

# Read in all meteorological data
meteo <- readRDS("Data/05_meteo/meteo_data.RDS")

# Read in discharge data
df_q <- readRDS("Data/04_discharge and stage/all_discharge_data_15min.RDS") %>%
  mutate(datetime = with_tz(datetime, "Europe/Berlin"))

# Load discharge meta data to be able to extrapolate discharge to all sites
df_q_meta <- read_xlsx("Data/01_metadata/site_area.xlsx")

# Calculate %DO saturation and specific conductivity
df <- df %>%
  left_join(meteo) %>%
  mutate(tair_site = tair - 0.0098 * (altitude_m - elev_ref)) %>% #air temp. at site; adiabatic dry lapse rate
  mutate(press_e = p_mbar * exp((-0.03416262 / (tair_site + 273)) * 
                                  (altitude_m - elev_ref))) %>%  #pressure at site; -0.034 = -g*M/R
  mutate(DOsat = streamMetabolizer::calc_DO_sat(DO_temp, press_e), #theoretical DO saturation based on garcia-benson eqn
         DOsat2 = if_else(DO_temp == 0,
                          0,
                          14.652 - 0.41022 * DO_temp + 0.007991 *
                            DO_temp^2 - 0.000077774 * DO_temp^3), #if pressure data is missing, estimate with this equation
         DO_per = if_else(is.na(DOsat), # percentage saturation
                          DO * 100 / DOsat2,
                          DO * 100/ DOsat),
         alpha = 0.0192+8E-5*cond_temp, # alpha constant for spc based on logger specs
         spc = cond / (1 + alpha * (cond_temp - 25))) # calculate spc based on logger specs

# Combine with discharge
df <- left_join(df, df_q_meta) %>%
  left_join(df_q)

# Get to hourly data to simplify
df_hour <- df %>%
  mutate(hour = floor_date(datetime, "hour")) %>%
  group_by(watershed, confluence, position, site, hour) %>%
  summarize(across(where(is.numeric) & !rain_mm, mean, na.rm = TRUE),
            rain_mm = sum(rain_mm, na.rm = T)) %>%
  rename(datetime = hour) %>%
  ungroup()

# Calculate discharge at each site
df2 <- df_hour %>%
  mutate(q_mmh = q_m3s * 3.6 / area_q_km2,
         qsite_m3s = q_mmh *area_km2 / 3.6)

# Save ardieres and yzeron data
saveRDS(df2, "Data/hourly_data_2021.RDS")

# Save ardieres and yzeron data
df2 <- readRDS("Data/hourly_data_2021.RDS") %>%
  left_join(read_xlsx("Z:/RHypoxie/Data/01_metadata/site_area.xlsx"))

# Load old Loire data
df_loire <- readRDS("Z:/RHypoxie/Data/headwaters_data_part2") %>%
  unnest() %>%
  mutate(site_code = tolower(site_code)) %>%
  left_join(read_xlsx("Z:/RHypoxie/Data/01_metadata/loire_headwaters_metadata.xlsx"))

# Combine data
df <- bind_rows(df2, df_loire)

# Save
saveRDS(df, "Data/hourly_data_all.RDS")

# Load daily discharge data
df_q <- df2 %>%
  mutate(date = date(datetime),
         q_mmd = q_mmh * 24) %>%
  select(-q_mmh) %>%
  distinct(siteq, q_mmd, date) %>%
  group_by(siteq, date) %>%
  summarize(across(where(is.numeric), mean, na.rm = T)) %>%
  bind_rows(mutate(df_loire, date = date(datetime),
                   watershed = case_when(site_code == "lig664" ~ "lignon aval poncins", 
                                         site_code == "lig426" ~ "lignon amont boen",
                                         site_code == "viz062" ~ "vizezy poncins",
                                         site_code == "viz221" ~ "vizezy poncins",
                                         TRUE ~ watershed)) %>%
              select(siteq =watershed, date, q_mmd) %>%
              distinct) %>%
  filter(siteq != "Loise") #doubled from Coise
distinct(df_q, site_code)
