# 
# Purpose: To do a first analysis at all the headwaters data
# Author: Jake Diamond
# Date: 18 January 2022
# 
# Load libraries
library(lubridate)
library(readxl)
library(tidytext)
library(tidyverse)
library(tidytable)

# Load DO/temp/conductivity data
sens <- readRDS(file.path("Data", "02_sensor_data", 
                          "all_sensor_data_clean_with_2022.RDS"))

# Read in all meteorological data
meteo <- readRDS(file.path("Data", "05_meteo", "meteo_data.RDS"))

# I think the baro pressure data for Yzeron is bad...use from airport for all
press <- filter(meteo, elev_ref == 0) %>%
  distinct(datetime, p_mbar)

meteo <- select(meteo, -p_mbar, -elev_ref) %>%
  left_join(press) %>%
  mutate(elev_ref = 0)

# Read in discharge data
q <- readRDS(file.path("data", "04_discharge and stage", "hourly_discharge_all.RDS")) %>%
  mutate(datetime = with_tz(datetime, "Europe/Berlin"))

# Load discharge meta data to be able to extrapolate discharge to all sites
q_meta <- read_xlsx(file.path("data", "01_metadata", "site_meta_data_all.xlsx")) %>%
  select(site, area_km2, siteq, area_q_km2)

# Get to hourly data to simplify
sens_hour <- sens %>%
  mutate(hour = floor_date(datetime, "hour")) %>%
  group_by(watershed, confluence, position, site, hour, type) %>%
  summarize(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  rename(datetime = hour) %>%
  ungroup() %>%
  pivot_wider(names_from = type, values_from = c(value, temp), names_sep = "_") %>%
  rename(DO = value_DO, cond = value_conductivity, light = value_light,
         DO_temp = temp_DO, cond_temp = temp_conductivity, light_temp = temp_light)

meteo_hour <- meteo %>%
  mutate(hour = floor_date(datetime, "hour")) %>%
  group_by(watershed, hour) %>%
  summarize(across(where(is.numeric) & !rain_mm, mean, na.rm = TRUE),
            rain_mm = sum(rain_mm, na.rm = T)) %>%
  rename(datetime = hour) %>%
  ungroup()

# Calculate %DO saturation and specific conductivity
df <- sens_hour %>%
  mutate(watershed = if_else(confluence == "vauxonne_sallarin", "vauxonne", watershed)) %>%
  left_join(meteo_hour) %>%
  ungroup() %>%
  mutate(tair_site = tair - 0.0098 * (altitude_m - elev_ref)) %>% #air temp. at site; adiabatic dry lapse rate
  mutate(press_e = p_mbar * exp((-0.03416262 / (tair_site + 273)) * 
                                  (altitude_m - elev_ref))) %>%   #pressure at site; -0.034 = -g*M/R) %>%  
  mutate(DOsat = if_else(is.na(press_e),
                         LakeMetabolizer::o2.at.sat.base(DO_temp, altitude = altitude_m),
                         LakeMetabolizer::o2.at.sat.base(DO_temp, press_e)), #theoretical DO saturation based on garcia-benson eqn
         DOsat2 = if_else(DO_temp == 0,
                          0,
                          14.652 - 0.41022 * DO_temp + 0.007991 *
                            DO_temp^2 - 0.000077774 * DO_temp^3), #if pressure data is missing, estimate with this equation
         DO_per = if_else(is.na(DOsat), # percentage saturation
                          DO * 100 / DOsat2,
                          DO * 100/ DOsat),
         alpha = 0.0192+8E-5*cond_temp, # alpha constant for spc based on logger specs
         spc = cond / (1 + alpha * (cond_temp - 25))) # calculate spc based on logger specs

# Save ardieres and yzeron data
saveRDS(df, file.path("data", "10_clean_data", "rhone_hourly_data_including2022.RDS"))

# Bring in some meta data
df2 <- readRDS(file.path("data", "10_clean_data", "hourly_data_2021.RDS")) %>%
  left_join(read_xlsx(file.path("data", "01_metadata", "site_meta_data_all.xlsx")) %>%
              select(site, site_code, strahler, area_km2))

# Load old Loire data
df_loire <- readRDS(file.path("data", "09_loire_data", "loire_headwaters_data.RDS")) %>%
  unnest() %>%
  mutate(site_code = tolower(site_code)) %>%
  rename(lux_water = lux) %>%
  left_join(read_xlsx(file.path("data", "01_metadata", "loire_headwaters_metadata.xlsx"))) %>%
  left_join(read_xlsx(file.path("data", "05_meteo", "hourly_radiation_loire.xlsx")) %>%
              select(datetime, rad = rad_jcm2)) %>%
  mutate(rad_wm2 = rad *100^2/3600, #J/h/cm2 -> W/m2,
         alpha = 0.0192+8E-5*DO_temp, # alpha constant for spc based on logger specs
         spc = cond / (1 + alpha * (DO_temp - 25))) %>% # calculate spc based on logger specs)
  drop_na(DO, DO_temp)

# Combine data
df_all <- bind_rows(df2, df_loire) %>%
  select(watershed, strahler, area_km2, latitude, longitude, altitude_m,
         number, confluence, position, site_code, site, datetime,
         DO, temp = DO_temp, DO_per, spc, rad_wm2, lux = lux_water, rain_mm, tair)

# Combine with discharge
df_all <- left_join(df_all, q_meta) %>%
  arrange(site, datetime) %>%
  mutate(year = year(datetime),
         date = date(datetime)) %>%
  mutate(siteq = if_else(site == "lignon aval poncins" & year == 2019,
                         "lignon_amont_boen",
                         siteq)) %>% #missing data for this year
  left_join(q)


# Calculate discharge at each site
df_all2 <- df_all %>%
  group_by(site, year) %>%
  mutate(q_int = imputeTS::na_kalman(q_m3s), #fill in gaps
         q_mmh = q_int * 3.6 / area_q_km2, # m3S to mm/h
         qsite_m3s = q_mmh *area_km2 / 3.6, # estimated discharge at each sensor
         q_mmd = q_mmh * 24) %>% # mm/h to mm/d
  ungroup()

# Determine if the sensor was out of water
df_oow <- df_all2 %>%
  mutate(date = date(datetime)) %>%
  group_by(site_code, date) %>%
  summarize(do_mean = quantile(DO_per, 0.1, na.rm = TRUE),
            do_amp = max(DO_per, na.rm = T) - min(DO_per, na.rm = T),
            wat_t = max(temp, na.rm = T) - min(temp, na.rm = T),
            air_t = max(tair, na.rm = T) - min(tair, na.rm = T),
            q_mmd = mean(q_mmd, na.rm = TRUE)) %>%
  mutate(oow = if_else((do_mean > 90 & wat_t > 8 & (q_mmd < 0.05 | is.na(q_mmd)) & 
                          between(month(date), 7, 10)) |
                         (do_mean > 90 & do_amp < 5 & (q_mmd < 0.05 | is.na(q_mmd)) & 
                            between(month(date), 7, 10)) |
                         (do_mean > 90 & wat_t >= air_t & (q_mmd < 0.05 | is.na(q_mmd)) & 
                            between(month(date), 7, 10)) |
                         (site_code == "rie009" & between(date, ymd("20200416"), ymd("20200515"))), 
                       "yes", "no"))

# Include oow data into data frame
df_final <- df_all2 %>%
  mutate(month = month(datetime)) %>%
  left_join(select(df_oow, site_code, date, oow)) %>%
  mutate(oow = if_else(!is.na(spc) & DO_per > 95 & spc < 50, 
                                "yes",
                                oow))

# Save
saveRDS(df_final, file.path("data","10_clean_data", "hourly_data_all_including2022.RDS"))

# Get a dataframe of daily discharge data
df_q <- df2 %>%
  mutate(date = date(datetime)) %>%
  select(-q_mmh) %>%
  distinct(siteq, q_mmd, date) %>%
  group_by(siteq, date) %>%
  summarize(across(where(is.numeric), mean, na.rm = T)) %>% # summarize hourly to daily
  bind_rows(mutate(df_loire, date = date(datetime), #add loire discharge
                   watershed = case_when(site_code == "lig664" ~ "lignon aval poncins", 
                                         site_code == "lig426" ~ "lignon amont boen",
                                         site_code == "viz062" ~ "vizezy poncins",
                                         site_code == "viz221" ~ "vizezy poncins",
                                         TRUE ~ watershed)) %>%
              select(siteq =watershed, date, q_mmd) %>%
              distinct() %>%
              group_by(siteq, date) %>%
              summarize(across(where(is.numeric), mean, na.rm = T))) %>%
  filter(siteq != "Loise") #doubled from Coise

# Save this data
saveRDS(df_q, file.path("data","10_clean_data", "daily_discharge.RDS"))
