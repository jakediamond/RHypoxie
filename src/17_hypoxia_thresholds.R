# 
# Purpose: To do a first analysis of hypoxia on data from 2019-2021
# Author: Jake Diamond
# Date: 18 January 2022
# 

# Load libraries
library(lubridate)
# library(readxl)
# library(scales)
# library(patchwork)
# library(tidytext)
library(tidyverse)

# Load data
df <- readRDS(file.path("data", "10_clean_data", "hourly_data_all.RDS"))

df %>%
  group_by(watershed) %>%
  summarize(min = min(area_km2),
            max = max(area_km2))

df %>%
  group_by(watershed) %>%
  summarize(min = min(altitude_m),
            max = max(altitude_m))

df %>%
  group_by(watershed) %>%
  distinct(site) %>%
  summarize(n  = n())

a=df %>%
  distinct(site, watershed, strahler) %>%
  group_by(watershed, strahler) %>%
  summarize(n  = n())

df %>%
  group_by(watershed) %>%
  summarize(mind = min(date),
            maxd = max(date))


df %>%
  group_by(watershed, year) %>%
  distinct(date, tair, rain_mm) %>%
  summarize(meant = mean(tair, na.rm = T),
            mint = min(tair, na.rm = T),
            maxt = max(tair, na.rm = T),
            sumrain = sum(rain_mm))

y = df %>%
  group_by(watershed) %>%
  filter(area_km2 == max(area_km2, na.rm = T)) %>%
  summarize(medq = median(qsite_m3s, na.rm = T))

x = readxl::read_xlsx(file.path("data", "03_field measurements", "field_msmts.xlsx")) %>%
  left_join(distinct(df, site, watershed)) %>%
  mutate(watershed = if_else(str_detect(site, "milonieres") & is.na(watershed),
                             "yzeron",
                             if_else(str_detect(site, "morcilles") & is.na(watershed),
                                     "ardieres", 
                                     if_else(str_detect(site, "ratier") & is.na(watershed),
                                             "yzeron", watershed))))

x %>%
  select(watershed, pH, sc = spcond_uscm, no = no3n_mgL) %>%
  pivot_longer(-watershed) %>%
  group_by(watershed, name) %>%
  drop_na() %>%
  summarize(mean = mean(value, na.rm = T),
            sd = sd(value, na.rm = T),
            n = n())
