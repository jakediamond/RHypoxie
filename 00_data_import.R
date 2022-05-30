# 
# Purpose: To grab all DO and light data in clean dataframes
# Author: Jake Diamond
# Date: July 20, 2020
# 

# Set working directory
setwd("Z:/Rhypoxie")
# setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")
# setwd("//ly-lhq-srv/jake.diamond/Loire_DO")
# setwd("C:/Users/diamo/Dropbox/Projects/RHypoxie")
# setwd("C:/Users/jake.diamond/Dropbox/Projects/RHypoxie")

# Load libraries
library(lubridate)
library(readxl)
library(tidyverse)

# Load data
# First get data path and names of files for DO
data_path_do <- "Data/02_sensor_data/raw/HOBO_excel"
files_do <- dir(data_path_do, pattern = "*.csv")

# Then load all data into one dataframe
df <- tibble(path = c(rep(data_path_do, length(files_do))),
             filename = files_do) %>%
  mutate(file_contents = map2(path, filename,
                              ~ read_csv(file.path(.x, .y),
                                         skip = 2,
                                         col_names = FALSE)),
         tz = map2(path, filename, # time zone information
                   ~ read_csv2(file.path(.x, .y), n_max = 1) %>%
                     str_extract("(?i)(?<=GMT\\D)\\d+") %>%
                     as.numeric()
         )
  ) %>%
  unnest(cols = c(file_contents))

# Load metadata
meta <- read_excel("Data/01_metadata/sensor_metadata.xlsx", sheet = "site_metadata") %>%
  select(-notes, -datetime_in_water) %>%
  pivot_longer(cols = starts_with("sensor"), 
               names_to = "type", values_to = "sensor") %>%
  drop_na() %>%
  mutate(type = word(type, 2, sep = "_"),
         sensor = as.character(sensor))

# Some data cleaning, make filename = sensor serial number
# Combine sites to sensors
df2 <- df %>%
  mutate(sensor = str_sub(filename, end = 8)) %>%
  select(-X1) %>%
  mutate(X2 = mdy_hms(X2),
         year = year(X2)) %>%
  rename(datetime = X2) %>%
  mutate(datetime_utc = force_tzs(datetime, tzones = paste0("Etc/GMT-", tz))) %>%
  distinct() #some files included previous uploads...remove duplicates

# Add back some meta data for each site
df3 <- df2 %>%
  left_join(meta) %>%
  mutate(X5 = if_else(type == "light", X4, X3),#temp and value are switched for light sensors
         X4 = if_else(type == "light", X3, X4),#temp and value are switched for light sensors
         X3 = X5) %>%
  select(-X5) %>%
  rename(value = X3, temp = X4) %>%
  distinct(site, datetime, value, temp, .keep_all = TRUE)

# Save raw data
saveRDS(df3, "Data/02_sensor_data/raw/raw_time_series_data.RDS")
