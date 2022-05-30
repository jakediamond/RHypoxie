# 
# Purpose: To smooth DO/cond./lux time-series, reduce outliers
# Author: Jake Diamond
# Date: September 5, 2019
# 

# Set working directory
setwd("Z:/Rhypoxie")

# Load libraries
library(zoo)
library(signal)
library(imputeTS)
library(plotly)
library(lubridate)
library(tidyverse)

# Read in all DO data
df <- readRDS("Data/02_sensor_data/raw/raw_time_series_data.RDS")

# Change some naming for consistency
df <- df %>%
  mutate(site = if_else(site == "milonier", "miloniere", site),
         confluence = if_else(confluence == "yzeron_milonier", "yzeron_miloniere", confluence))

# Fix the data for mercier amont ratier...somehow its days for one download are 
# 49 days and 5 hours behind
df <- df %>%
  mutate(datetime = if_else(filename == "20854287_21052021.csv",
                            datetime + days(49) + hours(5),
                            datetime),
         datetime_utc = if_else(filename == "20854287_21052021.csv",
                            datetime_utc + days(49) + hours(5),
                            datetime_utc))

# Also, we know of some periods with bad data...turn those to NA here
df <- df %>%
  mutate(value = if_else(type == "DO" & ((site == "ardevel amont vernus" &
                                           between(datetime, 
                                                   ymd_hm("202105270700"),
                                                   ymd_hm("202106041600"))) |
                                           (site == "mercier aval presles" &
                                              between(datetime, 
                                                      ymd_hm("202108282200"),
                                                      ymd_hm("202108311100"))) |
                                           (site == "ratier aval mercier" &
                                              between(datetime, 
                                                      ymd_hm("202108071200"),
                                                      ymd_hm("202108091500"))) |
                                           (site == "ratier" &
                                              between(datetime, 
                                                      ymd_hm("202107311700"),
                                                      ymd_hm("202108011900"))) |
                                           (site == "yzeron aval miloniere" &
                                              between(datetime, 
                                                      ymd_hm("202108041900"),
                                                      ymd_hm("202108261700"))) |
                                           (site == "yzeron amont miloniere" &
                                              between(datetime, 
                                                      ymd_hm("202108040300"),
                                                      ymd_hm("202108101900"))) |
                                           (site == "miloniere" &
                                              between(datetime, 
                                                      ymd_hm("202108280300"),
                                                      ymd_hm("202108311900")))
                                         ),
                         NA_real_,
                         value),
         temp = if_else(is.na(value), NA_real_, temp))

# Nest data by site
df_n <- df %>%
  select(filename, site, type, datetime, value) %>%
  group_by(type, site) %>%
  nest()

# Define functions --------------------------------------------------------
# Data cleaning function, finds anomalies/jumps/replaces with NA
# the probability limit for delta for detecting "bad" jumps (default 0.95),
# the multiplier for delta to detect the jump (default 2)
# also a minimum threshold for expected DO (default 2)
clean_fun <- function(data,
                      type,
                      prob = 0.95,
                      mult = 2){
  # Order data
  data = data[with(data, order(datetime)), ]

  # Get normal bounds based on type
  dat_bounds = switch(type,
                      DO = c(1, 17),
                      light = c(0, 120000),
                      conductivity = c(10, 800))
  
  # Calculate upper 95% average delta DO by month and year
  # Use this to set a bound on what to expect for big jumps
  dvalue_lim = data %>%
    mutate(dvalue = value - lag(value)) %>%
    ungroup() %>%
    summarize(dvalue_lim = quantile(abs(dvalue), 
                                 probs = prob, 
                                 na.rm = TRUE)) %>%
    as.numeric()
  
  # Make the data NA where there are big jumps, or just wrong data (anomalies)
  # If the data jump more than 2x the 95% value for that time period
  # If DO.obs is less than 1 mg/L (not really possible in this river)
  # If there was an anomaly from decomposition method
  data = data %>% 
    mutate(dvalue = value - lag(value),
           value2 = ifelse(abs(dvalue) > mult * dvalue_lim | 
                         is.na(dvalue) | 
                         !between(value, dat_bounds[1], dat_bounds[2])
                       , NA,
                       value
           )
    )
  data = data[with(data, order(datetime)), ]
}

# Define lowpass filter function
# Can prescribe cutoff_frequency for low pass bandwidth (default 0.05 15min^-1
# for 15-min data)
lowpass_fun <- function(data, 
                        cutoff_frequency = 0.05) {
  # Re-interpolate all NAs so that there are none with Stineman method
  data$value_an_int <- na_interpolation(data$value2, option = "stine")
  # Order the data, just in case
  data <- data[with(data, order(datetime)),]
  # Sampling rate [s^-1]
  sr <- 1 / (as.numeric(difftime(data$datetime[2], data$datetime[1], units = "secs")))
  # Nyquist frequency = half the sampling rate
  nyq <- sr / 2
  # Cutoff frequency (s^-1)
  cutoff <- cutoff_frequency  / (60 * 15)
  # Normalized cutoff frequency for Butterworth filter
  W <- cutoff / nyq
  # Butterworth low-pass filter, digital, 2nd order
  myfilter <- butter(2, W, type = 'low', plane = 'z')
  # Forward-reverse filter to remove phase-shift 
  # associated with Butterworth filter (must be in vector-form)
  vec <- as.vector(data$value_an_int)
  filtered <- filtfilt(myfilter, vec)
  # Filtered data
  data$filtered <- filtered
  data <- data[with(data, order(datetime)), ]
  rem <- sr / cutoff
  data <- data[-c(1:rem, (nrow(data) - rem):nrow(data)),]
}

# Apply functions ---------------------------------------------------------
# Clean the data and use the lowpass filter
df_n <- df_n %>%
  mutate(clean = map2(data, type, clean_fun),
         filt = map(clean, lowpass_fun))

# Get all in one dataframe
df2 <- df_n %>%
  ungroup() %>%
  select(-data, -clean) %>%
  unnest(cols = filt)

# Finally, add  back NAs for large chunks of missing data
# because the filter can't adequately fill these gaps
df3 <- df2 %>%
  group_by(filename, type, site) %>%
  mutate(na_check = 
           {res = rle(is.na(value2));
           rep(res$values * res$lengths, res$lengths)},
         value_use = ifelse(na_check > 11,
                            NA,
                            filtered)) %>%
  mutate(value_use = if_else(((type == "light" & (value2 == 0 & !is.na(value2))) |
                                 type == "light" & value_use < 0), 
                              value2, 
                              value_use)) %>%
  dplyr::ungroup()

# Add back to data
df_final <- select(df3, site:datetime, value_clean = value_use) %>%
  left_join(df) %>%
  select(-value, value = value_clean) %>%
  mutate(temp = if_else((temp < 0 & type != "light") | (temp > 30 & type != "light"), NA_real_, temp))

# Save data
saveRDS(df_final, "Data/02_sensor_data/all_sensor_data_clean.RDS")

# Clean up to wide format -------------------------------------------------
# Get into wider format
df_w <- df_final %>%
  mutate(datetime = with_tz(datetime_utc, "Europe/Berlin")) %>%
  select(-datetime_utc, -tz, -path, -filename, -sensor) %>%
  distinct(datetime, site, type, value, temp, .keep_all=TRUE) %>%
  pivot_wider(names_from = type, values_from = c(value, temp), values_fn = mean) %>%
  rename(DO = value_DO,
         DO_temp = temp_DO,
         lux_water = value_light, 
         lux_water_temp = temp_light,
         cond = value_conductivity,
         cond_temp = temp_conductivity)

# Some data cleaning
# df_w <- df_w %>%
#   mutate_at(vars(10:15), funs(if_else(. < 0, NA_real_, .))) # get rid of negative values

# Get rid of crazy values
# df_w <- df_w %>%
#   mutate(DO = if_else(DO > 25 | DO <0, NA_real_, DO),
#          DO_temp = if_else(DO_temp > 40 | DO_temp <0 , NA_real_, DO_temp),
#          lux_water = if_else(lux_water > 100000, NA_real_, lux_water),
#          lux_water_temp = if_else(lux_water_temp > 40, NA_real_, lux_water_temp),
#          cond = if_else(cond < 0 | cond > 2000, NA_real_, cond),
#          cond_temp = if_else(cond_temp > 40, NA_real_, cond_temp))

# Save data
saveRDS(df_w, "Data/02_sensor_data/all_sensor_data_clean_wide.RDS")

# Save as xlsx with sheets
# Split one data frame per site
df_w %>%
  group_split(site) -> list_of_dfs
# name of each sheet will be the site
list_of_dfs %>%
  map(~pull(., site)) %>%
  map(~unique(.)) -> names(list_of_dfs)

list_of_dfs %>%
  writexl::write_xlsx(path = "Data/02_sensor_data/all_sensor_data_clean_wide.xlsx")
