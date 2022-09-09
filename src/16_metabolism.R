# 
# Purpose: To estimate metabolism at Saône headwaters
# Author: Jake Diamond
# Date: 7 September 2022
# 

# Load libraries
library(lubridate)
library(streamMetabolizer)
library(tidyverse)

# Look at what data inputs are needed for bayes model
metab_inputs("bayes", "data")

# DO data load and clean --------------------------------------------------
# Load DO data
df <- readRDS(file.path("data", "10_clean_data", "hourly_data_all.RDS")) %>%
  filter(watershed %in% c("ardieres", "vauxonne", "yzeron"))

# Get daily discharge and depth data
df_hyd <- df %>%
  group_by(site, date) %>%
  summarize(discharge = mean(q_m3s, na.rm = T), depth = mean(h_cm, na.rm = T) / 100) %>%
  imputeTS::na_kalman()

# Overall metadata
df <- left_join(df, df_hyd)

# Prepare data for stream Metabolizer -------------------------------------
# Calculate solar time
df$solar.time <- calc_solar_time(df$datetime, longitude = df$longitude)

# Get data in good format
df <- select(df,
             site,
             solar.time,
             DO.obs = DO,
             DO.sat = DO_per,
             depth = depth,
             temp.water = temp,
             light = rad_wm2,
             discharge)

# Nest data by site
df_n <- df %>%
  group_by(site) %>%
  arrange(solar.time) %>%
  nest() 

# Configure the model -----------------------------------------------------
# Choose a model structure
# We choose a MLE model with both observation error and process error
# We will not pool K600 because we don't have discharge data
mod <- mm_name(type = "mle")

# Metabolism function for nested data ---------------------------------------
met_fun <- function(data, mod_name = mod){
  # Set the specifications
  specs <- specs(model_name = mod_name)
  
  # Do the metabolism
  metab(specs = specs, 
        data = as.data.frame(data))
}

# Run the metabolism model on nested data ---------------------------------
mm_all_mle <- df_n %>%
  mutate(mm = map(data, ~met_fun(data = .x)))

saveRDS(mm_all_mle, file.path("data", "rhone_metabolism_mle.RDS"))
mm_all_mle <- readRDS(file.path("data", "rhone_metabolism_mle.RDS"))
# Inspect the model -------------------------------------------------------
mm_mle <- mm_all_mle %>%
  mutate(met = map(mm, predict_metab)) %>%
  select(-data,-mm) %>%
  unnest(met) %>%
  ungroup()
saveRDS(mm_mle, file.path("data", "rhone_metabolism_mle_preds.RDS"))

tp <- ungroup(mm_all_mle) %>%
  filter(site == "mercier amont ratier") %>%
  select(mm) %>%
  pluck(1, 1)
plot_metab_preds(tp)
plot_DO_preds(tp, style = c("dygraphs"))
plot(get_params(tp)$K600.daily, get_params(tp)$ER.daily)

test2 <- mm_all_mle %>%
  mutate(met = map(mm, predict_metab)) %>%
  unnest(met) %>%
  gather(key = flux,
       value = value,
       c(GPP, ER))
streamMetabolizer::metab_night
