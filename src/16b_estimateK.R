library(lubridate)
library(MASS)
library(tidyverse)
library(tidytable)


# Load data ---------------------------------------------------------------
# Load DO data
df <- readRDS(file.path("data", "10_clean_data", "hourly_data_all.RDS")) %>%
  filter.(watershed %in% c("ardieres", "vauxonne", "yzeron"))

# Load depth data
df_d <- readRDS(file.path("data", "estimated_depths.RDS")) %>%
  ungroup() %>%
  group_by(site, date) %>%
  summarize(depth = mean(depth, na.rm = T))

# Get it to be only nighttime(ish) data,
df_night <- df %>%
  mutate.(hour = hour(datetime)) %>%
  filter.(hour > 16 | hour < 5) %>%
  mutate.(DO_sat = DO * 100 / DO_per,
         DO_def = DO_sat - DO,
         del_DO = DO - lag(DO),
         dateuse = if_else.(hour < 5, date - days(1), date))

# Rolling regression function
rolling_lm <- tibbletime::rollify(
  .f = function(DO_def, del_DO) {
  rlm(del_DO ~ DO_def, psi = psi.bisquare)
  }, 
  window = 6, 
  unlist = FALSE)

# Define an r2 function for robust IRWLS regression 
r2_fun <- function(x){
  SSe <- sum((x$w*x$resid)^2) # the residual sum of squares is weighted
  observed <- x$resid+x$fitted 
  SSt <- sum((x$w*observed-mean(x$w*observed))^2)
  value <- 1-SSe/SSt;  
  return(value);  
}

# Do rolling regression 
df_night_reg <- df_night %>%
  group_by(site, dateuse) %>%
  filter(sum(is.na(DO_def)) < 4) %>%
  mutate(roll_lm = rolling_lm(DO_def, del_DO))

# Results
df_night_res <- df_night_reg %>%
  filter.(!is.na(roll_lm)) %>%
  mutate.(int = map_dbl(roll_lm, ~coef(.)[1]),
          slope = map_dbl(roll_lm, ~coef(.)[2]),
          r2 = map_dbl(roll_lm, r2_fun))

# Better format
df_res <- df_night_res %>%
  # mutate(r2 = map_dbl(glanced, "r.squared")) %>%
  # unnest.(tidied) %>%
  # filter.(term == "DO_def") %>%
  select.(site, dateuse, hour, int, slope, r2)

# k_out 
k_out <- df_res %>%
  group_by(site, dateuse) %>%
  filter(slope > 0,
         int < 0,
         slope < 5,
         r2 > 0.9) %>%
  # filter(r2 == max(r2)) %>%
  # summarize(k2 = mean(estimate, na.rm = T),
  #           k2var = var(estimate, na.rm = T)) %>%
  rename(date = dateuse,
         k2 = slope) # mg/L/hr

# Get to k600 (m/d)
df_k600 <- df_night %>%
  group_by(site, date) %>%
  summarize(temp = mean(temp, na.rm = T),
            discharge = mean(qsite_m3s, na.rm = T)) %>%
  right_join(k_out) %>%
  mutate(k2 = k2 * 24,
         k600 = convert_kGAS_to_k600(k2, temp))

# Quick look just to convince ourselves
df_k600 %>%
    filter(site == "charbonnieres") %>%
  ggplot(aes(x = discharge,
             y = k600)) +
  geom_point() +
  theme_bw() +
  # scale_y_log10()+
  scale_x_log10()+
  scale_y_continuous(limits = c(0, 100)) +
  stat_summary_bin(bins = 5) +
  stat_smooth(method=function(formula,data,weights=weight) rlm(formula,
                                                               data,
                                                               weights=weight,
                                                               psi = psi.bisquare))
# add depth to get K600 (1/d)
df_k600 <- right_join(df_k600, df_d) %>%
  mutate(K600 = k600 / depth)

# Estimate regressions on binned data for use in metabolism estimates
df_met <- df_k600 %>%
  group_by(site) %>%
  mutate(lnQ = log(discharge),
         lnK = log(K600),
         Qbins = cut(lnQ, 5)) %>%
  group_by(site, Qbins) %>%
  drop_na(Qbins) %>%
  summarize(node = mean(lnQ),
            meanlnK = mean(lnK),
            sdlnK = sd(lnK)) %>%
  ungroup() %>%
  imputeTS::na_kalman()

saveRDS(df_met, file.path("data", "k_estimates_for_bayes.RDS"))
