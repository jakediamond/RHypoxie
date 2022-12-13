# 
# Purpose: To do basic DO and metabolism analysis of RHypoxie on data from 2019-2022
# Author: Jake Diamond
# Date: 9 December 2022
# 

# Load libraries
library(lubridate)
library(readxl)
library(scales)
library(patchwork)
library(tidytext)
library(tidyverse)
library(tidytable)

# Load all data
df <- readRDS(file.path("data", "10_clean_data", 
                        "rhone_hourly_data_including2022.RDS"))

df_met <- readRDS()
x= filter(df, confluence == "ratier_ribes", month(datetime) == 7, year == 2022,
          day(datetime) == 26)
# Max and min stuff -------------------------------------------------------
# DO daily min max etc
do_daily <- df %>%
  group_by(watershed, site, date, area_km2) %>%
  summarize(maxDO = max(DO, na.rm = T),
            minDO = min(DO, na.rm = T),
            maxDOsat = max(DO_per, na.rm = T),
            minDOsat = min(DO_per, na.rm = T)) %>%
  mutate(amp = maxDO - minDO,
         ampsat = maxDOsat - minDOsat) %>%
  ungroup()

# overall summary
do_daily %>%
  filter(!is.infinite(minDO), !is.infinite(maxDO)) %>%
  summarize(min_amp = min(amp),
            max_amp = max(amp),
            mean_amp = mean(amp),
            median_amp = median(amp),
            sd_amp = sd(amp))

ggplot(data = filter(do_daily, between(month(date), 3 ,4)),
       aes(x = area_km2,
           y = ampsat,
           color = watershed)) +
  stat_summary() +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()

# Timing of daily min max
do_time <- df %>%
  group_by(site, date) %>%
  filter(DO == max(DO, na.rm = T) | DO == min(DO, na.rm = T)) %>%
  mutate(type = if_else(DO == max(DO), "max", "min"))

do_time %>%
  mutate(hour = hour(datetime)) %>%
  ungroup() %>%
  group_by(type, month) %>%
  summarize(mn = mean(hour))

do_time %>%
  mutate(hour = hour(datetime)) %>%
  ungroup() %>%
  ggplot(aes(x = hour, fill = type)) +
  geom_density() +
  facet_wrap(~month(datetime))
  
# plot of daily min max for loire
p_minmax <- filter(do_daily, year(date) == 2020) %>%
  group_by(site) %>%
  filter(min(month(date)) == 3) %>%
  ungroup() %>%
  left_join(distinct(df, site, watershed, area_km2)) %>%
  mutate(site_f = factor(site),
         site_f = fct_reorder2(site_f, watershed, area_km2, .desc = FALSE)) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = maxDOsat), color = "red") +
  geom_line(aes(y = minDOsat), color = "blue") +
  facet_wrap(~site_f, ncol=3) +
  theme_bw() +
  labs(y = "DO (% sat.)") +
  theme(axis.title.x = element_blank())
p_minmax
ggsave(plot = p_minmax,
       filename = file.path("results", "Figures", "report", "loire_maxmin_2020.png"),
       dpi = 600,
       width = 18.2,
       height = 24,
       units = "cm") 
