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

# Load data
df <- readRDS("Data/hourly_data.RDS")

# Load some metadata
meta <- read_xlsx("Data/01_metadata/site_area.xlsx")

df <- left_join(df, meta)

# Load old Loire data
df_loire <- readRDS("//LY-LHQ-SRV/jake.diamond/Loire_DO/Headwaters/Data/headwaters_data_clean") %>%
  left_join(read_xlsx("Data/01_metadata/loire_headwaters_metadata.xlsx"))

# Combine data
df <- bind_rows(df, df_loire)

# Plot top to bottom summary
df_p <- df %>%
  mutate(date = date(datetime)) %>%
  group_by(watershed, number, site, confluence, date) %>%
  summarize(maxDO = max(DO, na.rm = T),
            minDO = min(DO, na.rm = T),
            maxper = max(DO_per, na.rm = T),
            minper = min(DO_per, na.rm = T)) %>%
  pivot_longer(maxDO:minper) %>%
  mutate(value = na_if(value, Inf),
         value = na_if(value, -Inf))

# Plot of daily min max
ggplot(data = filter(df_p, name %in% c("maxDO", "minDO"), watershed == "Loise"),
       aes(x = date,
           y = value,
           color = name)) +
  geom_line() +
  theme_bw() +
  scale_x_date(date_breaks = "1 month", date_labels = "%m") +
  labs(y = "DO (mg/L)", x = "", title = "Yzeron") +
  facet_wrap(~fct_reorder(site, number), ncol = 3) +
  theme(legend.position = "none", axis.title.x = element_blank())


# Some quick calculations
df <- df %>%
  mutate(date = date(datetime),
         month = month(datetime),
         DOhyp = if_else(DO_per < 50 | DO < 4, 
                         1, 0)) #define hypoxia as less than 50% saturation (Carter et al. 2021)

# overall summary of quality
# df_overall <- df %>%
#   group_by(watershed) %>%
#   mutate(quality = case_when(DO>8~ "très bonne",
#                              between(DO,6,8)~ "bonne",
#                              between(DO,4,6)~ "moyenne",
#                              between(DO,3,4)~"mauvaise",
#                              DO<3, "très mauvaise",
#                              TRUE ~ NA_character_))


# Calculate daily stats by site
df_daily <- df %>%
  group_by(watershed, site, date) %>%
  summarize(DOmaxper = max(DO_per, na.rm = T),
            DOminper = min(DO_per, na.rm = T),
            DOampper = DOmaxper - DOminper,
            DOmax = max(DO, na.rm = T),
            DOmin = min(DO, na.rm = T),
            DOamp = DOmax - DOmin,
            DOhypn = sum(DOhyp, na.rm = T))

# Hypoxia period lengths
df_hyp <- df %>%
  group_by(site) %>%
  arrange(site, datetime) %>%
  mutate(hyp_l = sequence(rle(DOhyp)$lengths),
         hyp_change = if_else(lag(DOhyp) != DOhyp, 1, 0)) %>%
  filter(DOhyp == 1) %>%
  mutate(hyp_pd = cumsum(hyp_change))

# Plot of daily max
ggplot(data = filter(df_p, name %in% c("maxDO", "minDO")),
       aes(x = date,
           y = value,
           color = name)) +
  geom_line() +
  theme_bw() +
  scale_x_date(date_breaks = "1 month", date_labels = "%m") +
  labs(y = "DO (mg/L)", x = "", title = "Ardières") +
  facet_grid(watershed~fct_reorder(site, number), ncol = 3) +
  theme(legend.position = "none", axis.title.x = element_blank())



# Summary of that data
df_daily_sum <- df_daily %>%
  pivot_longer(cols = -c(site, date)) %>%
  group_by(site, name) %>%
  filter(!is.infinite(value)) %>%
  summarize(mean = mean(value, na.rm = T),
            sd = sd(value, na.rm = T)) %>%
  ungroup() %>%
  mutate(name = as.factor(name),
         site_p = reorder_within(site, mean, name))

# Quick summary graph
df_daily %>%
  pivot_longer(cols = -c(site, date)) %>%
  left_join(df_daily_sum) %>%
  ggplot(aes(x = site_p,
             y = value)) +
  facet_wrap(~name, scales = "free") +
  stat_summary() +
  # geom_boxplot(show.legend = FALSE) + 
  # geom_errorbar(aes(ymax = mean + sd, ymin = mean - sd)) +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(expand = c(0,0)) +
  theme_bw() +
  labs(x = "")

ggsave("Z:/RHypoxie/Figures/basic_DO_stats.png",
       dpi = 600,
       width = 36,
       height  = 18.4,
       units = "cm",
       device = "png")

# Hypoxia period lengths
df_hyp <- df %>%
  group_by(site) %>%
  arrange(site, datetime) %>%
  mutate(hyp_l = sequence(rle(DOhyp)$lengths),
         hyp_change = if_else(lag(DOhyp) != DOhyp, 1, 0)) %>%
  filter(DOhyp == 1) %>%
  mutate(hyp_pd = cumsum(hyp_change))

# summarize that by mean length of time of hypoxia
ggplot(data = df_hyp,
       aes(x = fct_reorder(site, hyp_l, .fun = mean),
           y = hyp_l)) +
  stat_summary() +
  coord_flip() +
  theme_bw() +
  labs(x = "",
       y = "mean length of hypoxia (hours)")

ggsave("Z:/RHypoxie/Figures/mean_length_hypoxia.png",
       dpi = 600,
       width = 36,
       height  = 18.4,
       units = "cm",
       device = "png")

# Hypoxia summary with discharge
df_hyp2 <- df_hyp %>%
  group_by(site, hyp_pd) %>%
  summarize(across(where(is.numeric), c(sd = sd,
                                mean = mean, 
                                median =median),
                   na.rm = T))

# summarize that by mean length of time of hypoxia
ggplot(data = df,
       aes(x = rain_mm,
           y = q_mmh,
           color = log(area_km2))) +
  geom_point() +
  scale_color_viridis_c() +
  theme_bw() +
  labs(x = "",
       y = "mean length of hypoxia (hours)")

# summarize that by number of events
df_hyp %>%
  summarize(pds = max(hyp_pd, na.rm = T)) %>%
  ggplot(aes(x = fct_reorder(site, pds),
           y = pds)) +
  geom_point() +
  scale_y_continuous(breaks = seq(0,15,2)) +
  # stat_summary(fun = max) +
  coord_flip() +
  theme_bw() +
  labs(x = "",
       y = "number of hypoxic events")

ggsave("Z:/RHypoxie/Figures/number_hypoxic_events.png",
       dpi = 600,
       width = 36,
       height  = 18.4,
       units = "cm",
       device = "png")

# Exceedance curves by site
df_ex <- df %>%
  group_by(site) %>%
  transmute(DO_per = DO_per,
            cdf = cume_dist(DO_per))

ggplot(data = df_ex,
       aes(x = 1 - cdf, y = DO_per)) +
  geom_line() +
  theme_bw() +
  facet_wrap(~site) +
  labs(x = "exceedance probability",
       y = expression(DO[sat]~"(%)"))

ggsave("Z:/RHypoxie/Figures/DO_exceedance_curves.png",
       dpi = 600,
       width = 36,
       height  = 18.4,
       units = "cm",
       device = "png")


# Monthly summary
mutate(df, month = month(datetime)) %>%
  group_by(month) %>%
  filter(between(DO_per,
                 quantile(DO_per, 0.01, na.rm = T),
                 quantile(DO_per, 0.95, na.rm = T))) %>%
  ggplot(aes(x = month,
             y = DO_per,
             group = month)) +
  geom_violin() +
  scale_x_continuous(breaks = seq(1,10,1)) +
  stat_summary() +
  theme_bw() +
  labs(x = "month",
       y = expression(DO[sat]~"(%)"))

ggsave("Z:/RHypoxie/Figures/monthly_distributions.png",
       dpi = 600,
       width = 36,
       height  = 18.4,
       units = "cm",
       device = "png")


# discharge
distinct(df, datetime, Q_grez, Q_rat, Q_char) %>%
  pivot_longer(cols =-datetime) %>%
  ggplot(aes(x = datetime,
             y = value,
             color = name)) +
  geom_line() +
  scale_x_datetime(date_breaks = "1 month") +
  theme_bw() +
  labs(x = "",
       y = expression("Q ("*m^3~s^{-1}*")"))

ggsave("Z:/RHypoxie/Figures/discharge_comparison.png",
       dpi = 600,
       width = 36,
       height  = 18.4,
       units = "cm",
       device = "png")


# Some time series
df_daily %>%
  transmute(max = slider::slide_dbl(DOmax, mean, before = 7, na_rm = T),
         min = slider::slide_dbl(DOmin, mean, before = 7, na_rm = T),
         mean = slider::slide_dbl(DOmean, mean, before = 7, na_rm = T),
         date = date) %>%
  pivot_longer(cols = max:mean) %>%
  mutate(value = if_else(is.infinite(value), NA_real_, value)) %>%
  ggplot(aes(x = date,
             y = value,
             color = name)) +
  geom_line() +
  facet_wrap(~site, scales = "free_y") +
  theme_bw() +
  labs(x = "",
       y = expression(DO[sat]~"(%)"))

ggsave("Z:/RHypoxie/Figures/max_min_do_ts.png",
       dpi = 600,
       width = 36,
       height  = 18.4,
       units = "cm",
       device = "png")

# day vs night
df %>%
  filter(DOhyp == 1) %>%
  mutate(day = if_else(rad_wm2 == 0, "night", "day"),
            date = date) %>%
  group_by(site, day) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  group_by(site) %>%
  summarize(prop = n / sum(n)) %>%
  ggplot(aes(x = site,
             y = prop)) +
  geom_point() +
  coord_flip() +
  theme_bw() +
  labs(x = "",
       y = "proportion of ")

# Some map stuff ----------------------------------------------------------
library(tmap)
library(sf)

alt = raster::raster("Data/GIS/w001001.adf")
slope = raster::terrain(alt, opt = 'slope')
aspect = raster::terrain(alt, opt = 'aspect')
hill = raster::hillShade(slope, aspect, 30, 90)
ws = distinct(df, site, watershed, latitude, longitude) %>%
  st_as_sf(coords = c("longitude", 'latitude'), crs = 4326)
pts_a = filter(ws, watershed == "ardieres") %>%
  left_join(df_daily_sum)
bb_a = st_as_sfc(st_bbox(filter(ws, watershed == "ardieres")))
bb2_a <- st_transform(bb_a, st_crs(hill))


tm_shape(hill, bbox = bb2_a, raster.downsample = FALSE) +
  tm_raster(palette = gray(0:100 / 100), n = 100, legend.show = FALSE) +
  tm_shape(pts_a) +
  tm_dots(col = "DOhypn", palette)
