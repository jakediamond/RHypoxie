# 
# Purpose: To do a first analysis of hypoxia on data from 2019-2021
# Author: Jake Diamond
# Date: 18 January 2022
# 

# Load libraries
library(lubridate)
library(readxl)
library(scales)
library(patchwork)
library(tidytext)
library(tidyverse)

# Load data
df <- readRDS(file.path("data", "10_clean_data", "hourly_data_all.RDS"))

# Add info for light, time, and hypoxia (defined as less than 3 mg/L)
df <- df %>%
  mutate(date = date(datetime),
         month = month(datetime),
         year = year(date),
         solartime = streamMetabolizer::calc_solar_time(datetime, longitude),
         light = streamMetabolizer::calc_light(solartime, latitude, longitude)) %>%
  filter(between(month, 3, 10),#don't trust data outside of these months
         !(is.na(DO) & month == 10), # some data at the end of the files are NA
         (oow == "no" | is.na(oow))) %>% #don't want to include data when streams are dry
  mutate(DO = if_else(site %in% c("carrat", "toranche st cyr les vignes", "le rieu")
                      & month == 4 & DO < 3, NA_real_, DO)) %>% # also filter some times I know a sensor is buried
  mutate(DOhyp = if_else(DO < 3, 
                         1, 
                         0)) #define hypoxia as less than 3 mg/Lsaturation 
# (Carter et al. 2021 uses 50%)

# Some plotting info for the rest -----------------------------------------

theme_second_axis <- theme_minimal() +
  theme(panel.background = element_rect(fill='transparent', color = NA), #transparent panel bg
        plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
        panel.grid.major = element_blank(), #remove major gridlines
        panel.grid.minor = element_blank(), #remove minor gridlines
        legend.background = element_rect(fill='transparent'), #transparent legend bg
        legend.box.background = element_rect(fill='transparent'),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank()) #transparent legend panel)

layout <- c(
  area(t = 1, l = 1, b = 3, r = 4),
  area(t = 1, l = 1, b = 3, r = 4)
)

layout2 <- c(
  area(t = 1, l = 1, b = 6, r = 4),
  area(t = 1, l = 1, b = 3, r = 4),
  area(t = 3, l = 1, b = 6, r = 4)
)

filter(df, year(date) == 2019) %>%
  group_by(month) %>%
  summarize(hy = sum(DOhyp, na.rm = T),
            n = n(),
            per = hy / n)
# Plot of monthly hypoxia, temp, q ----------------------------------------

p_monthly_hyp <- df %>%
  mutate(year = year(date)) %>%
  group_by(year, month) %>%
  summarize(hy = sum(DOhyp, na.rm = T),
            n = n(),
            per = hy / n) %>%
  ggplot() + 
  geom_col(aes(x = month,
               y = per,
               group = month)) + 
  # geom_text(aes(x = 10, y = 0.18, label = sum(n), group = year)) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(min(df$month), max(df$month), 1)) +
  facet_grid(~year, space = "free", scales = "free_x") +
  theme_bw() +
  theme(strip.background = element_blank(),
        panel.grid = element_blank()) +
  labs(y = "% of measurements hypoxic",
       x = "month")
p_monthly_hyp

p_monthly_temp <- df %>%
  mutate(year = year(date)) %>%
  ggplot(aes(x = month,
             y = DO_temp)) + 
  stat_summary(color = "red", geom = "line") +
  stat_summary(color = "red") +
  facet_grid(~year, space = "free", scales = "free_x") +
  theme_second_axis +
  theme(axis.title = element_text(color = "red"),
        axis.text = element_text(color = "red")) +
  scale_x_continuous(breaks = seq(min(df$month), max(df$month), 1))+
  scale_y_continuous(position = "right") +
  # labs(y = expression("q (mm"~h^{-1}*")")) +
  labs(y = expression("stream temperature ("*degree*"C)"),
       x = "month")

p_monthly_q <- df %>%
  mutate(year = year(date),
         q_mmd = if_else(is.na(q_mmd), q_mmh *24, q_mmd)) %>%
  ggplot(aes(x = month,
             y = q_mmd)) + 
  stat_summary(fill = "blue", geom = "ribbon", alpha = 0.4, ymin = 0) +
  # stat_summary(color = "blue") +
  facet_grid(~year, space = "free", scales = "free_x") +
  theme_second_axis +
  theme(axis.title = element_text(color = "blue"),
        axis.text = element_text(color = "blue")) +
  scale_y_continuous(position = "right") +
  # labs(y = expression("q (mm"~h^{-1}*")")) +
  labs(y = expression("q ("*mm~d^{-1}*")"),
       x = "month")
p_monthly_q

p_month <- p_monthly_hyp + p_monthly_temp + p_monthly_q +
  plot_layout(design = layout2)
p_month

# Hypoxia events and their lengths ----------------------------------------
# Hypoxia period lengths
df_hyp <- df %>%
  group_by(site) %>%
  arrange(site, datetime) %>%
  mutate(hyp_l = sequence(rle(DOhyp)$lengths),
         hyp_change = if_else(((lag(DOhyp, default = 0) != DOhyp) &
                                 (lag(DOhyp, 2, default = 0) != DOhyp)), 
                              1, 0)) %>%
  filter(DOhyp == 1) %>%
  mutate(hyp_pd = cumsum(replace_na(hyp_change, 0)))

# number of hypoxic events
p_num_events <- ungroup(df_hyp) %>%
  group_by(month, site, year) %>%
  summarize(pds = max(hyp_pd, na.rm = T)) %>%
  ungroup() %>%
  ggplot(aes(x = month,
             y = pds)) +
  stat_summary() +
  stat_summary(geom = "line") +
  # geom_point() +
  facet_grid(~year, space = "free", scales = "free_x") +
  # scale_y_continuous(breaks = seq(0,15,2)) +
  # stat_summary(fun = max) +
  # coord_flip() +
  theme_bw() +
  theme(strip.background = element_blank(),
        panel.grid = element_blank()) +
  labs(x = "",
       y = " mean number of hypoxic events")
p_num_events

p_length_events <- ungroup(df_hyp) %>%
  group_by(site, hyp_pd) %>%
  filter(hyp_l == max(hyp_l)) %>%
  ggplot(aes(x = month,
             y = hyp_l)) +
  stat_summary(color = "dark grey") +
  stat_summary(geom = "line", color = "dark grey") +
  facet_grid(~year, space = "free", scales = "free_x") +
  scale_y_continuous(position = "right") +
  theme_second_axis + 
  theme(axis.text = element_text(color = "dark grey"),
        axis.title = element_text(color = "dark grey")) + 
  labs(x = "",
       y = "mean length of hypoxic events (hours)")

p_events <- p_num_events + p_length_events +
  plot_layout(design = layout)
p_events
# Analysis ----------------------------------------------------------------



# Get a dataframe of just discharge
df_q <- distinct(da)

# Calculate daily stats by site
df_daily <- df %>%
  group_by(site, date) %>%
  summarize(DOmax = max(DO, na.rm = T),
            DOmin = min(DO, na.rm = T),
            DOamp = DOmax - DOmin,
            DOmean = mean(DO, na.rm = T))

df_daily %>%
  ungroup() %>%
  group_by(site) %>%
  mutate(across(where(is.numeric), ~na_if(., Inf)),
            across(where(is.numeric), ~na_if(., -Inf))) %>%
  summarize(across(where(is.numeric), median, na.rm = T)) %>%
  left_join(group_by(df, site) %>% summarize(hyp = sum(DOhyp, na.rm = T))) %>%
  ggplot(aes(x = DOmin, y = hyp)) +
  geom_point() +
  theme_bw() +
  stat_smooth(method = "lm") +
  ggpubr::stat_cor()



# Summary of that data
df_daily_sum <- df_daily %>%
  pivot_longer(cols = -c(site, date)) %>%
  group_by(site, name) %>%
  filter(!is.infinite(value)) %>%
  summarize(mean = mean(value, na.rm = T),
            sd = sd(value, na.rm = T)) %>%
  ungroup() %>%
  mutate(name = as.factor(name),
         site_p = tidytext::reorder_within(site, mean, name))

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

ungroup(df_hyp) %>%
  group_by(site, hyp_pd) %>%
  filter(hyp_l == max(hyp_l),
         year(date) < 2021) %>%
  ggplot(aes(x = longitude,
             y = hyp_l)) +
  stat_summary() +
  # coord_flip() +
  theme_bw() +
  labs(x = "",
       y = "mean length of hypoxia (hours)")



df %>%
  mutate(day = if_else(light > 200, "day", "night")) %>%
  group_by(day) %>%
  summarize(hyp = sum(DOhyp, na.rm = T))

df %>%
  mutate(day = if_else(between(month, 7, 9), "summer", "not")) %>%
  group_by(day) %>%
  summarize(hyp = sum(DOhyp, na.rm = T))


df %>%
  mutate(hour = hour(datetime)) %>%
  ggplot(aes(x = hour, y = DOhyp)) + 
  stat_summary() +
  # geom_bar() +
  theme_bw()+
  labs(y = "hours of hypoxia",
       x = "hour of day")

ggsave("Figures/hypoxia_by_hour.png",
       dpi = 600,
       width = 12,
       height  = 8,
       units = "cm",
       device = "png")


ggsave("Figures/hypoxia_by_hour_month.png",
       dpi = 600,
       width = 12,
       height  = 8,
       units = "cm",
       device = "png")

df_hyp %>%
  ggplot(aes(x = DO_temp, y = DO)) + 
  stat_summary_bin() +
  theme_bw()+
  labs(y = "hours of hypoxia",
       x = "month")



df_hyp %>%
  ggplot(aes(x = month)) + 
  geom_bar() +
  facet_wrap(~fct_reorder(site, -altitude_m), ncol = 3) +
  theme_bw()+
  labs(y = "hours of hypoxia",
       x = "month")

ggsave("Figures/hypoxia_by_hour_month_site.png",
       dpi = 600,
       width = 32,
       height  = 24,
       units = "cm",
       device = "png")

df_hyp %>%
  mutate(hour = hour(datetime)) %>%
  ggplot(aes(x = hour)) + 
  geom_bar() +
  facet_wrap(~fct_reorder(site, -altitude_m), ncol = 3) +
  theme_bw()+
  labs(y = "hours of hypoxia",
       x = "hour of day")

ggsave("Figures/hypoxia_by_hour_and_site.png",
       dpi = 600,
       width = 32,
       height  = 24,
       units = "cm",
       device = "png")



# Sum of deficit

df_hyp %>%
  mutate(def = if_else(is.na(DOsat), DOsat - DO, DOsat2 - DO)) %>%
  filter(def > 0) %>%
  ggplot(aes(x = fct_reorder(site, def, median),
             y = def)) + 
  geom_boxplot() +
  coord_flip() +
  # facet_wrap(~) +
  theme_bw()+
  labs(y = "hypoxia DO deficit (mg/L)",
       x = "site")

ggsave("Figures/DO_hypoxia_deficit_and_site.png",
       dpi = 600,
       width = 18,
       height  = 12,
       units = "cm",
       device = "png")


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
df_day_night <- df %>%
  filter(DOhyp == 1) %>%
  mutate(day = if_else(light == 0, "night", "day"),
         date = date) %>%
  group_by(site, day) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  group_by(site) %>%
  mutate(tothyp = sum(n),
         prop = n / tothyp)

ggplot(data = df_day_night,
       aes(x = fct_reorder2(site, day, prop),
           y = prop,
           fill = day)) +
  scale_fill_manual(values = c("grey", "black")) +
  geom_col() +
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



# Some specific site stuff ------------------------------------------------

df_may_event <- df %>%
  filter(between(date, ymd(20210509), ymd(20210513))) %>%
  filter(site %in% c("miloniere",
                     "ribes",
                     "yzeron aval thiollet",
                     "thiollet",
                     "mercier amont ratier",
                     "ratier",
                     "ratier amont charbonnieres",
                     "sallarin",
                     "ardieres aval st didier",
                     "vernus")) %>%
  group_by(site) %>%
  mutate(DO = imputeTS::na_kalman(DO))
# group_by(site) %>%
# filter(sum(DOhyp, na.rm = T) > 1)

a = ggplot(data = df_may_event,
           aes(x = datetime,
               y = DO)) +
  geom_line()+
  facet_wrap(~site)+
  # scale_color_viridis_d()+
  theme_bw()
a



df_l <- df_may_event %>%
  select(site, datetime, Q_rat, Q_grez, Q_char, rain_mm) %>%
  pivot_longer(cols = c(Q_rat, Q_grez, Q_char, rain_mm))

b = ggplot(data = df_may_event,
           aes(x = datetime,
               y = rain_mm)) +
  geom_line(color = "blue") +
  scale_y_continuous(position = "right") +
  facet_wrap(~site)+
  theme_bw() +
  theme(panel.background = element_rect(fill='transparent'), #transparent panel bg
        plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
        panel.grid.major = element_blank(), #remove major gridlines
        panel.grid.minor = element_blank(), #remove minor gridlines
        legend.background = element_rect(fill='transparent'), #transparent legend bg
        legend.box.background = element_rect(fill='transparent')) #transparent legend panel)
b
library(patchwork)
layout <- c(
  area(t = 1, l = 1, b = 3, r = 4),
  area(t = 1, l = 1, b = 3, r = 4)
)
a+ b+ 
  plot_layout(design = layout)

ggsave("Figures/may10_event_DO.png",
       dpi = 600,
       width = 18.4,
       height  = 12,
       units = "cm",
       device = "png")

df_aug <- df %>%
  bind_rows(readRDS("C:/Users/diamo/Dropbox/Projects/Loire_headwaters/Headwaters/Data/headwaters_data_clean") %>%
              rename(site = Site, watershed = Subwatershed, q_mmh = q_mmd, lux_water = lux) %>%
              mutate(date = date(datetime),
                     year= year(date))) %>%
  filter((between(date, ymd(20210809), ymd(20210816)) |
            between(date, ymd(20190912), ymd(20190922)) |
            between(date, ymd(20200708), ymd(20200716)))) %>%
  group_by(site) %>%
  filter(oow == "no" | is.na(oow)) %>%
  mutate(DOhyp = if_else(DO <4, 1, 0)) %>%
  filter(sum(DOhyp, na.rm = T) > 1) %>%
  group_by(site, date) %>%
  mutate(color = min(DO) == DO,
         color = if_else(is.na(color), FALSE, color))


c = ggplot() +
  geom_line(data = df_aug,
            aes(x = datetime,
                y = DO, group = site),
            alpha = 0.5)+
  geom_point(data = filter(df_aug, color == TRUE),
             aes(x = datetime, y = DO, color = color)) +
  geom_hline(yintercept = 4, linetype = "dashed", color = "red") +
  facet_grid(site~year, scales = "free_x")+
  scale_color_manual(values = "blue") +
  theme_bw() +
  theme(legend.position = "none")
c


df_aug_m <- df_aug %>%
  group_by(site, date) %>%
  summarize(domin = min(DO)) %>%
  ungroup() %>%
  group_by(site) %>%
  nest() %>%
  mutate(mod = map(data, ~lm(domin~date, data = .)),
         t = map(mod, broom::tidy)) %>%
  select(t) %>%
  unnest(cols = t)


df_aug <- df_aug %>%
  filter(!(site %in% c("Charpassonne Chanin",
                       "Charpassonne la Jamarie",
                       "Coizet",
                       "Toranche aval",
                       "Coise aval Rieu",
                       "Doise",
                       "Vizézy amont Bullieux")),
         !(site == "Carrat" & date > ymd("20200715")),
         !(site == "Moulin Piquet la Rapine" & date > ymd("20200714")))

ggplot() +
  geom_line(data = filter(df_aug, year == 2020),
            aes(x = datetime,
                y = DO, group = site),
            alpha = 0.5)+
  geom_point(data = filter(df_aug, color == TRUE, year == 2020),
             aes(x = datetime, y = DO, color = color)) +
  stat_smooth(data = filter(df_aug, color == TRUE, year == 2020), 
              aes(x = datetime, y = DO),
              method = "lm", se = FALSE) +
  ggpubr::stat_regline_equation(data = filter(df_aug, color == TRUE, year == 2020), 
                                aes(x = datetime, y = DO)) +
  geom_hline(yintercept = 4, linetype = "dashed", color = "red") +
  facet_wrap(~site)+
  scale_color_manual(values = "blue") +
  theme_bw() +
  theme(legend.position = "none")


ggsave("Figures/regression_do_min_2020_drying_july.png",
       dpi = 600,
       width = 18.4,
       height  = 12,
       units = "cm",
       device = "png")


ggplot() +
  geom_line(data = filter(df_aug, year == 2019),
            aes(x = datetime,
                y = DO, group = site),
            alpha = 0.5)+
  geom_point(data = filter(df_aug, color == TRUE, year == 2019),
             aes(x = datetime, y = DO, color = color)) +
  stat_smooth(data = filter(df_aug, color == TRUE, year == 2019), 
              aes(x = datetime, y = DO),
              method = "lm", se = FALSE) +
  ggpubr::stat_regline_equation(data = filter(df_aug, color == TRUE, year == 2019), 
                                aes(x = datetime, y = DO)) +
  geom_hline(yintercept = 4, linetype = "dashed", color = "red") +
  facet_wrap(~site)+
  scale_color_manual(values = "blue") +
  theme_bw() +
  theme(legend.position = "none")

ggsave("Figures/regression_do_min_2019_drying_september.png",
       dpi = 600,
       width = 18.4,
       height  = 12,
       units = "cm",
       device = "png")








df2 <- df %>%
  bind_rows(readRDS("C:/Users/diamo/Dropbox/Projects/Loire_headwaters/Headwaters/Data/headwaters_data_clean") %>%
              rename(site = Site, watershed = Subwatershed, q_mmh = q_mmd, lux_water = lux) %>%
              mutate(date = date(datetime),
                     year= year(date))) %>%
  group_by(site) %>%
  filter(oow == "no" | is.na(oow)) %>%
  mutate(DOhyp = if_else(DO <4, 1, 0)) %>%
  group_by(site) %>%
  arrange(site, datetime) %>%
  mutate(hyp_l = sequence(rle(DOhyp)$lengths),
         hyp_change = if_else(lag(DOhyp) != DOhyp, 1, 0)) %>%
  filter(DOhyp == 1) %>%
  mutate(hyp_pd = cumsum(hyp_change))

df2 %>%
  mutate(hour = hour(datetime)) %>%
  ggplot(aes(x = hour)) + 
  geom_bar() +
  # facet_wrap(~site, ncol = 3) +
  theme_bw()+
  labs(y = "hours of hypoxia",
       x = "hour of day",
       title = "2019–2020")

ggsave("Figures/hypoxia_by_hour_2019_2020.png",
       dpi = 600,
       width = 12,
       height  = 10,
       units = "cm",
       device = "png")


df2 %>%
  mutate(month = month(datetime)) %>%
  ggplot(aes(x = month)) + 
  geom_bar() +
  scale_x_continuous(breaks = seq(3,11,1)) +
  # facet_wrap(~site, ncol = 3) +
  theme_bw()+
  labs(y = "hours of hypoxia",
       x = "month",
       title = "2019–2020")

ggsave("Figures/hypoxia_by_month_2019_2020.png",
       dpi = 600,
       width = 12,
       height  = 10,
       units = "cm",
       device = "png")







# Rainfall event


df_april_event <- df %>%
  bind_rows(readRDS("C:/Users/diamo/Dropbox/Projects/Loire_headwaters/Headwaters/Data/headwaters_data_clean") %>%
              rename(site = Site, watershed = Subwatershed, q_mmh = q_mmd, lux_water = lux) %>%
              mutate(date = date(datetime),
                     year= year(date))) %>%
  filter(between(date, ymd(20200416), ymd(20200420))) %>%
  filter(site_code %in% c("tor076",
                          "loi132",
                          "loi039",
                          "cha057",
                          "pot025",
                          "coi071")) %>%
  group_by(site_code) %>%
  mutate(DO = imputeTS::na_kalman(DO))
# group_by(site) %>%
# filter(sum(DOhyp, na.rm = T) > 1)

c = ggplot(data = df_april_event,
           aes(x = datetime,
               y = DO)) +
  geom_line()+
  facet_wrap(~site)+
  # scale_color_viridis_d()+
  theme_bw()
c


d = ggplot(data = df_april_event,
           aes(x = datetime,
               y = rain_mm)) +
  geom_line(color = "blue") +
  scale_y_continuous(position = "right") +
  facet_wrap(~site)+
  theme_bw() +
  theme(panel.background = element_rect(fill='transparent'), #transparent panel bg
        plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
        panel.grid.major = element_blank(), #remove major gridlines
        panel.grid.minor = element_blank(), #remove minor gridlines
        legend.background = element_rect(fill='transparent'), #transparent legend bg
        legend.box.background = element_rect(fill='transparent')) #transparent legend panel)
b
library(patchwork)
layout <- c(
  area(t = 1, l = 1, b = 3, r = 4),
  area(t = 1, l = 1, b = 3, r = 4)
)
c+ d+ 
  plot_layout(design = layout)

ggsave("Figures/april_event_DO.png",
       dpi = 600,
       width = 18.4,
       height  = 12,
       units = "cm",
       device = "png")








# # Some quick calculations
# df <- df %>%
#   mutate(date = date(datetime),
#          month = month(datetime),
#          DOhyp = if_else(DO_per < 50, 1, 0)) #define hypoxia as less than 50% saturation (Carter et al. 2021)
# 
# 
# 
# # Calculate daily stats by site
# df_daily <- df %>%
#   group_by(site, date) %>%
#   summarize(DOmax = max(DO_per, na.rm = T),
#             DOmin = min(DO_per, na.rm = T),
#             DOamp = DOmax - DOmin,
#             DOmean = mean(DO_per, na.rm = T),
#             DOhypn = round(sum(DOhyp, na.rm = T) / n() *100, 2)) #percentage of day that is hypoxic
# 
# # Summary of that data
# df_daily_sum <- df_daily %>%
#   pivot_longer(cols = -c(site, date)) %>%
#   group_by(site, name) %>%
#   filter(!is.infinite(value)) %>%
#   summarize(mean = mean(value, na.rm = T),
#             sd = sd(value, na.rm = T)) %>%
#   ungroup() %>%
#   mutate(name = as.factor(name),
#          site_p = reorder_within(site, mean, name))
# 
# # Quick summary graph
# df_daily %>%
#   pivot_longer(cols = -c(site, date)) %>%
#   left_join(df_daily_sum) %>%
#   ggplot(aes(x = site_p,
#              y = value)) +
#   facet_wrap(~name, scales = "free") +
#   stat_summary() +
#   # geom_boxplot(show.legend = FALSE) + 
#   # geom_errorbar(aes(ymax = mean + sd, ymin = mean - sd)) +
#   coord_flip() +
#   scale_x_reordered() +
#   scale_y_continuous(expand = c(0,0)) +
#   theme_bw() +
#   labs(x = "")
# 
# ggsave("Z:/RHypoxie/Figures/basic_DO_stats.png",
#        dpi = 600,
#        width = 36,
#        height  = 18.4,
#        units = "cm",
#        device = "png")
# 
# # Hypoxia period lengths
# df_hyp <- df %>%
#   group_by(site) %>%
#   arrange(site, datetime) %>%
#   mutate(hyp_l = sequence(rle(DOhyp)$lengths),
#          hyp_change = if_else(lag(DOhyp) != DOhyp, 1, 0)) %>%
#   filter(DOhyp == 1) %>%
#   mutate(hyp_pd = cumsum(hyp_change))
# 
# # summarize that by mean length of time of hypoxia
# ggplot(data = df_hyp,
#        aes(x = fct_reorder(site, hyp_l, .fun = mean),
#            y = hyp_l)) +
#   stat_summary() +
#   coord_flip() +
#   theme_bw() +
#   labs(x = "",
#        y = "mean length of hypoxia (hours)")
# 
# ggsave("Z:/RHypoxie/Figures/mean_length_hypoxia.png",
#        dpi = 600,
#        width = 36,
#        height  = 18.4,
#        units = "cm",
#        device = "png")
# 
# # summarize that by number of events
# df_hyp %>%
#   summarize(pds = max(hyp_pd, na.rm = T)) %>%
#   ggplot(aes(x = fct_reorder(site, pds),
#            y = pds)) +
#   geom_point() +
#   scale_y_continuous(breaks = seq(0,15,2)) +
#   # stat_summary(fun = max) +
#   coord_flip() +
#   theme_bw() +
#   labs(x = "",
#        y = "number of hypoxic events")
# 
# ggsave("Z:/RHypoxie/Figures/number_hypoxic_events.png",
#        dpi = 600,
#        width = 36,
#        height  = 18.4,
#        units = "cm",
#        device = "png")
# 
# # Exceedance curves by site
# df_ex <- df %>%
#   group_by(site) %>%
#   transmute(DO_per = DO_per,
#             cdf = cume_dist(DO_per))
# 
# ggplot(data = df_ex,
#        aes(x = 1 - cdf, y = DO_per)) +
#   geom_line() +
#   theme_bw() +
#   facet_wrap(~site) +
#   labs(x = "exceedance probability",
#        y = expression(DO[sat]~"(%)"))
# 
# ggsave("Z:/RHypoxie/Figures/DO_exceedance_curves.png",
#        dpi = 600,
#        width = 36,
#        height  = 18.4,
#        units = "cm",
#        device = "png")
# 
# 
# # Monthly summary
# mutate(df, month = month(datetime)) %>%
#   group_by(month) %>%
#   filter(between(DO_per,
#                  quantile(DO_per, 0.01, na.rm = T),
#                  quantile(DO_per, 0.95, na.rm = T))) %>%
#   ggplot(aes(x = month,
#              y = DO_per,
#              group = month)) +
#   geom_violin() +
#   scale_x_continuous(breaks = seq(1,10,1)) +
#   stat_summary() +
#   theme_bw() +
#   labs(x = "month",
#        y = expression(DO[sat]~"(%)"))
# 
# ggsave("Z:/RHypoxie/Figures/monthly_distributions.png",
#        dpi = 600,
#        width = 36,
#        height  = 18.4,
#        units = "cm",
#        device = "png")
# 
# 
# # discharge
# distinct(df, datetime, Q_grez, Q_rat, Q_char) %>%
#   pivot_longer(cols =-datetime) %>%
#   ggplot(aes(x = datetime,
#              y = value,
#              color = name)) +
#   geom_line() +
#   scale_x_datetime(date_breaks = "1 month") +
#   theme_bw() +
#   labs(x = "",
#        y = expression("Q ("*m^3~s^{-1}*")"))
# 
# ggsave("Z:/RHypoxie/Figures/discharge_comparison.png",
#        dpi = 600,
#        width = 36,
#        height  = 18.4,
#        units = "cm",
#        device = "png")
# 
# 
# # Some time series
# df_daily %>%
#   transmute(max = slider::slide_dbl(DOmax, mean, before = 7, na_rm = T),
#          min = slider::slide_dbl(DOmin, mean, before = 7, na_rm = T),
#          mean = slider::slide_dbl(DOmean, mean, before = 7, na_rm = T),
#          date = date) %>%
#   pivot_longer(cols = max:mean) %>%
#   mutate(value = if_else(is.infinite(value), NA_real_, value)) %>%
#   ggplot(aes(x = date,
#              y = value,
#              color = name)) +
#   geom_line() +
#   facet_wrap(~site, scales = "free_y") +
#   theme_bw() +
#   labs(x = "",
#        y = expression(DO[sat]~"(%)"))
# 
# ggsave("Z:/RHypoxie/Figures/max_min_do_ts.png",
#        dpi = 600,
#        width = 36,
#        height  = 18.4,
#        units = "cm",
#        device = "png")
# 
# # day vs night
# df %>%
#   filter(DOhyp == 1) %>%
#   mutate(day = if_else(rad_wm2 == 0, "night", "day"),
#             date = date) %>%
#   group_by(site, day) %>%
#   summarize(n = n()) %>%
#   ungroup() %>%
#   group_by(site) %>%
#   summarize(prop = n / sum(n)) %>%
#   ggplot(aes(x = site,
#              y = prop)) +
#   geom_point() +
#   coord_flip() +
#   theme_bw() +
#   labs(x = "",
#        y = "proportion of ")

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