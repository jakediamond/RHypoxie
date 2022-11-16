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

# Overall hypoxia info ----------------------------------------------------
ungroup(df) %>%
  filter(!is.na(DO)) %>%
  summarize(hy = sum(DOhyp, na.rm = T),
            n = n(),
            per = hy / n)

# Overall hypoxia percentages by site
df_hyp_per <- ungroup(df) %>%
  group_by(site) %>%
  summarize(hy = sum(DOhyp, na.rm = T),
            n = n(),
            per = hy / n)

# hypoxia run lengths
df_hyp_rls <- df %>%
  group_by(site) %>%
  arrange(site, datetime) %>%
  mutate(hyp_l = sequence(rle(DOhyp)$lengths),
         hyp_change = if_else(((lag(DOhyp, default = 0) != DOhyp) &
                                 (lag(DOhyp, 2, default = 0) != DOhyp)), 
                              1, 0)) %>%
  filter(DOhyp == 1) %>%
  mutate(hyp_pd = cumsum(replace_na(hyp_change, 0))) %>%
  mutate(year = year(date)) %>%
  ungroup()

# Median lengths of hypoxia
df_hyp_len <- ungroup(df_hyp_rls) %>%
  group_by(site, hyp_pd) %>%
  filter(hyp_l == max(hyp_l)) %>%
  ungroup() %>%
  group_by(site) %>%
  summarize(hyp_len_med = median(hyp_l))

# Median timing between hypoxic events
df_hyp_diff <- ungroup(df_hyp_rls) %>%
  group_by(site, hyp_pd) %>%
  filter(hyp_l == max(hyp_l) |
         hyp_l == min(hyp_l)) %>%
  ungroup() %>%
  group_by(site) %>%
  mutate(t_dif = if_else(hyp_pd - lag(hyp_pd) == 1, 
                         datetime - lag(datetime), NA_real_)) %>%
  summarize(hyp_t_dif_med = as.numeric(median(t_dif, na.rm = T) / 3600))

# Number of hypoxia periods
df_hyp_pds <- df_hyp_rls %>%
  group_by(site) %>%
  summarize(pds = max(hyp_pd, na.rm = T))

# probability of nighttime hypoxia given hypoxia
df_hyp_night <- df %>%
  filter(DOhyp == 1) %>%
  group_by(site) %>%
  mutate(night = if_else(light < 200, 1, 0)) %>%
  summarize(nhyp = sum(night == 1) / n())

# overall
df_hyp_all <- left_join(df_hyp_per, df_hyp_diff) %>%
  left_join(df_hyp_len) %>%
  left_join(df_hyp_pds) %>%
  left_join(df_hyp_night) %>%
  mutate_if(is.numeric, ~replace_na(., 0))

saveRDS(df_hyp_all, file.path("results", "hypoxia_summary.RDS"))

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
  labs(y = "hypoxia (%)",
       x = "month")
p_monthly_hyp

p_monthly_temp <- df %>%
  mutate(year = year(date)) %>%
  ggplot(aes(x = month,
             y = temp)) + 
  stat_summary(color = "red", geom = "line") +
  stat_summary(color = "red") +
  stat_summary(color = "transparent", fill = "transparent", geom = "bar") +
  facet_grid(~year, space = "free", scales = "free_x") +
  theme_second_axis +
  theme(axis.title = element_text(color = "red"),
        axis.text = element_text(color = "red")) +
  scale_x_continuous(breaks = seq(min(df$month), max(df$month), 1))+
  scale_y_continuous(position = "right") +
  # labs(y = expression("q (mm"~h^{-1}*")")) +
  labs(y = expression("stream temperature ("*degree*"C)"),
       x = "month")
p_monthly_temp

p_monthly_q <- df %>%
  mutate(year = year(date),
         q_mmd = if_else(is.na(q_mmd), q_mmh *24, q_mmd)) %>%
  ggplot(aes(x = date,
             y = q_mmd)) + 
  # geom_ribbon(fill = "blue", alpha = 0.4, ymin = 0, aes(ymax = q_mmd)) +
  stat_summary(fill = "blue", geom = "ribbon", alpha = 0.4, ymin = 0) +
  stat_summary(color = "transparent", fill = "transparent", geom = "bar") +
  # stat_summary(color = "blue") +
  facet_grid(~year, space = "free", scales = "free_x") +
  theme_second_axis +
  theme(axis.title = element_text(color = "blue"),
        axis.text = element_text(color = "blue")) +
  scale_y_continuous(position = "right") +
  scale_x_date(breaks = seq())
  # scale_x_continuous(breaks = seq(min(df$month), max(df$month), 1))+
  # labs(y = expression("q (mm"~h^{-1}*")")) +
  labs(y = expression("q ("*mm~d^{-1}*")"),
       x = "month")
p_monthly_q

p_month <- p_monthly_hyp + p_monthly_temp +
  plot_layout(design = layout) + labs(tag = "a")
p_month

ggsave(plot = p_month,
       filename = file.path("results", "Figures", "summaries", "hypoxia_summary.png"),
       dpi = 1200,
       width = 18.4,
       height = 16,
       units = "cm"
)

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
  right_join(df) %>%
  mutate(pds = if_else(is.na(pds), 0, pds)) %>%
  distinct(site, year, month, pds) %>%
  ggplot(aes(x = month,
             y = pds)) +
  stat_summary() +
  stat_summary(geom = "line") +
  stat_summary(color = "transparent", fill = "transparent", geom = "bar") +
  scale_x_continuous(breaks = seq(min(df$month), max(df$month), 1))+
  # geom_point() +
  facet_grid(~year, space = "free", scales = "free_x") +
  # scale_y_continuous(breaks = seq(0,15,2)) +
  # stat_summary(fun = max) +
  # coord_flip() +
  theme_bw() +
  theme(strip.background = element_blank(),
        panel.grid = element_blank()) +
  labs(x = "month",
       y = "# hypoxic events per site")
p_num_events

p_length_events <- ungroup(df_hyp) %>%
  group_by(site, hyp_pd) %>%
  filter(hyp_l == max(hyp_l)) %>%
  ungroup() %>%
  filter(between(hyp_l, quantile(hyp_l, 0.05), quantile(hyp_l, 0.95))) %>%
  ggplot(aes(x = month,
             y = hyp_l)) +
  # geom_boxplot() +
  # geom_violin() +
  stat_summary(color = "dark grey") +
  stat_summary(geom = "line", color = "dark grey") +
  stat_summary(color = "transparent", fill = "transparent", geom = "bar") +
  # facet_grid(~year, space = "free", scales = "free_x") +
  scale_y_continuous(position = "right") +
  theme_second_axis + 
  theme(axis.text = element_text(color = "dark grey"),
        axis.title = element_text(color = "dark grey")) + 
  labs(x = "month",
       y = "mean hypoxia duration (hours)")
p_length_events
p_events <- p_num_events + p_monthly_q + 
  plot_layout(design = layout) + labs(tag = "b")
p_events


# Hypoxia by hour of day --------------------------------------------------
p_hour <- df %>%
  mutate(hour = hour(solartime)) %>%
  group_by(hour) %>%
  summarize(hy = sum(DOhyp, na.rm = T),
            n = n(),
            per = hy / n,
            light = mean(light, na.rm = T)) %>%
  ggplot() + 
  geom_col(aes(x = hour,
               y = per,
               group = hour,
               fill = light)) + 
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(0,23, 4)) +
  scale_fill_viridis_c(option = "cividis") +
  theme_bw() +
  theme(strip.background = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none") +
  labs(y = "hypoxia (%)",
       x = "hour of day",
       tag = "c")

# Monthly summary across all years ------------------------------------------------
p_month_all <- df_hyp %>%
  ggplot() + 
  geom_col(aes(x = month,
               y = DOhyp,
               group = month)) + 
  # geom_text(aes(x = 10, y = 0.18, label = sum(n), group = year)) +
  scale_y_continuous(breaks = seq(0, 7000, 1000)) +
  scale_x_continuous(breaks = seq(min(df$month), max(df$month), 1)) +
  theme_bw() +
  theme(strip.background = element_blank(),
        panel.grid = element_blank()) +
  labs(y = "total hours of hypoxia",
       x = "month")
p_month_all

p_hours_all <- p_month_all + p_length_events + plot_layout(design = layout) +
  labs(tag = "d")
p_hours_all


p_month_min <- df %>%
  group_by(site, date, month) %>%
  summarize(min = min(DO, na.rm = T)) %>%
  ungroup() %>%
  filter(between(min, quantile(min, 0.05), quantile(min, 0.95))) %>%
  ggplot(aes(x = month,
             y = min,
             group = month)) + 
  geom_violin() +
  # geom_boxplot() +
  # geom_text(aes(x = 10, y = 0.18, label = sum(n), group = year)) +
  # scale_y_continuous(breaks = seq(0, 7000, 1000)) +
  scale_x_continuous(breaks = seq(min(df$month), max(df$month), 1)) +
  theme_bw() +
  theme(strip.background = element_blank(),
        panel.grid = element_blank()) +
  labs(y = "total hours of hypoxia",
       x = "month")
p_month_min
# Graph of all summary data -----------------------------------------------
p_all <- ((p_month * theme(axis.text.x = element_blank(),
                          axis.title.x = element_blank(),
                          plot.tag.position = c(0.08, 0.8))/ 
            (p_events * theme(strip.text.x = element_blank(),
                              plot.tag.position = c(0.08, 0.95))))) / 
  ((p_hour * theme(plot.tag.position = c(0.18, 0.95)) + 
      p_hours_all * theme(plot.tag.position = c(0.18, 0.95))))
p_all


ggsave(plot = p_all,
       filename = file.path("results", "Figures", "Fig2.svg"),
       dpi = 1200,
       width = 18.4,
       height  = 16,
       units = "cm",
       device = "svg")



# Day vs night, summer vs not ----------------------------------------------------------------

df %>%
  mutate(day = if_else(light > 200, "day", "night")) %>%
  group_by(day) %>%
  summarize(hyp = sum(DOhyp, na.rm = T))

df %>%
  mutate(day = if_else(between(month, 7, 9), "summer", "not")) %>%
  group_by(day) %>%
  summarize(hyp = sum(DOhyp, na.rm = T))


# Daily stats -------------------------------------------------------------
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



# Other -------------------------------------------------------------------






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

# Exceedance curves -------------------------------------------------------
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
                 quantile(DO_per, 0.0, na.rm = T),
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
  mutate(day = if_else(light < 200, "night", "day"),
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


