# 
# Purpose: To evaluate how drying induces hypoxia in headwater streams
# Author: Jake Diamond
# Date: 18 January 2022
# 

# Load libraries
library(lubridate)
library(readxl)
library(scales)
library(tidytext)
library(tidyverse)

# Load data ---------------------------------------------------------------
# Load sensor data
df <- readRDS(file.path("data", "10_clean_data","hourly_data_all.RDS"))

# Load drying start dates and length
meta_rew <- readxl::read_excel(file.path("data", "01_metadata", "hypoxia_dates.xlsx")) %>%
  filter(type == "rewet_down") %>%
  mutate(start = lubridate::force_tz(start, tzone = "Europe/Berlin"),
         end = lubridate::force_tz(end, tzone = "Europe/Berlin"),
         recovery_start = lubridate::force_tz(recovery_start, tzone = "Europe/Berlin"),
         recovery_end = lubridate::force_tz(recovery_end, tzone = "Europe/Berlin"))

# Load metadata about pool and riffle
meta_geom <- readxl::read_excel(file.path("data", "07_geomorphology",
                                          "geomorphic_units.xlsx"))

# Get only rewetting data
df_re <- ungroup(df) %>%
  select(-position, -number, -confluence) %>%
  distinct() %>%
  fuzzyjoin::fuzzy_semi_join(meta_rew,
                             by = c("site_code" = "site_code",
                                    "datetime" = "start",
                                    "datetime" = "end"),
                             match_fun = list(`==`, `>=`, `<=`))

# Save the data for later use
saveRDS(df_re, file.path("data", "10_clean_data", "hypoxia_rewetting_new.RDS"))
df_re <- readRDS(file.path("data", "10_clean_data", "hypoxia_rewetting_new.RDS"))

df_re_p <- df_re %>%
  distinct() %>%
  group_by(site) %>%
  # filter(oow == "no" | is.na(oow)) %>%
  mutate(DOhyp = if_else(DO < 3, 1, 0),
         year = year(datetime)) %>%
  mutate(group = cumsum(c(1, diff(datetime) > 1))) %>%
  group_by(site, group) %>%
  mutate(hour = row_number() - 1) %>%
  group_by(site, date, group) %>%
  mutate(color = min(DO) == DO,
         color = if_else(is.na(color), FALSE, color),
         gs = paste(group, site))

ggplot() +
  geom_line(data = filter(df_re_p, gs!="3 charpassonne le tél"),
            aes(x = hour,
                y = DO, group = gs),
            alpha = 0.5)+
  # geom_point(data = filter(df_re_p, color == TRUE, gs!="3 charpassonne le tél"),
  #            aes(x = hour, y = DO, color = color)) +
  # stat_smooth(data = filter(df_re_p, color == TRUE, gs!="3 charpassonne le tél"), 
  #             aes(x = hour, y = DO, group = gs),
  #             method = "lm", se = FALSE) +
  # ggpubr::stat_regline_equation(data = filter(df_dry_p, color == TRUE), 
  #                               aes(x = hour, y = DO, group = gs)) +
  geom_hline(yintercept = 4, linetype = "dashed", color = "red") +
  facet_wrap(~site, scales = "free_x")+
  scale_color_manual(values = "blue") +
  theme_bw() +
  theme(legend.position = "none") +
  labs(title = "rewetting patterns leading to hypoxia",
       x = "hours after rewetting",
       y = "DO (mg/L)")


ggsave(filename = "Z:/RHypoxie/Figures/rewetting_hypoxia_mgl.png",
       dpi = 1200,
       height = 18,
       width = 28,
       units = "cm")







# Plot of time series
p_ts_re <- ggplot() +
  geom_line(data = filter(df_re_p, year == 2020,
                          between(datetime, ymd_h(2020080100), ymd_h(2020093000))),#, datetime > ymd_h(2020071700),
                          # !between(datetime, ymd_h(2020080100), ymd_h(2020080300)),
                          # !between(datetime, ymd_h(2020082800), ymd_h(2020090100))),
            aes(x = datetime,
                y = DO, group = gs,
                color = log(area_km2))) +
  # geom_point(data = filter(df_dry_p, color == TRUE),
  #            aes(x = hour, y = DO, color = color)) +
  # geom_hline(yintercept = 3, linetype = "dashed", color = "red") +
  facet_grid(~month(datetime), scales = "free_x", space = "free")+
  scale_color_viridis_c() +
  # scale_color_manual(values = "blue") +
  theme_bw() +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        strip.background = element_blank(), strip.text = element_blank(),
        axis.title.x = element_blank())
p_ts_re

p_ts_re_q <- ggplot() +
  geom_line(data = filter(df, year == 2020, 
                          between(datetime, ymd_h(2020080100), ymd_h(2020093000)),#datetime > ymd_h(2020071700),
                          # siteq == "toranche_st_cyr_les_vignes",
                          siteq == "coise_le_nezel"),
                          # datetime < ymd_h(2020092100),
                          # !between(datetime, ymd_h(2020083100), ymd_h(2020090300)),
                          # !between(datetime, ymd_h(2020080100), ymd_h(2020080300)),
                          # !between(datetime, ymd_h(2020082800), ymd_h(2020090100))),
            aes(x = datetime,
                y = q_mmd,
                group = siteq),
            color = "blue") +
  p_theme_q +
  labs(y = expression("q (mm "*d^{-1}*")")) +
  scale_y_continuous(position = "right", limits = c(0,0.5)) +
  # geom_point(data = filter(df_dry_p, color == TRUE),
  #            aes(x = hour, y = DO, color = color)) +
  # geom_hline(yintercept = 3, linetype = "dashed", color = "red") +
  facet_grid(~month(datetime), scales = "free_x", space = "free")+
  scale_color_manual(values = "blue") +
  theme(strip.background = element_blank(), strip.text = element_blank(),
        axis.text.y = element_text(color = "blue"),
        axis.title.y = element_text(color = "blue"))
p_ts_re_q

p_ts_re_all <- p_ts_re + p_ts_re_q +
  plot_layout(design = layout)
p_ts_re_all
ggsave(plot = p_ts_re_all,
       filename = file.path("results", "Figures", "rewetting_summaries", "rewetting_ex_ts.png"),
       dpi = 1200,
       units = "cm",
       width = 18.4,
       height = 12)
