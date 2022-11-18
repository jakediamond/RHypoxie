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
# calculate duration and frequency of drying --------------------------------------------
# Data and metadata about rewetting events
df_rewet <- df %>%
  mutate(oow = if_else(site %in% 
                         c("charpassone chanin", 
                           "charpassonne moulin marcel", 
                           "carrat") & date == ymd(20200921), 
                       "yes", oow)) %>%
  group_by(site) %>%
  arrange(site, datetime) %>%
  mutate(oownum = if_else(oow == "no" | is.na(oow), 0, 1),
         group = cumsum(c(1, abs(diff(oownum))))) %>%
  ungroup() %>%
  group_by(site, group) %>%
  mutate(length = cumsum(oownum),
         type = if_else(group %% 2 != 0, "rewetting", "dry")) %>%
  filter(group > 1)

# How many total rewetting events
df_rewet %>%
  filter(type == "rewetting") %>%
  mutate(sitegroup = paste0(group, site)) %>%
  ungroup() %>%
  distinct(sitegroup)

# How many distinct sites
df_rewet %>%
  filter(type == "rewetting") %>%
  mutate(sitegroup = paste0(group, site)) %>%
  ungroup() %>%
  distinct(site)

# How many rewetting hypoxia
df_rewet %>%
  filter(type == "rewetting",
         sum(DO<3,na.rm = T) > 0) %>%
  mutate(sitegroup = paste0(group, site)) %>%
  ungroup() %>%
  distinct(site)

# Data about duration and frequency of drying before rewetting
meta_rewet <- df_rewet %>%
  slice(which.max(datetime)) %>%
  ungroup() %>%
  group_by(site, year) %>%
  mutate(dur_dry = if_else(type == "rewetting", lag(length), 0),
         freq_dry = if_else(type == "rewetting" & group > 1, lag(group) / 2, 0)) %>%
  # ungroup() %>%
  filter(type == "rewetting" & group > 1) %>%
  select(site, site_code, group, dur_dry, freq_dry, datetime)

# only rewetting events
df_events <- filter(ungroup(df_rewet), type == "rewetting")

# Summary for text
ungroup(meta_rewet) %>%
  filter(!(site %in% c("mercier amont presles", "thiollet"))) %>%
  summarize(meandur = mean(dur_dry/24, na.rm = T),
            meanfreq = mean(freq_dry, na.rm = T),
            sddur = sd(dur_dry/24, na.rm = T),
            sdfreq = sd(freq_dry, na.rm = T))

# meta_rewet %>%
#   filter(!(site %in% c("mercier amont presles", "thiollet"))) %>%
#   ggplot(aes(x = dur_dry / 24)) +
#   geom_density(fill = "orange", alpha = 0.5) +
#   theme_bw() +
#   scale_x_continuous(limits = c(0,10)) +
#   geom_vline(aes(xintercept = median(dur_dry / 24, na.rm = T))) +
#   labs(x = "duration of dry period before rewetting (days)")


# ggsave(plot = p_hist_droprates, 
#        filename = file.path("results", "Figures", "rewetting_summaries", "rewet_droprate_hist.png"),
#        dpi = 1200,
#        height = 9.2,
#        width = 9.2,
#        units = "cm")
# 
# meta_rewet %>%
#   filter(!(site %in% c("mercier amont presles", "thiollet"))) %>%
#   ggplot(aes(x = freq_dry)) +
#   geom_density(fill = "orange", alpha = 0.5) +
#   theme_bw() +
#   # scale_x_continuous(limits = c(0,10)) +
#   geom_vline(aes(xintercept = median(freq_dry, na.rm = T))) +
#   labs(x = "number of dry periods before rewetting")
# 
# 
# ggsave(plot = p_hist_droprates, 
#        filename = file.path("results", "Figures", "rewetting_summaries", "rewet_droprate_hist.png"),
#        dpi = 1200,
#        height = 9.2,
#        width = 9.2,
#        units = "cm")
# Plots -------------------------------------------------------------------
df_re_p <- df_re %>% #change to df_events to get everything
  distinct() %>%
  group_by(site) %>%
  # filter(oow == "no" | is.na(oow)) %>%
  mutate(DOhyp = if_else(DO < 3, 1, 0),
         year = year(datetime)) %>%
  mutate(group = cumsum(c(1, diff(datetime) > 1))) %>%
  group_by(site, group) %>%
  mutate(hour = row_number() - 1,
         sitegroup = paste0(group, site)) %>%
  left_join(meta_geom)

# # Text to avoid facet strips
# ann_text <- data.frame(time = 0.5, DO = 10, 
#                        label = c("pool", "riffle", "run"),
#                        geomorph = c("pool", "riffle","run"))
ann_text <- data.frame(time = 0.5, DO = 10,
                       label = c("pool", "run"),
                       geomorph = c("pool", "run"))


df_p <- ungroup(df_re_p) %>%
  drop_na(geomorph) %>%
  drop_na(DO) %>%
  # filter(geomorph != "riffle", strahler < 4) %>%
  group_by(site, sitegroup, geomorph) %>%
  # filter(between(hour, 0, 24*6),
  #        n() > 24, min(DO) < 3) %>%
  filter(n() > 24, n() < 24*8, min(DO) < 5) #%>%
  nest() %>%
  group_by(geomorph) %>%
  slice_sample(n = 3) %>%
  unnest()

# Time series plot
p_re_ts <- ggplot() +
  geom_line(data = df_p,
            aes(x = hour / 24,
                y = DO, group = sitegroup,
                color = as.factor(strahler)),
            alpha = 0.7,
            size = 1.2) +
  geom_hline(yintercept = 3, linetype = "dashed") +
  facet_grid(rows = vars(geomorph)) +
  scale_x_continuous(breaks = seq(-3,8, 1)) +
  # geom_vline(xintercept = 0) +
  scale_color_manual(name = "Strahler", values = c("black", "#0072B2", "#D55E00")) +
  geom_text(data = ann_text,
            aes(x = time, y = DO, label = label)) +
  theme_bw() +
  theme(legend.position = c(0.8,0.3),
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank()) +
  labs(x = "days after rewetting",
       y = expression("DO (mg "*L^{-1}*")"))

p_re_ts

# rate of drop ------------------------------------------------------------
df_m <- df_events %>%
  drop_na(DO) %>%
  filter(oow != "yes") %>%
  group_by(site, group) %>%
  mutate(hour = row_number() - 1) %>%
  select(site, group, hour, DO_per, DO, temp) %>%
  pivot_longer(cols = starts_with("DO")) %>%
  group_by(site, group, name) %>%
  # drop_na() %>%
  filter(hour < 48) %>%
  nest() %>%
  mutate(mod = map(data, ~lm(value~hour, data = .)),
         t = map(mod, broom::tidy)) %>%
  select(t) %>%
  unnest(cols = t)

df_m2 <- df_m %>%
  pivot_wider(names_from = term, values_from = estimate:p.value)

# text for figure
drop_txt <- ungroup(df_m2) %>%
  filter(name == "DO",
         p.value_hour < 0.05) %>%
  summarize(mean = mean(estimate_hour * 24, na.rm = T),
            sd = sd(estimate_hour * 24, na.rm = T),
            med = median(estimate_hour * 24, na.rm = T)) %>%
  mutate(x = -3, y = 0.33,
         txt = paste0("median = ",round(med,1),
                      "\n", "mean±sd = ",
                      round(mean,1), "±", round(sd,1)))

ungroup(df_m2) %>%
  filter(name == "DO",
         p.value_hour < 0.05,
         estimate_hour < 0 ) %>%
  summarize(mean = mean(estimate_hour * 24, na.rm = T),
            sd = sd(estimate_hour * 24, na.rm = T),
            med = median(estimate_hour * 24, na.rm = T))

p_hist_dochange <- df_m2 %>%
  filter(name == "DO",
         p.value_hour < 0.05) %>%
  ggplot(aes(x = estimate_hour * 24)) +
  geom_density(fill = "black", alpha = 0.5) +
  theme_classic() +
  geom_text(data=drop_txt, aes(x =x, y =y, label = txt)) +
  geom_vline(aes(xintercept = median(estimate_hour * 24, na.rm = T))) +
  labs(x = expression("DO change after rewetting (mg "*L^{-1}~d^{-1}*")"),
       y = "density")
p_hist_dochange

ggsave(plot = p_hist_dochange, 
       filename = file.path("results", "Figures", "rewetting_summaries", "rewet_dochange_hist.png"),
       dpi = 1200,
       height = 9.2,
       width = 9.2,
       units = "cm")


# Join meta data to rewetting do change data ----------------------------------------
df_rew <- df_m2 %>%
  left_join(meta_rewet) %>%
  filter(name == "DO",
         p.value_hour < 0.05) %>%
  mutate(dur_cat = case_when(
   dur_dry < 24 ~ "1d",
   dur_dry < 48 ~ "2d",
   dur_dry < 24*7 ~ "7d",
   dur_dry > 24*7 ~ ">7d",
   TRUE ~ "1d"
  )) %>%
  mutate(dur_cat = as.factor(dur_cat),
         dur_cat = fct_relevel(dur_cat, "1d", "2d", "7d", ">7d")) %>%
  filter(month(datetime) < 10,
         p.value_hour < 0.05)
  
p_pred <- ggplot(data= df_rew,
       aes(x = freq_dry,
           y = estimate_hour * 24,
           fill = dur_cat)) +
  geom_point(shape = 21) +
  guides(fill=guide_legend(ncol=2)) +
  # stat_summary_bin() +
  theme_classic() +
  geom_hline(yintercept = 0) +
  theme(legend.position = c(0.83,0.16), 
        legend.background = element_rect(fill = "transparent", color= "black"),
        legend.key.size = unit(0.4, 'cm')) + 
  scale_fill_viridis_d("dry length \n before rewetting", option = "magma") +
  labs(x = "# of dry periods before rewetting",
       y = expression("DO change after rewetting (mg "*L^{-1}~d^{-1}*")"))
p_pred
ggsave(filename = file.path("results", "Figures", "rewetting_summaries", "rewet_dochange_function_drying.png"),
       dpi = 1200,
       height = 9.2,
       width = 12.4,
       units = "cm")


# Plot everything -------------------------------------------------------------
(p_re_ts | (p_hist_dochange / p_pred)) + plot_annotation(tag_levels = "A")
ggsave(filename = file.path("results", "Figures", "rewetting_summaries", "fig5_v2.png"),
       dpi = 1200,
       height = 15,
       width = 18,
       units = "cm")

# Plots of time series ----------------------------------------------------
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



# All events --------------------------------------------------------------



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
  geom_hline(yintercept = 3, linetype = "dashed", color = "red") +
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

