# -------------------------------------
# Author: Jake Diamond
# Purpose: Analyze headwater data for storm-induced hypoxia
# Date: 12 may 2022
# -------------------------------------
# Set working directory
setwd("Z:/RHypoxie")
# setwd("C:/Users/diamo/Dropbox/Projects/RHypoxie")

# Load libraries
library(lubridate)
library(scales)
library(patchwork)
library(tidyverse)

# Single event 
# Load data
df <- readRDS("Data/hourly_data_all.RDS")

# Some quick calculations
df <- df %>%
  mutate(date = date(datetime),
         month = month(datetime),
         year = year(datetime),
         solartime = streamMetabolizer::calc_solar_time(datetime, longitude),
         light = streamMetabolizer::calc_light(solartime, latitude, longitude),
         DOsat2 = if_else(is.na(DOsat2), DO/DO_per*100, DOsat2),
         q_mmh = if_else(is.na(q_mmh), q_mmd / 24, q_mmh),
         DOhyp = if_else(DO < 4, 1, 0))
# Event look along network ------------------------------------------------
plot_fun <- function(startdate, enddate) {
  df_event = df %>%
    filter(between(date, ymd(startdate), ymd(enddate))) %>%
    filter(!(site %in% c("ardevel amont vernus",
                         "vernus",
                         "ardevel aval vernus",
                         "vauxonne",
                         "sallarin",
                         "thielas",
                         "samsons amont thielas",
                         "samsons aval thielas",
                         "ribes",
                         "le rieu",
                         "potensinet",
                         "coizet",
                         "miloniere",
                         "thiollet"))) %>%
    group_by(site) %>%
    filter(!all(is.na(DO))) %>%
    arrange(datetime) %>%
    mutate(DO = imputeTS::na_kalman(DO))

  a1 = ggplot(data = df_event,
               aes(x = datetime,
                   y = DO_per,
                   color = log(area_km2),
                   group = site)) +
    geom_line(size = 1.2, alpha = 0.5)+
    facet_wrap(~watershed, nrow = 1) + 
    scale_x_datetime(date_breaks = "2 days",
                     date_labels = "%m/%d") +
    # facet_wrap(~fct_reorder(site, number))+
    scale_color_viridis_c()+
    labs(y = "DO (% sat.)",
         title = ymd(startdate)) +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          legend.position = "bottom")
  
  a2 = ggplot(data = df_event,
              aes(x = datetime,
                  y = spc,
                  color = log(area_km2),
                  group = site)) +
    geom_line(size = 1.2, alpha = 0.5)+
    facet_wrap(~watershed, nrow = 1) + 
    scale_x_datetime(date_breaks = "2 days",
                     date_labels = "%m/%d") +
    # facet_wrap(~fct_reorder(site, number))+
    scale_color_viridis_c()+
    labs(y = expression("Sp. Cond. ("*mu*S~cm^{-1}*")")) +
    theme_bw() +
    theme(axis.title.x = element_blank())
  
  b1 = ggplot(data = distinct(df_event, watershed, site, datetime, q_mmh),
               aes(x = datetime,
                   y = q_mmh,
                   group = site)) +
    geom_line(color = "blue") +
    scale_y_continuous(position = "right") +
    facet_wrap(~watershed, nrow = 1) +
    # facet_wrap(~fct_reorder(site, number))+
    theme_minimal() +
    labs(y = expression("q (mm"~h^{-1}*")")) +
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
  if(!all(is.na(df_event$rain_mm))){
    c1 = ggplot(data = distinct(ungroup(df_event), watershed, datetime, rain_mm),
                aes(x = datetime,
                    y = rain_mm)) +
      geom_col() +
      scale_y_reverse(position = "right") +
      facet_wrap(~watershed, nrow = 1) +
      labs(y = expression("rain (mm"~h^{-1}*")")) +
      # facet_wrap(~fct_reorder(site, number))+
      theme_minimal() +
      theme(panel.background = element_rect(fill='transparent', color = NA), #transparent panel bg
            plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
            panel.grid.major = element_blank(), #remove major gridlines
            panel.grid.minor = element_blank(), #remove minor gridlines
            legend.background = element_rect(fill='transparent'), #transparent legend bg
            legend.box.background = element_rect(fill='transparent'),
            strip.background = element_blank(),
            strip.text = element_blank(),
            axis.text.x = element_blank(),
            axis.title.x = element_blank())
  } else{
    c1 = ggplot() + theme_void()
  }
  
  layout <- c(
    area(t = 1, l = 1, b = 8, r = 4),
    area(t = 6, l = 1, b = 8, r = 4),
    area(t = 1, l = 1, b = 2, r = 4)
  )
  DO = a1 + b1 + c1 + plot_layout(design = layout)
  if(!all(is.na(df_event$spc))){
    spc = a2 + b1 + c1 + plot_layout(design = layout)
    event_fig = DO / spc + plot_layout(guides = "collect") & theme(legend.position = 'bottom')
    return(event_fig)
  } else{
    event_fig = DO
    return(event_fig)
  }
}

plot_fun("20210618", "20210626")
ggsave(filename = "Figures/storm_event_examples/20210622.png",
       dpi = 1200,
       width = 22,
       height = 16,
       units = "cm")

# timing of do max and min ------------------------------------------------

df_t <- df %>%
  group_by(site, date) %>%
  filter(DO_per == max(DO_per, na.rm = T)) %>%
  summarize(tmax = hour(solartime))


testp <- df %>%
  filter(site == "coise larajasse",
         date %in% c(ymd(20200408), ymd(20200409), ymd(20200527), ymd(20200528))) %>%
  mutate(solarhour = hour(solartime)) %>%
  arrange(solartime) %>%
  group_by(month) %>%
  mutate(t = row_number() %% 24)

ggplot(data = testp,
       aes(x = solarhour,
           y = DO_per,
           linetype = as.factor(date))) +
  geom_line() +
  theme_bw() +
  scale_x_continuous(breaks = seq(0,23,4)) +
  labs(y = "DO (% sat.)",
       x = "solar time",
       title = "coise larajasse")

ggsave(filename = "Figures/DO_inversion_examples/coise_larajasse_april_may.png",
       dpi = 1200,
       width = 18.4,
       height = 12,
       units = "cm")
  

# Storms ------------------------------------------------------------------



st_meta = filter(df_hyp, type == "storm_down") %>%
  mutate(start = lubridate::force_tz(start, tzone = "Europe/Berlin"),
         end = lubridate::force_tz(end, tzone = "Europe/Berlin"),
         recovery_start = lubridate::force_tz(recovery_start, tzone = "Europe/Berlin"),
         recovery_end = lubridate::force_tz(recovery_end, tzone = "Europe/Berlin"))

df_st <- ungroup(df_loire) %>%
  select(-position, -number, -confluence) %>%
  distinct() %>%
  fuzzyjoin::fuzzy_semi_join(st_meta,
                             by = c("site_code" = "site_code",
                                    "datetime" = "start",
                                    "datetime" = "end"),
                             match_fun = list(`==`, `>=`, `<=`))


df_st_p <- df_st %>%
  distinct() %>%
  group_by(site) %>%
  # filter(oow == "no" | is.na(oow)) %>%
  mutate(DOhyp = if_else(DO <4, 1, 0),
         year = year(datetime)) %>%
  mutate(group = cumsum(c(1, diff(datetime) > 1))) %>%
  group_by(site, group) %>%
  mutate(hour = row_number() - 1) %>%
  group_by(site, date, group) %>%
  mutate(color = min(DO) == DO,
         color = if_else(is.na(color), FALSE, color),
         gs = paste(group, site))

ggplot() +
  geom_line(data = df_st_p,
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
  labs(title = "storm events leading to hypoxia",
       x = "hours after storm",
       y = "DO (mg/L)")


ggsave(filename = "Z:/RHypoxie/Figures/storm_hypoxia_mgl.png",
       dpi = 1200,
       height = 18,
       width = 28,
       units = "cm")



# Time until hypoxia
df_th_st <- df_st_p %>%
  filter(DOhyp == 1) %>%
  ungroup() %>%
  group_by(site, group) %>%
  filter(hour == min(hour)) %>%
  left_join(ungroup(df_st_p) %>%
              group_by(site, group) %>%
              filter(hour == 0) %>%
              select(Q_start = Q))

ggplot(data = df_th_st,
       aes(x = log(Q_start),
           y = hour))+
  geom_point() +
  theme_bw() +
  # geom_abline(intercept = 0, slope = 24) +
  labs(title = "time until first instance of hypoxia after storm",
       y = "time until hypoxia (hours)",
       x = "starting discharge (m3/s)")