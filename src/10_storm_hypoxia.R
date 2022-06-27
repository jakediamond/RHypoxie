# -------------------------------------
# Author: Jake Diamond
# Purpose: Analyze headwater data for storm-induced hypoxia
# Date: 12 may 2022
# -------------------------------------
# Load libraries
library(lubridate)
library(scales)
library(readxl)
library(patchwork)
library(tidyverse)

# Load all cleaned sensor data
df <- readRDS(file.path("data", "10_clean_data","hourly_data_all.RDS"))

# Load storm starts and lengths
storm_meta <- readRDS(file.path("data", "10_clean_data","storm_starts_lengths.RDS")) %>%
  filter(year < 2022,
         between(month(start.date), 3, 10)) %>%
  ungroup()


# Load hypoxia metadata
df_hyp <- read_xlsx("Z:/RHypoxie/Data/01_metadata/hypoxia_dates.xlsx")

# Load all hyporheic zone data for the Ratier amont Charbonnieres
df_hr <- read_excel(file.path("data", "02_sensor_data", "raw", "bdoh", "hyporheic_data.xlsx"))

# Clean it up to be same format as sensor data
df_hr <- df_hr %>%
  mutate(datetime = with_tz(datetime_UTC, "Europe/Berlin"), 
    hour = floor_date(datetime, "hour")) %>%
  group_by(hour) %>%
  summarize(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  rename(datetime = hour,
         spc = spc_uscm,
         DO = DO_mgL) %>%
  ungroup()

# Compare
df_storm <- filter(df, between(datetime, ymd_h(2021041500), ymd_h(2021043000)),
                   site == "ratier amont charbonnieres") %>%
  mutate(type = "surface") %>%
  bind_rows(filter(df_hr, between(datetime, ymd_h(2021041500), ymd_h(2021043000))) %>%
               mutate(type = "hyporheic"))

df_storm2 <- filter(df,
                   site == "ratier aval ribes") %>%
  mutate(type = "surface") %>%
  bind_rows(df_hr %>%
              mutate(type = "hyporheic"))

p <- ggplot(data = df_storm2,
       aes(x = datetime,
           y = spc,
           color = type)) +
  geom_line() +
  theme_bw()

plotly::ggplotly(p)

df_hrdif <- df_storm2 %>%
  mutate(date = date(datetime)) %>%
  group_by(date, type) %>%
  summarize(maxt = datetime[max(DO, na.rm = T)]) %>%
  mutate(hr = hour(maxt)) %>%
  ungroup() %>%
  select(-maxt) %>%
  pivot_wider(values_from = hr, names_from = type) %>%
  mutate(dift = surface - hyporheic)

ggplot(data = df_hrdif,
       aes(x = date,
           y = dift)) +
  geom_point() +
  geom_line() +
  theme_bw()

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

ggplot(data = filter(df, site == "ruisseau de violay",
                     between(datetime, ymd_h("2020041500"), ymd_h("2020052000"))
                     )) +
  # geom_point(aes(y = q_mmh)) +
  # stat_summary(aes(x = date, y = lux / 100)) +
  geom_point(aes(x = datetime, y = lux / 500)) +
  geom_line(aes(x = datetime, y = DO_per)) +
  theme_bw()


# Event look along network ------------------------------------------------
plot_fun <- function(startdate, stormlength) {
  df_event = df %>%
    filter(between(datetime, 
                   startdate - hours(24*2), 
                   startdate + hours(stormlength))) %>%
    # filter(!(site %in% c("ardevel amont vernus",
    #                      "vernus",
    #                      "ardevel aval vernus",
    #                      "vauxonne",
    #                      "sallarin",
    #                      "thielas",
    #                      "samsons amont thielas",
    #                      "samsons aval thielas",
    #                      "ribes",
    #                      "le rieu",
    #                      "potensinet",
    #                      "coizet",
    #                      "miloniere",
    #                      "thiollet"))) %>%
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
  
  b1 = ggplot(data = distinct(df_event, watershed, site, datetime, q_mmd),
               aes(x = datetime,
                   y = q_mmd,
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

plot_fun(storm_meta$start.date[1], storm_meta$spell.length[1])


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

df_st <- ungroup(df) %>%
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



# theme for discharge plots
p_theme_q <- 
  theme_minimal() +
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
  area(t = 1, l = 1, b = 8, r = 4),
  area(t = 6, l = 1, b = 8, r = 4)
)



# Test
df_s <- filter(df,
               site_code %in% c("loi033", "viz062", "mar167", "coi350"),
               between(datetime, ymd_h("2019071900"), ymd_h("2019080300")))
p_do <- ggplot(data = df_s,
       aes(x = datetime,
           y = DO_per,
           group = site_code)) +
  geom_line(color = "grey") +
  theme_bw()
p_do


p_q <- ggplot(data = df_s,
               aes(x = datetime,
                   y = q_mmd,
                   group = site_code)) +
  geom_line(color = "blue") +
  scale_y_continuous(position = "right") +
  labs(y = expression("q (mm"~d^{-1}*")")) +
  p_theme_q
p_q

p_do + p_q + plot_layout(design = layout)






# Test of spc and DO entropy ----------------------------------------------
ent.fun <- function(x) {
  runmean = slider::slide_dbl(x, mean, na.rm = T, .before = Inf)
  tosum = (x / runmean) * log(x / runmean)
  ent = slider::slide_dbl(tosum, sum, na.rm = T)
}

df_e <- df %>%
  arrange(site, datetime) %>%
  group_by(site) %>%
  mutate(DOent = ent.fun(DO_per),
         spcent = ent.fun(spc))

df_e2 <- filter(df_e, spcent != 0 )




ggplot(data=filter(df_e, site == "moulin piquet aval",
                   between(datetime, ymd_h("2020091200"), ymd_h("2020092500"))),
       aes(x = cond,
           y = DO_per,
           color = datetime)) +
  geom_path() +
  scale_color_viridis_c()


ggplot() +
  geom_line(data = filter(df_st_p, gs == "2 coise aval rieu"),
            aes(x = hour,
                y = DO, group = gs),
            alpha = 0.5)+
  geom_point(data = filter(df_st_p, gs == "2 coise aval rieu"),
             aes(x = hour, y = q_mmh * 100), color = "blue") +
  geom_hline(yintercept = 4, linetype = "dashed", color = "red") +
  # facet_wrap(~site, scales = "free_x")+
  # scale_color_manual(values = "blue") +
  theme_bw() +
  theme(legend.position = "none")

b

dats = c("2 charpassonne château de donzy", "2 coise aval rieu")

dat_st= filter(df_st_p, gs %in% c(dats)) %>%
  bind_rows(filter(df, site == "ratier aval ribes",
                   between(date, ymd("20210429"), ymd("20210506"))))
writexl::write_xlsx(dat_st, file.path("data", "storm_data_for_remi_model.xlsx"))
