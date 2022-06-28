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
meta_storm <- readRDS(file.path("data", "10_clean_data","storm_starts_lengths.RDS")) %>%
  filter(year < 2022,
         between(month(start.date), 3, 10)) %>%
  ungroup()

# Load metadata about pool and riffle
meta_geom <- readxl::read_excel(file.path("data", "07_geomorphology","geomorphic_units.xlsx"))

# Subset the sensor data for storm periods
# Clean data for only drying hypoxia events and get times right
meta_storm <- meta_storm %>%
  mutate(start = with_tz(start.date, tzone = "Europe/Berlin") - hours(3*24),
         end = force_tz(start.date, tzone = "Europe/Berlin") + hours(7*24)) %>%
  filter(month(start.date) != 3) # don't want the very first one of the season

# Only get the data for the storm events
df_storm <- ungroup(df) %>%
  select(-position, -number, -confluence) %>%
  mutate(siteq = if_else(siteq == "ardieres_beaujeu", "ardieres", siteq)) %>%
  fuzzyjoin::fuzzy_left_join(meta_storm,
                             by = c("siteq" = "siteq",
                                    "datetime" = "start",
                                    "datetime" = "end"),
                             match_fun = list(`==`, `>=`, `<=`))

# clean up for just storms
df_storm_final <- df_storm %>%
  drop_na(start) %>%
  arrange(site, start.date, datetime) %>%
  group_by(site) %>%
  mutate(group = cumsum(c(1, diff(start) > 1)),
         sitegroup = paste0(group, site_code)) %>%
  ungroup() %>%
  group_by(sitegroup) %>%
  mutate(time = as.numeric((datetime - start.date) / 86400))
  
  

# Save the data for later use
saveRDS(df_storm_final, file.path("data", "10_clean_data", "hypoxia_storm_new.RDS"))
df_storm_final <- readRDS(file.path("data", "10_clean_data", "hypoxia_storm_new.RDS"))


# Plotting theme ----------------------------------------------------------
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





# Storms ------------------------------------------------------------------

df_st_p <- df_storm_final %>%
  mutate(DOhyp = if_else(DO < 3, 1, 0)) %>%
  filter(sum(DOhyp) > 1) %>%
  ungroup() %>%
  # group_by(sitegroup) %>%
  group_by(sitegroup, date) %>%
  mutate(color = min(DO) == DO,
         color = if_else(is.na(color), FALSE, color),
         gs = paste(group, site)) %>%
  left_join(select(meta_geom, -site))


ggplot() +
  geom_line(data = df_st_p,
            aes(x = time,
                y = DO, group = sitegroup)) +
  geom_hline(yintercept = 3, linetype = "dashed", color = "red") +
  facet_wrap(~site, scales = "free_x")+
  geom_vline(xintercept = 0) +
  # scale_color_manual(values = "blue") +
  theme_bw() +
  theme(legend.position = "none") +
  labs(title = "storm events leading to hypoxia",
       x = "days after storm",
       y = "DO (mg/L)")


ggsave(filename = "Z:/RHypoxie/Figures/storm_hypoxia_mgl.png",
       dpi = 1200,
       height = 18,
       width = 28,
       units = "cm")

# Dissolved oxygen at the beginning and lowest value for each flushing flow
df_diff <- df_st_p %>%
  group_by(sitegroup) %>%
  filter(time >= 0) %>%
  filter(time == 0 | rank(DO, ties.method = "first") == 1) %>%
  select(DO, DO_per, start.date, time) %>%
  mutate(type = if_else(DO == max(DO), "begin", "end")) %>%
  distinct() %>%
  pivot_wider(names_from = type, values_from = c(DO, DO_per, time)) %>%
  left_join(distinct(select(ungroup(df_st_p), sitegroup, strahler, geomorph))) %>%
  mutate(drop = DO_begin - DO_end)
  
p_drop <- ggplot(data = df_diff) +
  geom_segment(aes(y = DO_begin, x = time_begin,
                   yend = DO_end, xend = time_end,
                   color = drop),
               size = 1) + 
  geom_point(aes(y = DO_begin, x = time_begin,
                   # yend = DO_end, xend = time_end,
                   color = drop),
               size = 1) +
  geom_point(aes(y = DO_end, x = time_end,
                 # yend = DO_end, xend = time_end,
                 color = drop),
             size = 1) +
  # facet_wrap(~geomorph) +
  theme_bw() +
  scale_color_viridis_c(name = "DO drop") +
  labs(x = "days since peak flow",
       y = expression("dissolved oxygen (mg "*L^{-1}*")"))
p_drop
ggsave(plot = p_drop, 
       filename = file.path("results", "Figures", "storm_summaries", "storm_drop_summary.png"),
       dpi = 1200,
       height = 9.2,
       width = 12.4,
       units = "cm")

# Time between storms
df_storm_summary <- meta_storm %>%
  group_by(siteq, year) %>%
  mutate(interstorm_d = as.numeric(start.date - lag(start.date)))

# storm pulse increase 
df_sp1 <- ungroup(df_st_p) %>%
  group_by(sitegroup) %>%
  filter(time >= 0) %>%
  mutate(q_min = min(q_mmd, na.rm = T),
         q_max = max(q_mmd, na.rm = T)) %>%
  select(q_min, q_max) %>%
  distinct()

# storm pulse increase 
df_sp2 <- ungroup(df_st_p) %>%
  group_by(sitegroup) %>%
  filter(time == 0 | time == -2/24) %>%
  mutate(q_change = max(q_mmd, na.rm = T) - min(q_mmd, na.rm = T),
         q_max = max(q_mmd, na.rm = T)) %>%
  select(q_change) %>%
  distinct()




# Time until hypoxia
df_th_st <- ungroup(df_st_p) %>%
  group_by(sitegroup) %>%
  filter(time >= 0) %>%
  filter(DOhyp == 1) %>%
  filter(time == min(time)) %>%
  distinct() %>%
  left_join(df_sp1) %>%
  left_join(df_sp2) %>%
  left_join(df_storm_summary)

# histogram of drop rates
p_hist_droprates <- ggplot(data = df_diff,
       aes(x = drop / time_end))+
  geom_density(fill = "red", alpha = 0.5) + 
  theme_bw() +
  scale_x_continuous(limits = c(0, 10), breaks = seq(0,10,1)) +
  geom_vline(aes(xintercept = median(drop/time_end, na.rm = T))) +
  labs(x = expression("rate of DO decrease (mg "*L^{-1}~d^{-1}*")"))

ggsave(plot = p_hist_droprates, 
       filename = file.path("results", "Figures", "storm_summaries", "storm_droprate_hist.png"),
       dpi = 1200,
       height = 9.2,
       width = 9.2,
       units = "cm")

# histogram of drops
p_hist_timehyp <- ggplot(data = df_th_st,
       aes(x = time))+
  geom_density(fill = "blue", alpha = 0.5) + 
  theme_bw() +
  scale_x_continuous(limits = c(0, 7), breaks = seq(0,7,1)) +
  geom_vline(aes(xintercept = median(time, na.rm = T))) +
  labs(x = "time until hypoxia (days)")

ggsave(plot = p_hist_timehyp, 
       filename = file.path("results", "Figures", "storm_summaries", "storm_timehyp_hist.png"),
       dpi = 1200,
       height = 9.2,
       width = 9.2,
       units = "cm")

df_mod <- df_th_st %>%
  select(sitegroup, time, start.date, interstorm_d, q_max, q_min, q_change) %>%
  right_join(df_diff)

# time until hypoxia as function of storm pulse
ggplot(data = df_mod,
       aes(x = interstorm_d,
           y = drop,
           color = geomorph,
           size = q_change)) +
  geom_point() +
  theme_bw() +
  # geom_abline(intercept = 0, slope = 24) +
  labs(y = "storm drop (mg/L)",
       x = "time since last storm (days)")
       # x = expression("storm pulse (mm "*d^{-1}*")"))
ggsave(filename = file.path("results", "Figures", "storm_summaries", "drop_explanation.png"),
       dpi = 1200,
       height = 9.2,
       width = 12.4,
       units = "cm")



df_mod <- df_th_st %>%
  select(sitegroup, time, start.date, interstorm_d, q_max, q_min, q_change) %>%
  right_join(df_diff)

mod0 <- lm(drop ~ strahler + geomorph + interstorm_d + q_max + q_min + q_change, data = df_mod)
summary(mod0)

mods <- leaps::regsubsets(drop~strahler + geomorph + interstorm_d + q_max + q_min + q_change, data = df_mod, nvmax = 5)
summary(mods)

res.sum <- summary(mods)
data.frame(
  Adj.R2 = which.max(res.sum$adjr2),
  CP = which.min(res.sum$cp),
  BIC = which.min(res.sum$bic)
)

mod1 <- lm(drop ~ strahler * geomorph * interstorm_d, data = df_mod)
summary(mod1)


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


# Event look along network ------------------------------------------------
plot_fun <- function(startdate, enddate) {
  # data %>%
  df_event = df %>%
    filter(between(datetime,
                   startdate,
                   enddate)) %>%
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
  # ggsave(plot = event_fig,
  #        filename = file.path("results", "Figures", "storm_event_examples", "20210622.png"),
  #        dpi = 1200,
  #        width = 22,
  #        height = 16,
  #        units = "cm")
}

plot_fun(meta_storm$start[63], meta_storm$end[63])


ggsave(filename = file.path("results", "Figures", "storm_event_examples", "20200419_Loire.png"),
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


# Old ---------------------------------------------------------------------
# # Load hypoxia metadata
# df_hyp <- read_xlsx("Z:/RHypoxie/Data/01_metadata/hypoxia_dates.xlsx")
# 
# # Some quick calculations
# df <- df %>%
#   mutate(date = date(datetime),
#          month = month(datetime),
#          year = year(datetime),
#          solartime = streamMetabolizer::calc_solar_time(datetime, longitude),
#          light = streamMetabolizer::calc_light(solartime, latitude, longitude),
#          DOsat2 = if_else(is.na(DOsat2), DO/DO_per*100, DOsat2),
#          q_mmh = if_else(is.na(q_mmh), q_mmd / 24, q_mmh),
#          DOhyp = if_else(DO < 4, 1, 0))
# 
# ggplot(data = filter(df, site == "ruisseau de violay",
#                      between(datetime, ymd_h("2020041500"), ymd_h("2020052000"))
# )) +
#   # geom_point(aes(y = q_mmh)) +
#   # stat_summary(aes(x = date, y = lux / 100)) +
#   geom_point(aes(x = datetime, y = lux / 500)) +
#   geom_line(aes(x = datetime, y = DO_per)) +
#   theme_bw()
#  
meta <- readxl::read_xlsx(file.path("data", "01_metadata","hypoxia_dates.xlsx")) 

meta_st = filter(meta, type == "storm_down") %>%
  mutate(start = lubridate::force_tz(start, tzone = "Europe/Berlin"),
         end = lubridate::force_tz(end, tzone = "Europe/Berlin"),
         recovery_start = lubridate::force_tz(recovery_start, tzone = "Europe/Berlin"),
         recovery_end = lubridate::force_tz(recovery_end, tzone = "Europe/Berlin"))

df_st <- ungroup(df) %>%
  select(-position, -number, -confluence) %>%
  distinct() %>%
  fuzzyjoin::fuzzy_semi_join(meta_st,
                             by = c("site_code" = "site_code",
                                    "datetime" = "start",
                                    "datetime" = "end"),
                             match_fun = list(`==`, `>=`, `<=`))
# 



# Old hyporheic check -----------------------------------------------------
# 
# # Load all hyporheic zone data for the Ratier amont Charbonnieres
# df_hr <- read_excel(file.path("data", "02_sensor_data", "raw", "bdoh", "hyporheic_data.xlsx"))
# 
# # Clean it up to be same format as sensor data
# df_hr <- df_hr %>%
#   mutate(datetime = with_tz(datetime_UTC, "Europe/Berlin"), 
#          hour = floor_date(datetime, "hour")) %>%
#   group_by(hour) %>%
#   summarize(across(where(is.numeric), mean, na.rm = TRUE)) %>%
#   rename(datetime = hour,
#          spc = spc_uscm,
#          DO = DO_mgL) %>%
#   ungroup()
# 
# # Compare
# df_storm <- filter(df, between(datetime, ymd_h(2021041500), ymd_h(2021043000)),
#                    site == "ratier amont charbonnieres") %>%
#   mutate(type = "surface") %>%
#   bind_rows(filter(df_hr, between(datetime, ymd_h(2021041500), ymd_h(2021043000))) %>%
#               mutate(type = "hyporheic"))
# 
# df_storm2 <- filter(df,
#                     site == "ratier aval ribes") %>%
#   mutate(type = "surface") %>%
#   bind_rows(df_hr %>%
#               mutate(type = "hyporheic"))
# 
# p <- ggplot(data = df_storm2,
#             aes(x = datetime,
#                 y = spc,
#                 color = type)) +
#   geom_line() +
#   theme_bw()
# 
# plotly::ggplotly(p)
# 
# df_hrdif <- df_storm2 %>%
#   mutate(date = date(datetime)) %>%
#   group_by(date, type) %>%
#   summarize(maxt = datetime[max(DO, na.rm = T)]) %>%
#   mutate(hr = hour(maxt)) %>%
#   ungroup() %>%
#   select(-maxt) %>%
#   pivot_wider(values_from = hr, names_from = type) %>%
#   mutate(dift = surface - hyporheic)
# 
# ggplot(data = df_hrdif,
#        aes(x = date,
#            y = dift)) +
#   geom_point() +
#   geom_line() +
#   theme_bw()


