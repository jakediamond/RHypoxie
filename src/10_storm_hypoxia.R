# -------------------------------------
# Author: Jake Diamond
# Purpose: Analyze headwater data for storm-induced hypoxia
# Date: 12 may 2022
# -------------------------------------
# Load libraries
library(lubridate)
library(scales)
library(readxl)
library(brooom)
library(rstatix)
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

# General stats -----------------------------------------------------------
# Total number of site events
nrow(distinct(df_storm_final, sitegroup))

# Total number of site events that led to hypoxia
df_storm_final %>%
  mutate(hyp = if_else(DO<3,1,0)) %>%
  filter(time > -6/24) %>% #only want to consider hypoxia after the storm (or just before)
  group_by(sitegroup) %>%
  summarize(hypt= sum(hyp, na.rm = T)) %>%
  filter(hypt > 0) %>%
  nrow()

# Total number of site events that led to hypoxia by strahler
df_storm_final %>%
  mutate(hyp = if_else(DO<3,1,0)) %>%
  filter(time > -6/24) %>% #only want to consider hypoxia after the storm (or just before)
  group_by(sitegroup, strahler) %>%
  summarize(hypt= sum(hyp, na.rm = T)) %>%
  filter(hypt > 0) %>%
  group_by(strahler) %>%
  summarize(n = n())

# Length of hypoxia
df_rls <- df_storm_final %>%
  group_by(sitegroup) %>%
  arrange(sitegroup, datetime) %>%
  mutate(DOhyp = if_else(DO<3,1,0),
         hyp_l = sequence(rle(DOhyp)$lengths),
         hyp_change = if_else(((lag(DOhyp, default = 0) != DOhyp) &
                                 (lag(DOhyp, 2, default = 0) != DOhyp)), 
                              1, 0))

# Characterizing storm responses ------------------------------------------
# Sites that were hypoxic before the storm
df_hyp_bef <- ungroup(df_rls) %>%
  group_by(sitegroup) %>%
  filter(time <0) %>%
  summarize(hypbefore = if_else(sum(DOhyp, na.rm = T) == 0, 0, 1))

# Sites that were hypoxic before the storm and oxic at the storm pulse
df_reox <- filter(df_hyp_bef, hypbefore ==1) %>%
  left_join(df_storm_final) %>%
  filter(between(time, 0, 6/24), DO >=3) %>%
  distinct(site, group, strahler) %>%
  mutate(stormtype = "hyp_ox")

# Sites that exhibited no change before or after the storm
df_nodif <- df_storm_final %>%
  mutate(befaf = if_else(time < 0, "before", "after")) %>%
  filter(between(time,-3,3)) %>%
  group_by(sitegroup, date, befaf) %>%
  summarize(mean = mean(DO, na.rm = T),
            amp = max(DO, na.rm = T) - min(DO, na.rm = T)) %>%
  group_by(sitegroup) %>%
  drop_na() %>%
  filter(sum(befaf == "before") >2, sum(befaf == "after") > 2) %>%
  ungroup()

df_ampdif <- df_nodif %>%
  group_by(sitegroup) %>%
  t_test(amp~befaf)

df_meandif <- df_nodif %>%
  group_by(sitegroup) %>%
  t_test(mean~befaf)

df_nodifsum <- df_nodif %>%
  group_by(sitegroup, befaf) %>%
  summarize(meanDO = mean(mean, na.rm = T),
            meanamp = mean(amp, na.rm = T)) %>%
  pivot_longer(cols = c(meanDO, meanamp), names_to = "type") %>%
  pivot_wider(names_from = befaf, values_from = value) %>%
  left_join(select(df_ampdif, sitegroup, p) %>%
              mutate(type = "meanamp")) %>%
  rows_update(select(df_meandif, sitegroup, p) %>%
              mutate(type = "meanDO"),
            by = c("sitegroup", "type")) %>%
  mutate(dif = if_else(after > before, "+", "-"),
         sig = if_else(p < 0.01, "sig", "ns"))

# Try to categorize every event
df_cat <- df_nodifsum %>%
  mutate(cat = case_when(sig == "ns" ~ "no change",
                             sig == "sig" ~ paste0(type, dif),
                             TRUE ~ NA_character_
                             )
            ) %>%
  ungroup() %>%
  group_by(sitegroup) %>%
  summarize(category = paste0(cat))
  # summarize(type = if_else(sum(sig == "ns") == 2, "nochange",
  #                          if_else(sum(sig == "sig") == 2,
  #                                  if_else(type == "meanDO" & dif == "greater",
  #                                          if_else(type == "meanamp" & dif == "greater",
  #                                                  "DO+amp+",
  #                                                  "DO+amp-"),
  #                                          if_else(type == "meanamp" & dif == "greater",
  #                                                  "DO-amp+",
  #                                                  "DO-amp-"),
  #                                          NA_character_
  #                                          )
  #                                  )
  #                          )
  # )
  
# Calculating hypoxia metrics ---------------------------------------------
# Median lengths of hypoxia
df_hyp_len <- left_join(df_rls, df_hyp_bef) %>%
  filter(hypbefore == 0,
         time > 0,
         DOhyp == 1) %>%
  ungroup() %>%
  group_by(sitegroup) %>%
  mutate(hyp_pd = cumsum(replace_na(hyp_change, 0))) %>%
  ungroup() %>%
  group_by(sitegroup, hyp_pd) %>%
  filter(hyp_pd == 1) %>%
  filter(hyp_l == max(hyp_l)) %>%
  ungroup() %>%
  summarize(hyp_len_med = median(hyp_l, na.rm = T),
            mean = mean(hyp_l, na.rm = T),
            sd = sd(hyp_l, na.rm = T))

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
# Get ready to plot
df_st_p <- df_storm_final %>%
  mutate(DOhyp = if_else(DO < 3, 1, 0)) %>%
  filter(sum(DOhyp, na.rm = T) > 1) %>%
  ungroup() %>%
  group_by(sitegroup, date) %>%
  mutate(color = min(DO) == DO,
         color = if_else(is.na(color), FALSE, color),
         gs = paste(group, site),
         maxq = max(q_mmd)) %>%
  left_join(select(meta_geom, -site))

# Choose the events to plot
ps <- c("1coi071", "1loi068", "4loi068", #riffle
        "2ard046", "2coi023", "12mor009", #pool
        "2ard008", "3mar233", "5fon013" #run
        )

# Text to avoid facet strips
ann_text <- data.frame(time = -2.5, DO = 4, 
                       label = c("pool", "riffle", "run"),
                       geomorph = c("pool", "riffle", "run"))

# Color by response type

# Plot
p_geomorph <- ggplot() +
  geom_line(data = filter(df_st_p, sitegroup %in% ps),
            aes(x = time,
                y = DO, group = sitegroup,
                color = q_mmd / 24),
            alpha = 0.7,
            size = 1.2) +
  geom_hline(yintercept = 3, linetype = "dashed") +
  facet_grid(rows = "geomorph") +
  scale_x_continuous(breaks = seq(-3,8, 1)) +
  geom_vline(xintercept = 0) +
  scale_color_gradient(trans = "log10") +
  geom_text(data = ann_text,
            aes(x = time, y = DO, label = label)) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank()) +
  labs(x = "days after storm",
       y = expression("DO (mg "*L^{-1}*")"))

p_geomorph
# ggsave(filename = file.path("results", "Figures", "storm_summaries", "storm_hypoxia_mgl_geomorph_facet.png"),
#        dpi = 1200,
#        height = 9.2,
#        width = 16,
#        units = "cm")

# Calculate drop rates ----------------------------------------------------
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


# text for figure
drop_txt <- ungroup(df_diff) %>%
  summarize(mean = mean(drop / time_end, na.rm = T),
            median = median(drop / time_end, na.rm = T),
            sd = sd(drop / time_end, na.rm = T)) %>%
  transmute(x = 5, y = 0.3,
            txt = paste0("median = ",round(median,1),
                         "\n", "mean±sd = ",
                         round(mean,1), "±", round(sd,1)))

# histogram of drop rates
p_hist_droprates <- ggplot(data = df_diff,
                           aes(x = drop / time_end)) +
  geom_density(fill = "red", alpha = 0.5) + 
  theme_bw() +
  scale_x_continuous(limits = c(0, 10), breaks = seq(0,10,1)) +
  geom_vline(aes(xintercept = median(drop/time_end, na.rm = T))) +
  geom_text(data=drop_txt, aes(x =x, y =y, label = txt)) +
  labs(x = expression("rate of DO decrease (mg "*L^{-1}~d^{-1}*")"),
       y = "density")
p_hist_droprates
# ggsave(plot = p_hist_droprates, 
#        filename = file.path("results", "Figures", "storm_summaries", "storm_droprate_hist.png"),
#        dpi = 1200,
#        height = 9.2,
#        width = 9.2,
#        units = "cm")


# Interstorm and storm metrics --------------------------------------------
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

# storm pulse increase 2 hours before storm to storm peak
df_sp2 <- ungroup(df_st_p) %>%
  group_by(sitegroup) %>%
  filter(time == 0 | time == -2/24) %>%
  mutate(q_change = max(q_mmd, na.rm = T) - min(q_mmd, na.rm = T),
         q_max = max(q_mmd, na.rm = T)) %>%
  select(q_change) %>%
  distinct()


# Time until hypoxia
df_th_st <- ungroup(df_st_p) %>%
  left_join(df_hyp_bef) %>%
  filter(hypbefore == 0) %>%
  group_by(sitegroup) %>%
  filter(time >= 0) %>%
  filter(DOhyp == 1) %>%
  filter(time == min(time)) %>%
  distinct() %>%
  left_join(df_sp1) %>%
  left_join(df_sp2) %>%
  left_join(df_storm_summary)

# Some stats for text
ungroup(df_th_st) %>%
  summarize(mean = mean(time*24, na.rm = T),
            sd = sd(time*24, na.rm = T),
            median = median(time*24, na.rm = T),
            min = min(time*24, na.rm = T),
            max = max(time*24, na.rm = T))

# histogram of drops
p_hist_timehyp <- ggplot(data = df_th_st,
       aes(x = time))+
  geom_density(fill = "blue", alpha = 0.5) + 
  theme_bw() +
  scale_x_continuous(limits = c(0, 7), breaks = seq(0,7,1)) +
  geom_vline(aes(xintercept = median(time, na.rm = T))) +
  labs(x = "time until hypoxia (days)")

# ggsave(plot = p_hist_timehyp, 
#        filename = file.path("results", "Figures", "storm_summaries", "storm_timehyp_hist.png"),
#        dpi = 1200,
#        height = 9.2,
#        width = 9.2,
#        units = "cm")

df_mod <- df_th_st %>%
  select(sitegroup, time, start.date, interstorm_d, q_max, q_min, q_change) %>%
  right_join(df_diff)

# time until hypoxia as function of storm pulse
p_change <- ggplot(data = df_mod,
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


p_geomorph | (p_hist_droprates / p_change)



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
                   startdate+hours(10),
                   enddate - hours(10)),
           watershed == "Coise") %>%
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
                  y = DO,
                  color = area_km2,
                  group = site)) +
    geom_line(size = 1, alpha = 0.5)+
    geom_hline(yintercept = 3, linetype = "dashed") +
    # facet_wrap(~watershed, nrow = 1) + 
    scale_x_datetime(date_breaks = "2 days",
                     date_labels = "%m/%d") +
    # facet_wrap(~fct_reorder(site, number))+
    scale_color_viridis_c(expression("area ("*km^2*")"),
                          guide = guide_colourbar(direction = "horizontal",
                                                  title.position = "top",
                                                  title.hjust = 0.5,
                                                  barheight = 0.8))+
    # guide_colorbar(color = colorbar())
    labs(y = expression("DO (mg "*L^{-1}*")")) +
         # title = ymd(startdate)) +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          legend.position = c(0.5, 0.93),
          legend.background = element_rect(fill = "transparent"),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank())
  
  a2 = ggplot(data = df_event,
              aes(x = datetime,
                  y = spc,
                  color = log(area_km2),
                  group = site)) +
    geom_line(size = 1.2, alpha = 0.5)+
    # facet_wrap(~watershed, nrow = 1) + 
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
                  color = q_mmd,
                  group = site)) +
    scale_color_gradient(trans = "log10") +
    geom_line(size = 1) +
    scale_y_continuous(position = "right") +
    # facet_wrap(~watershed, nrow = 1) +
    # facet_wrap(~fct_reorder(site, number))+
    theme_minimal() +
    labs(y = expression("q (mm"~d^{-1}*")")) +
    theme(legend.position = "none",
          panel.background = element_rect(fill='transparent', color = NA), #transparent panel bg
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
      # facet_wrap(~watershed, nrow = 1) +
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

p_storm_a <- plot_fun(meta_storm$start[8], meta_storm$end[8])
p_storm_a
(p_storm_a / p_hist_droprates) | p_geomorph

p_geomorph | (p_hist_droprates / p_drop)


# ggsave(filename = file.path("results", "Figures", "storm_event_examples", "20200419_Loire.png"),
#        dpi = 1200,
#        width = 22,
#        height = 16,
#        units = "cm")

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

# 
# p_drop <- ggplot(data = df_diff) +
#   geom_segment(aes(y = DO_begin, x = time_begin,
#                    yend = DO_end, xend = time_end,
#                    color = drop),
#                size = 1) + 
#   geom_point(aes(y = DO_begin, x = time_begin,
#                  # yend = DO_end, xend = time_end,
#                  color = drop),
#              size = 1) +
#   geom_point(aes(y = DO_end, x = time_end,
#                  # yend = DO_end, xend = time_end,
#                  color = drop),
#              size = 1) +
#   # facet_wrap(~geomorph) +
#   theme_bw() +
#   scale_color_viridis_c(name = "DO drop") +
#   labs(x = "days since peak flow",
#        y = expression("dissolved oxygen (mg "*L^{-1}*")"))
# p_drop
# ggsave(plot = p_drop, 
#        filename = file.path("results", "Figures", "storm_summaries", "storm_drop_summary.png"),
#        dpi = 1200,
#        height = 9.2,
#        width = 12.4,
#        units = "cm")
