# 
# Purpose: To evaluate how drying induces hypoxia in headwater streams
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

# Load data ---------------------------------------------------------------
# Load sensor data
df <- readRDS(file.path("data", "10_clean_data","hourly_data_all.RDS"))

# Load drying start dates and length
meta_dry <- readRDS(file.path("data", "10_clean_data","drying_starts_lengths.RDS")) %>%
  ungroup()

# Load metadata about pool and riffle
meta_geom <- readxl::read_excel(file.path("data", "07_geomorphology","geomorphic_units.xlsx"))

# Subset the sensor data for drying periods
# Clean data for only drying hypoxia events and get times right
meta_dry <- meta_dry %>%
  mutate(start = with_tz(start.date, tzone = "Europe/Berlin"),
         end = force_tz(start, tzone = "Europe/Berlin") + hours(spell.length))

# Only get the data for the drying hypoxia events
df_dry <- ungroup(df) %>%
  select(-position, -number, -confluence) %>%
  distinct() %>%
  fuzzyjoin::fuzzy_semi_join(meta_dry,
                             by = c("siteq" = "siteq",
                                    "datetime" = "start",
                                    "datetime" = "end"),
                             match_fun = list(`==`, `>=`, `<=`))

# Save the data for later use
saveRDS(df_dry, file.path("data", "10_clean_data", "hypoxia_drying_new.RDS"))
df_dry <- readRDS(file.path("data", "10_clean_data", "hypoxia_drying.RDS"))

# Dataframe of all drying event with lengths
df_dry <- df_dry %>%
  distinct() %>%
  group_by(site) %>%
  mutate(DOhyp = if_else(DO < 3, 1, 0),
         year = year(datetime)) %>%
  mutate(group = cumsum(c(1, diff(datetime) > 1))) %>% # group by drying events
  group_by(site, group) %>%
  filter(oow == "no" | is.na(oow))  %>%
  ungroup() %>%
  mutate(sitegroup = paste0(group, site)) %>%
  left_join(meta_geom) %>%
  mutate(strahler = if_else(site %in% c("toranche aval", "coise aval vaudragon"), 
                            3, strahler)) %>%
  mutate(geomorph = if_else(site == "coise aval vaudragon", 
                            "run", geomorph))

# How many of the sites that dried, became hypoxic?
df_dry_h <- df_dry %>%
  group_by(site, sitegroup) %>%
  filter(sum(DOhyp == 1, na.rm = T) > 1) %>%
  ungroup()

df_dry_h %>% distinct(site) %>% nrow()

# How many distinct events
distinct(df_dry_h, sitegroup)

# How many events per site
distinct(df_dry_h, sitegroup, site) %>%
  group_by(site) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  summarise(mean = mean(n),
            sd = sd(n))

# What percent of time hypoxic
df_dry_h %>%
  group_by(site, sitegroup) %>%
  summarize(n = sum(DOhyp == 1, na.rm = T),
            nt = n(),
            d = n/nt) %>%
  ungroup() %>%
  summarise(mean = mean(n),
            mt = mean(d),
            sdd = sd(d),
            sd = sd(n))

# How many of the sites that dried, did not become hypoxic?
df_dry_o <- df_dry %>%
  group_by(site, sitegroup) %>%
  filter(sum(DOhyp == 1, na.rm = T) == 0) %>%
  ungroup()

# Plot of which sites became hypoxic and which didn't ---------------------
select(df_dry_h, site, sitegroup, area_km2, strahler) %>%
  distinct() %>%
  mutate(hyp = "hypoxic") %>%
  bind_rows(select(df_dry_o, site, sitegroup, area_km2, strahler) %>%
              distinct() %>%
              mutate(hyp = "oxic")) %>%
  ggplot(aes(x = as.factor(strahler),
             fill = hyp)) + 
  geom_bar(width=.5, position = "dodge")

select(df_dry_h, site, sitegroup, area_km2, strahler) %>%
  distinct() %>%
  mutate(hyp = "hypoxic") %>%
  bind_rows(select(df_dry_o, site, sitegroup, area_km2, strahler) %>%
              distinct() %>%
              mutate(hyp = "oxic")) %>%
  group_by(strahler, hyp) %>%
  summarize(n = n())

# Characterize the hypoxia ------------------------------------------------
# Get ready for plotting
df_dry_p <- df_dry_h %>%
  group_by(sitegroup) %>%
  mutate(hour = row_number() - 1) %>%
  group_by(sitegroup) %>%
  mutate(hourbef = hour - max(hour)) %>%
  mutate(DOsmooth = stats::filter(DO, rep(1/5,5), sides =2)) %>%
  ungroup()

# Text to avoid facet strips
ann_text <- data.frame(hourbef = -18, DO = 10, 
                       label = c("pool", "riffle", "run"),
                       geomorph = c("pool", "riffle", "run"))

# Plot of time series
p_ts_dry <- ggplot() +
  geom_line(data = filter(df_dry_p, site_code %in% c("coi023", "loi039", "loi068", "cha002", "loi132")),
            aes(x = hourbef, y = DO, group = sitegroup, color = as.factor(strahler)),
            size = 1.1) +
  scale_color_manual(name = "Strahler", values = c("black", "#0072B2", "#D55E00", "#F0E442")) +
  scale_x_continuous(limits = c(-72, 0),
                     breaks = seq(-120, 0, 24)) +
  geom_hline(yintercept = 3, linetype = "dashed") +
  theme_bw() +
  geom_text(data = ann_text,
            aes(x = hourbef, y = DO, label = label)) +
  facet_grid(rows = vars(geomorph)) +
  theme(legend.position = "none",
        #legend.position = c(0.4,0.97), legend.direction = "horizontal",
        legend.background = element_rect(fill = "transparent"),
        strip.background = element_blank(), strip.text = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank()) +
  labs(y = expression("DO (mg "*L^{-1}*")"),
       x = "hours before dry")
p_ts_dry

# Regressions of drop rates -------------------------------------------------------------
# Caclulate regressions on DO minima
df_m <- df_dry_p %>%
  group_by(sitegroup, date) %>%
  slice(which.min(DO)) %>%
  ungroup() %>%
  select(site, group, hour, DO_per, DO, DO_temp) %>%
  pivot_longer(cols = starts_with("DO")) %>%
  group_by(site, group, name) %>%
  nest() %>%
  mutate(mod = map(data, ~lm(value~hour, data = .)),
         t = map(mod, broom::tidy)) %>%
  select(t) %>%
  unnest(cols = t)



df_m2 <- df_m %>%
  pivot_wider(names_from = term, values_from = estimate:p.value)




# text for figure
drop_txt <- ungroup(df_m2) %>%
  filter(name == "DO") %>%
  summarize(mean = mean(-estimate_hour * 24, na.rm = T),
            sd = sd(-estimate_hour * 24, na.rm = T),
            med = median(-estimate_hour * 24, na.rm = T)) %>%
  mutate(x = 5, y = 0.55,
            txt = paste0("median = ",round(med,1),
                         "\n", "mean±sd = ",
                         round(mean,1), "±", round(sd,1)))

p_hist_droprates <- df_m2 %>%
  filter(name == "DO") %>%
  ggplot(aes(x = -estimate_hour * 24)) +
  geom_density(fill = "black", alpha = 0.5) +
  theme_classic() +
  scale_x_continuous(limits = c(0, 10), breaks = seq(0,10, 1)) +
  geom_vline(aes(xintercept = median(-estimate_hour * 24, na.rm = T))) +
  geom_text(data=drop_txt, aes(x =x, y =y, label = txt)) +
  labs(x = expression("rate of DO decrease (mg "*L^{-1}~d^{-1}*")"),
       y = "density")
p_hist_droprates
  
# 
# ggsave(plot = p_hist_droprates, 
#        filename = file.path("results", "Figures", "drying_summaries", "dry_droprate_hist.png"),
#        dpi = 1200,
#        height = 9.2,
#        width = 9.2,
#        units = "cm")

# Drop rate by geomorph
p_test <- df_m2 %>%
  left_join(distinct(select(df, site, site_code))) %>%
  left_join(meta_geom, by = "site_code") %>%
  filter(name == "DO") %>%
  ggplot(aes(x = geomorph,
             y = -estimate_hour * 24)) +
  stat_summary() + 
  # geom_density(fill = "red", alpha = 0.5) +
  theme_classic() +
  # geom_vline(aes(xintercept = median(-estimate_hour * 24, na.rm = T))) +
  labs(y = expression("rate of DO decrease (mg "*L^{-1}~d^{-1}*")"))
p_test

ggsave(plot = p_test, 
       filename = file.path("results", "Figures", "drying_summaries", "dry_droprate_geomorph.png"),
       dpi = 1200,
       height = 9.2,
       width = 9.2,
       units = "cm")

ggplot(data = filter(df_m2, name == "DO"),
       aes(x = `estimate_(Intercept)`,
            y = estimate_hour)) +
  # facet_wrap(~site) +
  geom_point() +
  theme_bw() +
  stat_smooth(method = "lm") +
  # ggpubr::stat_regline_equation() +
  # ggpubr::stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "*`,`~")),
  #                   label.y = -2) +
  theme(legend.position = "none") +
  labs(title = "relationship between slope and intercept (%)",
       x = "drying regression intercept",
       y = "drying regression slope")

ggsave(filename = "Z:/RHypoxie/Figures/drying_regression_slopevsint_per.png",
       dpi = 1200,
       height = 9.2,
       width = 12,
       units = "cm")

mean(df_m2$estimate_hour, na.rm = T) * 24.

hist(df_m2$estimate_hour)

# mean of results
df_mean <- filter(df_m, term == "hour") %>%
  group_by(name) %>%
  summarize(mean = mean(estimate * 24, na.rm = T),
            med = median(estimate * 24, na.rm = T))

# density plot of ecosystem oxygen demand
p_dens_per <- ggplot(data = filter(df_m, term == "hour", p.value < 0.05)) +
  theme_bw(base_size = 11) +
  geom_density(aes(x = estimate * 24)) + 
  labs(x = expression("ecosystem oxygen demand (mg "*L^{-1}~d^{-1}*")"),
       ) +
  # geom_vline(data = df_mean, aes(xintercept = mean, group = name)) + 
  # geom_vline(data = df_mean, aes(xintercept = med, group = name), linetype = "dashed") +
  facet_wrap(~name, scales = "free_x")

p_dens_per




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

df_dry_p %>%
  ggplot() +
  geom_line(aes(x = hourbef,
                y = DO, group = sitegroup,
                color = geomorph),
            alpha = 0.5)+
  # geom_point(data = filter(df_dry_p, color == TRUE),
  # aes(x = hourbef, y = DO, color = color)) +
  # stat_smooth(data = filter(df_dry_p, color == TRUE),
  #             aes(x = hourbef, y = DO, group = gs),
  #             method = "lm", se = FALSE) +
  # ggpubr::stat_regline_equation(data = filter(df_dry_p, color == TRUE),
  #                               aes(x = hour, y = DO, group = gs)) +
  geom_hline(yintercept = 3, linetype = "dashed", color = "red") +
  facet_wrap(~site_code)+
  scale_x_continuous(breaks = seq(-24*10, 0, 48),
                     limits = c(-240, 0)) +
  # scale_color_manual(values = "blue") +
  theme_bw() +
  theme(legend.position = "none") +
  labs(title = "drying patterns leading to hypoxia",
       x = "hours before dry",
       y = "DO (mg/L)")

ggsave(filename = "Z:/RHypoxie/Figures/drying_hypoxia_just_lines_mgl.png",
       dpi = 1200,
       height = 18.4,
       width = 28,
       units = "cm")



# Time until hypoxia ------------------------------------------------------
# Time until hypoxia
df_th <- df_dry_p %>%
  group_by(sitegroup) %>%
  filter(hour == min(hour)) %>%
  left_join(ungroup(df_dry_p) %>%
              group_by(sitegroup) %>%
            filter(hour == 0) %>%
            select(DO_start = DO))

ungroup(df_th) %>%
  summarize(mean = mean(hour / 24, na.rm = T),
            sd = sd(hour / 24, na.rm = T),
            med = median(hour / 24, na.rm = T),
            q = quantile(hour / 24, c(0.25,0.5,0.75), na.rm = T))


p_hist_dry_timhyp <- ggplot(data = df_th,
       aes(x = hour / 24)) +
  geom_density(fill = "blue", alpha = 0.5) +
  geom_vline(aes(xintercept = median(hour / 24, na.rm = T))) +
  # geom_point() +
  theme_bw() +
  # geom_abline(intercept = 0, slope = 24) +
  labs(x = "time until hypoxia (days)")
p_hist_dry_timhyp

ggsave(plot = p_hist_dry_timhyp,
       filename = file.path("results", "Figures", "drying_summaries", "time_to_hyp_hist.png"),
       dpi = 1200,
       height = 9.2,
       width = 9.2,
       units = "cm")



df_h <- df_dry_p %>%
  ungroup() %>%
  drop_na(h) %>%
  group_by(site, group) %>%
  distinct(h, date, .keep_all = T) %>%
  nest() %>%
  mutate(mod = map(data, ~lm(h~hour, data = .)),
         t = map(mod, broom::tidy)) %>%
  select(t) %>%
  unnest(cols = t)

df_h2 <- df_h %>%
  filter((term == "hour" & p.value < 0.05 )) %>%
  pivot_wider(names_from = term, values_from = estimate:p.value) %>%
  select(hslope = estimate_hour)
# hint = `estimate_(Intercept)`, 

df_comp <- left_join(df_m2, df_h2)

ggplot(data = df_comp,
       aes(x = hslope,
           y = estimate_hour)) + 
  geom_point() +
  theme_bw() +
  stat_smooth(method = "lm") +
  ggpubr::stat_regline_equation() +
  ggpubr::stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "*`,`~")),
                   label.y = -0.1) +
  labs(title = "relationship between rate of drying and DO (mg/L) drop",
       x = "rate of water height change (m/h)",
       y = "rate of DO change (mg/L/h)")

ggsave(filename = "Z:/RHypoxie/Figures/drying_rate_DO_rate.png",
       dpi = 1200,
       height = 9.2,
       width = 12,
       units = "cm")



# Length of continuous hypoxia --------------------------------------------
# Length of continuous hypoxia
df_hyp_length <- df_dry_p %>% 
  ungroup() %>%
  group_by(site, group) %>%
  mutate(cons = runner::streak_run(DOhyp),
         hyp_change = abs(DOhyp - lag(DOhyp)),
         pd = cumsum(replace_na(hyp_change, 0)),
         subname = paste(DOhyp, pd, sitegroup)) %>%
  group_by(subname) %>%
  # filter(DOhyp == 1) %>%
  filter(cons == max(cons)) %>%
  left_join(select(meta_geom, -site))


x = ungroup(df_hyp_length) %>%
  select(sitegroup, geomorph, strahler, DOhyp, cons) %>%
  mutate(match = ceiling(row_number() /2)) %>%
  drop_na(DOhyp) %>%
  pivot_wider(names_from = DOhyp, values_from = cons)

ggplot(data = x,
       aes(x = strahler,
           y = `1`,
           group = strahler)) +
  geom_boxplot() +
  theme_bw() +
  scale_y_continuous(limits = c(0, 30)) +
  labs(x = "Strahler order",
       y = "length of consecutive hypoxia (hours)")

group_by(x, geomorph) %>%
  summarize(m = mean(`1`, na.rm = T),
            sd = sd(`1`, na.rm = T))


p_len_cons_hyp <- ggplot(data = df_hyp_length,
       aes(x = strahler,
           y = cons,
           group = strahler,
           fill = as.factor(strahler))) +
  geom_boxplot() +
  scale_fill_manual(name = "Strahler", values = c("black", "#0072B2", "#D55E00", "#F0E442")) +
  theme_classic() +
  theme(legend.position = "none") + 
  scale_y_continuous(limits = c(0, 30)) +
  labs(x = "Strahler order",
       y = "consecutive hypoxia duration (h)")


p_len_cons_hyp
ggsave(plot = p_len_cons_hyp,
       filename = file.path("results", "Figures", "drying_summaries", "cons_len_hyp_strah.png"),
       dpi = 1200,
       height = 9.2,
       width = 9.2,
       units = "cm")
  


p_len_cons_hyp_geo <- ggplot(data = df_hyp_length,
                         aes(x = geomorph,
                             y = cons,
                             group = geomorph,
                             fill = as.factor(geomorph))) +
  geom_boxplot() +
  scale_fill_manual(name = "Strahler", values = c("black", "#0072B2", "#D55E00")) +
  theme_classic() +
  theme(legend.position = "none") + 
  scale_y_continuous(limits = c(0, 30)) +
  labs(x = "habitat",
       y = "consecutive hypoxia duration (h)")
p_len_cons_hyp_geo
# Overall plot ------------------------------------------------------------
(p_ts_dry | (p_hist_droprates / p_len_cons_hyp)) + plot_annotation(tag_levels = "A")
ggsave(filename = file.path("results", "Figures", "drying_summaries", "fig4_v2.svg"),
       dpi = 1200,
       height = 15,
       width = 18,
       units = "cm")



# Other things ------------------------------------------------------------

  
ggplot(data = filter(df_hyp, !(type %in% c("pond", "pool", "respiration", "storm_up"))),
       aes(x = type)) +
  geom_bar() +
  theme_bw() +
  labs(title = "# of events for types of hypoxia drivers",
       x = "",
       y = "count of events")

ggsave(filename = "Z:/RHypoxie/Figures/hypoxia_type_count.png",
       dpi = 1200,
       height = 9.2,
       width = 12,
       units = "cm")



# Changes in amplitude ----------------------------------------------------
df_amp <- df_dry_p %>%
  group_by(gs, date, site) %>%
  summarize(amp_DO = max(DO) - min(DO),
            amp_temp = max(DO_temp) - min(DO_temp)) %>%
  ungroup() %>%
  pivot_longer(cols = starts_with("amp")) %>%
  group_by(site, gs, name) %>%
  mutate(dayno = row_number() - 1) %>%
  nest() %>%
  mutate(mod = map(data, ~lm(value~dayno, data = .)),
         t = map(mod, broom::tidy)) %>%
  select(t) %>%
  unnest(cols = t)


df_amp_w <- df_amp %>%
  pivot_wider(names_from = term, values_from = estimate:p.value)

p_hist_amp <- df_amp_w %>%
  filter(name == "amp_DO",
         p.value_dayno < 0.05) %>%
  ggplot(aes(x = estimate_dayno)) +
  geom_density(fill = "red", alpha = 0.5) +
  theme_bw() +
  geom_vline(aes(xintercept = median(estimate_dayno, na.rm = T))) +
  labs(x = expression("rate of amplitude increase (mg "*L^{-1}~d^{-1}*")"))
p_hist_amp

ungroup(df_amp_w) %>%
  filter(name == "amp_DO",
         p.value_dayno < 0.05) %>%
  summarize(mean = mean(estimate_dayno),
            sd = sd(estimate_dayno))

df_amp %>%
  filter(term == "dayno") %>%
  select(name, estimate) %>%
  pivot_wider(names_from = name, values_from = estimate) %>%
  ggplot(aes(amp_temp, amp_DO)) +
  geom_point()




# Old ---------------------------------------------------------------------




re_meta = filter(df_hyp, type == "rewet_down") %>%
  mutate(start = lubridate::force_tz(start, tzone = "Europe/Berlin"),
         end = lubridate::force_tz(end, tzone = "Europe/Berlin"),
         recovery_start = lubridate::force_tz(recovery_start, tzone = "Europe/Berlin"),
         recovery_end = lubridate::force_tz(recovery_end, tzone = "Europe/Berlin"))

df_re <- ungroup(df_loire) %>%
  select(-position, -number, -confluence) %>%
  distinct() %>%
  fuzzyjoin::fuzzy_semi_join(re_meta,
                             by = c("site_code" = "site_code",
                                    "datetime" = "start",
                                    "datetime" = "end"),
                             match_fun = list(`==`, `>=`, `<=`))


df_re_p <- df_re %>%
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






# # Old ---------------------------------------------------------------------
# 
# # Load hypoxia metadata
# df_hyp <- read_xlsx(file.path("data", "01_metadata","hypoxia_dates.xlsx"))
# 
# # Clean data for only drying hypoxia events and get times right
# dry_meta <- filter(df_hyp, type == "drying") %>%
#   mutate(start = lubridate::force_tz(start, tzone = "Europe/Berlin"),
#          end = lubridate::force_tz(end, tzone = "Europe/Berlin"),
#          recovery_start = lubridate::force_tz(recovery_start, tzone = "Europe/Berlin"),
#          recovery_end = lubridate::force_tz(recovery_end, tzone = "Europe/Berlin"))
# 
# # Only get the data for the drying hypoxia events
# df_dry <- ungroup(df_loire) %>%
#   select(-position, -number, -confluence) %>%
#   distinct() %>%
#   fuzzyjoin::fuzzy_semi_join(dry_meta,
#                              by = c("site_code" = "site_code",
#                                     "datetime" = "start",
#                                     "datetime" = "end"),
#                              match_fun = list(`==`, `>=`, `<=`))
# 
# # Save the data for later use
# saveRDS(df_dry, file.path("data", "10_clean_data", "hypoxia_drying.RDS"))
# df_dry <- readRDS(file.path("data", "10_clean_data", "hypoxia_drying.RDS"))



# p_ts_dry_q <- ggplot() +
# geom_line(data = filter(ungroup(df_dry_p), 
#                         between(datetime, ymd_h(2020071700), 
#                                 ymd_h(2020080200))) %>%
#             distinct(datetime, q_mmd),
#           aes(x = datetime, y = q_mmd),
#           size = 1.1,
#           color = "blue") +
#   # geom_line(data = filter(df, year == 2020, datetime > ymd_h(2020071700),
#   #                         siteq == "toranche_st_cyr_les_vignes",
#   #                         # siteq == "coise_le_nezel",
#   #                         datetime < ymd_h(2020092100),
#   #                         !between(datetime, ymd_h(2020083100), ymd_h(2020090300)),
#   #                         !between(datetime, ymd_h(2020080100), ymd_h(2020080300)),
#   #                         !between(datetime, ymd_h(2020082800), ymd_h(2020090100))),
#   #           aes(x = datetime,
#   #               y = q_mmd,
#   #               group = siteq),
#   #           color = "blue") +
# p_theme_q +
#   labs(y = expression("q (mm "*d^{-1}*")")) +
#   scale_y_continuous(position = "right", limits = c(0,0.5)) +
#   # geom_point(data = filter(df_dry_p, color == TRUE),
#   #            aes(x = hour, y = DO, color = color)) +
#   # geom_hline(yintercept = 3, linetype = "dashed", color = "red") +
#   facet_grid(~month(datetime), scales = "free_x", space = "free")+
#   scale_color_manual(values = "blue") +
#   theme(strip.background = element_blank(), strip.text = element_blank(),
#         axis.text.y = element_text(color = "blue"),
#         axis.title.y = element_text(color = "blue"))
# p_ts_dry_q
# 
# p_ts_dry_all <- p_ts_dry + p_ts_dry_q +
#   plot_layout(design = layout)
# p_ts_dry_all




# Plotting theme ----------------------------------------------------------
# theme for discharge plots
# p_theme_q <- 
#   theme_minimal() +
#   theme(panel.background = element_rect(fill='transparent', color = NA), #transparent panel bg
#         plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
#         panel.grid.major = element_blank(), #remove major gridlines
#         panel.grid.minor = element_blank(), #remove minor gridlines
#         legend.background = element_rect(fill='transparent'), #transparent legend bg
#         legend.box.background = element_rect(fill='transparent'),
#         axis.text.x = element_blank(),
#         axis.title.x = element_blank(),
#         strip.background = element_blank(),
#         strip.text = element_blank()) #transparent legend panel)
# 
# layout <- c(
#   area(t = 1, l = 1, b = 8, r = 4),
#   area(t = 6, l = 1, b = 8, r = 4)
# )


# ggsave(plot = p_ts_dry_all,
#        filename = file.path("results", "Figures", "drying_summaries", "drying_ex_ts.png"),
#        dpi = 1200,
#        units = "cm",
#        width = 18.4,
#        height = 12)