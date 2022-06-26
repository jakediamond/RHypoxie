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

# library(leaflet)
# library(gtsummary)
# library(sf)
# library(qwraps2)
# library(kableExtra)

# Load data ---------------------------------------------------------------
# Load old Loire data...only these sites experienced drying
df_loire <- readRDS(file.path("data", "09_loire_data","loire_headwaters_data.RDS")) %>%
  unnest() %>%
  mutate(site_code = tolower(site_code)) %>%
  left_join(read_xlsx(file.path("data", "01_metadata","loire_headwaters_metadata.xlsx")))

# Discharge data
df_q <- readRDS(file.path("data", "09_loire_data","discharge_plus_v2.RDS")) %>%
  mutate(strahler = round(strahler))

# join data
df_loire <- df_loire %>%
  mutate(date = date(datetime)) %>%
  left_join(df_q)

# Load hypoxia metadata
df_hyp <- read_xlsx(file.path("data", "01_metadata","hypoxia_dates.xlsx"))

# Clean data for only drying hypoxia events and get times right
dry_meta <- filter(df_hyp, type == "drying") %>%
  mutate(start = lubridate::force_tz(start, tzone = "Europe/Berlin"),
         end = lubridate::force_tz(end, tzone = "Europe/Berlin"),
         recovery_start = lubridate::force_tz(recovery_start, tzone = "Europe/Berlin"),
         recovery_end = lubridate::force_tz(recovery_end, tzone = "Europe/Berlin"))

# Only get the data for the drying hypoxia events
df_dry <- ungroup(df_loire) %>%
  select(-position, -number, -confluence) %>%
  distinct() %>%
  fuzzyjoin::fuzzy_semi_join(dry_meta,
                             by = c("site_code" = "site_code",
                                    "datetime" = "start",
                                    "datetime" = "end"),
                             match_fun = list(`==`, `>=`, `<=`))

# Save the data for later use
saveRDS(df_dry, file.path("data", "10_clean_data", "hypoxia_drying.RDS"))
df_dry <- readRDS(file.path("data", "10_clean_data", "hypoxia_drying.RDS"))


# Metadata about pool and riffle ------------------------------------------
meta_geom <- readxl::read_excel("geomorphic_units.xlsx")


# Characterize the hypoxia ------------------------------------------------
# Hypoxia is < 3 mg/L
df_dry_p <- df_dry %>%
  distinct() %>%
  group_by(site) %>%
  # filter(oow == "no" | is.na(oow)) %>%
  mutate(DOhyp = if_else(DO < 3, 1, 0),
         year = year(datetime)) %>%
  mutate(group = cumsum(c(1, diff(datetime) > 1))) %>% # group by drying events
  group_by(site, group) %>%
  mutate(hour = row_number() - 1) %>%
  group_by(site, date, group) %>%
  mutate(color = min(DO) == DO, 
         color = if_else(is.na(color), FALSE, color), # color for plotting
         gs = paste(group, site)) %>%
  ungroup() %>%
  group_by(gs) %>%
  mutate(hourbef = hour - max(hour))


c = ggplot() +
  geom_line(data = df_dry_p,
            aes(x = hourbef / 24,
                y = DO_per, group = gs),
            alpha = 0.5)+
  # geom_point(data = filter(df_dry_p, color == TRUE),
  #            aes(x = hour, y = DO, color = color)) +
  # geom_hline(yintercept = 3, linetype = "dashed", color = "red") +
  # facet_wrap(~site, scales = "free_x")+
  scale_color_manual(values = "blue") +
  theme_bw() +
  theme(legend.position = "none")
c


# Caclulate regressions on DO minima
df_m <- df_dry_p %>%
  group_by(gs, date) %>%
  slice(which.min(DO_per)) %>%
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

ggplot(data = df_m2, #filter(df_m2, `estimate_(Intercept)` > 2.5),
       aes(x = `estimate_(Intercept)`,
            y = estimate_hour)) +
  # facet_wrap(~site) +
  geom_point() +
  theme_bw() +
  stat_smooth(method = "lm") +
  ggpubr::stat_regline_equation() +
  ggpubr::stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "*`,`~")),
                    label.y = -0.1) +
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
  geom_vline(data = df_mean, aes(xintercept = mean, group = name)) + 
  geom_vline(data = df_mean, aes(xintercept = med, group = name), linetype = "dashed") +
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
  left_join(meta_geom, by = "site_code") %>%
  ggplot() +
  geom_line(aes(x = hourbef,
                y = DO, group = gs,
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



# Time until hypoxia
df_th <- df_dry_p %>%
  filter(DOhyp == 1) %>%
  ungroup() %>%
  group_by(site, group) %>%
  filter(hour == min(hour)) %>%
  left_join(ungroup(df_dry_p) %>%
              group_by(site, group) %>%
            filter(hour == 0) %>%
            select(DO_start = DO))


ggplot(data = df_th,
       aes(x = DO_start,
           y = hour)) +
  geom_point() +
  theme_bw() +
  # geom_abline(intercept = 0, slope = 24) +
  labs(title = "time until first instance of hypoxia under drying",
       y = "time until hypoxia (hours)",
       x = "starting DO (mg/L)")

ggsave(filename = "Z:/RHypoxie/Figures/drying_time_until_hypoxia.png",
       dpi = 1200,
       height = 9.2,
       width = 12,
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



# Length of continuous hypoxia
df_hyp_length <- df_dry_p %>% 
  ungroup() %>%
  group_by(site, group) %>%
  mutate(cons = runner::streak_run(DOhyp)) %>%
  filter(DOhyp == 1) %>%
  filter(cons == max(cons))

ggplot(data = df_hyp_length,
       aes(x = strahler,
           y = cons,
           group = strahler)) +
  geom_boxplot()


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






ggplot() +
  geom_line(data = filter(df_dry_p, gs == "2 charpassonne château de donzy"),
            aes(x = hourbef,
                y = DO, group = gs),
            alpha = 0.5)+
  geom_point(data = filter(df_dry_p, color == TRUE, gs == "5 charpassonne le tél"),
             aes(x = hourbef, y = DO, color = color)) +
  geom_hline(yintercept = 4, linetype = "dashed", color = "red") +
  # facet_wrap(~site, scales = "free_x")+
  scale_color_manual(values = "blue") +
  theme_bw() +
  theme(legend.position = "none")



dat = c("4 charpassonne le tél", "5 charpassonne le tél", "3 loise aval doise salt",
        "9 toranche aval", "3 coise la bruyere", "2 charpassonne château de donzy")
c
dat_dry = filter(df_dry_p, gs %in% c(dat))
writexl::write_xlsx(dat_dry, file.path("data", "data_for_remi_model.xlsx"))
