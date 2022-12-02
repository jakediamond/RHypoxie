# -------------------------------------
# Author: Jake Diamond
# Purpose: Compare DO and temperature time series before drying
# Date: July 10, 2020
# -------------------------------------

# Set working directory
# setwd("Z:/Loire_DO")
# setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")
# setwd("C:/Users/diamo/Dropbox/Projects/Loire_headwaters")
setwd("C:/Users/jake.diamond/Dropbox/Projects/Loire_headwaters")

# Load libraries
library(lubridate)
library(readxl)
library(broom)
library(ggridges)
library(patchwork)
library(ggpubr)
library(tidyverse)

# Load DO data
df <- readRDS("clean_data_with_periods") %>%
  mutate(site = tolower(Site))

# Load strahler order info
df_s <- readRDS("Headwaters/Data/Discharge/discharge_plus_v2.RDS") %>%
  distinct(site, strahler) %>%
  mutate(strahler = round(strahler))

# Periods before streams are dry
df_pds <- tibble(site_code = c("cha002", "cha007", "cha031", "cha057", "coi023", "coi071", "coi006", "doi028", "fon010", "fon013", "loi039", "loi068", "loi132", "mou019", "mou021", "tor055", "tor063", "car004", "fon010"),
                 periodd = c("drying", "drying", "drying", "drying", "drying", "drying", "drying", "drying", "drying", "drying", "drying", "drying", "drying", "drying", "drying", "drying", "drying", "drying", "drying"),
                 startd = c("2020062211", "2020063010", "2020070317", "2020072315", "2020072413", "2020072513", "2020062909", "2020070919", "2020090813", "2020082915", "2019070313", "2020072314", "2020082916", "2020071812", "2020070907", "2020071817", "2020070514", "2020070710", "2020072410"),
                 endd = c("2020062919", "2020070804", "2020071414", "2020072706", "2020080107", "2020080113", "2020070719", "2020071401", "2020091320", "2020090819", "2019071519", "2020072709", "2020090504", "2020072901", "2020071500", "2020073004", "2020071122", "2020071419", "2020080110")) %>%
  mutate(startd = ymd_h(startd), endd = ymd_h(endd))

# Periods after streams rewet
df_pds_wet <- tibble(site_code = c("cha007", "cha010", "cha031", "cha034", "cha057", "coi071", "coi006", "doi028", "fon003", "fon010", "fon013", "loi039", "loi068", "loi132", "mou004", "mou019", "mou021", "tor055", "tor063", "car004"),
                 periodd = c("wetting", "wetting", "wetting", "wetting", "wetting",  "wetting", "wetting", "wetting", "wetting", "wetting", "wetting", "wetting", "wetting", "wetting", "wetting", "wetting", "wetting", "wetting", "wetting", "wetting"),
                 startd = c("2020092714", "2020092006", "2020082900", "2019072103", "2020081407", "2020081912", "2020080120", "2019072100", "2020081400", "2020081400", "2020081400", "2019072100", "2020083000", "2020092400", "2020081400", "2020082900", "2020082900", "2020092800", "2020092312", "2020092600"),
                 endd = c("2020100118", "2020092706", "2020090600", "2019072703", "2020082107", "2020081512", "2020080220", "2019072700", "2020082100", "2020082100", "2020082100", "2019072700", "2020090400", "2020100200", "2020082000", "2020083020", "2020090600", "2020093023", "2020093000", "2020092800")) %>%
  mutate(startd = ymd_h(startd), endd = ymd_h(endd))


df_conf <- read_csv("paired_upstream_confluences.csv")

df_cor <- df_conf %>%
  group_by(conf, name, date) %>%
  nest() %>%
  mutate(rho = map(data, ~cor(x = .$up2, y = .$up1, method = "pearson"))) %>%
  select(-data) %>%
  unnest(cols = c(rho))

df_corp <- df_cor %>%
  filter(name %in% c("DO", "temp")) %>%
  pivot_wider(names_from = name, values_from = rho)

ggplot(data = filter(df_corp, conf == "char_car"),
       aes(x = temp, y = DO, color = conf)) +
  geom_point() +
  theme_bw(base_size = 9) +
  geom_hline(yintercept = 0) +
  scale_color_viridis_d() +
  theme(panel.grid = element_blank()) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(-1, 1)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed")  











# Long format with temperature and oxygen
df_dry <- df %>%
  # left_join(df_pds, by = "site_code") %>%
  left_join(bind_rows(df_pds, df_pds_wet), by = "site_code") %>%
  mutate(period = if_else((datetime >= startd & datetime <= endd), periodd, period)) %>%
  mutate(period = if_else((month > 9 & year(date) == 2019), "leafofffall", period)) %>%
  filter(period %in% c("drying", "wetting", "leafoff", "leafon", "leafofffall")) %>%
  mutate(site = tolower(Site)) %>%
  left_join(df_s) %>%
  select(site_code, Site, site, datetime, period, DO_per, DO_temp, area, strahler, startd, endd) %>% #, startd
  pivot_longer(cols = c(DO_per, DO_temp))

df_dry_p <- filter(df_dry, period == "drying") %>%
  mutate(enddround = round_date(endd, unit = "day"),
         # startdround = round_date(startd, unit = "day"),
         # afterdry = (datetime - startdround) / 86400,
         beforedry = (datetime - enddround) / 86400) %>%
  filter(beforedry < 0, beforedry > -8) %>%
  # filter(beforedry > -8, beforedry <0) %>%
  group_by(name, beforedry, strahler) %>% #beforedry,
  summarize(value  = mean(value, na.rm = T)) %>%
  mutate(stra = as.factor(strahler),
         pname = case_when(name == "DO_per" ~ "DO~saturation~('%')",
                           name == "DO_temp" ~ "water~temperature~(degree*C)"))

df_wet_p <- filter(df_dry, period == "wetting") %>%
  mutate(enddround = round_date(endd, unit = "day"),
         startdround = round_date(startd, unit = "day"),
         afterdry = (datetime - startdround) / 86400,
         beforedry = (datetime - enddround) / 86400) %>%
  filter(afterdry > 0, afterdry < 8) %>%
  # filter(beforedry > -8, beforedry <0) %>%
  group_by(name, afterdry, strahler) %>% #beforedry,
  summarize(value  = mean(value, na.rm = T)) %>%
  mutate(stra = as.factor(strahler),
         pname = case_when(name == "DO_per" ~ "DO~saturation~('%')",
                           name == "DO_temp" ~ "water~temperature~(degree*C)"))

dry_p <- ggplot(data = df_dry_p,
                aes(x = beforedry,
                     y = value, color = stra, group = stra)) +
  geom_line(size = 1.2, alpha = 0.5)  +
  scale_color_viridis_d(name = "Strahler order") + 
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  facet_wrap(~pname, scales = "free", switch = "y", labeller = label_parsed,) +
  theme_bw(base_size = 9) +
  labs(x = "days before dry") +
  theme(legend.position = c(0.11, 0.12),
        legend.box.background = element_rect(color = "black", fill = "transparent"),
        plot.tag.position = c(0.08, 0.93),
        axis.title.y = element_blank(),
        # plot.margin = unit(c(0, 0, 0, 0), "cm"),
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside")
dry_p


dry_p2<- ggplot(data = df_dry_p,
                aes(x = beforedry,
                    y = value, color = stra, group = stra)) +
  geom_line(size = 1.2, alpha = 0.5)  +
  scale_color_viridis_d(name = "Strahler order") + 
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  facet_wrap(~pname, scales = "free", switch = "y", labeller = label_parsed, ncol = 1) +
  theme_bw(base_size = 9) +
  labs(x = "days before dry") +
  theme(legend.position = c(0.11, 0.65),
        legend.box.background = element_rect(color = "black", fill = "transparent"),
        plot.tag.position = c(0.08, 0.93),
        axis.title.y = element_blank(),
        # plot.margin = unit(c(0, 0, 0, 0), "cm"),
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside")
dry_p2


wet_p2<- ggplot(data = df_wet_p,
                aes(x = afterdry,
                    y = value, color = stra, group = stra)) +
  geom_line(size = 1.2, alpha = 0.5)  +
  scale_color_viridis_d(name = "Strahler order") + 
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  facet_wrap(~pname, scales = "free", switch = "y", labeller = label_parsed, ncol = 1) +
  theme_bw(base_size = 9) +
  labs(x = "days after rewetting") +
  theme(legend.position = "none",
        plot.tag.position = c(0.08, 0.93),
        axis.title.y = element_blank(),
        # plot.margin = unit(c(0, 0, 0, 0), "cm"),
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside")
wet_p2


dry_p2 | wet_p2


p_dat <- rename(df_dry_p, time = beforedry) %>%
  mutate(type = "drying") %>%
  bind_rows(rename(df_wet_p, time = afterdry) %>%
              mutate(type = "wetting"))


fig_5 <- ggplot(data = filter(p_dat, time > -7, time < 7),
                aes(x = time,
                    y = value, color = stra, group = stra)) +
  geom_line(size = 1.2, alpha = 0.5)  +
  scale_color_viridis_d(name = "Strahler order") + 
  guides(colour = guide_legend(override.aes = list(alpha = 1),
                               direction = "horizontal", title.position = "top",
                               )) +
  facet_grid(pname~type, scales = "free", switch = "y", labeller = label_parsed) +
  theme_bw(base_size = 9) +
  labs(x = "days before or after dry") +
  theme(legend.position = c(0.188, 0.575),
        plot.tag.position = c(0.08, 0.93),
        legend.text=element_text(size=7),
        legend.title = element_text(size = 7),
        legend.margin=margin(b = 0, unit='cm'),
        legend.box.background = element_rect(color = "black", fill = "transparent"),
        axis.title.y = element_blank(),
        # legend.direction = "horizontal",
        legend.title.align = 0.5,
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside")
fig5 <- egg::tag_facet(fig_5, open = "", close = "", fontface =1) + theme(strip.text = element_text())
fig5
ggsave(plot = fig5,
       filename = "Headwaters/Figures/fig5_drying_and_rewetting.png",
       dpi = 600,
       device = "png",
       height = 10,
       width = 12,
       units = "cm")




# Load shade data
shade <- readRDS("Headwaters/Data/shade_site_v4.rds")
# Fill in gaps for 2020
shade2 <- mutate(shade, year = year(datetime)) %>%
  filter(year == 2018, between(month, 8, 10))
year(shade2$datetime) <- 2020
shade <- dplyr::select(shade2, -year) %>%
  bind_rows(shade)

df_shade <- shade %>%
  left_join(df)

df_shade_sum <- filter(df_shade, !(SF <= 0.01 & (hour < 6 | hour >20))) %>%
  mutate(date = date(datetime)) %>%
  group_by(site, date, area) %>%
  summarize(shade = mean(SF, na.rm = T))

# Shade summary plots
ggplot(data=filter(df_shade, !is.na(area), between(hour, 12, 14), between(month, 3, 10)),
       aes(x = area,
           y = SF, color = month, group = month)) +
  # geom_point() +
  stat_summary() +
  facet_wrap(~month) +
  scale_color_viridis_c() +
  theme_bw()+
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x, n = 4),
                labels = scales::trans_format("log10", scales::math_format(10^.x)),
                limits = c(1, 1000)) +
  annotation_logticks(sides = "b") +
  labs(x = expression("watershed area ("*km^2*")"),
       y = "mean daily shade factor (-)")
ggsave(filename = "Headwaters/Figures/mean_daily_shade_factor_by_area_v2.png",
       dpi = 300, width = 12, height = 10, units = "cm")

y = filter(df_shade_sum, is.na(area))

z = filter(df, site == "carrat")
# Density ridges
ggplot(data = filter(df_shade, !is.na(area), between(hour, 12, 14), between(month, 3, 10)), 
       aes(x = SF, y = as.factor(area), fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3, size = 0.3, rel_min_height = 0.01) +
  facet_wrap(~month) +
  scale_fill_viridis_c(name = "SF", option = "C", direction = -1) +
  labs(y=expression("watershed area ("*km^2*")"), x= "shade factor (-)") +
  theme_bw(base_size = 9) +
  theme(legend.position = "none")
ggsave(filename = "Headwaters/Figures/shade_factor_dist_by_site_new.png",
       dpi = 600, width = 9.2, height = 9.2, units = "cm")


shade_p <- filter(df_shade, between(hour, 8, 17), between(month, 3, 10)) %>%
  left_join(df_s) %>% mutate(stra = as.factor(strahler))

# Shade summary plots
fig_shade <- ggplot(data=shade_p,
       aes(x = month,
           y = SF, group = stra, color = stra)) +
  stat_summary(fun = var, geom = "line", size = 1.2) +
  stat_summary(fun = var, geom = "point", size = 2) +
  scale_color_viridis_d(name = "Strahler \n order") +
  scale_x_continuous(breaks = seq(3,10,1)) +
  theme_bw(base_size = 9)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.key.height = unit(0.3, 'cm'),
        legend.margin=margin(t = 0.1, b = 0.1, unit='cm'),
        legend.position = c(0.1, 0.81),
        legend.background = element_rect(fill = "transparent", color = "black")) +
  labs(x = "month",
       y = "across site variance in daily shade factor (-)")

ggsave(plot = fig_shade,
       filename = "Headwaters/Figures/shade_factor_monthly_variance.png",
       dpi = 600, width = 9.2, height = 9.2, units = "cm")
