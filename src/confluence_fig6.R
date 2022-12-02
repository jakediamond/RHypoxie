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
# library(ggridges)
library(patchwork)
library(ggpubr)
library(tidyverse)

# Load data ---------------------------------------------------------------
# Load DO data
df <- readRDS("clean_data_with_periods") %>%
  mutate(site = tolower(Site))

# Load strahler order info
df_s <- readRDS("Headwaters/Data/Discharge/discharge_plus_v2.RDS") %>%
  distinct(site, strahler) %>%
  mutate(strahler = round(strahler))

# Upstream confluence data
df_conf <- read_csv("paired_upstream_confluences.csv") %>%
  mutate(datetime  = ymd_hms(datetime))

# Read in watershed info
conf_meta <- read_excel("Headwaters/Data/confluence_metadata.xlsx") %>%
  mutate(site = tolower(Site))

# Confluence area ratios
conf_area <- left_join(conf_meta, distinct(df, site, area)) %>%
  filter(position != "down") %>%
  select(conf, pos_num, area) %>%
  pivot_wider(names_from = pos_num, values_from = area) %>%
  group_by(conf) %>%
  mutate(arat = min(up2, up1) / max(up1, up2))

# Confluence analysis -----------------------------------------------------------
# Quick look at time series
# Get samples of good data
df_conf_ts<- left_join(df, conf_meta) %>%
  filter((conf == "fon_vio" & between(date, ymd("2020-03-27"), ymd("2020-04-05"))) |
           (conf == "mar_cur" & between(date, ymd("2019-09-01"), ymd("2019-09-08")))) %>%
  pivot_longer(cols = c(DO_per, DO_temp)) %>%
  mutate(pname = case_when(name == "DO_per" ~ "DO~saturation~('%')",
                           name == "DO_temp" ~ "water~temperature~(degree*C)")) %>%
  group_by(conf) %>%
  mutate(time = (datetime - min(datetime)) / 86400) %>%
  left_join(conf_area) %>%
  mutate(prat = paste0("A[2]:A[1]==", round(arat, 2)),
          week = week(datetime))

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

df_conf_r_p <- df_conf_ts %>%
  distinct(conf, week, arat, prat, pname) %>%
  left_join(df_corp) %>%
  mutate(r = if_else(pname=="DO~saturation~('%')", DO, temp)) %>%
  # filter(year == 2020) %>%
  group_by(conf, arat, prat, pname) %>%
  summarize(r = mean(r)) %>%
  mutate(rtext = paste0("r[{'trib1,trib2'}]==", round(r, 2)),
         ypos = if_else(pname == "DO~saturation~('%')", 115, 20))

# Plot four periods with two watersheds
p_conf_ts <- ggplot(data = df_conf_ts,
                 aes(x = time,
                     y = value)) +
  geom_line(size = 1.1, aes(linetype = pos_num,
                            color = prat,
                            group = pos_num)) +
  scale_color_manual(values = c("#646770", "#efda54")) +
  scale_linetype_manual(name = "position", labels = c("downstream", "trib1", "trib2"),
                        values = c("solid", "dashed", "dotted")) +
  scale_x_continuous(breaks = seq(1,7,1), limits = c(0,7), expand = c(0,0)) +
  facet_grid(pname~prat, scales = "free", switch = "y", labeller = label_parsed) +
  guides(color = FALSE) +
  geom_text(data = df_conf_r_p, aes(x = 3.5, y = ypos, label = rtext), parse = TRUE, size = 3.5) + 
  theme_bw(base_size = 9) +
  theme(legend.position = c(0.09, 0.18),
        legend.text=element_text(size=7),
        legend.title = element_text(size = 7),
        # legend.margin=margin(b = 0, unit='cm'),
        legend.box.background = element_rect(color = "black", fill = "transparent"),
        axis.title.y = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent"),
        # panel.spacing=unit(0, "lines"),
        strip.background = element_blank(),
        strip.placement = "outside",
        axis.title.x=element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        # axis.text.x=element_blank(),
        # axis.ticks.x=element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm")) +
  labs(y = "",
       x = "")
p_conf_ts
p_conf_ts <- egg::tag_facet(p_conf_ts, open = "", close = "", fontface =1 ) + #, x = ymd_h("2020032702")
  theme(strip.text = element_text())
p_conf_ts


# Correlations of upstream confluence data
df_cor <- df_conf %>%
  mutate(week = week(date),
         year = year(date)) %>%
  group_by(conf, name, year, week) %>%
  nest() %>%
  mutate(rho = map(data, ~cor(x = .$up2, y = .$up1, method = "pearson"))) %>%
  select(-data) %>%
  unnest(cols = c(rho))

df_corp <- df_cor %>%
  filter(name %in% c("DO", "temp")) %>%
  pivot_wider(names_from = name, values_from = rho) %>%
  left_join(conf_area)

p_trib_ts <- ggplot(data = filter(df_corp, DO > 0),
       aes(x = week, y = DO/temp, color = arat, group = arat)) +
  # geom_line() +
  stat_smooth(geom = "line", size = 2) +
  theme_bw(base_size = 9) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(breaks = seq(0,1,0.25)) +
  coord_cartesian(xlim = c(10,40), ylim = c(0, 1.1)) +
  scale_color_viridis_c(name = expression(A[2]*":"*A[1]), 
                        option = "E") +
  theme(panel.grid = element_blank(),
        plot.tag.position = c(0.12, 0.95),
        legend.position = c(0.1, 0.35),
        legend.background = element_rect(color = "black")) +
  annotate(geom = "text", x = 28, y =1.08, label = "tributary DO correlation = temp. correlation") +
  annotate(geom = "text", x = 22, y = 0.1, label = "tributary DO correlation << temp correlation") +
  labs(y = expression(r[DO]~`:`~r[temp]~"for confluence tributaries"),
       tag = "e")
  # scale_x_continuous(limits = c(0, 1)) +
  # scale_y_continuous(limits = c(-1, 1)) +
  # geom_abline(slope = 1, intercept = 0, linetype = "dashed")  
p_trib_ts





fig6 <- cowplot::plot_grid(p_conf_ts, p_trib_ts, ncol = 1, align = 'v', axis = 'l')
# fig6
ggsave(plot = fig6,
       filename = "Headwaters/Figures/fig6_confluences_ratio.png",
       dpi = 600, width = 18.4, height = 18.4, units = "cm")
