# 
# Purpose: To summarize metabolism in Sâone headwaters
# Author: Jake Diamond
# Date: 7 September 2022
# 

# Load libraries
library(lubridate)
library(streamMetabolizer)
library(tidyverse)


# Load data ---------------------------------------------------------------
df <- readRDS(file.path("results", "rhone_metabolism_bayes_preds_new.RDS")) %>%
  left_join(readRDS(file.path("data", "site_meta_data.RDS")))

# Metabolism plots --------------------------------------------------------
df_met_p <- df %>%
  mutate(GPP = if_else(GPP < 0, 0, GPP),
         ER = if_else(ER > 0, 0, ER)) %>%
  group_by(site) %>%
  imputeTS::na_kalman() %>%
  mutate(across(where(is.numeric),
                ~stats::filter(., rep(1/5, 9), sides = 2))) %>%
  pivot_longer(cols = c(GPP, ER, K600)) %>%
  filter(name != "K600") %>%
  ungroup() %>%
  drop_na() %>%
  mutate(sitef = fct_reorder2(as.factor(site),#paste0(site, "; ", round(area_km2, 1),
                                         #     " km2")),
                              watershed, -area_km2))

# Ardieres plots
p_metabolism_ard <- filter(df_met_p, watershed == "ardieres") %>%
  ggplot(aes(x = date,
             y = value,
             color = name)) +
  theme_bw() + 
  geom_hline(yintercept = 0) +
  geom_point() +
  scale_x_date(date_breaks = "1 month", date_labels = "%m") +
  facet_wrap(~sitef) +
  labs(y = expression("flux ("*g~O[2]~m^{-2}~d^{-1}*")"),
       x = "") +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.85, 0.12))
p_metabolism_ard

ggsave(plot = p_metabolism_ard,
       filename = file.path("results", "ardieres_metabolism_ts.png"),
       dpi = 600,
       units = "cm",
       height = 16,
       width = 18)

# Yzeron plots
p_metabolism_yz <- filter(df_met_p, watershed == "yzeron") %>%
  ggplot(aes(x = date,
             y = value,
             color = name)) +
  theme_bw() + 
  geom_hline(yintercept = 0) +
  geom_point() +
  scale_x_date(date_breaks = "1 month", date_labels = "%m") +
  facet_wrap(~sitef) +
  labs(y = expression("flux ("*g~O[2]~m^{-2}~d^{-1}*")"),
       x = "") +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.85, 0.12))
p_metabolism_yz

ggsave(plot = p_metabolism_yz,
       filename = file.path("results", "yzeron_metabolism_ts.png"),
       dpi = 600,
       units = "cm",
       height = 16,
       width = 18)

# Vauxonne plots
p_metabolism_v <- filter(df_met_p, watershed == "vauxonne") %>%
  ggplot(aes(x = date,
             y = value,
             color = name)) +
  theme_bw() + 
  geom_hline(yintercept = 0) +
  geom_point() +
  scale_x_date(date_breaks = "1 month", date_labels = "%m") +
  facet_wrap(~sitef) +
  labs(y = expression("flux ("*g~O[2]~m^{-2}~d^{-1}*")"),
       x = "") +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "none")
p_metabolism_v

ggsave(plot = p_metabolism_v,
       filename = file.path("results", "vauxonne_metabolism_ts.png"),
       dpi = 600,
       units = "cm",
       height = 9,
       width = 18)

ggplot(data = filter(xx, site == "samsons amont thielas") %>%
         mutate(GPP = if_else(GPP < 0, 0, GPP),
                ER = if_else(ER > 0, 0, ER)),
       aes(x = date,
           y = GPP)) +
  geom_point() +
  geom_line() +
  theme_bw()

y = df %>%
  mutate(GPP = if_else(GPP < 0, 0, GPP),
         ER = if_else(ER > 0, 0, ER)) %>%
  filter(month(date) < 5) %>%
  group_by(site) %>%
  summarize(GPP = mean(GPP, na.rm = T),
            ER = mean(ER, na.rm = T)) %>%
  ungroup()

ggplot(data = filter(df_met_p, name != "K600"),
       aes(x = area_km2,
           y = value,
           color = name)) + 
  stat_summary() +
  theme_bw()+
  scale_x_log10() +
  scale_y_log10()

