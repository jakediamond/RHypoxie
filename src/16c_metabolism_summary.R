# 
# Purpose: To summarize metabolism in Sâone headwaters
# Author: Jake Diamond
# Date: 7 September 2022
# 

# Load libraries
library(lubridate)
library(ggpubr)
library(streamMetabolizer)
library(tidyverse)
library(tidytable)

# Load data ---------------------------------------------------------------
df_rhone <- readRDS(file.path("results", "rhone_metabolism_bayes_preds_new.RDS")) %>%
  left_join(df_d) %>%
  distinct() %>%
  mutate(k_md =depth*K600)

df_loire <- readRDS(file.path("data", "09_loire_data", 
                              "metab_DO_summary.RDS")) %>%
  select(site, date, GPP, ER, K600=k, depth) %>%
  distinct() %>%
  mutate(k_md = depth*K600)

df_met <- bind_rows(df_rhone, df_loire) %>%
  mutate(site = if_else(site == "coise aval coizet", "coise aval vaudragon", site)) #%>%
  # left_join(readRDS(file.path("data", "site_meta_data.RDS")))

rollmean <- tibbletime::rollify(~mean(.x, na.rm = TRUE), window = 7)
rollmean30 <- tibbletime::rollify(~mean(.x, na.rm = TRUE), window = 30)

df_temp <- readRDS(file.path("data", "10_clean_data", 
                        "hourly_data_all_including2022.RDS")) %>%
  select(site, date, temp) %>%
  group_by(site) %>%
  mutate(temp_7 = rollmean(temp),
         temp_30 = rollmean30(temp)) %>%
  summarize(temp_mean = mean(temp, na.rm = T),
            temp_max7 = max(temp_7, na.rm = T),
            temp_max30 = max(temp_30, na.rm = T)) %>%
  ungroup()

df_met_DO <- df_met %>%
  mutate(GPP = if_else(GPP < 0, 0, GPP),
         ER = if_else(ER > 0, 0, ER)) %>%
  mutate(NEP = GPP + ER) %>%
  group_by(site) %>%
  summarize(GPP = mean(GPP, na.rm = T),
            ER = mean(ER, na.rm = T),
            NEP = mean(NEP, na.rm = T),
            K600 = mean(K600, na.rm = T),
            k_md = mean(k_md, na.rm = T)) %>%
  left_join(df_temp)

df_met_DO <- df_met_DO %>%
  left_join(select(df_hyp_per, site, per_hyp = per)) %>%
  left_join(df %>%
              group_by(site) %>%
              summarize(DOmin = min(DO, na.rm = TRUE),
                        DO10 = quantile(DO, 0.1, na.rm = T)))

write_excel_csv(df_met_DO, file = file.path("data", "DO_temp_met_summary.csv"))

ggplot(data= filter(df, between(K600, 1000, 5000)),
       aes(x = K600,
           y = -ER,
           color = month(date))) +
  geom_point()+
  facet_wrap(~site)

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
                              watershed, -area_km2)) %>%
  left_join(df_temp)

filter(df_met_p, name == "GPP") %>%
  ggplot(aes(x = area_km2,
             y = value,
             color = watershed)) +
  stat_summary() +
  scale_x_log10() +
  scale_y_log10()

ggviolin(filter(df_met_p, name == "ER"), x = "position", 
         y = "value", fill = "position",
         palette = c("#00AFBB", "#E7B800", "#FC4E07"),
         facet.by = "confluence",
         add = "boxplot", add.params = list(fill = "white"))+
  stat_compare_means(comparisons = list( c("down", "up_1"), 
                                         # c("up_1", "up_2"), 
                                         c("down", "up_2") ),
                     label = "p.signif")  

ggviolin(filter(df_loire, name == "DO_def_min") %>%
           mutate(site = if_else(site == "coise aval coizet", 
                                 "coise aval vaudragon", site)) %>%
           left_join(readRDS(file.path("data", "site_meta_data.RDS"))), 
         x = "position", 
         y = "def_sum", fill = "position",
         palette = c("#00AFBB", "#E7B800", "#FC4E07"),
         facet.by = "confluence",
         add = "boxplot", add.params = list(fill = "white"))+
  stat_compare_means(comparisons = list( c("down", "up_1"), 
                                         # c("up_1", "up_2"), 
                                         c("down", "up_2") ),
                     label = "p.signif")  

# Confluence plots
ggplot(data = filter(df_met_p, name == "ER"),
       aes(x = position,
           y = value,
           fill = position)) +
  geom_violin(add = "boxplot", add.params = list(fill = "white")) +
  # stat_summary() +
  facet_wrap(~confluence) +
  scale_fill_manual(values =c("#00AFBB", "#E7B800", "#FC4E07")) +
  theme_classic() +
  ggpubr::stat_compare_means(comparisons = list( c("down", "up_1"), 
                                                 # c("up_1", "up_2"), 
                                                 c("down", "up_2") ),
                             label = "p.signif")#+ # Add pairwise comparisons p-value
  # ggpubr::stat_compare_means()  


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

