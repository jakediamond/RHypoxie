# -------------------------------------
# Author: Jake Diamond
# Purpose: first look at confluence water chemistry
# Date: 12 octobre 2020
# -------------------------------------

# set working directory
# setwd("C:/Users/diamo/Dropbox/Projects/Loire_DO")
# setwd("Z:/Loire_DO/Headwaters")
setwd("C:/Users/jake.diamond/Dropbox/Projects/Loire_headwaters/Headwaters")

# load libraries
library(lubridate)
library(readxl)
library(ggrepel)
library(tidyverse)


# load data
df <- read_xlsx("Data/Loire_headwaters_data_share.xlsx", 
                sheet = "water_chemistry")
meta <- read_xlsx("Data/Loire_headwaters_data_share.xlsx", 
                  sheet = "confluence_meta")
iso <- read_xlsx("Data/Loire_headwaters_data_share.xlsx", 
                 sheet = "isotopes")

# join data
conf <- df %>%
  mutate(month = month(datetime),
         year = year(datetime)) %>%
  select(-`sample bottle`, -datetime) %>%
  group_by(site, year, month) %>%
  nest() %>%
  ungroup() %>%
  left_join(meta) %>%
  drop_na() %>%
  arrange(year, month, name, number)

# ideal mixing based on chloride
conf_cl <- conf %>%
  mutate(cl = map(data, pull, "Cl")) %>%
  select(-site, -data) %>%
  group_by(name, year, month) %>%
  pivot_wider(names_from = number, values_from = cl) %>%
  ungroup() %>%
  # drop_na() %>%
  unnest(cols = c(`1`, `2`, `3`)) %>%
  mutate(r = (`3` - `2`) / (`1` - `3`)) #ratio of Q1/Q2
  
# now estimate conservative mixing for each element
conf_cons <- conf %>%
  left_join(select(conf_cl, month, year, name, r)) %>%
  drop_na() %>%
  group_by(name, month, year) %>%
  filter(n() ==3)

# Get data in good format
conf_est <- df %>%
  left_join(select(iso, -`sample bottle`), by = c("site", "datetime")) %>%
  mutate(date = date(datetime)) %>%
  select(-`sample bottle`, -datetime) %>%
  group_by(site) %>%
  mutate(across(.cols = where(is.character), 
                .fns = ~ if_else(str_detect(., pattern="<"), 
                                 NA_real_, 
                                 parse_number(.))
                )
         ) %>% # divide detection limits by half
  ungroup() %>%
  group_by(site, date) %>%
  pivot_longer(cols = -c(site, date), names_to = "solute") %>%
  ungroup() %>%
  left_join(meta) %>%
  drop_na() %>%
  group_by(name, date, solute) %>%
  select(-site) %>%
  pivot_wider(names_from = number)
  
# Join with estimates of r
conf_cons <- conf_est %>%
  ungroup() %>%
  mutate(year = year(date),
         month = month(date)) %>%
  left_join(select(conf_cl, year, month, name, r)) %>%
  mutate(cons_mix = (r*`1` + `2`) / (1 + r)) %>%
  drop_na()

# estimate error
conf_cons <- conf_cons %>%
  mutate(error = (`3` - cons_mix) / cons_mix * 100)

# summarize
conf_sum <- conf_cons %>%
  filter(r >0) %>%
  group_by(month, name, solute) %>%
  summarize(error = mean(error)) %>%
  mutate(dir = if_else(error <=0, "negative", "positive"))


p_cons <- ggplot(filter(conf_sum,
              solute %in% c("DOC-C", "NO3", "SO4", "Na", "18O", "15N")),
       aes(x = fct_reorder(name, error),
           y = error)) +
  # geom_boxplot() +
  geom_col(aes(fill = dir)) +
  ylab("difference from conservative mixing (%)") +
  xlab("") +
  # geom_jitter(aes(color = name)) + 
  scale_fill_manual(values = c("blue", "dark red")) +
  guides(fill  = FALSE) + 
  # geom_text_repel(aes(label = name, color = name)) +
  # geom_line() +
  facet_grid(solute~month, scales = "free") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.2))
  # scale_y_continuous(limits = c(-80,40),
  #                    breaks = seq(-80, 40, 20))

ggsave(plot = p_cons,
       filename = "Figures/confluence_mixing_chemistry.png",
       dpi = 600,
       width = 18.4,
       height = 16,
       units = "cm")
