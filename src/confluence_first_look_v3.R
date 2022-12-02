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
chem <- read_excel("Data/Loire_headwaters_data_share.xlsx",
                   sheet = "water_chemistry") %>%
  select(-`sample bottle`) %>%
  group_by(site) %>%
  mutate_if(is.character, parse_number) %>%  #convert LDL to the DL
  ungroup() %>%
  pivot_longer(-c(datetime, site)) %>%
  left_join(read_excel("Data/water_chemistry/detection_limits.xlsx")) %>%
  mutate(value = if_else(value < dl, value * 0.5, value)) %>% #take half of detection limit
  pivot_wider(id_cols = c("site", "datetime"), names_from = name)

# Get data into as P or as N, already done for february 2020 samples!!
chem <- chem %>%
  filter(month(datetime) != 2) %>%
  mutate(NH4 = 14.01 * NH4 / 18.04,
         NO2 = 14.01 * NO2 / 46.01,
         NO3 = 14.01 * NO3 / 62,
         PO4 = 30.97 * PO4 / 94.97) %>%
  bind_rows(filter(chem, month(datetime) == 2)) %>%
  mutate(date = date(datetime)) %>%
  rename(Site = site) %>%
  mutate(site = tolower(Site))

# Load DOM uv vis data
df_uv <- read_xlsx("Data/water_chemistry/uv_spectra.xlsx")

# Load water chem data
df_match <- read_xlsx("Data/water_chemistry/Water_chemistry_all_v2.xlsx") %>%
  mutate(year = year(datetime),
         month = month(datetime)) %>%
  # filter(year == 2020) %>%
  select(site, year, month, sampleid = `Sample bottle no.`)

# Combine UV and chem data
df_uv <- left_join(df_uv, df_match, by = c("sampleid", "month", "year")) %>%
  mutate(site = coalesce(site.x, site.y)) %>%
  select(-site.x, -site.y) %>%
  mutate(site = tolower(site)) %>%
  filter(is.na(soiltype)) %>%
  select(-soiltype, -sampleid, -type)

# Get meta data
meta <- read_xlsx("Data/Loire_headwaters_data_share.xlsx", 
                  sheet = "confluence_meta")
# Load data
iso <- read_xlsx("Data/water_chemistry/isotopes/isotope_results.xlsx") %>%
  select(-area) %>%
  left_join(meta) %>%
  rename(Site = site) %>%
  mutate(site = tolower(Site))

# Combine all chem data
df <- left_join(chem, select(iso, -NO3, - Site, -month), by = c("site", "date")) %>%
  left_join(df_uv, by = c("site", "date"))

# Load discharge data
df_q <- readRDS("Data/Discharge/discharge_plus_v2.RDS") %>%
  select(site, date, q, Q)

# join data
conf <- df %>%
  select(-`sampleid`, -datetime, -month, -year) %>%
  ungroup() %>%
  drop_na(name) %>%
  rename(conf = name) %>%
  mutate(number = case_when(
    number == 1 ~ "up1",
    number == 2 ~ "up2",
    number == 3 ~ "down")) %>%
  # left_join(df_q) %>%
  pivot_longer(cols = where(is.numeric)) %>%
  select(-site,-Site,-watershed) %>%
  pivot_wider(names_from = number, values_from = value)

# mixing ratios
conf_r <- conf %>%
  mutate(r = (down - up2) / (up1 - down)) %>% #ratio of Q1/Q2
  drop_na() %>%
  mutate(month = month(date))

ggplot(data = filter(conf_r,
                     name %in% c("Ca", "Cl", "cond", "K", "Mg", "Na")),
       aes(x = conf, y = r, color = name)) +
  geom_point() +
  facet_wrap(~month, scales = "free")

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
