# 
# Purpose: To do a first look at all the headwaters data
# Author: Jake Diamond
# Date: November 1, 2020
# 

# Load libraries
library(lubridate)
library(hydrostats)
library(scales)
library(tidyverse)

# Load data
dfq <- readRDS(file.path("data", "04_discharge and stage", "hourly_discharge_all.RDS"))

# Only data we need is site, datetime, and Q, get names common format
dfq <- select(dfq, -h_cm) %>%
  mutate(siteq = str_replace_all(siteq, " ", "_"),
         siteq = tolower(siteq))

# Nest the data for easy use
dfqn <- dfq %>%
  mutate(year = year(datetime)) %>%
  # date = as.POSIXct.Date(date)) %>%
  select(siteq, year, Date = datetime, Q = q_m3s) %>%
  group_by(siteq, year) %>%
  drop_na(Q) %>%
  nest()

# Baseflow analysis using Lyne–Hollick recursive digital filter
dfbf <- dfqn %>%
  mutate(bf = map(data, baseflows, ts = "daily")) %>%
  select(-data) %>%
  unnest(cols = bf)

ggplot(data = filter(dfbf, bfi <1),
       aes(x = Date,
           y = Q, 
           color = bfi)) +
  scale_color_viridis_c() + 
  geom_line() + 
  theme_bw() +
  facet_grid(year~siteq, space = "free", scales = "free_x")

# cease-to-flow analysis
dfctf <- dfqn %>%
  mutate(ctf = map(data, CTF, threshold = 0.001)) %>%
  select(-data) %>%
  unnest(cols = ctf)

# Low flow analysis
dflf <- dfqn %>%
  mutate(lf = map(data, low.spell.lengths, ind.days = 48,
                  ctf.threshold = 0.001, ignore.zeros = T)) %>%
  select(-data) %>%
  unnest(cols = lf)

dflf2 <- dfqn %>%
  mutate(lf = map(data, low.spells)) %>%
  select(-data) %>%
  unnest(cols = lf)

# Save this data
saveRDS(dflf, file.path("data", "10_clean_data", "drying_starts_lengths.RDS"))

# high flow analysis
dfhf <- dfq %>%
  mutate(year = year(datetime)) %>%
  # date = as.POSIXct.Date(date)) %>%
  select(siteq, year, Date = datetime, Q = q_m3s) %>%
  group_by(siteq, year) %>%
  drop_na(Q) %>%
  nest() %>%
  mutate(hf = map(data, high.spells, quant = 0.7, ind.days = 24,
                   ignore.zeros = TRUE, ctf.threshold = 0.001)) %>%
  select(-data) %>%
  unnest(cols = hf)

dfhfl <- dfq %>%
  mutate(year = year(datetime)) %>%
  # date = as.POSIXct.Date(date)) %>%
  select(siteq, year, Date = datetime, Q = q_m3s) %>%
  group_by(siteq, year) %>%
  drop_na(Q) %>%
  nest() %>%
  mutate(hfl = map(data, high.spell.lengths, quant = 0.7, ind.days = 24,
                  ignore.zeros = TRUE, ctf.threshold = 0.001)) %>%
  select(-data) %>%
  unnest(cols = hfl)

saveRDS(dfhfl, file.path("data", "10_clean_data", "storm_starts_lengths.RDS"))

dfhyd <- dfq %>%
  mutate(year = year(datetime)) %>%
  # date = as.POSIXct.Date(date)) %>%
  group_by(siteq) %>%
  summarize(mean = mean(q_m3s, na.rm = T),
         median = median(q_m3s, na.rm = T),
         cv = sd(q_m3s, na.rm = T) / mean)


# Quick summary
dfhfl %>%
  summarize(sum = n(),
            meanl = mean(spell.length),
            medl = median(spell.length),
            sd = sd(spell.length),
            minl = min(spell.length),
            maxl = max(spell.length))
distinct(storm_meta, siteq)


# Quick summary
dflf %>%
  ungroup() %>%
  summarize(sum = n(),
            meanl = mean(spell.length),
            medl = median(spell.length),
            sd = sd(spell.length),
            minl = min(spell.length),
            maxl = max(spell.length))
