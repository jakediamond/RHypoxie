# 
# Purpose: To load discharge data
# Author: Jake Diamond
# Date: 1 February 2022
# 

# Set working directory
setwd("Z:/RHypoxie")
# setwd("C:/Users/jake.diamond/Dropbox/Projects/RHypoxie")

# Load libraries
library(readxl)
library(scales)
library(tidyverse)

# Load data
# First get data path and names of files for discharge and stage
data_path <- "Data/04_discharge and stage/raw_bdoh_data"
files_q <- dir(data_path, pattern = "*discharge.xlsx")
files_h <- dir(data_path, pattern = "*stage.xlsx")

# Then load all discharge data 
dfq <- tibble(path = c(rep(data_path, length(files_q))),
             filename = files_q) %>%
  mutate(file_contents = map2(path, filename,
                              ~ read_xlsx(file.path(.x, .y),
                                         skip = 2,
                                         col_names = TRUE,
                                         col_types = c("date", "numeric",
                                                       "guess", "guess", "guess")))
         ) %>%
  unnest(cols = c(file_contents)) %>%
  mutate(siteq = word(filename, 1, sep = "_")) %>%
  select(siteq, 
         datetime = DateHeure, 
         q_m3s = Valeur) %>%
  mutate(q_m3s = if_else(!(siteq %in% c("ardieres", "morcilles")), #yzeron sites in L/s
                           q_m3s / 1000,
                         q_m3s
                         )
         )

# and stage data 
dfh <- tibble(path = c(rep(data_path, length(files_h))),
              filename = files_h) %>%
  mutate(file_contents = map2(path, filename,
                              ~ read_xlsx(file.path(.x, .y),
                                          skip = 2,
                                          col_names = TRUE,
                                          col_types = c("date", "numeric",
                                                        "guess", "guess", "guess")))
  ) %>%
  unnest(cols = c(file_contents)) %>%
  mutate(siteq = word(filename, 1, sep = "_")) %>%
  select(siteq, 
         datetime = DateHeure, 
         h_cm = Valeur) %>%
  mutate(h_cm = if_else(!(siteq %in% c("ardieres", "morcilles")), # yzeron sites in mm
                         h_cm / 10,
                         h_cm
  )
  )

# Two more sites from hydroreel, ardières beajeu
df_q2 <- read_xlsx("Data/04_discharge and stage/raw_hydroportail_data/ardieres_beaujeu/Q.xlsx") %>%
  select(datetime = `Date (TU)`, q_m3s = `Valeur (en m3/s)`) %>%
  mutate(siteq = "ardieres_beaujeu")

df_h2 <- read_xlsx("Data/04_discharge and stage/raw_hydroportail_data/ardieres_beaujeu/H.xlsx") %>%
  select(datetime = `Date (TU)`, h_cm = `Valeur (en cm)`) %>%
  mutate(siteq = "ardieres_beaujeu")

df2 <- full_join(df_q2, df_h2)

# And yzeron, craponne
df_q3 <- read_xlsx("Data/04_discharge and stage/raw_hydroportail_data/yzeron_craponne/Q.xlsx") %>%
  select(datetime = `Date (TU)`, q_m3s = `Valeur (en m3/s)`) %>%
  mutate(siteq = "yzeron_craponne")

df_h3 <- read_xlsx("Data/04_discharge and stage/raw_hydroportail_data/yzeron_craponne/H.xlsx") %>%
  select(datetime = `Date (TU)`, h_cm = `Valeur (en cm)`) %>%
  mutate(siteq = "yzeron_craponne")

df3 <- full_join(df_q3, df_h3)

#  all data
df <- full_join(dfh, dfq) %>%
  bind_rows(df2) %>%
  bind_rows(df3)

# Summarize by 15 minutes
df_15min <- df %>%
  arrange(siteq, datetime) %>%
  mutate(mins = floor_date(datetime, "15 minutes"),
         q_m3s = if_else(q_m3s < 0, NA_real_, q_m3s),
         h_cm = if_else(h_cm < 0, NA_real_, h_cm)) %>%
  group_by(siteq, mins) %>%
  summarize(q_m3s = mean(q_m3s, na.rm = T),
            h_cm = mean(h_cm, na.rm = T)) %>%
  # mutate(q_m3s = imputeTS::na_kalman(q_m3s),
  #        h_cm = imputeTS::na_kalman(h_cm)) %>%
  ungroup() %>%
  rename(datetime = mins)

# Quick plot
ggplot(data = df_15min,
       aes(x = q_m3s,
           y = h_cm,
           color = siteq)) +
  geom_point() +
  scale_x_log10() + scale_y_log10() +
  facet_wrap(~siteq, scales = "free") +
  stat_smooth(method = "lm") +
  ggpubr::stat_regline_equation()

# save data
saveRDS(df_15min, "Data/04_discharge and stage/all_discharge_data_15min.RDS")

# Save as xlsx with sheets
# Split one data frame per site
df_15min %>%
  group_split(siteq) -> list_of_dfs
# name of each sheet will be the site
list_of_dfs %>%
  map(~pull(., siteq)) %>%
  map(~unique(.)) -> names(list_of_dfs)

list_of_dfs %>%
  writexl::write_xlsx(path = "Data/04_discharge and stage/all_discharge_data_15min.xlsx")
