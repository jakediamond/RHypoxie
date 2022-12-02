# 
# Purpose: To plot confluence biofilm data for RHypoxie sites
# Author: Jake Diamond
# Date: November 19, 2021
# 

# Load libraries
library(lubridate)
library(readxl)
library(scales)
library(tidyverse)

# Read in watershed info
conf_meta <- read_excel(file.path("data", "01_metadata", "sensor_metadata.xlsx"), 
                        sheet = "site_metadata") %>%
  mutate(pos_num = position,
         position = word(position, 1,1, "_")) %>%
  select(site, confluence, pos_num, position, watershed)

# Load data
df <- read_xlsx(file.path("data", "06_EMA data", 
                          "2021_Rhypoxie_SyntheseResultatsMicrobio_EMA.xlsx"), 
                sheet = 2, na = c("", "ND")) %>%
  mutate(sampledate = ymd(SamplingDate),
         measuredate = ymd(MeasureDate)) %>%
  left_join(conf_meta) %>%
  mutate(pos_num = fct_relevel(pos_num, "up_1", "up_2", "down")) %>%
  left_join(read_xlsx(file.path("data", "06_EMA data", 
                                "2021_Rhypoxie_SyntheseResultatsMicrobio_EMA.xlsx"), 
                      sheet = 3))
  
# Look at distributions
df %>%
  select()


# plot
ggplot(data = filter(df, is.na(Comments)),
       aes(x = pos_num,
           y = Value)) +
  stat_summary() +
  facet_wrap(~Parameter, scales = "free_y")

# plot
ggplot(data = filter(df, is.na(Comments)),
       aes(x = pos_num,
           y = Value,
           color = factor(sampledate),
           group = sampledate)) +
  stat_summary() +
  scale_y_log10() +
  # facet_wrap(~Parameter, scales = "free_y")
  facet_grid(Parameter~confluence, scales = "free_y")

