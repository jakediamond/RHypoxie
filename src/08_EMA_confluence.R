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
  select(site, confluence, pos_num, position, watershed) %>%
  distinct()

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
  
# Look at CVs for replicates
df %>%
  mutate(date = as.factor(month(measuredate, TRUE, TRUE))) %>%
  drop_na(date) %>%
  group_by(confluence, pos_num, Parameter, date) %>%
  summarize(CV = sd(Value, na.rm = T) / mean(Value, na.rm = T)) %>%
  ggplot(aes(x = date,
             y = CV,
             fill = as.factor(pos_num))) +
  geom_jitter(size = 3, shape = 21) +
  facet_grid(confluence~Parameter) +
  theme_bw(base_size = 16) +
  geom_hline(yintercept = 1, linetype = "dashed") + 
  scale_fill_brewer(type = "qual", name= "conf. position") +
  labs(y = "coefficient of variation of 3 replicates",
       x = "measure date") +
  theme(legend.position = "bottom", legend.direction = "horizontal")

ggsave(filename = file.path("results", "confluences", "CV_replicates.png"),
       dpi = 600,
       height = 19,
       width = 32,
       units = "cm")

# plot
df %>%
  mutate(date = as.factor(month(measuredate, TRUE, TRUE))) %>%
  drop_na(date) %>%
  group_by(confluence, pos_num, Parameter, date) %>%
  mutate(CV = sd(Value, na.rm = T) / mean(Value, na.rm = T)) %>%
  filter(CV < 0.5) %>%
  ggplot(aes(x = pos_num,
             y = Value)) +
  geom_boxplot() +
  theme_bw(base_size = 16) +
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

