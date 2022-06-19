# 
# Purpose: To evaluate how sampling length and number of sites affects incidence of hypoxia
# Author: Jake Diamond
# Date: 17 June 2022
# 

# Load libraries
library(lubridate)
library(readxl)
library(scales)
library(tidytext)
library(tidyverse)
set.seed(42)

# Load data
df <- readRDS(file.path("data", "10_clean_data", "hourly_data_all.RDS"))

# Add info for light, time, and hypoxia (defined as less than 3 mg/L)
df <- df %>%
  mutate(DOhyp = if_else(DO < 3, 1, 0)) #define hypoxia as less than 3 mg/Lsaturation (Carter et al. 2021 uses 50%)

# Function to sample data based on length, l
sample.consecutive <- function(data, l) {
  runs <- rle(data$site)$lengths
  start <- ifelse(runs > l, sapply(pmax(runs-l, 1), sample.int, size=1), 1)
  end <- ifelse(runs >= l, l, runs)
  previous <- cumsum(c(0, head(runs, -1)))
  as.data.frame(data)[unlist(mapply(seq, previous + start, previous + start + end - 1), length),]
}

# function to grab data
sub.dat <- function(data, sites, times){
  # Get subset of sites
  subdat = filter(data, site %in% sample(unique(site), sites)) %>%
    arrange(site, datetime)
  # Get length of data in hours
  l = lubridate::time_length(times) / 3600
  # Get consecutive data of that length randomly
  dat = sample.consecutive(subdat, l)
}

# Increasingly subsample data in space and time
sample_frame = crossing(sites = c(1, 2, 5, 10, 20, 50, 78), 
                        times = c("1 hour", "1 day", "2 days", "1 week", 
                                  "2 weeks", "1 month", "2 months", "5 months"))

# Get just the hypoxia info to reduce data
dfhyp <- select(df, site, datetime, DOhyp)

# general function to get hypoxia lengths per sample
hyp.samp.fun <- function(sampleframe, basedata){
  data_samp = sampleframe %>%
    mutate(data = map2(sites, times, ~sub.dat(basedata, .x, .y)))
  
  hyp_lengths = data_samp %>%
    mutate(hypl = map(data, ~sum(.$DOhyp, na.rm = T))) %>%
    select(-data) %>%
    unnest(cols = hypl) %>%
    mutate(perhyp = hypl / ((lubridate::time_length(times) / 3600) * sites))
} 

# repeat this 100 times
hypl_boot <- map_dfr(1:100, ~ hyp.samp.fun(sample_frame, dfhyp))

# Save this
saveRDS(hypl_boot, "results/bootstrap_sampling_hypoxia.RDS")


# Plot
group_by(hypl_boot, sites, times) %>%
  summarize(per = mean(perhyp / sites),
            sd = sd(perhyp)) %>%
  ggplot(aes(x = sites, y = times,
             color = per * 100,
             size = sd * 100)) +
  geom_point() +
  scale_color_viridis_c()