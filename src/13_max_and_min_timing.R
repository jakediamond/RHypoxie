# 
# Purpose: To determine timing of min and max of daily DO, how this changes
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
  mutate(date = date(datetime),
         month = month(datetime),
         solartime = streamMetabolizer::calc_solar_time(datetime, longitude),
         light = streamMetabolizer::calc_light(solartime, latitude, longitude),
         DOhyp = if_else(DO < 3, 1, 0)) #define hypoxia as less than 3 mg/Lsaturation (Carter et al. 2021 uses 50%)

# Timing of max and min % DO (accounts for T and press)
df_timing <- df %>%
  mutate(solardate = date(solartime - hours(4))) %>%
  filter(oow != "yes" | is.na(oow)) %>%
  group_by(site, solardate) %>%
  filter(DO_per == max(DO_per, na.rm = T) | DO_per == min(DO_per, na.rm = T)) %>%
  mutate(type = if_else(DO_per == max(DO_per), "max", "min"),
         timing = hour(solartime),
         aspect = case_when(watershed %in% c("Coise", "Toranche", "Loise") ~ "west",
                            TRUE ~ "east"))

# Plot
ggplot(data = df_timing,
       aes(x = log(area_km2),
           y = timing,
           color = aspect)) +
  stat_summary() +
  theme_bw() +
  stat_smooth(method = "lm") +
  facet_grid(cols = vars(type), scales = "free", space = "free")
  # geom_point() +
  # facet_grid(cols = vars(year(date)), scales = "free", space = "free")


# summarize
df_summ <- ungroup(df_timing) %>%
  mutate(bin = cut(log(area_km2), 10)) %>%
  group_by(aspect, bin, type, timing) %>%
  summarize(n = n())


coord_radar <- function (theta = "x", start = 0, direction = 1) {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") "y" else "x"
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start, 
          direction = sign(direction),
          is_linear = function(coord) TRUE)
}

ggplot(filter(df_summ, type == "max"), aes(x = timing, y = n, color = aspect, fill = aspect)) + 
  geom_polygon(alpha = .70) +
  # geom_point(color = "#99d5cf", size = 1.3, alpha = .8) +
  scale_x_continuous(breaks = seq(0, 24, by = 1)) +
  coord_radar() +
  theme_minimal() +
  # theme(
  #   plot.background = element_rect(fill = "#22292F"),
  #   plot.margin = unit(rep(2, 4), "cm"),
  #   axis.text.x = element_text(size = 8, color = "#c6ced6"),
  #   axis.title = element_blank(),
  #   axis.text.y = element_blank(),
  #   axis.ticks = element_blank(),
  #   panel.grid = element_line(color = "#364049", linetype = "dashed"),
  #   panel.grid.minor = element_blank(),
  #   panel.background = element_rect(fill = "#1e242a", color = "#22292F"),
  #   panel.spacing = unit(3, "lines"),
  #   strip.text = element_text(size = 14, color = "#F1F5F8", 
  #                             face = "bold", margin = margin(b = 15))
  # ) +
  facet_wrap(bin ~ type) 
