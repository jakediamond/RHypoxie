# 
# Purpose: To summarize DO data for Loire headwaters
# Author: Jake Diamond
# Date: October 3, 2019
# 

# Set working directory
# setwd("Z:/Loire_DO")
# setwd("C:/Users/jake.diamond/Documents/Backup of Network/Loire_DO")
# setwd("C:/Users/diamo/Dropbox/Projects/Loire_headwaters")
setwd("C:/Users/jake.diamond/Dropbox/Projects/Loire_headwaters")
# Load libraries
library(lubridate)
library(tidytext)
library(readxl)
library(plotly)
library(patchwork)
library(tidyverse)

# Load data and calculate percent saturation
df <- readRDS("Headwaters/Data/headwaters_data_clean") %>%
  rename(site = Site,
         subwatershed = Subwatershed) %>%
  mutate(site = tolower(site),
         date = date(datetime))

# Discharge data
dfq <- readRDS("Headwaters/Data/Discharge/discharge_plus_v2.RDS")

# Join data
df <- left_join(df, dfq, by = c("site", "date", "subwatershed"))

# Read in watershed info
conf_meta <- read_excel("Headwaters/Data/confluence_metadata.xlsx") %>%
  mutate(site = tolower(Site))

# Need to remove a few points, still not completely clean
df_conf <- df %>%
  filter(site_code == "fon003" & datetime == "2020-04-13 15:00:00") %>%
  mutate_if(is.numeric, ~NA_real_) %>%
  bind_rows(df %>%
              filter(!(site_code == "fon003" & datetime == "2020-04-13 15:00:00"))) %>%
  # filter(!(site_code == "car004" & date >= ymd("2020-03-28"))) %>%
  arrange(subwatershed, area_km2, datetime) %>%
  ungroup() %>%
  group_by(subwatershed) %>%
  left_join(conf_meta) %>%
  mutate(DOd = DO * h)

# Interactive plotly graphs
# First need to get nest data
df_ply <- df_conf %>%
  dplyr::filter(!is.na(conf)) %>%
  ungroup() %>%
  mutate(alpha = 0.0192+8E-5*DO_temp,
         spc = cond / (1 + alpha * (DO_temp - 25))) %>%
  select(datetime, conf, position, pos_num, site, DO, DOd, DO_temp, q_mmd,
         DO_per, cond, lux, spc) %>%
  arrange(conf, position) %>%
  group_by(conf) %>%
  nest()

# Create a graphing function
graph_fun <- function(data, y1 = "DO", confluence = "conf") {
  
  p_y1 <- parse(text = y1)
  # p_y2 <- parse(text = y2)
  
  # Have to put NAs so that it doesn't connect gapped observations
  data = arrange(data, datetime) %>%
    select(datetime, {{ y1 }}, position, pos_num)#, {{ y2 }})#, oow)
  # data_oow = distinct(data, datetime) %>%
  #   left_join(filter(data, oow == "yes"))
  # data_inw = distinct(data, datetime) %>%
  #   left_join(filter(data, oow == "no"))
  
  # # Subset the points dataframe for the site
  # pts_site = filter(pts, site_code == site) %>%
  #   select(Datetime,{{ y1 }})
  
  # yaxis titles
  yaxis_1 = switch(y1,
                   "DO" = "DO (mg/L)",
                   "DO_per" = "DO (% sat.)",
                   "DO_temp" = "temp (deg C)",
                   "cond" = "conductivity (uS/cm)",
                   "spc" = "specific conductance (uS/cm)",
                   "DOd" = "DO x depth (g/m2)")
  # yaxis_2 = if_else(y2 == "DO_temp", "temp. (deg C)",
  #                   if_else(y2 == "DO",
  #                           "DO (mg/L)",
  #                           "lux"))
  
  # yaxis ranges
  rangep = 
    switch(y1,
           "DO" = c(0,15),
           "DO_per" = c(0,150),
           "DO_temp" = c(0, 25),
           "cond" = c(0,1500),
           "spc" = c(0, 1500),
           "DOd" = c(0,3))
  
  plot_ly() %>%
    add_trace(data = data,#data_inw,
              x=~datetime, y=~eval(p_y1), type = "scatter", mode='lines',
              color = ~pos_num, linetype = ~position,
              colors = c("purple", "blue", "red"),
              showlegend = TRUE) %>%
    # add_trace(data = data_oow,
    #           x=~datetime, y=~eval(p_y1), type = "scatter", mode='lines',
    #           color = I("darkgrey"), linetype = I("dot"),
    #           showlegend = FALSE) %>%
    # add_trace(data = data,#data_inw,
    #           x=~datetime, y=~eval(p_y2), type = "scatter", mode='lines',
    #           color = I("red"), linetype = I("solid"),
    #           yaxis="y2",
    #           showlegend = FALSE) %>%
    # add_trace(data = data_oow,
    #           x=~datetime, y=~eval(p_y2), type = "scatter", mode='lines',
    #           color = I("tomato"), linetype = I("dot"),
    #           yaxis="y2",
    #           showlegend = FALSE) %>%
    # add_trace(data = pts_site,
    #           color = I("black"),
    #           x=~Datetime, y=~eval(p_y1), type = "scatter", mode = 'markers',
    #           showlegend = FALSE) %>%
    # add_trace(data = pts_site,
    #           x=~Datetime, y=~eval(p_y2), type = "scatter", mode = 'markers',
  #           color = I("red"),
  #           yaxis = 'y2',
  #           showlegend = FALSE) %>%
  # add_segments(data = pts_site,
  #              x=~Datetime, xend =~Datetime,
  #              y = 0, yend = 15, type = 'scatter', mode = 'lines',
  #              linetype = I("solid"),
  #              color = I("black"), name = 'site visit') %>%
  # layout(yaxis2 = list(overlaying = "y", side = "right",
  #                      title = yaxis_2,
  #                      tickfont = list(color = "red"),
  #                      titlefont = list(color = "red")),
  #        xaxis = list(title = ""),
  #        yaxis = list(title = yaxis_1, range = rangep),
  #        title = site)
  layout(xaxis = list(title = ""),
         yaxis = list(title = yaxis_1, range = rangep),
         title = confluence)
}

# Apply graph function to data
df_ply <- df_ply %>%
  mutate(p_do = pmap(list(data, confluence = conf),
                          graph_fun),
         p_doper = pmap(list(data, "DO_per", conf),
                            graph_fun),
         p_temp = pmap(list(data, "DO_temp", conf),
                       graph_fun),
         p_cond = pmap(list(data, "cond", conf),
                           graph_fun),
         p_spc = pmap(list(data, "spc", conf),
                       graph_fun),
         p_dod = pmap(list(data, "DOd", conf),
                       graph_fun))
# Check out the graphs
htmltools::browsable(htmltools::tagList(pluck(df_ply, 3)))
htmltools::browsable(htmltools::tagList(pluck(df_ply, 4)))
htmltools::browsable(htmltools::tagList(pluck(df_ply, 5)))
htmltools::browsable(htmltools::tagList(pluck(df_ply, 6)))
htmltools::browsable(htmltools::tagList(pluck(df_ply, 7)))
