# 
# Purpose: To plot confluence data for RHypoxie sites
# Author: Jake Diamond
# Date: July 1, 2021
# 

# Set working directory
setwd("Z:/RHypoxie")

# Load libraries
library(htmltools)
library(plotly)
library(lubridate)
library(readxl)
library(scales)
library(tidyverse)

# Load data
df <- readRDS("Data/timeseries_data_hourly_new.RDS")

# Read in watershed info
conf_meta <- read_excel("Data/DO/sensor_metadata.xlsx", sheet = "site_metadata") %>%
  mutate(pos_num = position,
         position = word(position, 1,1, "_")) %>%
  select(site, pos_num, position, confluence) %>%
  distinct()

# Join to metadata
df_conf <- select(df, -position) %>%
  left_join(conf_meta)

# Interactive plotly graphs
# First need to get nest data
df_ply <- df_conf %>%
  ungroup() %>%
  arrange(confluence, position, datetime) %>%
  group_by(confluence) %>%
  nest()

# Create a graphing function
graph_fun <- function(data, y1 = "DO", confluence = "confluence") {
  
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
  mutate(p_do = pmap(list(data, confluence = confluence),
                     graph_fun),
         p_doper = pmap(list(data, "DO_per", confluence),
                        graph_fun),
         p_cond = pmap(list(data, "cond", confluence),
                       graph_fun),
         p_spc = pmap(list(data, "spc", confluence),
         graph_fun))

# Check out the graphs
htmltools::browsable(htmltools::tagList(pluck(df_ply, 3)))
htmltools::browsable(htmltools::tagList(pluck(df_ply, 4)))
htmltools::browsable(htmltools::tagList(pluck(df_ply, 5)))
htmltools::browsable(htmltools::tagList(pluck(df_ply, 6)))

df_conf2 <- df_conf %>%
  group_by(confluence) %>%
  select(datetime, confluence, pos_num, DO, DO_per, DO_temp, cond_temp, cond, spc) %>%
  pivot_wider(values_from = c(DO, DO_per, DO_temp, cond_temp, cond, spc), 
                        names_from = pos_num) %>%
  mutate(rc = (cond_down - cond_up_2) / (cond_up_1 - cond_down),
         rct = (cond_temp_down - cond_temp_up_2) / (cond_temp_up_1 - cond_temp_down),
         rspc = (spc_down - spc_up_2) / (spc_up_1 - spc_down),
         rdt = (DO_temp_down - DO_temp_up_2) / (DO_temp_up_1 - DO_temp_down),
         DO_pred = (rc*DO_up_1 + DO_up_2) / (1 + rc),
         DO_diff = (DO_down - DO_pred) / DO_pred * 100
         )

