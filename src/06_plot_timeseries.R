# 
# Purpose: To do a first look at all the headwaters data
# Author: Jake Diamond
# Date: November 1, 2020
# 

# Load libraries
library(htmltools)
library(plotly)
library(lubridate)
library(readxl)
library(scales)
library(tidyverse)

# Load data
df <- readRDS(file.path("data", "10_clean_data", "hourly_data_all.RDS"))

# # Load point measurements
# pts <- read_excel("Headwaters/Data/Field measurements/Field_data.xlsx") %>%
#   rename(DO_temp = `T (°C)`, DO = `DO (mg/L)`, DO_per = `DO (%)`, cond = `SC (us/cm)`) %>%
#   distinct() %>%
#   left_join(distinct(select(df, Site, site_code))) %>%
#   mutate(site_code = if_else(site_code == "Le 009",
#                              "Rie009",
#                              site_code)) %>%
#   mutate(site_code = if_else(site_code == "Le 025",
#                              "Pot025",
#                              site_code))
# 

# Interactive plotly graphs
# First need to get nest data
df_ply <- df %>%
  # select(datetime, watershed, confluence, position, site, DO, DO_temp, 
  #        DO_per, cond, spc, lux_water, rain_mm, ) %>%
  arrange(watershed, confluence, position) %>%
  select(-confluence, -position) %>%
  group_by(watershed, site) %>%
  nest()

# Create a graphing function
graph_fun <- function(data, y1 = "DO", y2 = "DO_temp", site = "sitename") {
  
  p_y1 <- parse(text = y1)
  p_y2 <- parse(text = y2)
  
  # Have to put NAs so that it doesn't connect gapped observations
  data = arrange(data, datetime) %>%
    select(datetime, {{ y1 }}, {{ y2 }})#, oow)
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
                   "lux_water" = "lux (lumens/m2)",
                   "q_mmh" = "specific discharge (mm/h)",
                   "qsite_m3s" = "Q (m3/s)",
                   "rain_mm" = "rainfall (mm/hr)",
                   "rad_Wm2" = "insolation (W/m2)")
  
  yaxis_2 = switch(y2,
                   "DO_temp" = "temp. (deg C)",
                   "DO_per" = "DO (% sat.)",
                   "cond" = "conductivity (uS/cm)",
                   "spc" = "specific conductance (uS/cm)",
                   "DOd" = "DO x depth (g/m2)",
                   "DO" = "DO (mg/L)",
                   "lux_water" = "lux (lumens/m2)",
                   "q_mmh" = "specific discharge (mm/h)",
                   "qsite_m3s" = "Q (m3/s)",
                   "rain_mm" = "rainfall (mm/hr)",
                   "rad_Wm2" = "insolation (W/m2)")

  # yaxis ranges
  rangep = 
    switch(y1,
           "DO" = c(0,15),
           "DO_per" = c(0,150),
           "cond" = c(0,1500),
           "spc" = c(0, 1500),
           "lux_water" = c(0, 1500),
           "q_mmh" = c(0, 5),
           "qsite_m3s" = c(0, 50),
           "rain_mm" = c(0, 5),
           "rad_Wm2" = c(0, 1500)
           )
  
  plot_ly() %>%
    add_trace(data = data,#data_inw,
              x=~datetime, y=~eval(p_y1), type = "scatter", mode='lines',
              color = I("black"), linetype = I("solid"),
              showlegend = FALSE) %>%
    # add_trace(data = data_oow,
    #           x=~datetime, y=~eval(p_y1), type = "scatter", mode='lines',
    #           color = I("darkgrey"), linetype = I("dot"),
    #           showlegend = FALSE) %>%
    add_trace(data = data,#data_inw,
              x=~datetime, y=~eval(p_y2), type = "scatter", 
              mode=if(y2=="rain_mm"){'bars'}else{'lines'},
              color = I("red"), linetype = I("solid"),
              yaxis="y2",
              showlegend = FALSE) %>%
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
    layout(yaxis2 = list(overlaying = "y", side = "right",
                         title = yaxis_2,
                         tickfont = list(color = "red"),
                         titlefont = list(color = "red")),
           xaxis = list(title = ""),
           yaxis = list(title = yaxis_1, range = rangep),
           title = site)
}

# Apply graph function to data
df_ply <- df_ply %>%
  mutate(p_do_temp = pmap(list(data, site = site),
                          graph_fun),
         p_doper_lux = pmap(list(data, "DO_per", "lux_water", site),
                            graph_fun),
         p_spc_dis = pmap(list(data, "spc", "q_m3s", site),
                           graph_fun),
         p_dis_rain = pmap(list(data, "q_mmh", "rain_mm", site),
                           graph_fun),
         p_cond_dis = pmap(list(data, "DO", "q_mmh", site),
                           graph_fun))
# Check out the graphs
htmltools::browsable(htmltools::tagList(pluck(df_ply, 4)))
htmltools::browsable(htmltools::tagList(pluck(df_ply, 5)))
htmltools::browsable(htmltools::tagList(pluck(df_ply, 6)))
htmltools::browsable(htmltools::tagList(pluck(df_ply, 7)))
htmltools::browsable(htmltools::tagList(pluck(df_ply, 8)))


df_q <- distinct(df_hour, datetime, rain_mm, Q_grez, Q_rat, Q_char) %>%
  pivot_longer(cols = starts_with("Q")) %>%
  arrange(datetime)
p_q <- plot_ly() %>%
  add_trace(data = df_q,
            x=~datetime, y=~value, type = "scatter", mode='lines',
            color = ~name, linetype = I("solid"),
            showlegend = FALSE) %>%
  add_trace(data = df_q,
            x=~datetime, y=~rain_mm, type = "scatter", 
            mode = "lines",
            color = I("black"), linetype = I("solid"),
            yaxis="y2",
            showlegend = FALSE) %>%
  layout(yaxis2 = list(overlaying = "y", side = "right",
                     title = "rainfall (mm/h)",
                     tickfont = list(color = "black"),
                     titlefont = list(color = "black"),
                     autorange = "reversed"),
       xaxis = list(title = ""),
       yaxis = list(title = "Q (m3/s)"))
p_q

