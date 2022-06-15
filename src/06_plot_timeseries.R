# 
# Purpose: To do a first look at all the headwaters data
# Author: Jake Diamond
# Date: November 1, 2020
# 

# Set working directory
setwd("Z:/RHypoxie")
# setwd("C:/Users/jake.diamond/Dropbox/Projects/RHypoxie")


# Load libraries
library(htmltools)
library(plotly)
library(lubridate)
library(readxl)
library(scales)
library(tidyverse)

# Load data
df <- readRDS("Data/02_sensor_data/all_sensor_data_clean_wide.RDS")
df_hour <- readRDS("Data/hourly_data.RDS")
# Load some metadata
meta <- read_xlsx("Data/01_metadata/site_area.xlsx")

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
# Read in all meteorological data
meteo <- readRDS("Data/05_meteo/meteo_data.RDS")

# Read in discharge data
df_q <- readRDS("Data/04_discharge and stage/all_discharge_data_15min.RDS")

# Load elevation data
elev <- read_xlsx("Data/01_metadata/site_and_watershed_info.xlsx") %>%
  select(site, elev_site = altitude_m)

# Calculate %DO saturation and specific conductivity
df <- df %>%
  left_join(meteo) %>%
  left_join(elev) %>%
  mutate(tair_site = tair - 0.0098 * (elev_site - elev_ref)) %>% #air temp. at site; adiabatic dry lapse rate
  mutate(press_e = p_mbar * exp((-0.03416262 / (tair_site + 273)) * 
                                  (elev_site - elev_ref))) %>%  #pressure at site; -0.034 = -g*M/R
  mutate(DOsat = streamMetabolizer::calc_DO_sat(DO_temp, press_e), #theoretical DO saturation based on garcia-benson eqn
         DOsat2 = if_else(DO_temp == 0,
                          0,
                          14.652 - 0.41022 * DO_temp + 0.007991 *
                            DO_temp^2 - 0.000077774 * DO_temp^3), #if pressure data is missing, estimate with this equation
         DO_per = if_else(is.na(DOsat), # percentage saturation
                          DO * 100 / DOsat2,
                          DO * 100/ DOsat),
         alpha = 0.0192+8E-5*cond_temp, # alpha constant for spc based on logger specs
         spc = cond / (1 + alpha * (cond_temp - 25))) # calculate spc based on logger specs

# Average data by hour 
df_hour <- df %>%
  mutate(hour = floor_date(datetime, "hour")) %>%
  group_by(watershed, confluence, position, site, hour) %>%
  summarize(across(where(is.numeric) & !rain_mm, mean, na.rm = TRUE),
            rain_mm = sum(rain_mm, na.rm = T)) %>%
  rename(datetime = hour) %>%
  ungroup()

# Plot top to bottom summary
df_p <- left_join(df_hour, meta) %>%
  mutate(date = date(datetime)) %>%
  group_by(watershed, number, site, confluence, date) %>%
  summarize(maxDO = max(DO, na.rm = T),
            minDO = min(DO, na.rm = T),
            maxper = max(DO_per, na.rm = T),
            minper = min(DO_per, na.rm = T)) %>%
  pivot_longer(maxDO:minper) %>%
  mutate(value = na_if(value, Inf),
         value = na_if(value, -Inf))

ggplot(data = filter(df_p, name %in% c("maxDO", "minDO"), watershed == "yzeron"),
       aes(x = date,
           y = value,
           color = name)) +
  geom_line() +
  theme_bw() +
  scale_x_date(date_breaks = "1 month", date_labels = "%m") +
  labs(y = "DO (mg/L)", x = "", title = "Yzeron") +
  facet_wrap(~fct_reorder(site, number), ncol = 3) +
  theme(legend.position = "none", axis.title.x = element_blank())
ggsave("Figures/yzeron_timeseries_max_min_DO.png",
       dpi = 600,
       width = 18.2,
       height = 24,
       units = "cm")

# All three confluence sites on the same panel
ggplot(data = filter(df_hour, watershed == "ardieres"),
       aes(x = datetime,
           y = DO,
           color = position)) +
  geom_line(size = 0.6, alpha = 0.5) +
  facet_wrap(~fct_reorder(confluence, -altitude_m), ncol = 1) +
  scale_color_manual(values = c("purple", "red", "blue")) +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%m-%d", expand = c(0,0)) +
  labs(y = "DO (mg/L)", x = "", title = "Ardières")
ggsave("Figures/watershed_timeseries/ardieres_timeseries_confluence_DO.png",
       dpi = 600,
       width = 18.2,
       height = 24,
       units = "cm")

# # Use threshold of daily temperature and %sat to see when out of water
# df_oow <- df_hour %>%
#   mutate(date = date(datetime)) %>%
#   group_by(site,date) %>%
#   summarize(do_mean = quantile(DO_per, 0.1, na.rm = TRUE),
#             do_amp = max(DO_per, na.rm = T) - min(DO_per, na.rm = T),
#             wat_t = max(DO_temp, na.rm = T) - min(DO_temp, na.rm = T)) %>%
#   mutate(oow = if_else((do_mean > 90 & wat_t > 8) |
#                          (do_mean > 90 & do_amp < 5), 
#                        "yes", "no"))
# 
# # join this to dataframe
# df_hour <- df_hour %>%
#   mutate(date = date(datetime)) %>%
#   left_join(select(df_oow, -q_mmd), 
#             by = c("Site", "date"))

# Interactive plotly graphs
# First need to get nest data
df_ply <- df_hour %>%
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
# Save the data
savedata <- select(df_ply, site, data)
saveRDS(df_hour, file = "Data/timeseries_data_hourly_new.RDS")
