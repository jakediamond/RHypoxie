library(tidyverse)

df_l <- readRDS(file.path("data", "reachlengths.RDS"))

# Only hypoxia
df_hypl <- filter(df_l, DO < 3)

# Estimate lengths for hypoxia
df_lh <- df_hypl %>%
  group_by(watershed, site, date, area_km2) %>%
  mutate(dens = case_when(watershed == "Coise" ~ 1.28,
                          watershed == "Loise" ~ 3.57,
                          watershed == "Toranche" ~ 2.26,
                          watershed == "Lignon" ~ 2.11,
                          watershed == "Mare" ~ 2.58)) %>%
  summarize(l = mean(reach_length, na.rm = T),
            totl = dens * area_km2 * 1000,
            per = l / totl)

df_w <- df_lh %>%
  group_by(watershed) %>%
  summarize(avg = mean(per, na.rm = T))

a = ggplot(data = df_hypl,
       aes(x = datetime,
           y = DO,
           color = log(area_km2))) +
  geom_line() +
  scale_color_viridis_c() +theme_bw() +
  facet_wrap(~watershed)
plotly::ggplotly(a)
e
library(tmap)
library(lubridate)
library(sf)
x = select(df_l, watershed, site, date, datetime, area_km2, 
           DO, lat = latitude, long = longitude) %>%
  mutate(hyp = if_else(DO <3, 1, 0)) %>%
  group_by(watershed, site, date, area_km2, lat, long) %>%
  summarize(h = if_else(sum(hyp==1, na.rm = T) > 1, 1, 0))

x2 = dplyr::filter(x, watershed == "Loise") %>%
  filter(date > ymd(20200715)) %>%
  filter(date < ymd(20200802))

xsf = st_as_sf(x2, coords = c("long", "lat"))

tmap_mode("view")
tm_shape(xsf) +
  tm_bubbles(col = "h", palette = "RdYlBu") +
  tm_facets(by = "date")

y = filter(x2, date == ymd(20200720))
