# 
# Purpose: To try to predict hypoxia metrics with land use/GIS daa
# Author: Jake Diamond
# Date: 18 January 2022
# 
library(car)
library(leaps)
library(lubridate)
library(tidyverse)

# Load hypoxia summary data -----------------------------------------------
df_hyp <- readRDS(file.path("results", "hypoxia_summary.RDS"))


# Load predictor data -----------------------------------------------------
# Load sensor data to get temperature summary, depth, discharge, make it only summer
df <- readRDS(file.path("data", "estimated_depths.RDS")) %>%
  group_by(site_code, site) %>%
  filter(between(month, 7 , 10)) %>%
  dplyr::summarize(temp = median(temp, na.rm = T),
                   depth = median(depth, na.rm = T),
                   q = median(q_mmd, na.rm = T),
                   altitude = mean(altitude_m))

df_q <- readRDS(file.path("data", "09_loire_data","discharge_plus_v2.RDS")) %>%
  group_by(site) %>%
  summarize(across(where(is.numeric), mean, na.rm = T))

# Load thermal regime data
df_therm <- read_tsv(file.path("data", "07_geomorphology", 
                                  "rhypoxie_RMC_join_regimes.txt"),
                     locale = locale(decimal_mark = ","))

# # Load watershed land use data
# ws <- readRDS(file.path("data", "08_GIS", "ws_land_use")) %>%
#   mutate(site = tolower(site),
#          area_lu_frac = units::drop_units(area_lu_frac)) %>%
#   rename(ws_area_frac = area_lu_frac)
# 
# # Load buffer land use data
# buf <- readRDS(file.path("data", "08_GIS", "buffer_land_use_v2")) %>%
#   mutate(area_lu_frac = units::drop_units(area_lu_frac)) %>%
#   rename(buff_area_frac = area_lu_frac)
# 
# # mod format for buffers, only look at 50m at first
# buf_mod <- filter(buf, buff_dists == 20) %>%
#   ungroup() %>%
#   select(site, plot_name, buff_area_frac, buff_dists) %>%
#   pivot_wider(names_from = plot_name, values_from = buff_area_frac)

# Load hydraulic data
# Load geometry data
df_k <- readxl::read_xlsx(file.path("data", "04_discharge and stage", "hydraulic_data.xlsx")) %>%
  select(Site = site, width = width_m, depth = depth_m, slope = slope_tnet) %>%
  mutate(site = tolower(Site)) %>%
  select(-Site)


# df <- readRDS(file.path("data", "09_loire_data","loire_headwaters_data.RDS")) %>%
#   unnest() %>%
#   ungroup() %>%
#   mutate(site = tolower(Site))



df_hl <- df_h %>%
  group_by(site) %>%
  arrange(site, datetime) %>%
  mutate(hyp_l = sequence(rle(DOhyp)$lengths),
         hyp_change = if_else(lag(DOhyp) != DOhyp, 1, 0)) %>%
  filter(DOhyp == 1) %>%
  mutate(hyp_pd = cumsum(hyp_change)) %>%
  group_by(site, hyp_pd) %>%
  filter(hyp_l == max(hyp_l)) %>%
  ungroup() %>%
  group_by(site) %>%
  dplyr::summarize(hypl = mean(hyp_l, na.rm = T)) %>%
                   # slope = mean(slope, na.rm = T),
                   # alt = mean(altitude_m, na.rm = T),
                   # area = mean(area_km2, na.rm = T),
                   # L50corr = mean(L50_corr, na.rm = T),
                   # H50corr = mean(H50_corr, na.rm = T),
                   # nw_b = mean(nw_b, na.rm = T),
                   # nw_f = mean(nw_f, na.rm = T),
                   # Q50corr = mean(Q50_corr, na.rm = T),
                   # strahler = mean(strahler, na.rm = T),
                   # # slope = mean(slope, na.rm = T),
                   # # SF = mean(SF, na.rm = T),
                   # h = mean(h ,na.rm = T),
                   # w = mean(w, na.rm = T),
                   # v = mean(v, na.rm = T)) %>%
  left_join(buf_mod) %>%
  left_join(df_tq) %>%
  left_join(df_q)

# Calculate daily stats by site
df_hyp_mod <- df_h %>%
  group_by(site) %>%
  dplyr::summarize(DOhypn = round(sum(DOhyp, na.rm = T) / n() *100, 2)) %>%#percentage of time that is hypoxic
            # slope = mean(slope, na.rm = T),
            # alt = mean(altitude_m, na.rm = T),
            # area = mean(area_km2, na.rm = T),
            # L50corr = mean(L50_corr, na.rm = T),
            # H50corr = mean(H50_corr, na.rm = T),
            # nw_b = mean(nw_b, na.rm = T),
            # nw_f = mean(nw_f, na.rm = T),
            # Q50corr = mean(Q50_corr, na.rm = T),
            # strahler = mean(strahler, na.rm = T)) %>%
            # slope = mean(slope, na.rm = T),
            # SF = mean(SF, na.rm = T),
            # h = mean(h ,na.rm = T),
            # w = mean(w, na.rm = T),
            # v = mean(v, na.rm = T)) %>%
  left_join(buf_mod) %>%
    left_join(df_tq) %>%
    left_join(df_q)
# 
# # Summary of that data
# df_daily_sum <- df_daily %>%
#   pivot_longer(cols = -c(site, date)) %>%
#   group_by(site, name) %>%
#   filter(!is.infinite(value)) %>%
#   summarize(mean = mean(value, na.rm = T),
#             sd = sd(value, na.rm = T)) %>%
#   ungroup()

# Best subsets

df_mod <- filter(df_hyp_mod, site != "coise aval vaudragon") %>%
  select(-site, -site_code) %>%
  # select(-site, -h, -w) %>%
  replace(is.na(.), 0) %>%
  mutate(area = log(area_km2))

pivot_longer(df_mod,  cols = everything()) %>%
  ggplot(aes(x = value)) + geom_histogram() + facet_wrap(.~name, scales = "free")

models <- regsubsets(DOhypn ~ ., data = df_mod, nvmax = 5)
summary(models)
res.sum <- summary(models)
data.frame(
  Adj.R2 = which.max(res.sum$adjr2),
  CP = which.min(res.sum$cp),
  BIC = which.min(res.sum$bic)
)
pairs(df_mod)



df_mod2 <- filter(df_hl, site != "coise aval vaudragon") %>%
  select(-site, -site_code, -h, -w) %>%
  replace(is.na(.), 0) %>%
  mutate(area = log(area_km2))


models2 <- regsubsets(hypl ~ ., data = df_mod2, nvmax = 5)
summary(models2)
res.sum2 <- summary(models2)
data.frame(
  Adj.R2 = which.max(res.sum2$adjr2),
  CP = which.min(res.sum2$cp),
  BIC = which.min(res.sum2$bic)
)
df_mod2$Q50_corr
mod2 <- lm(hypl ~ area + `irrigated agriculture` + urban + slope, data = df_mod2)
#produce added variable plots
avPlots(mod2)
summary(mod2)

pairs(df_mod2)



# Set up a 2x2 grid so we can look at 4 plots at once
par(mfrow = c(2,2))
plot(res.sum2$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(res.sum2$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")

# We will now plot a red dot to indicate the model with the largest adjusted R^2 statistic.
# The which.max() function can be used to identify the location of the maximum point of a vector
adj_r2_max = which.max(res.sum2$adjr2) # 11

# The points() command works like the plot() command, except that it puts points 
# on a plot that has already been created instead of creating a new plot
points(adj_r2_max, res.sum2$adjr2[adj_r2_max], col ="red", cex = 2, pch = 20)

# We'll do the same for C_p and BIC, this time looking for the models with the SMALLEST statistic
plot(res.sum2$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
cp_min = which.min(res.sum2$cp) # 10
points(cp_min, res.sum2$cp[cp_min], col = "red", cex = 2, pch = 20)

plot(res.sum2$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
bic_min = which.min(res.sum2$bic) # 6
points(bic_min, res.sum2$bic[bic_min], col = "red", cex = 2, pch = 20)
