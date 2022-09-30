# 
# Purpose: To try to predict hypoxia metrics with land use/GIS daa
# Author: Jake Diamond
# Date: 18 January 2022
# 
library(car)
library(leaps)
library(randomForest)
library(lubridate)
library(tidyverse)

# Set seed
set.seed(42)

# Load hypoxia summary data -----------------------------------------------
df_hyp <- readRDS(file.path("results", "hypoxia_summary.RDS")) %>%
  mutate(site = if_else(site == "coise aval vaudragon",
                        "coise aval coizet", site))

df_hyp %>%
  mutate(site = forcats::fct_reorder(site, mean_us)) %>%
  ggplot( aes(x=site, y=mean_us)) +
  geom_bar(stat="identity", fill="#f68060",width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()
  

# Load predictor data -----------------------------------------------------
# Load depth, discharge, make it only summer
df_rhone <- readRDS(file.path("data", "estimated_depths.RDS")) %>%
  group_by(site) %>%
  filter(between(month, 7 , 10)) %>%
  dplyr::summarize(depth = median(depth, na.rm = T),
                   q = median(q_mmd, na.rm = T),
                   altitude = mean(altitude_m)) %>%
  left_join(readxl::read_xlsx(file.path("data", "07_geomorphology", 
                                        "rhone_slopes.xlsx")))

# Load loire hydraulic data and land use
df_loire <- readRDS(file.path("data", "09_loire_data","discharge_plus_v2.RDS")) %>%
  group_by(site) %>%
  filter(between(month(date), 7 , 10)) %>%
  summarize(across(where(is.numeric), mean, na.rm = T)) %>%
  select(site, depth = h, q = q, altitude = altitude_m, slope = slope)

# Load thermal regime data
df_therm <- read_tsv(file.path("data", "07_geomorphology", 
                                  "rhypoxie_RMC_join_regimes.txt"),
                     locale = locale(decimal_mark = ",")) %>%
  bind_rows(read_csv2(file.path("data", "07_geomorphology", 
                               "stations_rhypoxie_loire_regimes.txt"))) %>%
  select(site, d_source = D_source, p_cl1 = PROBA_CL1, p_cl2 = PROBA_CL2,
         p_cl3 = PROBA_CL3, p_cl4 = PROBA_CL4, tclass = CLASS_MAJ) %>%
  mutate(site = tolower(site))

# Load all data to get median temp data
df_temp <- readRDS(file.path("data", "10_clean_data", "hourly_data_all.RDS")) %>%
  group_by(site) %>%
  filter(between(month, 7 , 10)) %>%
  summarize(temp_mean = mean(temp, na.rm = T)) %>%
  mutate(site = if_else(site == "coise aval vaudragon",
                        "coise aval coizet", site))

# Load geomorphic units
df_geo <- readxl::read_xlsx(file.path("data", "07_geomorphology",
                                      "geomorphic_units.xlsx")) %>%
  select(-site_code)

# Dataframe of all the predictors
df_preds <- bind_rows(df_loire, df_rhone) %>%
  left_join(df_temp, by = "site") %>%
  left_join(df_therm) %>%
  left_join(df_geo) %>%
  mutate(d_source = if_else(is.na(d_source), 0.5, d_source))

saveRDS(df_preds, file.path("data", "predictor_variables.RDS"))

# Quick look at the predictors 
Hmisc::hist.data.frame(df_preds[,-1])
pairs(df_preds[,-c(1,13)])

# Get model ---------------------------------------------------------------
# Start with just trying percentage of time hypoxic
df_mod <- select(df_hyp, site, mean_us) %>%
  left_join(df_preds) %>%
  select(-site)

# Default random forest model
mod_rf0 <- randomForest(formula = mean_us ~ ., data = df_mod,
                        importance = TRUE)
summary(mod_rf0)
mod_rf0
plot(mod_rf0)
# Get variable importance from the model fit
ImpData <- as.data.frame(importance(mod_rf0))
ImpData$Var.Names <- row.names(ImpData)

ggplot(ImpData, aes(x=Var.Names, y=`%IncMSE`)) +
  geom_segment( aes(x=Var.Names, xend=Var.Names, y=0, yend=`%IncMSE`), color="skyblue") +
  geom_point(aes(size = IncNodePurity), color="blue", alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    legend.position="bottom",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )


# Best subsets
pivot_longer(df_mod[,-13],  cols = everything()) %>%
  ggplot(aes(x = value)) + geom_histogram() + facet_wrap(.~name, scales = "free")

models <- regsubsets(mean_us ~ ., data = df_mod, nvmax = 5)
summary(models)
res.sum <- summary(models)
data.frame(
  Adj.R2 = which.max(res.sum$adjr2),
  CP = which.min(res.sum$cp),
  BIC = which.min(res.sum$bic)
)


mod_lm0 <- lm(mean_us ~ depth + d_source + p_cl1 + p_cl2 + tclass, data = df_mod)
#produce added variable plots
avPlots(mod_lm0)
summary(mod_lm0)

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



# older -------------------------------------------------------------------


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



# df <- readRDS(file.path("data", "09_loire_data","loire_headwaters_data.RDS")) %>%
#   unnest() %>%
#   ungroup() %>%
#   mutate(site = tolower(Site))



# # Load hydraulic data
# # Load geometry data
# df_k <- readxl::read_xlsx(file.path("data", "04_discharge and stage", "hydraulic_data.xlsx")) %>%
#   select(Site = site, width = width_m, depth = depth_m, slope = slope_tnet) %>%
#   mutate(site = tolower(Site)) %>%
#   select(-Site)
