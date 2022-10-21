# 
# Purpose: To estimate metabolism at Saône headwaters
# Author: Jake Diamond
# Date: 7 September 2022
# 

# Load libraries
library(lubridate)
library(streamMetabolizer)
library(tidyverse)

# DO data load and clean --------------------------------------------------
# Load DO data
df <- readRDS(file.path("data", "10_clean_data", "hourly_data_all.RDS")) %>%
  filter(watershed %in% c("ardieres", "vauxonne", "yzeron")) %>%
  mutate(DO.sat = DO *100 / DO_per)

# Load depth data
df_d <- readRDS(file.path("data", "estimated_depths.RDS")) %>%
  ungroup() %>%
  group_by(site, date) %>%
  summarize(depth = mean(depth, na.rm = T))

# Get daily discharge
df_q <- df %>%
  group_by(site, date) %>%
  summarize(discharge.daily = mean(q_m3s, na.rm = T)) %>%
  imputeTS::na_kalman()

# Load K600 info
df_k <- readRDS(file.path("data", "k_estimates_for_bayes.RDS"))

# Overall data
df <- left_join(df, df_d)

# Prepare data for stream Metabolizer -------------------------------------
# Calculate solar time
df$solar.time <- calc_solar_time(df$datetime, longitude = df$longitude)

# Get data in good format
df_use <- dplyr::select(df,
             site,
             solar.time,
             DO.obs = DO,
             DO.sat = DO.sat,
             depth = depth,
             temp.water = temp,
             light = rad_wm2) %>%
  group_by(site) %>%
  arrange(site, solar.time) %>%
  mutate(light = if_else(is.na(light) | light < 0, 0, light),
         light = convert_SW_to_PAR(light),
         DO.obs = if_else(DO.obs < 0.5, NA_real_, DO.obs),
         DO.sat = if_else(DO.sat < 0.5, NA_real_, DO.obs)) %>%
  mutate(DO.obs = imputeTS::na_kalman(DO.obs, maxgap = 4, type="level"),
         DO.sat = imputeTS::na_kalman(DO.sat, maxgap = 4, type="level"),
         depth = imputeTS::na_kalman(depth, maxgap = 4, type="level"),
         temp.water = imputeTS::na_kalman(temp.water, maxgap = 4, type="level")) %>%
  group_modify(~zoo::na.trim(.))

# Nest data by site
df_n <- df_use %>%
  nest() %>%
  left_join(df_q %>%
              group_by(site) %>%
              arrange(date) %>%
              nest() %>% rename(data_q = data)) %>%
  left_join(df_k %>%
              group_by(site) %>%
              arrange(node) %>%
              nest() %>% rename(data_k = data))

# Configure the model -----------------------------------------------------
# Choose a model structure
# We will pool K600 and include observation and process error
# Look at what data inputs are needed for bayes model
bayes_mod <- mm_name(type = 'bayes', 
                     pool_K600 = 'binned', 
                     err_obs_iid = TRUE, 
                     err_proc_iid = TRUE)
bayes_mod

# look at data needs
metab_inputs("bayes", "data")
specs(bayes_mod)
# Metabolism function for nested data ---------------------------------------
met_fun <- function(data, data_q, data_k, mod_type = bayes_mod){
  # Calculate the natural-log-space centers of the discharge bins
  # Visual inspection of the data suggests 6 bins is appropriate
  brks <- arrange(data_k, node) %>% pull(node)
  
  # Estimate the mean ln(K600) at each ln(Q) node from prior work
  # These are the hyperprior means for ln(K600) in log(Q) space 
  meanlnK <- arrange(data_k, node) %>% pull(meanlnK)
  
  # Same for standard deviation
  sdlnK <- arrange(data_k, node) %>% pull(sdlnK)
  
  # Set the specifications
  bayes_specs <- specs(model_name = mod_type,
                       burnin_steps = 500,
                       saved_steps = 250, 
                       # K600_lnQ_nodediffs_sdlog = 0.5,
                       K600_lnQ_nodes_centers = brks,
                       K600_lnQ_nodes_meanlog = meanlnK,
                       K600_lnQ_nodes_sdlog = sdlnK,
                       )
  
  # Do the metabolism
  metab(specs = bayes_specs, 
        data = as.data.frame(data), 
        data_daily = as.data.frame(data_q))
}

# Run the metabolism model on nested data ---------------------------------
metab_all <- df_n %>%
  filter(site == "charbonnieres") %>%
  transmute(mm = pmap(list(data, data_q, data_k),
                       ~met_fun(data = ..1,
                                data_q = ..2,
                                data_k = ..3)))

# saveRDS(metab_all, "C:/Users/jacob.diamond/Desktop/rhone_metabolism_bayes_new_no_priors.RDS")
# metab_all <- readRDS(file.path("data", "rhone_metabolism_mle.RDS"))
# Inspect the model -------------------------------------------------------
df_metab <- metab_all %>%
  mutate(met = map(mm, predict_metab),
         k = map(mm, get_params)) %>%
  select(-mm) %>%
  unnest(cols = c(k, met), names_repair = "unique") %>%
  ungroup()
saveRDS(mm_mle, file.path("data", "rhone_metabolism_mle_preds.RDS"))

df_metab <- left_join(df_metab, df_q)
mc <- streamMetabolizer::get_mcmc(m)
plot(log(df_metab$discharge.daily), df_metab$K600.daily)
plot(df_metab$K600.daily, -df_metab$ER.daily)

d = pluck(metab_all, 2, 1)
m = pluck(metab_all, 2, 1)
streamMetabolizer::plot_distribs(m, parname = "K600_daily")
get_specs(m)$params_in
plot_metab_preds(m)
rstan::traceplot(mc, pars='K600_daily', nrow=3)

fit <- get_fit(m)


df_metab <- readRDS(file.path("results", "rhone_metabolism_bayes_preds.RDS"))
x <- readRDS("C:/Users/jacob.diamond/Desktop/rhone_metabolism_bayes.RDS")

y = get_params(x)
x <- ungroup(metab_all) %>%
  # filter(site == "mercier amont ratier") %>%
  select(mm) %>%
  pluck(1, 1)
plot_metab_preds(x)
plot_DO_preds(x)
plot(get_params(x)$K600.daily, get_params(x)$ER.daily)

test2 <- mm_all_mle %>%
  mutate(met = map(mm, predict_metab)) %>%
  unnest(met) %>%
  gather(key = flux,
       value = value,
       c(GPP, ER))
streamMetabolizer::metab_night


