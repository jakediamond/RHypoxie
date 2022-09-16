library(nlme)
# Load DO data
df_q <- readRDS(file.path("data", "10_clean_data", "hourly_data_all.RDS")) %>%
  filter(watershed %in% c("ardieres", "vauxonne", "yzeron")) %>%
  group_by(siteq, area_q_km2, date) %>%
  summarize(q = mean(q_mmd, na.rm = TRUE),
            h = mean(h_cm / 100, na.rm = T)) %>%
  mutate(la = log(area_q_km2),
         laf = as.factor(la)) %>%
  imputeTS::na_kalman()


ggplot(data=df_q,
       aes(y = h, 
           x = q^0.5,
           color = la)) +
  geom_point() +
  stat_smooth(method = "lm") +
  scale_color_viridis_c()

mod =lme(h ~ sqrt(q),  
                random = ~1 | la,
                correlation = corAR1(form = ~ 1 | la), 
                data = df_q,
         method = "REML")
summary(mod)
plot(resid(mod, type = "normalized"))
plot(ACF(mod, resType = "normalized"))


re <- random.effects(mod) %>%
  rownames_to_column("la") %>%
  rename(int = `(Intercept)`) %>%
  mutate(la = as.numeric(la))

plot(re$la, re$int)
mod_re <- lm(int ~ la, data = re)
summary(mod_re)

fixed.effects(mod)

ggplot(data=df_q,
       aes(y = h, 
           x = 0.18*sqrt(q) + (0.25 - 0.346+0.11*la),
           color = la)) +
  geom_point() +
  stat_smooth(method = "lm") +
  scale_color_viridis_c()

str(df)

df_depth <- df %>%
  mutate(newdepth = if_else(data.table::between(area_km2, area_q_km2*0.9, area_q_km2*1.1),
                            h_cm / 100,
                            0.18*sqrt(q_mmd) + (0.25 - 0.346+0.11*log(area_km2)) - 0.05
                            )
         ) %>%
  mutate(depth = if_else(newdepth < 0, 0.05, newdepth),
         solar.time = calc_solar_time(datetime, longitude))

saveRDS(df_depth, file.path("data", "estimated_depths.RDS"))














df_night <- z %>%
  mutate(DO_sat = DO * 100 / DO_per) %>%
  select(site,
         solar.time,
         DO.obs = DO,
         DO.sat = DO_sat,
         depth = depth,
         temp.water = temp,
         light = rad_wm2,
         discharge = qsite_m3s)

# Nest data by site
df_night_n <- df_night %>%
  group_by(site) %>%
  arrange(solar.time) %>%
  nest() 

# Configure the model -----------------------------------------------------
# Choose a model structure
metab_mod <- mm_name(type = "night")

# Metabolism function for nested data ---------------------------------------
met_fun <- function(data, mod_name = metab_mod){
  # Set the specifications
  specs <- specs(model_name = mod_name)
  
  # Do the metabolism
  metab(specs = specs, 
        data = as.data.frame(data))
}

# Run the metabolism model on nested data ---------------------------------
k_mod <- df_night_n %>%
  mutate(mm = map(data, ~met_fun(data = .x)))


y = filter(ungroup(k_mod), site == "ratier amont ribes") %>%
  select(mm) %>%
  pluck(1, 1)
n = get_fit(y)


k <- ungroup(k_mod) %>%
  select(-data) %>%
  mutate(n = map(mm, get_fit)) %>%
  select(-mm) %>%
  unnest(n)

k2 <- filter(k, K600.daily > 0, r.squared > 0.5, p.value < 0.05) %>%
  left_join(df %>%
              group_by(site,date, watershed) %>%
              summarize(q = mean(q_m3s, na.rm = T)))

ggplot(data = filter(k2, watershed == "vauxonne"),
       aes(x = log(q),
           y = K600.daily,
           color = site)) +
  geom_point() +
  scale_color_viridis_d()
