# 
# Purpose: To do basic DO analysis of RHypoxie on confluences from 2019-2022
# Author: Jake Diamond
# Date: 14 December 2022
# 

# Load libraries
library(lubridate)
library(readxl)
library(plotly)
library(tidyverse)
library(tidytable)

# Load data ---------------------------------------------------------------
# Load hourly DO data
df <- readRDS(file.path("data", "10_clean_data", 
                        "hourly_data_all_including2022.RDS"))

# Load daily DO summary data
# df_DO <- readRDS("Headwaters/Data/DO_daily_summary.RDS")
# df_DO <- readRDS("Headwaters/Data/DO_summary_elevationsaturation_v2.RDS")

# # Read in watershed info
# conf_meta <- read_excel("Headwaters/Data/confluence_metadata.xlsx") %>%
#   mutate(site = tolower(Site))
# 
# # Read in watershed info
# ws_meta <- read_excel("Headwaters/Data/Loire_headwaters_data_share.xlsx",
#                       sheet = "watershed_meta") %>%
#   rename(Site = site) %>%
#   right_join(conf_meta)

# # Load discharge data (only want unique values)
# df_q <- readRDS("Headwaters/Data/Discharge/discharge_plus_v2.RDS") %>%
#   group_by(watershed, date) %>%
#   distinct(q) %>%
#   pivot_wider(names_from = watershed, values_from = q, values_fn = mean) %>%
#   pivot_longer(cols = -date, values_to = "q", names_to = "watershed") %>%
#   distinct(date, watershed, q, .keep_all = TRUE)
# Data manipulation -------------------------------------------------------
# Join the hourly data to the confluence meta data
# df <- conf_meta %>%
#   right_join(df) %>%
#   # mutate(side = fct_rev(side)) %>%
#   arrange(conf) %>%
#   mutate(sat = DO / if_else(temp == 0,
#                        0,
#                        14.652 - 0.41022 * temp + 0.007991 *
#                          temp^2 - 0.000077774 * temp^3))
  # mutate(site_code = fct_inorder(site_code),
  #        site = fct_inorder(site))

## Join the data
# df_DO <- conf_meta %>%
#   right_join(df_DO) %>%
#   # mutate(side = fct_rev(side)) %>%
#   arrange(watershed, area) %>%
#   mutate(site_code = fct_inorder(site_code),
#          site = fct_inorder(site))

# Calculate the theoretical mixing based on conductivity 
df_cdt <- df %>%
  filter(!is.na(spc)) %>%
  select(site, date, confluence, position, datetime, spc) %>%
  group_by(site, date, confluence, position) %>%
  summarize(cond = mean(spc, na.rm = TRUE)) %>%
  ungroup() %>%
  select(-site) %>%
  group_by(confluence, date) %>%
  mutate(pos = if_else(str_detect(position, "up"), "up", "down")) %>%
  select(-position) %>%
  tidyr::pivot_wider(names_from = pos, values_from = cond) %>%
  filter(map(up, length) > 1) %>% #filter for days where both upstreams have a value
  drop_na() %>%
  mutate(ratio = map2(up, down, ~(.y - pluck(.x, 1)) / (pluck(.x, 2) - .y)))

# Functions ---------------------------------------------------------------
# Error propagation function
mutate_with_error = function(.data, f) {
  exprs = list(
    # expression to compute new variable values
    deparse(f[[3]]),
    # expression to compute new variable errors
    sapply(all.vars(f[[3]]), function(v) { #function of all the variables in the expression
      dfdp = deparse(D(f[[3]], v)) #partial derivatives of the variables expression, v are the variables
      sprintf('(d%s*(%s))^2', v, dfdp)
    }) %>%
      paste(collapse='+') %>%
      sprintf('sqrt(%s)', .)
  )
  names(exprs) = c(
    deparse(f[[2]]),
    sprintf('d%s', deparse(f[[2]]))
  )
  .data %>%
    # the standard evaluation alternative of mutate()
    mutate_(.dots=exprs)
}

# function to compare
compare_fun <- function(data, type){
  data %>%
    mutate(upval = map_dbl(.$up, 
                           if(type == "min.") {min} else {max}),
           downval = map_dbl(.$down, mean)) %>%
    mutate(dupval = 0.2, ddownval =0.2, dupval2 = 0.2) %>% # 0.2 mg/L is the accuracy of the instrument
    mutate(out = map2(down, up, ~findInterval(.x, sort(.y))), # is the downstream value within the range of upstream values?
            outtype = if_else(out == 1, "in",
                              if_else(out == 0, "below", "above"))) %>%
    mutate(upval2 = if_else(outtype == "above", map_dbl(.$up, max), upval)) %>% #if the downstream value is above, use the upstream maximum to compare to
    mutate_with_error(diff ~ (downval - upval2) / upval2) %>% # calculate the percentage differences and the error propagation
    mutate(out2 = if_else(abs(diff) - ddiff < 0, "within", "out")) # account for the error to determine if the downstream value is within the upstream values
}

# Function to hysteresis index
hi_fun <- function(data){
  x = data %>% 
    dplyr::mutate(up1n = (up_1 - min(up_1)) / (max(up_1)-min(up_1)),
         up2n = (up_2 - min(up_2)) / (max(up_2)-min(up_2))) %>%
    complete(up1n = seq(0,1,0.02)) %>%
    dplyr::arrange(up1n) %>%
    dplyr::mutate(up2nj = imputeTS::na_interpolation(up2n),
           hi = up1n - up2nj)
  y = mean(x$hi, na.rm = T)
}
# Analysis ----------------------------------------------------------------
# get a confluence dataframe
df_conf <- df %>%
  select(site, datetime, DO, temp, DO_per, spc, confluence, position) %>%
  ungroup() %>%
  # filter(position != "down") %>% # only want to compare upstream signals
  pivot_longer(cols = c(DO, temp, DO_per, spc)) %>%
  mutate(pos_num = if_else(str_detect(position, "2"), "2", "1"),
         location = if_else(str_detect(position, "up"), "up", "down"))

# Get hystersis and lin mod between both upstream sites
df_conf2 <- df_conf %>%
  select(-site, -location, -pos_num) %>% 
  drop_na() %>%
  distinct() %>%
  tidyr::pivot_wider(., names_from = position, values_from = value) %>%
  mutate(date = date(datetime)) %>%
  drop_na() %>%
  group_by(confluence, name, date) %>%
  nest() %>%
  mutate(hi = map_dbl(data, possibly(hi_fun, otherwise = NA_real_)),
         mod = map(data, possibly(.f = ~lm(.$up_2 ~ .$up_1), 
                                  otherwise = NA_real_)))

# Save this base paired upstream data
y = tidyr::unnest(select(df_conf2, -hi, -mod), cols = c(data))
write_excel_csv(y, file.path("results",  "confluences", "paired_upstream_confluences.csv"))

# summary info
df_conf_sum <- df_conf2 %>%
  ungroup() %>%
  select(-data) %>%
  mutate(modsum = map(mod, summary),
         r2 = purrr::map_dbl(modsum, "r.squared"),
         tidied = map(mod, broom::tidy)) %>%
  select(-modsum, -mod) %>%
  tidyr::unnest(cols = tidied) %>%
  mutate(term = if_else(term == "(Intercept)", "intercept", "slope"))

# Add the area difference
df_area <- df %>%
  distinct(confluence, site, area_km2) %>%
  group_by(confluence) %>%
  slice_min(area_km2, n = 2) %>%
  mutate(ord = row_number()) %>%
  drop_na() %>%
  select(-site) %>%
  pivot_wider(names_from = ord, values_from = area_km2) %>%
  mutate(ar_rat = `1`/`2`,
         sim = `1` * (`1`+`2`) / `2`) %>%
  left_join(select(df, confluence, watershed) %>% distinct())

# # Bring in other data
# df_conf_sum2 <- left_join(df_conf_sum, df_area) %>%
#   left_join(df_q) %>%
#   mutate(q = if_else(q==0, 0.00001, q))

# now do the upstream-downstream comparison
# daily min max
df_minmax <- df_conf %>%
  mutate(date = date(datetime)) %>%
  group_by(position, pos_num, location, date, name, confluence) %>%
  summarize(max = max(value, na.rm = T),
            min = min(value, na.rm = T)) %>%
  ungroup() %>%
  drop_na() %>%
  filter(!is.infinite(max))
  
# Compare up and down 
df_comp <- df_minmax %>%
  select(-position, -pos_num) %>%
  pivot_longer(cols = c(min,max), names_to = "type") %>%
  group_by(confluence, name, date, type) %>%
  pivot_wider(names_from = location, values_from = value, 
                                              values_fn = list) %>%
  filter(map(up, length) == 2,
         map(down, length) == 1) %>% #filter for days where both upstreams have a value
  group_by(confluence, name, date, type) %>%
  nest() 

# Save for later use
saveRDS(df_comp, file.path("results", "confluences", "dailyminmax.RDS"))

# Do the compare function
df_comp2 <- df_comp %>%
  mutate(comp = map2(data, type, possibly(compare_fun, otherwise = NA_real_)))

# Save for later use
saveRDS(df_comp2, file.path("results", "confluences", "dailyminmax_diffs.RDS"))

# unnest and look at results
df_comp_results <- df_comp2 %>%
  select(-data) %>%
  tidyr::unnest(cols = c(comp))

# Plots -------------------------------------------------------------------
x = filter(df_conf, conf == "mar_cur", name == "DO", date == ymd("2020-09-02")) %>%
  pluck(4,1)

ggplot(data = x, aes(up1, up2, color = datetime)) + geom_point()

# Plot 
# df_conf_sum2 %>%
#   filter(p.value > 0.01, estimate > 0, term == "slope") %>% ggplot(aes(x = q, y = estimate)) +
#   stat_summary_bin(bins = 20) + theme_bw(base_size = 9) +
#   scale_x_log10() +
#   scale_color_viridis_c() +
#   # scale_y_continuous(limits= c(-2,10)) +
#   # facet_wrap(~name, scales = "free")
#  # scale_y_continuous() +#scale_y_log10() + 
#   facet_grid(term~name, scales = "free")
# str(df_conf_sum2)
# Plot it
df_comp_results %>%
  mutate(out3 = if_else(outtype == "in",
                        "within", outtype)) %>%
  mutate(out3 = if_else(out2 == "within" | out3 == "within",
                        "within", outtype)) %>% 
  # filter(msmt != "amp") %>%
  drop_na(out3) %>%
  left_join(df_area) %>%
  # mutate(conf = fct_reorder(confluence, ar_rat)) %>%
  ungroup() %>%
  filter(name == "DO") %>%
  # select(date, conf, min_out) %>%
  # pivot_longer(-c(conf,date)) %>%
  # mutate(name = recode(name, 
  #                      max_out = "downstream daily maximum DO",
  #                      min_out = "downstream daily minimum DO")) %>%
  # mutate(conf = fct_relevel(conf, "fon_vio", "lig_viz", "mou_fon", "loi_doi",
  #                           "mar_cur", "char_car", "coise_pot", "char_mou",
  #                           "coise_rieu", "coise_coiz")) %>%
  mutate(conf = fct_reorder(confluence, outtype, .fun = function(.x) sum(.x == "below"))) %>%
  ggplot(data = ., aes(x = conf,
             fill = out3)) +
  geom_bar(position = "fill") +
  coord_flip() +
  geom_text(aes(label = round(ar_rat, 2), y = 0.2), 
            check_overlap = TRUE, size = 2) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("darkgreen", "black", "lightgrey"),
                    name = "",
                    labels = c("au-dessus de la gamme amont",
                               "sous la gamme amont",
                               "dans la gamme amont")) +
  facet_grid(cols = vars(type)) +
  theme_bw(base_size = 10) +
  labs(y = "pourcentage de jours",
       x = "confluence") +
  theme(legend.position = "bottom", legend.direction = "horizontal")
ggsave(filename = file.path("results", "figures", "confluences", 
                            "confluence_summary_DO.png"),
       dpi = 600,
       width = 18.4,
       height = 16,
       units = "cm") 




df_comp_results %>%
  mutate(out3 = if_else(outtype == "in",
                        "within", outtype)) %>%
  mutate(out3 = if_else(out2 == "within" | out3 == "within",
                        "within", outtype)) %>% 
  filter(out3 != "within") %>%
  filter(name == "DO") %>%
  drop_na(out3) %>%
  left_join(df_area) %>%
  # select(date, conf, per_ab, per_be) %>%
  # unnest() %>%
  mutate(conf = fct_reorder(confluence, ar_rat, .fun = mean, na.rm = TRUE)) %>%
  # pivot_longer(-c(conf, date)) %>%
  # unnest() %>%
  # drop_na() %>%
  ggplot(aes(x = conf,
             y = diff,
             color = out3)) +
  stat_summary(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  # scale_y_log10() +
  # geom_point(shape = 21, position = position_jitterdodge()) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = c("darkgreen", "black"),
                    name = "",
                    labels = c("% > amont max.",
                               "% < amont min.")) +
  facet_grid(cols = vars(type), scales = "free") +
  theme_bw(base_size = 10) +
  labs(y = "aval-amont % de différence", 
       x = "confluence") +#expression(A[main]*"/"*A[trib])) +
  theme(legend.position = "bottom", legend.direction = "horizontal")

ggsave(filename = file.path("results", "figures", "confluences", 
                            "confluence_percent_diffs_DO.png"),
       device = "png",
       dpi = 600,
       width = 18.4,
       height = 16,
       units = "cm")

df_comp_results %>%
  mutate(out3 = if_else(outtype == "in",
                        "within", outtype)) %>%
  mutate(out3 = if_else(out2 == "within" | out3 == "within",
                        "within", outtype)) %>% 
  filter(out3 != "within") %>%
  filter(name == "DO") %>%
  drop_na(out3) %>%
  group_by(type, out3) %>%
  summarize(mn = mean(diff,na.rm=TRUE))
  


rm(f)
data = tibble(up = 7.33, down = 7.05, dup = 0.2, ddown = 0.2)
data %>% mutate_with_error(diff ~ (down-up)/((down + up)/ 2))
