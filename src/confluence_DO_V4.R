# Set working directory
# setwd("Z:/Loire_DO")
setwd("C:/Users/jake.diamond/Dropbox/Projects/Loire_headwaters")
# Load libraries
library(lubridate)
library(readxl)
library(transformr)
library(sf)
library(gganimate)
library(ggthemes)
library(ggspatial)
library(plotly)
library(tidyverse)

# Load data ---------------------------------------------------------------
# Load hourly DO data
df <- readRDS("Headwaters/Data/headwaters_data_clean") %>%
  select(Site, datetime, DO, temp = DO_temp)

# Load daily DO summary data
# df_DO <- readRDS("Headwaters/Data/DO_daily_summary.RDS")
# df_DO <- readRDS("Headwaters/Data/DO_summary_elevationsaturation_v2.RDS")

# Read in watershed info
conf_meta <- read_excel("Headwaters/Data/confluence_metadata.xlsx") %>%
  mutate(site = tolower(Site))

# Read in watershed info
ws_meta <- read_excel("Headwaters/Data/Loire_headwaters_data_share.xlsx",
                      sheet = "watershed_meta") %>%
  rename(Site = site) %>%
  right_join(conf_meta)

# Load discharge data (only want unique values)
df_q <- readRDS("Headwaters/Data/Discharge/discharge_plus_v2.RDS") %>%
  group_by(watershed, date) %>%
  distinct(q) %>%
  pivot_wider(names_from = watershed, values_from = q, values_fn = mean) %>%
  pivot_longer(cols = -date, values_to = "q", names_to = "watershed") %>%
  distinct(date, watershed, q, .keep_all = TRUE)
# Data manipulation -------------------------------------------------------
# Join the hourly data to the confluence meta data
df <- conf_meta %>%
  right_join(df) %>%
  # mutate(side = fct_rev(side)) %>%
  arrange(conf) %>%
  mutate(sat = DO / if_else(temp == 0,
                       0,
                       14.652 - 0.41022 * temp + 0.007991 *
                         temp^2 - 0.000077774 * temp^3))
  # mutate(site_code = fct_inorder(site_code),
  #        site = fct_inorder(site))

# Join the data
df_DO <- conf_meta %>%
  right_join(df_DO) %>%
  # mutate(side = fct_rev(side)) %>%
  arrange(watershed, area) %>%
  mutate(site_code = fct_inorder(site_code),
         site = fct_inorder(site))

# Calculate the theoretical mixing based on conductivity 
df_temp <- df_DO %>%
  filter(!is.na(cond)) %>%
  select(site, date, conf, position, datetime, cond) %>%
  group_by(site, date, conf, position) %>%
  summarize(cond = mean(cond, na.rm = TRUE)) %>%
  ungroup() %>%
  select(-site) %>%
  group_by(conf, date) %>%
  pivot_wider(names_from = position, values_from = cond) %>%
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
    mutate(up1n = (up1 - min(up1)) / (max(up1)-min(up1)),
         up2n = (up2 - min(up2)) / (max(up2)-min(up2))) %>%
    complete(up1n = seq(0,1,0.02)) %>%
    arrange(up1n) %>%
    mutate(up2nj = imputeTS::na_interpolation(up2n),
           hi = up1n - up2nj)
  x = mean(x$hi, na.rm = T)
}
# Analysis ----------------------------------------------------------------
# now do the comparison
df_conf <- df %>%
  ungroup() %>%
  filter(position != "down") %>% # only want to compare upstream signals
  pivot_longer(cols = c(DO, temp, sat)) %>%
  select(-Site, -site, -position) %>% 
  pivot_wider(., names_from = pos_num, values_from = value) %>%
  drop_na() %>% 
  mutate(date = date(datetime)) %>%
  group_by(conf, name, date) %>%
  nest() %>%
  mutate(hi = map_dbl(data, possibly(hi_fun, otherwise = NA_real_)),
         mod = map(data, possibly(.f = ~lm(.$up2 ~ .$up1), 
                                  otherwise = NA_real_)))


y = unnest(df_conf, cols = c(data))
write_excel_csv(y, "paired_upstream_confluences.csv")
# summary info
df_conf_sum <- df_conf %>%
  ungroup() %>%
  select(-data) %>%
  mutate(modsum = map(mod, summary),
         r2 = map_dbl(modsum, "r.squared"),
         tidied = map(mod, broom::tidy)) %>%
  select(-modsum, -mod) %>%
  unnest(cols = tidied) %>%
  mutate(term = if_else(term == "(Intercept)", "intercept", "slope"))

# Add the area difference
df_area <- ws_meta %>%
  distinct(conf, site, area_km2) %>%
  group_by(conf) %>%
  slice_min(area_km2, n = 2) %>%
  mutate(ord = row_number()) %>%
  select(-site) %>%
  pivot_wider(names_from = ord, values_from = area_km2) %>%
  mutate(ar_rat = `1`/`2`,
         sim = `1` * (`1`+`2`) / `2`) %>%
  left_join(select(ws_meta, conf, watershed) %>% distinct())

# Bring in other data
df_conf_sum2 <- left_join(df_conf_sum, df_area) %>%
  left_join(df_q) %>%
  mutate(q = if_else(q==0, 0.00001, q))

# now do the comparison
df_comp <- df_DO %>%
  ungroup() %>%
  # mutate(value = abs(value)) %>%
  mutate(vh = value * h )%>%
  dplyr::select(conf, position, value, date, name, type, msmt) %>%
  drop_na() %>%
  group_by(type, msmt) %>%
  nest() %>%
  mutate(dat_w = map(data,
                     ~pivot_wider(., names_from = position, values_from = value) %>%
           dplyr::filter(map(up, length) == 2,
                         map(down, length) == 1))) %>% #filter for days where both upstreams have a value 
  mutate(dat_l = map2(dat_w, msmt, compare_fun))

# unnest and look at results
df_test <- df_comp %>%
  select(-data, -dat_w) %>%
  unnest(cols = c(dat_l))

# Add the area difference
df_area <- ungroup(df_DO) %>%
  distinct(conf, site, area_km2) %>%
  drop_na() %>%
  group_by(conf) %>%
  slice_min(area_km2, n = 2) %>%
  mutate(ord = row_number()) %>%
  select(-site) %>%
  pivot_wider(names_from = ord, values_from = area_km2) %>%
  mutate(ar_rat = `1`/`2`)


# Plots -------------------------------------------------------------------
x = filter(df_conf, conf == "mar_cur", name == "DO", date == ymd("2020-09-02")) %>%
  pluck(4,1)

ggplot(data = x, aes(up1, up2, color = datetime)) + geom_point()

# Plot 
df_conf_sum2 %>%
  filter(p.value > 0.01, estimate > 0, term == "slope") %>% ggplot(aes(x = q, y = estimate)) +
  stat_summary_bin(bins = 20) + theme_bw(base_size = 9) +
  scale_x_log10() +
  scale_color_viridis_c() +
  # scale_y_continuous(limits= c(-2,10)) +
  # facet_wrap(~name, scales = "free")
 # scale_y_continuous() +#scale_y_log10() + 
  facet_grid(term~name, scales = "free")
str(df_conf_sum2)
# Plot it
df_test %>%
  mutate(out3 = if_else(outtype == "in",
                        "within", outtype)) %>%
  mutate(out3 = if_else(out2 == "within" | out3 == "within",
                        "within", outtype)) %>% 
  filter(msmt != "amp") %>%
  left_join(df_area) %>%
  mutate(conf = fct_reorder(conf, ar_rat)) %>%
  ungroup() %>%
  # select(date, conf, min_out) %>%
  # pivot_longer(-c(conf,date)) %>%
  # mutate(name = recode(name, 
  #                      max_out = "downstream daily maximum DO",
  #                      min_out = "downstream daily minimum DO")) %>%
  # mutate(conf = fct_relevel(conf, "fon_vio", "lig_viz", "mou_fon", "loi_doi",
  #                           "mar_cur", "char_car", "coise_pot", "char_mou",
  #                           "coise_rieu", "coise_coiz")) %>%
  # mutate(conf = fct_reorder(conf, value, .fun = function(.x) mean(.x == "below"))) %>%
  ggplot(data = ., aes(x = conf,
             fill = out3)) +
  geom_bar(position = "fill") +
  geom_text(aes(label = round(ar_rat, 2), y = 0.2), 
            check_overlap = TRUE, size = 2) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("darkgreen", "black", "lightgrey"),
                    name = "",
                    labels = c("above upstream range",
                               "below upstream range",
                               "within upstream range")) +
  facet_grid(type~msmt) +
  theme_bw(base_size = 10) +
  ylab("percentage of days") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        axis.title.x = element_blank())
ggsave(filename = "Headwaters/Figures/confluences/confluence_not_within_summary.png",
       dpi = 600,
       width = 18.4,
       height = 16,
       units = "cm") 




df_test %>%
  mutate(out3 = if_else(outtype == "in",
                        "within", outtype)) %>%
  mutate(out3 = if_else(out2 == "within" | out3 == "within",
                        "within", outtype)) %>% 
  filter(out3 != "within", msmt != "amp") %>%
  left_join(df_area) %>%
  # select(date, conf, per_ab, per_be) %>%
  # unnest() %>%
  mutate(diff = if_else(name == "DO_def_min", -diff, diff),
         conf = fct_reorder(conf, ar_rat, .fun = mean, na.rm = TRUE)) %>%
  # pivot_longer(-c(conf, date)) %>%
  # unnest() %>%
  # drop_na() %>%
  ggplot(aes(x = log(ar_rat),
             y = diff,
             color = out3)) +
  stat_summary(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  # scale_y_log10() +
  # geom_point(shape = 21, position = position_jitterdodge()) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = c("darkgreen", "black"),
                    name = "",
                    labels = c("% > upstream max.",
                               "% < upstream min.")) +
  facet_grid(type~msmt, scales = "free") +
  theme_bw(base_size = 10) +
  labs(y = "downstream-upstream % difference", 
       x = expression(A[main]*"/"*A[trib])) +
  theme()

ggsave(filename = "Headwaters/Figures/confluences/confluence_percent_diffs.png",
       device = "png",
       dpi = 600,
       width = 18.4,
       height = 16,
       units = "cm")




rm(f)
data = tibble(up = 7.33, down = 7.05, dup = 0.2, ddown = 0.2)
data %>% mutate_with_error(diff ~ (down-up)/((down + up)/ 2))
