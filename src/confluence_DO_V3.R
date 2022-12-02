# Set working directory
setwd("Z:/Loire_DO")
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

# Load data
# df_DO <- readRDS("Headwaters/Data/DO_daily_summary.RDS")
df_DO <- readRDS("Headwaters/Data/DO_summary_elevationsaturation_v2.RDS")

# Read in watershed info
conf_meta <- read_excel("Headwaters/Data/confluence_metadata.xlsx") %>%
  mutate(site = tolower(Site))

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
