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
# df_cond <- df_DO %>%
#   filter(!is.na(cond)) %>%
#   select(site, date, conf, position, datetime, cond) %>%
#   group_by(site, date, conf, position) %>%
#   summarize(cond = mean(cond, na.rm = TRUE)) %>%
#   ungroup() %>%
#   select(-site) %>%
#   group_by(conf, date) %>%
#   pivot_wider(names_from = position, values_from = cond) %>%
#   filter(map(up, length) > 1) %>% #filter for days where both upstreams have a value 
#   drop_na() %>%
#   mutate(ratio = map2(up, down, ~(.y - pluck(.x, 1)) / (pluck(.x, 2) - .y)))

# now test
df_test <- df_DO %>%
  ungroup() %>%
  mutate(value = abs(value)) %>%
  mutate(vh = value * h )%>%
  # filter(name %in% c("DO_min", "DO_max")) %>%
  dplyr::select(conf, position, value, date, name, type, msmt) %>%
  drop_na() %>%
  group_by(type, msmt) %>%
  nest() %>%
  mutate(dat_w = map(data,
                     ~pivot_wider(., names_from = position, values_from = value) %>%
           dplyr::filter(map(up, length) == 2,
                         map(down, length) == 1))) %>% #filter for days where both upstreams have a value 
  mutate(dat_d = map(dat_w,
                     ~map_dbl(.$up, min)))
  
  mutate(min_up = map_dbl(DO_def_min_up, min),
         # max_up = map_dbl(DO_max_up, max),
         min_down = map_dbl(DO_def_min_down, min),
         max_down = map_dbl(amp_down, max)) %>%
  mutate(dmin_up = 0.2, dmax_up = 0.2, dmin_down =0.2, dmax_down = 0.2) %>%
  mutate_with_error(diff ~ (min_down - min_up)/ min_up) %>%
  mutate(min_out = if_else(diff + ddiff < 0, "below", "above"))

  # drop_na() %>%
  # mutate(min_out = map2(DO_min_down, DO_min_up, ~findInterval(.x, sort(.y))),
  #        max_out = map2(DO_max_down, DO_max_up, ~findInterval(.x, sort(.y)))) %>%
  # mutate(min_out = if_else(min_out == 1, "in",
  #                          if_else(min_out == 0, "below", "above")),
  #        max_out = if_else(max_out == 1, "in",
  #                          if_else(max_out == 0, "below", "above"))) %>%
  # mutate(min_out_check = if_else(min_out == "below", 
  #                                map2(DO_min_down, DO_min_up, ~((min(.y)-0.2) -(.x+0.2))),
  #                                list(NA_real_))) %>%
  # filter(min_out_check > 0)
  # mutate(per_ab = if_else(max_out == "above",
  #                         map2(DO_max_down, DO_max_up, ~(.x - max(.y)) / max(.y)),
  #                         list(NA_real_)),
  #        per_be = if_else(min_out == "below",
  #                         map2(DO_min_down, DO_min_up, ~(.x - min(.y)) / min(.y)),
  #                         list(NA_real_)))

# Combine estimated ratios with DO
# df_test2 <- left_join(df_cond, df_test) %>%
#   mutate(ratio_min = map2(do_min_up, do_min_down, ~(.y - pluck(.x, 1)) / (pluck(.x, 2) - .y)),
#          ratio_max = map2(do_max_up, do_max_down, ~(.y - pluck(.x, 1)) / (pluck(.x, 2) - .y))) %>%
#   select(date, conf, ratio, ratio_min, ratio_max)

# Plot it
df_test %>%
  select(date, conf, min_out) %>%
  pivot_longer(-c(conf,date)) %>%
  mutate(name = recode(name, 
                       max_out = "downstream daily maximum DO",
                       min_out = "downstream daily minimum DO")) %>%
  # mutate(conf = fct_relevel(conf, "fon_vio", "lig_viz", "mou_fon", "loi_doi",
  #                           "mar_cur", "char_car", "coise_pot", "char_mou",
  #                           "coise_rieu", "coise_coiz")) %>%
  mutate(conf = fct_reorder(conf, value, .fun = function(.x) mean(.x == "below"))) %>%
  ggplot(aes(x = conf,
             fill = value)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("darkgreen", "black", "lightgrey"),
                    name = "",
                    labels = c("above upstream range",
                               "below upstream range",
                               "within upstream range")) +
  # facet_wrap(~name) +
  theme_bw(base_size = 18) +
  ylab("percentage of days") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        axis.title.x = element_blank())
ggsave(filename = "Headwaters/Figures/confluence_not_within_summary.png",
       dpi = 600,
       width = 36,
       height = 20,
       units = "cm") 




df_test %>%
  select(date, conf, per_ab, per_be) %>%
  unnest() %>%
  mutate(conf = fct_reorder(conf, per_ab, .fun = mean, na.rm = TRUE)) %>%
  pivot_longer(-c(conf, date)) %>%
  unnest() %>%
  drop_na() %>%
  ggplot(aes(x = conf,
             y = value,
             color = name)) +
  stat_summary(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  # geom_point(shape = 21, position = position_jitterdodge()) +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(-0.30, 0.3, 0.10)) +
  scale_color_manual(values = c("darkgreen", "black"),
                    name = "",
                    labels = c("% > upstream max.",
                               "% < upstream min.")) +
  theme_bw(base_size = 18) +
  ylab("downstream-upstream % difference") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        axis.title.x = element_blank())

ggsave(filename = "Headwaters/Figures/confluence_percent_diffs.png",
       device = "png",
       dpi = 300,
       width = 30,
       height = 20,
       units = "cm")



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
rm(f)
data = tibble(up = 7.33, down = 7.05, dup = 0.2, ddown = 0.2)
data %>% mutate_with_error(diff ~ (down-up)/((down + up)/ 2))
