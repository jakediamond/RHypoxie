do_hours <- df_loire %>%
  mutate(year = year(date),
         month = month(date),
         hour = hour(datetime)) %>%
  group_by(site_code, year, month, date, strahler) %>%
  select(hour, DO, DO_temp) %>%
  pivot_longer(cols = c(DO, DO_temp)) %>%
  ungroup() %>%
  group_by(site_code, year, month, date, strahler, name) %>%
  filter(value == max(value) | value == min(value)) %>%
  mutate(type = if_else(value==max(value), "max", "min")) %>%
  ungroup() %>%
  group_by(year, month, strahler, name, type) %>%
  summarize(val = mean(hour),
            sd = sd(hour))

ggplot(data = do_hours,
       aes(x = month,
           y = val,
           color = strahler,
           group = strahler)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_c() +
  theme_bw() +
  geom_hline(yintercept = 12, linetype = "dashed") +
  facet_grid(name~type) +
  labs(x = "month",
       y = "hour")
