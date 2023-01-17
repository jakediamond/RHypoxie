


df <- readRDS(file.path("data", "10_clean_data", 
                        "all_hourly_data_and_metabolism.RDS")) %>%
  mutate(basin = if_else(watershed %in% c("ardieres", "vauxonne", "yzeron"),
                         "Rhône",
                         "Loire"))

df_sup2 <- df %>%
  filter(oow == "no" | is.na(oow)) %>%
  group_by(basin, watershed, site, year) %>%
  select(DO, temp, DO_per, qsite_m3s, GPP, ER) %>%
  mutate(GPP = if_else(GPP < 0, 0, GPP),
         ER = if_else(ER > 0, 0, ER)) %>%
  summarize(across(everything(), list(min = min, 
                                      q10 = ~quantile(., 0.1, na.rm = T),
                                      q25 = ~quantile(., 0.25, na.rm = T),
                                      q75 = ~quantile(., 0.75, na.rm = T),
                                      q90 = ~quantile(., 0.90, na.rm = T),
                                      median = median, 
                                      mean = mean, 
                                      max = max,
                                      sd = sd), 
                   na.rm = T))

write_excel_csv2(df_sup2, file.path("results", "DO_temp_met_summary_rapport.csv"))
