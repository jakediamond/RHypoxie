library(tidyverse)
library(patchwork)
data_path <- file.path("data", "12_gammarid", "IT")   # path to the data
files_con <- dir(data_path, pattern = "Concentration.txt") # get file names for concentration
files_rate <- dir(data_path, pattern = "Rate.txt") # get file names for survival rates
files_dat <- dir(data_path, pattern = "0_") # get file names for data

df_conc <- data_frame(filename = files_con) %>% # create a data frame holding the file names
  mutate(file_contents = map(filename,          # read files into
                             ~ read_table(file.path(data_path, .))) # a new data column
  ) %>%
  unnest(file_contents) %>%
  mutate(site = word(filename, 2, sep = "_"),
         type = word(filename, 1, sep = "_")) %>%
  mutate(type = if_else(type == 1, "drying", "storm")) %>%
  set_names(names(.) %>% str_replace_all('"', ""))


df_rate <- data_frame(filename = files_rate) %>% # create a data frame holding the file names
  mutate(file_contents = map(filename,          # read files into
                             ~ read_table(file.path(data_path, .))) # a new data column
  ) %>%
  unnest(file_contents) %>%
  mutate(site = word(filename, 2, sep = "_"),
         type = word(filename, 1, sep = "_")) %>%
  mutate(type = if_else(type == 1, "drying", "storm")) %>%
  set_names(names(.) %>% str_replace_all('"', ""))

df_dat <- data_frame(filename = files_dat) %>% # create a data frame holding the file names
  mutate(file_contents = map(filename,          # read files into
                             ~ read_table(file.path(data_path, .))) # a new data column
  ) %>%
  unnest(file_contents) %>%
  mutate(site = str_match(filename, "[:digit:][:punct:]\\s*(.*?)\\s*.txt")[,2]) %>%
  left_join(distinct(df_conc, site, type))

# Time until 50% mortality
df_50 <- df_rate %>%
  group_by(site, type) %>%
  filter(abs(IT - 0.5) == min(abs(IT - 0.5)))


p_mort <- ggplot(data = df_rate,
       aes(x = times / 24,
           y = IT,
           color = site)) +
  geom_line() + 
  scale_color_manual(values = c("black", "#E69F00", "black", "#009E73", 
                                "#E69F00", "#D55E00", "#0072B2", "#0072B2")) +
  scale_fill_manual(values = c("black", "#E69F00", "black", "#009E73", 
                               "#E69F00", "#D55E00", "#0072B2", "#0072B2")) +
  geom_hline(yintercept = 0.5, linetype = "dashed") + 
  # geom_segment(data = df_50,
  #            aes(x = times / 24, xend = times /24,
  #                y = 0, yend = 0.5)) +
  facet_grid(cols = vars(type), scales = "free_x", space = "free_x") + 
  geom_ribbon(aes(ymin = IT_2.5, ymax = IT_97.5, fill = site), alpha = 0.2,
              show.legend = FALSE) + 
  theme_classic() +
  labs(x = "time (days)",
       y = "survival") +
  theme(strip.text = element_blank(),
        strip.background = element_blank(),
        legend.position = "none")


p_conc <- ggplot(data = df_dat,
       aes(x = time / 24,
           y = 12-conc,
           color = site)) +
  geom_line() + 
  # geom_segment(data = df_50,
  #              aes(x = times / 24, xend = times /24,
  #                  y = 0, yend = 3)) +
  scale_color_manual(values = c("black", "#E69F00", "black", "#009E73", 
                                "#E69F00", "#D55E00", "#0072B2", "#0072B2")) +
  facet_grid(cols = vars(type), scales = "free_x", space = "free_x") +
  geom_hline(yintercept = 3, linetype = "dashed") + 
  theme_classic() +
  labs(x = "time (days)",
       y = expression("DO (mg "*L^{-1}*")")) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        strip.background = element_blank(),
        legend.position  = "none")

p_all <- (p_conc / p_mort) + plot_annotation(tag_levels = "A")
ggsave(plot = p_all,
       filename = file.path("results", "Figures", "gammarid_survival.png"),
       dpi = 1200,
       units = "cm",
       width = 18,
       height = 12)
