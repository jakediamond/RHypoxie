setwd("C:/Users/diamo/Dropbox/Projects/RHypoxie")
load("Data/00_Naiades,db_data/oxygen_Temp_data.Rdata")

df <- readRDS("C:/Users/diamo/Dropbox/Projects/RHypoxie/Data/France_DO_data.RDS")

df_bp <- df %>%
  group_by(month) %>%
  summarize(y05 = quantile(DO, 0.05),
            y25 = quantile(DO, 0.25),
            y50 = median(DO),
            y75 = quantile(DO, 0.75),
            y95 = quantile(DO, 0.95))

ggplot(data = df_bp,
       aes(x = month,
           group = month)) +
  theme_bw() +
  geom_boxplot(aes(ymin = y05, lower = y25, middle = y50, upper = y75, ymax = y95),
               stat = "identity") +
  # geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  scale_x_continuous(breaks = seq(1, 12, 1)) +
  geom_hline(yintercept = 4, linetype = "dashed", color = "red") +
  labs(title = "monthly distribution of DO in France",
       x = "month",
       y = "DO (mg/L)")

ggsave(filename = "Figures/monthly_DO_dist_France.png",
       height = 9.2,
       width = 9.2,
       units = "cm",
       dpi = 1200)

df_pr <- df %>%
  group_by(month) %>%
  summarize(nhyp = sum(DO < 4),
            prhyp = nhyp / n())

ggplot(data = df_pr,
       aes(x = month,
           y = prhyp)) +
  geom_area() +
  theme_bw() +
  scale_x_continuous(breaks = seq(1, 12, 1)) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "hypoxia probability in France",
       x = "month",
       y = "P(hypoxia)")


ggsave(filename = "Figures/monthly_hypoxia_prob_France.png",
       height = 9.2,
       width = 9.2,
       units = "cm",
       dpi = 1200)
