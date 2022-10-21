x <- tibble(temp = seq(1, 25, 1)) %>%
  mutate(ScO2 = 1801-120.1*temp+3.782*temp^2-0.0476*temp^3,
         ScCO2 = 1911-118.11*temp+3.453*temp^2-0.0413*temp^3)

ggplot(x,
       aes(x = temp)) +
  geom_line(aes(y = ScO2)) +
  geom_line(aes(y = ScCO2), color = "red")


ggplot(x,
       aes(x = temp,
           y = sqrt(ScO2/ScCO2))) +
  geom_line()
