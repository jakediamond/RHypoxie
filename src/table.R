# Load libraries
library(lubridate)
library(caret)
library(tidyverse)

# Load data
df <- readRDS(file.path("data", "10_clean_data", "hourly_data_all.RDS"))

# Add info for light, time, and hypoxia (defined as less than 3 mg/L)
df <- df %>%
  mutate(date = date(datetime),
         month = month(datetime),
         year = year(date),
         solartime = streamMetabolizer::calc_solar_time(datetime, longitude),
         light = streamMetabolizer::calc_light(solartime, latitude, longitude)) %>%
  filter(between(month, 3, 10),#don't trust data outside of these months
         !(is.na(DO) & month == 10), # some data at the end of the files are NA
         (oow == "no" | is.na(oow))) %>% #don't want to include data when streams are dry
  mutate(DO = if_else(site %in% c("carrat", "toranche st cyr les vignes", "le rieu")
                      & month == 4 & DO < 3, NA_real_, DO)) %>% # also filter some times I know a sensor is buried
  mutate(DOhyp = if_else(DO < 3, 
                         1, 
                         0)) #define hypoxia as less than 3 mg/Lsaturation 

# Overall hypoxia info ----------------------------------------------------
ungroup(df) %>%
  filter(!is.na(DO)) %>%
  summarize(hy = sum(DOhyp, na.rm = T),
            n = n(),
            per = hy / n)

# Hypoxia by strahler order and watershed ---------------------------------
df_hyp_per <- ungroup(df) %>%
  filter(!is.na(DO)) %>%
  group_by(strahler, year) %>%
  summarize(hy = sum(DOhyp, na.rm = T),
            n = n(),
            per = hy / n)

# hypoxia run lengths
df_hyp_rls <- df %>%
  group_by(site) %>%
  arrange(site, datetime) %>%
  mutate(hyp_l = sequence(rle(DOhyp)$lengths),
         hyp_change = if_else(((lag(DOhyp, default = 0) != DOhyp) &
                                 (lag(DOhyp, 2, default = 0) != DOhyp)), 
                              1, 0)) %>%
  filter(DOhyp == 1) %>%
  mutate(hyp_pd = cumsum(replace_na(hyp_change, 0))) %>%
  mutate(year = year(date)) %>%
  ungroup()

# Median lengths of hypoxia
df_hyp_len <- ungroup(df_hyp_rls) %>%
  group_by(site, hyp_pd) %>%
  filter(hyp_l == max(hyp_l)) %>%
  ungroup() %>%
  group_by(strahler, year) %>%
  summarize(#hyp_len_med = median(hyp_l),
            hyp_len_mean = mean(hyp_l),
            sd_len = sd(hyp_l))

# Median timing between hypoxic events
df_hyp_diff <- ungroup(df_hyp_rls) %>%
  group_by(site, hyp_pd) %>%
  filter(hyp_l == max(hyp_l) |
           hyp_l == min(hyp_l)) %>%
  ungroup() %>%
  group_by(site) %>%
  mutate(t_dif = if_else(hyp_pd - lag(hyp_pd) == 1, 
                         datetime - lag(datetime), NA_real_)) %>%
  ungroup() %>%
  group_by(strahler, year) %>%
  summarize(hyp_t_dif_mean = as.numeric(mean(t_dif, na.rm = T) / 3600/24),
            hyp_t_dif_sd = as.numeric(sd(t_dif, na.rm = T) / 3600/24))

# Number of hypoxia periods
df_hyp_pds <- df_hyp_rls %>%
  group_by(strahler, year) %>%
  summarize(pds = max(hyp_pd, na.rm = T))

# probability of nighttime hypoxia given hypoxia
df_hyp_night <- df %>%
  filter(DOhyp == 1) %>%
  group_by(strahler, year) %>%
  mutate(night = if_else(light < 200, 1, 0)) %>%
  summarize(nhyp = sum(night == 1) / n())

# overall
df_hyp_all2 <- left_join(df_hyp_per, df_hyp_diff) %>%
  left_join(df_hyp_len) %>%
  left_join(df_hyp_pds) %>%
  left_join(df_hyp_night) %>%
  mutate_if(is.numeric, ~replace_na(., 0))

write_csv(df_hyp_all, "hypoxia_strahler_order_year_table.csv")


summary(lm(hyp_len_mean ~ strahler, data = df_hyp_all))



# Logistic regression -----------------------------------------------------

df_mod <- mutate(df, logq = if_else(q_mmd >0, log(q_mmd), log(q_mmd+0.001))) %>%
  drop_na(DOhyp, temp, logq)

# Split the data into training and test set
set.seed(42)
training.samples <- df_mod$DOhyp %>% 
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- df_mod[training.samples, ]
test.data <- df_mod[-training.samples, ]

# Fit the model
model <- glm(DOhyp ~ temp, data = train.data, family = binomial)
# Summarize the model
summary(model)
# Make predictions
probabilities <- model %>% predict(test.data, type = "response")
predicted.classes <- ifelse(probabilities > 0.2, 1, 0)
# Model accuracy
mean(predicted.classes == test.data$DOhyp)

train.data %>%
  ggplot(aes(temp, DOhyp)) +
  # geom_point(alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(
    title = "Logistic Regression Model", 
    x = "Plasma Glucose Concentration",
    y = "Probability of being diabete-pos"
  )


model2 <- glm(DOhyp ~ logq, 
              data = train.data, family = binomial)
summary(model2)

# Make predictions
probabilities2 <- model2 %>% predict(test.data, type = "response")
predicted.classes2 <- ifelse(probabilities2 > 0.9, 1, 0)
# Model accuracy
mean(predicted.classes2 == test.data$DOhyp)

train.data %>%
  ggplot(aes(logq, DOhyp)) +
  # geom_point(alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(
    title = "Logistic Regression Model", 
    x = "specific discharge",
    y = "hypoxia prob."
  )


library(ROSE)
#check table
table(df_mod$DOhyp)
#20x more non hypoxic than hypoxic (4%), not balanced

# Balance
df_bal <- ovun.sample(DOhyp ~ temp + logq, data = df_mod, 
                                  method = "both", p=0.5,  
                                  N=2000, seed = 42)$data
table(df_bal$DOhyp)

# new datasetes
training.samples2 <- df_bal$DOhyp %>% 
  createDataPartition(p = 0.8, list = FALSE)
train.data2  <- df_bal[training.samples2, ]
test.data2 <- df_bal[-training.samples2, ]

# Model on balanced dataset
modelbal<- glm(DOhyp ~ logq + temp, 
              data = train.data2, family = binomial)
summary(modelbal)

# roc.curve(test.data$DOhyp, pred.treeimb[,2], plotit = F)

# Make predictions
probabilitiesbal <- modelbal %>% predict(test.data2, type = "response")
predicted.classesbal <- ifelse(probabilitiesbal > 0.5, 1, 0)
# Model accuracy
mean(predicted.classesbal == test.data2$DOhyp)

train.data2 %>%
  ggplot(aes(temp, DOhyp, color = logq)) +
  geom_point(alpha = 0.2) +
  scale_color_viridis_c()+
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(
    title = "Logistic Regression Model", 
    x = "specific discharge",
    y = "hypoxia prob."
  )


roc.curve(train.data2$DOhyp, probabilitiesbal,
          main="ROC curve \n (Half circle depleted data)")


roc.curve(test.data2$DOhyp, predicted.classesbal,  col=2,
          lwd=2, lty=2)


roc.curve(test.data$DOhyp, predicted.classes)




library(rpart)

treeimb <- rpart(DOhyp ~ temp+logq, data = df_bal)
pred.treeimb <- predict(treeimb, newdata = test.data2)
accuracy.meas(test.data2$DOhyp, pred.treeimb)
roc.curve(test.data2$DOhyp, pred.treeimb, plotit = F)
