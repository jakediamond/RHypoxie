# 
# Purpose: To try to predict hypoxia metrics with land use/GIS daa
# Author: Jake Diamond
# Date: 18 January 2022
# 
library(car)
library(leaps)
library(caret)
library(ROSE)
library(rpart)
library(rpart.plot)
library(randomForest)
library(lubridate)
library(tidyverse)

# Set seed
set.seed(42)

# Load hourly data -----------------------------------------------
# Load data
df <- readRDS(file.path("data", "10_clean_data", "hourly_data_all.RDS"))

# Add info for light, time, and hypoxia (defined as less than 3 mg/L)
df <- df %>%
  mutate(date = date(datetime),
         month = month(datetime),
         year = year(date)) %>%
  filter(between(month, 3, 10),#don't trust data outside of these months
         !(is.na(DO) & month == 10), # some data at the end of the files are NA
         (oow == "no" | is.na(oow))) %>% #don't want to include data when streams are dry
  mutate(DO = if_else(site %in% c("carrat", "toranche st cyr les vignes", "le rieu")
                      & month == 4 & DO < 3, NA_real_, DO)) %>% # also filter some times I know a sensor is buried
  mutate(site = if_else(site == "coise aval vaudragon",
                        "coise aval coizet", site)) %>% #rename a site
  mutate(DOhyp = if_else(DO < 3, 
                         1, 
                         0)) #define hypoxia as less than 3 mg/Lsaturation 

# Load predictor variables
df_preds <- readRDS(file.path("data", "predictor_variables.RDS"))

# Logistic regression -----------------------------------------------------
# Get rid of 0 flows to take log discharge
df_mod <- mutate(df, logq = if_else(q_mmd >0, log(q_mmd), log(q_mmd+0.001))) %>%
  drop_na(DOhyp, temp, logq) %>%
  left_join(df_preds) %>%
  select(DOhyp, logq, temp, strahler, slope, geomorph) %>%
  mutate(geomorph = as.factor(geomorph))

# check the balance of hypoxic and non-hypoxic data
table(df_mod$DOhyp)
#20x more non hypoxic than hypoxic (4%), not balanced, won't work for regression

# Split the data into training and test set
training.samples <- df_mod$DOhyp %>% 
  createDataPartition(p = 0.7, list = FALSE)
df_train  <- df_mod[training.samples, ]
df_test <- df_mod[-training.samples, ]

# Balance the data
# First with over-sampling hypoxic data
df_ov <- ovun.sample(DOhyp ~ ., data = df_train, 
                      method = "over", seed = 42)$data
table(df_ov$DOhyp)

# Second with under-sampling non-hypoxic 
df_un <- ovun.sample(DOhyp ~ ., data = df_train, 
                     method = "under", seed = 42)$data
table(df_un$DOhyp)

# Third over-under sampling
df_ou <- ovun.sample(DOhyp ~ ., data = df_train, 
                     method = "both", seed = 42)$data
table(df_ou$DOhyp)

# Finally try with ROSE
df_rose <- ROSE(DOhyp ~ ., data = df_train, seed = 42)$data
table(df_rose$DOhyp)

# build decision tree models
tree.rose <- rpart(DOhyp ~ ., data = df_rose, method = "class")
tree.over <- rpart(DOhyp ~ ., data = df_ov, method = "class")
tree.under <- rpart(DOhyp ~ ., data = df_un, method = "class")
tree.both <- rpart(DOhyp ~ ., data = df_ou, method = "class")

# make predictions on unseen data
pred.tree.rose <- predict(tree.rose, newdata = df_test)
pred.tree.over <- predict(tree.over, newdata = df_test)
pred.tree.under <- predict(tree.under, newdata = df_test)
pred.tree.both <- predict(tree.both, newdata = df_test)

# Area under the curve comparisons
roc.curve(df_test$DOhyp, pred.tree.rose[,2])
roc.curve(df_test$DOhyp, pred.tree.over[,2])
roc.curve(df_test$DOhyp, pred.tree.under[,2])
roc.curve(df_test$DOhyp, pred.tree.both[,2])

summary(tree.both)
rpart.plot(tree.both)
# rpart.rules(tree.both, extra = 4, cover = TRUE)
# Save file
png(filename = file.path("results", "Figures", 
                         "classification_trees", "hypoxia.png"),
    width = 18,
    height = 12,
    units = "cm",
    res = 600
    )
rpart.plot(tree.both)
dev.off()
# Make predictions
mod_probs <- pred.tree.both[,2]
pred_classes <- ifelse(mod_probs > 0.5, 1, 0)

# Model accuracy
mean(pred_classes == df_test$DOhyp)

# accuracy and sensitivity
df_ac <- tibble(obs = df_test$DOhyp, 
                pred = pred_classes) %>%
  mutate(obs = if_else(obs == 0, "oxic", "hypoxic"),
         pred = if_else(pred == 0, "oxic", "hypoxic"))
df_ac$obs <- as.factor(df_ac$obs)
df_ac$pred <- as.factor(df_ac$pred)

twoClassSummary(as.data.frame(df_ac), lev = c("hypoxic", "oxic"))

# Holdout to Check for variance inflation
ROSE.holdout <- ROSE.eval(DOhyp ~ ., data = df_train, learner = rpart, 
                          method.assess = "holdout", 
                          control.learner = list(method = "class"),
                          extr.pred = function(obj)obj[,2], seed = 42)
ROSE.holdout
# Roughly the same as we find with the testing, all good

# Continuous rpart --------------------------------------------------------
# Get rid of 0 flows to take log discharge
df_mod2 <- mutate(df, logq = if_else(q_mmd >0, log(q_mmd), log(q_mmd+0.001))) %>%
  drop_na(DO, temp, logq) %>%
  left_join(df_preds) %>%
  select(DO, logq, temp, strahler, slope, geomorph, rad_wm2) %>%
  mutate(geomorph = as.factor(geomorph))

# Split the data into training and test set
training.samples2 <- df_mod2$DO %>% 
  createDataPartition(p = 0.7, list = FALSE)
df_train2  <- df_mod2[training.samples2, ]
df_test2 <- df_mod2[-training.samples2, ]

# Continous rpart
mod_con <- rpart(DO ~ ., data = df_train2)
pred_mod_con <- predict(mod_con, newdata = df_test2)
rpart.plot(mod_con)
summary(mod_con)
mean((pred_mod_con - df_test2$DO)^2)^0.5
mean(df_test2$DO - pred_mod_con)
postResample(pred = pred_mod_con, obs = df_test2$DO)


# Save file
png(filename = file.path("results", "Figures", 
                         "classification_trees", "DO_regression.png"),
    width = 18,
    height = 12,
    units = "cm",
    res = 600
)
rpart.plot(mod_con)
dev.off()

# Logistic regression -----------------------------------------------------
# Try logisitic regression
# Model on balanced dataset
modelbal<- glm(DOhyp ~ logq + temp, 
               data = df_ou, family = binomial)
summary(modelbal)
plot(modelbal)

# roc.curve(test.data$DOhyp, pred.treeimb[,2], plotit = F)

# Make predictions
probabilitiesbal <- modelbal %>% predict(df_un, type = "response")
predicted.classesbal <- ifelse(probabilitiesbal > 0.5, 1, 0)

# Model accuracy
mean(predicted.classesbal == df_un$DOhyp)

# Confusion matrix
preds <- ifelse(predicted.classesbal == 0, "oxic", "hypoxic") %>%
  as.factor()
obs <- ifelse(df_un$DOhyp == 0, "oxic", "hypoxic") %>%
  as.factor()
confusionMatrix(preds, obs)

# Plot
p_log <- df_ou %>%
  ggplot(aes(temp, DOhyp, color = logq)) +
  geom_point(alpha = 0.2) +
  theme_classic() +
  scale_color_viridis_c(option = "magma",
                        name = "ln(q)") +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(x = "stream temperature (°C)",
       y = "hypoxia probability")

ggsave(plot = p_log,
       filename = file.path("results", "Figures", "classification_trees",
                            "logistic_regression.png"),
       dpi = 600,
       width = 13,
       height = 9.2,
       units = "cm")
