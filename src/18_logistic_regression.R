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

df_preds <- readRDS(file.path("data", "predictor_variables.RDS"))
# Logistic regression -----------------------------------------------------
# Get rid of 0 flows to take log discharge
df_mod <- group_by(df, site, year) %>%
  mutate(qdif = q_mmd - lag(q_mmd),
         logq = if_else(q_mmd >0, log(q_mmd), log(q_mmd+0.001))) %>%
  drop_na(DOhyp, temp, logq, qdif) %>%
  ungroup() %>%
  left_join(df_preds) %>%
  select(DOhyp, logq, qdif, temp, strahler, slope, geomorph) %>%
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
rpart.rules(tree.both, extra = 4, cover = TRUE)
# Make predictions
mod_probs <- pred.tree.both[,2]
pred_classes <- ifelse(mod_probs > 0.5, 1, 0)
# Model accuracy
mean(pred_classes == df_test$DOhyp)

# Check for variance inflation
ROSE.holdout <- ROSE.eval(DOhyp ~ ., data = df_train, learner = rpart, 
                          method.assess = "holdout", 
                          control.learner = list(method = "class"),
                          extr.pred = function(obj)obj[,2], seed = 42)
ROSE.holdout


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

treeimb <- rpart(DOhyp ~ temp+logq, data = df_bal)
pred.treeimb <- predict(treeimb, newdata = test.data2)
accuracy.meas(test.data2$DOhyp, pred.treeimb)
roc.curve(test.data2$DOhyp, pred.treeimb, plotit = F)




# Binomial model on balanced dataset
model_bin<- glm(DOhyp ~ temp + slope + logq, 
               data = df_ou, family = binomial)
summary(model_bin)

# Make predictions
probabilitiesbal <- model_bin %>% predict(df_test, type = "response")
predicted.classesbal <- ifelse(probabilitiesbal > 0.5, 1, 0)
# Model accuracy
mean(predicted.classesbal == df_test$DOhyp)

df_ou %>%
  ggplot(aes(temp, DOhyp, color = logq)) +
  # geom_point(alpha = 0.2) +
  scale_color_viridis_c()+
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(
    title = "Logistic Regression Model", 
    x = "specific discharge",
    y = "hypoxia prob."
  )

roc.curve(df_test$DOhyp, predicted.classesbal)





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


df_hyp_grp <- df_hyp_rls %>%
  group_by(site, hyp_pd) %>%
  summarize(len = n(),
            maxq = max(log(q_mmd + 0.001), na.rm = T),
            minq = min(log(q_mmd + 0.001), na.rm = T),
            medq = median(log(q_mmd + 0.001), na.rm = T),
            temp = mean(temp, na.rm = T)) %>%
  left_join(df_preds)

# Default random forest model
mod_rf0 <- randomForest(formula = len ~ ., data = df_hyp_grp[,-c(1,2)],
                        importance = TRUE)
summary(mod_rf0)
mod_rf0
plot(mod_rf0)
# Get variable importance from the model fit
ImpData <- as.data.frame(importance(mod_rf0))
ImpData$Var.Names <- row.names(ImpData)
