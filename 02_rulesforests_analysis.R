#*******************************************************************************
# From rules to forests - Data analysis
# Author: Álvaro F. Junquera (UAB)
#*******************************************************************************

# 1. Analysis with {tidymodels} -------------

dsp1723x <- readRDS("C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Dataset_final/dsp1723xids.RDS")

library(tidymodels)
library(probably)
library(vip)
library(DALEX)
library(DALEXtra)
library(tictoc)
library(vip)
library(ggplot2)
library(pmcalibration)

## Data splitting (3 subsets) ----------
YX_1718192022 <- dsp1723x %>%
  filter(yearstart %in% c(2017, 2018, 2019, 2020, 2022)) %>%
  as.data.frame()

# 08/04/2024: {rsample}, from {tidymodels}, does not currently support a combination of resampling techniques,
# i.e. one logic for the first split (e.g. temporal) and other for the second split (e.g. stratified random).


### First split (training+evaluation, testing)
table(YX_1718192022$yearstart)
44852 + 46548 + 47648 + 57473

prof_split <- initial_time_split(data = YX_1718192022,
                                 prop = 196521/231443)
prof_traineval <- training(prof_split)
prof_test  <- testing(prof_split)

### Second split (training, evaluation)
set.seed(1910)
prof_split2 <- initial_split(prof_traineval, prop = 4/5,
                             strata = ltu)

prof_train <- training(prof_split2)
prof_eval <- testing(prof_split2)

### Joining training and test subset and then "deceiving" {rsample}
prof_traintest <- bind_rows(prof_train, prof_test)
prof_traintest <- prof_traintest %>%
  arrange(startepisode)

table(prof_traintest$yearstart)
35901 + 37092 + 38196 + 46027

prof_split3 <- initial_time_split(data = prof_traintest,
                                  prop = (35901 + 37092 + 38196 + 46027)/nrow(prof_traintest))

prof_train3 <- training(prof_split3)
prof_test3  <- testing(prof_split3)


## Logistic regression ----------
### 1. Formula & preprocessing {recipe}
lr_rec <- 
  recipe(ltu ~ ., data = prof_train3) %>% 
  update_role(id_ind, id_indepi, yearstart, startepisode, new_role = "ID") %>% # maybe including startepisode as predictor? see https://www.tidymodels.org/start/recipes/#fit-workflow
  step_dummy(all_nominal_predictors()) %>% # convert factors to dummies
  step_zv(all_predictors()) # zv = zero variance. Removes columns that have only 1 value (rare categories)

### 2. Specify model {parsnip}
lr_mod <- 
  logistic_reg() %>% 
  set_engine("glm")

### 3. Join {parsnip} and {recipe}
lr_wflow <-
  workflow() %>%
  add_model(lr_mod) %>%
  add_recipe(lr_rec)

### 4. Estimation of regression coefficients
lr_fit <- 
  lr_wflow %>% 
  fit(data = prof_train3)

### 5. Ranking performance in test subset
### 5.a. Discrimination
lr_preds_test <- 
  augment(lr_fit, prof_test3) 

lr_preds_test %>% 
  roc_auc(truth = ltu, .pred_Yes, event_level = "second") # ROC-AUC: 0.722

lr_preds_test %>% 
  roc_curve(ltu, .pred_Yes, event_level = "second") %>% 
  autoplot()

lr_preds_test %>% 
  pr_auc(ltu, .pred_Yes, event_level = "second")

### 5.b. Calibration
round(mean(lr_preds_test$ltu == "Yes") / mean(lr_preds_test$.pred_Yes), 3) # mean calibration

observed_test <- ifelse(lr_preds_test$ltu == "Yes", 1, 0)
pred_test_lr <- lr_preds_test$.pred_Yes

calib_lr <- pmcalibration(y = observed_test, p = pred_test_lr,
                          smooth = "rcs",
                          transf="none",
                          ci = "sim", method="REML")
calib_lr$metrics

### 6. Classification performance according to different policies
## 6.a. Learning policy
## 1- Learning the optimal cutoff 
lr_preds_eval <- 
  augment(lr_fit, prof_eval) 

threshold_lr <- lr_preds_eval %>%
  threshold_perf(truth = ltu, estimate = .pred_Yes,
                 event_level = "second",
                 thresholds = seq(0.01, 0.99, by = 0.0025))

# plot
threshold_lr <- threshold_lr %>%
  filter(.metric != "distance") %>%
  mutate(group = case_when(.metric == "sens" | .metric == "spec" ~ "1",
                           TRUE ~ "2"
  ))

lr_max_j_threshold <- threshold_lr %>%
  filter(.metric == "j_index") %>%
  filter(.estimate == max(.estimate)) %>%
  pull(.threshold)

ggplot(threshold_lr, aes(x = .threshold, y = .estimate, color = .metric, alpha = group)) +
  geom_line() +
  theme_light() +
  scale_color_viridis_d(end = 0.9) +
  scale_alpha_manual(values = c(.4, 1), guide = "none") +
  geom_vline(xintercept = lr_max_j_threshold, alpha = .6, color = "grey30") +
  labs(
    x = "'Good' Threshold\n(above this value is considered 'good')",
    y = "Metric Estimate",
    title = "Balancing performance by varying the threshold",
    subtitle = "Sensitivity or specificity alone might not be enough!\nVertical line = Max J-Index"
  )

## 2- Checking classification performance
preds_lr_learn <- lr_preds_test %>%
  mutate(classpred_AP = if_else(.pred_Yes > lr_max_j_threshold, "Yes", "No")) %>%
  mutate(classpred_AP = as.factor(classpred_AP))

accuracy(preds_lr_learn, truth = ltu, estimate = classpred_AP)
precision(preds_lr_learn, truth = ltu, estimate = classpred_AP, event_level = "second")
sensitivity(preds_lr_learn, truth = ltu, estimate = classpred_AP, event_level = "second")


## 6.b. Budget policy, based on deciles (LTU if Pr[LTU=Yes] \in Q_10)
qs_lrYES <- quantile(lr_preds_eval$.pred_Yes, probs = seq(0, 1, length = 11))

preds_lr_budget <- lr_preds_test %>%
  mutate(classpred_AP = if_else(.pred_Yes > qs_lrYES[10], "Yes", "No")) %>%
  mutate(classpred_AP = as.factor(classpred_AP))

accuracy(preds_lr_budget, truth = ltu, estimate = classpred_AP)
precision(preds_lr_budget, truth = ltu, estimate = classpred_AP, event_level = "second")
sensitivity(preds_lr_budget, truth = ltu, estimate = classpred_AP, event_level = "second")


### 7. Interpretation

## a) {DALEX}
prof_test3_num <- prof_test3
prof_test3_num$ltunum <- ifelse(prof_test3_num$ltu == "Yes", 1, 0)

lr_explainer <- explain_tidymodels(lr_fit,
                                   data = prof_test3_num,
                                   y = prof_test3_num$ltunum)

tic()
set.seed(1803)
vip_lr <- model_parts(lr_explainer, B = 10, N = 10000)
toc() # 152 secs for N= 1000, 514.5 sec for N= 5,000; 913 secs for N= 10,000

saveRDS(vip_lr, "C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Variable_importance/vip_lr.RDS")


# Plot with DALEX
vip_lr$label <- "LR"

plot(vip_lr, max_vars = 10, bar_width = 5, show_boxplots = T) +
  ggtitle(NULL) +
  labs(subtitle = NULL,
       y = "Loss in (1 - ROC-AUC)") +
  #xlab("Loss in (1 - ROC-AUC)") +
  theme_light() +
  theme(strip.text = element_text(colour = 'black')) +
  guides(colour = "none") # saved 400x350

# Plot with ggplot
vip_lr <- readRDS("C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Variable_importance/vip_lr.RDS")

vip_lr_m <- vip_lr %>%
  group_by(variable) %>%
  summarise(mean_perm = mean(dropout_loss)) %>%
  arrange(desc(mean_perm)) %>%
  slice(2:11) %>%
  #mutate(variable_desc = case_when(variable == "n_un" ~ "")) %>%
  mutate(category = factor(variable, levels = variable[order(mean_perm)]))

vip_lr2 <- left_join(vip_lr_m, vip_lr, by = "variable")

scales::hue_pal()(5) # default ggplot palette
#RColorBrewer::brewer.pal(n=5,"Set1")

ggplot(vip_lr2, aes(x = category, y = dropout_loss)) +
  geom_col(data = vip_lr_m, aes(x = category, y = mean_perm), fill = "#E41A1C", width = 0.5) +
  geom_boxplot(alpha = 0.3, width = 0.5, outlier.shape = NA) +
  scale_y_continuous(breaks = seq(0.23, 0.33, by = 0.02)) +
  theme_light() +
  coord_flip(ylim = c(0.23, 0.33)) +
  labs(y = "Loss in (1 - ROC-AUC)",
       x = "", title = "LR") +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 10),
        axis.title.x = element_text(size = 10)) +
  scale_x_discrete(labels= c("Mean duration of \npast inact. epis.",
                             "Industry \nof last job",
                             "Total duration \nof past ben. epis.",
                             "Last job \nwas part-time",
                             "Occupation \nof last job",
                             "Age",
                             "Total duration \nof past unemp. epis.",
                             "Total duration of past \nunemp. epis. (by age)",
                             "Days since \nlast unemp. episode",
                             "Unemployment \nepisodes in the past")) # inverse order (from less to more), 400 x 350
  



### Testing the model in the RESTRICTIVE test subset -------------
# Read
q22kposthoc_clean <- readRDS("C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Dataset_final/q22kposthoc_cleant.RDS")

# Estimate K prediction
### a. Discrimination
lr_cleantest <- 
  augment(lr_fit, q22kposthoc_clean)

roc_auc(data = lr_cleantest,
        truth = ltu2, .pred_Yes, event_level = "second")

pr_auc(data = lr_cleantest,
       truth = ltu2, .pred_Yes, event_level = "second")

### Plot
lr_roc <- roc_curve(data = lr_cleantest,
                    truth = ltu2, .pred_Yes, event_level = "second")

lr_pr <- pr_curve(data = lr_cleantest,
                  truth = ltu2, .pred_Yes, event_level = "second")

saveRDS(lr_roc, "C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Figures/lr_roc.RDS")
saveRDS(lr_pr, "C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Figures/lr_pr.RDS")

### b. Calibration
round(mean(lr_cleantest$ltu2 == "Yes") / mean(lr_cleantest$.pred_Yes), 3)

observed_test_clean <- ifelse(q22kposthoc_clean$ltu2 == "Yes", 1, 0)
pred_test_clean_lr <- lr_cleantest$.pred_Yes

calib_clean_lr <- pmcalibration(y = observed_test_clean, p = pred_test_clean_lr,
                                smooth = "rcs",
                                transf="none",
                                ci = "sim", method="REML")
calib_clean_lr$metrics

calibp_clean_lr <- get_cc(calib_clean_lr, conf_level = .95)
saveRDS(calibp_clean_lr, "C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Figures/calibp_clean_lr.RDS")


## Policy A
preds_lr_cleanA <- lr_cleantest %>%
  mutate(classpred_AP = if_else(.pred_Yes > lr_max_j_threshold, "Yes", "No")) %>%
  mutate(classpred_AP = as.factor(classpred_AP))

accuracy(preds_lr_cleanA, truth = ltu2, estimate = classpred_AP)
precision(preds_lr_cleanA, truth = ltu2, estimate = classpred_AP, event_level = "second")
sensitivity(preds_lr_cleanA, truth = ltu2, estimate = classpred_AP, event_level = "second")
kap(preds_lr_cleanA, truth = ltu2, estimate = classpred_AP, event_level = "second")
specificity(preds_lr_cleanA, truth = ltu2, estimate = classpred_AP, event_level = "second")

## Policy B
preds_lr_cleanB <- lr_cleantest %>%
  mutate(classpred_AP = if_else(.pred_Yes > qs_lrYES[10], "Yes", "No")) %>%
  mutate(classpred_AP = as.factor(classpred_AP))

accuracy(preds_lr_cleanB, truth = ltu2, estimate = classpred_AP)
precision(preds_lr_cleanB, truth = ltu2, estimate = classpred_AP, event_level = "second")
sensitivity(preds_lr_cleanB, truth = ltu2, estimate = classpred_AP, event_level = "second")
kap(preds_lr_cleanB, truth = ltu2, estimate = classpred_AP, event_level = "second")

### Testing the model in the test subset for subpopulations -------------
preds_lr_learn_young <- preds_lr_learn %>%
  filter(age < 30) # if we filter on the restrictive test subset, we only get 3 observations!

preds_lr_learn_mature <- preds_lr_learn %>%
  filter(age > 45) 

accuracy(preds_lr_learn_mature, truth = ltu, estimate = classpred_AP)
precision(preds_lr_learn_mature, truth = ltu, estimate = classpred_AP, event_level = "second")
sensitivity(preds_lr_learn_mature, truth = ltu, estimate = classpred_AP, event_level = "second")

specificity(preds_lr_learn_mature, truth = ltu, estimate = classpred_AP, event_level = "second")

preds_lr_learn_matureF <- preds_lr_learn %>%
  filter(age > 45 & sex_ti == "DONA")

accuracy(preds_lr_learn_matureF, truth = ltu, estimate = classpred_AP)
precision(preds_lr_learn_matureF, truth = ltu, estimate = classpred_AP, event_level = "second")
sensitivity(preds_lr_learn_matureF, truth = ltu, estimate = classpred_AP, event_level = "second")

specificity(preds_lr_learn_matureF, truth = ltu, estimate = classpred_AP, event_level = "second")

## Penalized logistic regression -------------

### 0. Splitting for cross-validation {rsample}
prof_train3 <- prof_train3 %>%
  arrange(startepisode)

cvindex3 <- sliding_period(data = prof_train3,
                           index = startepisode,
                           period = "year",
                           origin = lubridate::ymd("2017-01-01"))

### 1. Formula & preprocessing {recipe}
plr_rec <- 
  recipe(ltu ~ ., data = prof_train3) %>% 
  update_role(id_ind, id_indepi, yearstart, startepisode, new_role = "ID") %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>%
  step_normalize(all_predictors(), -all_nominal_predictors())

### 2. Specify model {parsnip}
plr_mod <- 
  logistic_reg(penalty = tune(), mixture = tune()) %>% 
  set_engine("glmnet")


### 3. Join {parsnip} and {recipe}
plr_wflow <-
  workflow() %>%
  add_model(plr_mod) %>%
  add_recipe(plr_rec)


### 4. Specifying grid for CV and CV {dials} and {tune}
plr_grid <- tibble(penalty = rep(1/c(0.001, 0.01, 0.1, 1, 10, 100, 1000), 2),
                   mixture = c(0, 0, 0, 0, 0, 0, 0,
                               1, 1, 1, 1, 1, 1, 1))

tic()
plr_cv <- 
  plr_wflow %>% 
  tune_grid(resamples = cvindex3,
            grid = plr_grid,
            control = control_grid(save_pred = TRUE,
                                   verbose = T),
            metrics = metric_set(roc_auc))
toc() # 101.02 secs

plr_cv_plot <- plr_cv %>% 
  collect_metrics() %>% 
  ggplot(aes(x = penalty, y = mean)) + 
  geom_point() + 
  geom_line() + 
  ylab("Area under the ROC Curve") +
  scale_x_log10(labels = scales::label_number())

plr_cv_plot

show_best(plr_cv, metric = "roc_auc")

best_tunings_plr <- plr_cv %>%
  select_best("roc_auc")

final_plr_wf <- 
  plr_wflow %>% 
  finalize_workflow(best_tunings_plr)

### 5. Estimation of regression coefficients with best tuning par.
tic()
final_plr_fit <- 
  final_plr_wf %>%
  last_fit(prof_split3)
toc() # 62 secs

final_plr <- extract_workflow(final_plr_fit)

### 6. Ranking performance in test subset
### a. Discrimination
final_plr_fit %>%
  collect_predictions() %>% 
  roc_curve(ltu, .pred_Yes, event_level = "second") %>% 
  autoplot()

final_plr_fit %>%
  collect_metrics()

final_plr_fit %>% 
  collect_predictions() %>% 
  pr_auc(ltu, .pred_Yes, event_level = "second")

final_plr_fit %>% 
  collect_predictions() %>% 
  brier_class(ltu, .pred_Yes, event_level = "second")

### b. Calibration
plr_preds_test <- 
  augment(final_plr, prof_test) 

round(mean(plr_preds_test$ltu == "Yes") / mean(plr_preds_test$.pred_Yes), 3)

observed_test <- ifelse(plr_preds_test$ltu == "Yes", 1, 0)
pred_test_plr <- plr_preds_test$.pred_Yes

calib_test_plr <- pmcalibration(y = observed_test, p = pred_test_plr,
                                smooth = "rcs",
                                transf="none",
                                ci = "sim", method="REML")
calib_test_plr$metrics

### 7. Classification performance according to different policies
## 7.a. Policy A (learning)
## 1- Learning the optimal cutoff 
plr_preds_eval <- 
  augment(final_plr, prof_eval) 

threshold_plr <- plr_preds_eval %>%
  threshold_perf(truth = ltu, estimate = .pred_Yes,
                 event_level = "second",
                 thresholds = seq(0.01, 0.99, by = 0.0025))

# plot
threshold_plr <- threshold_plr %>%
  filter(.metric != "distance") %>%
  mutate(group = case_when(.metric == "sens" | .metric == "spec" ~ "1",
                           TRUE ~ "2"
  ))

plr_max_j_threshold <- threshold_plr %>%
  filter(.metric == "j_index") %>%
  filter(.estimate == max(.estimate)) %>%
  pull(.threshold)

ggplot(threshold_plr, aes(x = .threshold, y = .estimate, color = .metric, alpha = group)) +
  geom_line() +
  theme_light() +
  scale_color_viridis_d(end = 0.9) +
  scale_alpha_manual(values = c(.4, 1), guide = "none") +
  geom_vline(xintercept = plr_max_j_threshold, alpha = .6, color = "grey30") +
  labs(
    x = "'Good' Threshold\n(above this value is considered 'good')",
    y = "Metric Estimate",
    title = "Balancing performance by varying the threshold",
    subtitle = "Sensitivity or specificity alone might not be enough!\nVertical line = Max J-Index"
  )

## 2- Checking classification performance
preds_plr_learn <- plr_preds_test %>%
  mutate(classpred_AP = if_else(.pred_Yes > plr_max_j_threshold, "Yes", "No")) %>%
  mutate(classpred_AP = as.factor(classpred_AP))

accuracy(preds_plr_learn, truth = ltu, estimate = classpred_AP)
precision(preds_plr_learn, truth = ltu, estimate = classpred_AP, event_level = "second")
sensitivity(preds_plr_learn, truth = ltu, estimate = classpred_AP, event_level = "second")


## 7.b. Alternative policy, based on deciles (LTU if Pr[LTU=Yes] \in Q_10)
qs_plrYES <- quantile(plr_preds_eval$.pred_Yes, probs = seq(0, 1, length = 11))

preds_plr_budget <- plr_preds_test %>%
  mutate(classpred_AP = if_else(.pred_Yes > qs_plrYES[10], "Yes", "No")) %>%
  mutate(classpred_AP = as.factor(classpred_AP))

accuracy(preds_plr_budget, truth = ltu, estimate = classpred_AP)
precision(preds_plr_budget, truth = ltu, estimate = classpred_AP, event_level = "second")
sensitivity(preds_plr_budget, truth = ltu, estimate = classpred_AP, event_level = "second")

### 8. Interpretation

## a) {DALEX}
prof_test3_num <- prof_test3
prof_test3_num$ltunum <- ifelse(prof_test3_num$ltu == "Yes", 1, 0)

plr_explainer <- explain_tidymodels(final_plr,
                                    data = prof_test3_num,
                                    y = prof_test3_num$ltunum)

tic()
set.seed(1804)
vip_plr <- model_parts(plr_explainer, B = 10, N = 10000)
toc() # 792.81 secs for N= 10,000

saveRDS(vip_plr, "C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Variable_importance/vip_plr.RDS")


# Plot with DALEX
vip_plr$label <- "PLR"

plot(vip_plr, max_vars = 10, bar_width = 5, show_boxplots = T) +
  ggtitle(NULL) +
  labs(subtitle = NULL,
       y = "Loss in (1 - ROC-AUC)") +
  #xlab("Loss in (1 - ROC-AUC)") +
  theme_light() +
  theme(strip.text = element_text(colour = 'black')) +
  guides(colour = "none") # saved 400x350

# Plot with ggplot
vip_plr <- readRDS("C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Variable_importance/vip_plr.RDS")

vip_plr_m <- vip_plr %>%
  group_by(variable) %>%
  summarise(mean_perm = mean(dropout_loss)) %>%
  arrange(desc(mean_perm)) %>%
  slice(2:11) %>%
  mutate(category = factor(variable, levels = variable[order(mean_perm)]))

vip_plr2 <- left_join(vip_plr_m, vip_plr, by = "variable")

scales::hue_pal()(5) # default ggplot palette
RColorBrewer::brewer.pal(n=5,"Set1")

ggplot(vip_plr2, aes(x = category, y = dropout_loss)) +
  geom_col(data = vip_plr_m, aes(x = category, y = mean_perm), fill = "#377EB8", width = 0.5) +
  geom_boxplot(alpha = 0.3, width = 0.5, outlier.shape = NA) +
  scale_y_continuous(breaks = seq(0.23, 0.33, by = 0.02)) +
  theme_light() +
  coord_flip(ylim = c(0.23, 0.33)) +
  labs(y = "Loss in (1 - ROC-AUC)",
       x = "", title = "PLR") +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 10),
        axis.title.x = element_text(size = 10)) +
  scale_x_discrete(labels= c("Local \nlabour market",
                             "Mean duration \nof inactivity episodes",
                             "Inactivity \nepisodes in the past",
                             "Industry \nof last job",
                             "Total duration of past \nunemp. epis. (by age)",
                             "Occupation \nof last job",
                             "Last job \nwas part-time",
                             "Age",
                             "Days since \nlast unemp. episode",
                             "Unemployment \nepisodes in the past")) # inverse order (from less to more), 400 x 350



### Testing the model in the RESTRICTIVE test subset -------------
plr_cleantest <- 
  augment(final_plr, q22kposthoc_clean)

# a. Discrimination
roc_auc(data = plr_cleantest,
        truth = ltu2, .pred_Yes, event_level = "second")

pr_auc(data = plr_cleantest,
       truth = ltu2, .pred_Yes, event_level = "second")

brier_class(data = plr_cleantest,
            truth = ltu2, .pred_Yes, event_level = "second")

## Plot
plr_roc <- roc_curve(data = plr_cleantest,
                     truth = ltu2, .pred_Yes, event_level = "second")

plr_pr <- pr_curve(data = plr_cleantest,
                   truth = ltu2, .pred_Yes, event_level = "second")

saveRDS(plr_roc, "C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Figures/plr_roc.RDS")
saveRDS(plr_pr, "C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Figures/plr_pr.RDS")

## Policy A
preds_plr_cleanA <- plr_cleantest %>%
  mutate(classpred_AP = if_else(.pred_Yes > plr_max_j_threshold, "Yes", "No")) %>%
  mutate(classpred_AP = as.factor(classpred_AP))

accuracy(preds_plr_cleanA, truth = ltu2, estimate = classpred_AP)
precision(preds_plr_cleanA, truth = ltu2, estimate = classpred_AP, event_level = "second")
sensitivity(preds_plr_cleanA, truth = ltu2, estimate = classpred_AP, event_level = "second")
kap(preds_plr_cleanA, truth = ltu2, estimate = classpred_AP, event_level = "second")
specificity(preds_plr_cleanA, truth = ltu2, estimate = classpred_AP, event_level = "second")

## Policy B
preds_plr_cleanB <- plr_cleantest %>%
  mutate(classpred_AP = if_else(.pred_Yes > qs_plrYES[10], "Yes", "No")) %>%
  mutate(classpred_AP = as.factor(classpred_AP))

accuracy(preds_plr_cleanB, truth = ltu2, estimate = classpred_AP)
precision(preds_plr_cleanB, truth = ltu2, estimate = classpred_AP, event_level = "second")
sensitivity(preds_plr_cleanB, truth = ltu2, estimate = classpred_AP, event_level = "second")
kap(preds_plr_cleanB, truth = ltu2, estimate = classpred_AP, event_level = "second")


# b. Calibration
round(mean(plr_cleantest$ltu2 == "Yes") / mean(plr_cleantest$.pred_Yes), 3)

observed_test_clean <- ifelse(q22kposthoc_clean$ltu2 == "Yes", 1, 0)
pred_test_clean_plr <- plr_cleantest$.pred_Yes

calib_clean_plr <- pmcalibration(y = observed_test_clean, p = pred_test_clean_plr,
                                 smooth = "rcs",
                                 transf="none",
                                 ci = "sim", method="REML")
calib_clean_plr$metrics

calibp_clean_plr <- get_cc(calib_clean_plr, conf_level = .95)
saveRDS(calibp_clean_plr, "C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Figures/calibp_clean_plr.RDS")

### Testing the model in the test subset for subpopulations -------------
preds_plr_learn_mature <- preds_plr_learn %>%
  filter(age > 45) 

accuracy(preds_plr_learn_mature, truth = ltu, estimate = classpred_AP)
precision(preds_plr_learn_mature, truth = ltu, estimate = classpred_AP, event_level = "second")
sensitivity(preds_plr_learn_mature, truth = ltu, estimate = classpred_AP, event_level = "second")

specificity(preds_plr_learn_mature, truth = ltu, estimate = classpred_AP, event_level = "second")

preds_plr_learn_matureF <- preds_plr_learn %>%
  filter(age > 45 & sex_ti == "DONA")

accuracy(preds_plr_learn_matureF, truth = ltu, estimate = classpred_AP)
precision(preds_plr_learn_matureF, truth = ltu, estimate = classpred_AP, event_level = "second")
sensitivity(preds_plr_learn_matureF, truth = ltu, estimate = classpred_AP, event_level = "second")

specificity(preds_plr_learn_matureF, truth = ltu, estimate = classpred_AP, event_level = "second")


## Random forest --------------
cores <- parallel::detectCores()
cores

### 0. Splitting for cross-validation {rsample}
# Done in PLR section

# 1. Preprocessing
rf_rec <- 
  recipe(ltu ~ ., data = prof_train3) %>% 
  update_role(id_ind, id_indepi, yearstart, startepisode, new_role = "ID")

# 2. Declaring the model
rf_mod <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = tune()) %>% 
  set_engine("ranger") %>% 
  set_mode("classification")

# 3. Declaraing the workflow
rf_workflow <- 
  workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(rf_rec)

# 4. Specifying grid and tuning
rf_grid <- expand_grid(mtry = c(floor(sqrt(ncol(prof_train3) - 5)), floor(log2(ncol(prof_train3) - 5))), # with this formulation mtry isn't considering the outcome (1) and ID variables (4...
                       min_n = c(1, 5, 10),
                       trees = c(500, 750))

set.seed(1997)
tic()
rf_cv <- 
  rf_workflow %>% 
  tune_grid(resamples = cvindex3,
            grid = rf_grid,
            control = control_grid(save_pred = TRUE,
                                   verbose = T,
                                   parallel_over = "everything"),
            metrics = metric_set(roc_auc))
toc() # 3860.81 sec elapsed

best_tunings_rf <- rf_cv %>%
  select_best("roc_auc")

show_best(rf_cv, metric = "roc_auc")

final_rf_workflow <- 
  rf_workflow %>% 
  finalize_workflow(best_tunings_rf)

# 5. Final model fit with best tuning parameters
# the final fit
set.seed(1998)

tic()
final_rf_fit <- 
  final_rf_workflow %>% 
  last_fit(prof_split3)
toc() # 790.48 sec elapsed

final_rf <- extract_workflow(final_rf_fit)

# 6. Ranking performance in test subset
## a. Discrimination
final_rf_fit %>% 
  collect_metrics()

final_rf_fit %>% 
  collect_predictions() %>% 
  pr_auc(ltu, .pred_Yes, event_level = "second")

final_rf_fit %>% 
  collect_predictions() %>% 
  roc_curve(ltu, .pred_Yes, event_level = "second") %>% 
  autoplot()

final_rf_fit %>% 
  collect_predictions() %>% 
  brier_class(ltu, .pred_Yes, event_level = "second")

## b. Calibration
rf_preds_test <- 
  augment(final_rf, prof_test) 

round(mean(rf_preds_test$ltu == "Yes") / mean(rf_preds_test$.pred_Yes), 3)

observed_test <- ifelse(rf_preds_test$ltu == "Yes", 1, 0)
pred_test_clean_rf <- rf_preds_test$.pred_Yes

calib_test_rf <- pmcalibration(y = observed_test, p = pred_test_clean_rf,
                               smooth = "rcs",
                               transf="none",
                               ci = "sim", method="REML")
calib_test_rf$metrics

# 7. Classification performance according to different policies
## 7.a. Policy A (learning)
## 1- Learning the optimal cutoff 
rf_preds_eval <- 
  augment(final_rf, prof_eval) 

threshold_rf <- rf_preds_eval %>%
  threshold_perf(truth = ltu, estimate = .pred_Yes,
                 event_level = "second",
                 thresholds = seq(0.01, 0.99, by = 0.0025))

# plot
threshold_rf <- threshold_rf %>%
  filter(.metric != "distance") %>%
  mutate(group = case_when(.metric == "sens" | .metric == "spec" ~ "1",
                           TRUE ~ "2"
  ))

rf_max_j_threshold <- threshold_rf %>%
  filter(.metric == "j_index") %>%
  filter(.estimate == max(.estimate)) %>%
  pull(.threshold)

ggplot(threshold_rf, aes(x = .threshold, y = .estimate, color = .metric, alpha = group)) +
  geom_line() +
  theme_light() +
  scale_color_viridis_d(end = 0.9) +
  scale_alpha_manual(values = c(.4, 1), guide = "none") +
  geom_vline(xintercept = rf_max_j_threshold, alpha = .6, color = "grey30") +
  labs(
    x = "'Good' Threshold\n(above this value is considered 'good')",
    y = "Metric Estimate",
    title = "Balancing performance by varying the threshold",
    subtitle = "Sensitivity or specificity alone might not be enough!\nVertical line = Max J-Index"
  )

## 2- Checking classification performance
preds_rf_learn <- rf_preds_test %>%
  mutate(classpred_AP = if_else(.pred_Yes > rf_max_j_threshold, "Yes", "No")) %>%
  mutate(classpred_AP = as.factor(classpred_AP))

accuracy(preds_rf_learn, truth = ltu, estimate = classpred_AP)
precision(preds_rf_learn, truth = ltu, estimate = classpred_AP, event_level = "second")
sensitivity(preds_rf_learn, truth = ltu, estimate = classpred_AP, event_level = "second")

## 7.b. Alternative policy, based on deciles (LTU if Pr[LTU=Yes] \in Q_10)
qs_rfYES <- quantile(rf_preds_eval$.pred_Yes, probs = seq(0, 1, length = 11))

preds_rf_budget <- rf_preds_test %>%
  mutate(classpred_AP = if_else(.pred_Yes > qs_rfYES[10], "Yes", "No")) %>%
  mutate(classpred_AP = as.factor(classpred_AP))

accuracy(preds_rf_budget, truth = ltu, estimate = classpred_AP)
precision(preds_rf_budget, truth = ltu, estimate = classpred_AP, event_level = "second")
sensitivity(preds_rf_budget, truth = ltu, estimate = classpred_AP, event_level = "second")


### 8. Interpretation

## a) {DALEX}
prof_test3_num <- prof_test3
prof_test3_num$ltunum <- ifelse(prof_test3_num$ltu == "Yes", 1, 0)

rf_explainer <- explain_tidymodels(final_rf,
                                   data = prof_test3_num,
                                   y = prof_test3_num$ltunum)

tic()
set.seed(1805)
vip_rf <- model_parts(rf_explainer, B = 10, N = 10000)
toc() # 32086.98 for N= 10,000

saveRDS(vip_rf, "C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Variable_importance/vip_rf.RDS")


# Plot with DALEX
vip_rf$label <- "RF"

plot(vip_rf, max_vars = 10, bar_width = 5, show_boxplots = T) +
  ggtitle(NULL) +
  labs(subtitle = NULL,
       y = "Loss in (1 - ROC-AUC)") +
  #xlab("Loss in (1 - ROC-AUC)") +
  theme_light() +
  theme(strip.text = element_text(colour = 'black')) +
  guides(colour = "none") # saved 400x350


# Plot with ggplot
vip_rf <- readRDS("C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Variable_importance/vip_rf.RDS")

vip_rf_m <- vip_rf %>%
  group_by(variable) %>%
  summarise(mean_perm = mean(dropout_loss)) %>%
  arrange(desc(mean_perm)) %>%
  slice(2:11) %>%
  mutate(category = factor(variable, levels = variable[order(mean_perm)]))

vip_rf2 <- left_join(vip_rf_m, vip_rf, by = "variable")

scales::hue_pal()(5) # default ggplot palette
RColorBrewer::brewer.pal(n=5,"Set1")

ggplot(vip_rf2, aes(x = category, y = dropout_loss)) +
  geom_col(data = vip_rf_m, aes(x = category, y = mean_perm), fill = "#4DAF4A", width = 0.5) +
  geom_boxplot(alpha = 0.3, width = 0.5, outlier.shape = NA) +
  scale_y_continuous(breaks = seq(0.23, 0.33, by = 0.02)) +
  theme_light() +
  coord_flip(ylim = c(0.23, 0.33)) +
  labs(y = "Loss in (1 - ROC-AUC)",
       x = "", title = "RF") +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 10),
        axis.title.x = element_text(size = 10)) +
  scale_x_discrete(labels= c("Missing info. \non unemp. episodes",
                             "Industry of last job",
                             "Employment episodes \nof temporary contract",
                             "Employment episodes \nof general training",
                             "Employment \nepisodes in the past",
                             "Mean duration \nof unemp. episodes",
                             "Age",
                             "Days since \nlast emp. episode",
                             "Unemployment \nepisodes in the past",
                             "Days since \nlast unemp. episode")) # inverse order (from less to more), 400 x 350




### Testing the model in the RESTRICTIVE test subset -------------
# Estimate K prediction
rf_cleantest <- 
  augment(final_rf, q22kposthoc_clean)

#### a. Discrimination
roc_auc(data = rf_cleantest,
        truth = ltu2, .pred_Yes, event_level = "second")

pr_auc(data = rf_cleantest,
       truth = ltu2, .pred_Yes, event_level = "second")

brier_class(data = rf_cleantest,
            truth = ltu2, .pred_Yes, event_level = "second")

## Plot
rf_roc <- roc_curve(data = rf_cleantest,
                    truth = ltu2, .pred_Yes, event_level = "second")

rf_pr <- pr_curve(data = rf_cleantest,
                  truth = ltu2, .pred_Yes, event_level = "second")

saveRDS(rf_roc, "C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Figures/rf_roc.RDS")
saveRDS(rf_pr, "C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Figures/rf_pr.RDS")



## Policy A
preds_rf_cleanA <- rf_cleantest %>%
  mutate(classpred_AP = if_else(.pred_Yes > rf_max_j_threshold, "Yes", "No")) %>%
  mutate(classpred_AP = as.factor(classpred_AP))

accuracy(preds_rf_cleanA, truth = ltu2, estimate = classpred_AP)
precision(preds_rf_cleanA, truth = ltu2, estimate = classpred_AP, event_level = "second")
sensitivity(preds_rf_cleanA, truth = ltu2, estimate = classpred_AP, event_level = "second")
kap(preds_rf_cleanA, truth = ltu2, estimate = classpred_AP, event_level = "second")
specificity(preds_rf_cleanA, truth = ltu2, estimate = classpred_AP, event_level = "second")

## Policy B
preds_rf_cleanB <- rf_cleantest %>%
  mutate(classpred_AP = if_else(.pred_Yes > qs_rfYES[10], "Yes", "No")) %>%
  mutate(classpred_AP = as.factor(classpred_AP))

accuracy(preds_rf_cleanB, truth = ltu2, estimate = classpred_AP)
precision(preds_rf_cleanB, truth = ltu2, estimate = classpred_AP, event_level = "second")
sensitivity(preds_rf_cleanB, truth = ltu2, estimate = classpred_AP, event_level = "second")
kap(preds_rf_cleanB, truth = ltu2, estimate = classpred_AP, event_level = "second")


#### b. Calibration
round(mean(rf_cleantest$ltu2 == "Yes") / mean(rf_cleantest$.pred_Yes), 3)

pred_test_clean_rf <- rf_cleantest$.pred_Yes

calib_clean_rf <- pmcalibration(y = observed_test_clean, p = pred_test_clean_rf,
                                smooth = "rcs",
                                transf="none",
                                ci = "sim", method="REML")
calib_clean_rf$metrics

calibp_clean_rf <- get_cc(calib_clean_rf, conf_level = .95)
saveRDS(calibp_clean_rf, "C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Figures/calibp_clean_rf.RDS")


### Testing the model in the test subset for subpopulations -------------
preds_rf_learn_mature <- preds_rf_learn %>%
  filter(age > 45)

accuracy(preds_rf_learn_mature, truth = ltu, estimate = classpred_AP)
precision(preds_rf_learn_mature, truth = ltu, estimate = classpred_AP, event_level = "second")
sensitivity(preds_rf_learn_mature, truth = ltu, estimate = classpred_AP, event_level = "second")
specificity(preds_rf_learn_mature, truth = ltu, estimate = classpred_AP, event_level = "second")

preds_rf_learn_matureF <- preds_rf_learn %>%
  filter(age > 45 & sex_ti == "DONA")

accuracy(preds_rf_learn_matureF, truth = ltu, estimate = classpred_AP)
precision(preds_rf_learn_matureF, truth = ltu, estimate = classpred_AP, event_level = "second")
sensitivity(preds_rf_learn_matureF, truth = ltu, estimate = classpred_AP, event_level = "second")
specificity(preds_rf_learn_matureF, truth = ltu, estimate = classpred_AP, event_level = "second")


## Gradient boosting -------------

### 0. Splitting for cross-validation {rsample}
# Done in PLR section

### 1. Preprocessing
gbm_rec <- 
  recipe(ltu ~ ., data = prof_train3) %>% 
  update_role(id_ind, id_indepi, yearstart, startepisode, new_role = "ID") %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors())

### 2. Declaring the model
gbm_mod <- 
  boost_tree(tree_depth = tune(), mtry = tune(), trees = tune(),
             learn_rate = tune(), sample_size = tune()) %>% 
  set_engine("xgboost", event_level = "second") %>% 
  set_mode("classification")

### 3. Declaraing the workflow
gbm_workflow <- 
  workflow() %>% 
  add_model(gbm_mod) %>% 
  add_recipe(gbm_rec)

### 4. Specifying grid and tuning
gbm_grid <- expand_grid(mtry = c(floor(sqrt(ncol(prof_train3) - 5)), floor(log2(ncol(prof_train3) - 5))), # with this formulation mtry isn't considering the outcome (1) and ID variables (4...
                        tree_depth = c(3, 5, 7),
                        trees = c(250, 500, 750),
                        learn_rate = c(0.01, 0.025, 0.05),
                        sample_size = c(0.6, 0.8))

set.seed(1999)
tic()
gbm_cv <- 
  gbm_workflow %>% 
  tune_grid(resamples = cvindex3,
            grid = gbm_grid,
            control = control_grid(save_pred = TRUE,
                                   verbose = T,
                                   parallel_over = "everything"),
            metrics = metric_set(roc_auc))
toc() # 3392 sec elapsed

best_tunings_gbm <- gbm_cv %>%
  select_best("roc_auc")

show_best(gbm_cv, metric = "roc_auc")

final_gbm_workflow <- 
  gbm_workflow %>% 
  finalize_workflow(best_tunings_gbm)

### 5. Final model fit with best tuning parameters
# the final fit
set.seed(1999)

tic()
final_gbm_fit <- 
  final_gbm_workflow %>% 
  last_fit(prof_split3)
toc() # 103.19 sec elapsed

final_gbm <- extract_workflow(final_gbm_fit)

# 6. Ranking performance in test subset
gbm_preds_test <- 
  augment(final_gbm, prof_test) 

## a. Discrimination
final_gbm_fit %>% 
  collect_metrics()

final_gbm_fit %>% 
  collect_predictions() %>% 
  pr_auc(ltu, .pred_Yes, event_level = "second")

final_gbm_fit %>% 
  collect_predictions() %>% 
  roc_curve(ltu, .pred_Yes, event_level = "second") %>% 
  autoplot()

final_gbm_fit %>% 
  collect_predictions() %>%
  brier_class(ltu, .pred_Yes, event_level = "second")

## b. Calibration
round(mean(gbm_preds_test$ltu == "Yes") / mean(gbm_preds_test$.pred_Yes), 3)

observed_test <- ifelse(gbm_preds_test$ltu == "Yes", 1, 0)
pred_test_gbm <- gbm_preds_test$.pred_Yes

calib_test_gbm <- pmcalibration(y = observed_test, p = pred_test_gbm,
                                smooth = "rcs",
                                transf="none",
                                ci = "sim", method="REML")
calib_test_gbm$metrics

# 7. Classification performance according to different policies
## 7.a. Policy A (learning)
## 1- Learning the optimal cutoff 
gbm_preds_eval <- 
  augment(final_gbm, prof_eval) 

threshold_gbm <- gbm_preds_eval %>%
  threshold_perf(truth = ltu, estimate = .pred_Yes,
                 event_level = "second",
                 thresholds = seq(0.01, 0.99, by = 0.0025))

# plot
threshold_gbm <- threshold_gbm %>%
  filter(.metric != "distance") %>%
  mutate(group = case_when(.metric == "sens" | .metric == "spec" ~ "1",
                           TRUE ~ "2"
  ))

gbm_max_j_threshold <- threshold_gbm %>%
  filter(.metric == "j_index") %>%
  filter(.estimate == max(.estimate)) %>%
  pull(.threshold)

ggplot(threshold_gbm, aes(x = .threshold, y = .estimate, color = .metric, alpha = group)) +
  geom_line() +
  theme_light() +
  scale_color_viridis_d(end = 0.9) +
  scale_alpha_manual(values = c(.4, 1), guide = "none") +
  geom_vline(xintercept = gbm_max_j_threshold, alpha = .6, color = "grey30") +
  labs(
    x = "'Good' Threshold\n(above this value is considered 'good')",
    y = "Metric Estimate",
    title = "Balancing performance by varying the threshold",
    subtitle = "Sensitivity or specificity alone might not be enough!\nVertical line = Max J-Index"
  )

## 2- Checking classification performance
preds_gbm_learn <- gbm_preds_test %>%
  mutate(classpred_AP = if_else(.pred_Yes > gbm_max_j_threshold, "Yes", "No")) %>%
  mutate(classpred_AP = as.factor(classpred_AP))

accuracy(preds_gbm_learn, truth = ltu, estimate = classpred_AP)
precision(preds_gbm_learn, truth = ltu, estimate = classpred_AP, event_level = "second")
sensitivity(preds_gbm_learn, truth = ltu, estimate = classpred_AP, event_level = "second")


## 7.b. Alternative policy, based on deciles (LTU if Pr[LTU=Yes] \in Q_10)
qs_gbmYES <- quantile(gbm_preds_eval$.pred_Yes, probs = seq(0, 1, length = 11))

preds_gbm_budget <- gbm_preds_test %>%
  mutate(classpred_AP = if_else(.pred_Yes > qs_gbmYES[10], "Yes", "No")) %>%
  mutate(classpred_AP = as.factor(classpred_AP))

accuracy(preds_gbm_budget, truth = ltu, estimate = classpred_AP)
precision(preds_gbm_budget, truth = ltu, estimate = classpred_AP, event_level = "second")
sensitivity(preds_gbm_budget, truth = ltu, estimate = classpred_AP, event_level = "second")


# 8. Interpretation

## a) {DALEX}
prof_test3_num <- prof_test3
prof_test3_num$ltunum <- ifelse(prof_test3_num$ltu == "Yes", 1, 0)

gbm_explainer <- explain_tidymodels(final_gbm,
                                    data = prof_test3_num,
                                    y = prof_test3_num$ltunum)

tic()
set.seed(1806)
vip_gbm <- model_parts(gbm_explainer, B = 10, N = 10000)
toc() # 1006 for N= 10,000

saveRDS(vip_gbm, "C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Variable_importance/vip_gbm.RDS")


# Plot with DALEX
vip_gbm$label <- "GB"

plot(vip_gbm, max_vars = 10, bar_width = 5, show_boxplots = T) +
  ggtitle(NULL) +
  labs(subtitle = NULL,
       y = "Loss in (1 - ROC-AUC)") +
  theme_light() +
  theme(strip.text = element_text(colour = 'black')) +
  guides(colour = "none") # saved 400x350

# Plot with ggplot
vip_gbm <- readRDS("C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Variable_importance/vip_gbm.RDS")

vip_gbm_m <- vip_gbm %>%
  group_by(variable) %>%
  summarise(mean_perm = mean(dropout_loss)) %>%
  arrange(desc(mean_perm)) %>%
  slice(2:11) %>%
  mutate(category = factor(variable, levels = variable[order(mean_perm)]))

vip_gbm2 <- left_join(vip_gbm_m, vip_gbm, by = "variable")

ggplot(vip_gbm2, aes(x = category, y = dropout_loss)) +
  geom_col(data = vip_gbm_m, aes(x = category, y = mean_perm), fill = "#984EA3", width = 0.5) +
  geom_boxplot(alpha = 0.3, width = 0.5, outlier.shape = NA) +
  scale_y_continuous(breaks = seq(0.23, 0.33, by = 0.02)) +
  theme_light() +
  coord_flip(ylim = c(0.23, 0.33)) +
  labs(y = "Loss in (1 - ROC-AUC)",
       x = "", title = "GB") +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 10),
        axis.title.x = element_text(size = 10)) +
  scale_x_discrete(labels= c("Employment \nepisodes in the past",
                             "Benefit \n episodes in the past",
                             "Industry of last job",
                             "Last job was part-time",
                             "Occupation of last job",
                             "Mean duration \nof unemp. episodes",
                             "Days since \nlast emp. episode",
                             "Age",
                             "Unemployment \nepisodes in the past",
                             "Days since \nlast unemp. episode")) # inverse order (from less to more), 400 x 350






### Testing the model in the RESTRICTIVE test subset -------------
gbm_cleantest <- 
  augment(final_gbm, q22kposthoc_clean)

# a. Discrimination
roc_auc(data = gbm_cleantest,
        truth = ltu2, .pred_Yes, event_level = "second")

pr_auc(data = gbm_cleantest,
       truth = ltu2, .pred_Yes, event_level = "second")

brier_class(data = gbm_cleantest,
            truth = ltu2, .pred_Yes, event_level = "second")

## Plot
gbm_roc <- roc_curve(data = gbm_cleantest,
                     truth = ltu2, .pred_Yes, event_level = "second")

gbm_pr <- pr_curve(data = gbm_cleantest,
                   truth = ltu2, .pred_Yes, event_level = "second")

saveRDS(gbm_roc, "C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Figures/gbm_roc.RDS")
saveRDS(gbm_pr, "C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Figures/gbm_pr.RDS")


## Policy A
preds_gbm_cleanA <- gbm_cleantest %>%
  mutate(classpred_AP = if_else(.pred_Yes > gbm_max_j_threshold, "Yes", "No")) %>%
  mutate(classpred_AP = as.factor(classpred_AP))

accuracy(preds_gbm_cleanA, truth = ltu2, estimate = classpred_AP)
precision(preds_gbm_cleanA, truth = ltu2, estimate = classpred_AP, event_level = "second")
sensitivity(preds_gbm_cleanA, truth = ltu2, estimate = classpred_AP, event_level = "second")
kap(preds_gbm_cleanA, truth = ltu2, estimate = classpred_AP, event_level = "second")
specificity(preds_gbm_cleanA, truth = ltu2, estimate = classpred_AP, event_level = "second")

## Policy B
preds_gbm_cleanB <- gbm_cleantest %>%
  mutate(classpred_AP = if_else(.pred_Yes > qs_gbmYES[10], "Yes", "No")) %>%
  mutate(classpred_AP = as.factor(classpred_AP))

accuracy(preds_gbm_cleanB, truth = ltu2, estimate = classpred_AP)
precision(preds_gbm_cleanB, truth = ltu2, estimate = classpred_AP, event_level = "second")
sensitivity(preds_gbm_cleanB, truth = ltu2, estimate = classpred_AP, event_level = "second")
kap(preds_gbm_cleanB, truth = ltu2, estimate = classpred_AP, event_level = "second")

# b. Calibration
round(mean(gbm_cleantest$ltu2 == "Yes") / mean(gbm_cleantest$.pred_Yes), 3)

observed_test_clean <- ifelse(q22kposthoc_clean$ltu2 == "Yes", 1, 0)
pred_test_clean_gbm <- gbm_cleantest$.pred_Yes

calib_clean_gbm <- pmcalibration(y = observed_test_clean, p = pred_test_clean_gbm,
                                 smooth = "rcs",
                                 transf="none",
                                 ci = "sim", method="REML")
calib_clean_gbm$metrics

calibp_clean_gbm <- get_cc(calib_clean_gbm, conf_level = .95)
saveRDS(calibp_clean_gbm, "C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Figures/calibp_clean_gbm.RDS")

ggplot(calibp_clean_gbm, aes(x = p, y = p_c, ymin=lower, ymax=upper)) +
  geom_abline(intercept = 0, slope = 1, lty=2) +
  geom_line() +
  geom_ribbon(alpha = 1/4) +
  lims(x=c(0,1), y=c(0,1)) +
  theme_light()

### Testing the model in the test subset for subpopulations -------------
preds_gbm_learn_mature <- preds_gbm_learn %>%
  filter(age > 45)

accuracy(preds_gbm_learn_mature, truth = ltu, estimate = classpred_AP)
precision(preds_gbm_learn_mature, truth = ltu, estimate = classpred_AP, event_level = "second")
sensitivity(preds_gbm_learn_mature, truth = ltu, estimate = classpred_AP, event_level = "second")

specificity(preds_gbm_learn_mature, truth = ltu, estimate = classpred_AP, event_level = "second")

preds_gbm_learn_matureF <- preds_gbm_learn %>%
  filter(age > 45 & sex_ti == "DONA")

accuracy(preds_gbm_learn_matureF, truth = ltu, estimate = classpred_AP)
precision(preds_gbm_learn_matureF, truth = ltu, estimate = classpred_AP, event_level = "second")
sensitivity(preds_gbm_learn_matureF, truth = ltu, estimate = classpred_AP, event_level = "second")

specificity(preds_gbm_learn_matureF, truth = ltu, estimate = classpred_AP, event_level = "second")


## Q models ----------------
### Testing the model in the RESTRICTIVE test subset -------------
q22kposthoc_clean$index_es_r <- 139 - q22kposthoc_clean$index_es

q22kposthoc_clean$index_es_rprob <- q22kposthoc_clean$index_es_r / 139

#### a. Discrimination ------------
### Ranking performance
roc_auc(data = q22kposthoc_clean,
        truth = ltu2, index_es_r, event_level = "second")

pr_auc(data = q22kposthoc_clean,
       truth = ltu2, index_es_r, event_level = "second")

qs_roc <- roc_curve(data = q22kposthoc_clean,
                    truth = ltu2, index_es_r, event_level = "second")

qs_pr <- pr_curve(data = q22kposthoc_clean,
                  truth = ltu2, index_es_r, event_level = "second")

roc_qs <- roc_curve(data = q22kposthoc_clean,
                    truth = ltu2, index_es_r, event_level = "second") %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_path() +
  geom_abline(lty = 3) +
  coord_equal() +
  theme_light()

pr_qs <- pr_curve(data = q22kposthoc_clean,
                  truth = ltu2, index_es_r, event_level = "second") %>%
  ggplot(aes(x = recall, y = precision)) +
  labs(x = "sensitivity") +
  geom_path() +
  coord_equal() +
  theme_light()

saveRDS(qs_roc, "C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Figures/qs_roc.RDS")
saveRDS(qs_pr, "C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Figures/qs_pr.RDS")



#
roc_auc(data = q22kposthoc_clean,
        truth = ltu2, index_es, event_level = "second")

pr_auc(data = q22kposthoc_clean,
       truth = ltu2, index_es, event_level = "second")

## Classification performance
## Q-S (based on the score)
### Q-S25
q22kposthoc_clean$QSpred <- if_else(q22kposthoc_clean$index_es <= (139 - (139/4*3)),
                                    "Yes", "No")

q22kposthoc_clean$QSpred <- as.factor(q22kposthoc_clean$QSpred)

accuracy(data = q22kposthoc_clean, truth = ltu2, estimate = QSpred)
precision(data = q22kposthoc_clean, truth = ltu2, estimate = QSpred, event_level = "second")
sensitivity(data = q22kposthoc_clean, truth = ltu2, estimate = QSpred, event_level = "second")
kap(data = q22kposthoc_clean, truth = ltu2, estimate = QSpred, event_level = "second")
specificity(data = q22kposthoc_clean, truth = ltu2, estimate = QSpred, event_level = "second")

### Q-S50
q22kposthoc_clean$QS50pred <- if_else(q22kposthoc_clean$index_es <= (139 - (139/4*2)),
                                      "Yes", "No")

q22kposthoc_clean$QS50pred <- as.factor(q22kposthoc_clean$QS50pred)

accuracy(data = q22kposthoc_clean, truth = ltu2, estimate = QS50pred)
precision(data = q22kposthoc_clean, truth = ltu2, estimate = QS50pred, event_level = "second")
sensitivity(data = q22kposthoc_clean, truth = ltu2, estimate = QS50pred, event_level = "second")
kap(data = q22kposthoc_clean, truth = ltu2, estimate = QS50pred, event_level = "second")
specificity(data = q22kposthoc_clean, truth = ltu2, estimate = QS50pred, event_level = "second")

## Q-G (based on the group)
q22kposthoc_clean$Qpred <- if_else(q22kposthoc_clean$qgroup %in% c("C", "D"),
                                   "Yes", "No")
q22kposthoc_clean$Qpred <- as.factor(q22kposthoc_clean$Qpred)

accuracy(data = q22kposthoc_clean, truth = ltu2, estimate = Qpred)
precision(data = q22kposthoc_clean, truth = ltu2, estimate = Qpred, event_level = "second")
sensitivity(data = q22kposthoc_clean, truth = ltu2, estimate = Qpred, event_level = "second")
kap(data = q22kposthoc_clean, truth = ltu2, estimate = Qpred, event_level = "second")
specificity(data = q22kposthoc_clean, truth = ltu2, estimate = Qpred, event_level = "second")


#### b. Calibration -----------
round(mean(q22kposthoc_clean$ltu2 == "Yes") / mean(q22kposthoc_clean$index_es_rprob), 3) # mean calibration

observed_test_clean <- ifelse(q22kposthoc_clean$ltu2 == "Yes", 1, 0)
pred_test_clean_qs <- q22kposthoc_clean$index_es_rprob

calib_clean_qs <- pmcalibration(y = observed_test_clean, p = pred_test_clean_qs,
                                smooth = "rcs",
                                transf="none",
                                ci = "sim", method="REML")
calib_clean_qs$metrics

calibp_clean_qs <- get_cc(calib_clean_qs, conf_level = .95)
saveRDS(calibp_clean_qs, "C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Figures/calibp_clean_qs.RDS")


## Episodes with events predicted by each model ------------
### Q models --------------
### Predicted episodes by Q-S25
epis_predicted_QS25 <- data.frame(ypred = q22kposthoc_clean$QSpred,
                                  yobs = q22kposthoc_clean$ltu2,
                                  index_epi = 1:length(q22kposthoc_clean$QSpred))

predicted_events_QS25 <- epis_predicted_QS25 %>%
  filter(ypred == "Yes") %>%
  select(index_epi)

saveRDS(predicted_events_QS25, "C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Predicted_episodes_march/predicted_events_QS25.RDS")


predicted_true_events_QS25 <- epis_predicted_QS25 %>%
  filter(ypred == "Yes" & yobs == "Yes") %>%
  select(index_epi)

saveRDS(predicted_true_events_QS25, "C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Predicted_episodes_march/predicted_true_events_QS25.RDS")


### Predicted episodes by Q-S50
epis_predicted_QS50 <- data.frame(ypred = q22kposthoc_clean$QS50pred,
                                  yobs = q22kposthoc_clean$ltu2,
                                  index_epi = 1:length(q22kposthoc_clean$QS50pred))

predicted_events_QS50 <- epis_predicted_QS50 %>%
  filter(ypred == "Yes") %>%
  select(index_epi)

saveRDS(predicted_events_QS50, "C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Predicted_episodes_march/predicted_events_QS50.RDS")


predicted_true_events_QS50 <- epis_predicted_QS50 %>%
  filter(ypred == "Yes" & yobs == "Yes") %>%
  select(index_epi)

saveRDS(predicted_true_events_QS50, "C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Predicted_episodes_march/predicted_true_events_QS50.RDS")

### Predicted episodes by Q-G
epis_predicted_QG <- data.frame(ypred = q22kposthoc_clean$Qpred,
                                yobs = q22kposthoc_clean$ltu2,
                                index_epi = 1:length(q22kposthoc_clean$Qpred))

predicted_events_QG <- epis_predicted_QG %>%
  filter(ypred == "Yes") %>%
  select(index_epi)

saveRDS(predicted_events_QG, "C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Predicted_episodes_march/predicted_events_QG.RDS")


predicted_true_events_QG <- epis_predicted_QG %>%
  filter(ypred == "Yes" & yobs == "Yes") %>%
  select(index_epi)

saveRDS(predicted_true_events_QG, "C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Predicted_episodes_march/predicted_true_events_QG.RDS")


### Policy A ------------------
### Predicted episodes by LR
epis_Apredicted_lr <- data.frame(ypred = preds_lr_cleanA$classpred_AP,
                                 yobs = preds_lr_cleanA$ltu2,
                                 index_epi = 1:length(preds_lr_cleanA$classpred_AP))

Apredicted_events_lr <- epis_Apredicted_lr %>%
  filter(ypred == "Yes") %>%
  select(index_epi)

saveRDS(Apredicted_events_lr, "C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Predicted_episodes_march/Apredicted_events_lr.RDS")


Apredicted_true_events_lr <- epis_Apredicted_lr %>%
  filter(ypred == "Yes" & yobs == "Yes") %>%
  select(index_epi)

saveRDS(Apredicted_true_events_lr, "C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Predicted_episodes_march/Apredicted_true_events_lr.RDS")


### Predicted episodes by PLR
epis_Apredicted_plr <- data.frame(ypred = preds_plr_cleanA$classpred_AP,
                                  yobs = preds_plr_cleanA$ltu2,
                                  index_epi = 1:length(preds_plr_cleanA$classpred_AP))

Apredicted_events_plr <- epis_Apredicted_plr %>%
  filter(ypred == "Yes") %>%
  select(index_epi)

saveRDS(Apredicted_events_plr, "C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Predicted_episodes_march/Apredicted_events_plr.RDS")


Apredicted_true_events_plr <- epis_Apredicted_plr %>%
  filter(ypred == "Yes" & yobs == "Yes") %>%
  select(index_epi)

saveRDS(Apredicted_true_events_plr, "C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Predicted_episodes_march/Apredicted_true_events_plr.RDS")

### Predicted episodes by RF
epis_Apredicted_rf <- data.frame(ypred = preds_rf_cleanA$classpred_AP,
                                 yobs = preds_rf_cleanA$ltu2,
                                 index_epi = 1:length(preds_rf_cleanA$classpred_AP))

Apredicted_events_rf <- epis_Apredicted_rf %>%
  filter(ypred == "Yes") %>%
  select(index_epi)

saveRDS(Apredicted_events_rf, "C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Predicted_episodes_march/Apredicted_events_rf.RDS")


Apredicted_true_events_rf <- epis_Apredicted_rf %>%
  filter(ypred == "Yes" & yobs == "Yes") %>%
  select(index_epi)

saveRDS(Apredicted_true_events_rf, "C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Predicted_episodes_march/Apredicted_true_events_rf.RDS")

### Predicted episodes by GB
epis_Apredicted_gbm <- data.frame(ypred = preds_gbm_cleanA$classpred_AP,
                                  yobs = preds_gbm_cleanA$ltu2,
                                  index_epi = 1:length(preds_gbm_cleanA$classpred_AP))

Apredicted_events_gbm <- epis_Apredicted_gbm %>%
  filter(ypred == "Yes") %>%
  select(index_epi)

saveRDS(Apredicted_events_gbm, "C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Predicted_episodes_march/Apredicted_events_gbm.RDS")


Apredicted_true_events_gbm <- epis_Apredicted_gbm %>%
  filter(ypred == "Yes" & yobs == "Yes") %>%
  select(index_epi)

saveRDS(Apredicted_true_events_gbm, "C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Predicted_episodes_march/Apredicted_true_events_gbm.RDS")


### Policy B --------

### Predicted episodes by LR
epis_Bpredicted_lr <- data.frame(ypred = preds_lr_cleanB$classpred_AP,
                                 yobs = preds_lr_cleanB$ltu2,
                                 index_epi = 1:length(preds_lr_cleanB$classpred_AP))

Bpredicted_events_lr <- epis_Bpredicted_lr %>%
  filter(ypred == "Yes") %>%
  select(index_epi)

saveRDS(Bpredicted_events_lr, "C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Predicted_episodes_march/Bpredicted_events_lr.RDS")


Bpredicted_true_events_lr <- epis_Bpredicted_lr %>%
  filter(ypred == "Yes" & yobs == "Yes") %>%
  select(index_epi)

saveRDS(Bpredicted_true_events_lr, "C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Predicted_episodes_march/Bpredicted_true_events_lr.RDS")

### Predicted episodes by PLR
epis_Bpredicted_plr <- data.frame(ypred = preds_plr_cleanB$classpred_AP,
                                  yobs = preds_plr_cleanA$ltu2,
                                  index_epi = 1:length(preds_plr_cleanB$classpred_AP))

Bpredicted_events_plr <- epis_Bpredicted_plr %>%
  filter(ypred == "Yes") %>%
  select(index_epi)

saveRDS(Bpredicted_events_plr, "C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Predicted_episodes_march/Bpredicted_events_plr.RDS")


Bpredicted_true_events_plr <- epis_Bpredicted_plr %>%
  filter(ypred == "Yes" & yobs == "Yes") %>%
  select(index_epi)

saveRDS(Bpredicted_true_events_plr, "C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Predicted_episodes_march/Bpredicted_true_events_plr.RDS")


### Predicted episodes by RF
epis_Bpredicted_rf <- data.frame(ypred = preds_rf_cleanB$classpred_AP,
                                 yobs = preds_rf_cleanB$ltu2,
                                 index_epi = 1:length(preds_rf_cleanB$classpred_AP))

Bpredicted_events_rf <- epis_Bpredicted_rf %>%
  filter(ypred == "Yes") %>%
  select(index_epi)

saveRDS(Bpredicted_events_rf, "C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Predicted_episodes_march/Bpredicted_events_rf.RDS")


Bpredicted_true_events_rf <- epis_Bpredicted_rf %>%
  filter(ypred == "Yes" & yobs == "Yes") %>%
  select(index_epi)

saveRDS(Bpredicted_true_events_rf, "C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Predicted_episodes_march/Bpredicted_true_events_rf.RDS")


### Predicted episodes by GB
epis_Bpredicted_gbm <- data.frame(ypred = preds_gbm_cleanB$classpred_AP,
                                  yobs = preds_gbm_cleanB$ltu2,
                                  index_epi = 1:length(preds_gbm_cleanB$classpred_AP))

Bpredicted_events_gbm <- epis_Bpredicted_gbm %>%
  filter(ypred == "Yes") %>%
  select(index_epi)

saveRDS(Bpredicted_events_gbm, "C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Predicted_episodes_march/Bpredicted_events_gbm.RDS")


Bpredicted_true_events_gbm <- epis_Bpredicted_gbm %>%
  filter(ypred == "Yes" & yobs == "Yes") %>%
  select(index_epi)

saveRDS(Bpredicted_true_events_gbm, "C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Predicted_episodes_march/Bpredicted_true_events_gbm.RDS")


## Comparison of models through Kappa coefficient -------------
predicted_events_QS25 <- readRDS("C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Predicted_episodes_march/predicted_events_QS25.RDS")
predicted_events_QS50 <- readRDS("C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Predicted_episodes_march/predicted_events_QS50.RDS")
predicted_events_QG <- readRDS("C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Predicted_episodes_march/predicted_events_QG.RDS")

Apredicted_events_lr <- readRDS("C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Predicted_episodes_march/Apredicted_events_lr.RDS")
Apredicted_events_plr <- readRDS("C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Predicted_episodes_march/Apredicted_events_plr.RDS")
Apredicted_events_rf <- readRDS("C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Predicted_episodes_march/Apredicted_events_rf.RDS")
Apredicted_events_gbm <- readRDS("C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Predicted_episodes_march/Apredicted_events_gbm.RDS")

Bpredicted_events_lr <- readRDS("C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Predicted_episodes_march/Bpredicted_events_lr.RDS")
Bpredicted_events_plr <- readRDS("C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Predicted_episodes_march/Bpredicted_events_plr.RDS")
Bpredicted_events_rf <- readRDS("C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Predicted_episodes_march/Bpredicted_events_rf.RDS")
Bpredicted_events_gbm <- readRDS("C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Predicted_episodes_march/Bpredicted_events_gbm.RDS")

### QS25
predicted_events_QS25$pred <- 1

negatives_QS25 <- data.frame(index_epi = setdiff(1:11082, predicted_events_QS25$index_epi),
                             pred = 0)

predictions_QS25 <- bind_rows(predicted_events_QS25, negatives_QS25) %>%
  arrange(index_epi)

### QS50
predicted_events_QS50$pred <- 1

negatives_QS50 <- data.frame(index_epi = setdiff(1:11082, predicted_events_QS50$index_epi),
                             pred = 0)

predictions_QS50 <- bind_rows(predicted_events_QS50, negatives_QS50) %>%
  arrange(index_epi)

### QG
predicted_events_QG$pred <- 1

negatives_QG <- data.frame(index_epi = setdiff(1:11082, predicted_events_QG$index_epi),
                           pred = 0)

predictions_QG <- bind_rows(predicted_events_QG, negatives_QG) %>%
  arrange(index_epi)

### LR-A
Apredicted_events_lr$pred <- 1
negatives_LR <- data.frame(index_epi = setdiff(1:11082, Apredicted_events_lr$index_epi),
                           pred = 0)

predictions_LRA <- bind_rows(Apredicted_events_lr, negatives_LR) %>%
  arrange(index_epi)


### PLR-A
Apredicted_events_plr$pred <- 1
negatives_PLR <- data.frame(index_epi = setdiff(1:11082, Apredicted_events_plr$index_epi),
                            pred = 0)

predictions_PLRA <- bind_rows(Apredicted_events_plr, negatives_PLR) %>%
  arrange(index_epi)


### RF-A
Apredicted_events_rf$pred <- 1

negatives_RFA <- data.frame(index_epi = setdiff(1:11082, Apredicted_events_rf$index_epi),
                            pred = 0)

predictions_RFA <- bind_rows(Apredicted_events_rf, negatives_RFA) %>%
  arrange(index_epi)


### GB-A
Apredicted_events_gbm$pred <- 1

negatives_GBMA <- data.frame(index_epi = setdiff(1:11082, Apredicted_events_gbm$index_epi),
                             pred = 0)

predictions_GBMA <- bind_rows(Apredicted_events_gbm, negatives_GBMA) %>%
  arrange(index_epi)


### LR-B
Bpredicted_events_lr$pred <- 1
negativesB_LR <- data.frame(index_epi = setdiff(1:11082, Bpredicted_events_lr$index_epi),
                            pred = 0)

predictions_LRB <- bind_rows(Bpredicted_events_lr, negativesB_LR) %>%
  arrange(index_epi)


### PLR-B
Bpredicted_events_plr$pred <- 1
negativesB_PLR <- data.frame(index_epi = setdiff(1:11082, Bpredicted_events_plr$index_epi),
                             pred = 0)

predictions_PLRB <- bind_rows(Bpredicted_events_plr, negativesB_PLR) %>%
  arrange(index_epi)


### RF-B
Bpredicted_events_rf$pred <- 1

negativesB_RFB <- data.frame(index_epi = setdiff(1:11082, Bpredicted_events_rf$index_epi),
                             pred = 0)

predictions_RFB <- bind_rows(Bpredicted_events_rf, negativesB_RFB) %>%
  arrange(index_epi)


### GB-B
Bpredicted_events_gbm$pred <- 1

negativesB_GBMB <- data.frame(index_epi = setdiff(1:11082, Bpredicted_events_gbm$index_epi),
                              pred = 0)

predictions_GBMB <- bind_rows(Bpredicted_events_gbm, negativesB_GBMB) %>%
  arrange(index_epi)


### KAPPA with tidymodels
predictions_QS25$predF <- as.factor(predictions_QS25$pred)
predictions_QS50$predF <- as.factor(predictions_QS50$pred)
predictions_QG$predF <- as.factor(predictions_QG$pred)

predictions_LRA$predF <- as.factor(predictions_LRA$pred)
predictions_PLRA$predF <- as.factor(predictions_PLRA$pred)
predictions_RFA$predF <- as.factor(predictions_RFA$pred)
predictions_GBMA$predF <- as.factor(predictions_GBMA$pred)

predictions_LRB$predF <- as.factor(predictions_LRB$pred)
predictions_PLRB$predF <- as.factor(predictions_PLRB$pred)
predictions_RFB$predF <- as.factor(predictions_RFB$pred)
predictions_GBMB$predF <- as.factor(predictions_GBMB$pred)

kap_vec(predictions_RFA$predF, predictions_QS50$predF)

# Estimation
# Column 1 (Q-S25)
kappa_s50_s25 <-  kap_vec(predictions_QS50$predF, predictions_QS25$predF)
kappa_g_s25 <-  kap_vec(predictions_QG$predF, predictions_QS25$predF)

kappa_lrA_s25 <-  kap_vec(predictions_LRA$predF, predictions_QS25$predF)
kappa_plrA_s25 <-  kap_vec(predictions_PLRA$predF, predictions_QS25$predF)
kappa_rfA_s25 <-  kap_vec(predictions_RFA$predF, predictions_QS25$predF)
kappa_gbmA_s25 <-  kap_vec(predictions_GBMA$predF, predictions_QS25$predF)

kappa_lrB_s25 <-  kap_vec(predictions_LRB$predF, predictions_QS25$predF)
kappa_plrB_s25 <-  kap_vec(predictions_PLRB$predF, predictions_QS25$predF)
kappa_rfB_s25 <-  kap_vec(predictions_RFB$predF, predictions_QS25$predF)
kappa_gbmB_s25 <-  kap_vec(predictions_GBMB$predF, predictions_QS25$predF)

# Column 2 (Q-S50)
kappa_g_s50 <-  kap_vec(predictions_QG$predF, predictions_QS50$predF)

kappa_lrA_s50 <-  kap_vec(predictions_LRA$predF, predictions_QS50$predF)
kappa_plrA_s50 <-  kap_vec(predictions_PLRA$predF, predictions_QS50$predF)
kappa_rfA_s50 <-  kap_vec(predictions_RFA$predF, predictions_QS50$predF)
kappa_gbmA_s50 <-  kap_vec(predictions_GBMA$predF, predictions_QS50$predF)

kappa_lrB_s50 <-  kap_vec(predictions_LRB$predF, predictions_QS50$predF)
kappa_plrB_s50 <-  kap_vec(predictions_PLRB$predF, predictions_QS50$predF)
kappa_rfB_s50 <-  kap_vec(predictions_RFB$predF, predictions_QS50$predF)
kappa_gbmB_s50 <-  kap_vec(predictions_GBMB$predF, predictions_QS50$predF)

# Column 3 (Q-G)
kappa_lrA_g <-  kap_vec(predictions_LRA$predF, predictions_QG$predF)
kappa_plrA_g <-  kap_vec(predictions_PLRA$predF, predictions_QG$predF)
kappa_rfA_g <-  kap_vec(predictions_RFA$predF, predictions_QG$predF)
kappa_gbmA_g <-  kap_vec(predictions_GBMA$predF, predictions_QG$predF)

kappa_lrB_g <-  kap_vec(predictions_LRB$predF, predictions_QG$predF)
kappa_plrB_g <-  kap_vec(predictions_PLRB$predF, predictions_QG$predF)
kappa_rfB_g <-  kap_vec(predictions_RFB$predF, predictions_QG$predF)
kappa_gbmB_g <-  kap_vec(predictions_GBMB$predF, predictions_QG$predF)

# Column 4 (LR)
kappa_plrA_lrA <-  kap_vec(predictions_PLRA$predF, predictions_LRA$predF)
kappa_rfA_lrA <-  kap_vec(predictions_RFA$predF, predictions_LRA$predF)
kappa_gbmA_lrA <-  kap_vec(predictions_GBMA$predF, predictions_LRA$predF)

kappa_plrB_lrB <-  kap_vec(predictions_PLRB$predF, predictions_LRB$predF)
kappa_rfB_lrB <-  kap_vec(predictions_RFB$predF, predictions_LRB$predF)
kappa_gbmB_lrB <-  kap_vec(predictions_GBMB$predF, predictions_LRB$predF)

# Column 5 (PLR)
kappa_rfA_plrA <-  kap_vec(predictions_RFA$predF, predictions_PLRA$predF)
kappa_gbmA_plrA <-  kap_vec(predictions_GBMA$predF, predictions_PLRA$predF)

kappa_rfB_plrB <-  kap_vec(predictions_RFB$predF, predictions_PLRB$predF)
kappa_gbmB_plrB <-  kap_vec(predictions_GBMB$predF, predictions_PLRB$predF)

# Column 6 (PLR)
kappa_gbmA_rfA <-  kap_vec(predictions_GBMA$predF, predictions_RFA$predF)

kappa_gbmB_rfB <-  kap_vec(predictions_GBMB$predF, predictions_RFB$predF)

# Table
table_kappamodels <- data.frame(model = c("Q-S25", "Q-S50", "Q-G", "LR", "PLR", "RF", "GBM", "LR", "PLR", "RF", "GBM"),
                                `Q-S25` = c(1, round(kappa_s50_s25, 3), round(kappa_g_s25, 3),
                                            round(kappa_lrA_s25, 3), round(kappa_plrA_s25, 3), round(kappa_rfA_s25, 3), round(kappa_gbmA_s25, 3),
                                            round(kappa_lrB_s25, 3), round(kappa_plrB_s25, 3), round(kappa_rfB_s25, 3), round(kappa_gbmB_s25, 3)),
                                `Q-S50` = c(NA, 1, round(kappa_g_s50, 3),
                                            round(kappa_lrA_s50, 3), round(kappa_plrA_s50, 3), round(kappa_rfA_s50, 3), round(kappa_gbmA_s50, 3),
                                            round(kappa_lrB_s50, 3), round(kappa_plrB_s50, 3), round(kappa_rfB_s50, 3), round(kappa_gbmB_s50, 3)),
                                `Q-G` = c(NA, NA, 1,
                                          round(kappa_lrA_g, 3), round(kappa_plrA_g, 3), round(kappa_rfA_g, 3), round(kappa_gbmA_g, 3),
                                          round(kappa_lrB_g, 3), round(kappa_plrB_g, 3), round(kappa_rfB_g, 3), round(kappa_gbmB_g, 3)),
                                LR = c(NA, NA, NA,
                                       1, round(kappa_plrA_lrA, 3), round(kappa_rfA_lrA, 3), round(kappa_gbmA_lrA, 3),
                                       1, round(kappa_plrB_lrB, 3), round(kappa_rfB_lrB, 3), round(kappa_gbmB_lrB, 3)),
                                PLR = c(NA, NA, NA,
                                        NA, 1, round(kappa_rfA_plrA, 3), round(kappa_gbmA_plrA, 3),
                                        NA, 1, round(kappa_rfB_plrB, 3), round(kappa_gbmB_plrB, 3)),
                                RF = c(NA, NA, NA,
                                       NA, NA, 1, round(kappa_gbmA_rfA, 3),
                                       NA, NA, 1, round(kappa_gbmB_rfB, 3)),
                                GBM = c(NA, NA, NA,
                                        NA, NA, NA, 1,
                                        NA, NA, NA, 1))

openxlsx::write.xlsx(table_kappamodels, 'C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Other_tables/table_kappamodels.xlsx')



# b. manual
cobs <- mean(predictions_RFA$pred == predictions_QS50$pred)
cexp <- mean(predictions_RFA$pred) * mean(predictions_QS50$pred) +
  (1 - mean(predictions_RFA$pred)) * (1 - mean(predictions_QS50$pred))

kappa <- (cobs - cexp) / (1 - cexp) # equal than with tidymodels

# 2. Calibration curves for all models -----------
calibp_clean_qs <- readRDS("C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Figures/calibp_clean_qs.RDS")
calibp_clean_lr <- readRDS("C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Figures/calibp_clean_lr.RDS")
calibp_clean_plr <- readRDS("C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Figures/calibp_clean_plr.RDS")
calibp_clean_rf <- readRDS("C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Figures/calibp_clean_rf.RDS")
calibp_clean_gbm <- readRDS("C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/Figures/calibp_clean_gbm.RDS")

calibp_clean_qs$Model <- "Q-S"
calibp_clean_lr$Model <- "LR"
calibp_clean_plr$Model <- "PLR"
calibp_clean_rf$Model <- "RF"
calibp_clean_gbm$Model <- "GB"

calibp_clean_all <- dplyr::bind_rows(list(calibp_clean_qs, calibp_clean_lr, calibp_clean_plr,
                                          calibp_clean_rf, calibp_clean_gbm))

calibp_clean_all$Model <- factor(calibp_clean_all$Model,
                                 levels = unique(calibp_clean_all$Model))

ggplot(calibp_clean_all, aes(x = p, y = p_c, ymin=lower, ymax=upper, colour = Model)) +
  geom_abline(intercept = 0, slope = 1, lty=2) +
  geom_line() +
  scale_color_manual(values = c("#FF7F00", "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3")) +
  #scale_color_brewer(palette = "Set1") +
  geom_ribbon(alpha = 1/10, linetype = "blank") +
  lims(x=c(0,1), y=c(0,1)) +
  theme_light() +
  labs(x = "Predicted probability of LTU",
       y = "Observed proportion of LTU") # saved 400 x 300

RColorBrewer::brewer.pal(n=5,"Set1")

# 3. Comparing test and restrictive test subsets ---------------
## This section is run just after generating prof_test3 and loading q22kposthoc_clean

## Test subset ---------
test_predictors <- prof_test3 %>%
  select(-c(ltu:startepisode, yearstart)) # yearstart!

predictors_test_mm <- model.matrix(~ 0 + .,
                                   test_predictors)

meansprops_test <- predictors_test_mm %>%
  as.data.frame() %>%
  summarise(across(.cols = everything(),
                   .fns = mean)) %>%
  pivot_longer(cols = everything(),
               names_to = "predictor",
               values_to = "meanpropTEST")

sd_test <- prof_test3 %>%
  select(-c(ltu:startepisode)) %>%
  summarise(across(where(is.numeric), .fns = sd)) %>%
  pivot_longer(cols = everything(),
               names_to = "predictor",
               values_to = "sdTEST")

## Restrictive test subset ------------
clean_predictors <- q22kposthoc_clean %>%
  select(-c(id_ind:startepisode, model:Qpred, yearstart)) #yearstart!

predictors_clean_mm <- model.matrix(~ 0 + .,
                                    clean_predictors)

meansprops_clean <- predictors_clean_mm %>%
  as.data.frame() %>%
  summarise(across(.cols = everything(),
                   .fns = mean)) %>%
  pivot_longer(cols = everything(),
               names_to = "predictor",
               values_to = "meanpropCLEAN")

sd_clean <- q22kposthoc_clean %>%
  select(-c(id_ind:startepisode, model:Qpred)) %>%
  summarise(across(where(is.numeric), .fns = sd)) %>%
  pivot_longer(cols = everything(),
               names_to = "predictor",
               values_to = "sdCLEAN")

## Binding -----------
TESTvsCLEAN <- meansprops_test %>%
  left_join(., meansprops_clean, by = "predictor") %>%
  left_join(., sd_test, by = "predictor") %>%
  left_join(., sd_clean, by = "predictor") %>%
  mutate(difference = case_when(is.na(sdCLEAN) ~ meanpropTEST - meanpropCLEAN, # is a factor (a category)
                                TRUE ~ (meanpropTEST / sdTEST) - (meanpropCLEAN / sdTEST))) %>% 
  arrange(desc(abs(difference))) %>%
  mutate(dif_round = round(difference, 3))

TESTvsCLEAN_quanti <- TESTvsCLEAN %>%
  filter(!is.na(sdTEST))

TESTvsCLEAN_quali <- TESTvsCLEAN %>%
  filter(is.na(sdTEST))

TESTvsCLEAN_quanti$predictor <- factor(TESTvsCLEAN_quanti$predictor,
                                       levels = rev(unique(TESTvsCLEAN_quanti$predictor)))

TESTvsCLEAN_quanti$sign <- if_else(TESTvsCLEAN_quanti$difference > 0, "Positive", "Negative")
TESTvsCLEAN_quanti$sign <- as.factor(TESTvsCLEAN_quanti$sign)

TESTvsCLEAN_quanti %>%
  slice(1:10) %>%
  ggplot(aes(x = predictor, y = abs(dif_round), fill = sign)) +
  geom_bar(stat = "identity", show.legend = F) +
  coord_flip() +  # Esto gira el gráfico horizontalmente
  labs(title = NULL,
       x = "Quantitative variable",
       y = "Abs. difference in standardized means") +
  theme_light() +
  scale_fill_manual(values = c("Positive" = "#1f78b4",
                               "Negative" = "#e41a1c")) # saved 400x350


TESTvsCLEAN_quali$predictor <- factor(TESTvsCLEAN_quali$predictor,
                                      levels = rev(unique(TESTvsCLEAN_quali$predictor)))

TESTvsCLEAN_quali$sign <- if_else(TESTvsCLEAN_quali$difference > 0, "Positive", "Negative")
TESTvsCLEAN_quali$sign <- as.factor(TESTvsCLEAN_quali$sign)

TESTvsCLEAN_quali %>%
  filter(!str_ends(predictor, "Missing") &
           !str_starts(predictor, "MIndicator")) %>%
  slice(1:10) %>%
  ggplot(aes(x = predictor, y = abs(dif_round), fill = sign)) +
  geom_bar(stat = "identity", show.legend = F) +
  coord_flip() +  # Esto gira el gráfico horizontalmente
  labs(title = NULL,
       x = "Category",
       y = "Abs. difference in proportions") +
  theme_light() +
  scale_fill_manual(values = c("Positive" = "#1f78b4",
                               "Negative" = "#e41a1c")) # saved 400x350

# 4. More interpretation: global surrogate model ------------
#saveRDS(rf_preds_test, "C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/predictions_test_RF.RDS")

rf_preds_test <- readRDS("C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/2_profiling_Cat/Results/predictions_test_RF.RDS")

### 0. Data splitting
set.seed(1988)
rfpreds_split <- initial_split(rf_preds_test, prop = 8/10)

rfpreds_train <- training(rfpreds_split)
rfpreds_test <- testing(rfpreds_split)

### 1. Formula & preprocessing {recipe}
dec_rec <- 
  recipe(.pred_Yes ~ ., data = rfpreds_split) %>% 
  update_role(ltu, .pred_class, .pred_No, new_role = "other_outcomes") %>%
  update_role(id_ind, id_indepi, yearstart, startepisode, new_role = "ID") %>% # maybe including startepisode as predictor? see https://www.tidymodels.org/start/recipes/#fit-workflow
  step_zv(all_predictors()) # zv = zero variance. Removes columns that have only 1 value (rare categories)

### 2. Specify model {parsnip}
dec_mod <- 
  decision_tree(cost_complexity = 0.005) %>%
  set_mode("regression") %>%
  set_engine("rpart")

### 3. Join {parsnip} and {recipe}
dec_wflow <-
  workflow() %>%
  add_model(dec_mod) %>%
  add_recipe(dec_rec)

### 4. Estimation of coefficients
set.seed(2024)

dec_fit <- 
  dec_wflow %>% 
  fit(data = rfpreds_train)

### 5. Performance
### ...in training subset
dec_preds_train <- 
  augment(dec_fit, rfpreds_train) 

dec_preds_train %>% 
  rsq(truth = .pred_Yes, estimate = .pred)

### ...in test subset
dec_preds_test <- 
  augment(dec_fit, rfpreds_test) 

dec_preds_test %>% 
  rsq(truth = .pred_Yes, estimate = .pred)

### 6. Plotting
tree_fit_rpart <- extract_fit_engine(dec_fit)

library(rpart.plot)
rpart.plot(tree_fit_rpart)
