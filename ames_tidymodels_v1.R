library(tidyverse)
library(DataExplorer)
library(skimr)
library(tidymodels)

train <- read_csv("input/train.csv")

train %>% glimpse()
skim_train <- skim_without_charts(train)

plot_missing(train)
plot_bar(train)

train %>% 
  group_by(OverallQual) %>% 
  plot_histogram()

create_report(train)

### sale price plot
train %>% ggplot(aes(SalePrice)) + geom_histogram()
train <- train %>% mutate(SalePrice=log10(SalePrice))

#### other transformation 
train <- train %>% mutate(TotalBsmtSF=if_else(BsmtExposure=='NoBasement', 0,TotalBsmtSF))
train <- train %>% mutate(MoSold=as.character(MoSold))
train <- train %>% mutate(YrSold=as.character(YrSold))
train <- train %>% mutate(MSSubClass=as.character(MSSubClass))
train %>% count(YearBuilt)
train %>% ggplot(aes(YearBuilt)) + geom_bar()
train %>% ggplot(aes(YearBuilt,SalePrice)) + geom_point() + geom_smooth()


### rf recipe
rf_recipe <- recipe(SalePrice ~ ., data=train) %>% 
  update_role(Id,new_role = "id variable") %>%
  step_novel(all_nominal_predictors()) %>% 
  step_unknown(all_nominal_predictors()) %>% 
  step_impute_knn(all_numeric_predictors()) %>% 
  step_zv(all_predictors()) 
 
rf_prep_data <- prep(x = rf_recipe, training=train)

plot_bar(rf_bake_train)

####model
rf_model <- rand_forest(mode = 'regression') %>% set_engine("ranger")



### rf workflow
rf_wflow <- 
  workflow() %>% 
  add_model(rf_model) %>% 
  add_recipe(rf_recipe)

### resampling

set.seed(1001)
train_folds <- bootstraps(train, times = 10)

keep_pred <- control_resamples(save_pred = TRUE, save_workflow = TRUE)


rf_res <- 
  rf_wflow %>% 
  fit_resamples(train_folds,control = keep_pred)


rf_res_best_fit <- fit_best(rf_res)



#### test

test <- read_csv("input/test.csv")

#### other transformation 
test <- test %>% mutate(TotalBsmtSF=if_else(BsmtExposure=='NoBasement', 0,TotalBsmtSF))
test <- test %>% mutate(MoSold=as.character(MoSold))
test <- test %>% mutate(YrSold=as.character(YrSold))
test <- test %>% mutate(MSSubClass=as.character(MSSubClass))




### rf test
rf_test_pred <- predict(rf_res_best_fit, test)

rf_submission <- bind_cols(test %>% select(Id), rf_test_pred) %>% 
  mutate(SalePrice=10^.pred) %>% 
  select(-.pred)
rf_submission %>% write_csv('rf_submission.csv')


 
  