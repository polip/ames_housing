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

#### missing categorical values
train <- train %>% mutate(PoolQC=if_else(is.na(PoolQC), 'NoPool',PoolQC))
train <- train %>% mutate(GarageType=if_else(is.na(GarageType), 'NoGarage',GarageType))
train <- train %>% mutate(GarageFinish=if_else(is.na(GarageFinish), 'NoGarage',GarageFinish))
train <- train %>% mutate(GarageQual=if_else(is.na(GarageQual), 'NoGarage',GarageQual))
train <- train %>% mutate(Fence=if_else(is.na(Fence), 'NoFence',Fence))
train <- train %>% mutate(BsmtQual=if_else(is.na(BsmtQual), 'NoBasement',BsmtQual))
train <- train %>% mutate(BsmtCond=if_else(is.na(BsmtCond), 'NoBasement',BsmtCond))
train <- train %>% mutate(BsmtExposure=if_else(is.na(BsmtExposure), 'NoBasement',BsmtExposure))
train <- train %>% mutate(BsmtFinType1=if_else(is.na(BsmtFinType1), 'NoBasement',BsmtFinType1))
train <- train %>% mutate(MiscFeature=if_else(is.na(MiscFeature), 'NoMiscFeature',MiscFeature))
train <- train %>% mutate(Alley=if_else(is.na(Alley), 'NoAlley',Alley))
train <- train %>% mutate(FireplaceQu=if_else(is.na(FireplaceQu), 'NoFireplaceQu',FireplaceQu))

#### other transformation 
train <- train %>% mutate(TotalBsmtSF=if_else(BsmtExposure=='NoBasement', 0,TotalBsmtSF))
train <- train %>% mutate(MoSold=as.character(MoSold))
train <- train %>% mutate(YrSold=as.character(YrSold))
train <- train %>% mutate(MSSubClass=as.character(MSSubClass))
train %>% count(YearBuilt)
train %>% ggplot(aes(YearBuilt)) + geom_bar()
train %>% ggplot(aes(YearBuilt,SalePrice)) + geom_point() + geom_smooth()

### lm recipe
lm_recipe <- recipe(SalePrice ~ ., data=train) %>% 
  update_role(Id,new_role = "id variable") %>%
  step_novel(all_nominal_predictors()) %>% 
  step_unknown(all_nominal_predictors(),new_level = 'No') %>% 
  step_cut(starts_with('Year'),breaks = c(1900,1940,1980)) %>% 
  step_dummy(all_nominal_predictors()) %>%
  step_impute_median(all_numeric_predictors())%>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_BoxCox(all_numeric_predictors()) %>% 
  step_pca(all_numeric_predictors())  
lm_prep_data <- prep(x = lm_recipe, training=train)

lm_bake_train <- bake(lm_prep_data, new_data = NULL)
lm_bake_train %>% count(YearBuilt)

### rf recipe
rf_recipe <- recipe(SalePrice ~ ., data=train) %>% 
  update_role(Id,new_role = "id variable") %>%
  step_novel(all_nominal_predictors()) %>% 
  step_unknown(all_nominal_predictors()) %>% 
  step_impute_knn(all_numeric_predictors()) %>% 
  step_zv(all_predictors()) 
 
rf_prep_data <- prep(x = rf_recipe, training=train)
rf_bake_train <- bake(rf_prep_data, new_data = NULL)

plot_bar(rf_bake_train)

####model
lm_model <- linear_reg() %>% set_engine("lm")
rf_model <- rand_forest(mode = 'regression') %>% set_engine("ranger")

### quick random forest
rf_train_data<- rf_bake_train %>% select(-Id) %>% janitor::clean_names()
quick_rf <- ranger(sale_price ~ ., data = rf_train_data, importance = "impurity")
tidy(quick_rf$variable.importance)

### lm workflow
lm_wflow <- 
  workflow() %>% 
  add_model(lm_model) %>% 
  add_recipe(lm_recipe)

### rf workflow
rf_wflow <- 
  workflow() %>% 
  add_model(rf_model) %>% 
  add_recipe(rf_recipe)

### resampling

set.seed(1001)
train_folds <- vfold_cv(train, v = 10,repeats = 5)

keep_pred <- control_resamples(save_pred = TRUE, save_workflow = TRUE)

lm_res <- 
  lm_wflow %>% 
  fit_resamples(resamples = train_folds, control = keep_pred)

rf_res <- 
  rf_wflow %>% 
  fit_resamples(train_folds,control = keep_pred)

### fit
lm_fit <- fit(lm_wflow, train)

lm_fit %>% 
  tidy()

rf_res_best_fit <- fit_best(rf_res)
rf_res_best_fit %>% 
  rmse()


#### test

test <- read_csv("ames_housing prices/house-prices-advanced-regression-techniques/test.csv")

test <- test %>% mutate(PoolQC=if_else(is.na(PoolQC), 'NoPool',PoolQC))
test <- test %>% mutate(GarageType=if_else(is.na(GarageType), 'NoGarage',GarageType))
test <- test %>% mutate(GarageFinish=if_else(is.na(GarageFinish), 'NoGarage',GarageFinish))
test <- test %>% mutate(GarageQual=if_else(is.na(GarageQual), 'NoGarage',GarageQual))
test <- test %>% mutate(Fence=if_else(is.na(Fence), 'NoFence',Fence))
test <- test %>% mutate(BsmtQual=if_else(is.na(BsmtQual), 'NoBasement',BsmtQual))
test <- test %>% mutate(BsmtCond=if_else(is.na(BsmtCond), 'NoBasement',BsmtCond))
test <- test %>% mutate(BsmtExposure=if_else(is.na(BsmtExposure), 'NoBasement',BsmtExposure))
test <- test %>% mutate(BsmtFinType1=if_else(is.na(BsmtFinType1), 'NoBasement',BsmtFinType1))
test <- test %>% mutate(MiscFeature=if_else(is.na(MiscFeature), 'NoMiscFeature',MiscFeature))
test <- test %>% mutate(Alley=if_else(is.na(Alley), 'NoAlley',Alley))
test <- test %>% mutate(FireplaceQu=if_else(is.na(FireplaceQu), 'NoFireplaceQu',FireplaceQu))

#### other transformation 
test <- test %>% mutate(TotalBsmtSF=if_else(BsmtExposure=='NoBasement', 0,TotalBsmtSF))
test <- test %>% mutate(MoSold=as.character(MoSold))
test <- test %>% mutate(YrSold=as.character(YrSold))
test <- test %>% mutate(MSSubClass=as.character(MSSubClass))


### lm test
lm_test_pred <- predict(lm_fit_res, test)

lm_submission <- bind_cols(test %>% select(Id), lm_test_pred) %>% 
  mutate(SalePrice=10^.pred) %>% 
  select(-.pred)
lm_submission %>% write_csv('lm_submission.csv')

### rf test
rf_test_pred <- predict(rf_res_best_fit, test)

rf_submission <- bind_cols(test %>% select(Id), rf_test_pred) %>% 
  mutate(SalePrice=10^.pred) %>% 
  select(-.pred)
rf_submission %>% write_csv('rf_submission.csv')


 
  