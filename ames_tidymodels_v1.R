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

train %>% count(YearBuilt)
train %>% ggplot(aes(YearBuilt)) + geom_bar()
train %>% ggplot(aes(YearBuilt,SalePrice)) + geom_point() + geom_smooth()

### rf recipe
cols_to_none <- c("Alley", "BsmtQual", "BsmtCond", "BsmtExposure", "BsmtFinType1",
                  "BsmtFinType2", "FireplaceQu", "GarageType", "GarageFinish",
                  "GarageQual", "GarageCond", "PoolQC", "Fence", "MiscFeature")

cols_to_char <- c("MSSubClass", "MoSold", "YrSold")

rf_recipe <- recipe(SalePrice ~ ., data = train) %>%
  update_role(Id,new_role = "id variable") %>%
  # Convert specified numeric columns to character
  step_mutate(across(all_of(cols_to_char), as.character)) %>%
  # Handle new levels in nominal predictors that might appear in test set
  step_novel(all_nominal_predictors()) %>%
  # Replace NA with "None" for specific categorical features
  # This is more accurate than mode imputation for these variables
  step_unknown(all_of(cols_to_none), new_level = "None") %>%
  # Impute remaining numeric predictors with knn
  step_impute_knn(all_numeric_predictors()) %>%
  # Catch-all for any other NA values in categorical columns
  step_unknown(all_nominal_predictors(), new_level = "unknown") %>%
  # Remove zero-variance predictors
  step_zv(all_predictors())
 
####model
rf_model <- rand_forest(mode = 'regression') %>% set_engine("ranger")

### rf workflow
rf_wflow <- 
  workflow() %>% 
  add_model(rf_model) %>% 
  add_recipe(rf_recipe)

### resampling
set.seed(1001)
train_folds <- vfold_cv(train, v = 10,repeats = 5)

keep_pred <- control_resamples(save_pred = TRUE, save_workflow = TRUE)


rf_res <- 
  rf_wflow %>% 
  fit_resamples(train_folds,control = keep_pred)
rf_res |> collect_metrics(summarize = F)
### fit final model
rf_final_fit <- fit(rf_wflow, train)

#### test
test <- read_csv("input/test.csv")

### rf test
rf_test_pred <- predict(rf_final_fit, test)

rf_submission <- bind_cols(test %>% select(Id), rf_test_pred) %>% 
  mutate(SalePrice=10^.pred) %>% 
  select(-.pred)
rf_submission %>% write_csv('rf_submission.csv')


 
  