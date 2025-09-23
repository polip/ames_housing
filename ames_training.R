library(tidyverse)
library(vip)
library(tidymodels)
library(ranger)


train <- read_csv("input/train.csv")

### sale price log transform
train <- train %>% mutate(SalePrice=log10(SalePrice))

### rf recipe

rf_recipe <- recipe(SalePrice ~ ., data = train) %>%
  update_role(Id,new_role = "id variable") %>%
  
  # Convert specified numeric columns to character
  step_mutate(across( c("MSSubClass", "MoSold", "YrSold"), as.character)) %>%
  
  # Handle new levels in nominal predictors that might appear in test set
  step_novel(all_nominal_predictors()) %>%
  
  # Replace NA with "None" for specific categorical features
  # This is more accurate than mode imputation for these variables
  step_unknown(c("Alley", "BsmtQual", "BsmtCond", "BsmtExposure", "BsmtFinType1",
                  "BsmtFinType2", "FireplaceQu", "GarageType", "GarageFinish",
                  "GarageQual", "GarageCond", "PoolQC", "Fence", "MiscFeature"), new_level = "None") %>%
  # Impute remaining numeric predictors with knn
  step_impute_knn(all_numeric_predictors()) %>%
  # Catch-all for any other NA values in categorical columns
  step_unknown(all_nominal_predictors(), new_level = "unknown") %>%
  # Remove zero-variance predictors
  step_zv(all_predictors())
 
####model
rf_model <- rand_forest(mode = "regression") %>% set_engine("ranger")

### rf workflow
rf_wflow <- 
  workflow() %>% 
  add_model(rf_model) %>%
  add_recipe(rf_recipe)

### resampling
set.seed(1001)
train_folds <- vfold_cv(train, v = 10, repeats = 5)

rf_res <- rf_wflow %>% 
  fit_resamples(train_folds,
              control = control_resamples(
                save_pred = TRUE, save_workflow = TRUE))

### rf metrics
rf_res |> collect_metrics(summarize = F)

### fit final model
rf_final_fit <- fit(rf_wflow, train)

### save vetiver versioned model
library(plumber)
library(vetiver)
library(pins)
library(googleCloudStorageR)

vet_model <- vetiver_model(rf_final_fit, "ames_rf", save_prototype=train%>%select(-SalePrice)%>%head(1))

### local board
board_local <- board_folder(path = "board",versioned = TRUE)
vet_model <- vetiver_pin_read(board = board_local, name = "ames_rf", "20250919T053824Z-34a6b")
vetiver_pin_write(board_local, vet_model)

## google cloud storage
gcs_auth(json_file = "ames-housing-472418-0f31c1a0322f.json")
gcs_list_buckets("ames-housing-472418")

### google cloud board
board_gcs <- board_gcs("ames-r-model", versioned = T)
vetiver_pin_write(board_gcs,vet_model)


### create plumber and docker file
vetiver_write_plumber(name="ames_rf", file = "plumber.R", board=board_gcs)
vetiver_write_docker(vet_model, plumber_file = "plumber.R",base_image = glue::glue("FROM rocker/r-ver:{getRversion()}"))


#### test predictions kaggle submission file
test <- read_csv("input/test.csv")

### rf test
rf_test_pred <- predict(rf_final_fit, test)

### kaggle submission file
rf_submission <- bind_cols(test %>% select(Id), rf_test_pred) %>% 
  mutate(SalePrice=10^.pred) %>% 
  select(-.pred)
rf_submission %>% write_csv("output/rf_submission.csv")


 
  