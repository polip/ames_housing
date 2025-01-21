library(tidyverse)
library(DataExplorer)
library(caret)
library(skimr)
library(summarytools) 
library(recipes)

train <- read_csv("input/train.csv")

train %>% glimpse()
train %>% select(starts_with('Year'))
skim_train <- skim(train)

plot_missing(train)
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

### uvid u missing i cijeli report
plot_missing(train)
create_report(train)
train %>% ggplot(aes(YearBuilt)) + geom_bar()
train %>% ggplot(aes(YearBuilt,SalePrice)) + geom_point() + geom_smooth()

lm_recipe <- recipe(SalePrice ~ ., data=train) %>% 
  update_role(Id,new_role = "id variable") %>%
  step_novel(all_nominal_predictors()) %>% 
  step_unknown(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>%
  step_impute_median(all_numeric_predictors())%>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_BoxCox(all_numeric_predictors()) %>%
  step_pca(all_numeric_predictors()) %>% 
  prep()
train_trans <- lm_recipe %>% bake(new_data = NULL)

lm_fit <- train(lm_recipe, train, method='lm')

### test

test <- read_csv("input/test.csv")

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
test <- test %>% mutate(SalePrice=0)


### lm test
lm_test_pred <- predict(lm_fit, test)
lm_submission <- bind_cols(test %>% select(Id), lm_test_pred) %>% 
  mutate(SalePrice=10^...2) %>% 
  select(-...2)
lm_submission %>% write_csv('lm_submission.csv')

test_trans <- lm_recipe %>% bake(new_data = test)

