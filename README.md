This is an R tidymodels script used for the estimation of sale prices in a Kaggle competition on Ames housing data. The achieved score is 0.14541 on test data using the random forest algorithm, ranking 2124. position on the leaderboard list.

https://www.kaggle.com/competitions/house-prices-advanced-regression-techniques/overview

API service is deployed to Google Cloud Run service: https://ames-api-576662713224.europe-west12.run.app/__docs__/

You can test it using:

curl -X POST "https://ames-api-576662713224.europe-west12.run.app" \
-H "Authorization: bearer $(gcloud auth print-identity-token)" \
-H "Content-Type: application/json" \
-d '{
  "name": "Developer"
}'


