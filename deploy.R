library(plumber)
library(vetiver)
library(pins)

rf_final_fit <- read_rds("output/rf_final_fit.rds")
vet_model <- vetiver_model(rf_final_fit, "ames_rf")

board <- board_local( versioned = TRUE)

vetiver_pin_write(board, vet_model)
