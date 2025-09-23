run_api <- function (port) {

library(plumber)

pr <- plumber::plumb("plumber.R")
pr$run(host = "0.0.0.0", port = port)}

run_api(8080)
