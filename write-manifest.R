library(renv)
renv::snapshot()
rsconnect::writeManifest(appFiles = "ames.Qmd")
