# This runs the individual processing scripts
source("./R/process_births.R")
source("./R/process_deaths.R")
source("./R/process_pops.R")
source("./R/process_marriages.R")

# This clears the global environment
rm(list = ls())

