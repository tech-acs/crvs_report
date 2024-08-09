library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
library(janitor)
library(readxl)
library(pointblank)


source("./R/process_births.R")
source("./R/process_deaths.R")
source("./R/process_pops.R")
source("./R/process_marriages.R")

rm(list = ls())

