library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
library(janitor)
library(readxl)
library(pointblank)
library(data.table)

source("./R/data_checks.R")


source("./R/process_data.R")
source("./R/read_in_processed_data.R")

suppressMessages(source("./R/births_tables.R"))
suppressMessages(source("./R/deaths_tables.R"))
suppressMessages(source("./R/marriages_tables.R"))

