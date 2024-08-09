# Load necessary libraries into R environment
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
library(janitor)
library(readxl)
library(pointblank)
library(data.table)

# Run the Overview and rule based checks
source("./R/data_checks.R")

# Import, clean and derive variables
source("./R/process_data.R")

# Read in the cleaned data
source("./R/read_in_processed_data.R")

# Produce the tables
suppressMessages(source("./R/births_tables.R"))
suppressMessages(source("./R/deaths_tables.R"))
suppressMessages(source("./R/marriages_tables.R"))

