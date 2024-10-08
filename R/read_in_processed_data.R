# This code reads in the cleaned, and processed, data into the R environment

print("Reading in Deaths")
deaths_data <- read.csv("./data/processed/deaths_data.csv", na.strings = c("\\N", "NA", ""))
print("Reading in Births")
births_data <- read.csv("./data/processed/births_data.csv", na.strings = c("\\N", "NA", ""))
print("Reading in Marriages")
marriages_data <- read.csv("./data/processed/marriages_data.csv", na.strings = c("\\N", "NA", ""))
print("Reading in Populations")
pops_data <- read.csv("./data/processed/pops.csv", na.strings = c("\\N", "NA", ""))
