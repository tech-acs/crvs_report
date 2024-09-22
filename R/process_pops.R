print("Importing and processing Populations")

pops <- read.csv("./data/raw/population_data.csv")

pops <- pops %>%
  mutate(popn = as.numeric(gsub(",", "", popn)))

fwrite(pops, "./data/processed/pops.csv")

