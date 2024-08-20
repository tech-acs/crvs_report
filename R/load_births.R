print("Importing and processing Births")

# Import data
# This is where you load the births data that you have 
full_births_data <- read.csv("./data/raw/births_malawi_sample.csv", na.strings = c("\\N", "NA", ""))

# Construct a still births indicator
#births_data$birth1j <- ifelse(births_data$number_of_children_born_alive_inclusive == births_data$number_of_children_born_still_alive, 1, 0)

# Subset the dataframe to keep only the specified columns in order
my_columns <- c("Gender", "Place.of.Birth",	"District.of.Birth",
                "Type.of.Birth",	"Parents.Married.",	"Mother.Date.of.Birth",
                "Mother.Physical.Residential.District", "Date.of.Birth", "Date.Registered")  
births_data <- full_births_data[, my_columns]

# And you have a list of new column names in new_names for specific columns
new_names <- c("birth2a" = "Gender",
               "birth1i" = "Place.of.Birth",
               "birth1c" = "District.of.Birth",
               "birth1g" = "Type.of.Birth",
               "birth3c" = "Parents.Married.",
               "birth3a" = "Mother.Date.of.Birth",
               "birth3l" = "Mother.Physical.Residential.District",
               "birth1a" = "Date.of.Birth",
               "birth1b" = "Date.Registered")

# Rename the specified columns to align with the package
births_data <- rename(births_data, !!!new_names)

#Store the data as a .csv
write.csv(births_data, "./data/raw/birth_data_processed.csv", row.names = FALSE)
