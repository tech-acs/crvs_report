print("Importing and processing Births")

# Import data
# This is where you load the births data that you have 
births_data <- read.csv("./data/raw/birth_data.csv", na.strings = c("\\N", "NA", ""))

# Construct a still births indicator
births_data$birth1j <- ifelse(births_data$number_of_children_born_alive_inclusive == births_data$number_of_children_born_still_alive, 1, 0)

# Subset the dataframe to keep only the specified columns in order
my_columns <- c("gender", "PlaceofBirth",	"District_of_Birth",
                "Type_of_Birth",	"ParentsMarried",	"Mother_Birth_Date",
                "MotherResidentialCountry", "birthdate", "Date_Registered",
                "birth1j")  
births_data <- births_data[, my_columns]

# And you have a list of new column names in new_names for specific columns
new_names <- c("gender" = "Birth2a",
               "PlaceofBirth" = "Birth1i",
               "District_of_Birth" = "Birth1c",
               "Type_of_Birth" = "Birth1g",
               "ParentsMarried" = "Birth3c",
               "Mother_Birth_Date" = "Birth3a",
               "MotherResidentialCountry" = "Birth3l",
               "birthdate" = "Birth1a",
               "Date_Registered" = "Birth1b",
               "birth1j" = "Birth1j",)

# Rename the specified columns to align with the package
births_data <- rename(births_data, !!!new_names)

#Store the data as a .csv
write.csv(births_data, "./data/raw/birth_data_processed.csv", row.names = FALSE)