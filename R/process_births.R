print("Importing and processing Births")

# Import data
# This is where you 
births_data <- read.csv("./data/raw/Merged_Births_Jan2018_2024.csv", na.strings = c("\\N", "NA", ""))

# Remove the duplicates from the file
births_data <- births_data[!duplicated(births_data),]

# Format dates to Date format and derive 'year' variables
births_data <- births_data %>%
  mutate(date_of_birth = dmy(date_of_birth),
         registration_date = dmy(registration_date),
         notification_date = dmy(notification_date),
         birth_year = year(date_of_birth),
         registration_year = year(registration_date),
         notification_year = year(notification_date))

# Derive necessary variables
births_data <- births_data %>%
  mutate(mother_age_at_birth = as.numeric(mother_age_at_birth),
         sex = str_to_sentence(sex), 
         mother_age_at_birth = ifelse(!mother_age_at_birth %in% c(10:60), NA,
                                      mother_age_at_birth),
         agegroup_mother = case_when(
           mother_age_at_birth < 15 ~ "under 15",
           mother_age_at_birth %in% c(15:19) ~ "15 - 19",
           mother_age_at_birth %in% c(20:24) ~ "20 - 24",
           mother_age_at_birth %in% c(25:29) ~ "25 - 29",
           mother_age_at_birth %in% c(30:34) ~ "30 - 34",
           mother_age_at_birth %in% c(35:39) ~ "35 - 39",
           mother_age_at_birth %in% c(40:44) ~ "40 - 44",
           mother_age_at_birth %in% c(45:49) ~ "45 - 49",
           mother_age_at_birth > 49 ~ "50 +",
           TRUE ~ "Not Stated"),
         delay = as.numeric(difftime(registration_date, date_of_birth, units = "days")),
         timeliness = case_when(
           delay %in% c(0:29) ~ "Current",
           delay %in% c(30:100) ~ "Late", 
           delay > 100 ~ "Delayed",
           TRUE ~ "check"))

# Clean character variables: set title case and clean white space
births_data <- births_data %>%
  mutate(across(where(is.character), ~ str_to_title(.)), 
         across(where(is.character), ~ str_squish(.)))       

# Write cleaned and processed data to a new file in the 'processed' folder
fwrite(births_data, "./data/processed/births_data.csv")
