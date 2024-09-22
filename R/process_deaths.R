# Import deaths
# This is where you put the paths to the raw data files

print("Importing and processing Deaths")
deaths_2019 <- read.csv("./data/raw/Death_2019.csv", na.strings = c("\\N", "NA", ""), encoding = "latin1")
deaths_2020 <- read.csv("./data/raw/Death_2020.csv", na.strings = c("\\N", "NA", ""), encoding = "latin1")
deaths_2021 <- read.csv("./data/raw/Death_2021.csv", na.strings = c("\\N", "NA", ""), encoding = "latin1")
deaths_2022 <- read.csv("./data/raw/Death_2022.csv", na.strings = c("\\N", "NA", ""), encoding = "latin1")


# This step appends the separate imported files into one large multi-year file
deaths_data <- rbind(deaths_2019, deaths_2020, deaths_2021, deaths_2022)

# This step removes the individual year files from the environment
rm(deaths_2019, deaths_2020, deaths_2021, deaths_2022)


# Removal of duplicates from the file
deaths_data <- deaths_data[!duplicated(deaths_data),]

# Process the deaths data
deaths_data <- deaths_data %>%
  mutate(sex = str_to_sentence(sex),
         age_at_death = as.numeric(age_at_death),
         age_at_death = case_when(
           !age_at_death %in% c(0:115) ~ NA,
           TRUE ~ age_at_death),
         date_of_death = ymd(date_of_death),
         date_of_birth = ymd(date_of_birth),
         date_of_notification = ymd(date_of_notification),
         date_of_registration = ymd(date_of_registration),
         death_year = year(date_of_death),
         birth_year = year(date_of_birth),
         notification_year = year(date_of_notification),
         registration_year = year(date_of_registration),
         delay = as.numeric(difftime(date_of_registration, date_of_death, units = "days")),
         age_in_days = as.numeric(difftime(date_of_death, date_of_birth, units = "days")),
         location = case_when(
           residence_district == occurrence_district ~ "same place as occurrence",
           residence_district != occurrence_district ~ "other location",
           is.na(residence_district) ~ "Not stated", 
           is.na(occurrence_district) ~ "Not stated"),
         age_grp_lead = as.character(cut(as.numeric(age_at_death), 
                                         breaks = c(0, 5, 15, 70, Inf),
                                         right = F,
                                         labels = c("<5", "5-14", "15-69", "70+"))),
         age_group_wide = as.character(cut(as.numeric(age_at_death), 
                                       breaks = c(0, 5, 25, 75, Inf),
                                       right = FALSE, 
                                       labels = c("0–4", "5–24", "25–74", "75+"))),
         timeliness = case_when(
           delay %in% c(0:29) ~ "Current",
           delay %in% c(30:100) ~ "Late", 
           delay > 100 ~ "Delayed",
           TRUE ~ "check"), 
         sbind = ifelse(grepl("stillbirth", cause_of_death_desc, ignore.case = TRUE), 1, 0),
         age_grp_lead = ifelse(is.na(age_grp_lead), "Not stated", age_grp_lead),
         neo_deaths = case_when(
           age_in_days < 8 ~ "early neonatal",
           age_in_days %in% (8:28) ~ "late neonatal",
           TRUE ~ NA),
         age_group_80 = as.character(cut(as.numeric(age_at_death), 
                                         breaks = c(0, 1, seq(5, 80, 5), Inf), 
                                         labels = c("<1", "01-04", "05-09", "10-14", "15-19", "20-24", "25-29", 
                                                    "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", 
                                                    "65-69", "70-74", "75-79", "80+"),
                                         right = F))) %>%
  mutate(age_group_80 = ifelse(is.na(age_group_80), "Not stated", age_group_80), 
         age_grp_lex = as.character(cut(as.numeric(age_at_death), 
                                         breaks = c(0, seq(5, 80, 5), Inf), 
                                         labels = c("01-04", "05-09", "10-14", "15-19", "20-24", "25-29", 
                                                    "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", 
                                                    "65-69", "70-74", "75-79", "80+"),
                                         right = F)))

# Write the cleaned, processed, data to a new file in to the 'processed' folder
fwrite(deaths_data, "./data/processed/deaths_data.csv")
