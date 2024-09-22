# Read in raw data
births_data <- read.csv("./data/raw/Merged_Births_Jan2018_2024.csv", na.strings = c("\\N", "NA", "")) %>%
  mutate(date_of_birth = dmy(date_of_birth),
         registration_date = dmy(registration_date),
         notification_date = dmy(notification_date))

deaths_2019 <- read.csv("./data/raw/Death_2019.csv", na.strings = c("\\N","NA"), encoding = "latin1")
deaths_2020 <- read.csv("./data/raw/Death_2020.csv", na.strings = c("\\N","NA"), encoding = "latin1")
deaths_2021 <- read.csv("./data/raw/Death_2021.csv", na.strings = c("\\N","NA"), encoding = "latin1")
deaths_2022 <- read.csv("./data/raw/Death_2022.csv", na.strings = c("\\N","NA"), encoding = "latin1")


deaths_data <- rbind(deaths_2019, deaths_2020, deaths_2021, deaths_2022)
rm(deaths_2019, deaths_2020, deaths_2021, deaths_2022)

marriages_data <- read_excel("./data/raw/marriages data set-19-21.xlsx")


# Perform data scans
bths_scan <- scan_data(births_data, "OV")
export_report(bths_scan, "./data_reports/birth_data_scan.html")

al <- action_levels(
  notify_at = 0.01,
  warn_at = 0.02,
  stop_at = 0.1
)

agent <- births_data %>%
  create_agent(actions = al, label = "Births data quality report") %>%
  col_vals_not_between(mother_age_at_birth, left = 0, right = 115, na_pass = TRUE) %>%
  col_vals_gte(registration_date, vars(date_of_birth), na_pass = TRUE) %>%
  col_vals_not_null(date_of_birth) %>%
  col_vals_not_null(mother_age_at_birth) %>%
  col_vals_in_set(sex, set = c("Male","Female")) %>%
  col_vals_not_null(birth_district) %>%
  col_vals_not_null(birth_county) %>%
  col_vals_not_null(birth_subcounty) %>%
  col_vals_not_null(birth_village, actions = c(warn_on_fail(warn_at = 0.07), stop_on_fail(stop_at = 0.2))) %>%
  col_vals_not_null(birth_facility) %>%
  col_vals_not_between(weight_at_birth, left = 230, right = 4000, na_pass = FALSE) %>%
  interrogate()

#### Export reports ####
export_report(agent, "./data_reports/birth_data_report.html")

#### Remove unneeded files ####
rm(agent, al, bths_scan)


dths_scan <- scan_data(deaths_data, "OV")
export_report(dths_scan, "./data_reports/death_data_scan.html")


#### Validation checks ####
al <- action_levels(
  notify_at = 0.01,
  warn_at = 0.02,
  stop_at = 0.1
)

# Below are the validation rules for daeths data.
agent <- deaths_data %>%
  create_agent(actions = al, label = "Deaths data quality report") %>%
  col_vals_between(age_at_death, left = 0, right = 115, na_pass = TRUE) %>%
  col_vals_gte(date_of_registration, vars(date_of_death), na_pass = TRUE) %>%
  col_vals_gte(date_of_death, vars(date_of_birth), na_pass = TRUE) %>%
  col_vals_gte(date_of_registration, vars(date_of_notification), na.pass = TRUE) %>%
  col_vals_not_null(date_of_birth) %>%
  col_vals_not_null(age_at_death) %>%
  interrogate()

#### Export reports ####
export_report(agent, "./data_reports/death_data_report.html")

#### Remove unneeded files ####
rm(agent, al, dths_scan)


marr_scan <- scan_data(marriages_data, "OV")
export_report(marr_scan, "./data_reports/marriage_data_scan.html")
