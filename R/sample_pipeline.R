# This script provides a pipeline to run with the test data in the package.
library(yaml)
library(crvsreportpackage)
library(rlang)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)

# Load the configuration
config <- yaml.load_file("./config/config.yml")
config <- config$default

# Set configuration parameters
raw_data_path <- config$data$raw
processed_data_path <- config$data$processed
external_data_path <- config$data$external

output_tables_path <- config$output$tables
output_figures_path <- config$output$figures

# Set the birth variables dictionary
birth_variables <- config$births_mapper
death_variables <- config$deaths_mapper
marriage_variables <- config$marriage_mapper
divorce_variables <- config$divorce_mapper

#####################################
###   AUXILIARY
#####################################
# Load auxiliary datasets
birth_estimates <- read.csv("data/processed/created_birth_estim.csv")
death_estimates <- read.csv("data/processed/created_death_estim.csv")
pop_estimates <- read.csv("data/processed/created_population_estim.csv")
causes_dict <- read.csv("data/processed/causes_dict.csv")

# Add fertility age groups for the population data
pop_estimates <- construct_age_group(pop_estimates, "age")

# Add the total column for birth estimates
birth_estimates <- birth_estimates %>%
  mutate(total = male + female)
# Add fertility age groups for the birth estimates
birth_estimates <- construct_age_group(birth_estimates, "age")

#####################################
###   BIRTHS
#####################################
# Load the birth data
birth_data <- read.csv("./data/raw/birth_data_processed.csv", na.strings = c("\\N", "NA", ""))

# Add timeliness data
birth_data <- construct_timeliness(birth_data)
# Add dobyr
birth_data <- construct_year(birth_data, date_col = "birth1a", year_col = "dobyr")
# Add boryr
birth_data <- construct_year(birth_data, date_col = "birth1b",  year_col = "doryr")
# Add age of mother
birth_data$birth3a <- as.Date(birth_data$birth3a)
birth_data$birth3b <- as.numeric(difftime(as.Date(birth_data$birth1a),
                                          as.Date(birth_data$birth3a)),
                                 unit="weeks") / 52.25
birth_data$birth3b <- floor(birth_data$birth3b)

# Add fertility age groups
birth_data <- construct_age_group(birth_data, "birth3b")

# Drop birth dates that 
birth_data <- birth_data %>% drop_na(dobyr)
birth_data = birth_data %>%
  filter(dobyr < 2024)
birth_data <- birth_data %>% drop_na()

# Add empty birth1j
birth_data <- construct_empty_var(birth_data)

# Refactor the sex variable
birth_data <- birth_data %>% 
  mutate(birth2a = str_replace(birth2a, "F", "female")) %>% 
  mutate(birth2a = str_replace(birth2a, "M", "male"))

birth_data$birth3n <- "Rural"


birth_data$month_reg <- format(birth_data$birth1b, "%m")
birth_data$month_birth <- format(birth_data$birth1a, "%m")
birth_data$month_yreg <- as.numeric(birth_data$month_reg)*0.01
birth_data$month_ybirth <- as.numeric(birth_data$month_birth)*0.01
birth_data$month_yreg <- birth_data$doryr + birth_data$month_yreg 
birth_data$month_ybirth <- birth_data$dobyr + birth_data$month_ybirth
#####################################
###   DEATHS
#####################################

# Load the death data
death_data <- read_sample_death_data()
# Add dobyr
death_data <- construct_year(death_data, date_col = "death1a", year_col = "dodyr")
# Add leading groups data
death_data <- construct_leading_groups(death_data, age_col = 'death2b', age_group_col = "age_grp_lead")

#####################################
###   MARRIAGE AND DIVORCE
#####################################

# Load the marriage data
marriage_data <- read_sample_marriage_data()
# Load the death data
divorce_data <- read_sample_divorce_data()

## HERE THERE SHOULD BE A FILTER OF THE DATA LOADED USING THE BIRTH VARIABLES
## HERE THERE SHOULD BE A VARIABLE RENAMING USING THE MAPPERS
## HERE THERE SHOULD BE SOME DATA VALIDATION, TO ENSURE DATA IS AS EXPECTED
## HERE THERE COULD BE SOME SUMMARY STATISTICS OR DATA CLEANING

# Access tables configuration
all_tables <- config$tables
tables_selected <- config$tables_selected

# Generate a list of tables to be run
base_name <- "tables_selected$section_"
sectnumbs <- 3:8
tables_to_run <- list()

for (sect_num in sectnumbs) {
  full_var_name <- paste0(base_name, sect_num)
  sub_list <- eval(parse(text = full_var_name))
  if (is.null(sub_list)) {
    tables_to_add <- select_tables(as.character(sect_num), all_tables)
    tables_to_run <- c(tables_to_run, tables_to_add)
    message <- paste("For Section ", sect_num, " adding all tables: ", length(tables_to_add), " added.")
    print(message)
  } else if (identical(sub_list, "")) {
    message <- paste("For Section ", sect_num, " adding no tables.")
    print(message)
  } else {
    tables_to_run <- c(tables_to_run, sub_list)
    combined_sub_list <- paste(sub_list, collapse = ", ")
    message <- paste("For Section ", sect_num, " adding tables: ", combined_sub_list, ".")
    print(message)
  }
}

# Filter the tables in the config for the specified table_ids
filtered_tables <- lapply(all_tables, function(table) {
  if (table$table_id %in% tables_to_run) {
    return(table)
  } else {
    return(NULL)
  }
})

# Remove NULL values from the filtered list
filtered_tables <- Filter(Negate(is.null), filtered_tables)

# Iterate over the filtered tables and call the respective functions
for (table in filtered_tables) {
  function_name <- table$function_name
  args <- convert_config_args(table$function_args)
  if (exists(function_name)) {
    cat("Running function for table_id:", table$table_id, "\n")
    do.call(function_name, args)
  } else {
    cat("Function", function_name, "not found!\n")
  }
}

# Convert all the .csv files into .xlsx files
output_xls_tables_path <- paste(output_tables_path, "output.xlsx")
convert_csv_xlsx(input_path = output_tables_path, output_path = output_xls_tables_path)



# Generate Table 3.2
output <- birth_data |>
  filter(is.na(birth1j) & !is.na(doryr) &
           dobyr %in% generate_year_sequence(2023)) |>
  group_by(doryr, dobyr) |>
  summarise(Total = n())

output2 <- output %>%
  group_by(doryr) %>%
  summarise(total = sum(Total))

output <- output %>%
  left_join(output2, by = c("doryr" = "doryr")) %>%
  mutate(Percentage := round_excel((Total/ total) * 100, 2)) %>%
  select(-c(total, Total)) |>
  pivot_wider(names_from = doryr, values_from = Percentage, values_fill = 0)



# Generate Table 4.2
output42 <- birth_data |>
  filter(dobyr == 2023 & is.na(birth1j)) |>
  group_by(birth1c, birth2a) |>
  summarise(total = n()) |>
  pivot_wider(names_from = birth2a, values_from = total, values_fill = 0) |>
  mutate(ratio = round_excel(male/female,1))

# Generate Table Month and place of registration
output_month_registration <- birth_data |>
  filter(dobyr == 2023 & is.na(birth1j)) |>
  group_by(birth1c, month_reg) |>
  summarise(total = n()) |>
  pivot_wider(names_from = month_reg, values_from = total, values_fill = 0) |>
  adorn_totals("row")

# Generate Table Month and place of registration
output_month_birth <- birth_data |>
  filter(dobyr == 2023 & is.na(birth1j)) |>
  group_by(birth1c, month_birth) |>
  summarise(total = n()) |>
  pivot_wider(names_from = month_birth, values_from = total, values_fill = 0) |>
  adorn_totals("row")

# Generate Table Month and place of registration
output_month_birth <- birth_data |>
  filter(is.na(birth1j)) |>
  group_by(birth1c, month_birth) |>
  summarise(total = n()) |>
  pivot_wider(names_from = month_birth, values_from = total, values_fill = 0) |>
  adorn_totals("row")

