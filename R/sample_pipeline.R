# This script provides a pipeline to run with the test data in the package.

library(yaml)
library(crvsreportpackage)
library(rlang)

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


# Load the birth data
birth_data <- read_sample_birth_data()
# Load the death data
death_data <- read_sample_death_data()
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