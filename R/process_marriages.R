print("Importing and processing Marriages")
#read in data from Excel 
marriages_data <- read_excel("./data/raw/marriages data set-19-21.xlsx")

#clean the column names to make consistent
marriages_data <- clean_names(marriages_data)

#rename gender columns
marriages_data <- marriages_data %>%
  rename(male = gender_9,
         female = gender_10)

# set ages of groom and bride to NA if over 80 years old
marriages_data <- marriages_data %>%
  mutate(groom_age = ifelse(groom_age > 80, NA, groom_age),
         bride_age  = ifelse(bride_age > 80, NA, bride_age))

#derive groom and bride agegroups
marriages_data <- marriages_data %>%
  mutate(groom_agegroup = as.character(cut(as.numeric(groom_age), 
                                           breaks = c(18, seq(20, 60, 5), Inf),
                                           labels = c("18-19", "20-24", "25-29", 
                                                      "30-34", "35-39", "40-44", "45-49",
                                                      "50-54", "55-59", "60+"),
                                           right = F))) %>%
  mutate(groom_agegroup = ifelse(is.na(groom_agegroup), "Not stated", groom_agegroup))

marriages_data <- marriages_data %>%
  mutate(bride_agegroup = as.character(cut(as.numeric(bride_age), 
                                           breaks = c(18, seq(20, 60, 5), Inf),
                                           labels = c("18-19", "20-24", "25-29", 
                                                      "30-34", "35-39", "40-44", "45-49",
                                                      "50-54", "55-59", "60+"),
                                           right = F))) %>%
  mutate(bride_agegroup = ifelse(is.na(bride_agegroup), "Not stated", bride_agegroup))

fwrite(marriages_data, "./data/processed/marriages_data.csv")


