###############################################
#load the required libraries
library(tidyr)
library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(janitor)
library(pointblank)
library(glue)
###############################################
#Check the working directory
getwd()
setwd ("path/to/your/data")

# The goal is to make data analysis easy and get a reliable result.
# Cleaning, Editing, Standardization and transforming (deriving) data.
# Remember Garbage in Garbage out (GIGO).
# Data Cleaning on both Variables and Values.

# Rounding function
round_excel <- function(x, n = 0){
  scale <- 10^n
  rounded <- trunc(x * scale + sign(x) * 0.5) / scale
  return(rounded)
}



# Sample data Review Birth and Death Registration data
# Read Sample Birth registration Data
bdata<-read.csv("./births_data.csv", header = T,
                na.string=c("NA","N/A", "na", "99", "-", "***"))   


#Review the data set
dim(bdata)
str(bdata)
View(bdata)


#### Clean column names ####
# The 'clean_names' function will set the column names of the data to snake_case as default.
# This means that any space(s) in the names will be replaced with an underscore and the text will be lower case
bdata <- clean_names(bdata)
colnames(bdata)


#  Re factor the sex variable to work well with the package
bdata <- bdata %>% 
  mutate(gender = str_replace(gender, "F", "female")) %>% 
  mutate(gender = str_replace(gender, "M", "male"))



# Data review must meet expectations. Some variables will fall under normal curve  having higher frequency around the median value
table(bdata$gestation_age_at_birth_in_weeks)
summary(bdata$gestation_age_at_birth_in_weeks)


# Data cleaning includes dealing with duplicates, missing values and outliers
# Finding duplicates and how to remove them. But first find the unique identifier(s) of an observation
# To confirm  the unique identifier(s)
library(dlookr)
diagnose(bdata) %>% select(-missing_count, -missing_percent)%>%
filter(unique_rate==1) 

dup_bdata<-bdata[duplicated(bdata$person_id),]
dim(dup_bdata)

#Note that the dimension has not been changed, that is because there is no duplicated data
nodup_bdata <- bdata[!duplicated(bdata$person_id), ]
dim(nodup_bdata)

# an Other way to remove duplicates if any
bdata <- bdata %>% distinct()   
dim(bdata)

# Lets focus on two variables
diagnose(bdata,gestation_age_at_birth_in_weeks) # Missing data 5.9%
diagnose(bdata,birth_weight)

sort(unique(bdata$gestation_age_at_birth_in_weeks))
sort(unique(bdata$birth_weight))     # birth weight can not be 0

#diagnose some categorical variables
diagnose_category(bdata)%>% 
  filter(variables=="level_of_education" & levels!="")
  
diagnose_category(bdata)%>% 
  filter(variables=="place_of_birth" & levels!="")

#More to review missing data
highly_missed_bdata<-bdata %>%
  diagnose() %>%
    filter(missing_count >50000) %>% 
  select(missing_count,missing_percent)%>%
  arrange(desc(missing_count))
highly_missed_bdata



# Replace 0 birth weight with mean value
count_no_of_zero_bw <- sum(bdata$birth_weight==0)
count_no_of_zero_bw

bdata$birth_weight[bdata$birth_weight==0] <-mean(bdata$birth_weight, na.rm = TRUE)
#round_excel(unique(bdata$birth_weight),2)        
glue('We have replaces {count_no_of_zero_bw} values')

# Replace missing data with mean value  
bdata$gestation_age_at_birth_in_weeks
bdata$gestation_age_at_birth_in_weeks[is.na(bdata$gestation_age_at_birth_in_weeks)]<- mean(bdata$gestation_age_at_birth_in_weeks, na.rm = TRUE)

round_excel(bdata$gestation_age_at_birth_in_weeks,2)  

# Count the no of missing values from birth data
n <- sum(is.na(bdata$gestation_age_at_birth_in_week))	
cat("\nNo of missing Data (gestation_age_at_birth_in_week):", n)

# Examine the data distribution with and without outliers
plot_outlier(bdata, gestation_age_at_birth_in_weeks)

# change the date format
#### Convert date variables to date type ####
bdata <- bdata %>%
  mutate(across(starts_with("date"), ~ as_date(ymd_hms(.))),
         mother_date_of_birth = as_date(ymd_hms(mother_date_of_birth)),
         father_date_of_birth = as_date(ymd_hms(father_date_of_birth)))
View(bdata)

#extract year of birth (yob) for further caculation
bdata <- bdata %>% mutate(yob=year(date_of_birth))

# Calculate Age of a mother
bdata$age_of_mother_at_birth= as.numeric(difftime(as.Date(bdata$date_of_birth), 
          as.Date(bdata$mother_date_of_birth), ), units = "days")%/% 365.25 
View(bdata)

bdata <- bdata %>%
  mutate(age_group_mother = case_when(
    age_of_mother_at_birth < 10 ~ "under 10",
    age_of_mother_at_birth %in% c(10:14) ~ "10 - 14",  
    age_of_mother_at_birth %in% c(15:19) ~ "15 - 19",
    age_of_mother_at_birth %in% c(20:24) ~ "20 - 24",
    age_of_mother_at_birth %in% c(25:29) ~ "25 - 29",
    age_of_mother_at_birth %in% c(30:34) ~ "30 - 34",
    age_of_mother_at_birth %in% c(35:39) ~ "35 - 39",
    age_of_mother_at_birth %in% c(40:44) ~ "40 - 44",
    age_of_mother_at_birth %in% c(45:49) ~ "45 - 49",
    age_of_mother_at_birth > 49 ~ "Unknown",
    is.na(age_of_mother_at_birth) ~ "Unknown",
    TRUE ~ "Not Stated"),
    gestation_age_group = case_when(
      gestation_age_at_birth_in_weeks <20 ~ "<20",
      gestation_age_at_birth_in_weeks %in% c(20:21) ~ "20 – 21",
      gestation_age_at_birth_in_weeks %in% c(22:27) ~ "22 – 27",
      gestation_age_at_birth_in_weeks %in% c(28:31) ~ "28 – 31",
      gestation_age_at_birth_in_weeks %in% c(32:35) ~ "32 – 35",
      gestation_age_at_birth_in_weeks %in% c(36) ~ "36",
      gestation_age_at_birth_in_weeks %in% c(37:41) ~ "37 – 41",
      gestation_age_at_birth_in_weeks >= 42 ~ "42+",
      TRUE ~ "Not stated"))

View(bdata)


#  Subset the data frame to keep only the specified columns in order
new_columns <- c(
  "serno", "gender", "date_of_birth", "place_of_birth",
  "country_of_birth", "yob", "district_of_birth", "type_of_birth",
  "birth_weight", "gestation_age_at_birth_in_weeks",
  "number_of_prenatal_visits", "parents_married", "date_of_marriage",
  "mother_date_of_birth", "mother_nationality", "mother_home_district",
  "mother_residential_country", "date_registered", "date_reported",
  "place_of_registration", "district_of_registration", "level_of_education",
  "father_date_of_birth", "father_home_district", "father_nationality",
  "age_of_mother_at_birth", "age_group_mother", "gestation_age_group"
  )  

cleaned_bdata <- bdata[ ,new_columns]
col_dropped <- dim(bdata)[2] - dim(cleaned_bdata)[2]
glue('We have dropped {col_dropped} Columns')

View(cleaned_bdata)
print(diagnose(cleaned_bdata))

# First distinguish missing and NA data
library(DataExplorer)
plot_missing(cleaned_bdata,
             group = c("Excellent" = 0.01, "Good" = .05, "Ok" = .1, "Bad" = .2),
             title= "Missing and NA Birth Data",
             theme_config = list(legend.position = c("top")))

miss_plot <- plot_missing(cleaned_bdata)

#Can Drop all rows containing NA data !!!!
cleaned_bdata <- cleaned_bdata %>% drop_na()
#To check run:
dim(cleaned_bdata)

# save cleaned data
write.csv(cleaned_bdata,"./cleaned_birth_reg.csv")

cleaned_bdata<-read.csv("./cleaned_birth_reg.csv")
View(cleaned_bdata)

# further cleaning with pointblank
birth_data_scan <- scan_data(cleaned_bdata, "OV")
export_report(birth_data_scan, "./birth_data_scan.html")
browseURL("./birth_data_scan.html")

#### Rules based data check ####
# A more detailed report can be produced using the 'create_agent' function.
# This runs rules based validation check where you can set the parameters.
# There is an additional section called 'action_levels'. This is where you can set 
# a percentage threshold for 3 different levels.
al <- action_levels(
  notify_at = 0.01,
  warn_at = 0.02,
  stop_at = 0.1
)

district_list <- c(
  "District A", "District B", "District C", "District D",
  "District E", "District F", "District G", "District H",
  "District I", "District J", "District K", "District L", "District L2"
  )

# Correct the known issues in district variables
cleaned_bdata <- cleaned_bdata %>%
  mutate(across(contains("district"), ~ case_when(
    . == "District L2" ~ "District L",
    is.na(.) ~ "Unknown",
    TRUE ~ gsub(" City", "", .))))





# To create the report you need to build a list of rules to be run.
# There are many different checks that can be run.
agent <- cleaned_bdata %>%
  create_agent(actions = al, label = "Births data quality report. Pre Processing") %>%
  col_vals_in_set(gender, set = c("M","F", "male", "female")) %>%
  col_vals_not_null(date_of_birth) %>%    
  col_vals_lt(yob,2024, na_pass = TRUE) %>%
  col_vals_in_set(district_of_birth, set = district_list) %>%
  col_vals_lte(date_of_birth, vars(date_registered), na_pass = TRUE) %>%
  col_vals_gte(date_reported, vars(date_of_birth), na_pass = TRUE) %>%
  col_vals_between(age_of_mother_at_birth,10,50,inclusive = c(TRUE, FALSE),na_pass = TRUE)%>%
  interrogate()


# Export reports 
export_report(agent, "./birth_data_report.html")
#open the report file
browseURL("./birth_data_report.html")


###########################################################################################
# Death Registration Data

ddata<-read.csv("./deaths_sample.csv",  header = T,
                na.string=c("NA", "N/A", "na", "99", "-","..."))  # reads death data

dim(ddata)
str(ddata)
view(ddata) 

# similarly using clean names function
# This means that any spaces in the names will be replaces with an underscore and the text will be lower case
ddata <- clean_names(ddata)
colnames(ddata)

ddata[duplicated(ddata$person_id),]  

# count the no of missing values from death data
n <- sum(is.na(ddata$date_Registered))	
cat("\nNo of missing Death Reg. Data:", n)


# finding duplicates and how to remove them. But first find the unique identifier(s) of an observation
# To confirm  the unique identifier(s)
diagnose(ddata) %>% select(-missing_count, -missing_percent)%>%
  filter(unique_rate==1) 

dup_ddata<-ddata[duplicated(ddata$death_entry_number ),]
dim(dup_ddata)


#Note that the dimension has not been changed, that is because there is no duplicated data
nodup_ddata <- ddata[!duplicated(ddata$death_entry_number ), ]
dim(nodup_ddata)

# an Other way to remove duplicates if any
ddata <- ddata %>% distinct()   
dim(ddata)


# Drop  column(s) not needed for the analysis
ddata <- subset(
  ddata, select = -c(
    drn, first_name, middle_name,	surname, date_reported, date_entered_in_crvs,
    date_drn_generated, date_printed,	date_dispatched, type_of_registration,
    type_of_form, ta_of_death, other_place_of_death_details, deseased_home_ta,
    record_status, status_comment, mother_first_name, mother_middle_name,
    mother_surname,	mother_id_number, father_first_name, father_middle_name,
    father_surname,	father_national_id, informant_first_name
    ))
                
View(ddata)

# Calculate Age of decedent
ddata$age_of_decedent_in_days= as.numeric(difftime(as.Date(ddata$date_of_death),as.Date(ddata$date_of_birth),),units = "days")

ddata$age_of_decedent<-ddata$age_of_decedent_in_days %/% 365.25
View(ddata)

# finding duplicates and how to remove them
dup_ddata<-ddata[duplicated(ddata$person_id),]

nodup_ddata <- ddata[!duplicated(ddata$person_id), ]
dim(nodup_ddata)

# To confirm  the unique key/s
diagnose(ddata)

# examine the distributions with and without outlines
plot_outlier(ddata, age_of_decedent)

# change the date format
#### Convert date variables to date type ####
ddata <- ddata %>%
  mutate(across(starts_with("date"), ~ as_date(ymd_hms(.))))
        
View(ddata)

#extract year of birth (yod) for further caculation
ddata <- ddata %>% mutate(yod=year(date_of_death))

ddata <- ddata %>%
  mutate(age_group_decedent = case_when(
    age_of_decedent < 1 ~ "under 1",
    age_of_decedent %in% c(0:5) ~ "under 5",
    age_of_decedent %in% c(5:14) ~ "5 - 14",
    age_of_decedent %in% c(15:24) ~ "15 - 24",
    age_of_decedent %in% c(25:49) ~ "25 - 49",
    age_of_decedent %in% c(50:64) ~ "50 - 64",
    age_of_decedent %in% c(65:79) ~ "65 - 79",
    age_of_decedent %in% c(80:120) ~ "80+",
    age_of_decedent > 121 ~ "Unknown",
    is.na(age_of_decedent) ~ "Unknown",
    TRUE ~ "Not Stated"))
  
View(ddata)

cleaned_ddata <- ddata

View(cleaned_ddata)
print(diagnose(cleaned_ddata), n=50)
dim(cleaned_ddata)

plot_missing(cleaned_ddata)

#Can Drop all rows containing NA data !!!!
cleaned_ddata <- cleaned_ddata %>% drop_na()
dim(cleaned_ddata)

write.csv(cleaned_ddata,"./cleaned_death_reg.csv")
cleaned_ddata<-read.csv("./cleaned_death_reg.csv")
View(cleaned_ddata)

# further cleaning with pointblank

death_data_scan <- scan_data(cleaned_ddata, "OV")
export_report(birth_data_scan, "./death_data_scan.html")
browseURL("./death_data_scan.html")



#### Rules based data check ####
# A more detailed report can be produced using the 'create_agent' function.
# This runs rules based validation check where you can set the parameters.
# There is an additional section called 'action_levels'. This is where you can set 
# a percentage threshold for 3 different levels.
al <- action_levels(
  notify_at = 0.01,
  warn_at = 0.02,
  stop_at = 0.1
)

district_list <- c(
  "District A", "District B", "District C", "District D",
  "District E", "District F", "District G", "District H",
  "District I", "District J", "District K", "District L", "District L2"
  )

# Correct the known issues in district variables
cleaned_bdata <- cleaned_bdata %>%
  mutate(across(contains("district"), ~ case_when(
    . == "District L2" ~ "District L",
    is.na(.) ~ "Unknown",
    TRUE ~ gsub(" City", "", .))))


# List of rules
# There are many different checks that can be run.
agent <- cleaned_ddata %>%
  create_agent(actions = al, label = "Deaths data quality report. Pre Processing") %>%
  col_vals_in_set(sex, set = c("M","F")) %>%
  col_vals_not_null(date_of_birth) %>%    
  col_vals_not_null(date_of_death) %>%  
  col_vals_in_set(district_of_death, set = district_list) %>%
  col_vals_gte(date_of_death, vars(date_of_birth), na_pass = TRUE) %>%
  col_vals_lte(date_of_death, vars(date_registered), na_pass = TRUE) %>%
  col_vals_lte( age_of_decedent,120,na_pass = TRUE)%>%
  interrogate()


# Export reports 
export_report(agent, "./death_data_report.html")
#open the report file
browseURL("./death_data_report.html")
