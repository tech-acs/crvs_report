# Read in raw data
births_data <- read.csv("./data/raw/Merged_Births_Jan2018_2024.csv", na.strings = c("\\N", "NA", ""))

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
dths_scan <- scan_data(deaths_data, "OV")
export_report(bths_scan, "./data_reports/death_data_scan.html")
marr_scan <- scan_data(marriages_data, "OV")
export_report(marr_scan, "./data_reports/marriage_data_scan.html")
