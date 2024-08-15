data_year <- 2022

#Table3.1
print("creating Table 3.1")
bths <- births_data %>%
  filter(registration_year > 2018 & timeliness != "check") %>%
  group_by(registration_year, timeliness) %>%
  summarise(counts = n())

dths <- deaths_data %>%
  filter(registration_year > 2018 & timeliness != "check") %>%
  group_by(registration_year, timeliness) %>%
  summarise(counts = n())

table3.1 <- merge(dths, bths, by = c("registration_year", "timeliness"), all.x = TRUE) 
table3.1[is.na(table3.1)] <- 0

table3.1 <- table3.1 %>%
  rename(deaths = `counts.x`,
         births = `counts.y`) %>%
  pivot_wider(names_from = registration_year, values_from = c(deaths, births), values_fill = 0) %>%
select(timeliness, births_2019, deaths_2019, births_2020, deaths_2020,
       births_2021, deaths_2021, births_2022, deaths_2022, births_2023, deaths_2023)

write.csv(table3.1, ("./outputs/table3_1.csv"), row.names = FALSE)


#Table3.2
print("creating Table 3.2")
output <- births_data %>%
  filter(!is.na(registration_year) &
           birth_year > 2018) %>%
  group_by(registration_year, birth_year) %>%
  summarise(Total = n())

output2 <- output %>%
  group_by(registration_year) %>%
  summarise(total = sum(Total))

# Merge total live births back into the original dataframe
table3.2 <- output %>%
  left_join(output2, by = c("registration_year" = "registration_year")) %>%
  mutate(Percentage := round((Total/ total) * 100, 2)) %>%
  select(-c(total, Total)) %>%
  pivot_wider(names_from = birth_year, values_from = Percentage, values_fill = 0) %>%
  adorn_totals("col", name = "Grand total")

write.csv(table3.2, ("./outputs/table3_2.csv"), row.names = FALSE)

#Table3.3
print("creating Table 3.3")
output <- deaths_data %>%
filter(registration_year < 2030 &
         death_year %in% c(2018:2023)) %>%
  group_by(registration_year, death_year) %>%
  summarise(Total = n())

output2 <- output %>%
  group_by(registration_year) %>%
  summarise(total = sum(Total))

# Merge total death back into the original dataframe
table3.3 <- output %>%
  left_join(output2, by = c("registration_year" = "registration_year")) %>%
  mutate(Percentage := round((Total/ total) * 100, 2)) %>%
  select(-c(total, Total)) %>%
  pivot_wider(names_from = death_year, values_from = Percentage, values_fill = 0) %>%
  adorn_totals("col", name = "Grand total")

write.csv(table3.3, ("./outputs/table3_3.csv"), row.names = FALSE)

#Table3.4
print("creating Table 3.4 (partial)")
table3.4 <- births_data %>%
  filter(sex %in% c("Male", "Female") & birth_year > 2010) %>%
  group_by(birth_year, sex) %>%
  summarise(counts = n()) %>%
  pivot_wider(names_from = sex, values_from = counts, values_fill = 0) %>%
  adorn_totals("col", name = "Total")
write.csv(table3.4, ("./outputs/partial_table3_4.csv"), row.names = FALSE)

#Table3.11
print("creating Table 3.11")
agegrp <- births_data %>%
  filter(registration_year == data_year) %>%
  group_by(agegroup_mother) %>%
  summarise(counts = n())

all_out = sum(agegrp$counts)

agegrp <- agegrp %>%
  mutate(total_count = all_out) %>%
  mutate(proportion = counts/total_count*100) %>%
  select(-total_count) %>%
  arrange(desc(agegroup_mother == "Under 15")) %>%
  adorn_totals("row", name = "Grand total") %>%
  mutate(proportion = round(proportion, 0))

na_index <- agegrp$agegroup_mother == "Not Stated"
total_na <- sum(agegrp$counts[na_index])

table3.11 <- agegrp %>%
  mutate(adjusted_total =  floor(counts + (total_na/100 *proportion ))) %>%
  arrange(desc(agegroup_mother == "Under 15")) 

rm(agegrp, output, output2)
write.csv(table3.11, ("./outputs/table3_11.csv"), row.names = FALSE)

#table4.1
print("creating Table 4.1")
table4.1 <- births_data %>%
  filter(!is.na(registration_year)) %>%
  group_by(registration_year, sex) %>%
  summarise(counts = n()) %>%
  pivot_wider(names_from = sex, values_from = counts, values_fill = 0) %>%
  mutate(Total_Births = sum(Male, Female),
         Ratio = round(Male/Female*100, 0)) 

pop <- pops_data %>%
  group_by(year) %>%
  summarise(Total = sum(popn))

table4.1 <- cbind(table4.1, pop) %>%
  mutate(cbr = round((Total_Births/Total)*1000,2)) %>%
  select(-year, -Total) %>%
  pivot_longer(cols = -registration_year, names_to = "Category", values_to = "Value") %>%
  pivot_wider(names_from = registration_year, values_from = Value) %>%
  arrange(desc(Category == c("Male"))) %>%
  arrange(desc(Category == c("Total_Births")))

  write.csv(table4.1, ("./outputs/partial_table4_1.csv"), row.names = FALSE)

#table4.2
print("creating Table 4.2")
table4.2 <- births_data %>%
  filter(registration_year == data_year) %>%
  group_by(birth_district, sex) %>%
  summarise(counts = n()) %>%
  pivot_wider(names_from = sex, values_from = counts, values_fill = 0) %>%
  adorn_totals("row", name = "Grand total") %>%
  adorn_totals("col", name = "Total live births") %>%
  mutate(sex_ratio = round(Male/Female*100, 0))

write.csv(table4.2, ("./outputs/partial_table4_2.csv"), row.names = FALSE)

