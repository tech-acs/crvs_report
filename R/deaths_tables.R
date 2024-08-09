data_year <- 2022

# partial Table5.1
print("Creating Table 5.1 (partial)")
output <- deaths_data %>%
  filter(sex != "Unknown" & registration_year > 2019) %>%
  group_by(sex, registration_year) %>%
  rename(Indicator = sex) %>%
  summarise(total = n()) 

deaths_out <- output %>%
  pivot_wider(names_from = registration_year, values_from = total, values_fill = 0) %>%
  adorn_totals("row") %>%
  arrange(desc(Indicator))

population <- pops_data %>%
  filter(year > 2019) %>%
  group_by(year, sex)  %>%
  summarise(total_pop = sum(popn))  %>%
  arrange(sex)

output_cdr <- cbind(output, population) %>%
  select(year, Indicator, total, total_pop ) %>%
  group_by(year) %>%
  summarise(total = sum(total), total_pop = sum(total_pop)) %>%
  mutate(cdr = round((total/total_pop)*1000,2)) %>%
  select(year, cdr) %>%
  mutate(Indicator = "CDR") %>%
  pivot_wider(names_from = year, values_from = cdr)

dthu5 <- deaths_data %>%
  filter(registration_year & age_at_death < 5) %>%
  group_by(registration_year) %>%
  summarise(reg_dths = n())

bths <- births_data %>%
  group_by(registration_year)%>%
  summarise(total = n())

under5 <- merge(dthu5, bths, by.x = "registration_year", by.y = "registration_year") %>%
  mutate(u5_rate = round((reg_dths/total)*1000, 2),
         year = registration_year,
         Indicator = "Under-5 mortality") %>%
  select(year, Indicator, u5_rate) %>%
  pivot_wider(names_from = year, values_from = u5_rate)

rm(bths, population, dthu5, output)

table5.1 <- rbind(deaths_out, output_cdr, under5)

write.csv(table5.1, ("./outputs/partial_table5_1.csv"), row.names = FALSE)

# Table 5.2
print("Creating Table 5.2")
table5.2 <- deaths_data %>%
  filter(registration_year == data_year) %>%
  group_by(residence_district, sex) %>%
  summarise(total = n()) %>%
  pivot_wider(names_from = sex, values_from = total, values_fill = 0) %>%
  adorn_totals(c("col", "row")) %>%
  select(residence_district, Male, Female, Unknown, Total)

write.csv(table5.2, "./outputs/table5_2.csv", row.names = FALSE)

# Table5.3
print("Creating Table 5.3")
table5.3 <- deaths_data %>%
  filter(registration_year %in% c(data_year) & sex == "Male") %>%
  group_by(occurrence_district, location) %>%
  summarise(total = n()) %>%
  pivot_wider(names_from = location, values_from = total, values_fill = 0) %>%
  adorn_totals(c("row","col"))

write.csv(table5.3, "./outputs/table5_3.csv", row.names = FALSE)

# Table5.4
print("Creating Table 5.4")
table5.4 <- deaths_data %>%
  filter(registration_year %in% c(data_year) & sex == "Female") %>%
  group_by(occurrence_district, location) %>%
  summarise(total = n()) %>%
  pivot_wider(names_from = location, values_from = total, values_fill = 0) %>%
  adorn_totals(c("row","col"))

write.csv(table5.4, "./outputs/table5_4.csv", row.names = FALSE)


# Table5.6
print("Creating Table 5.6")
table5.6 <- deaths_data %>%
  filter(registration_year %in% c(data_year) ) %>%
  group_by(age_group_80, sex) %>%
  summarise(total = n()) %>%
  pivot_wider(names_from = sex, values_from = total, values_fill = 0) %>%
  adorn_totals(c("row","col")) %>%
  arrange(desc(age_group_80 == "<1"))

write.csv(table5.6, "./outputs/table5_6.csv", row.names = FALSE)


