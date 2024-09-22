#table7.2
print("Creating Table 7.2")
table7.2  <- marriages_data %>%
  filter(year == 2021) %>%
  group_by(groom_agegroup, bride_agegroup) %>%
  summarise(counts = n()) %>%
  pivot_wider(names_from = bride_agegroup, values_from = counts, values_fill = 0) %>%
  select(groom_agegroup, `18-19`, `20-24`, `25-29`, `30-34`,
         `35-39`, `40-44`, `45-49`, `50-54`, `55-59`,
         `60+`, `Not stated`) %>%
  adorn_totals(c("row","col"))

write.csv(table7.2, "./outputs/table7_2.csv", row.names = FALSE)
