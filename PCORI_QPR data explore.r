library(tidyverse)
library(readxl)
library(janitor)

# Load data
ann_dat<- read_xlsx("fd2396_ASUC_final_report.xlsx", sheet = "Table_1",  skip =7) %>%
  purrr::set_names("Characteristics", "Year_2024", "PctSD_2024", "Year_2023", "PctSD_2023", "Year_2022", "PctSD_2022", "Year_2021", "PctSD_2021") %>%
  slice(2:81)                 

# Select out anti-inflamm drug use
drug_ann_dat <- ann_dat %>%
  slice(49:54) |> 
  select(Characteristics, Year_2024, Year_2023, Year_2022, Year_2021) %>%
    relocate(Characteristics, Year_2021, Year_2022, Year_2023, Year_2024) %>%
  rename(drugs_used = Characteristics)

# select for PERIOD 1 (first line drugs used in first 2 days)   
drug_ann_dat %>%
  tibble() |> 
  filter(str_detect(drugs_used,"PERIOD 1")) |> 
  pivot_longer(cols = -drugs_used, names_to = "year", values_to = "n") %>%
  mutate(year = str_remove(year, "Year_"),
         year = as.integer(year),
         n = as.integer(n)) %>%
  mutate(drugs_used = str_remove(drugs_used, "PERIOD 1 ")) ->
drug_ann_dat2

# Plot
drug_ann_dat2 %>%
  ggplot(aes(x = year, y = n, color = drugs_used)) +
  geom_line() +
  geom_point() +
  labs(title = "Drugs Used in the First 2 Days Among ASUC Patients",
       x = "Year",
       y = "Number of Patients",
       color = "Drugs Used") +
  theme_minimal()
  
# add total_jak
total_jak <- tibble(drugs_used = c("total_jak", "total_jak", "total_jak", "total_jak"),
       year = c(2021,  2022, 2023, 2024),
       n = c(52,  60,  88,  130))
#bind
ann_drug_plus <- bind_rows(drug_ann_dat2, total_jak)

# Plot with total_jak
ann_drug_plus %>%
  ggplot(aes(x = year, y = n, color = drugs_used)) +
  geom_line() +
  geom_point() +
  labs(title = "Frequency of Use of First-Line Drugs \n(Used in the First 2 Days) Among ASUC Patients\nAt 62 PCORnet Sites",
       x = "Year",
       y = "Number of Patients",
       color = "Drugs Used") +
  theme_minimal()

#look at 2nd line (rescue) drugs in PERIOD 2
drug_ann_dat %>%
  tibble() |> 
  filter(str_detect(drugs_used,"PERIOD 2")) |> 
  pivot_longer(cols = -drugs_used, names_to = "year", values_to = "n") %>%
  mutate(year = str_remove(year, "Year_"),
         year = as.integer(year),
         n = as.integer(n)) %>%
  mutate(drugs_used = str_remove(drugs_used, "PERIOD 2 ")) |> 
  replace_na(list(n = 0)) ->
drug_ann_dat_per2

#Plot period 2
# Plot with total_jak
drug_ann_dat_per2 %>%
  ggplot(aes(x = year, y = n, color = drugs_used)) +
  geom_line() +
  geom_point() +
  labs(title = "Frequency of Use of Second-Line Drugs \n(Used in Days 3-14) Among ASUC Patients\nAt 62 PCORnet Sites",
       x = "Year",
       y = "Number of Patients",
       color = "Drugs Used") +
  theme_minimal()

# add total_jak to period 2
total_jak2 <- tibble(drugs_used = c("total_jak", "total_jak", "total_jak", "total_jak"),
                    year = c(2021,  2022, 2023, 2024),
                    n = c(76,  90,  151,  209))
#bind
ann_drug_plus2 <- bind_rows(drug_ann_dat_per2, total_jak2)

# plot with total jak for period 2
# NOTE - need to subtract first line
# NOTE need to add Cyclo to plot
# rerder  legend with total jak before each jak
ann_drug_plus2 %>%
  ggplot(aes(x = year, y = n, color = drugs_used)) +
  geom_line() +
  geom_point() +
  labs(title = "Frequency of Use of Second-Line (Rescue) Drugs \n(Used in Days 3-14) Among ASUC Patients\nAt 62 PCORnet Sites",
       x = "Year",
       y = "Number of Patients",
       color = "Drugs Used") +
  theme_minimal()

