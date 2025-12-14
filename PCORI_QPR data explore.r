library(tidyverse)
library(readxl)
library(janitor)
library(gt)

# Reference on guidelines out of date
# https://academic.oup.com/intqhc/article-abstract/18/3/165/1786412
# https://pmc.ncbi.nlm.nih.gov/articles/PMC4216254/
# https://jamanetwork.com/journals/jamainternalmedicine/fullarticle/1384245
# GOOGLE origin of "medical guidelines are out of date by the time they are published"
#
# Load data
ann_dat <- read_xlsx("fd2396_ASUC_final_report.xlsx", sheet = "Table_1", skip = 7) %>%
  purrr::set_names("Characteristics", "Year_2024", "PctSD_2024", "Year_2023", "PctSD_2023", "Year_2022", "PctSD_2022", "Year_2021", "PctSD_2021") %>%
  slice(2:81)

# Work on Table 1
#
tab1 <- read_xlsx("fd2396_ASUC_final_report.xlsx", sheet = "Tab1") |>
  unite(2:3, col = "Y2024", sep = " (") |>
  mutate(
    Y2024 = str_replace(Y2024, "NA", ""),
    Y2024 = str_trim(Y2024),
    Y2024 = paste0(Y2024, ")")
  ) |>
  unite(3:4, col = "Y2023", sep = " (") |>
  mutate(
    Y2023 = str_replace(Y2023, "NA", ""),
    Y2023 = str_trim(Y2023),
    Y2023 = paste0(Y2023, ")")
  ) |>
  unite(4:5, col = "Y2022", sep = " (") |>
  mutate(
    Y2022 = str_replace(Y2022, "NA", ""),
    Y2022 = str_trim(Y2022),
    Y2022 = paste0(Y2022, ")")
  ) |>
  unite(5:6, col = "Y2021", sep = " (") |>
  mutate(
    Y2021 = str_replace(Y2021, "NA", ""),
    Y2021 = str_trim(Y2021),
    Y2021 = paste0(Y2021, ")")
  ) |>
  gt()

tab1



ann_dat |>
  slice(2:35) |>
  slice(1:10, 13:35) |>
  slice(1:16, 18:32) |>
  untangle2(regex = "^By|^Area", Characteristics, group) |>
  relocate(group) |>
  mutate(group = str_sub(group, start = 4L, end = -8L)) ->
ann_dat2

ann_dat2 <- ann_dat2[c(21, 1:19, 22:26), ]
ann_dat2[1, 1] <- "Total"
ann_dat2[1, 2] <- "Patients with ASUC"
ann_dat2 |> View()





# Select out anti-inflamm drug use
drug_ann_dat <- ann_dat %>%
  slice(49:54) |>
  select(Characteristics, Year_2024, Year_2023, Year_2022, Year_2021) %>%
  relocate(Characteristics, Year_2021, Year_2022, Year_2023, Year_2024) %>%
  rename(drugs_used = Characteristics)

# select for PERIOD 1 (first line drugs used in first 2 days)
drug_ann_dat %>%
  tibble() |>
  filter(str_detect(drugs_used, "PERIOD 1")) |>
  pivot_longer(cols = -drugs_used, names_to = "year", values_to = "n") %>%
  mutate(
    year = str_remove(year, "Year_"),
    year = as.integer(year),
    n = as.integer(n)
  ) %>%
  mutate(drugs_used = str_remove(drugs_used, "PERIOD 1 ")) ->
drug_ann_dat2

# Plot
drug_ann_dat2 %>%
  ggplot(aes(x = year, y = n, color = drugs_used)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Drugs Used in the First 2 Days Among ASUC Patients",
    x = "Year",
    y = "Number of Patients",
    color = "Drugs Used"
  ) +
  theme_minimal()

# add total_jak
total_jak <- tibble(
  drugs_used = c("total_jak", "total_jak", "total_jak", "total_jak"),
  year = c(2021, 2022, 2023, 2024),
  n = c(52, 60, 88, 130)
)
# bind
ann_drug_plus <- bind_rows(drug_ann_dat2, total_jak)

# Plot with total_jak and cyclosporine
ann_drug_plus %>%
  ggplot(aes(x = year, y = n, color = drugs_used)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Frequency of Use of First-Line Drugs \n(Used in the First 2 Days) Among ASUC Patients\nAt 62 PCORnet Sites",
    x = "Year",
    y = "Number of Patients",
    color = "Drugs Used"
  ) +
  theme_minimal()

ggsave(filename = here("per1_ann_drug.jpg"), width = 8, height = 6, units = "in", dpi = 300)


# look at 2nd line (rescue) drugs in PERIOD 2
drug_ann_dat %>%
  tibble() |>
  filter(str_detect(drugs_used, "PERIOD 2")) |>
  pivot_longer(cols = -drugs_used, names_to = "year", values_to = "n") %>%
  mutate(
    year = str_remove(year, "Year_"),
    year = as.integer(year),
    n = as.integer(n)
  ) %>%
  mutate(drugs_used = str_remove(drugs_used, "PERIOD 2 ")) |>
  replace_na(list(n = 0)) ->
drug_ann_dat_per2

# Plot period 2
# Plot with total_jak
drug_ann_dat_per2 %>%
  ggplot(aes(x = year, y = n, color = drugs_used)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Frequency of Use of Second-Line Drugs \n(Used in Days 3-14) Among ASUC Patients\nAt 62 PCORnet Sites",
    x = "Year",
    y = "Number of Patients",
    color = "Drugs Used"
  ) +
  theme_minimal()

# add total_jak and cyclo to period 2
total_jak2 <- tibble(
  drugs_used = c(
    "total_jak", "total_jak", "total_jak", "total_jak",
    "cyclosporine", "cyclosporine", "cyclosporine", "cyclosporine"
  ),
  year = c(2021, 2022, 2023, 2024, 2021, 2022, 2023, 2024),
  n = c(76, 90, 151, 209, 37, 31, 26, 43)
)

# bind
ann_drug_plus2 <- bind_rows(drug_ann_dat_per2, total_jak2)

# plot with total jak for period 2
# NOTE - need to subtract first line
# NOTE need to add Cyclo to plot#done
# rerder  legend with total jak before each jak
ann_drug_plus2 %>%
  ggplot(aes(x = year, y = n, color = drugs_used)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Frequency of Use of Second-Line (Rescue) Drugs \n(Used in Days 3-14) Among ASUC Patients\nAt 62 PCORnet Sites",
    x = "Year",
    y = "Number of Patients",
    color = "Drugs Used"
  ) +
  theme_minimal()

ggsave(filename = here("per2_ann_drug.jpg"), width = 8, height = 6, units = "in", dpi = 300)
