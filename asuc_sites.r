library(tidyverse)
library(janitor)
library(readxl)

ann_dat<- read_xlsx("fd2396_ASUC_final_report.xlsx", sheet = "Table_1.A",  skip =7) |> 
  select(Characteristics, `26`, ends_with("Mean")) |> 
  select(-3 ) |>
  slice(2:76) |> 
  rename("DataMart 01" = `26`)

names(ann_dat) <- str_sub(names(ann_dat), start = 1L, end = 11L) 

crosswalk <- read_xlsx("FD2396_anonymous_datamart_id_crosswalk.xlsx") %>%
  clean_names() %>%
  select(2,5,7) %>%
  rename(datamart = anonymous_datamart_id,
         network = network_name,
         site = datamart_name) |> 
  slice(1:62) |> 
  t() |> 
  as.data.frame() |>
  rownames_to_column("Characteristics") |> 
  row_to_names(row_number = 1) |> 
  rename("Characteristics" = datamart)

sites <- read_xlsx("FD2396_anonymous_datamart_id_crosswalk.xlsx") %>%
  clean_names() %>%
  select(2,5,7) %>%
  rename(datamart = anonymous_datamart_id,
         network = network_name,
         site = datamart_name) |> 
  slice(1:62) 



merged <- bind_rows(ann_dat, crosswalk)  
merged$Characteris[76] <- 'Network'
merged$Characteris[77] <- 'site'



tall <- merged|> 
  slice(1, 77) |> 
  pivot_longer(cols = -Characteris, names_to = "datamart", values_to = "count") 


site_count <- left_join(tall, sites) |> 
  slice(1:62) |>
  mutate(count = as.numeric(count)) |> 
  arrange(desc(count))

site_count |>
  filter(!is.na(count)) |>
  ggplot(aes(y = count, x = reorder(site, count), fill = network)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Unique ASUC Patients per Site in 2024",
       x = "Number of Patients",
       y = "Site") +
  theme_minimal() +
  coord_flip()
  
  