library(tidyverse)
library(stringr)
library(treemap)
library(stringr)
library(zoo)
library(lubridate)


source("/Users/adamchazan/Documents/R Stuff/silver_chips_v3.R")

# Load data.
data <-
  read_csv(
    "/Users/adamchazan/Documents/School/2022-2023/Diamondback/food_inspection_2018_2020 - food_inspection_2018_2020.csv"
  )

# Create list of Restaurants in Stamp and matching IDs used in the dataset.
establishments <-
  data.frame(
    name = c(
      "Chick-fil-A",
      "The Coffee Bar",
      "Hibachi San",
      "Maryland Dairy",
      "Moby Dick",
      "Panda Express",
      "Qdoba",
      "Saladworks",
      "Subway",
      "Union Pizza"
    ),
    id = c(1597, 2341, 8480, 7349, 2353, 1605, 1613, 1607, 1611, 1610)
  )

# Filter out restaurants that aren't in stamp, and remove irrelevent columns.
stamp_data <-
  data %>% filter(Establishment_id %in% c(1597, 2341, 8480, 7349, 2353, 1605, 1613, 1607, 1611, 1610)) %>%
  select(
    Establishment_id,
    Name,
    Inspection_date,
    Inspection_results,
    Food_from_approved_source,
    Food_protected_from_contamination,
    Ill_workers_restricted,
    Proper_hand_washing,
    Cooling_time_and_temperature,
    Cold_holding_temperature,
    Hot_holding_temperature,
    Cooking_time_and_temperature,
    Reheating_time_and_temperature,
    Hot_and_cold_running_water_provided,
    Proper_sewage_disposal,
    No_bare_hand_contact,
    Adequate_hand_washing_facilities,
    Rodent_and_insects,
    Food_contact_surfaces_and_equipment
  )

# Code passes and fails to integers for easier calculation.
stamp_data$results <-
  ifelse(
    stamp_data$Inspection_results %in% c(
      "Non-Compliant - Violations Observed",
      "Critical Violations observed"
    ),
    1,
    0
  )

# Calculate percentage of failed inspections.
stamp_data <-
  stamp_data %>% group_by(Establishment_id) %>% summarise(
    percent = sum(results) / n(),
    sum = sum(results),
    n = n()
  )

# Add correct names.
stamp_data <-
  full_join(stamp_data, establishments, by = c("Establishment_id" = "id"))

# Plot the data.
stamp_plot <-
  ggplot(stamp_data, aes(reorder(name, percent), y = percent)) +
  geom_bar(stat = "identity",
           fill = "#e51d37",
           width = 0.5) +
  scale_y_continuous(labels = scales::percent) +
  theme_silver_chips_minimal() +
  theme(plot.background = element_rect(fill = 'white', color = NA_character_)) +
  theme(plot.title = element_text(size = 30)) +
  # theme(subtitle_margin = 0) +
  labs(
    x = NULL,
    y = NULL,
    title =  "Percent of food inspections with violations",
    subtitle = "Restaurants in Adele H. STAMP Student Union"
  )

stamp_plot

ggsave(
  filename = "dbk_stamp_food.png",
  plot = stamp_plot,
  width = 11,
  height = 12,
  units = "in",
  dpi = 300
)
