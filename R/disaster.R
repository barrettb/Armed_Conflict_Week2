## -----------------------------------------------------------------------------
library(here)
library(tidyverse)
here()

rawdat <- read.csv(here('original', 'disaster.csv'),header=TRUE)
new_dist <- read.csv(here('original', 'disaster.csv'),header=TRUE)
filtered_data <- rawdat %>%
  filter(Year >= 2000 & Year <= 2019, Disaster.Type %in% c("Earthquake", "Drought"))
new_dist <- new_dist %>%
  filter(Year >= 2000 & Year <= 2019, Disaster.Type %in% c("Earthquake", "Drought"))



## -----------------------------------------------------------------------------
filtered_data <- filtered_data %>%
  mutate(
    drought = ifelse(Disaster.Type == "Drought", 1, 0),
    earthquake = ifelse(Disaster.Type == "Earthquake", 1, 0)
  )


## -----------------------------------------------------------------------------
new_dist <- new_dist %>%
  mutate(
    drought = ifelse(Disaster.Type == "Drought", 1, 0),
    earthquake = ifelse(Disaster.Type == "Earthquake", 1, 0)
  )
new_dist$ISO <- countrycode(new_dist$Country.Name,
origin = "country.name",
destination = "iso3c")
new_dist <- data.frame(
  ISO = new_dist$ISO,
  year = new_dist$Year,
  Dummy_Earthquake = new_dist$earthquake,
  Dummy_Drought = new_dist$drought
)


## -----------------------------------------------------------------------------
summarized_data <- filtered_data %>%
  group_by(ISO, Year) %>%
  summarize(
    total_droughts = sum(drought, na.rm = TRUE),
    total_earthquakes = sum(earthquake, na.rm = TRUE),
    .groups = 'drop'
  )
summarized_data <- summarized_data %>%
  rename(year = Year)

