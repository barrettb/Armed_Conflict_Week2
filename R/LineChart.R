library(dplyr)
library(ggplot2)
library(here)

finaldata <- read.csv(here("data", "finaldata.csv"), header = TRUE)

finaldata <- finaldata |>
  select(country_name, ISO, year, MatMor) |>
  filter(year < 2018) |>
  arrange(ISO, year) |>
  group_by(ISO) |>
  mutate(diffmatmor = MatMor - first(MatMor))

positive_diff_ISO <- data.frame()

for (iso in unique(finaldata$ISO)) {
  iso_data <- finaldata |>
    filter(ISO == iso, year == 2017)
  
  if (nrow(iso_data) > 0 && (iso_data$diffmatmor > 0 | is.na(iso_data$diffmatmor))) {
    positive_diff_ISO <- rbind(positive_diff_ISO, iso_data)
  }
}

iso_list <- unique(positive_diff_ISO$ISO)

finaldata <- finaldata %>% filter(ISO %in% iso_list)

ggplot(finaldata, aes(x = year, y = MatMor, color = ISO, group = ISO)) +
  geom_line() +
  geom_point() +
  labs(title = "Maternal Mortality Over Years by ISO",
       x = "Year",
       y = "Maternal Mortality") +
  theme_minimal() +
  scale_color_discrete(name = "ISO")
