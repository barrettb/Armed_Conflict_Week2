library(here)
library(dplyr)

df <- read.csv(here("data", "finaldata.csv"), header=TRUE)

df <- df %>%
  select(-year_lag, -Country.Name)

df$total_droughts[!df$total_droughts %in% c(0, 1, 2, 3, 4)] <- 0
df$total_earthquakes[!df$total_earthquakes %in% 0:11] <- 0

library(knitr)

grouped_df <- df %>%
  group_by(ISO) %>%
  summarise(
    Gdp1000Mean = round(mean(gdp1000, na.rm = TRUE), 2),
    OECD_Mean = round(mean(OECD, na.rm = TRUE), 2),
    PopDens_Mean = round(mean(popdens, na.rm = TRUE), 2),
    Mean_Urban = round(mean(urban, na.rm = TRUE), 2),
    AgeDep_Mean = round(mean(agedep, na.rm = TRUE), 2),
    Average_Male_Education = round(mean(male_edu, na.rm = TRUE), 2),
    Total_Rainfall = round(mean(rainfall1000, na.rm = TRUE), 2),
    Total_Earthquakes = round(mean(total_earthquakes, na.rm = TRUE), 2),
    Total_Droughts = round(mean(total_droughts, na.rm = TRUE), 2),
    Armed_Conflict_Count = sum(armed_dummy, na.rm = TRUE),  
    Maternal_Mortality = sum(MatMor, na.rm = TRUE)
  ) %>%
  mutate(
    Armed_Conflict_Category = case_when(
      Armed_Conflict_Count == 0 ~ 0,
      Armed_Conflict_Count < 5 ~ 1,
      Armed_Conflict_Count >= 5 ~ 2
    )
  )

stratified_summary <- grouped_df %>%
  group_by(Armed_Conflict_Category) %>%
  summarise(
    Count = n(),
    Avg_Gdp1000Mean = round(mean(Gdp1000Mean, na.rm = TRUE), 2),
    Avg_OECD_Mean = round(mean(OECD_Mean, na.rm = TRUE), 2),
    Avg_PopDens_Mean = round(mean(PopDens_Mean, na.rm = TRUE), 2),
    Avg_Mean_Urban = round(mean(Mean_Urban, na.rm = TRUE), 2),
    Avg_AgeDep_Mean = round(mean(AgeDep_Mean, na.rm = TRUE), 2),
    Avg_Average_Male_Education = round(mean(Average_Male_Education, na.rm = TRUE), 2),
    Avg_Total_Rainfall = round(mean(Total_Rainfall, na.rm = TRUE), 2),
    Avg_Total_Earthquakes = round(mean(Total_Earthquakes, na.rm = TRUE), 2),
    Avg_Total_Droughts = round(mean(Total_Droughts, na.rm = TRUE), 2),
    Total_Maternal_Mortality = sum(Maternal_Mortality, na.rm = TRUE)
  )

gt_stratified_table <- stratified_summary %>%
  gt() %>%
  tab_header(
    title = "Stratified Summary by Armed Conflict Category",
    subtitle = "Summary statistics for each armed conflict category"
  )

gtsave(gt_stratified_table, filename = "stratified_summary_table.html")
