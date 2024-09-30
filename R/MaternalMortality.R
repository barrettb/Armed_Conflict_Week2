## -----------------------------------------------------------------------------
library(here)
library(tidyverse)
here()
maternal_df <- read.csv(here('original', 'maternalmortality.csv'),header=TRUE)
read_data <- function(csv){
  data <- read.csv(here('original', csv),header=TRUE)
}
under5_df <- read_data('under5mortality.csv')
neonatal_df <- read_data('neonatalmortality.csv')
inf_df <- read_data('infantmortality.csv')


## -----------------------------------------------------------------------------
edit_df <- function(data,var_name){
  data <- data %>% select(Country.Name,X2000:X2019)
  data <- data %>% tidyr::pivot_longer(
  cols = dplyr::starts_with("X"),
  names_to= "year",
  names_prefix = "X",
  values_to = var_name
  
) %>%
  mutate(year=as.numeric(year))
}



## -----------------------------------------------------------------------------
mat_df <- edit_df(maternal_df, 'MatMor')
under5_df <- edit_df(under5_df,'U5Mor')
neonatal_df <- edit_df(neonatal_df,'NeoMor')
inf_df <- edit_df(inf_df,'InfMor')


## -----------------------------------------------------------------------------
datasets <- list(mat_df,under5_df,neonatal_df,inf_df)

merged <- reduce(datasets, full_join, by = c("Country.Name","year"))



## -----------------------------------------------------------------------------
library(countrycode)
merged$ISO <- countrycode(merged$Country.Name,
origin = "country.name",
destination = "iso3c")



## -----------------------------------------------------------------------------
view(merged)


