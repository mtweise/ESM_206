##hw 1


library(janitor)
library(here)
library(tidyverse)

##read in data
urban_data_raw <- read_csv(here("hw_1/data", "urban_data.csv")) |>
  clean_names()

#data types
class(urban_data_raw$city)
class(urban_data_raw$total_bird)
class(urban_data_raw$total_plant)


urban_data_q3 <- urban_data_raw |>
  