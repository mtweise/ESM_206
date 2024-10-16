#Lab 3
###########
#clear the environment
rm(list=ls())

#load libraries
library(tidyverse)
library(here)
library(janitor)
library(rnaturalearth)
library(ggspatial)
library(ggrepel)

#import data for joining
lizard_names <- read_csv(here("esm_206_oct16", "data", "raw_data", "lizard_abb.csv")) |>
  clean_names()

lizard_traps <- read_csv(here("esm_206_oct16", "data", "raw_data", "jornada_lizards.csv")) |>
  clean_names()

#full join
lizard_join <- lizard_traps |>
  full_join(lizard_names)

#full join with different variable names
lizard_traps_rn <- lizard_traps|>
  rename(lizard_names= common_name)

lizard_join_2 <- lizard_traps_rn|>
  full_join(lizard_names, by=c("lizard_names"= "common_name"))

#Get a summary
tail_counts <- lizard_join |>
  filter(common_name %in% c("Western Whiptail", "Eastern Fence")) |>
  count(common_name, tail) |> drop_na()

tail_counts_fig <- ggplot(tail_counts, aes(x= common_name, y=n, fill= tail))+
  geom_col()+
  theme_bw()
tail_counts_fig



