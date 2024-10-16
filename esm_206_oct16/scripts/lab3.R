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
