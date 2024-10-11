#Lab 1: Data gymnastics
#Maddy Weise ESM 206 Wed section Oct 2
##################################################################

#clean the environment
rm(list=ls())

#load libraries
install.packages("here")
library(here)
library(tidyverse)
install.packages("janitor")
library(janitor) #cleans the names of variables in my data

#get some data
vert_data <- read_csv(here("data", "mack_creek_verts.csv"))

#clean the data
vert_clean <- vert_data |> clean_names()

#Row operations





vert_1988 <- vert_clean |> filter(year==1988)

vert_after_1988 <- vert_clean |> filter(year > 1988)

vert_1988_after <- vert_clean |> filter(year >= 1988)

vert_no_1988 <- vert_clean |> filter(year != 1988)

vert_ONCL_all <- vert_clean |> filter(species == "ONCL")

#Column operations

vert_mod <- vert_clean |> select(year, section, species,
                                 length1, weight)

vert_mod <- vert_mod |> rename(sec=section)

#the power of a pipe operator

cutthroat_1990 <- vert_data
##did the above because it will oroduce an exact copy of the og data

cutthroat_1990 <- vert_data |> 
  clean_names() |>
  select(year, section, species, length1, weight) |>
  filter(year==1990, species=="ONCL")

#mutate to change units
cutthroat_1990 <- vert_data |> 
  clean_names() |>
  select(year, section, species, length1, weight) |>
  filter(year==1990, species=="ONCL") |>
  mutate(weight_kg = weight / 1000)


species_summary <- vert_data |>
  clean_names() |>
  select(year, species, length1, weight) |>
  drop_na(species) |>
  group_by(species) |>
  summarize(mean_length=mean(length1, na.rm=TRUE),
            mean_weight = mean(weight, na.rm=TRUE))
            
##mean weight by group(ie species) but we gotta drop NAs bc it will 
##count them as zero and affect the mean

##na.rm means remove all na's















