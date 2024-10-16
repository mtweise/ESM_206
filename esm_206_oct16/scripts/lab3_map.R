# lab 3 map
#ESM 206
#######################################
#clean the environment
rm(list=ls())




#load libraries
library(tidyverse)
library(here)
library(janitor)

#Spatial libraries
library(sf) #This helps with plotting boundaries and lots of other things
library(rnaturalearth) #base commands and some maps
library(ggspatial) #north arrow and scale
library(ggrepel) #labels

#data
ssn_locs <- read_csv("esm_206_oct16/data/raw_data/map_data.csv")

#get base maps
us_sf <- ne_states(geounit = "United States of America", returnclass = "sf")
mx_sf <- ne_states(geounit = "Mexico", returnclass = "sf")

#map of the NRS

nrs_locations <- ssn_locs|>
  filter(admin=="UC")

base_nrs <- ggplot()+
  geom_sf(data=us_sf, fill= NA, color="black")+
  geom_sf(data=mx_sf, fill=NA, color="black")+
  geom_point(nrs_locations, mapping=aes(x=long, y=lat), color="red",
             size=3)+
  geom_point(nrs_locations, mapping=aes(x=long, y=lat), color="black",
             size=1)+
  xlim(-124, -114)+
  ylim(32,43)+
  theme_void()
base_nrs

base_nrs_labels <- base_nrs+
  geom_label_repel(data=nrs_locations, aes(x=long, y=lat, label=short), size=2,
                   box.padding = 0.75, point.padding = 0.2, segment.color= "black",
                   max.overlaps = 1000)
base_nrs_labels


ggsave(here("esm_206_oct16/figures", "nrs_labels.jpg"), base_nrs_labels, dpi=500,
       width=6, height=8, unit="in")




