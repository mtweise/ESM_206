#modified from ESM-206 HW1 (taught by Dr. Chris Jerde)
#map of eDNA shitgun sequencing papers

#################################
library(janitor)
library(here)
library(tidyverse)
library(tidyr)
library(tidygeocoder)
library(dplyr)
library(stringr)
#Spatial libraries
library(sf) #This helps with plotting boundaries and lots of other things
library(rnaturalearth) #base commands and some maps
library(ggspatial) #north arrow and scale
library(ggrepel) #labels

##read in data
edna_locations_raw <- read_csv(here("writtens/data", "edna_locations"))|>
  clean_names()


#data types
class(urban_data_raw$city)
class(urban_data_raw$total_bird)
class(urban_data_raw$total_plant)


##question 6 modified


continents <- data.frame(
  continent= c(rep("North America", length(n_am)),
               rep("South America", length(s_am))),
  country = c(n_am, s_am))




#i need to make a df with coordinates for each city
cities_americas <- unique(urban_data_q4$city)

# Create a data frame with cities and their corresponding latitude and longitude
city_coords <- data.frame(
  city = c("La Paz", "Porto Alegre", "Ottawa", "Vancouver", "Mexico City", 
           "Morelia", "Queretaro", "Ames", "Baltimore", "Boston", 
           "Chicago", "Concord", "Detroit", "Fresno", "Indianapolis", 
           "Los Angeles", "Minneapolis", "New York", "Philadelphia", 
           "Saint Louis", "San Diego", "San Francisco", "Seattle", 
           "Tucson", "Washington, DC", "Worcester", "Cayenne"),
  
  latitude = c(-16.5000, -30.0331, 45.4215, 49.2827, 19.4326, 
               19.7054, 20.5884, 42.0340, 39.2904, 42.3601, 
               41.8781, 43.2081, 42.3314, 36.7378, 39.7684, 
               34.0522, 44.9778, 40.7128, 39.9526, 
               38.6270, 32.7157, 37.7749, 47.6062, 
               32.2226, 38.8951, 42.2626, 4.937200),
  
  longitude = c(-68.1193, -51.2300, -75.6972, -123.1207, -99.1332, 
                -101.1823, -100.3880, -93.6150, -76.6122, -71.0589, 
                -87.6298, -71.5370, -83.0458, -119.7871, -86.1581, 
                -118.2437, -93.2650, -74.0060, -75.1652, 
                -90.1994, -117.1611, -122.4194, -122.3321, 
                -110.9747, -77.0369, -71.8023, -52.326000)
)

#join coordinate df with existing df
urban_data_q6 <- urban_data_q4 |>
  full_join(city_coords) |>
  drop_na()

world_sf <- ne_countries(returnclass = "sf")

city_locations <- urban_data_q6 |>
  select(city, longitude, latitude) |>
  distinct()

base_cities <- ggplot()+
  geom_sf(data= world_sf, fill= NA, color= "black")+
  geom_point(city_locations, mapping= aes(x=longitude, y=latitude), color="red",
             size=2)+
  geom_point(city_locations, mapping=aes(x=longitude, y=latitude), color="black",
             size=0.5)+
  xlim(-170, -30) +  # Set longitude limits if necessary
  ylim(-60, 80) +
  theme_void()
print(base_cities)

base_cities_labels <- base_cities+
  geom_label_repel(data=city_locations, aes(x=longitude, y=latitude, label= city),
                   size=3, box.padding = 0.75, point.padding = 0.2, segment.color= "black",
                   max.overlaps = 1000)
base_cities_labels

ggsave(here("writtens/figures", ".jpg"), base_cities_labels, dpi=500,
       width=8, height=9, unit="in")

