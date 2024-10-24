##hw 1


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
urban_data_raw <- read_csv(here("hw_1/data", "urban_data.csv")) |>
  clean_names()

urban_data_wo_accent <- urban_data_raw %>%
  mutate(city = str_replace_all(city, "Quer\x8etaro", "Queretaro"))

#data types
class(urban_data_raw$city)
class(urban_data_raw$total_bird)
class(urban_data_raw$total_plant)

#find which ones are n and s america
unique(urban_data_raw$country)
n_am <- c("Canada", "USA", "Mexico", 
          "Guatemala", "Belize", "El Salvador", 
          "Honduras", "Nicaragua", "Costa Rica", 
          "Panama", "Bahamas", "Cuba", 
          "Jamaica", "Haiti", "Dominican Republic", 
          "Barbados", "Saint Lucia", "Grenada", 
          "Trinidad and Tobago", "Saint Vincent and the Grenadines", 
          "Saint Kitts and Nevis", "Antigua and Barbuda", "Dominica")
s_am <- c("Argentina", "Bolivia", "Brazil", 
         "Chile", "Colombia", "Ecuador", 
         "Guyana", "Paraguay", "Peru", 
         "Suriname", "Uruguay", "Venezuela", "French Guyana")
  
##urban_data_q3 <- urban_data_raw |>
  #filter(country %in% n_am | country %in% s_am) |> 
  #pivot_longer(cols = starts_with("total") | starts_with("exotic") | starts_with("native"),
               #names_to = c("origin", "species_type"),
               #names_sep = "_", 
               #values_to = "count") |>
  #select(city, country, species_type, origin, count) |>
  #na.omit()

#you need to remove the plants
urban_data_q3 <- urban_data_wo_accent |>
  select(city, country, exotic_bird, native_bird, total_bird) |>
  drop_na() |>
  filter(country %in% c(n_am, s_am)) |>
  pivot_longer(cols= exotic_bird:total_bird,
               names_to = "species_type", values_to = "species_richness")
urban_data_q3

#39 observ and 4 variables
##################################################

##question 4
#The authors look at the data by realm, not continents. Make a new variable
#and identify each observations continent. Provide your code and report how many
#cities are in North America 

continents <- data.frame(
  continent= c(rep("North America", length(n_am)),
               rep("South America", length(s_am))),
  country = c(n_am, s_am))

urban_data_q4_pt1 <- urban_data_q3 |>
  full_join(continents) |>
  na.omit()

urban_data_q4 <- urban_data_q4_pt1 |>
  mutate(city = str_replace_all(city, "Quer\x8etaro", "Queretaro"))
  
#unique cities per continent
cities_in_n_am <- unique(urban_data_q4$city[urban_data_q4$continent== "North America"])
print(cities_in_n_am)
#10 cities

cities_in_s_am <- unique(urban_data_q4$city[urban_data_q4$continent== "South America"])
print(cities_in_s_am)
#2 cities
###########################################

##question 5
#(Proficient) Use your expertise in data visualization to improve upon Figure 1 in the
#paper to explain the species richness of exotic and native bird species in North and
#South American cities. You may use figure styles other than boxplots. The figure should
#follow the best practices of figure creation and captions.
urban_data_q5 <-  urban_data_q4 |>
  filter(species_type != "total_bird")

geomcol_bird_americas <- ggplot(data = urban_data_q5, aes(x = city, y = species_richness, fill = species_type)) +
  geom_col() + 
  labs(title = "Exotic and Native Birds by City in North and South America",
       x = "City",
       y = "Count") +
  facet_grid(. ~ continent, scales = "free_x", space = "free_x") +  # This separates the plots by continent
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8),
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.line = element_line(color = "black"),
    strip.text = element_text(size = 12, face = "bold"),  # Adjust facet label text size and style
    plot.margin = margin(t = 10, r = 20, b = 10, l = 20)
  )
  
# Display the plot
print(geomcol_bird_americas)

ggsave(here("hw_1/figures", "q_5.jpg"), geomcol_bird_americas, dpi=500,
       width=10, height=5, unit="in")
####################################################

##question 6


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
                   size=2, box.padding = 0.75, point.padding = 0.2, segment.color= "black",
                   max.overlaps = 1000)
base_cities_labels

ggsave(here("hw_1/figures", "q_6.jpg"), base_cities_labels, dpi=500,
       width=8, height=10, unit="in")




