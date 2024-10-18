##hw 1


library(janitor)
library(here)
library(tidyverse)
library(tidyr)

##read in data
urban_data_raw <- read_csv(here("hw_1/data", "urban_data.csv")) |>
  clean_names()

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
         "Suriname", "Uruguay", "Venezuela")
  
urban_data_q3 <- urban_data_raw |>
  filter(country %in% n_am | country %in% s_am) |> 
  pivot_longer(cols = starts_with("total") | starts_with("exotic") | starts_with("native"),
               names_to = c("origin", "species_type"),
               names_sep = "_", 
               values_to = "count") |>
  select(city, country, species_type, origin, count) |>
  na.omit()

##question 4
#The authors look at the data by realm, not continents. Make a new variable
#and identify each observations continent. Provide your code and report how many
#cities are in North America 

continents <- data.frame(
  continent= c(rep("North America", length(n_am)),
               rep("South America", length(s_am))),
  country = c(n_am, s_am))

urban_data_q4 <- urban_data_q3 |>
  full_join(continents) |>
  na.omit()

cities_in_n_am <- unique(urban_data_q4$city[urban_data_q4$continent== "North America"])
print(cities_in_n_am)

##question 5
#(Proficient) Use your expertise in data visualization to improve upon Figure 1 in the
#paper to explain the species richness of exotic and native bird species in North and
#South American cities. You may use figure styles other than boxplots. The figure should
#follow the best practices of figure creation and captions.

viol_bird_americas <- ggplot(data = urban_data_q4, aes(x = origin, y = count, fill = continent)) +
  geom_violin(trim = FALSE) + 
  labs(title = "Violin Plot of Exotic and Native Birds by Continent",
       x = "Bird Type",
       y = "Count") +
  facet_wrap(~ continent) +  # This separates the plots by continent
  theme_minimal() 
  



# Display the plot
print(viol_bird_americas)






