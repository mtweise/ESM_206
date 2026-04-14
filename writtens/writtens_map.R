#modified from ESM-206 HW1 (taught by Dr. Chris Jerde)
#map of eDNA shotgun sequencing papers

#################################
library(tidyverse)
library(sf)
library(rnaturalearth)
library(ggrepel)
library(here)

# 1. Read and convert to spatial object
edna_sf <- read_csv(here("writtens/data", "edna_2.csv")) %>%
  janitor::clean_names() %>%
  # crs 4326 is standard GPS coordinates (WGS84)
  st_as_sf(coords = c("long", "lat"), crs = 4326)

# 2. Get map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# 3. Plot
final_map <- ggplot() +
  geom_sf(data = world, fill = "antiquewhite1", color = "gray80", size = 0.2) +
  geom_sf(data = edna_sf, color = "steelblue", size = 2) +
  geom_label_repel(
    data = edna_sf,
    aes(label = location, geometry = geometry),
    stat = "sf_coordinates",
    size = 2.5, min.segment.length = 0
  ) +
  # Robinson projection looks much more professional for global eDNA maps
  coord_sf(crs = "+proj=robin") + 
  theme_minimal()

print(final_map)

# 4. Save
ggsave(here("writtens/figures", "edna_map_fixed.jpg"), final_map, width = 10, height = 6)
