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



######################################
library(tidyverse)
library(sf)
library(rnaturalearth)
library(ggrepel)
library(here)

# 1. Read and Prep Data
edna_sf <- read_csv(here("writtens/data", "edna_2.csv")) %>%
  janitor::clean_names() %>%
  # 1) CREATE FULL LABELS (Location + Author + Year)
  # \n creates a line break for cleaner looking labels
  mutate(full_label = paste0(location, "\n(", author, ", ", year, ")")) %>%
  
  # 2) CONSOLIDATE EDNA TYPES
  # We combine the 3 columns into one, removing the "NA" strings
  unite("combined_type", e_dna_type_1:e_dna_type_3, sep = " & ", na.rm = TRUE) %>%
  
  # Convert to spatial object
  st_as_sf(coords = c("long", "lat"), crs = 4326)

# 2. Get map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# 3. Plot
final_map <- ggplot() +
  geom_sf(data = world, fill = "antiquewhite1", color = "gray80", size = 0.2) +
  
  # 3) COLOR BY TYPE
  geom_sf(data = edna_sf, aes(color = combined_type), size = 1.2) +
  
  # 4) ADJUST LABELS TO PREVENT OVERLAP
  geom_label_repel(
    data = edna_sf,
    aes(label = full_label, geometry = geometry),
    stat = "sf_coordinates",
    size = 3,
    fontface = "italic",
    box.padding = 0.8,      # Increases distance between label and point
    point.padding = 0.5,    # Space around the dot
    max.overlaps = Inf,     # Force it to show all labels even if crowded
    segment.color = 'grey50',
    min.segment.length = 0  # Always draw the line connecting label to point
  ) +
  
  coord_sf(crs = "+proj=robin") + 
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_text(face = "bold")) +
  labs(color = "eDNA Substrate Type",
       title = "Global eDNA Metagenomic Studies")

print(final_map)

# 4. Save
ggsave(here("writtens/figures", "edna_map_final.jpg"), 
       final_map, width = 12, height = 8, dpi = 300)

############
#color blind


# 3. Plot with Colorblind-Friendly Palette
final_map <- ggplot() +
  geom_sf(data = world, fill = "antiquewhite1", color = "gray80", size = 0.2) +
  
  # Color by type using the smaller size we discussed
  geom_sf(data = edna_sf, aes(color = combined_type), size = 1.2, alpha = 0.9) +
  
  # APPLY COLORBLIND FRIENDLY SCALE
  # option = "viridis" is the standard (purple-to-yellow)
  # option = "cividis" is specifically optimized for color vision deficiency
  scale_color_viridis_d(option = "viridis", end = 0.9) + 
  
  geom_label_repel(
    data = edna_sf,
    aes(label = full_label, geometry = geometry),
    stat = "sf_coordinates",
    size = 2.8,
    fontface = "italic",
    box.padding = 0.8,
    point.padding = 0.5,
    max.overlaps = Inf,
    segment.color = 'grey50',
    min.segment.length = 0
  ) +
  
  coord_sf(crs = "+proj=robin") + 
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "gray95")
  ) +
  labs(color = "eDNA Substrate Type",
       title = "Global eDNA Metagenomic Studies")

print(final_map)
ggsave(here("writtens/figures", "edna_map_colorblind.jpg"), 
       final_map, width = 12, height = 8, dpi = 300)



