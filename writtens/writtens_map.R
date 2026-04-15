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


#####################################################################
#final cleaned map lets go

######################################
library(tidyverse)
library(sf)
library(rnaturalearth)
library(ggrepel)
library(here)

# 1. Read and Prep Data
edna_sf <- read_csv(here("writtens/data", "edna_3.csv")) %>%
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
  geom_sf(data = edna_sf, aes(color = combined_type), size = 1.5) +
  
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

##########
#colorblind cleaned
library(tidyverse)
library(sf)
library(rnaturalearth)
library(ggrepel)
library(here)

# 1. Read and Prep Data
edna_sf <- read_csv(here("writtens/data", "edna_3.csv")) %>%
  janitor::clean_names() %>%
  mutate(full_label = paste0(location, "\n(", author, ", ", year, ")")) %>%
  unite("combined_type", e_dna_type_1:e_dna_type_3, sep = " & ", na.rm = TRUE) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)

# 2. Get map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# 3. Plot
final_map <- ggplot() +
  geom_sf(data = world, fill = "antiquewhite1", color = "gray80", size = 0.2) +
  
  # Points with colorblind-friendly scale and smaller size
  geom_sf(data = edna_sf, aes(color = combined_type), size = 1.8, alpha = 0.9) +
  scale_color_viridis_d(option = "viridis", end = 0.9) +
  
  # Labels
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
    # 1) CENTER THE TITLE
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    
    # 2) REMOVE AXES AND GRID LINES
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
  #  panel.grid.major = element_blank(), # Removes the lat/long grid lines
  #  panel.grid.minor = element_blank(),
    
    # Legend formatting
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  ) +
  labs(color = "eDNA Substrate Type",
       title = "Metagenomic eDNA Studies for Macrobial Recovery")

print(final_map)

# 4. Save
ggsave(here("writtens/figures", "edna_map_04152026_colorblind.jpg"), 
       final_map, width = 12, height = 8, dpi = 300)




###########################
#sample size

library(tidyverse)
library(here)
library(janitor)

# 1. Read and Prep Data
edna_data <- read_csv(here("writtens/data", "edna_3.csv")) %>%
  clean_names() %>%
  # Create a clean label for each paper: "Author (Year)"
  mutate(paper_label = paste0(author, " (", year, ")")) %>%
  # Consolidate types for coloring (matching your map)
  unite("combined_type", e_dna_type_1:e_dna_type_3, sep = " & ", na.rm = TRUE)

# 2. Create the Bar Chart
n_comparison_plot <- ggplot(edna_data, aes(x = reorder(paper_label, n), y = n, fill = combined_type)) +
  geom_col() +
  # Add the sample size number at the end of each bar for quick reading
  geom_text(aes(label = n), hjust = -0.2, size = 3.5, fontface = "bold") +
  # Flip the coordinates so labels are horizontal
  coord_flip() +
  # Use the same colorblind-friendly scale as your map
  scale_fill_viridis_d(option = "viridis", end = 0.9) +
  # Make sure the bars don't get cut off by the text labels
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title.y = element_blank(), # Remove "paper_label" title
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(), # Cleaner look for horizontal bars
    legend.position = "bottom"
  ) +
  labs(
    title = "Comparison of Mwtagenomic eDNA Sample Sizes (n)",
    x = "Study",
    y = "Number of Samples (n)",
    fill = "Substrate Type"
  )

print(n_comparison_plot)

# 3. Save
ggsave(here("writtens/figures", "edna_sample_comparison.jpg"), 
       n_comparison_plot, width = 10, height = 7, dpi = 300)


#####################
#patchwork


library(tidyverse)
library(sf)
library(rnaturalearth)
library(ggrepel)
library(here)
library(patchwork) # The magic package for A) and B)

# --- 1. PREP DATA (Same as before) ---
edna_data <- read_csv(here("writtens/data", "edna_3.csv")) %>%
  janitor::clean_names() %>%
  mutate(paper_label = paste0(author, " (", year, ")"),
         full_label = paste0(location, "\n(", author, ", ", year, ")")) %>%
  unite("combined_type", e_dna_type_1:e_dna_type_3, sep = " & ", na.rm = TRUE)

edna_sf <- st_as_sf(edna_data, coords = c("long", "lat"), crs = 4326)
world <- ne_countries(scale = "medium", returnclass = "sf")

# --- 2. PLOT A (Map) ---
plot_a <- ggplot() +
  geom_sf(data = world, fill = "antiquewhite1", color = "gray80", size = 0.2) +
  geom_sf(data = edna_sf, aes(color = combined_type), size = 1.8) +
  scale_color_viridis_d(option = "viridis", end = 0.9) +
  geom_label_repel(data = edna_sf, aes(label = full_label, geometry = geometry),
                   stat = "sf_coordinates", size = 2.5, fontface = "italic",
                   box.padding = 0.8, max.overlaps = Inf) +
  coord_sf(crs = "+proj=robin") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text = element_blank(),
        panel.grid = element_blank()) +
  labs(title = "Metagenomic eDNA Study Locations", color = "Substrate Type")

# --- 3. PLOT B (Bar Chart) ---
plot_b <- ggplot(edna_data, aes(x = reorder(paper_label, n), y = n, fill = combined_type)) +
  geom_col() +
  geom_text(aes(label = n), hjust = -0.2, size = 3, fontface = "bold") +
  coord_flip() +
  scale_fill_viridis_d(option = "viridis", end = 0.9) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.y = element_blank()) +
  labs(title = "Sample Size Comparison", fill = "Substrate Type", y = "n")

# --- 4. COMBINE AND FORMAT LEGEND ---
# Using / stacks them vertically. Use + to put them side-by-side.
combined_figure <- (plot_a / plot_b) + 
  plot_layout(guides = "collect") + # This merges the legends into one
  plot_annotation(tag_levels = 'A') & # This adds the A) and B) automatically
  theme(
    legend.position = "bottom",
    # MAKE LEGEND BIG AND READABLE
    legend.title = element_text(size = 16, face = "bold"), 
    legend.text = element_text(size = 14),
    legend.key.size = unit(1, "cm") # Makes the color boxes/dots bigger too
  )

# --- 5. SAVE ---
ggsave(here("writtens/figures", "edna_combined_04152026.jpg"), 
       combined_figure, width = 12, height = 14, dpi = 300)


##########
#fixing it

library(tidyverse)
library(sf)
library(rnaturalearth)
library(ggrepel)
library(here)
library(patchwork)

# --- 1. PREP DATA ---
edna_data <- read_csv(here("writtens/data", "edna_3.csv")) %>%
  janitor::clean_names() %>%
  mutate(paper_label = paste0(author, " (", year, ")"),
         full_label = paste0(location, "\n(", author, ", ", year, ")")) %>%
  unite("combined_type", e_dna_type_1:e_dna_type_3, sep = " & ", na.rm = TRUE)

edna_sf <- st_as_sf(edna_data, coords = c("long", "lat"), crs = 4326)
world <- ne_countries(scale = "medium", returnclass = "sf")

# --- 2. PLOT A: THE MAP (Sized Larger) ---
plot_a <- ggplot() +
  geom_sf(data = world, fill = "antiquewhite1", color = "gray80", size = 0.2) +
  geom_sf(data = edna_sf, aes(color = combined_type), size = 2) +
  scale_color_viridis_d(option = "viridis", end = 0.9) +
  geom_label_repel(data = edna_sf, aes(label = full_label, geometry = geometry),
                   stat = "sf_coordinates", size = 2.5, fontface = "italic",
                   box.padding = 0.8, max.overlaps = Inf) +
  coord_sf(crs = "+proj=robin") + 
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    # KEEP grid lines but REMOVE axis labels/ticks
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_line(color = "gray90", size = 0.2) 
  ) +
  labs(title = "Metagenomic eDNA Study Locations", color = "Substrate Type")

# --- 3. PLOT B: THE BAR CHART ---
plot_b <- ggplot(edna_data, aes(x = reorder(paper_label, n), y = n, fill = combined_type)) +
  geom_col() +
  geom_text(aes(label = n), hjust = -0.2, size = 3, fontface = "bold") +
  coord_flip() +
  scale_fill_viridis_d(option = "viridis", end = 0.9) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title.y = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  labs(title = "Sample Size Comparison", fill = "Substrate Type", y = "Number of Samples (n)")

# --- 4. COMBINE WITH CUSTOM RATIOS ---
combined_figure <- (plot_a / plot_b) + 
  # Set the map to be 2.5x larger than the bar chart
  plot_layout(heights = c(2.5, 1), guides = "collect") + 
  plot_annotation(tag_levels = 'A') & 
  theme(
    legend.position = "bottom",
    # ENHANCED LEGEND FOR READABILITY
    legend.title = element_text(size = 14, face = "bold"), 
    legend.text = element_text(size = 12),
    legend.key.size = unit(1, "cm")
  )

# --- 5. SAVE ---
ggsave(here("writtens/figures", "edna_combined_final.jpg"), 
       combined_figure, width = 12, height = 15, dpi = 300)

#####
#round 3- clean
library(tidyverse)
library(sf)
library(rnaturalearth)
library(ggrepel)
library(here)
library(patchwork)

# --- 1. PREP DATA ---
edna_data <- read_csv(here("writtens/data", "edna_3.csv")) %>%
  janitor::clean_names() %>%
  mutate(paper_label = paste0(author, " (", year, ")"),
         full_label = paste0(location, "\n(", author, ", ", year, ")")) %>%
  unite("combined_type", e_dna_type_1:e_dna_type_3, sep = " & ", na.rm = TRUE)

edna_sf <- st_as_sf(edna_data, coords = c("long", "lat"), crs = 4326)
world <- ne_countries(scale = "medium", returnclass = "sf")

# --- 2. PLOT A: THE MAP ---
plot_a <- ggplot() +
  geom_sf(data = world, fill = "antiquewhite1", color = "gray80", size = 0.2) +
  geom_sf(data = edna_sf, aes(color = combined_type), size = 2) +
  scale_color_viridis_d(option = "viridis", end = 0.9) +
  geom_label_repel(data = edna_sf, aes(label = full_label, geometry = geometry),
                   stat = "sf_coordinates", size = 2.5, fontface = "italic",
                   box.padding = 0.6, max.overlaps = Inf) +
  coord_sf(crs = "+proj=robin", expand = FALSE) + # expand=FALSE helps trim edges
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_line(color = "gray90", size = 0.1),
    # REDUCE MARGINS (Top, Right, Bottom, Left)
    plot.margin = margin(t = 5, r = 0, b = -10, l = 0, unit = "pt") 
  ) +
  labs(title = "Metagenomic eDNA Study Locations", color = "Substrate Type")

# --- 3. PLOT B: THE BAR CHART ---
plot_b <- ggplot(edna_data, aes(x = reorder(paper_label, n), y = n, fill = combined_type)) +
  geom_col() +
  geom_text(aes(label = n), hjust = -0.2, size = 3, fontface = "bold") +
  coord_flip() +
  scale_fill_viridis_d(option = "viridis", end = 0.9) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title.y = element_blank(),
    panel.grid.major.y = element_blank(),
    # REDUCE MARGINS
    plot.margin = margin(t = -10, r = 0, b = 5, l = 0, unit = "pt")
  ) +
  labs(title = "Sample Size Comparison", fill = "Substrate Type", y = "Number of Samples (n)")

# --- 4. COMBINE WITH RIGHT-HAND LEGEND ---
combined_figure <- (plot_a / plot_b) + 
  # Increased the map ratio (3 to 1) to make it much bigger than the bar chart
  plot_layout(heights = c(3, 1), guides = "collect") + 
  plot_annotation(tag_levels = 'A') & 
  theme(
    # ONE LEGEND ON THE RIGHT
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"), 
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.8, "cm")
  )

# --- 5. SAVE ---
# Note: I reduced height to 10. Tall heights create that white space above/below maps.
ggsave(here("writtens/figures", "edna_combined_tight.jpg"), 
       combined_figure, width = 14, height = 10, dpi = 300)



########
library(tidyverse)
library(sf)
library(rnaturalearth)
library(ggrepel)
library(here)
library(patchwork)

# --- 1. DATA PREP ---
edna_data <- read_csv(here("writtens/data", "edna_3.csv")) %>%
  janitor::clean_names() %>%
  mutate(paper_label = paste0(author, " (", year, ")"),
         full_label = paste0(location, "\n(", author, ", ", year, ")")) %>%
  unite("combined_type", e_dna_type_1:e_dna_type_3, sep = " & ", na.rm = TRUE)

edna_sf <- st_as_sf(edna_data, coords = c("long", "lat"), crs = 4326)
world <- ne_countries(scale = "medium", returnclass = "sf")

# --- 2. PLOT A: THE MAP ---
plot_a <- ggplot() +
  geom_sf(data = world, fill = "antiquewhite1", color = "gray80", size = 0.2) +
  geom_sf(data = edna_sf, aes(color = combined_type), size = 2) +
  # Use the exact same name for both scales to help R merge them
  scale_color_viridis_d(option = "viridis", end = 0.9, name = "Substrate Type") +
  geom_label_repel(
    data = edna_sf,
    aes(label = full_label, geometry = geometry),
    stat = "sf_coordinates",
    size = 2.5,
    fontface = "italic",
    box.padding = 1.5,      # Pushes labels much further away
    point.padding = 0.5,
    force = 10,             # Increased force to push overlapping labels apart
    max.overlaps = Inf,
    segment.color = 'grey50',
    min.segment.length = 0
  ) +
  coord_sf(crs = "+proj=robin", expand = FALSE) + 
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_line(color = "gray90", size = 0.1),
    plot.margin = margin(b = -20) # Pulls plot B up closer
  ) +
  labs(title = "Metagenomic eDNA Study Locations")

# --- 3. PLOT B: THE BAR CHART ---
plot_b <- ggplot(edna_data, aes(x = reorder(paper_label, n), y = n, fill = combined_type)) +
  geom_col() +
  geom_text(aes(label = n), hjust = -0.2, size = 3, fontface = "bold") +
  coord_flip() +
  # Match the name here exactly to the map scale
  scale_fill_viridis_d(option = "viridis", end = 0.9, name = "Substrate Type") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title.y = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(t = -20) # Pulls plot A down closer
  ) +
  labs(title = "Sample Size Comparison", y = "Number of Samples (n)")

# --- 4. THE MASTER COMBINATION ---
combined_figure <- (plot_a / plot_b) + 
  plot_layout(heights = c(3.5, 1), guides = "collect") + 
  plot_annotation(tag_levels = 'A') & 
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"),
    # This guides() call forces the color and fill to share one legend entry
    legend.justification = "top"
  )

# --- 5. SAVE ---
# Making the width much wider (16) gives the labels more room to spread horizontally
ggsave(here("writtens/figures", "edna_final_tight_one_legend.jpg"), 
       combined_figure, width = 16, height = 10, dpi = 300)


######
library(tidyverse)
library(sf)
library(rnaturalearth)
library(ggrepel)
library(here)
library(patchwork)

# --- 1. DATA PREP ---
edna_data <- read_csv(here("writtens/data", "edna_3.csv")) %>%
  janitor::clean_names() %>%
  mutate(paper_label = paste0(author, " (", year, ")"),
         full_label = paste0(location, "\n(", author, ", ", year, ")")) %>%
  unite("combined_type", e_dna_type_1:e_dna_type_3, sep = " & ", na.rm = TRUE)

edna_sf <- st_as_sf(edna_data, coords = c("long", "lat"), crs = 4326)
world <- ne_countries(scale = "medium", returnclass = "sf")

# --- 2. PLOT A: THE MAP ---
plot_a <- ggplot() +
  geom_sf(data = world, fill = "antiquewhite1", color = "gray80", size = 0.2) +
  # show.legend = FALSE is the key fix here!
  geom_sf(data = edna_sf, aes(color = combined_type), size = 2, show.legend = FALSE) +
  scale_color_viridis_d(option = "viridis", end = 0.9) +
  geom_label_repel(
    data = edna_sf,
    aes(label = full_label, geometry = geometry),
    stat = "sf_coordinates",
    size = 2.5,
    fontface = "italic",
    box.padding = 1.8,      # Even more social distancing for labels
    point.padding = 0.5,
    force = 20,             # Doubled the force to push them apart
    max.overlaps = Inf,
    segment.color = 'grey50',
    min.segment.length = 0,
    seed = 42               # Setting a seed keeps the labels in the same spot every time you run it
  ) +
  coord_sf(crs = "+proj=robin", expand = FALSE) + 
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_line(color = "gray90", size = 0.1),
    plot.margin = margin(b = -30) # Aggressively pull Plot B up
  ) +
  labs(title = "Metagenomic eDNA Study Locations")

# --- 3. PLOT B: THE BAR CHART ---
plot_b <- ggplot(edna_data, aes(x = reorder(paper_label, n), y = n, fill = combined_type)) +
  geom_col() +
  geom_text(aes(label = n), hjust = -0.2, size = 3, fontface = "bold") +
  coord_flip() +
  # This legend will now represent both figures
  scale_fill_viridis_d(option = "viridis", end = 0.9, name = "Substrate Type") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title.y = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(t = -30) # Aggressively pull Plot A down
  ) +
  labs(title = "Sample Size Comparison", y = "Number of Samples (n)")

# --- 4. THE MASTER COMBINATION ---
combined_figure <- (plot_a / plot_b) + 
  plot_layout(heights = c(3.5, 1)) + 
  plot_annotation(tag_levels = 'A') & 
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  )

# --- 5. SAVE ---
# Keeping it wide (16) to give those labels room to breathe
ggsave(here("writtens/figures", "edna_final_fixed_legend.jpg"), 
       combined_figure, width = 16, height = 10, dpi = 300)
