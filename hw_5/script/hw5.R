#HW 5

library("adegenet")
library(here)


# Load required libraries
library(adegenet)

# Load the ape package
library(ape)

# Load the woodmouse dataset
data(woodmouse)

# Inspect the dataset
woodmouse
library(pegas)
library(adegenet)
library(ape)       # For the woodmouse dataset
library(pegas)     # For DNAbin2genind function
library(adegenet)  # For PCA and other analyses


# Convert woodmouse to genind format
woodmouse_genind <- DNAbin2genind(woodmouse)

# Inspect the genind object
summary(woodmouse_genind)

# Perform PCA on woodmouse data
woodmouse_scaled <- scaleGen(woodmouse_genind, NA.method = "mean")
woodmouse_pca <- dudi.pca(woodmouse_scaled, center = TRUE, scale = FALSE, scannf = FALSE, nf = 3)

# Visualize PCA
s.class(woodmouse_pca$li, fac = as.factor(1:nInd(woodmouse_genind)), col = rainbow(nInd(woodmouse_genind)))


# Load the woodmouse dataset
data(woodmouse)

# Convert the DNA sequences to genind format
woodmouse_genind <- DNAbin2genind(woodmouse)

# Inspect the genind object
summary(woodmouse_genind)

# Scale the genetic data
woodmouse_scaled <- scaleGen(woodmouse_genind, NA.method = "mean")

# Perform PCA
woodmouse_pca <- dudi.pca(woodmouse_scaled, center = TRUE, scale = FALSE, scannf = FALSE, nf = 3)

# Inspect PCA results
woodmouse_pca

# Create a factor for the individual labels
ind_labels <- as.factor(indNames(woodmouse_genind))

# Plot PCA results with labeled groups
pca_fig <- s.class(woodmouse_pca$li, fac = ind_labels, col = rainbow(length(unique(ind_labels))))
# Save PCA figure as a PNG file
ggsave("PCA_woodmouse_plot.png", plot = last_plot(), width = 8, height = 6, dpi = 300)

ggsave(here("hw_5", "PCA_woodmouse_plot.png"), pca_fig, dpi=300, height=4, width=6, unit="in")

# Save PCA figure using base R graphic device
png(here("hw_5", "PCA_woodmouse_plot.png"), width = 6, height = 4, units = "in", res = 300)
s.class(woodmouse_pca$li, fac = pop_factor, col = rainbow(length(unique(pop_factor))))
dev.off()

