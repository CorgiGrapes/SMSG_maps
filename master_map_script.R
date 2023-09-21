#This script will plot the range of each species. 

#load the required packages
library('viridis')  
library('viridisLite')
library('dismo')  
library('rworldmap') 
library('sf') 
library('geodata')
library('ggplot2')
library('maps')
library('rgdal')
library("dplyr")
library("letsR")



##BASIC PLOT WITHOUT ARROWS, POINTS OR SANGIHE ISLAND

# Specify the file path for the province boundaries shapefile
shapefile_path <- "/Users/thihoanguyen/Desktop/year 3/SMSG internship/shapefiles/gadm41_IDN_shp/gadm41_IDN_1.shp"
# Read the province boundaries shapefile
province_boundaries <- st_read(shapefile_path)
#Specify the file path for the species shapefile
species_shapefile_path <- "/Users/thihoanguyen/Desktop/year 3/SMSG internship/shapefiles/wetransfer_shapefiles_2023-08-15_1348/Bunomys_andrewsi.shp"
# Read the species' shapefile
species_data <- st_read(species_shapefile_path)


# Define presence levels and create a color palette
presence_levels <- 1:6  # Define all possible presence levels
custom_colors <- c( "#64b15e", "#89CFF7",  "#F0E442", "#D55E00", "#CC79A7", "#D6D6D6")# Define custom colors for each presence code
## Create a named vector for legend labels and colors
legend_data <- data.frame(PRESENCE = presence_levels, Color = custom_colors)
names(legend_data$Color) <- as.character(presence_levels)
species_data$PRESENCE <- factor(match(species_data$PRESENCE, presence_levels),
                                levels = presence_levels)

# Making the plot
ggplot() +
    geom_sf(data = province_boundaries, aes(fill = "white")) +  # Fill provinces with white color
    geom_sf(data = species_data, aes(fill = as.character(PRESENCE)), color = NA, alpha=0.8) +  # Fill species' polygons by presence code
    scale_fill_manual(values = c("white" = "white", setNames(legend_data$Color, legend_data$PRESENCE))) +  
    coord_sf(xlim = c(118.5, 125.5), ylim = c(-6, 2)) + # Set the coordinates of the plot
    theme_void() +   # Set transparent background
    theme(panel.background = element_rect(fill = "#D6F1FF")) +  # Set background color to light blue
    scale_alpha(guide = "none")+  # Remove the alpha (transparency) legend
    guides(fill = "none")  # Remove the fill legend
  




### MAP WITH ARROW FOR THE RANGES WITH SMALL EXTENTS
shapefile_path <- "/Users/thihoanguyen/Desktop/year 3/SMSG internship/shapefiles/gadm41_IDN_shp/gadm41_IDN_1.shp"
province_boundaries <- st_read(shapefile_path)
species_shapefile_path <- "/Users/thihoanguyen/Desktop/year 3/SMSG internship/shapefiles/wetransfer_shapefiles_2023-08-15_1348/Crocidura_tenebrosa.shp"
species_data <- st_read(species_shapefile_path)
presence_levels <- 1:6
custom_colors <- c( "#64b15e", "#89CFF7",  "#F0E442", "#D55E00", "#CC79A7", "#D6D6D6")
legend_data <- data.frame(presence = presence_levels, Color = custom_colors)
names(legend_data$Color) <- as.character(presence_levels)
species_data$presence <- factor(match(species_data$presence, presence_levels),
                                levels = presence_levels)

# Calculate the centroid of the species polygon
centroid <- st_centroid(species_data)
# Extract X and Y coordinates from the "geometry" column using regular expressions
geometry_string <- as.character(centroid$geometry)
centroid_coords <- as.numeric(unlist(regmatches(geometry_string, gregexpr("\\d+\\.\\d+", geometry_string))))
# Making the plot
ggplot() +
  geom_sf(data = province_boundaries, aes(fill = "white")) +
  geom_sf(data = species_data, aes(fill = as.character(presence)), color = NA, alpha=0.8) +
  geom_segment(aes(x = centroid_coords[1]-1.05, y = centroid_coords[2]-5.6, xend = centroid_coords[1]-0.25, yend = centroid_coords[2]-5.6), #edit this line to move the arrow
               arrow = arrow(type = "closed", length = unit(0.1, "inches")),
               color = "black", lineend = "round", linejoin = "round") +  # Add arrow segment
  scale_fill_manual(values = c("white" = "white", setNames(legend_data$Color, legend_data$presence)), 
                    name = "Presence Code") +
  coord_sf(xlim = c(118.5, 125.5), ylim = c(-6, 2)) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#D6F1FF")) +
  scale_alpha(guide = "none") +
  guides(fill = "none")









### MAP TO INCLUDE SANGIHE ISLAND WITH ARROW
shapefile_path <- "/Users/thihoanguyen/Desktop/year 3/SMSG internship/shapefiles/gadm41_IDN_shp/gadm41_IDN_1.shp"
province_boundaries <- st_read(shapefile_path)
species_shapefile_path <- "/Users/thihoanguyen/Desktop/year 3/SMSG internship/shapefiles/wetransfer_shapefiles_2023-08-15_1348/Prosciurillus_rosenbergii.shp"
species_data <- st_read(species_shapefile_path)
presence_levels <- 1:6 
custom_colors <- c( "#64b15e", "#89CFF7",  "#F0E442", "#D55E00", "#CC79A7", "#D6D6D6")
legend_data <- data.frame(PRESENCE = presence_levels, Color = custom_colors)
names(legend_data$Color) <- as.character(presence_levels)
species_data$PRESENCE <- factor(match(species_data$PRESENCE, presence_levels),
                                levels = presence_levels)
# Calculate the centroid of the species polygon
centroid <- st_centroid(species_data)
# Extract X and Y coordinates from the "geometry" column using regular expressions
geometry_string <- as.character(centroid$geometry)
centroid_coords <- as.numeric(unlist(regmatches(geometry_string, gregexpr("\\d+\\.\\d+", geometry_string))))
# Making the plot
ggplot() +
  geom_sf(data = province_boundaries, aes(fill = "white")) +
  geom_sf(data = species_data, aes(fill = as.character(PRESENCE)), color = NA, alpha=0.8) +
  geom_segment(aes(x = centroid_coords[1]-1, y = centroid_coords[2], xend = centroid_coords[1]-0.15, yend = centroid_coords[2]),
               arrow = arrow(type = "closed", length = unit(0.1, "inches")),
               color = "black", lineend = "round", linejoin = "round") +  # Add arrow segment
  scale_fill_manual(values = c("white" = "white", setNames(legend_data$Color, legend_data$PRESENCE)), 
                    name = "Presence Code") +
  coord_sf(xlim = c(118.5, 125.5), ylim = c(-6, 4)) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#D6F1FF")) +
  scale_alpha(guide = "none") +
  guides(fill = "none")













## ADDING SPECIMEN COORDINATES TO PLOT
shapefile_path <- "/Users/thihoanguyen/Desktop/year 3/SMSG internship/shapefiles/gadm41_IDN_shp/gadm41_IDN_1.shp"
province_boundaries <- st_read(shapefile_path)
species_shapefile_path <- "/Users/thihoanguyen/Desktop/year 3/SMSG internship/shapefiles/wetransfer_shapefiles_2023-08-15_1348/Taeromys_punicans.shp"
species_data <- st_read(species_shapefile_path)
presence_levels <- 1:6
custom_colors <- c( "#64b15e", "#89CFF7",  "#F0E442", "#D55E00", "#CC79A7", "#D6D6D6")
legend_data <- data.frame(PRESENCE = presence_levels, Color = custom_colors)
names(legend_data$Color) <- as.character(presence_levels)
species_data$PRESENCE <- factor(match(species_data$PRESENCE, presence_levels),
                                levels = presence_levels)

# Create a data frame with major cities' coordinates
specimen_localities <- data.frame(
  name = c("Pinedapa", "Mt. Balease"),
  lon = c(120.54, 120.54),
  lat = c(-1.402, -2.4)
)
# Convert the data frame to a spatial points dataset
specimen_localities_sf <- st_as_sf(specimen_localities, coords = c("lon", "lat"), crs = st_crs(province_boundaries))
# Making the plot
ggplot() +
  geom_sf(data = province_boundaries, aes(fill = "white")) +  # Fill provinces with white color
  geom_sf(data = species_data, aes(fill = as.character(PRESENCE)), color = NA, alpha=0.8) + 
  geom_sf(data = specimen_localities_sf, color = "black", size = 1.35, shape=16) +  # Plot major cities
  scale_fill_manual(values = c("white" = "white", setNames(legend_data$Color, legend_data$PRESENCE))) +  
  coord_sf(xlim = c(118.5, 125.5), ylim = c(-6, 2)) + # Set the coordinates of the plot
  theme_void() +   # Set transparent background
  theme(panel.background = element_rect(fill = "#D6F1FF")) +  # Set background color to light blue
  scale_alpha(guide = "none")+  # Remove the alpha (transparency) legend
  guides(fill = "none")  # Remove the fill legend





