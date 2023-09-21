#This script will plot the provinces of Sulawesi along with the major cities

# Load the necessary libraries
library(sf)
library(ggplot2)
library(viridisLite)

# Specify the file path for the province boundaries shapefile
shapefile_path <- "/Users/thihoanguyen/Desktop/year 3/SMSG internship/shapefiles/gadm41_IDN_shp/gadm41_IDN_1.shp"

# Read the province boundaries shapefile
province_boundaries <- st_read(shapefile_path)

# Create a data frame with major cities' coordinates
cities <- data.frame(
  name = c("Makassar", "Manado", "Palu", "Kendari", "Gorontalo"),
  lon = c(119.423790, 124.842079, 119.877999, 122.514900, 123.0568),
  lat = c(-5.135399, 1.474831, -0.900292, -3.972201, 0.5435),
  nudge_x = c(-1, -0.95, -0.8, 0.9, 0),  # Adjust x-axis nudging for each city
  nudge_y = c(0.2, 0, 0.1, 0.15, -0.4)  # Adjust y-axis nudging for each city
)

# Convert the data frame to a spatial points dataset
major_cities_sf <- st_as_sf(cities, coords = c("lon", "lat"), crs = st_crs(province_boundaries))

# List of provinces to include in the plot
provinces_to_include <- c("Sulawesi Utara", "Gorontalo", "Sulawesi Tengah", "Sulawesi Barat", "Sulawesi Selatan","Sulawesi Tenggara" )

# Filter the province boundaries data to include only the specified provinces
filtered_province_boundaries <- province_boundaries[province_boundaries$NAME_1 %in% provinces_to_include, ]


# Generate a color-blind friendly color palette using viridis
num_provinces <- length(unique(filtered_province_boundaries$NAME_1))
province_colors <- viridis(num_provinces)

# Plot the province boundaries and major cities using ggplot2
ggplot() +
  geom_sf(data = filtered_province_boundaries, aes(fill = NAME_1)) +  # Color provinces differently
  geom_sf(data = major_cities_sf, color = "red", size = 3) +  # Plot major cities
  geom_text(data = cities, aes(label = name, x = lon + nudge_x , y = lat+ nudge_y), size=5) +  # Add city labels
  ggtitle("Province Boundaries and Major Cities in Sulawesi") +
  coord_sf(xlim = c(117.5, 125), ylim = c(-6, 2)) + # Zoom into Sulawesi
  scale_fill_manual(values = province_colors) +  # Set custom province colors
  labs(fill = NULL) +  # Remove legend title for the color scale
  theme_void() +  # Set transparent background
  theme(legend.key.size = unit(0.5, "cm"), 
        plot.title = element_text(size = 16, face = "bold"))  # Adjust the size of the legend keys

