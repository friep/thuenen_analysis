# Ziel: wie viel mittel sind in welche Kategorien geflossen
# wurden bestimmte Maßnahmenbereiche stärker beantragt in ländlichen vs. städtischen Räumen? 

# =============================================================================
# SECTION 1: DATA LOADING
# =============================================================================

# Load the 'sf' library for handling spatial data in R
library(sf)

# Load the 'ggplot2' library for data visualization
library(ggplot2)

# Load the 'readxl' library for reading Excel files
library(readxl)

# Read the CSV file containing the 'standorte' data
standorte <- read.csv("nk_zentren_geodaten_100.csv")

# Convert the 'standorte' dataframe to a spatial object (sf) using specific columns for coordinates
# We specify that the data uses the WGS 84 coordinate system (crs = 4326)
standorte_sf <- st_as_sf(standorte, coords = c("z_laengengrad", "z_breitengrad"), crs = 4326)

# Read the geopackage file containing the 'kreisregionen' spatial data
kreisregionen <- st_read("thuenen/gpkg_export.gpkg")

# =============================================================================
# SECTION 2: VISUALIZE FOR TESTING REASONS
# =============================================================================

# Plot the 'standorte_sf' data with each point colored by 'z_bundesland'
# The size of the points is set to 1
ggplot(data = standorte_sf) + 
  geom_sf(aes(color = "z_bundesland"), size = 1)  

# Plot the 'kreisregionen' data without any specific aesthetics
ggplot(data = kreisregionen) + 
  geom_sf(aes()) 

# Combine the 'kreisregionen' and 'standorte_sf' data in a single plot
# First, we plot 'kreisregionen' as a base layer
# Next, we overlay the 'standorte_sf' data on top, colored by 'z_bundesland'
# We also set a minimal theme for aesthetics and provide a title to the plot
ggplot() + 
  geom_sf(data = kreisregionen) + 
  geom_sf(data = standorte_sf, aes(color = z_bundesland), size = 1) +
  theme_minimal() +
  labs(title = "Standorte on Kreisregionen")

# =============================================================================
# SECTION 3: COORDINATE SYSTEM CHECK 
# =============================================================================

# Extract CRS from both spatial objects
crs_standorte <- st_crs(standorte_sf)
crs_kreisregionen <- st_crs(kreisregionen)

# Check if they are identical
are_identical <- identical(crs_standorte, crs_kreisregionen)

# Print the result
if (are_identical) {
  cat("The CRS of standorte_sf and kreisregionen are identical.\n")
} else {
  cat("The CRS of standorte_sf and kreisregionen are not identical.\n")
}

# It's weird because both have CRS "4326" but are shown as having different CRS (the string is likely different)
# To be sure, let's transform both datasets to EPSG:4326 (WGS 84) manually
kreisregionen_4326 <- st_transform(kreisregionen, 4326)
standorte_sf_4326 <- st_transform(standorte_sf, 4326)

# =============================================================================
# SECTION 4: COMBINE DATASETS
# =============================================================================

# Let's merge both datasets.
merged_datasets <- st_join(standorte_sf_4326, kreisregionen_4326)

# Plot for testing, looks good!
plot(merged_datasets)

# Check the first 5 lines to have a look at the data. Looks good!
head(merged_datasets, 10)

# =============================================================================
# SECTION 4: CHECKING OUR ASSUMPTIONS
# =============================================================================

# To analyze whether more rural Points of Interest (POIs) received more or less 
# funding than their counterparts outside the cities, the following approach can 
# be employed:
#
# 1. Group the data by rurality (Ländlichkeit). The column 'Ländlichkeit' 
#    appears to represent the rurality index.
#
# 2. Calculate the mean funding for each rurality group. The column 
#    'z_auszahlungsbetrag' represents the funding amount.
#
# 3. Visualize the result to understand the funding distribution across 
#    different levels of rurality.

# Load the 'dplyr' library for handling and editing data in R
library(dplyr)

# Group by Ländlichkeit and summarize the mean funding
rural_funding_summary <- merged_datasets %>%
  group_by(Ländlichkeit) %>%
  summarize(mean_funding = mean(z_auszahlungsbetrag, na.rm = TRUE),
            count = n()) %>%
  arrange(Ländlichkeit)

# Print summary
print(rural_funding_summary)

# Visualization
ggplot(rural_funding_summary, aes(x = Ländlichkeit, y = mean_funding)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Mean Funding by Rurality",
       x = "Rurality Index",
       y = "Mean Funding") +
  theme_minimal()


# Group by Typologie and summarize the mean funding
typology_funding_summary <- merged_datasets %>%
  group_by(Typologie) %>%
  summarize(mean_funding = mean(z_auszahlungsbetrag, na.rm = TRUE),
            count = n()) %>%
  arrange(Typologie)
typology_funding_summary

ggplot(merged_datasets, aes(group = as.character(Typologie), x = as.character(Typologie), y = z_auszahlungsbetrag))+
  geom_boxplot()+
  geom_point(alpha = 0.5)
