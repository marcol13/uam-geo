# Package installation
if (!requireNamespace(c("sf", "ggplot2", "rnaturalearth", "rnaturalearthhires", "dplyr"), quietly = TRUE)) install.packages(c("sf", "ggplot2", "rnaturalearth", "rnaturalearthhires", "dplyr"))

# Loading the libraries
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthhires)
library(dplyr)

setwd("/Users/marcin/Documents/Projects/uam-geo/geography-of-settlement/lab1")


# Hardcoded coordinates for South West England
min_lat <- 49.5
max_lat <- 52.5
min_lon <- -6.0
max_lon <- -1.0

# Load the regions of the United Kingdom
regions <- ne_states(country = "United Kingdom", returnclass = "sf")

# Filter South West England
south_west <- regions %>%
    filter(region %in% c("South West"))

# Merge outdated counties of Bournemouth and Poole into one "Bournemouth, Christchurch and Poole"
updated_counties <- south_west %>%
    filter(name %in% c("Bournemouth", "Poole"))
other_counties <- south_west %>%
    filter(!name %in% c("Bournemouth", "Poole"))
updated_counties <- updated_counties %>%
    summarise(name = "Bournemouth, Christchurch and Poole", geometry = st_union(geometry))

south_west <- bind_rows(other_counties, updated_counties)

# Read the cities data from file
counties_df <- read.csv(file = file.path("data", "en_swe_counties.csv"), header = T)
cities_sf <- st_as_sf(counties_df, coords = c("lon", "lat"), crs = 4326)

# Crop the map to the specified area
bbox <- st_bbox(c(xmin = min_lon, xmax = max_lon, ymin = min_lat, ymax = max_lat), crs = st_crs(regions))
regions_of_interest <- st_crop(south_west, bbox)

# Plot map of South West England
ggplot(data = regions_of_interest) +
    geom_sf(aes(fill = name), color = "black", lwd = 0.3) +
    geom_sf(data = cities_sf, color = "red", size = 3) +
    geom_text(data = counties_df, aes(x = lon, y = lat, label = city, vjust = vjust, hjust = hjust), size = 3, fontface = "bold") +
    coord_sf(xlim = c(min_lon, max_lon), ylim = c(min_lat, max_lat)) +
    theme_minimal() +
    guides(fill = guide_legend(title = "Jednostki administracyjne")) +
    labs(
        title = "",
        x = "",
        y = ""
    )
