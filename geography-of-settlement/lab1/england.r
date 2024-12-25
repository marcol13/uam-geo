# Package installation
if (!requireNamespace(c("sf", "ggplot2", "rnaturalearth", "rnaturalearthhires", "dplyr"), quietly = TRUE)) install.packages(c("sf", "ggplot2", "rnaturalearth", "rnaturalearthhires", "dplyr"))

# Loading the libraries
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthhires)
library(dplyr)


# Hardcoded coordinates for South West England
min_lat <- 49.5  
max_lat <- 52.5  
min_lon <- -6.0  
max_lon <- -1.0  

# Load the regions of the United Kingdom
regions <- ne_states(country = "United Kingdom", returnclass = "sf") # Regiony UK

# Filter South West England
south_west <- regions %>% 
  filter(region %in% c("South West"))

# Merge outdated counties of Bournemouth and Poole into one "Bournemouth, Christchurch and Poole"
updated_counties <- england_only %>%
    filter(name %in% c("Bournemouth", "Poole"))
other_counties <- england_only %>%
    filter(!name %in% c("Bournemouth", "Poole"))
updated_counties <- updated_counties %>% 
  summarise(name = "Bournemouth, Christchurch and Poole", geometry = st_union(geometry))

south_west <- bind_rows(other_counties, updated_counties)

counties_df <- data.frame(
    name = c("Gloucestershire",
"Dorset",
"Bournemouth, Christchurch and Poole",
"Devon",
"Torbay",
"Plymouth",
"Cornwall",
"Somerset",
"North Somerset",
"Bristol",
"South Gloucestershire",
"Swindon",
"Bath and North East Somerset",
"Wiltshire"),
city=c(
"Gloucester",
"Dorchester",
"Bournemouth",
"Exeter",
"Torquay",
"Plymouth",
"Truro",
"Taunton",
"Weston-super-Mare",
"Bristol",
"Yate",
"Swindon",
"Bath",
"Trowbridge"
),
lat=c(
    51.864445,
50.711163,
50.72048,
50.716667,
50.46384,
50.376289,
50.259998,
51.01494,
51.34603,
51.454514,
51.54074,
51.568535,
51.380001,
51.319195
),
lon=c(
    -2.244444,
-2.441181,
-1.8795,
-3.533333,
-3.51434,
-4.143841,
-5.051000,
-3.10293,
-2.97665,
-2.587910,
-2.41839,
-1.772232,
-2.360000,
-2.204079
),
population=c(
    318898,
383274,
401898,
1232660,
139479,
266862,
575413,
576852,
219145,
479024,
294765,
235657,
195618,
515885
),
vjust = c(1.7, 1.7, 1.7, 1.7, 1.7, 1.7, 
                 1.7, 1.7, 1.7, 1.7, 1.7, 1.7, 
                 1.7, 1.7),
hjust = c(0.5, 0.5, 0.5, 0.4, 0.5, 0.5, 
                 0.45, 0.5, 0.6, 0.5, 0.5, 0.5, 
                 0.5, 0.5)
)

cities_sf <- st_as_sf(counties_df, coords = c("lon", "lat"), crs = 4326)  # Współrzędne geograficzne

# Crop map to the specified area
bbox <- st_bbox(c(xmin = min_lon, xmax = max_lon, ymin = min_lat, ymax = max_lat), crs = st_crs(regions))
regions_of_interest <- st_crop(south_west, bbox)

# Plot map of South West England
ggplot(data = regions_of_interest) +
  geom_sf(aes(fill = name), color = "black", lwd = 0.3) +
  geom_sf(data = cities_sf, color = "red", size = 3) + 
  geom_text(data = counties_df, aes(x = lon, y = lat, label = city, vjust = vjust, hjust = hjust), size=3, fontface='bold') +
  coord_sf(xlim = c(min_lon, max_lon), ylim = c(min_lat, max_lat)) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    axis.line = element_line(color = "black")
  ) +
  labs(title = "Podział administracyjny Wielkiej Brytanii",
       subtitle = paste("Obszar: ", min_lat, "-", max_lat, "N, ", min_lon, "-", max_lon, "E"),
       x = "Długość geograficzna",
       y = "Szerokość geograficzna")


