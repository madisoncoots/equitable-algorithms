library(readr)
library(sf)
library(ggplot2)
library(ggmap)
library(sp)
library(Cairo)
library(tidycensus)

save_path <- "/Users/madisoncoots/Documents/harvard/research/equitable-algorithms/diabetes/figures/"
data_path <- "/Users/madisoncoots/Documents/harvard/research/equitable-algorithms/data/county_samples.csv"

register_google(key = "AIzaSyCz3G2llgSQlpJfcEkoMYhmXAKUK_N3zgs")

county_samples <- read_csv(data_path)

county_samples_geo <- st_as_sf(county_samples, coords = c("longitude", "latitude"), 
                               crs = 4326, agr = "constant")
# Convert to 3857
county_samples_geo_3857 <- st_transform(county_samples_geo, 3857)

# Shuffle rows to make sure there are no biases in the order in which dots are plotted
set.seed(1)
county_samples_geo_3857 <- county_samples_geo_3857[sample(nrow(county_samples_geo_3857)),]

city_center_geocode <- geocode("Boston")
city_center_geocode$lat <- city_center_geocode$lat - 0.02 # adjustment to center dots more on the map

county_base_map <- get_map(city_center_geocode, maptype = 'terrain', color='bw', zoom = 12)

# Define a function to fix the bbox to be in EPSG:3857
ggmap_bbox <- function(map) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, 
  # and set the names to what sf::st_bbox expects:
  map_bbox <- setNames(unlist(attr(map, "bb")), 
                       c("ymin", "xmin", "ymax", "xmax"))
  
  # Coonvert the bbox to an sf polygon, transform it to 3857, 
  # and convert back to a bbox (convoluted, but it works)
  bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))
  
  # Overwrite the bbox of the ggmap object with the transformed coordinates 
  attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
  attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
  attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
  attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
  return(map)
}

# Use the function:
county_base_map <- ggmap_bbox(county_base_map)


county_courthouse <- st_transform(st_as_sf(data.frame(lon = -71.06043, lat = 42.35869), 
                                           coords = c("lon", "lat"),
                                           crs = 4326, agr = "constant"), 
                                  3857)

county_map <- ggmap(county_base_map, darken = c(0.6, "white")) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = county_samples_geo_3857[1:11000,], inherit.aes = FALSE, 
          aes(color = Race), shape = 16, size=1, alpha = 0.6) +
  ylab("") + xlab("") + 
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = c(0.11, 0.85),
        plot.margin=unit(c(-0.01, -0.06, -0.05, -0.12), "null")) + #top, right, bottom, left
  scale_color_manual(values = c("Asian" = "#00a400", 
                                "Hispanic" = "#ff7700", 
                                "Black" = "#984ea3", 
                                "White" = "#00ABFD")) + 
  guides(color = guide_legend(override.aes = list(shape = 16, alpha = 1, size = 4))) + 
  annotate("text", x=unlist(county_courthouse$geometry)[1], 
           y=unlist(county_courthouse$geometry)[2], label = "★", size = 8, color = "black")

ggsave("boston.pdf", county_map, device=cairo_pdf,
       width = 5, height = 5) # cairo_pdf allows star to print to pdf


