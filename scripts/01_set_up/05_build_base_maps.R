## BUILD BASE MAPS

# This scripts builds base maps used for visualization.

# Colors and palettes ----

light_grey <- "grey95"
mid_grey <- "grey80"
dark_grey <- "grey65"



# California state base map ----

ca_boundary <- us_states(resolution = "high", states = "California") %>%
  st_transform(crs = global_crs) %>%
  select(state_name)

ca_base_map <- tm_shape(shp = ca_boundary) +
  tm_polygons(col = light_grey) +
  tm_compass(position = c("right", "top"),
             text.size = 0.75,
             text.color = dark_grey,
             color.light = mid_grey,
             color.dark = dark_grey) +
  tm_scale_bar(position = c("left", "bottom"),
               breaks = c(0, 50, 100, 150, 200),
               text.size = 0.75,
               text.color = dark_grey,
               color.light = light_grey,
               color.dark = dark_grey) +
  tm_layout(frame = FALSE)

