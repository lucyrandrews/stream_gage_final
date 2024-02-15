## BUILD BASE MAPS

# This scripts builds base maps used for visualization.

# Colors and palettes ----
white <- "white"
black <- "black"

grey_1 <- "#f7f7f7"
grey_2 <- "#cccccc"
grey_3 <- "#969696"
grey_4 <- "#636363"
grey_5 <- "#252525"

light_green <- "#edf8e9"
mid_green <- "#41ab5d"
dark_green <- "#006d2c"

light_yellow <- "#f7fcb9"
mid_yellow <- "#fce436"
dark_yellow <- "#dfbc2d"

light_blue <- "#afdcf7"
mid_blue <- "#6baed6"
mid_dark_blue <- "#3c78c6"
dark_blue <- "#084594"

mid_purple <- "#756bb1"

mid_orange <- "#fe9929"
dark_orange <- "#d57000"



# California state base map ----

ca_boundary <- us_states(resolution = "high", states = "California") %>%
  st_transform(crs = global_crs) %>%
  select(state_name)

ca_base_map <- tm_shape(shp = ca_boundary) +
  tm_polygons(col = white) +
  tm_compass(position = c("left", "bottom"),
             text.size = 0.75,
             text.color = grey_4,
             color.light = grey_1,
             color.dark = grey_4) +
  tm_scale_bar(position = c("left", "bottom"),
               breaks = c(0, 50, 100, 150, 200),
               text.size = 0.75,
               text.color = grey_4,
               color.light = grey_1,
               color.dark = grey_4) +
  tm_layout(frame = FALSE)

