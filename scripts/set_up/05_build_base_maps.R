## BUILD BASE MAPS

# This scripts builds base maps used for visualization.

# Color ramps ----


# California state base map ----

ca_boundary <- states() %>%
  rename(name = NAME) %>%
  filter(name == "California") %>%
  select(name) %>%
  st_transform(global_crs)