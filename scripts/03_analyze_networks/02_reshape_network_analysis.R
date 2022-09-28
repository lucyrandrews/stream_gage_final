## RESHAPE NETWORK ANALYSIS DATA FOR FURTHER ANALYSIS AND VISUALIZATION

# This script analyze the full stream network to identify the stream segements
# upstream and downstream of each segment that fall within the specified
# drainage area threshold. This script also reshapes data output to make it
# easy to analyze and visualize.

# Pivot network analysis downstream results longer ----

# identify longest segment count gaged in the downstream direction
max_n_down <- max(network_analysis$step_n_down, na.rm = TRUE)

# prepare dataframe names
down_names <- paste0("down_comid_", 1:max_n_down)

# reshape comid downstream relationships to long format
network_analysis_long_down <- network_analysis %>%
  select(COMID, down_COMIDs) %>%
  separate(col = down_COMIDs, into = down_names, sep = " ", remove = TRUE) %>%
  pivot_longer(cols = !COMID, names_to = "location", values_to = "associated_comid") %>%
  filter(!is.na(associated_comid)) %>%
  rename(gage_comid = COMID,
         comid = associated_comid) %>%
  mutate(gage_comid = as.character(gage_comid),
         gage_location = "upstream") %>%
  select(gage_comid, comid, gage_location)

# clean up
rm(max_n_down, down_names)



# Pivot network analysis upstream results longer ----

# identify longest segment count gaged in the upstream direction
max_n_up <- max(network_analysis$step_n_up, na.rm = TRUE)

# prepare dataframe names
up_names <- paste0("up_comid_", 1:max_n_up)

# reshape comid downstream relationships to long format
network_analysis_long_up <- network_analysis %>%
  select(COMID, up_COMIDs) %>%
  separate(col = up_COMIDs, into = up_names, sep = " ", remove = TRUE) %>%
  pivot_longer(cols = !COMID, names_to = "location", values_to = "associated_comid") %>%
  filter(!is.na(associated_comid)) %>%
  rename(gage_comid = COMID,
         comid = associated_comid) %>%
  mutate(gage_location = "downstream") %>%
  select(gage_comid, comid, gage_location)

# clean up
rm(max_n_up, up_names)



# Create rows for gages on comids (instead of upstream or downstream) ----

# create vectors to compose dataframe
comid <- unique(network_analysis$COMID)
gage_comid <- unique(network_analysis$COMID)
gage_location <- rep("on comid", length(unique(network_analysis$COMID)))

# compose dataframe
network_analysis_long_on <- tibble(gage_comid, comid, gage_location)



# Create single long format dataframe ----

# bind all rows together and add HUC4 ID
network_analysis_long <- rbind(network_analysis_long_up,
                               network_analysis_long_down,
                               network_analysis_long_on) %>%
  left_join(st_drop_geometry(select(flowlines, comid, huc4_group)),
            by = c("gage_comid" = "comid"))

# identify gaged network flowlines
gaged_comids <- network_analysis_long %>%
  filter(gage_comid %in% gages$comid) %>%
  select(comid) %>%
  unique() %>%
  pull()

flowlines <- flowlines %>%
  mutate(in_gaged_network = comid %in% gaged_comids)

# clean up
rm(network_analysis_long_down, network_analysis_long_up,
   network_analysis_long_on, comid, gage_comid, gage_location)