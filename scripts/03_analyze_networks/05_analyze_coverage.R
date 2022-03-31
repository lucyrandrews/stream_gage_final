## ANALYZE CURRENT AND HYPOTHETICAL COVERAGE

# This script runs a set-cover analysis to identify the compete set of gages
# that most efficiently covers the entires stream network.

# Produce data products summarizing results ----

# identify gaged network flowlines
gaged_comids <- network_analysis_long %>%
  filter(gage_comid %in% gages$comid) %>%
  select(comid) %>%
  unique() %>%
  pull()

flowlines <- flowlines %>%
  mutate(in_gaged_network = comid %in% gaged_comids)

# update HUC12 polygons to list gaged coverage status and ACE value
huc12s <- huc12s %>%
  left_join(flowlines %>%
              st_drop_geometry() %>%
              filter(ace_outlet_biodiv_value > 0) %>%
              select(huc12_id, ace_outlet_biodiv_value, in_gaged_network),
            by = "huc12_id")



# Evaluate management objectives coverage for current network ----





