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

# update HUC12 polygons to list gaged coverage status, outlet comid, and ACE
# value
huc12s <- huc12s %>%
  left_join(flowlines %>%
              st_drop_geometry() %>%
              filter(ace_outlet_biodiv_value > 0) %>%
              select(huc12_id, comid, ace_outlet_biodiv_value, in_gaged_network),
            by = "huc12_id")



# Create reconfigured networks ----

# start with a simple network - top-value gages independent of region
simple_reconfig_sets <- set_costs %>%
  filter(gage_comid %in% set_cover_output$gage_comid) %>%
  slice_max(order_by = set_value, n = nrow(gages))

simple_reconfig_comids <- network_analysis_long %>%
  filter(gage_comid %in% simple_reconfig_sets$gage_comid) %>%
  pull(comid) %>%
  unique()

flowlines <- flowlines %>% 
  mutate(in_simple_reconfig_network = comid %in% simple_reconfig_comids)

# now for a network with gages distributed across HUC4 regions, proportional to
# the stream length in each region; ceiling round to start, since regular
# rounding doesn't do the trick in selecting enough sets in following lines
huc4_lengths <- flowlines %>%
  st_drop_geometry() %>%
  group_by(huc4_name, huc4_group) %>%
  summarize(huc4_lengthkm = sum(lengthkm)) %>%
  mutate(huc4_lengthkm_prop = huc4_lengthkm / sum(flowlines$lengthkm),
         reconfig_gage_count = ceiling(nrow(gages) * huc4_lengthkm_prop))

# create an object to hold results
region_reconfig_sets <- simple_reconfig_sets[0, ]

# loop through regions to grab the right count of sets for each
for(huc4 in huc4_lengths$huc4_group) {
  
  gage_count <- huc4_lengths %>%
    filter(huc4_group == huc4) %>%
    pull(reconfig_gage_count)
    
  sets_to_bind <- set_costs %>%
    filter(huc4_group == huc4,
           gage_comid %in% set_cover_output$gage_comid) %>%
    slice_max(order_by = set_value, n = gage_count, with_ties = FALSE)
  
  region_reconfig_sets <- rbind(region_reconfig_sets, sets_to_bind)
  
}

# clean up
rm(huc4, gage_count)

# trim the final set to match the original desired count of gages
region_reconfig_sets <- region_reconfig_sets %>%
  arrange(desc(set_value)) %>%
  head(nrow(simple_reconfig_sets))

# grab reconfigured gaged comids
region_reconfig_comids <- network_analysis_long %>%
  filter(gage_comid %in% region_reconfig_sets$gage_comid) %>%
  pull(comid) %>%
  unique()

# add region reconfiguration to flowlines
flowlines <- flowlines %>% 
  mutate(in_region_reconfig_network = comid %in% region_reconfig_comids)
