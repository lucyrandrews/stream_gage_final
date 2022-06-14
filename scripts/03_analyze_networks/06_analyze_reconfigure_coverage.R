## ANALYZE CURRENT AND HYPOTHETICAL COVERAGE

# This script reshapes set cover output and other data to prepare for summary
# analysis and visualization

# Produce data products summarizing results ----

# identify gaged network flowlines
gaged_comids <- network_analysis_long_all %>%
  filter(gage_comid %in% gages$comid) %>%
  select(comid) %>%
  unique() %>%
  pull()

flowlines <- flowlines %>%
  mutate(in_gaged_network = comid %in% gaged_comids)

# identify top-n most valuable expansion sets
expansion_sets <- set_costs_expansion %>%
  slice_max(order_by = set_value, n = n_expansion, with_ties = FALSE)

# identify top-n expansion network comids
expansion_comids <- network_analysis_long_expansion %>%
  filter(gage_comid %in% expansion_sets$gage_comid) %>%
  pull(comid) %>%
  unique()

# identify expansion network flowlines
flowlines <- flowlines %>%
  mutate(in_expansion_network = comid %in% expansion_comids,
         has_expansion_gage = comid %in% expansion_sets$gage_comid)

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
simple_reconfig_sets <- set_costs_all %>%
  filter(gage_comid %in% set_cover_output_all$gage_comid) %>%
  slice_max(order_by = set_value, n = nrow(gages), with_ties = FALSE)

simple_reconfig_comids <- network_analysis_long_all %>%
  filter(gage_comid %in% simple_reconfig_sets$gage_comid) %>%
  pull(comid) %>%
  unique()

flowlines <- flowlines %>% 
  mutate(in_simple_reconfig_network = comid %in% simple_reconfig_comids,
         has_simple_reconfig_network_gage = comid %in% simple_reconfig_sets$gage_comid)

# now for a network with gages distributed across HUC4 regions, proportional to
# the stream length in each region; ceiling round to start, since regular
# rounding doesn't do the trick in selecting enough sets in following lines
huc4_lengths <- flowlines %>%
  st_drop_geometry() %>%
  group_by(huc4_name, huc4_group) %>%
  summarize(huc4_lengthkm = sum(lengthkm)) %>%
  mutate(huc4_lengthkm_prop = huc4_lengthkm / sum(flowlines$lengthkm),
         reconfig_gage_count = ceiling(nrow(gages) * huc4_lengthkm_prop)) %>%
  ungroup()

# create an object to hold results
region_reconfig_sets <- simple_reconfig_sets[0, ]

# loop through regions to grab the right count of sets for each
for(huc4 in huc4_lengths$huc4_group) {
  
  gage_count <- huc4_lengths %>%
    filter(huc4_group == huc4) %>%
    pull(reconfig_gage_count)
    
  sets_to_bind <- set_costs_all %>%
    filter(huc4_group == huc4,
           gage_comid %in% set_cover_output_all$gage_comid) %>%
    slice_max(order_by = set_value, n = gage_count, with_ties = FALSE)
  
  region_reconfig_sets <- rbind(region_reconfig_sets, sets_to_bind)
  
}

# clean up
rm(huc4, gage_count, sets_to_bind)

# trim the final set to match the original desired count of gages
region_reconfig_sets <- region_reconfig_sets %>%
  arrange(desc(set_value)) %>%
  head(nrow(simple_reconfig_sets))

# grab reconfigured gaged comids
region_reconfig_comids <- network_analysis_long_all %>%
  filter(gage_comid %in% region_reconfig_sets$gage_comid) %>%
  pull(comid) %>%
  unique()

# add region reconfiguration to flowlines
flowlines <- flowlines %>% 
  mutate(in_region_reconfig_network = comid %in% region_reconfig_comids,
         has_region_reconfig_gage = comid %in% region_reconfig_sets$gage_comid)



# Create other useful summary objects ----

# create dams object for mapping
dams <- flowlines_midpoints %>%
  inner_join(flowlines %>%
               st_drop_geometry() %>%
               filter(nid_dam) %>%
               select(comid, in_gaged_network, in_simple_reconfig_network,
                      in_region_reconfig_network),
             by = "comid")