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
         reconfig_gage_count = ceiling(nrow(gages) * huc4_lengthkm_prop)) %>%
  ungroup()

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
rm(huc4, gage_count, sets_to_bind)

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



# Create other useful summary objects ----

# create dams object for mapping
dams <- flowlines_midpoints %>%
  inner_join(flowlines %>%
               st_drop_geometry() %>%
               filter(nid_dam) %>%
               select(comid, in_gaged_network, in_simple_reconfig_network,
                      in_region_reconfig_network),
             by = "comid")



# Compute summary values - total network ----

# print total stream length in analysis network

print("total network length (km):")
sum(flowlines$lengthkm) %>%
  print()

# print the ACE aquatic biodiversity value associated with the 75th percentile
print("ACE aquatic biodiversity value at the 75th percentile:")

ace_outlet_75pct <- st_drop_geometry(flowlines) %>%
  filter(ace_outlet_biodiv_value > 0) %>%
  filter(ace_outlet_biodiv_value > quantile(ace_outlet_biodiv_value, 0.75)) %>%
  pull(ace_outlet_biodiv_value) %>%
  min()

print(ace_outlet_75pct)

print("count of HUC12s with biodiversity values above the 75th percentile:")

flowlines %>%
  filter(ace_outlet_biodiv_value >= ace_outlet_75pct) %>%
  nrow() %>%
  print()

# print total length of stream segments that are reference quality

print("total length of flowlines that are reference quality (km):")
sum(flowlines$ref_quality_lengthkm) %>%
  print()

# print total length of stream segments that intersect an NCCAG object

print("total length of flowlines that intersect an NCCAG feature (km):")
sum(flowlines$nccag_lengthkm) %>%
  print()

# print total count of dams

print("total count of dams:")

sum(flowlines$nid_dam) %>%
  print()



# Compute summary values - actively gaged network ----

# print a heading

print("CURRENT ACTIVE NETWORK ----------")

# print actively gaged stream length in analysis network

print("actively gaged network length (km):")
flowlines %>%
  filter(in_gaged_network) %>%
  pull(lengthkm) %>%
  sum() %>%
  print()

# print count of HUC12 outlets above the 75th biodiversity percentile that are gaged

print("count of HUC12 outlets above the 75th biodiversity percentile that are actively gaged")
flowlines %>%
  filter(in_gaged_network, ace_outlet_biodiv_value >= ace_outlet_75pct) %>%
  nrow() %>%
  print()

# print the length of NCCAG-intersecting stream that is gaged
print("length of NCCAG-intersecting stream that is actively gaged (km):")
flowlines %>%
  filter(in_gaged_network) %>%
  pull(nccag_lengthkm) %>%
  sum() %>%
  print()

# print the length of reference quality stream that is gaged
print("length of reference quality stream that is actively gaged (km)")
flowlines %>%
  filter(in_gaged_network) %>%
  pull(ref_quality_lengthkm) %>%
  sum() %>%
  print()

# print the count of dams that are gaged
print("count of dams that are actively gaged:")
flowlines %>%
  filter(in_gaged_network, nid_dam) %>%
  nrow() %>%
  print()



# Compute summary values - simple reconfigured gaged network ----

# print a heading

print("SIMPLE RECONFIGURED NETWORK ----------")

# print gaged stream length in simple reconfigured network

print("simple reconfigured gaged network length (km):")
flowlines %>%
  filter(in_simple_reconfig_network) %>%
  pull(lengthkm) %>%
  sum() %>%
  print()

# print count of HUC12 outlets above the 75th biodiversity percentile that are gaged

print("count of HUC12 outlets above the 75th biodiversity percentile that are gaged in a simple reconfiguration")
flowlines %>%
  filter(in_simple_reconfig_network, ace_outlet_biodiv_value >= ace_outlet_75pct) %>%
  nrow() %>%
  print()

# print the length of NCCAG-intersecting stream that is gaged
print("length of NCCAG-intersecting stream that is gaged in a simple reconfiguration (km):")
flowlines %>%
  filter(in_simple_reconfig_network) %>%
  pull(nccag_lengthkm) %>%
  sum() %>%
  print()

# print the length of reference quality stream that is gaged
print("length of reference quality stream that is gaged in a simple reconfiguration (km)")
flowlines %>%
  filter(in_simple_reconfig_network) %>%
  pull(ref_quality_lengthkm) %>%
  sum() %>%
  print()

# print the count of dams that are gaged
print("count of dams that are gaged in a simple reconfiguration:")
flowlines %>%
  filter(in_simple_reconfig_network, nid_dam) %>%
  nrow() %>%
  print()



# Compute summary values - regional reconfigured gaged network ----

# print a heading

print("REGIONAL RECONFIGURED NETWORK ----------")

# print gaged stream length in regional reconfigured network

print("regional reconfigured gaged network length (km):")
flowlines %>%
  filter(in_region_reconfig_network) %>%
  pull(lengthkm) %>%
  sum() %>%
  print()

# print count of HUC12 outlets above the 75th biodiversity percentile that are gaged

print("count of HUC12 outlets above the 75th biodiversity percentile that are gaged in a regional reconfiguration")
flowlines %>%
  filter(in_region_reconfig_network, ace_outlet_biodiv_value >= ace_outlet_75pct) %>%
  nrow() %>%
  print()

# print the length of NCCAG-intersecting stream that is gaged
print("length of NCCAG-intersecting stream that is gaged in a regional reconfiguration (km):")
flowlines %>%
  filter(in_region_reconfig_network) %>%
  pull(nccag_lengthkm) %>%
  sum() %>%
  print()

# print the length of reference quality stream that is gaged
print("length of reference quality stream that is gaged in a regional reconfiguration (km)")
flowlines %>%
  filter(in_region_reconfig_network) %>%
  pull(ref_quality_lengthkm) %>%
  sum() %>%
  print()

# print the count of dams that are gaged
print("count of dams that are gaged in a regional reconfiguration:")
flowlines %>%
  filter(in_region_reconfig_network, nid_dam) %>%
  nrow() %>%
  print()