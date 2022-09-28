## PRODUCE TEXT OUTPUT

# This script writes out statistics used in the manuscript.

# Compute summary values - total network ----

# direct R to write printed outputs to .txt file
sink(file = here("output", "text", "output_values.txt"),
     append = FALSE,
     type = "output")

# print a heading
writeLines("\n\n\nTOTAL STREAM NETWORK AND MANAGEMENT-RELEVANT ATTRIBUTES ----------")

# print total count of stream segments in the full network
writeLines("\ntotal count of network segments:")

nrow(flowlines) %>%
  print()

# print total stream length in analysis network
writeLines("\ntotal network length (km):")

sum(flowlines$lengthkm) %>%
  print()

# print flowline length statistics:
writeLines("\n flowline length statistics (km):")

summary(flowlines$lengthkm) %>%
  print()

writeLines("\nstandard deviation of flowline length (km):")

sd(flowlines$lengthkm) %>%
  print()

# print count of active gages
writeLines("\ncount of active gages:")

nrow(gages) %>%
  print()

# print count of co-located active gages
writeLines("\ncount of active gages co-located on a stream segment:")

dup_gage_comids <- gages %>%
  group_by(comid) %>%
  tally() %>%
  filter(n >= 2)

gages %>%
  filter(comid %in% dup_gage_comids$comid) %>%
  nrow() %>%
  print()

# print ACE aquatic biodiversity statistics
writeLines("\nACE aquatic biodiversity statistics:")

flowlines %>%
  filter(ace_outlet_biodiv_value > 0) %>%
  pull(ace_outlet_biodiv_value) %>%
  summary(ace_outlet_biodiv_value)

# print the count of HUC12s with upper-quartile biodiversity values
writeLines("\ncount of HUC12s with biodiversity values above the 75th percentile:")

ace_outlet_75pct <- st_drop_geometry(flowlines) %>%
  filter(ace_outlet_biodiv_value > 0) %>%
  filter(ace_outlet_biodiv_value > quantile(ace_outlet_biodiv_value, 0.75)) %>%
  pull(ace_outlet_biodiv_value) %>%
  min()

flowlines %>%
  filter(ace_outlet_biodiv_value >= ace_outlet_75pct) %>%
  nrow() %>%
  print()

# print total length of stream segments that are reference quality
writeLines("\ntotal length of flowlines that are reference quality (km):")

sum(flowlines$ref_quality_lengthkm) %>%
  print()

# print total length of stream segments that intersect an NCCAG object
writeLines("\ntotal length of flowlines that intersect an NCCAG feature (km):")

sum(flowlines$nccag_lengthkm) %>%
  print()

# print total count of dams
writeLines("\ntotal count of dams:")

sum(flowlines$nid_dam) %>%
  print()



# Compute summary values - actively gaged network ----

# print a heading
writeLines("\n\n\nCURRENT ACTIVE NETWORK ----------")

# print actively gaged stream length in analysis network
writeLines("\nactively gaged network length (km):")

flowlines %>%
  filter(in_gaged_network) %>%
  pull(lengthkm) %>%
  sum() %>%
  print()

# print average length that a given active gage covers
writeLines("\naverage length covered by an active gage (km):")

set_costs_all %>%
  filter(gage_comid %in% gages$comid) %>%
  pull(length_gaged) %>%
  mean() %>%
  print()

# print standard deviation of the length that a given active gage covers
writeLines("\nstandard deviation of average length covered by an active gage (km):")

set_costs_all %>%
  filter(gage_comid %in% gages$comid) %>%
  pull(length_gaged) %>%
  sd() %>%
  print()

# print length of flowlines that are redundantly and uniquely gaged
writeLines("\nsummary statistics of flowlines that are redundantly gaged:")

network_analysis_long_all %>%
  filter(gage_comid %in% gages$comid) %>%
  group_by(comid) %>%
  tally() %>%
  filter(n >= 2) %>%
  left_join(st_drop_geometry(flowlines) %>% select(comid, lengthkm),
            by = "comid") %>%
  summarize(count_of_redundantly_gaged_comids = n_distinct(comid),
            max_redundant_gages_per_segment = max(n),
            total_length_redundantly_gaged = sum(lengthkm)) %>%
  print()

writeLines("\nsummary statistics of flowlines that are uniquely gaged")

network_analysis_long_all %>%
  filter(gage_comid %in% gages$comid) %>%
  group_by(comid) %>%
  tally() %>%
  filter(n == 1) %>%
  left_join(st_drop_geometry(flowlines) %>% select(comid, lengthkm),
            by = "comid") %>%
  summarize(count_of_uniquely_gaged_comids = n_distinct(comid),
            total_length_uniquely_gaged = sum(lengthkm)) %>%
  print()

# print proportion of gaged length per HUC4
writeLines("\nproportion of each HUC4 that is actively gaged")

flowlines %>%
  st_drop_geometry() %>%
  group_by(huc4_name, in_gaged_network) %>%
  summarize(lengthkm = sum(lengthkm)) %>%
  pivot_wider(names_from = huc4_name, values_from = lengthkm)


# print count of HUC12 outlets above the 75th biodiversity percentile that are gaged
writeLines("\ncount of HUC12 outlets above the 75th biodiversity percentile that are actively gaged")

flowlines %>%
  filter(in_gaged_network, ace_outlet_biodiv_value >= ace_outlet_75pct) %>%
  nrow() %>%
  print()

# print the length of NCCAG-intersecting stream that is gaged
writeLines("\nlength of NCCAG-intersecting stream that is actively gaged (km):")

flowlines %>%
  filter(in_gaged_network) %>%
  pull(nccag_lengthkm) %>%
  sum() %>%
  print()

# print the length of reference quality stream that is gaged
writeLines("\nlength of reference quality stream that is actively gaged (km)")

flowlines %>%
  filter(in_gaged_network) %>%
  pull(ref_quality_lengthkm) %>%
  sum() %>%
  print()

# print the count of dams that are gaged
writeLines("\ncount of dams that are actively gaged:")

flowlines %>%
  filter(in_gaged_network, nid_dam) %>%
  nrow() %>%
  print()



# Compute summary values - expansion network -----

# print a heading
writeLines("\n\n\nEXPANSION NETWORK ----------")

# print expansion network gaged stream length in analysis network
writeLines("\nexpansion gaged network length (km):")

flowlines %>%
  st_drop_geometry() %>%
  filter(in_expansion_network) %>%
  group_by(expansion_set) %>%
  summarize(lengthkm = sum(lengthkm)) %>%
  print()

# print count of HUC12 outlets above the 75th biodiversity percentile that are expansion gaged
writeLines("\ncount of HUC12 outlets above the 75th biodiversity percentile that are expansion gaged")

flowlines %>%
  st_drop_geometry() %>%
  filter(in_expansion_network, ace_outlet_biodiv_value >= ace_outlet_75pct) %>%
  group_by(expansion_set) %>%
  summarize(ace_outlet_75pct_count = n()) %>%
  print()

# print the length of NCCAG-intersecting stream that is expansion gaged
writeLines("\nlength of NCCAG-intersecting stream that is expansion gaged (km):")

flowlines %>%
  st_drop_geometry() %>%
  filter(in_expansion_network) %>%
  group_by(expansion_set) %>%
  summarize(nccag_lengthkm = sum(nccag_lengthkm)) %>%
  print()

# print the length of reference quality stream that is expansion gaged
writeLines("\nlength of reference quality stream that is expansion gaged (km)")

flowlines %>%
  st_drop_geometry() %>%
  filter(in_expansion_network) %>%
  group_by(expansion_set) %>%
  summarize(ref_quality_lengthkm = sum(ref_quality_lengthkm)) %>%
  print()

# print the count of dams that are expansion gaged
writeLines("\ncount of dams that are expansion gaged:")

flowlines %>%
  st_drop_geometry() %>%
  filter(in_expansion_network, nid_dam) %>%
  group_by(expansion_set) %>%
  summarize(nid_dam_count = n()) %>%
  print()

# Compute summary values - simple reconfigured gaged network ----

# print a heading
writeLines("\n\n\nSIMPLE RECONFIGURED NETWORK ----------")

# print gaged stream length in simple reconfigured network
writeLines("\nsimple reconfigured gaged network length (km):")

flowlines %>%
  filter(in_simple_reconfig_network) %>%
  pull(lengthkm) %>%
  sum() %>%
  print()

# print count of HUC12 outlets above the 75th biodiversity percentile that are gaged
writeLines("\ncount of HUC12 outlets above the 75th biodiversity percentile that are gaged in a simple reconfiguration")

flowlines %>%
  filter(in_simple_reconfig_network, ace_outlet_biodiv_value >= ace_outlet_75pct) %>%
  nrow() %>%
  print()

# print the length of NCCAG-intersecting stream that is gaged
writeLines("\nlength of NCCAG-intersecting stream that is gaged in a simple reconfiguration (km):")

flowlines %>%
  filter(in_simple_reconfig_network) %>%
  pull(nccag_lengthkm) %>%
  sum() %>%
  print()

# print the length of reference quality stream that is gaged
writeLines("\nlength of reference quality stream that is gaged in a simple reconfiguration (km)")

flowlines %>%
  filter(in_simple_reconfig_network) %>%
  pull(ref_quality_lengthkm) %>%
  sum() %>%
  print()

# print the count of dams that are gaged
writeLines("\ncount of dams that are gaged in a simple reconfiguration:")

flowlines %>%
  filter(in_simple_reconfig_network, nid_dam) %>%
  nrow() %>%
  print()



# Compute summary values - regional reconfigured gaged network ----

# print a heading
writeLines("\n\n\nREGIONAL RECONFIGURED NETWORK ----------")

# print gaged stream length in regional reconfigured network
writeLines("\nregional reconfigured gaged network length (km):")

flowlines %>%
  filter(in_region_reconfig_network) %>%
  pull(lengthkm) %>%
  sum() %>%
  print()

# print count of HUC12 outlets above the 75th biodiversity percentile that are gaged
writeLines("\ncount of HUC12 outlets above the 75th biodiversity percentile that are gaged in a regional reconfiguration")

flowlines %>%
  filter(in_region_reconfig_network, ace_outlet_biodiv_value >= ace_outlet_75pct) %>%
  nrow() %>%
  print()

# print the length of NCCAG-intersecting stream that is gaged
writeLines("\nlength of NCCAG-intersecting stream that is gaged in a regional reconfiguration (km):")

flowlines %>%
  filter(in_region_reconfig_network) %>%
  pull(nccag_lengthkm) %>%
  sum() %>%
  print()

# print the length of reference quality stream that is gaged
writeLines("\nlength of reference quality stream that is gaged in a regional reconfiguration (km)")

flowlines %>%
  filter(in_region_reconfig_network) %>%
  pull(ref_quality_lengthkm) %>%
  sum() %>%
  print()

# print the count of dams that are gaged
writeLines("\ncount of dams that are gaged in a regional reconfiguration:")

flowlines %>%
  filter(in_region_reconfig_network, nid_dam) %>%
  nrow() %>%
  print()

# stop writing to .txt file
sink(file = NULL)



# Create and save a figure of the HUC2 watershed boundary ----

m <- ca_base_map +
  tm_shape(huc2) +
  tm_fill(col = grey_2) +
  tm_add_legend(type = "fill",
                labels = "California HUC2 watershed",
                col = grey_2,
                border.col = NULL)

tmap_save(tm = m,
          filename = here("output", "figures", "ca_huc2.png"))