## PRODUCE TEXT OUTPUT

# This script writes out statistics used in the manuscript.

# Compute summary values - total network ----

# direct R to write printed outputs to .txt file
sink(file = here("output", "text", "output_values.txt"),
     append = FALSE,
     type = "output")

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