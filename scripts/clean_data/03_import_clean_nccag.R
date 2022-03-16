## IMPORT AND CLEAN NCCAG DATA

# This scripts downloads, imports, and cleans the California Natural Resources
# Agency's Natural Communities Commonly Associated with Groundwater (NCCAG)
# dataset, which estimates the spatial locations of vegetation and wetlands that
# signal locations of ecologically significant surface water-groundwater
# interaction.

# Create directories ----

# create NCCAG directory
if(!dir.exists(here("data", "raw_data", "nccag"))) {
  dir.create(here("data", "raw_data", "nccag"))
}



# Download NCCAG ----

