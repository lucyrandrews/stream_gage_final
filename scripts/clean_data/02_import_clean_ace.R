## IMPORT AND CLEAN ACE BIODIVERSITY DATA

# This scripts downloads, imports, and cleans the California Department of Fish
# and Wildlife's Areas of Conservations Emphasis (ACE) aquatic biodiversity
# dataset. This dataset identifies high-priority conservation areas based on
# endemic species richness.

# Create directories ----

if(!dir.exists(here("data", "raw_data", "ace"))) {
  dir.create(here("data", "raw_data", "ace"))
}



# Download ACE ----

