## IMPORT AND CLEAN REFERENCE-QUALITY STREAM SEGMENTS

# This scripts downloads, imports, and cleans NHD stream segments (indexed by
# comid) that have been identified as reference quality for hydrologic
# management.

# Create directories ----

# create reference-quality streams data directory
if(!dir.exists(here("data", "raw_data", "reference_streams"))) {
  dir.create(here("data", "raw_data", "reference_streams"))
}



# Download reference-quality stream segments ----

# download from file stored on Google Drive with open view permissions via
# its Google Drive ID
drive_download(file = as_id("135y7bL7e3RzMDLBO6o5PcL4ye020WyC0"),
               path = here("data", "raw_data", "reference_streams", "ref_comids.csv"),
               overwrite = TRUE)



# Import and clean reference-quality stream segments ----

ref_comids <- read_csv(here("data", "raw_data", "reference_streams", "ref_comids.csv")) %>%
  rename(comid = COMID) %>%
  mutate(comid = as.character(comid),
         ref_quality = TRUE)

