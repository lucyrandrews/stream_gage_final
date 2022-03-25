## ANALYZE UPSTREAM AND DOWNSTREAM CONNECTIVITY FOR EVERY FLOWLINE

# This script analyze the full stream network to identify the stream segements
# upstream and downstream of each segment that fall within the specified
# drainage area threshold. This script also reshapes data output to make it
# easy to analyze and visualize.

# Create directories ----

# create directory for all analysis set-up objects
if(!dir.exists(here("data", "processed_data", "flowlines_for_analysis"))) {
  dir.create(here("data", "processed_data", "flowlines_for_analysis"))
}

# create directory for flowlines as shapefile
if(!dir.exists(here("data", "processed_data", "flowlines_for_analysis",
                    "flowlines_shp"))) {
  dir.create(here("data", "processed_data", "flowlines_for_analysis",
                  "flowlines_shp"))
}

# create directory for flowline splits for parallel processing
if(!dir.exists(here("data", "processed_data", "flowlines_for_analysis",
                    "flowlines_splits_for_parallel"))) {
  dir.create(here("data", "processed_data", "flowlines_for_analysis",
                  "flowlines_splits_for_parallel"))
}



# Format objects for analysis ----

# save a flowlines shapefile
flowlines %>%
  select(comid, gnis_name, ftype, lengthkm, totdasqkm) %>%
  mutate(comid = as.character(comid)) %>%
  rename(COMID = comid,
         GNIS_Name = gnis_name,
         FTYPE = ftype,
         LengthKM = lengthkm,
         TotDASqKM = totdasqkm) %>%
  st_write(dsn = here("data", "processed_data", "flowlines_for_analysis",
                      "flowlines_shp", "flowlines_full_network.shp"),
           driver = "ESRI Shapefile",
           delete_dsn = TRUE)

# split comids into separate files and save for parallel processing
comids_split <- flowlines %>%
  st_drop_geometry() %>%
  select(comid) %>%
  rename(COMID = comid) %>%
  mutate(split_num = rep_len(1:n_cores, length.out = nrow(.))) %>%
  split(.$split_num) %>%
  set_names(paste0("split_", as.list(1:n_cores)))


lapply(names(comids_split),
       function(x) {
         write_csv(comids_split[[x]],
                   file = paste0(here("data", "processed_data",
                                      "flowlines_for_analysis",
                                      "flowlines_splits_for_parallel"), "/", x, ".csv"))
         })

# save file of to-from comid relationships
write.dbf(dataframe = comids_to_from,
          file = here("data", "processed_data", "flowlines_for_analysis",
                      "comids_to_from.dbf"))

# clean up
rm(comids_split)



# Analyze full network ----

# create list of comid split filenames
comids_split_filenames <- list.files(here("data", "processed_data",
                                          "flowlines_for_analysis",
                                          "flowlines_splits_for_parallel"),
                                     full.names = TRUE)

network_analysis <- foreach(i = 1:n_cores, .combine = rbind) %dopar% {
  analyzeStreams(pct_threshold_down = threshold_downstream,
                 pct_threshold_up = threshold_upstream,
                 NHDFlowline_folder = here("data", "processed_data",
                                           "flowlines_for_analysis",
                                           "flowlines_shp"),
                 gages_filename = comids_split_filenames[i],
                 to_from_COMIDs_filename = here("data", "processed_data",
                                                "flowlines_for_analysis",
                                                "comids_to_from.dbf"))
  }

# drop unnecessary column
network_analysis <- network_analysis %>%
  select(-has_gage)

# clean up
rm(comids_split_filenames)