## RUN SET-COVER ANALYSIS

# This script runs a set-cover analysis to identify the compete set of gages
# that most efficiently covers the entire stream network.

# Run set cover analysis ----

# prep set cover input dataframe
set_cover_input_all <- network_analysis_long %>%
  select(gage_comid, comid) %>%
  rename(set = gage_comid,
         element = comid)

# run set cover and clean output
set_cover_output_all <- greedySetCover(set_cover_input_all, data.table = TRUE) %>%
  rename(gage_comid = set,
         comid = element)

# clean up
rm(set_cover_input_all)