## RUN SET-COVER ANALYSIS FOR THE EXPANSION NETWORK

# This script runs a set-cover analysis of currently ungaged stream segments
# to identify the compete set of gages that most efficiently covers the entire
# stream network and then grabs the top-n sets by additional value.

# Run set cover analysis ----

# prep set cover input dataframe
set_cover_input_expansion <- network_analysis_long %>%
  filter(!gage_comid %in% gages$comid,
         !comid %in% gaged_comids) %>%
  select(gage_comid, comid) %>%
  rename(set = gage_comid,
         element = comid)

# run set cover and clean output
set_cover_output_expansion <- greedySetCover(set_cover_input_expansion,
                                             data.table = TRUE) %>%
  rename(gage_comid = set,
         comid = element)

# clean up
rm(set_cover_input_expansion)