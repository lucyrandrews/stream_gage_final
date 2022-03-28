## RUN SET-COVER ANALYSIS

# This script runs a set-cover analysis to identify the compete set of gages
# that most efficiently covers the entires stream network.

# Run set cover analysis ----

if(unweighted_set_cover) {
  
  # prep set cover input dataframe
  set_cover_input <- network_analysis_long %>%
    select(gage_comid, comid) %>%
    rename(set = gage_comid,
           element = comid)
  
  # run set cover and clean output
  set_cover_output <- greedySetCover(set_cover_input, data.table = TRUE) %>%
    rename(gage_comid = set,
           comid = element)
  
  # clean up
  rm(set_cover_input)
  
} else {
  
  # create list of ids contained in each set
  sets_w_elements <- sapply(set_costs$comids_str_concat, strsplit, split = " ")
  
  # name each set with the gaged comids
  names(sets_w_elements) <- set_costs$gage_comid
  
  # create costs vector
  cost_weights <- as.vector(set_costs$set_cost_rescaled)
  
  # name the costs
  names(cost_weights) <- set_costs$gage_comid
  
  # Run the greedy set cover analysis with weights (aka costs - inverted values)
  # to preferentially select "high value" gage sites
  # See draft functions for what is in progress here
  set_cover_output <- NULL
  
  # clean up
  rm(sets_w_elements, cost_weights)
  
}