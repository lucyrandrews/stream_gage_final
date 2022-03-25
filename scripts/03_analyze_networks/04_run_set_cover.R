## RUN SET-COVER ANALYSIS

# This script runs a set-cover analysis to identify the compete set of gages
# that most efficiently covers the entires stream network.

# Structure data to use in set cover analysis ----

# prep data structure for weighted set cover with function `weightedSetCover`
# from `WebGestaltR` package

# create list of ids contained in each set
ids_in_set <- sapply(set_costs$comids_str_concat, strsplit, split = " ")

# name each set with the gaged comids
names(ids_in_set) <- set_costs$gage_comid



# Run the greedy set cover analysis with weights (aka costs - inverted values)
# to preferentially select "high value" gage sites
set_cover_output <- weightedSetCover(idsInSet = ids_in_set,
                                     costs = set_costs$set_cost_rescaled,
                                     topN = nrow(set_costs))

