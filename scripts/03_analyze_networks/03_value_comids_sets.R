## CHARACTERIZE COMID VALUE AND SET COST

# This script characterizes comid value to inform set cover analysis costs.

# Compute the value of each flowline in association with its gage(s) ----

# mutate on value-relevant columns
network_analysis_long <- network_analysis_long %>%
  left_join(select(flowlines, comid, lengthkm,
                   ace_outlet_biodiv_value, nccag, ref_quality, nid_dam) %>%
              st_drop_geometry(),
            by = "comid") %>% 
  mutate(nccag = as.numeric(nccag),
         ref_quality = as.numeric(nccag),
         nid_dam = as.numeric(nid_dam)) %>%
  replace(is.na(.), 0) %>%
  mutate(upstream_nccag = case_when(gage_location == "downstream" & nccag == 1 ~ 0.5,
                                    TRUE ~ 0),
         downstream_nccag = case_when(gage_location == "upstream" & nccag == 1 ~ 0.5,
                                      TRUE ~ 0),
         on_nccag = case_when(gage_location == "on comid" & nccag == 1 ~ 0.5,
                              TRUE ~ 0),
         upstream_nid_dam = case_when(gage_location == "downstream" & nid_dam == 1 ~ 0.5,
                                      TRUE ~ 0),
         downstream_nid_dam = case_when(gage_location == "upstream" & nid_dam == 1 ~ 0.5,
                                        TRUE ~ 0),
         on_nid_dam = case_when(gage_location == "on comid" & nid_dam == 1 ~ 0.5,
                                TRUE ~ 0))

# Compute the value (inverted cost) of each set ----

# The ACE biodiversity value is the value associated with a comid, only if it
# is a HUC12 outlet

# The NCCAG value is sum of the maximum two values of upstream, downstream,
# and on the comid values (to recognize that a stream gage would realistically
# only cover upstream and downstream - co-location with an NCCAG site is the
# "same" as either an upstream or downstream gage location (this assumes
# flexibility in where on a comid a gage could be installed for comids that
# contain and NCCAG site)

# The reference quality value is binary based on whether the gage location
# covers a reference quality comid(s)

# The NID dam value is sum of the maximum two values of upstream, downstream,
# and on the comid values (to recognize that a stream gage can't cover three
# dams - only the two dams it is between; this assumes flexibility in where on a
# comid a gage could be installed for comids that contain a dam)
set_costs <- network_analysis_long %>%
  group_by(gage_comid) %>%
  summarize(comids_str_concat = paste(comid, collapse = " "),
            comids_count = n(),
            length_gaged = sum(lengthkm),
            ace_outlet_biodiv_value = max(ace_outlet_biodiv_value),
            upstream_nccag = max(upstream_nccag),
            downstream_nccag = max(downstream_nccag),
            on_nccag = max(on_nccag),
            ref_quality = max(ref_quality),
            upstream_nid_dam = max(upstream_nid_dam),
            downstream_nid_dam = max(downstream_nid_dam),
            on_nid_dam = max(on_nid_dam)) %>%
  rowwise() %>%
  mutate(nccag = sum(upstream_nccag, downstream_nccag, on_nccag),
         nid_dam = sum(upstream_nid_dam, downstream_nid_dam, on_nid_dam)) %>%
  ungroup()

# replace all instances of value = 1.5 with value = 1.0 (per explanation above
# of maximum total upstream, downstream, and on-comid value)
set_costs[set_costs == 1.5] <- 1.0

# compute the total value (cost) of each set and add HUC4 ID for regional
# analysis
set_costs <- set_costs %>%
  rowwise() %>%
  mutate(set_value = sum((ace_outlet_biodiv_value * use_ace),
                         (nccag * use_nccag),
                         (ref_quality * use_ref_streams),
                         (nid_dam * use_dams))) %>%
  ungroup() %>%
  rowid_to_column(var = "set_id") %>%
  left_join(st_drop_geometry(select(flowlines, comid, huc4_group)),
            by = c("gage_comid" = "comid"))

# rescale value column to a [0, 1] range for ease
set_costs <- set_costs %>%
  mutate(set_cost_rescaled = 1 - rescale_zero_one(set_value))

# replace 0 values with a tiny number, since 0 cost indicates a set of maximum
# value but will get in the way of set-cover weighting; essentially, there is
# no "cost" to selecting these sets
set_costs[set_costs == 0] <- 0.000001

