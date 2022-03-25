## ADD MANAGEMENT-RELEVANT ATTRIBUTES TO FLOWLINES

# This script associates flowlines with management-relevant attributes:  
#   - Biodiversity: California Department of Fish and Wildlife Areas of  
#       Conservation Emphasis (ACE) aquatic biodiversity index  
#         > [0, 1]  
#   - Groundwater-surface water connectivity: California Department of Water  
#       Resources Natural Communities Commonly Associated with Groundwater  
#       (NCCAG)
#         > {0, 1}
#   - Reference-quality streams: streams whose hydrology can be considered  
#       relatively unimpaired, such that the hydrograph can serve as a reference
#       point for environmental flows analysis  
#         > {0, 1}
#   - Dams: large dams listed in the United States Army Corps of Engineers  
#       National Inventory of Dams (NID) for which flow monitoring can lend
#       useful water release management information  
#         > {0, 1}

# Associate flowlines with ACE data ----

# left join ACE data on HUC 12 ID
flowlines <- flowlines %>%
  left_join(st_drop_geometry(ace), by = "huc12_id") %>%
  mutate(ace_outlet_biodiv_value = huc12_outlet * ace_aq_biodiv_value)

# clean up
rm(ace)



# Associate flowlines with NCCAG data ----

# join NCCAG to flowlines with spatial intersection and select distinct records
# to attend to circumstances in which a flowline intersects with more than one
# NCCAG polygon
flowlines <- flowlines %>%
  st_join(nccag) %>%
  distinct()

# clean up
rm(nccag)



# Associate flowlines with reference-quality streams ----

# left join reference quality stream segments to flowlines on comid
flowlines <- flowlines %>%
  left_join(ref_comids, by = "comid")

# clean up
rm(ref_comids)



# Associate flowlines with NID dams ----

# create a dataframe for indexing and nearest neighbor search that has the right
# column names
flowlines_indexing <- flowlines %>%
  select(comid, reachcode, tomeas, frommeas) %>%
  rename(COMID = comid,
         REACHCODE = reachcode,
         ToMeas = tomeas,
         FromMeas = frommeas)

# search for neighbors with a distance tolerance
nid_nn <- get_flowline_index(flines = flowlines_indexing,
                             points = st_cast(x = nid, to = "POINT"),
                             search_radius = nn_search_radius,
                             precision = 10,
                             # max_matches is a large number to grab all matches
                             max_matches = 100) %>%
  rename(nid_index = id,
         comid = COMID,
         reachcode = REACHCODE,
         reachmeasure = REACH_meas)

# join in flowline information to neighbors
nid_nn <- nid_nn %>%
  left_join(dplyr::select(flowlines, comid, reachcode, totdasqkm),
            by = c("comid", "reachcode")) %>%
  select(-geometry)

# join dam information to neighbors
nid_nn <- nid_nn %>%
  left_join(st_drop_geometry(nid),
            by = "nid_index") %>%
  rowwise() %>%
  mutate(da_dif = abs(totdasqkm - nid_totdasqkm)) %>%
  ungroup()

# select neighbor for each dam that has the closest drainage area
# to the drainage area reported in the NID
nid_nn_sliced <- nid_nn %>%
  group_by(nid_index) %>%
  slice(which.min(da_dif)) %>%
  ungroup()

# add geometry in for the associated comid to locate chosen neighbor dams
# and filter to drop dams whose drainage area can't be matched with a flowline
# within a reasonable tolerance (thereby likely indicating off-channel location)
nid_nn_sliced <- flowlines %>%
  st_drop_geometry() %>%
  select(comid) %>%
  right_join(nid_nn_sliced, by = "comid") %>%
  rowwise() %>%
  mutate(da_dif_ratio =  totdasqkm / nid_totdasqkm) %>%
  ungroup() %>%
  filter(da_dif_ratio >= da_dif_ratio_min & da_dif_ratio <= da_dif_ratio_max |
           da_dif < da_dif_max & !is.infinite(da_dif_ratio))

# clean up
rm(flowlines_indexing, nn_search_radius, nid, nid_nn, nid_nn_sliced, da_dif_max,
   da_dif_ratio_min, da_dif_ratio_max)