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
#       useful water release management information, including weighting option
#       for degree of regulation
#         > {0, 1}
#         > DoR: [0, 1]
#   - Gages: gages operated by USGS and listed in the GagesII dataset maintained
#       in the National Hydrography Dataset
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
         FromMeas = frommeas) %>%
  st_cast(to = "MULTILINESTRING")

# search for neighbors with a distance tolerance
# set `precision` argument to `NA` due to run-time issues; this means that
# dams are matching to nearby flowline nodes (upstream and downstream ends)
nid_nn <- get_flowline_index(flines = flowlines_indexing,
                             points = st_cast(x = nid, to = "POINT"),
                             search_radius = nn_search_radius,
                             precision = NA,
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

# join dam information to comid neighbors
nid_nn <- nid_nn %>%
  left_join(st_drop_geometry(nid),
            by = "nid_index") %>%
  rowwise() %>%
  mutate(da_dif = abs(totdasqkm - nid_totdasqkm)) %>%
  ungroup()

# select comid neighbor for each dam that has the closest drainage area to the
# drainage area reported in the NID
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

# associate flowlines with dams
flowlines <- flowlines %>%
  mutate(nid_dam = comid %in% nid_nn_sliced$comid) %>%
  left_join(select(nid_nn_sliced, comid, nid_storage_af), by = "comid") %>%
  mutate(degree_of_reg = nid_storage_af / est_annual_discharge_af)

# clean up
rm(nid, nid_nn, nid_nn_sliced)



# Associate gages with flowlines ----

# search for neighbors with a distance tolerance
# same run-time issue as before (with dams) addressed with `precision = NA`
gages_nn <- get_flowline_index(flines = flowlines_indexing,
                               points = gages,
                               search_radius = nn_search_radius,
                               precision = NA,
                               # max_matches is a large number to grab all matches
                               max_matches = 100) %>%
  rename(gage_index = id,
         comid = COMID,
         reachcode = REACHCODE,
         reachmeasure = REACH_meas)

# join in flowline information to neighbors
gages_nn <- gages_nn %>%
  left_join(dplyr::select(flowlines, comid, reachcode, totdasqkm),
            by = c("comid", "reachcode")) %>%
  select(-geometry)

# join gage information to comid neighbors
gages_nn <- gages_nn %>%
  left_join(st_drop_geometry(gages),
            by = "gage_index") %>%
  rowwise() %>%
  mutate(da_dif = abs(totdasqkm - gage_totdasqkm)) %>%
  ungroup()

# select comid neighbor for each gage that has the closest drainage area to the
# drainage area reported in the gages dataset
gages_nn_sliced <- gages_nn %>%
  group_by(gage_index) %>%
  slice(which.min(da_dif)) %>%
  ungroup()

# add geometry in for the associated comid to locate chosen neighbor gages
# and filter to drop gages whose drainage area can't be matched with a flowline
# within a reasonable tolerance (thereby likely indicating off-channel or
# erroneous location)
gages_nn_sliced <- flowlines %>%
  st_drop_geometry() %>%
  select(comid) %>%
  right_join(gages_nn_sliced, by = "comid") %>%
  rowwise() %>%
  mutate(da_dif_ratio =  totdasqkm / gage_totdasqkm) %>%
  ungroup() %>%
  filter(da_dif_ratio >= da_dif_ratio_min & da_dif_ratio <= da_dif_ratio_max |
           da_dif < da_dif_max & !is.infinite(da_dif_ratio))

# associate flowlines with gages
flowlines <- flowlines %>%
  mutate(has_gage = comid %in% gages_nn_sliced$comid)

# associate gages with flowline comids
gages <- gages %>%
  left_join(select(gages_nn_sliced, comid, gage_index, totdasqkm),
            by = "gage_index") %>%
  filter(!is.na(comid))

# clean up
rm(gages_nn, gages_nn_sliced, da_dif_max, da_dif_ratio_max, da_dif_ratio_min,
   nn_search_radius)



# Clean NA, Inf, and Extreme values ----

# update flowlines to clean NA values in NCCAG and reference quality
flowlines <- flowlines %>%
  mutate(nccag = case_when(is.na(nccag) ~ 0,
                           TRUE ~ 1),
         ref_quality = case_when(is.na(ref_quality) ~ 0,
                                 TRUE ~ 1),
         nccag_lengthkm = nccag * lengthkm,
         ref_quality_lengthkm = ref_quality * lengthkm,
         degree_of_reg = case_when(is.na(degree_of_reg) ~ 0,
                                   TRUE ~ degree_of_reg)) %>%
  rowwise() %>%
  mutate(flowline_value =
           sum((ace_aq_biodiv_value * use_ace),
               (nccag * use_nccag),
               (ref_quality * use_ref_streams),
               (nid_dam * use_dams))) %>%
  ungroup()

# update flowlines to clean infinite values in degree of regulation
flowlines <- flowlines %>%
  mutate(degree_of_reg = ifelse(is.finite(degree_of_reg), degree_of_reg, 0))

# update flowlines to cap degree of regulation at 1.0 for points system
flowlines <- flowlines %>%
  mutate(degree_of_reg = ifelse(degree_of_reg <= 1.0, degree_of_reg, 1.0))