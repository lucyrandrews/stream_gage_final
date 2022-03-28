## ANALYZE CURRENT AND HYPOTHETICAL COVERAGE

# This script runs a set-cover analysis to identify the compete set of gages
# that most efficiently covers the entires stream network.

# Produce data products summarizing results ----

# identify gaged network flowlines
gaged_comids <- network_analysis_long %>%
  filter(gage_comid %in% gages$comid) %>%
  select(comid) %>%
  unique() %>%
  pull()

flowlines <- flowlines %>%
  mutate(in_gaged_network = comid %in% gaged_comids)