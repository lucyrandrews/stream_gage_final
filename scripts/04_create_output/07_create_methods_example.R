## CREATE A SERIES OF MAPS THAT ILLUSTRATE METHODS

# This script creates example figures used to illustrates methods.

# Note that figures generated here should have legends created in an image editing
# program - I didn't have time to sort that out in code before the submission deadline!

# set up objects for example
huc12_ids_example <- c(180500050101, 180500050102, 180500050103, 180500050104)

huc12s_example <- filter(huc12s, huc12_id %in% huc12_ids_example)

flowlines_example <- filter(flowlines, huc12_id %in% huc12_ids_example) %>%
  mutate(is_ref_quality = ifelse(ref_quality_lengthkm == 0, "false", "true")) %>%
  mutate(is_ref_quality_manual = ifelse(comid %in% c(5329505, 5329493, 5329481, 5330703, 5329499, 5329497), "true", "false"))

flowlines_midpoints_example <- filter(flowlines_midpoints, huc12_id %in% huc12_ids_example)

network_analysis_long_example <- filter(network_analysis_long, gage_comid %in% flowlines_example$comid)

dams_example <- unique(filter(dams, huc12_id %in% huc12_ids_example))


# network and management objective representation
ggplot() + 
  geom_sf(data = huc12s_example, aes(fill = ace_outlet_biodiv_value), color = grey_2) +
  scale_fill_gradient(name = "Biodiversity Score", low = white, high = grey_2) +
  geom_sf(data = flowlines_example, aes(color = is_ref_quality_manual, linewidth = is_ref_quality_manual)) +
  scale_color_manual(name = "Reference Quality Stream", values = c(grey_5, mid_purple)) +
  scale_linewidth_manual(name = "Reference Quality Stream", values = c(0.4, 1)) +
  new_scale_color() +
  geom_sf(data = flowlines_midpoints_example, aes(color = "potential gauge site"), fill = black, shape = 21, size = 3) +
  scale_color_manual(values = c("potential gauge site" = white),
                     name = NULL,
                     guide = guide_legend(override.aes = list(linetype = "blank"))) +
  geom_sf(data = dams_example, color = white, fill = black, shape = 22, size = 6) +
  annotation_north_arrow(location = "tr",
                         pad_x = unit(0.3, "in"),
                         pad_y = unit(0.4, "in"),
                         height = unit(1, "cm"),
                         width = unit(1, "cm"),
                         style = north_arrow_orienteering(line_col = grey_3,
                                                          text_col = grey_3,
                                                          fill = c(grey_2, grey_3))) +
  annotation_scale(location = "tr",
                   bar_cols = c(grey_2, grey_3),
                   line_col = grey_3,
                   text_col = grey_4) +
  labs(title = "Represent the stream network and management objectives") +
  theme_minimal() +
  theme(text = element_text(color = black, family = "Times", size = axis_text_size * 1.75),
        plot.title = element_text(size = title_size, face = "bold"),
        legend.position = "right")



# upstream and downstream coverage evaluation example
gauge_upstream_downstream_example <- tribble(
  ~comid,  ~color,
  "5329505", "red",
  "5329613", dark_yellow,
  "5330487", mid_orange,
  "5329511", mid_blue
)

flowlines_midpoints_upstream_downstream_example <- filter(flowlines_midpoints_example, comid %in% gauge_upstream_downstream_example$comid) %>%
  left_join(gauge_upstream_downstream_example, by = "comid")

comids_upstream_downstream_example <- filter(network_analysis_long_example, gage_comid %in% gauge_upstream_downstream_example$comid) %>%
  left_join(gauge_upstream_downstream_example, by = c("gage_comid" = "comid"))

flowlines_upstream_downstream_example <- filter(flowlines_example, comid %in% comids_upstream_downstream_example$comid) %>%
  left_join(comids_upstream_downstream_example, by = "comid")

ggplot() +
  geom_sf(data = huc12s_example, aes(fill = ace_outlet_biodiv_value), color = grey_2) +
  scale_fill_gradient(name = "Biodiversity Score", low = white, high = grey_2) +
  new_scale_fill() +
  geom_sf(data = flowlines_example, color = grey_5, linewidth = 0.4) +
  geom_sf(data = dams_example, color = white, fill = black, shape = 22, size = 4) +
  geom_sf(data = flowlines_upstream_downstream_example, aes(color = color), linewidth = 1) +
  scale_color_identity(guide = "legend",
                       labels = NULL) +
  geom_sf(data = flowlines_midpoints_upstream_downstream_example, aes(fill = color), color = black, shape = 21, size = 3) +
  scale_fill_identity(guide = "legend",
                      labels = NULL) +
  annotation_north_arrow(location = "tr",
                         pad_x = unit(0.3, "in"),
                         pad_y = unit(0.4, "in"),
                         height = unit(1, "cm"),
                         width = unit(1, "cm"),
                         style = north_arrow_orienteering(line_col = grey_3,
                                                          text_col = grey_3,
                                                          fill = c(grey_2, grey_3))) +
  annotation_scale(location = "tr",
                   bar_cols = c(grey_2, grey_3),
                   line_col = grey_3,
                   text_col = grey_4) +
  labs(title = "Evaluate the stream network coverage of each\npotential gauge site",
       subtitle = "Four example sites; repeat for all potential gauge sites in the network") +
  guides(fill = guide_legend(title = "example potential gauge sites and\nassociated stream network coverage"),
         color = guide_legend(title = "example potential gauge sites and\nassociated stream network coverage")) +
  theme_minimal() +
  theme(text = element_text(color = black, family = "Times", size = axis_text_size * 1.75),
        plot.title = element_text(size = title_size, face = "bold"),
        plot.subtitle = element_text(face = "italic", size = axis_text_size * 1.5),
        legend.title = element_text(size = axis_text_size * 1.5),
        legend.position = "bottom",
        legend.key.width = unit(1, "cm"))



# active network
ggplot() +
  geom_sf(data = huc12s_example, aes(fill = ace_outlet_biodiv_value), color = grey_2) +
  scale_fill_gradient(name = "Biodiversity Score", low = white, high = grey_2) +
  new_scale_fill() +
  geom_sf(data = dams_example, color = white, fill = black, shape = 22, size = 4) +
  geom_sf(data = flowlines_example, aes(color = in_gaged_network, linewidth = in_gaged_network)) +
  scale_color_manual(name = "active gauges and stream\nnetwork coverage", values = c(grey_5, mid_green)) +
  scale_linewidth_manual(name = "active gauges and stream\nnetwork coverage", values = c(0.4, 1)) +
  geom_sf(data = filter(flowlines_midpoints_example, has_gage), aes(fill = has_gage), color = black, shape = 21, size = 3) +
  scale_fill_manual(name = "active gauges and stream\nnetwork coverage", values = mid_green) +
  annotation_north_arrow(location = "tr",
                         pad_x = unit(0.3, "in"),
                         pad_y = unit(0.4, "in"),
                         height = unit(1, "cm"),
                         width = unit(1, "cm"),
                         style = north_arrow_orienteering(line_col = grey_3,
                                                          text_col = grey_3,
                                                          fill = c(grey_2, grey_3))) +
  annotation_scale(location = "tr",
                   bar_cols = c(grey_2, grey_3),
                   line_col = grey_3,
                   text_col = grey_4) +
  labs(title = "Evaluate the active gauged network") +
  theme_minimal() +
  theme(text = element_text(color = black, family = "Times", size = axis_text_size * 1.75),
        plot.title = element_text(size = title_size, face = "bold"),
        plot.subtitle = element_text(face = "italic"),
        legend.title = element_text(size = axis_text_size * 1.5),
        legend.position = "bottom",
        legend.key.width = unit(1, "cm"))



# efficient expansion network
set_cover_output_expansion_example <- filter(set_cover_output_expansion, comid %in% flowlines_example$comid)
expansion_gages_example <- filter(flowlines_midpoints_example, comid %in% set_cover_output_expansion_example$gage_comid)
expansion_gages_manual_example <- c(pull(expansion_gages_example, comid), 948050169, 5329853)

ggplot() +
  geom_sf(data = huc12s_example, aes(fill = ace_outlet_biodiv_value), color = grey_2) +
  scale_fill_gradient(name = "Biodiversity Score", low = white, high = grey_2) +
  new_scale_fill() +
  geom_sf(data = dams_example, color = white, fill = black, shape = 22, size = 4) +
  geom_sf(data = filter(flowlines_example, in_gaged_network), color = grey_5, linewidth = 0.4) +
  geom_sf(data = filter(flowlines_example, !in_gaged_network), aes(color = "expansion gauge"), linewidth = 1) +
  scale_color_manual(values = mid_blue) +
  geom_sf(data = filter(flowlines_midpoints_example, comid %in% expansion_gages_manual_example),
          aes(fill = "expansion gauge"),
          color = black,
          shape = 21,
          size = 3) +
  scale_fill_manual(values = mid_blue) +
  annotation_north_arrow(location = "tr",
                         pad_x = unit(0.3, "in"),
                         pad_y = unit(0.4, "in"),
                         height = unit(1, "cm"),
                         width = unit(1, "cm"),
                         style = north_arrow_orienteering(line_col = grey_3,
                                                          text_col = grey_3,
                                                          fill = c(grey_2, grey_3))) +
  annotation_scale(location = "tr",
                   bar_cols = c(grey_2, grey_3),
                   line_col = grey_3,
                   text_col = grey_4) +
  labs(title = "Identify the most efficient gauge sites that\ncover the ungauged network") +
  guides(fill = guide_legend(title = "most efficient gauge expansion sites and\nassociated stream network coverage"),
         color = guide_legend(title = "most efficient gauge expansion sites and\nassociated stream network coverage")) +
  theme_minimal() +
  theme(text = element_text(color = black, family = "Times", size = axis_text_size * 1.75),
        plot.title = element_text(size = title_size, face = "bold"),
        plot.subtitle = element_text(face = "italic"),
        legend.title = element_text(size = axis_text_size * 1.5),
        legend.position = "bottom",
        legend.key.width = unit(1, "cm"))



# top expansion network
expansion_gages_comids_example <- c(5329493, 5329497, 948050170)
expansion_flowlines_comids <- example <- tribble(
  ~gage_comid, ~comid,
  "5329493",   "5329481",
  "5329493",   "5329493",
  "5329493",   "5329497",
  "5329497",   "5329499",
  "5329493",   "5329505",
  "948050170", "948050168",
  "948050170", "948050169",
  "948050170", "948050170",
  "948050170", "948050164",
  "948050170", "5330753",
  "948050170", "5330767"
)

ggplot() +
  geom_sf(data = huc12s_example, aes(fill = ace_outlet_biodiv_value), color = grey_2) +
  scale_fill_gradient(name = "Biodiversity Score", low = white, high = grey_2) +
  new_scale_fill() +
  geom_sf(data = dams_example, color = white, fill = black, shape = 22, size = 4) +
  geom_sf(data = flowlines_example, aes(color = in_gaged_network, linewidth = in_gaged_network), show.legend = FALSE) +
  scale_color_manual(values = c(grey_5, mid_green)) +
  scale_linewidth_manual(values = c(0.4, 1)) +
  geom_sf(data = filter(flowlines_midpoints_example, has_gage),
          aes(fill = has_gage),
          color = black,
          shape = 21,
          size = 3,
          show.legend = FALSE) +
  scale_fill_manual(values = mid_green) +
  new_scale_color() +
  new_scale_fill() +
  geom_sf(data = filter(flowlines_example, comid %in% expansion_flowlines_comids$comid), color = mid_blue, linewidth = 1) +
  geom_sf(data = filter(flowlines_midpoints_example, comid %in% expansion_flowlines_comids$gage_comid),
          color = black, 
          fill = mid_blue, 
          shape = 21, 
          size = 3) +
  annotation_north_arrow(location = "tr",
                         pad_x = unit(0.3, "in"),
                         pad_y = unit(0.4, "in"),
                         height = unit(1, "cm"),
                         width = unit(1, "cm"),
                         style = north_arrow_orienteering(line_col = grey_3,
                                                          text_col = grey_3,
                                                          fill = c(grey_2, grey_3))) +
  annotation_scale(location = "tr",
                   bar_cols = c(grey_2, grey_3),
                   line_col = grey_3,
                   text_col = grey_4) +
  labs(title = "Select the top expansion gauge sites that best cover\nmanagement objectives",
       subtitle = "Three sites chosen in this scenario to double the count of\ngauges") +
  theme_minimal() +
  theme(text = element_text(color = black, family = "Times", size = axis_text_size * 1.75),
        plot.title = element_text(size = title_size, face = "bold"),
        plot.subtitle = element_text(face = "italic"))



# efficient reconfigured network
set_cover_output_reconfigured_example <- filter(set_cover_output_all, comid %in% flowlines_example$comid)
reconfigured_gages_example <- filter(flowlines_midpoints_example, comid %in% set_cover_output_reconfigured_example$gage_comid)
reconfigured_gages_manual_example <- c(pull(reconfigured_gages_example, comid), 948050170, 5329853)
set_costs_reconfigured_example <- filter(set_costs_all, gage_comid %in% reconfigured_gages_manual_example)

ggplot() +
  geom_sf(data = huc12s_example, aes(fill = ace_outlet_biodiv_value), color = grey_2) +
  scale_fill_gradient(name = "Biodiversity Score", low = white, high = grey_2) +
  new_scale_fill() +
  geom_sf(data = dams_example, color = white, fill = black, shape = 22, size = 4) +
  geom_sf(data = flowlines_example, color = mid_orange, linewidth = 1) +
  geom_sf(data = filter(flowlines_midpoints, comid %in% reconfigured_gages_manual_example),
          color = black,
          fill = mid_orange,
          shape = 21,
          size = 3) +
  annotation_north_arrow(location = "tr",
                         pad_x = unit(0.3, "in"),
                         pad_y = unit(0.4, "in"),
                         height = unit(1, "cm"),
                         width = unit(1, "cm"),
                         style = north_arrow_orienteering(line_col = grey_3,
                                                          text_col = grey_3,
                                                          fill = c(grey_2, grey_3))) +
  annotation_scale(location = "tr",
                   bar_cols = c(grey_2, grey_3),
                   line_col = grey_3,
                   text_col = grey_4) +
  labs(title = "Identify the most efficient gauge sites that cover the\nentire network") +
  theme_minimal() +
  theme(text = element_text(color = black, family = "Times", size = axis_text_size * 1.75),
        plot.title = element_text(size = title_size, face = "bold"),
        plot.subtitle = element_text(face = "italic"))



# top reconfigured network
reconfigured_gages_comids_example <- c(5329583, 5329497, 948050170)
reconfigured_flowlines_comids_manual <- tribble(
  ~gage_comid, ~comid,
  "948050170", "948050168",
  "948050170", "948050169",
  "948050170", "948050170",
  "948050170", "948050164",
  "948050170", "5330753",
  "948050170", "5330767",
  "5329583",   "5329831"
)

reconfigured_flowlines_comids <- filter(set_cover_output_reconfigured_example, gage_comid %in% reconfigured_gages_comids_example) %>%
  rbind(reconfigured_flowlines_comids_manual)

ggplot() +
  geom_sf(data = huc12s_example, aes(fill = ace_outlet_biodiv_value), color = grey_2) +
  scale_fill_gradient(name = "Biodiversity Score", low = white, high = grey_2) +
  new_scale_fill() +
  geom_sf(data = dams_example, color = white, fill = black, shape = 22, size = 4) +
  geom_sf(data = flowlines_example, color = grey_5, linewidth = 0.4) +
  geom_sf(data = filter(flowlines_example, comid %in% reconfigured_flowlines_comids$comid), color = mid_orange, linewidth = 1) +
  geom_sf(data = filter(flowlines_midpoints, comid %in% reconfigured_flowlines_comids$gage_comid),
          color = black,
          fill = mid_orange,
          shape = 21,
          size = 3) +
  annotation_north_arrow(location = "tr",
                         pad_x = unit(0.3, "in"),
                         pad_y = unit(0.4, "in"),
                         height = unit(1, "cm"),
                         width = unit(1, "cm"),
                         style = north_arrow_orienteering(line_col = grey_3,
                                                          text_col = grey_3,
                                                          fill = c(grey_2, grey_3))) +
  annotation_scale(location = "tr",
                   bar_cols = c(grey_2, grey_3),
                   line_col = grey_3,
                   text_col = grey_4) +
  labs(title = "Select the most valuable gauge sites to reconfigure\nthe network",
       subtitle = "Three gauge sites chosen to hypothetically relocate the three\nactive gauges") +
  theme_minimal() +
  theme(text = element_text(color = black, family = "Times", size = axis_text_size * 1.75),
        plot.title = element_text(size = title_size, face = "bold"),
        plot.subtitle = element_text(face = "italic"))