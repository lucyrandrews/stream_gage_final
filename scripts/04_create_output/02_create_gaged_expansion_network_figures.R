## CREATE GAGED NETWORK FIGURES

# This script creates figures that visualize the currently gaged network.

# Map currently gaged network ----

# create map of gaged and expansion networks
m <- ca_base_map +
  tm_shape(huc2) +
  tm_fill(col = grey_1) +
  tm_shape(filter(flowlines, streamorde >= 1)) +
  tm_lines(col = grey_2, lwd = 0.3) +
  tm_shape(filter(flowlines, streamorde >= 1, in_gaged_network)) +
  tm_lines(col = mid_green, lwd = 1) +
  tm_shape(filter(flowlines, streamorde >= 1, in_expansion_network)) +
  tm_lines(col = mid_orange, lwd = 1) +
  tm_add_legend(type = "line",
                labels = c("ungaged network",
                           "gaged network",
                           "expanded gaged network "),
                col = c(grey_3, mid_green, mid_orange),
                lwd = 2) +
  tm_layout(main.title = "Currently Gaged and Expansion Networks",
            main.title.position = "center",
            legend.text.size = 0.8,
            legend.width = 0.8,
            legend.position = c("right", "top"))

tmap_save(tm = m,
          filename = here("output", "figures", "currently_gaged_expansion_network.png"))

# create a map of gaged and expansion flowlines colored by value
m <- ca_base_map +
  tm_shape(huc2) +
  tm_fill(col = grey_1) +
  tm_shape(filter(flowlines, streamorde >= 1)) +
  tm_lines(col = "flowline_value",
           palette = "Greys",
           style = "cont",
           title.col = "",
           breaks = c(0, 4),
           labels = c("lowest value", "highest value")) +
  tm_shape(filter(flowlines, streamorde >= 1, in_gaged_network)) +
  tm_lines(col = "flowline_value",
           palette = get_brewer_pal("Greens", contrast = c(0.3, 1)),
           style = "cont",
           legend.col.show = FALSE) +
  tm_shape(filter(flowlines, streamorde >= 1, in_expansion_network)) +
  tm_lines(col = "flowline_value",
           palette = get_brewer_pal("YlOrBr", contrast = c(0.3, 1)),
           style = "cont",
           legend.col.show = FALSE) +
  tm_add_legend(type = "fill",
                labels = c("ungaged network",
                           "gaged network",
                           "expanded gaged network"),
                col = c(grey_3, mid_green, mid_orange),
                border.col = white) +
  tm_layout(main.title = "Currently Gaged and Expansion Networks -\nFlowline Value",
            main.title.position = "center",
            legend.text.size = 0.8,
            legend.width = 0.8,
            legend.height = 0.8)

tmap_save(tm = m,
          filename = here("output", "figures", "currently_gaged_expansion_network_value.png"))
  
# make a map of active and expansion gage locations

flowlines_midpoints <- flowlines_midpoints %>%
  left_join(flowlines %>%
              st_drop_geometry() %>%
              filter(has_gage | has_expansion_gage | has_simple_reconfig_network_gage | has_region_reconfig_gage) %>%
              select(comid, has_gage, has_expansion_gage, has_simple_reconfig_network_gage, has_region_reconfig_gage),
            by = "comid")
  
m <- ca_base_map +
  tm_shape(huc2) +
  tm_fill(col = grey_1) +
  tm_shape(filter(flowlines, streamorde >= 1)) +
  tm_lines(col = grey_2, lwd = 0.3) +
  tm_shape(filter(flowlines_midpoints, has_gage)) +
  tm_dots(col = mid_green,
          size = 0.025) +
  tm_shape(filter(flowlines_midpoints, has_expansion_gage)) +
  tm_dots(col = mid_orange,
          size = 0.025) +
  tm_add_legend(type = "symbol",
                labels = c("current gage",
                           "expansion gage"),
                col = c(mid_green, mid_orange),
                border.col = white) +
  tm_layout(main.title = "Current and Expansion\nGage Locations",
            main.title.position = "center",
            legend.text.size = 0.8,
            legend.width = 0.8,
            legend.height = 0.8)  

tmap_save(tm = m,
          filename = here("output", "figures", "currently_gaged_expansion_network_gages.png"))  



# Visualize gaged and expansion network coverage of management objectives ----

# make a map of ACE HUC12 outlet coverage
m <- ca_base_map +
  tm_shape(ms_simplify(huc12s)) +
  tm_fill(col = white) +
  tm_shape(filter(huc12s, !is.na(ace_outlet_biodiv_value)) %>% ms_simplify()) +
  tm_fill(col = "ace_outlet_biodiv_value",
          palette = "Greys",
          style = "cont",
          title = "",
          breaks = c(0, 1),
          labels = c("lowest biodiversity", "highest biodiversity")) +
  tm_shape(filter(huc12s, in_expansion_network) %>% ms_simplify()) +
  tm_fill(col = "ace_outlet_biodiv_value",
          palette = "YlOrBr",
          style = "cont",
          legend.show = FALSE) +
  tm_borders(col = black,
             lwd = 0.2) +
  tm_shape(filter(huc12s, in_gaged_network) %>% ms_simplify()) +
  tm_fill(col = "ace_outlet_biodiv_value",
          palette = "Greens",
          style = "cont",
          legend.show = FALSE) +
  tm_borders(col = black,
             lwd = 0.2) +
  tm_add_legend(type = "fill",
                labels = c("ungaged", "gaged", "expanded gaged"),
                col = c(grey_3, mid_green, mid_orange),
                border.col = white) +
  tm_layout(main.title = "Currently Gaged and Expansion Networks:\nHUC12 Outlet Gage Coverage",
            main.title.position = "center",
            legend.text.size = 0.8,
            legend.width = 1)
  
tmap_save(tm = m,
          filename = here("output", "figures", "currently_gaged_ace_outlets.png"))

# make a stacked bar chart of NCCAG coverage
ggplot(filter(flowlines, nccag == 1)) +
  geom_bar(aes(x = huc4_name,
               y = nccag_lengthkm,
               fill = gaged_vs_expansion),
           stat = "identity") +
  scale_fill_manual(values = c(grey_3, mid_orange, mid_green)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Currently Gaged and Expansion Network:\nNCCAG Coverage",
       x = "HUC4 watershed",
       y = "length of stream channel (km)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  coord_flip()

ggsave(filename = here("output", "figures", "currently_gaged_nccag.png"))

# make a stacked bar chart of reference quality coverage
ggplot(filter(flowlines, ref_quality == 1)) +
  geom_bar(aes(x = huc4_name,
               y = ref_quality_lengthkm,
               fill = gaged_vs_expansion),
           stat = "identity") +
  scale_fill_manual(values = c(grey_3, mid_orange, mid_green)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Currently Gaged and Expansion Networks:\nReference Quality Streams Coverage",
       x = "HUC4 watershed",
       y = "length of stream channel (km)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  coord_flip()

ggsave(filename = here("output", "figures", "currently_gaged_ref_quality.png"))

# make a stacked bar chart of dams coverage
ggplot(filter(flowlines, nid_dam)) +
  geom_bar(aes(x = huc4_name, fill = gaged_vs_expansion)) +
  scale_fill_manual(values = c(grey_3, mid_orange, mid_green)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 250)) +
  labs(title = "Currently Gaged and Expansion Networks:\nDams Coverage",
       x = "HUC4 watershed",
       y = "count of dams") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  coord_flip()

ggsave(filename = here("output", "figures", "currently_gaged_dams.png"))

# clean up
rm(m)
