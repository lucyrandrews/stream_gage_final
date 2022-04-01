## CREATE GAGED NETWORK FIGURES

# This script creates figures that visualize the currently gaged network.

# Map currently gaged network ----

m <- ca_base_map +
  tm_shape(huc2) +
  tm_fill(col = grey_1) +
  tm_shape(filter(flowlines, streamorde >= 2)) +
  tm_lines(col = grey_2, lwd = 0.3) +
  tm_shape(filter(flowlines, streamorde >=2, in_gaged_network)) +
  tm_lines(col = mid_green, lwd = 1) +
  tm_add_legend(type = "line",
                labels = c("ungaged", "gaged"),
                col = c(grey_3, mid_green),
                lwd = 2) +
  tm_layout(main.title = "Currently Gaged Network",
            main.title.position = "center",
            legend.text.size = 1)

tmap_save(tm = m,
          filename = here("output", "figures", "currently_gaged_network.png"))

# create a map of gaged flowlines colored by value
m <- ca_base_map +
  tm_shape(huc2) +
  tm_fill(col = grey_1) +
  tm_shape(filter(flowlines, streamorde >= 2)) +
  tm_lines(col = "flowline_value",
           palette = "Greys",
           style = "cont",
           title.col = "flowline value",
           breaks = c(0, 3.2),
           labels = c("lowest value", "highest value")) +
  tm_shape(filter(flowlines, streamorde >=2, in_gaged_network)) +
  tm_lines(col = "flowline_value",
           palette = get_brewer_pal("Blues", n = 11, contrast = c(0.3, 1)),
           style = "cont",
           legend.col.show = FALSE) +
  tm_add_legend(type = "fill",
                labels = c("ungaged", "gaged"),
                col = c(grey_3, dark_blue),
                border.col = white) +
  tm_layout(main.title = "Currently Gaged Network",
            main.title.position = "center",
            legend.text.size = 1,
            legend.height = 0.75)

tmap_save(tm = m,
          filename = here("output", "figures", "currently_gaged_network_value.png"))
  
  

# Visualize gaged network coverage of management objectives ----

# make a map of ACE HUC12 outlet coverage
m <- ca_base_map +
  tm_shape(ms_simplify(huc12s)) +
  tm_fill(col = white) +
  tm_shape(filter(huc12s, !is.na(ace_outlet_biodiv_value)) %>% ms_simplify()) +
  tm_fill(col = "ace_outlet_biodiv_value",
          palette = "Greys",
          style = "cont",
          title = "biodiversity",
          breaks = c(0, 1),
          labels = c("lowest biodiversity", "highest biodiversity")) +
  tm_shape(filter(huc12s, in_gaged_network) %>% ms_simplify()) +
  tm_fill(col = "ace_outlet_biodiv_value",
          palette = "Greens",
          style = "cont",
          legend.show = FALSE) +
  tm_borders(col = black,
             lwd = 0.2) +
  tm_add_legend(type = "fill",
                labels = c("ungaged", "gaged"),
                col = c(grey_3, mid_green),
                border.col = white) +
  tm_layout(main.title = "Currently Gaged Network:\nHUC12 Outlet Gage Coverage",
            main.title.position = "center",
            legend.text.size = 1,
            legend.width = 2)
  
  tmap_save(tm = m,
            filename = here("output", "figures", "currently_gaged_ace_outlets.png"))

# make a stacked bar chart of NCCAG coverage
ggplot(filter(flowlines, nccag == 1)) +
  geom_bar(aes(x = huc4_name,
               y = nccag_lengthkm,
               fill = in_gaged_network),
           stat = "identity") +
  scale_fill_manual(values = c(grey_3, mid_green),
                    labels = c("ungaged", "gaged")) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Currently Gaged Network:\nNCCAG Coverage",
       x = "HUC4 watershed",
       y = "length of stream channel (km)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))

ggsave(filename = here("output", "figures", "currently_gaged_nccag.png"))

# make a stacked bar chart of reference quality coverage
ggplot(filter(flowlines, ref_quality == 1)) +
  geom_bar(aes(x = huc4_name,
               y = ref_quality_lengthkm,
               fill = in_gaged_network),
           stat = "identity") +
  scale_fill_manual(values = c(grey_3, mid_green),
                    labels = c("ungaged", "gaged")) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Currently Gaged Network:\nReference Quality Streams Coverage",
       x = "HUC4 watershed",
       y = "length of stream channel (km)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))

ggsave(filename = here("output", "figures", "currently_gaged_ref_quality.png"))

# make a stacked bar chart of dams coverage
ggplot(filter(flowlines, nid_dam)) +
  geom_bar(aes(x = huc4_name, fill = in_gaged_network)) +
  scale_fill_manual(values = c(grey_3, mid_green),
                    labels = c("ungaged", "gaged")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 250)) +
  labs(title = "Currently Gaged Network:\nDams Coverage",
       x = "HUC4 watershed",
       y = "count of dams") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))

ggsave(filename = here("output", "figures", "currently_gaged_dams.png"))

# clean up
rm(m)
