## CREATE UPDATED MANUSCRIPT FIGURES

# This script creates figures that are used in a manuscript second draft.

# Note that figures generated here should have legends created in an image editing
# program - I didn't have time to sort that out in code before the submission deadline!

# Specify text size defaults -----
geom_text_size <- 3.5
axis_text_size <- 14/5 * geom_text_size
axis_title_size <- 14/5 * geom_text_size * 1.4
title_size <- 14/5 * geom_text_size * 2

# Map California HUC2 -----
ggplot() + 
  geom_sf(data = ca_boundary, fill = white, alpha = 0.5, show.legend = FALSE) +
  geom_sf(data = huc2, aes(fill = "California HUC2 18"), alpha = 0.5, show.legend = TRUE) + 
  scale_fill_manual(values = grey_3) +
  annotation_north_arrow(location = "tr",
                         pad_x = unit(0.39, "in"),
                         pad_y = unit(0.5, "in"),
                         style = north_arrow_orienteering(line_col = grey_3,
                                                          text_col = grey_3,
                                                          fill = c(grey_2, grey_3))) +
  annotation_scale(location = "tr",
                   bar_cols = c(grey_2, grey_3),
                   line_col = grey_3,
                   text_col = grey_4) +
  labs(title = "California HUC2") +
  theme_minimal() +
  theme(text = element_text(color = black, family = "Times", size = axis_text_size * 2),
        plot.title = element_text(size = title_size, face = "bold"),
        legend.title = element_blank(),
        legend.position = "bottom")



# Map California stream network -----
ggplot() + 
  geom_sf(data = ca_boundary, fill = white, show.legend = FALSE) +
  geom_sf(data = flowlines, aes(color = "stream channel"), linewidth = 0.05, show.legend = "line") +
  geom_sf(data = gages, aes(color = "active gauge"), size = 1, show.legend = "point") +
  scale_color_manual(values = c("stream channel" = grey_4, "active gauge" = black),
                    name = NULL,
                    guide = guide_legend(override.aes = list(linetype = c("blank", "solid"),
                                                             linewidth = c(0.1, 1),
                                                             shape = c(16, NA),
                                                             size = c(3, 5)))) +
  annotation_north_arrow(location = "tr",
                         pad_x = unit(0.4, "in"),
                         pad_y = unit(0.5, "in"),
                         style = north_arrow_orienteering(line_col = grey_3,
                                                          text_col = grey_3,
                                                          fill = c(grey_2, grey_3))) +
  annotation_scale(location = "tr",
                   bar_cols = c(grey_2, grey_3),
                   line_col = grey_3,
                   text_col = grey_4) +
  labs(title = "California stream network and active gauges") +
  theme_minimal() +
  theme(text = element_text(color = black, family = "Times", size = axis_text_size * 2),
        plot.title = element_text(size = title_size, face = "bold"),
        legend.position = "bottom")


# Update flowlines to create factors -----
flowlines <- flowlines %>%
  mutate(huc4_name_short = case_when(huc4_name == "Klamath-Northern California Coastal" ~ "Kl-NorCal",
                                     huc4_name == "Sacramento" ~ "Sac",
                                     huc4_name == "Tulare-Buena Vista Lakes" ~ "Tul-BVL",
                                     huc4_name == "San Joaquin" ~ "SJ",
                                     huc4_name == "San Francisco Bay" ~ "SF",
                                     huc4_name == "Central California Coastal" ~ "CenCal",
                                     huc4_name == "Southern California Coastal" ~ "SoCal",
                                     huc4_name == "North Lahontan" ~ "NorLah",
                                     huc4_name == "Northern Mojave-Mono Lake" ~ "NorMoj",
                                     huc4_name == "Southern Mojave-Salton Sea" ~ "SoMoj")) %>%
  mutate(huc4_name = factor(huc4_name, levels = c("Klamath-Northern California Coastal",
                                                  "Sacramento",
                                                  "North Lahontan",
                                                  "San Joaquin",
                                                  "San Francisco Bay",
                                                  "Central California Coastal",
                                                  "Tulare-Buena Vista Lakes",
                                                  "Northern Mojave-Mono Lake",
                                                  "Southern California Coastal",
                                                  "Southern Mojave-Salton Sea")),
         huc4_name_short = factor(huc4_name_short, levels = c("Kl-NorCal",
                                                              "Sac",
                                                              "NorLah",
                                                              "SJ",
                                                              "SF",
                                                              "CenCal",
                                                              "Tul-BVL",
                                                              "NorMoj",
                                                              "SoCal",
                                                              "SoMoj")))

huc4s <- huc4s %>%
  mutate(huc4_name = factor(huc4_name, levels = c("Klamath-Northern California Coastal",
                                                  "Sacramento",
                                                  "North Lahontan",
                                                  "San Joaquin",
                                                  "San Francisco Bay",
                                                  "Central California Coastal",
                                                  "Tulare-Buena Vista Lakes",
                                                  "Northern Mojave-Mono Lake",
                                                  "Southern California Coastal",
                                                  "Southern Mojave-Salton Sea")))

# Graph active network coverage ----

# create an object for summarizing active network coverage by region
region_summary <- flowlines %>%
  st_drop_geometry() %>%
  select(comid, lengthkm, huc4_group, huc4_name, huc4_name_short, in_gaged_network,
         nid_dam, ace_outlet_biodiv_value, ref_quality_lengthkm) %>%
  mutate(ace_outlet_75pct = ifelse(ace_outlet_biodiv_value >= ace_outlet_75pct, 1, 0)) %>%
  select(-ace_outlet_biodiv_value) %>%
  pivot_longer(cols = c(lengthkm, nid_dam, ace_outlet_75pct, ref_quality_lengthkm),
               names_to = "series", 
               values_to = "value") %>%
  group_by(huc4_group, huc4_name, huc4_name_short, in_gaged_network, series) %>%
  summarize(sum = sum(value))

denoms <- region_summary %>%
  group_by(huc4_name, series) %>%
  summarize(total_denom = sum(sum))

region_summary <- region_summary %>%
  left_join(denoms, by = c("huc4_name", "series")) %>%
  mutate(prop = sum / total_denom,
         one_hundred = 1,
         series_label = case_when(series == "lengthkm" ~ "stream length (km)",
                                  series == "nid_dam" ~ "dams",
                                  series == "ace_outlet_75pct" ~ "biodiverse watersheds",
                                  series == "ref_quality_lengthkm" ~ "reference-quality streams (km)")) %>%
  mutate(series_label = factor(series_label, levels = c("stream length (km)",
                                                        "dams",
                                                        "biodiverse watersheds",
                                                        "reference-quality streams (km)")))

# create a figure faceted by management objective
ggplot(filter(region_summary, in_gaged_network)) +
  geom_col(aes(x = huc4_name_short, y = one_hundred), fill = grey_2, alpha = 0.85, width = 0.7) +
  geom_text(aes(x = huc4_name_short, y = one_hundred, label = comma(total_denom, accuracy = 1)),
            family = "Times",
            size = geom_text_size,
            vjust = -0.7) +
  geom_col(aes(x = huc4_name_short, y = prop, fill = huc4_name), width = 0.7) +
  geom_text(aes(x = huc4_name_short, y = prop, label = comma(sum, accuracy = 1)),
            family = "Times",
            size = geom_text_size,
            vjust = -0.7) +
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = 0.2),
                     minor_breaks = seq(from = 0.1, to = 0.9, by = 0.2),
                     limits = c(0, 1.2),
                     expand = c(0, 0),
                     labels = percent) +
  labs(title = "Active Network Gauge Coverage",
       x = "hydrologic region",
       y = "proportion gauged") +
  facet_wrap(facets = vars(series_label),
             ncol = 1) +
  theme_minimal() +
  theme(text = element_text(family = "Times", color = black),
        plot.title = element_text(size = title_size, face = "bold", margin = margin(b = 20)),
        legend.position = "none",
        axis.text.x = element_text(angle = 60, hjust = 1, size = axis_text_size),
        axis.title.x = element_text(size = axis_title_size),
        axis.text.y = element_text(size = axis_text_size),
        axis.title.y = element_text(vjust = 5, size = axis_title_size),
        strip.text = element_text(size = axis_title_size, face = "italic", margin = margin(b = 10)),
        panel.grid.major.x = element_blank(),
        panel.spacing = unit(1.3, "lines"),
        aspect.ratio = 0.2)

# ggsave(filename = here("output", "manuscript", "T1_active_summary_objective.jpg"))

gages <- gages %>%
  mutate(constant = 1)

ggplot() +
  geom_sf(data = ms_simplify(huc4s, keep = 0.1),
          aes(fill = huc4_name),
          alpha = 0.5) +
  geom_sf(data = filter(flowlines, streamorde >= 1),
          aes(color = in_gaged_network, linewidth = in_gaged_network)) +
  scale_color_manual(values = c(grey_3, black),
                     labels = c("not gauged", "gauged")) +
  scale_linewidth_manual(values = c(0.1, 0.5)) +
  geom_sf(data = gages,
          color = black,
          size = 1) +
  guides(fill = guide_legend(title = "Hydrologic Region"),
         color = guide_legend(title = "Gauge Status", override.aes = list(size = 2)),
         linewidth = "none",
         shape = guide_legend(title = "Gauge")) +
  annotation_north_arrow(location = "tr",
                         pad_x = unit(0.46, "in"),
                         pad_y = unit(0.5, "in"),
                         style = north_arrow_orienteering(line_col = grey_3,
                                                          text_col = grey_3,
                                                          fill = c(grey_2, grey_3))) +
  annotation_scale(location = "tr",
                   bar_cols = c(grey_2, grey_3),
                   line_col = grey_3,
                   text_col = grey_4) +
  theme_minimal() +
  theme(text = element_text(color = black, family = "Times", size = axis_text_size * 2))

# I ended up editing the above figure's legend in Word because it was getting
# too clunky and buggy for `ggplot` to handle such a customized legend.

# ggplot() + 
#   geom_sf(data = ms_simplify(huc4s, keep = 0.1),
#           aes(fill = huc4_name), alpha = 0.25) + 
#   guides(fill = guide_legend(ncol = 1, title.position = "top")) +
#   scale_fill_discrete(name = "Hydrologic Region") +
#   geom_sf(data = filter(flowlines, streamorde >= 1),
#           aes(color = in_gaged_network, linewidth = in_gaged_network),
#           show.legend = "line") +
#   scale_discrete_manual("linewidth", values = c(0.2, 0.5), guide = "none") +
#   scale_color_manual(values = c(grey_3, black),
#                      name = "Gauge Status",
#                      labels = c("not gauged", "gauged")) +
#   geom_sf(data = gages, size = 1, show.legend = FALSE) +
#   guides(linetype = guide_legend(override.aes = list(size = 2))) +
#   annotation_north_arrow(location = "tr",
#                          pad_x = unit(0.46, "in"),
#                          pad_y = unit(0.5, "in"),
#                          style = north_arrow_orienteering(line_col = grey_3,
#                                                           text_col = grey_3,
#                                                           fill = c(grey_2, grey_3))) +
#   annotation_scale(location = "tr",
#                    bar_cols = c(grey_2, grey_3),
#                    line_col = grey_3,
#                    text_col = grey_4) +
#   theme_minimal() +
#   theme(text = element_text(color = black, family = "Times", size = axis_text_size * 2))



# Create reconfigured network figures -----

# create a figure of network reconfiguration results
network_summary <- flowlines %>%
  st_drop_geometry() %>%
  select(comid, lengthkm, in_gaged_network, in_simple_reconfig_network, in_region_reconfig_network,
         nid_dam, ace_outlet_biodiv_value, ref_quality_lengthkm) %>%
  mutate(ace_outlet_75pct = ifelse(ace_outlet_biodiv_value >= ace_outlet_75pct, 1, 0)) %>%
  select(-ace_outlet_biodiv_value) %>%
  pivot_longer(cols = c(in_gaged_network, in_simple_reconfig_network, in_region_reconfig_network),
               names_to = "network", 
               values_to = "value") %>%
  group_by(network, value) %>%
  summarize(lengthkm = sum(lengthkm),
            nid_dam = sum(nid_dam),
            ace_outlet_75pct = sum(ace_outlet_75pct),
            ref_quality_lengthkm = sum(ref_quality_lengthkm)) %>%
  pivot_longer(cols = c(lengthkm, nid_dam, ace_outlet_75pct, ref_quality_lengthkm),
               names_to = "series",
               values_to = "sum")

denoms <- network_summary %>%
  group_by(network, series) %>%
  summarize(total_denom = sum(sum))

network_summary <- network_summary %>%
  left_join(denoms, by = c("network", "series")) %>%
  mutate(prop = sum / total_denom,
         one_hundred = 1,
         series_label = case_when(series == "lengthkm" ~ "stream length (km)",
                                  series == "nid_dam" ~ "dams",
                                  series == "ace_outlet_75pct" ~ "biodiverse watersheds",
                                  series == "ref_quality_lengthkm" ~ "reference-quality streams (km)"),
         network_label = case_when(network == "in_gaged_network" ~ "current",
                                   network == "in_simple_reconfig_network" ~ "statewide\nreconfigure",
                                   network == "in_region_reconfig_network" ~ "regional\nreconfigure")) %>%
  mutate(series_label = factor(series_label, levels = c("stream length (km)",
                                                        "dams",
                                                        "biodiverse watersheds",
                                                        "reference-quality streams (km)")),
         network_label = factor(network_label, levels = c("current",
                                                          "statewide\nreconfigure",
                                                          "regional\nreconfigure")))

# create a figure faceted by management objective
ggplot(filter(network_summary, value)) +
  geom_col(aes(x = network_label, y = one_hundred), fill = grey_2, alpha = 0.85, width = 0.7) +
  geom_text(aes(x = network_label, y = one_hundred, label = comma(total_denom, accuracy = 1)),
            family = "Times",
            size = geom_text_size * 0.8,
            vjust = -0.7) +
  geom_col(aes(x = network_label, y = prop, fill = network_label), width = 0.7) +
  scale_fill_manual(values = c(black, dark_orange, dark_blue)) +
  geom_text(aes(x = network_label, y = prop, label = comma(sum, accuracy = 1)),
            family = "Times",
            size = geom_text_size * 0.8,
            vjust = -0.7) +
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = 0.2),
                     minor_breaks = seq(from = 0.1, to = 0.9, by = 0.2),
                     limits = c(0, 1.2),
                     expand = c(0, 0),
                     labels = percent) +
  labs(title = "Management objective coverage in active and optimal gauge network scenarios",
       x = "scenario",
       y = "proportion gauged") +
  facet_wrap(facets = vars(series_label),
             ncol = 1) +
  theme_minimal() +
  theme(text = element_text(family = "Times", color = black),
        plot.title = element_text(size = title_size * 0.8, face = "bold", margin = margin(b = 20)),
        plot.margin = margin(b = 20, t = 20, r = 100),
        legend.position = "none",
        axis.text.x = element_text(size = axis_text_size * 0.9),
        axis.title.x = element_text(vjust = -4, size = axis_title_size * 0.8),
        axis.text.y = element_text(size = axis_text_size * 0.9),
        axis.title.y = element_text(vjust = 5, size = axis_title_size * 0.8),
        strip.text = element_text(size = axis_title_size * 0.8, face = "italic", margin = margin(b = 10)),
        panel.grid.major.x = element_blank(),
        panel.spacing = unit(1.3, "lines"),
        aspect.ratio = 0.6)

# map reconfigured networks
ggplot() + 
  geom_sf(data = ms_simplify(huc4s, keep = 0.1),
          fill = white, alpha = 0.25, show.legend = FALSE) + 
  geom_sf(data = filter(flowlines, streamorde >= 1),
          aes(color = in_simple_reconfig_network, linewidth = in_simple_reconfig_network),
          show.legend = "line") +
  geom_sf(data = filter(flowlines_midpoints, comid %in% simple_reconfig_sets$gage_comid),
          color = dark_orange,
          size = 0.75) +
  scale_discrete_manual("linewidth", values = c(0.2, 0.5), guide = "none") +
  scale_color_manual(values = c(grey_2, mid_orange),
                     labels = c("not gauged", "gauged")) +
  labs(title = "Statewide Reconfigured Network") +
  guides(color = guide_legend(override.aes = list(linewidth = 1, size = 10))) +
  theme_minimal() +
  theme(text = element_text(color = black, family = "Times", size = axis_text_size * 2),
        plot.title = element_text(size = title_size * 0.8),
        legend.position = "bottom",
        legend.title = element_blank())

ggplot() + 
  geom_sf(data = ms_simplify(huc4s, keep = 0.1),
          fill = white, alpha = 0.25, show.legend = FALSE) + 
  geom_sf(data = filter(flowlines, streamorde >= 1),
          aes(color = in_region_reconfig_network, linewidth = in_region_reconfig_network),
          show.legend = "line") +
  geom_sf(data = filter(flowlines_midpoints, comid %in% region_reconfig_sets$gage_comid),
          color = dark_blue,
          size = 0.75) +
  scale_discrete_manual("linewidth", values = c(0.2, 0.5), guide = "none") +
  scale_color_manual(values = c(grey_2, mid_dark_blue),
                     labels = c("not gauged", "gauged")) +
  annotation_north_arrow(location = "tr",
                         pad_x = unit(0.46, "in"),
                         pad_y = unit(0.5, "in"),
                         style = north_arrow_orienteering(line_col = grey_3,
                                                          text_col = grey_3,
                                                          fill = c(grey_2, grey_3))) +
  annotation_scale(location = "tr",
                   bar_cols = c(grey_2, grey_3),
                   line_col = grey_3,
                   text_col = grey_4) +
  labs(title = "Regional Reconfigured Network") +
  guides(color = guide_legend(override.aes = list(linewidth = 1, size = 10))) +
  theme_minimal() +
  theme(text = element_text(color = black, family = "Times", size = axis_text_size * 2),
        plot.title = element_text(size = title_size * 0.8),
        legend.position = "bottom",
        legend.title = element_blank())



# Create expansion network figures -----

# stream length
active_length <- flowlines %>%
  filter(in_gaged_network) %>%
  pull(lengthkm) %>%
  sum()

ggplot(expansion_sets,
       aes(x = expansion_set_index_ordered,
           y = cumsum(length_gaged) + active_length)) +
  geom_area(color = black, fill = grey_2, alpha = 0.75) +
  geom_hline(yintercept = active_length, linetype = "dashed", color = grey_4) +
  geom_text(aes(x = 490, y = active_length + 900),
            label = paste(format(active_length, digits = 1, big.mark = ",", scientific = FALSE),
                          "km of stream channel gauged in active network"),
            family = "Times",
            fontface = "italic",
            size = axis_text_size * 0.7,
            hjust = "inward",
            vjust = "inward") +
  scale_x_continuous(limits = c(0, 500),
                     breaks = seq(from = 0, to = 500, by = 50),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 27500),
                     breaks = seq(from = 0, to = 27500, by = 2500),
                     expand = c(0, 0),
                     labels = comma) +
  labs(title = "  Total Stream Channel",
       x = "additional gauges installed",
       y = "stream channel gauged (km)") +
  theme_minimal() +
  theme(text = element_text(color = black, family = "Times", size = axis_text_size * 2),
        plot.title = element_text(size = title_size * 2, margin = margin(b = 20)),
        plot.margin = margin(b = 30, t = 10, l = 20, r = 20),
        axis.text.x = element_text(size = axis_text_size * 2),
        axis.title.x = element_text(size = axis_title_size * 2, margin = margin(t = 20)),
        axis.text.y = element_text(size = axis_text_size * 2),
        axis.title.y = element_text(size = axis_title_size * 2, margin = margin(r = 20)))

# dams
active_dams <- flowlines %>%
  filter(in_gaged_network) %>%
  pull(nid_dam) %>%
  sum()

ggplot(expansion_sets,
       aes(x = expansion_set_index_ordered,
           y = cumsum(nid_dam) + active_dams)) +
  geom_area(color = black, fill = grey_2, alpha = 0.75) +
  geom_hline(yintercept = active_dams, linetype = "dashed", color = grey_4) +
  geom_text(aes(x = 490, y = active_dams + 7),
            label = paste(active_dams, " dams gauged in active network"),
            family = "Times",
            fontface = "italic",
            size = axis_text_size * 0.7,
            hjust = "inward",
            vjust = "inward") +
  scale_x_continuous(limits = c(0, 500),
                     breaks = seq(from = 0, to = 500, by = 50),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 200),
                     breaks = seq(from = 0, to = 200, by = 20),
                     expand = c(0, 0)) +
  labs(title = "  Dams",
       x = "additional gauges installed",
       y = "dams gauged") +
  theme_minimal() +
  theme(text = element_text(color = black, family = "Times", size = axis_text_size * 2),
        plot.title = element_text(size = title_size * 2, margin = margin(b = 20)),
        plot.margin = margin(b = 30, t = 10, l = 20, r = 20),
        axis.text.x = element_text(size = axis_text_size * 2),
        axis.title.x = element_text(size = axis_title_size * 2, margin = margin(t = 20)),
        axis.text.y = element_text(size = axis_text_size * 2),
        axis.title.y = element_text(size = axis_title_size * 2, margin = margin(r = 35)))

# biodiverse watersheds
active_ace_watersheds <- flowlines %>%
  filter(in_gaged_network, ace_outlet_biodiv_value > ace_outlet_75pct) %>%
  nrow()

ggplot(expansion_sets,
       aes(x = expansion_set_index_ordered,
           y = cumsum(ace_outlet_biodiv_count) + active_ace_watersheds)) +
  geom_area(color = black, fill = grey_2, alpha = 0.75) +
  geom_hline(yintercept = active_ace_watersheds, linetype = "dashed", color = grey_4) +
  geom_text(aes(x = 490, y = active_ace_watersheds + 20),
            label = paste(active_ace_watersheds, " biodiverse watersheds gauged in active network"),
            family = "Times",
            fontface = "italic",
            size = axis_text_size * 0.7,
            hjust = "inward",
            vjust = "inward") +
  scale_x_continuous(limits = c(0, 500),
                     breaks = seq(from = 0, to = 500, by = 50),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 650),
                     breaks = seq(from = 0, to = 650, by = 50),
                     expand = c(0, 0)) +
  labs(title = "  Biodiverse Watersheds",
       x = "additional gauges installed",
       y = "highly biodiverse watersheds gauged") +
  theme_minimal() +
  theme(text = element_text(color = black, family = "Times", size = axis_text_size * 2),
        plot.title = element_text(size = title_size * 2, margin = margin(b = 20)),
        plot.margin = margin(b = 30, t = 10, l = 20, r = 20),
        axis.text.x = element_text(size = axis_text_size * 2),
        axis.title.x = element_text(size = axis_title_size * 2, margin = margin(t = 20)),
        axis.text.y = element_text(size = axis_text_size * 2),
        axis.title.y = element_text(size = axis_title_size * 2, margin = margin(r = 35)))

# reference quality stream
ref_length <- flowlines %>%
  filter(in_gaged_network) %>%
  pull(ref_quality_lengthkm) %>%
  sum()

ggplot(expansion_sets,
       aes(x = expansion_set_index_ordered,
           y = cumsum(ref_quality_lengthkm) + ref_length)) +
  geom_area(color = black, fill = grey_2, alpha = 0.75) +
  geom_hline(yintercept = ref_length, linetype = "dashed", color = grey_4) +
  geom_text(aes(x = 490, y = ref_length + 400),
            label = paste(format(ref_length, digits = 1, big.mark = ",", scientific = FALSE),
                          "km of reference-quality stream channel gauged in active network"),
            family = "Times",
            fontface = "italic",
            size = axis_text_size * 0.7,
            hjust = "inward",
            vjust = "inward") +
  scale_x_continuous(limits = c(0, 500),
                     breaks = seq(from = 0, to = 500, by = 50),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 12000),
                     breaks = seq(from = 0, to = 12000, by = 2000),
                     expand = c(0, 0),
                     labels = comma) +
  labs(title = "  Reference-Quality Stream Channel",
       x = "additional gauges installed",
       y = "reference-quality stream channel gauged (km)") +
  theme_minimal() +
  theme(text = element_text(color = black, family = "Times", size = axis_text_size * 2),
        plot.title = element_text(size = title_size * 2, margin = margin(b = 20)),
        plot.margin = margin(b = 30, t = 10, l = 20, r = 20),
        axis.text.x = element_text(size = axis_text_size * 2),
        axis.title.x = element_text(size = axis_title_size * 2, margin = margin(t = 20)),
        axis.text.y = element_text(size = axis_text_size * 2),
        axis.title.y = element_text(size = axis_title_size * 2, margin = margin(r = 20)))

# map of additional expansion gauging
ggplot() +
  geom_sf(data = ms_simplify(huc4s, keep = 0.1),
          fill = white, color = "#c7c7c7", alpha = 0.25, show.legend = FALSE) +
  geom_sf(data = filter(flowlines, streamorde >= 1),
          aes(color = gaged_vs_expansion, linewidth = gaged_vs_expansion),
          show.legend = "line") +
  scale_discrete_manual("linewidth", values = c(0.05, 0.5, 0.3), guide = "none") +
  scale_color_manual(values = c("#dedede", mid_green, grey_4),
                     labels = c("not gauged", "expanded gauged network", "active gauged network"),
                     name = "Network") +
  annotation_north_arrow(location = "tr",
                         pad_x = unit(0.4, "in"),
                         pad_y = unit(0.5, "in"),
                         style = north_arrow_orienteering(line_col = grey_3,
                                                          text_col = grey_3,
                                                          fill = c(grey_2, grey_3))) +
  annotation_scale(location = "tr",
                   bar_cols = c(grey_2, grey_3),
                   line_col = grey_3,
                   text_col = grey_4) +
  labs(title = "Active and Expanded Network") +
  guides(color = guide_legend(ncol = 1)) +
  theme_minimal() +
  theme(text = element_text(color = black, family = "Times", size = axis_text_size * 2),
        plot.title = element_text(size = title_size, face = "bold"),
        legend.position = "bottom",
        legend.title = element_blank())

# regional expansion
ggplot() +
  geom_sf(data = ms_simplify(huc4s, keep = 0.1),
          fill = white, color = "#c7c7c7", alpha = 0.25, show.legend = FALSE) +
  geom_sf(data = filter(flowlines, streamorde >= 1),
          aes(color = in_gaged_network, linewidth = in_gaged_network),
          show.legend = "line") +
  scale_discrete_manual("linewidth", values = c(0.05, 0.3), guide = "none") +
  scale_color_manual(values = c("#dedede", grey_4),
                     labels = c("not gauged", "active gauged network"),
                     name = "Network") +
  geom_sf(data = filter(flowlines, streamorde >= 1, in_expansion_network_regional),
          color = mid_green,
          linewidth = 0.5) +
  annotation_north_arrow(location = "tr",
                         pad_x = unit(0.4, "in"),
                         pad_y = unit(0.5, "in"),
                         style = north_arrow_orienteering(line_col = grey_3,
                                                          text_col = grey_3,
                                                          fill = c(grey_2, grey_3))) +
  annotation_scale(location = "tr",
                   bar_cols = c(grey_2, grey_3),
                   line_col = grey_3,
                   text_col = grey_4) +
  labs(title = "Active and Regionally Expanded Network") +
  guides(color = guide_legend(ncol = 1)) +
  theme_minimal() +
  theme(text = element_text(color = black, family = "Times", size = axis_text_size * 2),
        plot.title = element_text(size = title_size, face = "bold"),
        legend.position = "bottom",
        legend.title = element_blank())


ggplot() +
  geom_sf(data = ms_simplify(huc4s, keep = 0.1),
          fill = white, color = "#c7c7c7", alpha = 0.25, show.legend = FALSE) +
  geom_sf(data = filter(flowlines, streamorde >= 1),
          aes(color = gaged_vs_expansion, linewidth = gaged_vs_expansion),
          show.legend = "line") +
  scale_discrete_manual("linewidth", values = c(0.05, 0.5, 0.3), guide = "none") +
  scale_color_manual(values = c("#dedede", mid_green, grey_4),
                     labels = c("not gauged", "expanded gauged network", "active gauged network"),
                     name = "Network") +
  annotation_north_arrow(location = "tr",
                         pad_x = unit(0.4, "in"),
                         pad_y = unit(0.5, "in"),
                         style = north_arrow_orienteering(line_col = grey_3,
                                                          text_col = grey_3,
                                                          fill = c(grey_2, grey_3))) +
  annotation_scale(location = "tr",
                   bar_cols = c(grey_2, grey_3),
                   line_col = grey_3,
                   text_col = grey_4) +
  labs(title = "Active and Expanded Network") +
  guides(color = guide_legend(ncol = 1)) +
  theme_minimal() +
  theme(text = element_text(color = black, family = "Times", size = axis_text_size * 2),
        plot.title = element_text(size = title_size, face = "bold"),
        legend.position = "bottom",
        legend.title = element_blank())



# create a figure of coverage faceted by region --> archive
# p_summary_region <- ggplot(region_summary) +
#   geom_col(aes(x = series, y = prop, fill = in_gaged_network),
#            alpha = 0.85,
#            width = 0.6) +
#   scale_fill_manual(values = c(grey_2, mid_green),
#                     labels = c("ungauged", "gauged")) +
#   geom_text(data = denoms,
#             aes(x = series, y = 0.99, label = comma(total_denom, accuracy = 1)),
#             family = "Times",
#             size = 4,
#             hjust = 1) +
#   geom_text(data = filter(region_summary, in_gaged_network),
#             aes(x = series, y = prop, label = comma(sum, accuracy = 1)),
#             family = "Times",
#             size = 4,
#             hjust = -0.3) +
#   scale_x_discrete(labels = c("reference-quality streams",
#                               "biodiverse watersheds",
#                               "dams",
#                               "stream channel length")) +
#   scale_y_continuous(breaks = seq(from = 0, to = 1, by = 0.2),
#                      labels = percent,
#                      expand = c(0, 0)) +
#   labs(title = "Active Network Gauge Coverage",
#        y = "proportion gauged") +
#   facet_wrap(facets = vars(huc4_name), ncol = 1, scales = "free_x") +
#   coord_flip() +
#   theme_minimal() +
#   theme(text = element_text(family = "Times", color = black),
#         plot.title = element_text(color = black, size = 20, face = "bold", hjust = 0.5, vjust = 4),
#         legend.text = element_text(color = black, size = 12),
#         legend.title = element_blank(),
#         legend.position = "bottom",
#         legend.margin = margin(t = 50),
#         axis.text.x = element_text(color = black, size = 12),
#         axis.title.x = element_text(color = black, size = 16, vjust = -3.5),
#         axis.text.y = element_text(color = black, size = 12),
#         axis.title.y = element_blank(),
#         strip.text.x = element_text(color = black, size = 16),
#         panel.spacing = unit(0.2, "in"),
#         plot.margin = margin(50, 50, 50, 50))
# 
# ggsave(plot = p_summary_region,
#        filename = here("output", "manuscript", "T1_active_summary_region.jpg"),
#        width = 8, height = 18, units = "in")



# Compute figures quoted in manuscript -----

# count of active gauges covering reference-quality streams
network_analysis_long %>%
  filter(gage_comid %in% gages$comid) %>%
  select(gage_comid, comid) %>%
  left_join(flowlines %>% st_drop_geometry() %>% select(comid, ref_quality),
            by = "comid") %>%
  filter(ref_quality == 1) %>%
  pull(gage_comid) %>%
  unique() %>%
  length()

