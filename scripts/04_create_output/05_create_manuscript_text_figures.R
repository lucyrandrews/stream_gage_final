## CREATE MANUSCRIPT FIGURES

# This script creates figures for the manuscript.

# Figure 1: HUC4s, gaged network, gage locations map ----

m <- ca_base_map +
  tm_shape(ms_simplify(huc4s)) +
  tm_fill(col = "huc4_name",
          palette = "Pastel1",
          alpha = 0.6,
          title  = "Hydrologic Region") +
  tm_shape(filter(flowlines, streamorde >= 2, !in_gaged_network)) +
  tm_lines(col = grey_3, lwd = 0.3) +
  tm_shape(filter(flowlines, streamorde >= 2, in_gaged_network)) +
  tm_lines(col = black, lwd = 0.9) +
  tm_shape(gages) +
  tm_dots(size = 0.07) +
  tm_add_legend(type = "symbol",
                labels = "gauge",
                size = 0.75,
                col = black) +
  tm_add_legend(type = "line",
                labels = c("ungauged network",
                           "gauged network"),
                col = c(grey_3, black),
                lwd = 2,
                title = "Gauged Status") +
  tm_layout(main.title = "Figure 1:\nActive Gauged Network and Gauge Locations",
            main.title.position = "left",
            legend.outside = TRUE,
            legend.text.size = 1,
            legend.outside.size = 0.35,
            asp = 0)

tmap_save(tm = m,
          filename = here("output", "manuscript", "1_active_gaged_network_gages_huc4s.png"),
          width = 10,
          height = 10,
          units = "in")


# Figure 2A: biases in gauge placement ----

# reshape for bar graphing
# and note where biodiversity value is/not in upper quartile for summing
flowlines_long <- flowlines %>%
  st_drop_geometry() %>%
  select(comid, huc4_name, lengthkm, nid_dam, ace_outlet_biodiv_value, ref_quality_lengthkm,
         in_gaged_network, in_simple_reconfig_network, in_region_reconfig_network) %>%
  pivot_longer(cols = c(lengthkm, nid_dam, ace_outlet_biodiv_value, ref_quality_lengthkm),
               names_to = "mgmt_obj",
               values_to = "value") %>%
  mutate(value = case_when(mgmt_obj == "ace_outlet_biodiv_value" &
                             value < ace_outlet_75pct ~ 0,
                           mgmt_obj == "ace_outlet_biodiv_value" &
                             value >= ace_outlet_75pct ~ 1,
                           TRUE ~ value)) %>%
  pivot_longer(cols = c(in_gaged_network, in_simple_reconfig_network, in_region_reconfig_network),
               names_to = "network",
               values_to = "network_membership") %>%
  select(comid, huc4_name, network, network_membership, mgmt_obj, value)

# summarize network management objectives for proportion calculations
network_denoms <- flowlines %>%
  st_drop_geometry() %>%
  summarize(total_lengthkm = sum(lengthkm),
            total_dams = sum(nid_dam),
            total_ace_outlet_biodiv_value = sum(ace_outlet_biodiv_value >= ace_outlet_75pct), # upper quartile
            total_ref_quality_lengthkm = sum(ref_quality_lengthkm)) %>%
  pivot_longer(cols = colnames(.),
               names_to = "total_mgmt_obj",
               values_to = "network_total") %>%
  cbind("mgmt_obj" = c("lengthkm", "nid_dam", "ace_outlet_biodiv_value", "ref_quality_lengthkm"))

# left join network summary and calculate proportions
flowlines_summary <- flowlines_long %>%
  group_by(network, network_membership, mgmt_obj) %>%
  summarize(sum_value = sum(value)) %>%
  left_join(network_denoms, by = "mgmt_obj") %>%
  mutate(prop_mgmt_obj = sum_value / network_total,
         mgmt_facet_label = case_when(mgmt_obj == "lengthkm" ~ "length (km)",
                                      mgmt_obj == "ace_outlet_biodiv_value" ~ "biodiversity",
                                      mgmt_obj == "nid_dam" ~ "dams",
                                      mgmt_obj == "ref_quality_lengthkm" ~ "reference streams length (km)"))

# add in lengths gaged (more reliable)

# make a bar graph
ggplot(filter(flowlines_summary, mgmt_obj != "ref_quality_lengthkm")) +
  geom_bar(aes(x = network, y = prop_mgmt_obj, fill = network_membership), stat = "identity") +
  geom_text(aes(x = network, y = prop_mgmt_obj,
                label = ifelse(network_membership, percent(round(prop_mgmt_obj, 3)), "")),
            size = 6,
            hjust = -0.15) +
  scale_fill_manual(values = c(grey_2, mid_green),
                    labels = c("ungauged", "gauged"),
                    name = "Gauge status") +
  scale_x_discrete(name = "Scenario",
                   labels = c("simple reconfigured\nnetwork", "regional reconfigured\nnetwork", "active network"),
                   limits = rev) +
  scale_y_continuous(name = NULL,
                     labels = percent_format(accuracy = 1),
                     breaks = c(seq(from = 0, to = 1, by = 0.2))) +
  labs(title = "Figure 2B:\nGauge Scenario Coverage",
       caption = "The reference-quality stream length gauged in each scenario is not shown because the proportion of length gauged is nominal for all scenarios.") +
  coord_flip() +
  facet_wrap(~mgmt_facet_label) +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0, size = 30,
                                  vjust = 4),
        axis.title.y = element_text(size = 26,
                                    vjust = 5),
        plot.caption = element_text(hjust = 0, size = 18, face = "italic",
                                    margin = margin(t = 30, r = 0, b = 0, l = 0)),
        axis.text = element_text(size = 18),
        strip.text.x = element_text(size = 26),
        plot.margin = margin(t = 1, r = 1, b = 1, l = 1, "cm"),
        panel.spacing = unit(3, "lines"))

# write out figure
ggsave(here("output", "manuscript", "2A_network_scenarios_bar_graph.png"),
       width = 24, height = 8)


# Figure 2B: HUC4s, optimal networks, gage locations maps ----

# simple reconfiguration
m <- ca_base_map +
  tm_shape(ms_simplify(huc4s)) +
  tm_fill(col = "huc4_name",
          palette = "Pastel1",
          alpha = 0.6,
          title  = "Hydrologic Region",
          legend.show = TRUE) +
  tm_shape(filter(flowlines, streamorde >= 2, !in_simple_reconfig_network)) +
  tm_lines(col = grey_3, lwd = 0.3) +
  tm_shape(filter(flowlines, streamorde >= 2, in_simple_reconfig_network)) +
  tm_lines(col = black, lwd = 0.9) +
  tm_shape(filter(flowlines_midpoints, has_simple_reconfig_network_gage)) +
  tm_dots(size = 0.07) +
  tm_add_legend(type = "symbol",
                labels = "gauge",
                size = 0.75,
                col = black) +
  tm_add_legend(type = "line",
                labels = c("ungauged network",
                           "optimal gauged network"),
                col = c(grey_3, black),
                lwd = 2,
                title = "Gauged Status") +
  tm_credits("simple optimal gauged network",
             size = 1.25,
             position = c("left", "bottom")) +
  tm_layout(main.title = "_\n_\n_",
            main.title.position = "left",
            main.title.color = "white",
            asp = 0)

# write out map
tmap_save(tm = m,
          filename = here("output", "manuscript", "2B_reconfig_simple_network_gages_huc4s.png"),
          width = 10,
          height = 10,
          units = "in")

# regional reconfiguration
m <- ca_base_map +
  tm_shape(ms_simplify(huc4s)) +
  tm_fill(col = "huc4_name",
          palette = "Pastel1",
          alpha = 0.6,
          legend.show = FALSE) +
  tm_shape(filter(flowlines, streamorde >= 2, !in_region_reconfig_network)) +
  tm_lines(col = grey_3, lwd = 0.3) +
  tm_shape(filter(flowlines, streamorde >= 2, in_region_reconfig_network)) +
  tm_lines(col = black, lwd = 0.9) +
  tm_shape(filter(flowlines_midpoints, has_region_reconfig_gage)) +
  tm_dots(size = 0.07) +
  tm_credits("regional optimal gauged network",
             size = 1.25,
             position = c("left", "bottom")) +
  tm_layout(main.title = "Figure 2B:\nOptimal Reconfigured Gauged Network -\nSimple and Regional Distributions",
            main.title.position = "left")

# write out map
tmap_save(tm = m,
          filename = here("output", "manuscript", "2B_reconfig_regional_network_gages_huc4s.png"),
          width = 12,
          height = 10,
          units = "in")


# Figure 3: Expansion Network ----

# summarize expansion network for continuous visualization
# flag where upper-quartile biodiversity HUC12 outlets are/not expansion gaged
expansion_sets <- expansion_sets %>%
  mutate(ace_outlet_biodiv_count = as.numeric(!ace_outlet_biodiv_value == 0))

# create a plot for each management objective
p_dam <- ggplot(expansion_sets, aes(x = expansion_set_index_ordered, y = cumsum(nid_dam))) +
  geom_area(color = black, fill = light_blue, alpha = 0.75) +
  geom_text(aes(x = 475, y = 7.5), size = 8, fontface = 3, label = "dams",
            hjust = "inward", vjust = "inward") +
  scale_x_continuous(limits = c(0, 500),
                     breaks = seq(from = 0, to = 500, by = 50),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 125),
                     breaks = seq(from = 0, to = 125, by = 25),
                     expand = c(0, 0)) +
  labs(title = "Figure 3A: Gauge Network Expansion Benefits",
       y = "Cumulative Additional Count of Dams Gauged") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 20,
                                    vjust = 5),
        axis.text = element_text(size = 14),
        plot.title = element_text(hjust = 0, size = 30,
                                  vjust = 4),
        plot.margin = margin(t = 1, r = 1, b = 1, l = 1, "cm"))

p_biodiv <- ggplot(expansion_sets, aes(x = expansion_set_index_ordered, y = cumsum(ace_outlet_biodiv_count))) +
  geom_area(color = black, fill = light_yellow, alpha = 0.75) +
  geom_text(aes(x = 475, y = 20), size = 8, fontface = 3, label = "biodiversity",
            hjust = "inward", vjust = "inward") +
  scale_x_continuous(limits = c(0, 500),
                     breaks = seq(from = 0, to = 500, by = 50),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 350),
                     breaks = seq(from = 0, to = 350, by = 50),
                     expand = c(0, 0)) +
  labs(y = "Cumulative Additional Count of\nHigh-Biodiversity Watersheds Gauged") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 20,
                                    vjust = 8),
        axis.text = element_text(size = 14),
        plot.margin = margin(t = 1, r = 1, b = 1, l = 1, "cm"))

p_ref_lengthkm <- ggplot(expansion_sets, aes(x = expansion_set_index_ordered, y = cumsum(ref_quality_lengthkm))) +
  geom_area(color = black, fill = light_green, alpha = 0.75) +
  geom_text(aes(x = 475, y = 700), size = 8, fontface = 3, label = "reference-quality\nstream channel",
            hjust = "inward", vjust = "inward") +
  scale_x_continuous(limits = c(0, 500),
                     breaks = seq(from = 0, to = 500, by = 50),
                     expand = c(0, 0)) +
  scale_y_continuous(label = comma,
                     limits = c(0, 12000),
                     breaks = seq(from = 0, to = 12000, by = 1000),
                     expand = c(0, 0)) +
  labs(y = "Cumulative Additional Length of\nReference-Quality Stream Channel Gauged (km)") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 20,
                                    vjust = 5),
        axis.text = element_text(size = 14),
        plot.margin = margin(t = 1, r = 1, b = 1, l = 1, "cm"))

p_lengthkm <- ggplot(expansion_sets, aes(x = expansion_set_index_ordered, y = cumsum(length_gaged))) +
  geom_area(color = black, fill = grey_2, alpha = 0.75) +
  geom_text(aes(x = 475, y = 1000), size = 8, fontface = 3, label = "total stream channel",
            hjust = "inward", vjust = "inward") +
  scale_x_continuous(limits = c(0, 500),
                     breaks = seq(from = 0, to = 500, by = 50),
                     expand = c(0, 0)) +
  scale_y_continuous(label = comma,
                     limits = c(0, 16000),
                     breaks = seq(from = 0, to = 16000, by = 1000),
                     expand = c(0, 0)) +
  labs(x = "Additional Gauges Installed",
       y = "Cumulative Additional Length of\nTotal Stream Channel Gauged (km)") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 20,
                                    vjust = -5),
        axis.title.y = element_text(size = 20,
                                    vjust = 5),
        axis.text = element_text(size = 14),
        plot.margin = margin(t = 1, r = 1, b = 1, l = 1, "cm"))

# write out figure
ggsave(here("output", "manuscript", "3A_expansion_cumulative_gains.png"),
       plot = grid.arrange(p_dam, p_biodiv, p_ref_lengthkm, p_lengthkm, ncol = 1),
       width = 12, height = 24)


# Table 3: Expansion Network Scenarios ----

# summarize expansion scenarios
expansion_summary <- expansion_sets %>%
  select(expansion_set_index_ordered, length_gaged, nid_dam, ace_outlet_biodiv_count,
         ref_quality_lengthkm, expansion_50, expansion_100, expansion_500) %>%
  pivot_longer(cols = c(expansion_50, expansion_100, expansion_500),
               names_to = "expansion_scenario",
               values_to = "in_expansion_scenario") %>%
  mutate(expansion_scenario = factor(expansion_scenario,
                                     levels = c("expansion_50", "expansion_100", "expansion_500"))) %>%
  filter(in_expansion_scenario) %>%
  select(expansion_set_index_ordered, expansion_scenario, nid_dam, ace_outlet_biodiv_count,
         ref_quality_lengthkm, length_gaged) %>%
  group_by(expansion_scenario) %>%
  summarize(nid_dam = sum(nid_dam),
            ace_outlet_biodiv_count = sum(ace_outlet_biodiv_count),
            ref_quality_lengthkm = sum(ref_quality_lengthkm),
            length_gaged = sum(length_gaged))

# write out expansion summary table
sink(file = here("output", "manuscript", "3_expansion_summary.txt"),
     append = FALSE,
     type = "output")

expansion_summary

sink(file = NULL)


# Figure 3B: Map of Expansion Networks ----

# map expansion network
m <- ca_base_map +
  tm_shape(filter(flowlines, streamorde >= 3, !in_gaged_network)) +
  tm_lines(col = grey_2, lwd = 0.3) +
  tm_shape(filter(flowlines, streamorde >= 2, in_gaged_network)) +
  tm_lines(col = grey_4, lwd = 1.5) +
  tm_shape(filter(flowlines, streamorde >= 2, expansion_set == "expansion_500")) +
  tm_lines(col = mid_green, lwd = 1.5) +
  tm_shape(filter(flowlines, streamorde >= 2, expansion_set == "expansion_100")) +
  tm_lines(col = mid_green, lwd = 1.5) +
  tm_shape(filter(flowlines, streamorde >= 2, expansion_set == "expansion_50")) +
  tm_lines(col = mid_green, lwd = 1.5) +
  tm_add_legend(type = "line",
                labels = c("ungauged network",
                           "active gauged network",
                           "expansion gauged network"),
                col = c(grey_2, grey_4, mid_green),
                lwd = 2,
                title = "Gauged Status") +
  tm_layout(main.title = "Figure 3B:\nExpanded Network - 500 Additional Gauges",
            main.title.position = "left",
            legend.outside = FALSE,
            legend.text.size = 1,
            legend.title.size = 1.3,
            legend.width = 1,
            asp = 0)
  
tmap_save(tm = m,
          filename = here("output", "manuscript", "3B_expansion_network.png"),
          width = 8,
          height = 10,
          units = "in")  

  
  