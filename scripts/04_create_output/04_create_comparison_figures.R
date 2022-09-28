## CREATE COMPARISON NETWORK FIGURES

# This script creates figures that compare active and reconfigured networks.

# Create comparison bar charts ----

# create a chart for gaged length
flowlines %>%
  st_drop_geometry() %>%
  select(lengthkm, in_gaged_network, in_simple_reconfig_network,
         in_region_reconfig_network) %>%
  pivot_longer(cols = 2:4,
               names_to = "network",
               values_to = "status") %>%
  mutate(network = factor(network,
                          levels = c("in_region_reconfig_network",
                                     "in_simple_reconfig_network",
                                     "in_gaged_network"))) %>%
  group_by(network, status) %>%
  summarize(lengthkm = sum(lengthkm)) %>%
  ungroup() %>%
  ggplot() +
  geom_bar(aes(x = network, y = lengthkm, fill = status), stat = "identity") +
  scale_fill_manual(values = c(grey_2, mid_green),
                    labels = c("ungaged", "gaged")) +
  scale_x_discrete(labels = c("regional reconfigured network",
                              "simple reconfigured network",
                              "active gaged network")) +
  scale_y_continuous(breaks = seq(from = 0,
                                  to = sum(flowlines$lengthkm) + 12500,
                                  by = 25000),
                     expand = c(0, 12500),
                     label = comma) +
  labs(title = "Network Comparison:\nGaged Length",
       x = "network",
       y = "length of stream channel (km)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  coord_flip()

ggsave(here("output", "figures", "compare_length.png"))

# specify facet grid labels
network_labels <- c("simple reconfigured", "regional reconfigured", "active")
names(network_labels) <- c("in_simple_reconfig_network", "in_region_reconfig_network",
                           "in_gaged_network")

# create a bar chart for ACE HUC12 outlet coverage
flowlines %>%
  st_drop_geometry() %>%
  filter(ace_outlet_biodiv_value > 0.015) %>%
  select(ace_outlet_biodiv_value, in_gaged_network, in_simple_reconfig_network,
         in_region_reconfig_network) %>%
  pivot_longer(cols = 2:4,
               names_to = "network",
               values_to = "status") %>%
  mutate(network = factor(network,
                          levels = c("in_gaged_network",
                                     "in_simple_reconfig_network",
                                     "in_region_reconfig_network"))) %>%
  ggplot() +
  geom_histogram(aes(x = ace_outlet_biodiv_value, fill = status),
                 binwidth = 0.02) +
  facet_grid(network ~ .,
             labeller = labeller(network = network_labels)) +
  scale_fill_manual(values = c(grey_2, mid_green),
                    labels = c("ungaged", "gaged")) + 
  scale_x_continuous(breaks = seq(from = 0, to = 1.0, by = 0.2),
                     expand = c(0, 0),
                     limits = c(0, 1)) +
  scale_y_continuous(breaks = seq(from = 0, to = 250, by = 25),
                     expand = c(0, 0),
                     limits = c(0, 250)) +
  labs(title = "Biodiversity Gage Coverage",
       x = "aquatic biodiversity value",
       y = "count of HUC12s") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.spacing = unit(2, "lines")) +
  coord_flip()

ggsave(here("output", "figures", "compare_ace_outlet.png"))

# remove labels
rm(network_labels)

# create a chart for NCCAG coverage
flowlines %>%
  st_drop_geometry() %>%
  select(nccag_lengthkm, in_gaged_network, in_simple_reconfig_network,
         in_region_reconfig_network) %>%
  pivot_longer(cols = 2:4,
               names_to = "network",
               values_to = "status") %>%
  group_by(network, status) %>%
  summarize(nccag_lengthkm = sum(nccag_lengthkm)) %>%
  ungroup() %>%
  ggplot() +
  geom_bar(aes(x = network, y = nccag_lengthkm, fill = status), stat = "identity") +
  scale_fill_manual(values = c(grey_2, mid_green),
                    labels = c("ungaged", "gaged")) +
  scale_x_discrete(labels = c("active gaged network",
                              "regional reconfigured network",
                              "simple reconfigured network")) +
  scale_y_continuous(breaks = seq(from = 0,
                                  to = sum(flowlines$nccag_lengthkm),
                                  by = 5000),
                     expand = c(0, 0),
                     limits = c(0, 50000),
                     label = comma) +
  labs(title = "Network Comparison:\nNCCAG Gaged Length",
       x = "network",
       y = "length of NCCAG stream channel (km)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  coord_flip()

ggsave(here("output", "figures", "compare_nccag.png"))

# create a chart for reference quality coverage
flowlines %>%
  st_drop_geometry() %>%
  select(ref_quality_lengthkm, in_gaged_network, in_simple_reconfig_network,
         in_region_reconfig_network) %>%
  pivot_longer(cols = 2:4,
               names_to = "network",
               values_to = "status") %>%
  mutate(network = factor(network,
                          levels = c("in_region_reconfig_network",
                                     "in_simple_reconfig_network",
                                     "in_gaged_network"))) %>%
  group_by(network, status) %>%
  summarize(ref_quality_lengthkm = sum(ref_quality_lengthkm)) %>%
  ungroup() %>%
  ggplot() +
  geom_bar(aes(x = network, y = ref_quality_lengthkm, fill = status), stat = "identity") +
  scale_fill_manual(values = c(grey_2, mid_green),
                    labels = c("ungaged", "gaged")) +
  scale_x_discrete(labels = c("regional reconfigured network",
                              "simple reconfigured network",
                              "active gaged network")) +
  scale_y_continuous(breaks = seq(from = 0,
                                  to = sum(flowlines$nccag_lengthkm),
                                  by = 5000),
                     expand = c(0, 0),
                     limits = c(0, 45000),
                     label = comma) +
  labs(title = "Network Comparison:\nReference Quality Gaged Length",
       x = "network",
       y = "length of reference quality stream channel (km)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  coord_flip()

ggsave(here("output", "figures", "compare_ref_quality.png"))

# create a chart for dam coverage
flowlines %>%
  st_drop_geometry() %>%
  filter(nid_dam) %>%
  select(nid_dam, in_gaged_network, in_simple_reconfig_network,
         in_region_reconfig_network) %>%
  pivot_longer(cols = 2:4,
               names_to = "network",
               values_to = "status") %>%
  mutate(network = factor(network,
                          levels = c("in_region_reconfig_network",
                                     "in_simple_reconfig_network",
                                     "in_gaged_network"))) %>%
  ggplot() +
  geom_bar(aes(x = network, fill = status)) +
  scale_fill_manual(values = c(grey_2, mid_green),
                    labels = c("ungaged", "gaged")) +
  scale_x_discrete(labels = c("regional reconfigured network",
                              "simple reconfigured network",
                              "active gaged network")) +
  scale_y_continuous(breaks = seq(from = 0,
                                  to = sum(flowlines$nid_dam) + 50,
                                  by = 100),
                     expand = c(0, 0),
                     limits = c(0, 800)) +
  labs(title = "Network Comparison:\nDams Gaged",
       x = "network",
       y = "count of dams") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  coord_flip()

ggsave(here("output", "figures", "compare_dams.png"))

