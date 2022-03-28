## CREATE FIGURES

# This script creates figures from final results.

# Map currently gaged network ----

ca_base_map +
  tm_shape(huc2) +
  tm_fill(col = white) +
  tm_shape(filter(flowlines, streamorde >= 2)) +
  tm_lines(col = mid_grey, lwd = 0.3) +
  tm_shape(filter(flowlines, streamorde >=2, in_gaged_network)) +
  tm_lines(col = mid_green, lwd = 1) +
  tm_add_legend(type = "line",
                labels = c("Ungaged", "Gaged"),
                col = c(mid_grey, mid_green),
                lwd = 2) +
  tm_layout(main.title = "Currently Gaged Network",
            main.title.position = "center",
            legend.text.size = 1)



# Visualize gaged network coverage of management objectives ----